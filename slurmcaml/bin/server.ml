open Slurmcaml.Matrixutils

let users = Hashtbl.create 10
let head_props = ref None
let jobs = Queue.create ()
let job_table = Hashtbl.create 10

let sock_addr_to_string (add : Unix.sockaddr) =
  match add with
  | ADDR_INET (a, port) -> Unix.string_of_inet_addr a ^ ":" ^ string_of_int port
  | _ -> failwith "unsupported"

let find_available_worker () =
  Hashtbl.fold
    (fun key (username, in_, out_, available) acc ->
      if available then Some (key, username, in_, out_) else acc)
    users None

let parse_status_string status_string =
  match String.split_on_char '|' status_string with
  | status :: job :: output_parts ->
      let output = String.concat "|" output_parts in
      Some (status, job, output)
  | _ -> None

type int_job = {
  aint : int array array;
  bint : int array array;
  opi : string;
}

type float_job = {
  afloat : float array array;
  bfloat : float array array;
  opf : string;
}

type job =
  | IntJob of int_job
  | FloatJob of float_job

let process_job in_channel =
  let%lwt valuetype = Lwt_io.read_line in_channel in
  let%lwt op = Lwt_io.read_line in_channel in
  match valuetype with
  | "int" ->
      let%lwt mat1 = read_int_matrix_input in_channel in
      let%lwt () = Lwt_io.printf "mat1: %d rows\n%!" (Array.length mat1) in
      let%lwt mat2 = read_int_matrix_input in_channel in
      let%lwt () = Lwt_io.printf "mat1: %d rows\n%!" (Array.length mat2) in
      Lwt.return (IntJob { aint = mat1; bint = mat2; opi = op })
  | "float" ->
      let%lwt mat1 = read_float_matrix_input in_channel in
      let%lwt mat2 = read_float_matrix_input in_channel in
      Lwt.return (FloatJob { afloat = mat1; bfloat = mat2; opf = op })
  | _ -> failwith "not a supported type"

type split =
  | IntJobSplit of int_job array
  | FloatJobSplit of float_job array

let split_matrix mat num =
  let n = Array.length mat in
  let base = n / num in
  let extra = n mod num in
  Array.init num (fun i ->
      let size = base + if i < extra then 1 else 0 in
      let start = (i * base) + min i extra in
      Array.sub mat start size)

let split_int_job numworkers job =
  let { aint; bint; opi } = job in
  let a_chunks = split_matrix aint numworkers in
  let b_chunks = split_matrix bint numworkers in
  IntJobSplit
    (Array.init numworkers (fun i ->
         { aint = a_chunks.(i); bint = b_chunks.(i); opi }))

let split_float_job numworkers job =
  let { afloat; bfloat; opf } = job in
  let a_chunks = split_matrix afloat numworkers in
  let b_chunks = split_matrix bfloat numworkers in
  FloatJobSplit
    (Array.init numworkers (fun i ->
         { afloat = a_chunks.(i); bfloat = b_chunks.(i); opf }))

let dispatch_job split =
  let workers = List.of_seq (Hashtbl.to_seq users) in
  match split with
  | IntJobSplit a ->
      let chunks = Array.to_list a in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun (chunk, (key, (username, client_in, client_out, _))) ->
          let%lwt () = Lwt_io.fprintl client_out "job" in
          let%lwt () = Lwt_io.fprintl client_out "int" in
          let%lwt () = Lwt_io.fprintlf client_out "%s" chunk.opi in
          let%lwt () = print_matrix (IntMatrix chunk.aint) client_out in
          print_matrix (IntMatrix chunk.bint) client_out)
        work_pairs
  | FloatJobSplit a ->
      let chunks = Array.to_list a in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun (chunk, (key, (username, client_in, client_out, _))) ->
          let%lwt () = Lwt_io.fprintl client_out "job" in
          let%lwt () = Lwt_io.fprintl client_out "int" in
          let%lwt () = Lwt_io.fprintlf client_out "%s" chunk.opf in
          let%lwt () = print_matrix (FloatMatrix chunk.afloat) client_out in
          print_matrix (FloatMatrix chunk.bfloat) client_out)
        work_pairs

let client_handler client_socket_address (client_in, client_out) =
  let key = sock_addr_to_string client_socket_address in

  (*client should automatically send their node type as the first msg*)
  let%lwt node_type_opt = Lwt_io.read_line_opt client_in in
  match node_type_opt with
  | None ->
      let%lwt () =
        Lwt_io.printlf "Client %s disconnected before sending node type." key
      in
      Lwt.return_unit
  | Some node_type -> (
      match node_type with
      | "CLIENT" ->
          let%lwt () = Lwt_io.printlf "CLIENT node connected: %s" key in
          let%lwt () =
            head_props := Some (key, client_in, client_out);
            Lwt.return_unit
          in
          let rec handle_jobs () =
            let%lwt job_opt = Lwt_io.read_line_opt client_in in
            match job_opt with
            | None ->
                let%lwt () =
                  Lwt_io.printlf "CLIENT node %s disconnected." key
                in
                Lwt.return_unit
            | Some "job" ->
                let%lwt () = Lwt_io.printl "Received job from client" in
                let%lwt job = process_job client_in in
                let split =
                  match job with
                  | IntJob i -> split_int_job (Hashtbl.length users) i
                  | FloatJob j -> split_float_job (Hashtbl.length users) j
                in
                let%lwt () = dispatch_job split in

                handle_jobs ()
            | Some _ -> failwith "not implemented"
          in
          handle_jobs ()
      | instanceName ->
          let username = instanceName in
          let%lwt () =
            Hashtbl.replace users key (username, client_in, client_out, true);
            Lwt.return_unit
          in
          let%lwt () = Lwt_io.printlf "%s (%S) connected." key username in

          let rec handle_status () =
            let%lwt status = Lwt_io.read_line_opt client_in in
            match status with
            | None ->
                let%lwt () =
                  Lwt_io.printlf "%s (%s) disconnected." key username
                in
                Hashtbl.remove users key;
                Lwt.return_unit
            | Some status -> (
                let%lwt () =
                  Lwt_io.printlf "%s (%s): Status Update: %S" key username
                    status
                in

                (*statuses are formatted as follows: <AVAILABLE> <JOB>
                  <OUTPUT>*)
                match parse_status_string status with
                | None ->
                    let%lwt () =
                      Lwt_io.printlf "%s (%s): Malformed status string: %s" key
                        username status
                    in
                    handle_status ()
                | Some ("AVAILABLE", job, output) ->
                    Hashtbl.replace users key
                      (username, client_in, client_out, true);
                    Hashtbl.replace job_table job
                      (true, Some username, Some output);
                    handle_status ()
                | _ -> handle_status ())
          in
          let rec send_jobs () =
            match Queue.is_empty jobs with
            | true ->
                let%lwt () = Lwt_unix.sleep 1.0 in
                send_jobs ()
            | false -> (
                match find_available_worker () with
                | None ->
                    let%lwt () = Lwt_unix.sleep 1.0 in
                    send_jobs ()
                | Some (worker_key, worker_name, worker_in, worker_out) ->
                    let job = Queue.pop jobs in
                    let%lwt () =
                      Lwt_io.printlf "Assigning job %s to worker %s (%s)" job
                        worker_name worker_key
                    in
                    let%lwt () =
                      Lwt.return
                        (Hashtbl.replace job_table job
                           (false, Some worker_name, None))
                    in
                    let%lwt () = Lwt_io.write_line worker_out job in
                    let%lwt () = Lwt_io.flush worker_out in
                    Hashtbl.replace users worker_key
                      (worker_name, worker_in, worker_out, false);
                    send_jobs ())
          in
          Lwt.choose [ handle_status (); send_jobs () ])

let run_server ipaddr port =
  let server () =
    let%lwt () = Lwt_io.printlf "I am the server." in
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address
        (ADDR_INET (Unix.inet_addr_of_string ipaddr, port))
        client_handler
    in
    let never_resolved, _ = Lwt.wait () in
    never_resolved
  in
  Lwt_main.run (server ())

let _ =
  let print_usage () =
    Printf.printf "Usage:\n %s <server | client>\n" Sys.argv.(0)
  in
  if Array.length Sys.argv < 3 then print_usage ()
  else
    let ipaddr = Sys.argv.(1) in
    let port =
      match int_of_string_opt Sys.argv.(2) with
      | Some p -> p
      | None ->
          Printf.printf "Invalid port number: %s\n" Sys.argv.(2);
          exit 1
    in

    print_endline ("Using IP: " ^ ipaddr ^ " Port: " ^ string_of_int port);
    run_server ipaddr port
(* | "head" -> if Array.length Sys.argv < 4 then print_usage () else run_head
   ipaddr port | "worker" -> if Array.length Sys.argv < 5 then print_usage ()
   else let instanceName = Sys.argv.(4) in print_endline ("Worker Name: " ^
   instanceName); run_client ipaddr port instanceName | _ -> print_usage () *)

(* | Some "view_jobs" -> let%lwt () = Lwt_io.fprintlf client_out "Current Job
   Table" in let entries = Hashtbl.fold (fun job (completed, worker, output) acc
   -> let status_str = if completed then "COMPLETED" else "PENDING" in let
   worker_str = match worker with | Some w -> w | None -> "N/A" in let
   output_str = match output with | Some o -> o | None -> "N/A" in
   Printf.sprintf "Job: %s | Status: %s | Worker: %s | Output: %s" job
   status_str worker_str output_str :: acc) job_table [] in let%lwt () =
   Lwt_list.iter_s (fun line -> Lwt_io.fprintlf client_out "%s" line) (List.rev
   entries) in let%lwt () = Lwt_io.fprintlf client_out "END_OF_JOBS" in
   handle_jobs ()*)
