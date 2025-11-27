open Slurmcaml.Matrixutils

let users = Hashtbl.create 10
let head_props = ref Lwt_io.stdout
let jobs = Queue.create ()

(* ignoring this for now*)
let job_table = Hashtbl.create 10

type currentJobType =
  | IntJobType
  | FloatJobType

(* this is tracking the job type, the worker assignments, the result matrix, and
   the number of workers who have finished their work*)
let assignment_table =
  ref (IntJobType, Hashtbl.create 10, IntMatrix [| [| 0 |] |], 0)

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
      let%lwt () = Lwt_io.printf "mat2: %d rows\n%!" (Array.length mat2) in
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

let determine_assignments job =
  match job with
  | IntJob { aint; bint; opi } ->
      let mat = aint in
      let workers = List.of_seq (Hashtbl.to_seq users) in
      let num = List.length workers in
      let n = Array.length mat in
      let base = n / num in
      let extra = n mod num in
      let worker_assignments = Hashtbl.create 10 in
      List.iteri
        (fun i (sock, (username, _, _, _)) ->
          let size = base + if i < extra then 1 else 0 in
          let start = (i * base) + min i extra in
          Hashtbl.replace worker_assignments sock (start, start + size))
        workers;
      ( IntJobType,
        worker_assignments,
        construct_int_matrix (Array.length aint) (Array.length aint.(0)),
        0 )
  | FloatJob { afloat; bfloat; opf } ->
      let mat = afloat in
      let workers = List.of_seq (Hashtbl.to_seq users) in
      let num = List.length workers in
      let n = Array.length mat in
      let base = n / num in
      let extra = n mod num in
      let worker_assignments = Hashtbl.create 10 in
      List.iteri
        (fun i (sock, (username, _, _, _)) ->
          let size = base + if i < extra then 1 else 0 in
          let start = (i * base) + min i extra in
          Hashtbl.replace worker_assignments sock (start, start + size))
        workers;
      ( FloatJobType,
        worker_assignments,
        construct_float_matrix (Array.length afloat) (Array.length afloat.(0)),
        0 )

(*Portion is tuple, first row - last row(Non inclusive), Res is result. Need
  function that fills in matrix given the result matrix and portion, Updates
  assignment_table*)
let fill_matrix_int (res : int array array) ((first_row, last_row) : 'a * 'b)
    (old_array : int array array) =
  print_endline "entering fill matrix";
  print_int first_row;
  print_int last_row;
  Array.iteri
    (fun num_row matrix_row -> old_array.(num_row + first_row) <- matrix_row)
    res

let fill_matrix_float (res : float array array)
    ((first_row, last_row) : 'a * 'b) (old_array : float array array) =
  Array.iteri
    (fun num_row matrix_row -> old_array.(num_row + first_row) <- matrix_row)
    res

(*[[1,2,3][4,5,6]] *)
(*123
  456*)
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
            head_props := client_out;
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
                let%lwt () = Lwt_io.printlf "Received job from %s" key in
                let%lwt job = process_job client_in in
                if Hashtbl.length users = 0 then
                  let%lwt () =
                    Lwt_io.printl "ERROR: There are no worker nodes available"
                  in
                  let%lwt () =
                    Lwt_io.fprintl client_out
                      "There are no worker nodes available, please try again  \
                       later"
                  in
                  handle_jobs ()
                else
                  let () = assignment_table := determine_assignments job in
                  let split =
                    match job with
                    | IntJob i -> split_int_job (Hashtbl.length users) i
                    | FloatJob j -> split_float_job (Hashtbl.length users) j
                  in
                  let%lwt () = dispatch_job split in
                  (* determine assignments and then update the table with jobid,
                     assignments *)
                  handle_jobs ()
            | Some command ->
                let%lwt () = Lwt_io.printlf "Received job from %s\n" key in
                let%lwt () =
                  Lwt_io.printlf "ERROR: '%s' is not a valid command" command
                in
                handle_jobs ()
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
                match !assignment_table with
                | IntJobType, tbl, IntMatrix work, counter ->
                    let%lwt res = read_int_matrix_input client_in in
                    let portion = Hashtbl.find tbl key in
                    let () = fill_matrix_int res portion work in
                    assignment_table :=
                      (IntJobType, tbl, IntMatrix work, counter + 1);
                    let%lwt () =
                      if counter + 1 = Hashtbl.length users then
                        print_matrix (IntMatrix work) Lwt_io.stdout
                      (*!head_props*) else Lwt.return ()
                    in
                    handle_status () (* print to client*)
                | FloatJobType, tbl, FloatMatrix work, counter ->
                    let%lwt res = read_float_matrix_input client_in in
                    let portion = Hashtbl.find tbl key in
                    let () = fill_matrix_float res portion work in
                    assignment_table :=
                      (FloatJobType, tbl, FloatMatrix work, counter + 1);
                    let%lwt () =
                      if counter + 1 = Hashtbl.length users then
                        print_matrix (FloatMatrix work) Lwt_io.stdout
                      (*!head_props *) else Lwt.return ()
                    in
                    handle_status () (*print to client *)
                | _ -> Lwt.fail_with "this should not happen :(")
          in
          handle_status ())

(*Portion is tuple, first row - last row(Non inclusive), Res is result. Need
  function that fills in matrix given the result matrix and portion*)
let run_server ipaddr port =
  let server () =
    let%lwt () = Lwt_io.printlf "Starting Server..." in
    let%lwt running_server =
      try%lwt
        Lwt_io.establish_server_with_client_address
          (ADDR_INET (Unix.inet_addr_of_string ipaddr, port))
          client_handler
      with exn ->
        let%lwt () =
          Lwt_io.printl
            (Printf.sprintf "Failed to connect to server at %s:%d" ipaddr port)
        in
        exit 0
    in
    let%lwt () = Lwt_io.printlf "Server Started!" in
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

(* let rec send_jobs () = match Queue.is_empty jobs with | true -> let%lwt () =
   Lwt_unix.sleep 1.0 in send_jobs () | false -> ( match find_available_worker
   () with | None -> let%lwt () = Lwt_unix.sleep 1.0 in send_jobs () | Some
   (worker_key, worker_name, worker_in, worker_out) -> let job = Queue.pop jobs
   in let%lwt () = Lwt_io.printlf "Assigning job %s to worker %s (%s)" job
   worker_name worker_key in let%lwt () = Lwt.return (Hashtbl.replace job_table
   job (false, Some worker_name, None)) in let%lwt () = Lwt_io.write_line
   worker_out job in let%lwt () = Lwt_io.flush worker_out in Hashtbl.replace
   users worker_key (worker_name, worker_in, worker_out, false); send_jobs ())
   in Lwt.choose [ handle_status (); send_jobs () ]) *)
(* (
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

                    (* read from worker — last worker will do the compilation
                       work*)

                    (* start, end, *)
                    handle_status ()
                | _ -> handle_status ()) 
)*)
