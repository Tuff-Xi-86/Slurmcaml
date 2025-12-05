open Slurmcaml.Matrixutils

let users = Hashtbl.create 10
let head_props = ref Lwt_io.stdout
let jobs = Queue.create ()

(* this is tracking the job type, the worker assignments, the result matrix, and
   the number of workers who have finished their work *)
let assignment_table =
  ref (IntJobASMType, Hashtbl.create 10, IntMatrix [| [| 0 |] |], 0)

let sock_addr_to_string (add : Unix.sockaddr) =
  match add with
  | ADDR_INET (a, port) -> Unix.string_of_inet_addr a ^ ":" ^ string_of_int port
  | _ -> failwith "unsupported"

(*Portion is tuple, first row - last row(Non inclusive), Res is result. Need
  function that fills in matrix given the result matrix and portion, Updates
  assignment_table*)
let dispatch_job split =
  let workers = List.of_seq (Hashtbl.to_seq users) in
  match split with
  | IntJobASMSplit a ->
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
  | FloatJobASMSplit a ->
      let chunks = Array.to_list a in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun (chunk, (key, (username, client_in, client_out, _))) ->
          let%lwt () = Lwt_io.fprintl client_out "job" in
          let%lwt () = Lwt_io.fprintl client_out "float" in
          let%lwt () = Lwt_io.fprintlf client_out "%s" chunk.opf in
          let%lwt () = print_matrix (FloatMatrix chunk.afloat) client_out in
          print_matrix (FloatMatrix chunk.bfloat) client_out)
        work_pairs
  | IntJobSSplit a ->
      let chunks = Array.to_list a in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun ((chunk : int_job_s), (key, (username, client_in, client_out, _)))
           ->
          let%lwt () = Lwt_io.fprintl client_out "job" in
          let%lwt () = Lwt_io.fprintl client_out "int" in
          let%lwt () = Lwt_io.fprintlf client_out "scale" in
          let%lwt () = Lwt_io.fprintlf client_out "%d" chunk.scalar in
          print_matrix (IntMatrix chunk.aint) client_out)
        work_pairs
  | FloatJobSSplit a ->
      let chunks = Array.to_list a in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun ((chunk : float_job_s), (key, (username, client_in, client_out, _)))
           ->
          let%lwt () = Lwt_io.fprintl client_out "job" in
          let%lwt () = Lwt_io.fprintl client_out "float" in
          let%lwt () = Lwt_io.fprintlf client_out "scale" in
          let%lwt () = Lwt_io.fprintlf client_out "%f" chunk.scalar in
          print_matrix (FloatMatrix chunk.afloat) client_out)
        work_pairs

let process_job in_channel =
  let%lwt valuetype = Lwt_io.read_line in_channel in
  let%lwt op = Lwt_io.read_line in_channel in
  match valuetype with
  | "int" -> (
      match op with
      | "add" | "subtract" | "multiply" ->
          let%lwt mat1 = read_int_matrix_input in_channel in
          let%lwt () = Lwt_io.printf "mat1: %d rows\n%!" (Array.length mat1) in
          let%lwt mat2 = read_int_matrix_input in_channel in
          let%lwt () = Lwt_io.printf "mat2: %d rows\n%!" (Array.length mat2) in
          Lwt.return (IntJobASM { aint = mat1; bint = mat2; opi = op })
      | "scale" ->
          let%lwt scalar =
            Lwt.map int_of_string (Lwt_io.read_line in_channel)
          in
          let%lwt mat = read_int_matrix_input in_channel in
          let%lwt () = Lwt_io.printf "mat1: %d rows\n%!" (Array.length mat) in
          Lwt.return (IntJobS { aint = mat; scalar })
      | _ -> failwith "TODO Transpose")
  | "float" -> (
      match op with
      | "add" | "subtract" | "multiply" ->
          let%lwt mat1 = read_float_matrix_input in_channel in
          let%lwt mat2 = read_float_matrix_input in_channel in
          Lwt.return (FloatJobASM { afloat = mat1; bfloat = mat2; opf = op })
      | "scale" ->
          let%lwt scalar =
            Lwt.map float_of_string (Lwt_io.read_line in_channel)
          in
          let%lwt mat = read_float_matrix_input in_channel in
          let%lwt () = Lwt_io.printf "mat1: %d rows\n%!" (Array.length mat) in
          Lwt.return (FloatJobS { afloat = mat; scalar })
      | _ -> failwith "TODO Transpose")
  | _ -> failwith "not a supported type"

let rec handle_jobs_from_client client_in client_out key =
  let%lwt job_opt = Lwt_io.read_line_opt client_in in
  match job_opt with
  | None ->
      let%lwt () = Lwt_io.printlf "CLIENT node %s disconnected." key in
      Lwt.return_unit
  | Some "job" ->
      let%lwt () = Lwt_io.printlf "Received job from %s" key in
      let%lwt job = process_job client_in in
      let%lwt () = Lwt_io.printl "processed job" in
      if Hashtbl.length users = 0 then
        let%lwt () =
          Lwt_io.printl "ERROR: There are no worker nodes available"
        in
        let%lwt () =
          Lwt_io.fprintl client_out
            "There are no worker nodes available, please try again later"
        in
        handle_jobs_from_client client_in client_out key
      else
        let () = assignment_table := determine_assignments job users in
        let split =
          match job with
          | IntJobASM i -> split_int_job_asm (Hashtbl.length users) i
          | FloatJobASM j -> split_float_job_asm (Hashtbl.length users) j
          | IntJobS i -> split_int_job_s (Hashtbl.length users) i
          | FloatJobS j -> split_float_job_s (Hashtbl.length users) j
        in
        let%lwt () = dispatch_job split in
        handle_jobs_from_client client_in client_out key
  | Some command ->
      let%lwt () = Lwt_io.printlf "Received job from %s\n" key in
      let%lwt () =
        Lwt_io.printlf "ERROR: '%s' is not a valid command" command
      in
      handle_jobs_from_client client_in client_out key

let handle_int_ASM_job tbl work counter client_in key =
  let%lwt res = read_int_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_int res portion work in
  assignment_table := (IntJobASMType, tbl, IntMatrix work, counter + 1);
  if counter + 1 = Hashtbl.length users then
    let%lwt () = Lwt_io.fprintl !head_props "INT_JOB_OUTPUT" in
    let%lwt () = print_matrix (IntMatrix work) Lwt_io.stdout in
    print_matrix (IntMatrix work) !head_props
  else Lwt.return ()

let handle_float_ASM_job tbl work counter client_in key =
  let%lwt res = read_float_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_float res portion work in
  assignment_table := (FloatJobASMType, tbl, FloatMatrix work, counter + 1);
  if counter + 1 = Hashtbl.length users then
    let%lwt () = Lwt_io.fprintl !head_props "FLOAT_JOB_OUTPUT" in
    let%lwt () = print_matrix (FloatMatrix work) Lwt_io.stdout in
    print_matrix (FloatMatrix work) !head_props
  else Lwt.return ()

let handle_int_job_s tbl work counter client_in key =
  let%lwt res = read_int_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_int res portion work in
  assignment_table := (IntJobSType, tbl, IntMatrix work, counter + 1);
  if counter + 1 = Hashtbl.length users then
    let%lwt () = Lwt_io.fprintl !head_props "INT_JOB_OUTPUT" in
    let%lwt () = print_matrix (IntMatrix work) Lwt_io.stdout in
    print_matrix (IntMatrix work) !head_props
  else Lwt.return ()

let handle_float_job_s tbl work counter client_in key =
  let%lwt res = read_float_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_float res portion work in
  assignment_table := (FloatJobSType, tbl, FloatMatrix work, counter + 1);
  if counter + 1 = Hashtbl.length users then
    let%lwt () = Lwt_io.fprintl !head_props "FLOAT_JOB_OUTPUT" in
    let%lwt () = print_matrix (FloatMatrix work) Lwt_io.stdout in
    print_matrix (FloatMatrix work) !head_props
  else Lwt.return ()

let handle_failure client_in =
  let%lwt failure = Lwt_io.read_line client_in in
  let%lwt () = Lwt_io.fprintl !head_props "Failure" in
  Lwt_io.fprintl !head_props failure

let handle_none key username =
  let%lwt () = Lwt_io.printlf "%s (%S) disconnected." key username in
  Lwt.return_unit

let handle_worker_connection instanceName key client_in client_out =
  let username = instanceName in
  let%lwt () =
    Hashtbl.replace users key (username, client_in, client_out, true);
    Lwt.return_unit
  in
  let%lwt () = Lwt_io.printlf "%s (%S) connected." key username in
  let rec handle_status () =
    let%lwt status = Lwt_io.read_line_opt client_in in
    match status with
    | None -> handle_none key username
    | Some "Failure" ->
        let%lwt () = handle_failure client_in in
        handle_status ()
    | Some status -> (
        match !assignment_table with
        | IntJobASMType, tbl, IntMatrix work, counter ->
            let%lwt () = handle_int_ASM_job tbl work counter client_in key in
            handle_status ()
        | FloatJobASMType, tbl, FloatMatrix work, counter ->
            let%lwt () = handle_float_ASM_job tbl work counter client_in key in
            handle_status ()
        | IntJobSType, tbl, IntMatrix work, counter ->
            let%lwt () = handle_int_job_s tbl work counter client_in key in
            handle_status ()
        | FloatJobSType, tbl, FloatMatrix work, counter ->
            let%lwt () = handle_float_job_s tbl work counter client_in key in
            handle_status ()
        | _ -> Lwt.fail_with "this should not happen :(")
  in
  handle_status ()

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
          handle_jobs_from_client client_in client_out key
      | instanceName ->
          handle_worker_connection instanceName key client_in client_out)

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
