open Slurmcaml.Matrixutils
(* too many workers and worker quits mid execution*)

(** Tracks all the workers currently connected to the server *)
let worker_table = Hashtbl.create 10

(** Keeps track of the client's channel *)
let head_props = ref Lwt_io.null

(** Track if some worker disconnected mid job*)
let flag = ref false

(** Tracks the job type, the worker assignments, the result matrix, and the
    number of workers who have finished their work *)
let assignment_table =
  ref (IntJobASMType, Hashtbl.create 10, IntMatrix [| [| 0 |] |], 0)

(** Converts a socked address to string *)
let sock_addr_to_string (add : Unix.sockaddr) =
  match add with
  | ADDR_INET (a, port) -> Unix.string_of_inet_addr a ^ ":" ^ string_of_int port
  | _ -> failwith "unsupported"

(**[print_headers client_out provided_op jtype] prints a header to a worker with
   the job type and operator. In the front end because it requires I/O. *)
let print_headers client_out provided_op jtype =
  let%lwt () = Lwt_io.fprintl client_out "job" in
  let%lwt () = Lwt_io.fprintl client_out jtype in
  Lwt_io.fprintl client_out provided_op

(**[dispatch_job split] takes a job split, and transmits each worker its portion
   of the work, along with information about what kind of job it is. This
   function is in the front end because it requires directly producing input and
   output to the workers.*)
let dispatch_job split =
  let determine_workers chunks =
    if
      List.length (List.of_seq (Hashtbl.to_seq worker_table))
      > List.length chunks
    then
      List.take (List.length chunks) (List.of_seq (Hashtbl.to_seq worker_table))
    else List.of_seq (Hashtbl.to_seq worker_table)
  in
  match split with
  | IntJobASMSplit a ->
      let chunks = Array.to_list a in
      let workers = determine_workers chunks in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun (chunk, (key, (username, client_in, client_out, _))) ->
          let%lwt () = print_headers client_out chunk.opi "int" in
          let%lwt () = print_matrix (IntMatrix chunk.aint) client_out in
          print_matrix (IntMatrix chunk.bint) client_out)
        work_pairs
  | FloatJobASMSplit a ->
      let chunks = Array.to_list a in
      let workers = determine_workers chunks in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun (chunk, (key, (username, client_in, client_out, _))) ->
          let%lwt () = print_headers client_out chunk.opf "float" in
          let%lwt () = print_matrix (FloatMatrix chunk.afloat) client_out in
          print_matrix (FloatMatrix chunk.bfloat) client_out)
        work_pairs
  | IntJobSSplit a ->
      let chunks = Array.to_list a in
      let workers = determine_workers chunks in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun ((chunk : int_job_s), (key, (username, client_in, client_out, _)))
           ->
          let%lwt () = print_headers client_out "scale" "int" in
          let%lwt () = Lwt_io.fprintlf client_out "%d" chunk.scalar in
          print_matrix (IntMatrix chunk.aint) client_out)
        work_pairs
  | FloatJobSSplit a ->
      let chunks = Array.to_list a in
      let workers = determine_workers chunks in
      let work_pairs = List.combine chunks workers in
      Lwt_list.iter_s
        (fun (chunk, (key, (username, client_in, client_out, _))) ->
          let%lwt () = print_headers client_out "scale" "float" in
          let%lwt () = Lwt_io.fprintlf client_out "%f" chunk.scalar in
          print_matrix (FloatMatrix chunk.afloat) client_out)
        work_pairs

(** [process_job in_channel] processes a job sent from the client on
    [in_channel], producing a job as the result with the one or two matrices and
    operator. This function is in the front end because it requires processing
    directly from the in_channel. *)
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

(**[handle_jobs_from_client client_in client_out key] waits for the client to
   send a job, and when they do, calls [process_job] to turn it into a job,
   splits the job based on its type, and then dispatches it to the workers. This
   function is in the front end because it requires parsing input from the
   client with Lwt.*)
let rec handle_jobs_from_client client_in client_out key =
  let%lwt job_opt = Lwt_io.read_line_opt client_in in
  match job_opt with
  | None ->
      let%lwt () = Lwt_io.printlf "CLIENT node %s disconnected." key in
      head_props := Lwt_io.null;
      Lwt.return_unit
  | Some "job" ->
      let%lwt () = Lwt_io.printlf "Received job from %s" key in
      let () = flag := false in
      let%lwt job = process_job client_in in
      let%lwt () = Lwt_io.printl "processed job" in
      if Hashtbl.length worker_table = 0 then
        let%lwt () =
          Lwt_io.printl "ERROR: There are no worker nodes available"
        in
        let%lwt () =
          Lwt_io.fprintl client_out
            "There are no worker nodes available, please try again later"
        in
        handle_jobs_from_client client_in client_out key
      else
        let () = assignment_table := determine_assignments job worker_table in
        let split =
          match job with
          | IntJobASM i -> split_int_job_asm (Hashtbl.length worker_table) i
          | FloatJobASM j -> split_float_job_asm (Hashtbl.length worker_table) j
          | IntJobS i -> split_int_job_s (Hashtbl.length worker_table) i
          | FloatJobS j -> split_float_job_s (Hashtbl.length worker_table) j
        in
        let%lwt () = dispatch_job split in
        handle_jobs_from_client client_in client_out key
  | Some command ->
      let%lwt () = Lwt_io.printlf "Received job from %s\n" key in
      let%lwt () =
        Lwt_io.printlf "ERROR: '%s' is not a valid command" command
      in
      handle_jobs_from_client client_in client_out key

(**[handle_int_ASM_job tbl work counter client_in_key] processes a partial
   result from a worker in a int add, subtract, or multiply operation and
   updates the assignment table with the new number of workers that have
   finishes and the new work matrix. If the worker is the last worker, it prints
   the result to the client. This function is in the front end because it
   requires parsing input from a worker and printing to client.*)
let handle_int_ASM_job tbl work counter client_in key =
  let%lwt res = read_int_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_int res portion work in
  assignment_table := (IntJobASMType, tbl, IntMatrix work, counter - 1);
  if not !flag then
    if counter - 1 = 0 then
      let%lwt () = Lwt_io.fprintl !head_props "INT_JOB_OUTPUT" in
      let%lwt () = print_matrix (IntMatrix work) Lwt_io.stdout in
      print_matrix (IntMatrix work) !head_props
    else Lwt.return_unit
  else Lwt.return ()

(**[handle_float_ASM_job tbl work counter client_in key] processes a partial
   result from a worker in a float add, subtract, or multiply operation and
   updates the assignment table with the new number of workers that have
   finishes and the new work matrix. If the worker is the last worker, it prints
   the result to the client. This function is in the front end because it
   requires parsing input from a worker and printing to client.*)
let handle_float_ASM_job tbl work counter client_in key =
  let%lwt res = read_float_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_float res portion work in
  assignment_table := (FloatJobASMType, tbl, FloatMatrix work, counter - 1);
  if not !flag then
    if counter - 1 = 0 then
      let%lwt () = Lwt_io.fprintl !head_props "FLOAT_JOB_OUTPUT" in
      let%lwt () = print_matrix (FloatMatrix work) Lwt_io.stdout in
      print_matrix (FloatMatrix work) !head_props
    else Lwt.return_unit
  else Lwt.return ()

(**[handle_int_job_s tbl work counter client_in key] processes a partial result
   from a worker in a int scale operation and updates the assignment table with
   the new number of workers that have finishes and the new work matrix. If the
   worker is the last worker, it prints the result to the client. This function
   is in the front end because it requires parsing input from a worker and
   printing to client.*)
let handle_int_job_s tbl work counter client_in key =
  let%lwt res = read_int_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_int res portion work in
  assignment_table := (IntJobSType, tbl, IntMatrix work, counter - 1);
  if not !flag then
    if counter - 1 = 0 then
      let%lwt () = Lwt_io.fprintl !head_props "INT_JOB_OUTPUT" in
      let%lwt () = print_matrix (IntMatrix work) Lwt_io.stdout in
      print_matrix (IntMatrix work) !head_props
    else Lwt.return_unit
  else Lwt.return ()

(**[handle_float_job_s tbl work counter client_in key] processes a partial
   result from a worker in a float scale operation and updates the assignment
   table with the new number of workers that have finishes and the new work
   matrix. If the worker is the last worker, it prints the result to the client.
   This function is in the front end because it requires parsing input from a
   worker and printing to client.*)
let handle_float_job_s tbl work counter client_in key =
  let%lwt res = read_float_matrix_input client_in in
  let portion = Hashtbl.find tbl key in
  let () = fill_matrix_float res portion work in
  assignment_table := (FloatJobSType, tbl, FloatMatrix work, counter - 1);
  if not !flag then
    if counter - 1 = 0 then
      let%lwt () = Lwt_io.fprintl !head_props "FLOAT_JOB_OUTPUT" in
      let%lwt () = print_matrix (FloatMatrix work) Lwt_io.stdout in
      print_matrix (FloatMatrix work) !head_props
    else Lwt.return_unit
  else Lwt.return ()

(**[handle_failure client_in] handles a worker reporting a failure in an
   operation. This function is in the front end because it requires interfacing
   with a worker's channel and the client's channel.*)
let handle_failure failure username = Lwt_io.fprintl !head_props failure

(**[handle_none key username] handles a worker disconnecting. This function is
   in the front end because it requires printing to output.*)
let handle_none key username =
  let%lwt () = Lwt_io.printlf "%s (%S) disconnected." key username in
  Hashtbl.remove worker_table key;
  Lwt.return_unit

(**[handle_worker_connection instanceName key client_in client_out] handles a
   worker with name [instanceName] connecting to the server and reporting
   results from its work portion. This function is in the front end because it
   requires handling server networking and reading input from a channel.*)
let handle_worker_connection instanceName key client_in client_out =
  let username = instanceName in
  let%lwt () =
    Hashtbl.replace worker_table key (username, client_in, client_out, true);
    Lwt.return_unit
  in
  let%lwt () = Lwt_io.printlf "%s (%S) connected." key username in
  let rec handle_status () =
    let%lwt status = Lwt_io.read_line_opt client_in in
    match status with
    | None ->
        let%lwt () = handle_none key username in
        flag := true;
        handle_failure
          "Some worker disconnected. If you were mid job, your job failed."
          username
    | Some "Failure" ->
        let%lwt failure = Lwt_io.read_line client_in in
        let%lwt () = handle_failure failure username in
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

(**[client_handler client_socket_address (client_in, client_out)] handles a node
   connecting to the server, and dispatches to [handle_worker_connection] and
   [handle_jobs_from_client] depending on the type of the node. This function is
   in the front end because it requires interfacing with networking between
   nodes.*)
let client_handler client_socket_address (client_in, client_out) =
  let key = sock_addr_to_string client_socket_address in
  let%lwt node_type_opt = Lwt_io.read_line_opt client_in in
  match node_type_opt with
  | None ->
      let%lwt () =
        Lwt_io.printlf "Node %s disconnected before sending node type." key
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

(**[run_server ipaddr port] starts a server on [ipaddr] with [port]. This
   function is in the front end because it requires starting a server.*)
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

(**Program entry point*)
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
