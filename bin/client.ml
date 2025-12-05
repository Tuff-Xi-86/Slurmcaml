open Slurmcaml.Matrixutils

exception InvalidMatrixArgument of string

let start_time = ref (Unix.gettimeofday ())
let end_time = ref (Unix.gettimeofday ())

(** [send_matrix matrix_type matrix_op optional_scale matrix_path_one
     optional_matrix_two out_channel] Generalized function that handles both
    unary and binary operations.
    - Unary ops call: send_matrix t op scale (Some path1) None
    - Binary ops call: send_matrix t op None (Some path1) (Some path2). This
      function is in the front end because it requires input and output with Lwt
      to the server.*)
let send_matrix matrix_type matrix_op optional_scale matrix_path_one
    optional_matrix_two out_channel =
  let read_matrix path =
    match matrix_type with
    | "int" -> read_int_matrix_from_file path
    | "float" -> read_float_matrix_from_file path
    | _ -> failwith "Invalid matrix_type in send_matrix"
  in
  let matrix_one = read_matrix matrix_path_one in
  let matrix_two =
    match optional_matrix_two with
    | None -> None
    | Some p -> Some (read_matrix p)
  in
  let%lwt () = Lwt_io.fprintl out_channel "job" in
  let%lwt () = Lwt_io.fprintl out_channel matrix_type in
  let%lwt () = Lwt_io.fprintl out_channel matrix_op in
  let%lwt () =
    match optional_scale with
    | None -> Lwt.return_unit
    | Some scalar -> Lwt_io.fprintl out_channel scalar
  in
  let%lwt () = print_matrix matrix_one out_channel in
  match matrix_two with
  | None -> Lwt.return_unit
  | Some m -> print_matrix m out_channel

(** [process_job out_channel job] parses [job], a raw job string (e.g., "int add
    mat1.csv mat2.csv"), validates that the specified files exist and are .csv
    files, and dispatches the task to either [send_one_matrix] or
    [send_two_matrix] depending on the operation. Raises Failure if file paths
    are invalid or operations are unknown. This function is in the front end
    because it requires writing output to the user, and non-output functionality
    is abstracted away.*)
let process_job out_channel job =
  let rest_string = ref (String.lowercase_ascii job) in
  let matrix_type = parse_matrix_type rest_string in
  let matrix_op = process_op rest_string in
  let first_matrix_path = parse_path rest_string in
  if matrix_op = "scale" then
    let scale_opt = parse_scale rest_string in
    if matrix_type = "int" then
      let () = check_scalar_int scale_opt in
      let%lwt () = Lwt_io.printlf "Sending job: %s" job in
      send_matrix matrix_type matrix_op (Some scale_opt) first_matrix_path None
        out_channel
    else
      let () = check_scalar_float scale_opt in
      let%lwt () = Lwt_io.printlf "Sending job: %s" job in
      send_matrix matrix_type matrix_op (Some scale_opt) first_matrix_path None
        out_channel
  else
    let second_matrix_path = parse_path rest_string in
    let%lwt () = Lwt_io.printlf "Sending job: %s" job in
    send_matrix matrix_type matrix_op None first_matrix_path
      (Some second_matrix_path) out_channel

(**[send_job server_out] reads a job from the client, processes it with
   [process_job], and sends the job to the server, handling any errors and
   printing a helpful message to the user. This function is in the front end
   because it requires writing to the server and to the user*)
let rec send_job server_out =
  match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
  | Some job ->
      let%lwt () = Lwt_io.printlf "Attempting to send job: %s" job in
      let%lwt () =
        try process_job server_out job with
        | InvalidMatrixArgument x ->
            let%lwt () = Lwt_io.printl ("Job Failed: " ^ x) in
            let%lwt () =
              Lwt_io.printl
                "Usage: <int, float> Matrix_Operation Matrix_CSV_Path_One \
                 Optional_Matrix_CSV_Path_Two"
            in
            Lwt.return_unit
        | MatrixNotReadable ->
            let%lwt () =
              Lwt_io.printl
                "One of the provided matrices could not be parsed. Please make \
                 sure it is a CSV file with only numbers."
            in
            Lwt.return_unit
      in
      let%lwt () = Lwt_io.flush server_out in
      start_time := Unix.gettimeofday ();
      send_job server_out
  | None -> Lwt_io.printl "Input closed (EOF). Stopping sends."

(**[receive_responses server_in] reads a response from server_in, and prints the
   result to the user. This function is in the front end because it requires
   reading from the server's channel and printing to the user.*)
let rec receive_responses server_in =
  let%lwt response_opt = Lwt_io.read_line_opt server_in in
  match response_opt with
  | Some "Failure" ->
      let%lwt failtype = Lwt_io.read_line server_in in
      let%lwt () = Lwt_io.print "Job failed. Reason: " in
      let%lwt () = Lwt_io.printl failtype in
      receive_responses server_in
  | Some "INT_JOB_OUTPUT" ->
      let%lwt () = Lwt_io.printlf "int job output got" in
      let%lwt matrix = read_int_matrix_input server_in in
      let int_mat_res = IntMatrix matrix in
      let%lwt () = Lwt_io.printlf "Received Result Matrix: " in
      let%lwt () = print_matrix int_mat_res Lwt_io.stdout in
      end_time := Unix.gettimeofday ();
      let%lwt () =
        Lwt_io.printlf "Job Time: %f seconds" (!end_time -. !start_time)
      in
      receive_responses server_in
  | Some "FLOAT_JOB_OUTPUT" ->
      let%lwt () = Lwt_io.printlf "float job output got" in
      let%lwt matrix = read_float_matrix_input server_in in
      let float_mat_res = FloatMatrix matrix in
      let%lwt () = Lwt_io.printlf "Received Result Matrix: " in
      let%lwt () = print_matrix float_mat_res Lwt_io.stdout in
      end_time := Unix.gettimeofday ();
      let%lwt () =
        Lwt_io.printlf "Job Time: %f seconds" (!end_time -. !start_time)
      in
      receive_responses server_in
  | Some "END_OF_JOBS" ->
      let%lwt () = Lwt_io.printlf "End of job list." in
      receive_responses server_in
  | Some other ->
      let%lwt () = Lwt_io.printlf "Job Status: \n %s" other in
      receive_responses server_in
  | None ->
      print_endline "Server Closed";
      Lwt.return_unit

(**[client_loop server_in server_out] loops to receive responses or send jobs.
   This is in the front end because it requires Lwt.*)
let client_loop server_in server_out =
  Lwt.choose [ send_job server_out; receive_responses server_in ]

(**[run_client ipaddr port] starts a client on [ipaddr] with [port]. This is in
   the front end because it requires networking.*)
let run_client ipaddr port =
  let head () =
    try%lwt
      let%lwt server_in, server_out =
        try%lwt
          Lwt_io.open_connection
            (ADDR_INET (Unix.inet_addr_of_string ipaddr, port))
        with exn ->
          let%lwt () =
            Lwt_io.printl
              (Printf.sprintf "Failed to connect to server at %s:%d" ipaddr port)
          in
          exit 0
      in
      let%lwt () = Lwt_io.printlf "Connected to Server as CLIENT" in
      let%lwt () = Lwt_io.fprintlf server_out "CLIENT" in
      let%lwt () = Lwt_io.flush server_out in
      client_loop server_in server_out
    with exn ->
      print_endline "Client Error";
      exit 0
  in
  Lwt_main.run (head ())

(** Program entry point*)
let _ =
  let print_usage () =
    Printf.printf "Usage: %s Server_IP Server_port\n" Sys.argv.(0)
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
    run_client ipaddr port
