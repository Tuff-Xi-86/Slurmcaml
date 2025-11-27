open Slurmcaml.Matrixutils

exception InvalidMatrixArgument of string

(* reads int matrix from a file and returns the matrix, # rows, # columns,
   valuetype*)
let read_int_matrix_from_file pathname =
  let csv = Csv.load pathname in
  let arrayrows =
    List.map (fun x -> Array.of_list (List.map int_of_string x)) csv
  in
  IntMatrix (Array.of_list arrayrows)

(* reads float matrix from a file, returns the matrix, # of rows, # of columsn,
   valuetype*)
let read_float_matrix_from_file pathname =
  let csv = Csv.load pathname in
  let arrayrows =
    List.map (fun x -> Array.of_list (List.map float_of_string x)) csv
  in
  FloatMatrix (Array.of_list arrayrows)

(** [send_one_matrix matrix_type matrix_opp matrix_path out_channel] Reads a
    single CSV file from [matrix_path], converts it to the specified
    [matrix_type] ("int" or "float"), and sends the operation metadata and
    matrix data to [out_channel]. Used for unary operations like "transpose" or
    "scale". *)
let send_one_matrix matrix_type matrix_opp optional_scale matrix_path
    out_channel =
  let matrix_list_rep =
    match matrix_type with
    | "int" -> read_int_matrix_from_file matrix_path
    | "float" -> read_float_matrix_from_file matrix_path
    | _ ->
        failwith
          "Precondition type error in send_one_matrix, this should be \
           impossibles"
  in
  let%lwt () = Lwt_io.fprintl out_channel "job" in
  let%lwt () = Lwt_io.fprintl out_channel matrix_type in
  let%lwt () = Lwt_io.fprintl out_channel matrix_opp in
  match optional_scale with
  | None -> print_matrix matrix_list_rep out_channel
  (*Type of scaler matchs that of matrix type*)
  | Some scaler ->
      let%lwt () = Lwt_io.fprintl out_channel scaler in
      print_matrix matrix_list_rep out_channel

(** [send_two_matrix matrix_type matrix_opp matrix_path_one matrix_path_two
     out_channel] Reads two CSV files from [matrix_path_one] and
    [matrix_path_two], converts them to the specified [matrix_type], and sends
    the operation metadata and both matrices to [out_channel]. Used for binary
    operations like "add", "subtract", or "multiply". *)
let send_two_matrix matrix_type matrix_opp matrix_path_one matrix_path_two
    out_channel =
  let matrix_one_list_rep =
    match matrix_type with
    | "int" ->
        IntMatrix
          (Array.map
             (fun row -> Array.map int_of_string row)
             (Csv.to_array (Csv.load matrix_path_one)))
    | "float" ->
        FloatMatrix
          (Array.map
             (fun row -> Array.map float_of_string row)
             (Csv.to_array (Csv.load matrix_path_one)))
    | _ ->
        failwith
          "Precondition type error in send_two_matrix, this should be \
           impossible"
  in
  let matrix_two_list_rep =
    match matrix_type with
    | "int" ->
        IntMatrix
          (Array.map
             (fun row -> Array.map int_of_string row)
             (Csv.to_array (Csv.load matrix_path_two)))
    | "float" ->
        FloatMatrix
          (Array.map
             (fun row -> Array.map float_of_string row)
             (Csv.to_array (Csv.load matrix_path_two)))
    | _ ->
        failwith
          "Precondition type error in send_two_matrix, this should be \
           impossible"
  in
  let%lwt () = Lwt_io.fprintl out_channel "job" in
  let%lwt () = Lwt_io.fprintl out_channel matrix_type in
  let%lwt () = Lwt_io.fprintl out_channel matrix_opp in
  let%lwt () = print_matrix matrix_one_list_rep out_channel in
  print_matrix matrix_two_list_rep out_channel

(** [split_first_space s] Locates the first space in string [s] and splits the
    string into a tuple: (word_before_space, rest_of_string). If no space is
    found, returns (s, ""). *)
let split_first_space s =
  match String.index_opt s ' ' with
  | Some i ->
      let first = String.sub s 0 i in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      (first, rest)
  | None -> (s, "")

(** [process_job out_channel job] Parses a raw job string (e.g., "int add
    mat1.csv mat2.csv"), validates that the specified files exist and are .csv
    files, and dispatches the task to either [send_one_matrix] or
    [send_two_matrix] depending on the operation. Raises Failure if file paths
    are invalid or operations are unknown. *)
let process_job out_channel job =
  let rest_string = ref (String.lowercase_ascii job) in
  let matrix_type =
    match split_first_space !rest_string with
    | "", x -> raise (InvalidMatrixArgument "Missing argument for matrix type")
    | "int", rest ->
        rest_string := rest;
        "int"
    | "float", rest ->
        rest_string := rest;
        "float"
    | _ ->
        raise
          (InvalidMatrixArgument "unknown matrix type: must be int or float")
  in
  let matrix_opp =
    match split_first_space !rest_string with
    | "", x ->
        raise (InvalidMatrixArgument "Missing argument for matrix opperation")
    | "add", rest ->
        rest_string := rest;
        "add"
    | "subtract", rest ->
        rest_string := rest;
        "subtract"
    | "multiply", rest ->
        rest_string := rest;
        "multiply"
    | "scale", rest ->
        rest_string := rest;
        "scale"
    | "transpose", rest ->
        rest_string := rest;
        "transpose"
    | _ -> raise (InvalidMatrixArgument "unknown matrix opperation")
  in
  let first_matrix_path =
    match split_first_space !rest_string with
    | "", x -> raise (InvalidMatrixArgument "Missing Path for first matrix")
    | file_path, rest ->
        if
          Sys.file_exists file_path
          && String.lowercase_ascii (Filename.extension file_path) = ".csv"
        then (
          rest_string := rest;
          file_path)
        else
          raise
            (InvalidMatrixArgument
               ("File path " ^ file_path
              ^ " does not exist or is not a .csv file"))
  in
  if matrix_opp = "transpose" then
    let%lwt () = Lwt_io.printlf "Sending job: %s" job in
    send_one_matrix matrix_type matrix_opp None first_matrix_path out_channel
  else if matrix_opp = "scale" then
    let scale_opt =
      match split_first_space !rest_string with
      | "", x ->
          raise (InvalidMatrixArgument "Missing argument for matrix scalar")
      | num, x -> num
    in
    (*We are going to need to check if its a float or int for now its NA*)
    if matrix_type = "int" then
      let () =
        try ignore (int_of_string scale_opt)
        with _ ->
          raise
            (InvalidMatrixArgument
               "Matrix scaler must be an int / match that of your matrix type")
      in
      let%lwt () = Lwt_io.printlf "Sending job: %s" job in
      send_one_matrix matrix_type matrix_opp (Some scale_opt) first_matrix_path
        out_channel
    else
      let () =
        try ignore (float_of_string scale_opt)
        with _ ->
          raise
            (InvalidMatrixArgument
               "Matrix scaler must be an float / match that of your matrix type")
      in
      let%lwt () = Lwt_io.printlf "Sending job: %s" job in
      send_one_matrix matrix_type matrix_opp (Some scale_opt) first_matrix_path
        out_channel
  else
    let second_matrix_path =
      match split_first_space !rest_string with
      | "", x -> raise (InvalidMatrixArgument "Missing Path for second matrix")
      | file_path, rest ->
          if
            Sys.file_exists file_path
            && String.lowercase_ascii (Filename.extension file_path) = ".csv"
          then (
            rest_string := rest;
            file_path)
          else
            raise
              (InvalidMatrixArgument
                 ("File path " ^ file_path
                ^ " does not exist or is not a .csv file"))
    in
    let%lwt () = Lwt_io.printlf "Sending job: %s" job in
    send_two_matrix matrix_type matrix_opp first_matrix_path second_matrix_path
      out_channel

let client_loop server_in server_out =
  let rec send_job () =
    match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
    | Some job ->
        let%lwt () = Lwt_io.printlf "Attempting to send job: %s" job in
        let%lwt () =
          try process_job server_out job
          with InvalidMatrixArgument x ->
            let%lwt () = Lwt_io.printl ("Job Failed: " ^ x) in
            let%lwt () =
              Lwt_io.printl
                "Usage: <int, float> Matrix_Operation Matrix_CSV_Path_One \
                 Optional_Matrix_CSV_Path_One"
            in
            Lwt.return_unit
        in
        let%lwt () = Lwt_io.flush server_out in
        send_job ()
    | None -> Lwt_io.printl "Input closed (EOF). Stopping sends."
  in

  let rec receive_responses () =
    let%lwt response_opt = Lwt_io.read_line_opt server_in in
    match response_opt with
    | Some "INT_JOB_OUTPUT" ->
        let%lwt () = Lwt_io.printlf "int job output got" in
        let%lwt matrix = read_int_matrix_input server_in in
        let int_mat_res = IntMatrix matrix in
        let%lwt () = Lwt_io.printlf "Received Result Matrix: " in
        let%lwt () = print_matrix int_mat_res Lwt_io.stdout in
        receive_responses ()
    | Some "FLOAT_JOB_OUTPUT" ->
        let%lwt () = Lwt_io.printlf "float job output got" in
        let%lwt matrix = read_float_matrix_input server_in in
        let float_mat_res = FloatMatrix matrix in
        let%lwt () = Lwt_io.printlf "Received Result Matrix: " in
        let%lwt () = print_matrix float_mat_res Lwt_io.stdout in
        receive_responses ()
    | Some "END_OF_JOBS" ->
        let%lwt () = Lwt_io.printlf "End of job list." in
        receive_responses ()
    | Some other ->
        let%lwt () = Lwt_io.printlf "Job Status: \n %s" other in
        receive_responses ()
    | None ->
        print_endline "Server Closed";
        Lwt.return_unit
  in

  Lwt.choose [ send_job (); receive_responses () ]

(** int add path1 path2 -> int add *)

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

(** Program entry point.*)
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
