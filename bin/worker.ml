open Slurmcaml.Functions
open Slurmcaml.Matrixutils

(**[sock_addr_to_string] converts a unix socket address to a string*)
let sock_addr_to_string (add : Unix.sockaddr) =
  match add with
  | ADDR_INET (a, port) -> Unix.string_of_inet_addr a ^ ":" ^ string_of_int port
  | _ -> failwith "unsupported"

(**[return_matrix valuetype op server_in] checks the int or float type of the
   matrix, and based on the operation, dispatches to the appropriate read matrix
   functions, and calls the matrix operation function. This function is in the
   front end because it requires reading from input channels.*)
let return_matrix valuetype op server_in =
  match valuetype with
  | "int" -> (
      match op with
      | "add" ->
          let%lwt mat1 = read_int_matrix_input server_in in
          let%lwt mat2 = read_int_matrix_input server_in in
          Lwt.return (IntMatrix (IntegerMatrixOperations.add mat1 mat2))
      | "subtract" ->
          let%lwt mat1 = read_int_matrix_input server_in in
          let%lwt mat2 = read_int_matrix_input server_in in
          Lwt.return (IntMatrix (IntegerMatrixOperations.subtract mat1 mat2))
      | "multiply" ->
          let%lwt mat1 = read_int_matrix_input server_in in
          let%lwt mat2 = read_int_matrix_input server_in in
          Lwt.return (IntMatrix (IntegerMatrixOperations.multiply mat1 mat2))
      | "scale" ->
          let%lwt k = Lwt_io.read_line server_in in
          let%lwt mat = read_int_matrix_input server_in in
          Lwt.return
            (IntMatrix (IntegerMatrixOperations.scale (int_of_string k) mat))
      | _ -> failwith "not an implemented function")
  | "float" -> (
      let%lwt () = Lwt_io.printl "received float job" in
      match op with
      | "add" ->
          let%lwt mat1 = read_float_matrix_input server_in in
          let%lwt mat2 = read_float_matrix_input server_in in
          Lwt.return (FloatMatrix (FloatMatrixOperations.add mat1 mat2))
      | "subtract" ->
          let%lwt mat1 = read_float_matrix_input server_in in
          let%lwt mat2 = read_float_matrix_input server_in in
          Lwt.return (FloatMatrix (FloatMatrixOperations.subtract mat1 mat2))
      | "multiply" ->
          let%lwt mat1 = read_float_matrix_input server_in in
          let%lwt mat2 = read_float_matrix_input server_in in
          Lwt.return (FloatMatrix (FloatMatrixOperations.multiply mat1 mat2))
      | "scale" ->
          let%lwt k = Lwt_io.read_line server_in in
          let%lwt mat = read_float_matrix_input server_in in
          Lwt.return
            (FloatMatrix (FloatMatrixOperations.scale (float_of_string k) mat))
      | _ -> failwith "not an implemented function")
  | _ -> failwith "not an implemented type"

(**[run_client ipaddr port instanceName] starts a worker on [ipaddr] with [port]
   and [instanceName]. It handles jobs from the server and dispatches to the
   [return_matrix] function. This function is in the front end because it
   requires starting up a worker on the network and reading input/producing
   output.*)
let run_client ipaddr port instanceName =
  let client () =
    let%lwt () = Lwt_io.printl "Attempting to connect to Server..." in
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
    let%lwt () =
      Lwt_io.printlf "Connected! Worker has joined as %s" instanceName
    in
    let%lwt () = Lwt_io.fprintlf server_out "%s" instanceName in
    let%lwt () = Lwt_io.flush server_out in
    let rec handle_job () =
      let%lwt job_opt = Lwt_io.read_line_opt server_in in
      match job_opt with
      | None ->
          let%lwt () = Lwt_io.printlf "Disconnected from server." in
          Lwt.return_unit
      | Some job ->
          let%lwt () =
            try%lwt
              let%lwt () = Lwt_io.printlf "Received job: %s" job in
              let%lwt valuetype = Lwt_io.read_line server_in in
              let%lwt op = Lwt_io.read_line server_in in
              let%lwt res = return_matrix valuetype op server_in in
              let%lwt () = Lwt_io.printlf "Completed job: %s" job in
              let%lwt () = Lwt_io.fprintl server_out "result" in
              let%lwt () = print_matrix res server_out in
              let%lwt () = print_matrix res Lwt_io.stdout in
              Lwt_io.flush server_out
            with Invalid_argument s ->
              let%lwt () = Lwt_io.fprintl server_out "Failure" in
              let%lwt () = Lwt_io.fprintl server_out s in
              Lwt_io.flush server_out
          in
          handle_job ()
    in
    handle_job ()
  in
  Lwt_main.run (client ())

(**Program entry point*)
let _ =
  let print_usage () =
    Printf.printf "Usage: %s <server | client>\n" Sys.argv.(0)
  in
  if Array.length Sys.argv < 4 then print_usage ()
  else
    let ipaddr = Sys.argv.(1) in
    let port =
      match int_of_string_opt Sys.argv.(2) with
      | Some p -> p
      | None ->
          Printf.printf "Invalid port number: %s\n" Sys.argv.(2);
          exit 1
    in

    if Array.length Sys.argv < 4 then print_usage ()
    else
      let instanceName = Sys.argv.(3) in
      run_client ipaddr port instanceName
