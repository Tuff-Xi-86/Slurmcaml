open Functions

exception MatrixNotReadable
(** Raised when a matrix file or input stream cannot be parsed into a valid
    matrix. *)

exception InvalidMatrixArgument of string
(** Raised when an invalid scalar, file path, matrix type, or operation is
    provided. The included message describes the issue. *)

(** A result value representing either an integer matrix or a floating-point
    matrix. *)
type result =
  | IntMatrix of int array array
  | FloatMatrix of float array array

type int_job_asm = {
  aint : int array array;
  bint : int array array;
  opi : string;
}
(** A job describing an integer matrix operation involving two matrices (add,
    subtract, multiply). *)

type float_job_asm = {
  afloat : float array array;
  bfloat : float array array;
  opf : string;
}
(** A job describing a floating-point matrix operation involving two matrices
    (add, subtract, multiply). *)

type int_job_s = {
  aint : int array array;
  scalar : int;
}
(** A job describing an integer matrix scalar operation (scale). *)

type float_job_s = {
  afloat : float array array;
  scalar : float;
}
(** A job describing a floating-point matrix scalar operation (scale). *)

(** A variant representing any form of matrix job (integer or float, ASM or
    scalar). *)
type job =
  | IntJobASM of int_job_asm
  | FloatJobASM of float_job_asm
  | IntJobS of int_job_s
  | FloatJobS of float_job_s

(** A variant representing a matrix job that has been split into row-level
    subjobs for parallel execution. *)
type split =
  | IntJobASMSplit of int_job_asm array
  | FloatJobASMSplit of float_job_asm array
  | IntJobSSplit of int_job_s array
  | FloatJobSSplit of float_job_s array

(** A type indicating the form of the current job being executed (integer/float,
    ASM/scalar). *)
type currentJobType =
  | IntJobASMType
  | FloatJobASMType
  | IntJobSType
  | FloatJobSType

(** [split_matrix m n] splits matrix [m] into [n] contiguous row segments. Extra
    rows are distributed among the first segments to balance load. *)
let split_matrix mat num =
  let n = Array.length mat in
  let base = n / num in
  let extra = n mod num in
  Array.init num (fun i ->
      let size = base + if i < extra then 1 else 0 in
      let start = (i * base) + min i extra in
      Array.sub mat start size)

(** Splits an integer ASM (add/subtract/multiply) job into [numworkers]
    independent subjobs. Multiplication duplicates the right-hand matrix for
    each worker. *)
let split_int_job_asm numworkers job =
  let { aint; bint; opi } = job in
  let a_chunks = split_matrix aint numworkers in
  let b_chunks =
    match opi with
    | "multiply" -> Array.make numworkers bint
    | _ -> split_matrix bint numworkers
  in
  IntJobASMSplit
    (Array.init numworkers (fun i ->
         { aint = a_chunks.(i); bint = b_chunks.(i); opi }))

(** Floating-point equivalent of [split_int_job_asm]. *)
let split_float_job_asm numworkers job =
  let { afloat; bfloat; opf } = job in
  let a_chunks = split_matrix afloat numworkers in
  let b_chunks =
    match opf with
    | "multiply" -> Array.make numworkers bfloat
    | _ -> split_matrix bfloat numworkers
  in
  FloatJobASMSplit
    (Array.init numworkers (fun i ->
         { afloat = a_chunks.(i); bfloat = b_chunks.(i); opf }))

(** Splits an integer scalar job into [numworkers] independent subjobs. *)
let split_int_job_s numworkers (job : int_job_s) =
  let { aint; scalar } = job in
  let a_chunks = split_matrix aint numworkers in
  IntJobSSplit
    (Array.init numworkers (fun i -> { aint = a_chunks.(i); scalar }))

(** Floating-point equivalent of [split_int_job_s]. *)
let split_float_job_s numworkers (job : float_job_s) =
  let { afloat; scalar } = job in
  let a_chunks = split_matrix afloat numworkers in
  FloatJobSSplit
    (Array.init numworkers (fun i -> { afloat = a_chunks.(i); scalar }))

(** Creates an empty integer matrix of dimensions [row × col]. *)
let construct_int_matrix row col = IntMatrix (Array.make row (Array.make col 0))

(** Fills an integer matrix [old_array] with the rows of [res], beginning at
    index [first_row]. *)
let fill_matrix_int (res : int array array) ((first_row, last_row) : 'a * 'b)
    (old_array : int array array) =
  Array.iteri
    (fun num_row matrix_row -> old_array.(num_row + first_row) <- matrix_row)
    res

(** Fills a float matrix [old_array] with the rows of [res], beginning at index
    [first_row]. *)
let fill_matrix_float (res : float array array)
    ((first_row, last_row) : 'a * 'b) (old_array : float array array) =
  Array.iteri
    (fun num_row matrix_row -> old_array.(num_row + first_row) <- matrix_row)
    res

(** Creates an empty floating-point matrix of dimensions [row × col]. *)
let construct_float_matrix row col =
  FloatMatrix (Array.make row (Array.make col 0.0))

(** [split_first_space s] splits [s] at the first space, returning
    (text_before_space, text_after_space). *)
let split_first_space s =
  match String.index_opt s ' ' with
  | Some i ->
      let first = String.sub s 0 i in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      (first, rest)
  | None -> (s, "")

(** Reads an integer CSV file into an integer matrix. Raises [MatrixNotReadable]
    on malformed content. *)
let read_int_matrix_from_file pathname =
  try
    let csv = Csv.load pathname in
    let arrayrows =
      List.map (fun x -> Array.of_list (List.map int_of_string x)) csv
    in
    IntMatrix (Array.of_list arrayrows)
  with exn -> raise MatrixNotReadable

(** Reads a float CSV file into a floating-point matrix. Raises
    [MatrixNotReadable] on malformed content. *)
let read_float_matrix_from_file pathname =
  try
    let csv = Csv.load pathname in
    let arrayrows =
      List.map (fun x -> Array.of_list (List.map float_of_string x)) csv
    in
    FloatMatrix (Array.of_list arrayrows)
  with exn -> raise MatrixNotReadable

(** Prints a matrix to the provided output channel in the format: row_count
    column_count comma-separated rows... end *)
let print_matrix res channel =
  match res with
  | IntMatrix mat ->
      let%lwt () = Lwt_io.fprintl channel (string_of_int (Array.length mat)) in
      let%lwt () =
        Lwt_io.fprintl channel (string_of_int (Array.length mat.(0)))
      in
      let%lwt () =
        Lwt_list.iter_s
          (fun x ->
            Lwt_io.fprintl channel
              (String.concat "," (List.map string_of_int (Array.to_list x))))
          (Array.to_list mat)
      in
      Lwt_io.fprintl channel "end"
  | FloatMatrix mat ->
      let%lwt () = Lwt_io.fprintl channel (string_of_int (Array.length mat)) in
      let%lwt () =
        Lwt_io.fprintl channel (string_of_int (Array.length mat.(0)))
      in
      let%lwt () =
        Lwt_list.iter_s
          (fun x ->
            Lwt_io.fprintl channel
              (String.concat "," (List.map string_of_float (Array.to_list x))))
          (Array.to_list mat)
      in
      Lwt_io.fprintl channel "end"

(** Sets row [i] of integer matrix [mat] using a comma-separated string of
    integers. *)
let set_row_int i mat row =
  let row =
    Array.of_list (List.map int_of_string (String.split_on_char ',' row))
  in
  mat.(i) <- row

(** Sets row [i] of float matrix [mat] using a comma-separated string of floats.
*)
let set_row_float i mat row =
  let row =
    Array.of_list (List.map float_of_string (String.split_on_char ',' row))
  in
  mat.(i) <- row

(** Reads an integer matrix from an Lwt channel using the format produced by
    [print_matrix]. *)
let read_int_matrix_input channel =
  let%lwt row = Lwt_io.read_line channel in
  let%lwt columns = Lwt_io.read_line channel in
  let mat =
    Array.init (int_of_string row) (fun _ ->
        Array.make (int_of_string columns) 0)
  in
  let rec set_row i =
    let%lwt line = Lwt_io.read_line channel in
    match line with
    | "end" -> Lwt.return ()
    | s ->
        set_row_int i mat s;
        set_row (i + 1)
  in
  let%lwt () = set_row 0 in
  Lwt.return mat

(** Reads a floating-point matrix from an Lwt channel using the format produced
    by [print_matrix]. *)
let read_float_matrix_input channel =
  let%lwt row = Lwt_io.read_line channel in
  let%lwt columns = Lwt_io.read_line channel in
  let mat =
    Array.init (int_of_string row) (fun _ ->
        Array.make (int_of_string columns) 0.0)
  in
  let rec set_row i =
    let%lwt line = Lwt_io.read_line channel in
    match line with
    | "end" -> Lwt.return ()
    | s ->
        set_row_float i mat s;
        set_row (i + 1)
  in
  let%lwt () = set_row 0 in
  Lwt.return mat

(** [compute_assignments rows users] computes the row ranges assigned to each
    worker, returning a hashtable mapping sockets to (start_row, end_row). *)
let compute_assignments rows users =
  let workers = List.of_seq (Hashtbl.to_seq users) in
  let num = List.length workers in
  let base = rows / num in
  let extra = rows mod num in
  let assignments = Hashtbl.create num in
  List.iteri
    (fun i (sock, _) ->
      let size = base + if i < extra then 1 else 0 in
      let start = (i * base) + min i extra in
      Hashtbl.replace assignments sock (start, start + size))
    workers;
  assignments

(** [empty_matrix_for_job job] constructs an empty matrix matching the size of
    the appropriate left-hand matrix of the job. *)
let empty_matrix_for_job = function
  | IntJobASM { aint; _ } | IntJobS { aint; _ } ->
      construct_int_matrix (Array.length aint) (Array.length aint.(0))
  | FloatJobASM { afloat; _ } | FloatJobS { afloat; _ } ->
      construct_float_matrix (Array.length afloat) (Array.length afloat.(0))

(** [job_type job] returns the tag describing the job's type. *)
let job_type = function
  | IntJobASM _ -> IntJobASMType
  | FloatJobASM _ -> FloatJobASMType
  | IntJobS _ -> IntJobSType
  | FloatJobS _ -> FloatJobSType

(** Computes worker assignments, the job type, an empty output matrix, and the
    starting worker index (0). *)
let determine_assignments job users =
  let rows =
    match job with
    | IntJobASM { aint; _ } | IntJobS { aint; _ } -> Array.length aint
    | FloatJobASM { afloat; _ } | FloatJobS { afloat; _ } -> Array.length afloat
  in
  let assignments = compute_assignments rows users in
  let matrix = empty_matrix_for_job job in
  let jtype = job_type job in
  (jtype, assignments, matrix, 0)

(** Parses the operation name from [rest_string], consuming it and returning one
    of: "add", "subtract", "multiply", "scale", "transpose". Raises an error on
    unknown operations. *)
let process_op rest_string =
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
  | _ -> raise (InvalidMatrixArgument "unknown matrix operation")

(** Extracts and validates a CSV file path from [rest_string], ensuring the file
    exists and ends with ".csv". *)
let parse_path rest_string =
  match split_first_space !rest_string with
  | "", x -> raise (InvalidMatrixArgument "Missing Path for matrix")
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
             ("File path " ^ file_path ^ " does not exist or is not a .csv file"))

(** Parses the matrix type ("int" or "float") from [rest_string]. *)
let parse_matrix_type rest_string =
  match split_first_space !rest_string with
  | "", x -> raise (InvalidMatrixArgument "Missing argument for matrix type")
  | "int", rest ->
      rest_string := rest;
      "int"
  | "float", rest ->
      rest_string := rest;
      "float"
  | _ ->
      raise (InvalidMatrixArgument "unknown matrix type: must be int or float")

(** Extracts the scalar string argument for a scale operation. Does not validate
    its numeric form. *)
let parse_scale rest_string =
  match split_first_space !rest_string with
  | "", x -> raise (InvalidMatrixArgument "Missing argument for matrix scalar")
  | num, x -> num

(** Ensures that [scale_opt] is a valid integer scalar. Raises on failure. *)
let check_scalar_int scale_opt =
  try ignore (int_of_string scale_opt)
  with _ ->
    raise
      (InvalidMatrixArgument
         "Matrix scalar must be an int / match that of your matrix type")

(** Ensures that [scale_opt] is a valid floating-point scalar. Raises on
    failure. *)
let check_scalar_float scale_opt =
  try ignore (float_of_string scale_opt)
  with _ ->
    raise
      (InvalidMatrixArgument
         "Matrix scalar must be an float / match that of your matrix type")
