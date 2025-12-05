(** A result value representing either an integer matrix or a floating-point
    matrix. *)
type result =
  | IntMatrix of int array array
  | FloatMatrix of float array array

(** A type indicating the form of the current job being executed
    (integer/float, ASM/scalar). *)
type currentJobType =
  | IntJobASMType
  | FloatJobASMType
  | IntJobSType
  | FloatJobSType

exception MatrixNotReadable
(** Raised when a matrix file or input stream cannot be parsed into a valid
    matrix. *)

(** A job describing an integer matrix operation involving two matrices
    (add, subtract, multiply). *)
type int_job_asm = {
  aint : int array array;
  bint : int array array;
  opi : string;
}

(** A job describing a floating-point matrix operation involving two matrices
    (add, subtract, multiply). *)
type float_job_asm = {
  afloat : float array array;
  bfloat : float array array;
  opf : string;
}

(** A job describing an integer matrix scalar operation (scale). *)
type int_job_s = {
  aint : int array array;
  scalar : int;
}

(** A job describing a floating-point matrix scalar operation (scale). *)
type float_job_s = {
  afloat : float array array;
  scalar : float;
}

(** A variant representing any form of matrix job (integer or float,
    ASM or scalar). *)
type job =
  | IntJobASM of int_job_asm
  | FloatJobASM of float_job_asm
  | IntJobS of int_job_s
  | FloatJobS of float_job_s

(** A variant representing a job that has been split into per-worker chunks
    for distributed execution. *)
type split =
  | IntJobASMSplit of int_job_asm array
  | FloatJobASMSplit of float_job_asm array
  | IntJobSSplit of int_job_s array
  | FloatJobSSplit of float_job_s array

(** Prints a matrix to the provided output channel in the format:
      row_count
      column_count
      comma-separated rows...
      end *)
val print_matrix : result -> Lwt_io.output_channel -> unit Lwt.t

(** Reads an integer matrix from the input channel using the same format
    produced by [print_matrix]. *)
val read_int_matrix_input : Lwt_io.input_channel -> int array array Lwt.t

(** Reads a floating-point matrix from the input channel using the same
    format produced by [print_matrix]. *)
val read_float_matrix_input : Lwt_io.input_channel -> float array array Lwt.t

(** Creates an empty integer matrix of dimensions [rows × cols]. *)
val construct_int_matrix : int -> int -> result

(** Creates an empty floating-point matrix of dimensions [rows × cols]. *)
val construct_float_matrix : int -> int -> result

(** Reads a CSV file into an integer matrix. Raises [MatrixNotReadable] on
    malformed content. *)
val read_int_matrix_from_file : string -> result

(** Reads a CSV file into a floating-point matrix. Raises [MatrixNotReadable]
    on malformed content. *)
val read_float_matrix_from_file : string -> result

(** Fills an integer matrix with the rows of a subresult starting at the given
    [first_row, last_row] range. *)
val fill_matrix_int :
  int array array -> int * 'b -> int array array -> unit

(** Floating-point equivalent of [fill_matrix_int]. *)
val fill_matrix_float :
  float array array -> int * 'b -> float array array -> unit

(** Splits a matrix into [n] contiguous row segments, distributing extra rows
    among earlier segments. *)
val split_matrix : 'a array array -> int -> 'a array array array

(** Splits a string into (word_before_space, rest_after_space). *)
val split_first_space : string -> string * string

(** Splits an integer ASM job into [n] independent subjobs. *)
val split_int_job_asm : int -> int_job_asm -> split

(** Splits a floating-point ASM job into [n] independent subjobs. *)
val split_float_job_asm : int -> float_job_asm -> split

(** Splits an integer scalar job into [n] independent subjobs. *)
val split_int_job_s : int -> int_job_s -> split

(** Splits a floating-point scalar job into [n] independent subjobs. *)
val split_float_job_s : int -> float_job_s -> split

(** Computes:
    - the job type,
    - per-worker row assignments,
    - an empty output matrix,
    - and the starting worker index (0). *)
val determine_assignments :
  job ->
  ('a, 'b * 'c * 'd * 'e) Hashtbl.t ->
  currentJobType * ('a, int * int) Hashtbl.t * result * int

(** Parses a matrix operation (add, subtract, multiply, scale, transpose)
    from [rest_string], consuming it and returning its string name. *)
val process_op : string ref -> string

(** Parses and validates a CSV path from [rest_string]. The path must exist and
    have a .csv extension. *)
val parse_path : string ref -> string

(** Parses and validates a matrix type ("int" or "float") from [rest_string]. *)
val parse_matrix_type : string ref -> string

(** Extracts the scalar value from [rest_string] without validating it. *)
val parse_scale : string ref -> string

(** Ensures the scalar string is a valid integer. *)
val check_scalar_int : string -> unit

(** Ensures the scalar string is a valid floating-point value. *)
val check_scalar_float : string -> unit
