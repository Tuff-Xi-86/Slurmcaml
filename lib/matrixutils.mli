type result =
  | IntMatrix of int array array
  | FloatMatrix of float array array

type currentJobType =
  | IntJobASMType
  | FloatJobASMType
  | IntJobSType
  | FloatJobSType

exception MatrixNotReadable

type int_job_asm = {
  aint : int array array;
  bint : int array array;
  opi : string;
}

type float_job_asm = {
  afloat : float array array;
  bfloat : float array array;
  opf : string;
}

type int_job_s = {
  aint : int array array;
  scalar : int;
}

type float_job_s = {
  afloat : float array array;
  scalar : float;
}

type job =
  | IntJobASM of int_job_asm
  | FloatJobASM of float_job_asm
  | IntJobS of int_job_s
  | FloatJobS of float_job_s

type split =
  | IntJobASMSplit of int_job_asm array
  | FloatJobASMSplit of float_job_asm array
  | IntJobSSplit of int_job_s array
  | FloatJobSSplit of float_job_s array

val print_matrix : result -> Lwt_io.output_channel -> unit Lwt.t
val read_int_matrix_input : Lwt_io.input_channel -> int array array Lwt.t
val read_float_matrix_input : Lwt_io.input_channel -> float array array Lwt.t
val construct_int_matrix : int -> int -> result
val construct_float_matrix : int -> int -> result
val read_int_matrix_from_file : string -> result
val read_float_matrix_from_file : string -> result
val fill_matrix_int : int array array -> int * 'b -> int array array -> unit

val fill_matrix_float :
  float array array -> int * 'b -> float array array -> unit

val split_matrix : 'a array -> int -> 'a array array
val split_first_space : string -> string * string
val split_int_job_asm : int -> int_job_asm -> split
val split_float_job_asm : int -> float_job_asm -> split
val split_int_job_s : int -> int_job_s -> split
val split_float_job_s : int -> float_job_s -> split
