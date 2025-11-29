open Functions

exception MatrixNotReadable

type result =
  | IntMatrix of int array array
  | FloatMatrix of float array array

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

type currentJobType =
  | IntJobASMType
  | FloatJobASMType
  | IntJobSType
  | FloatJobSType

let split_matrix mat num =
  let n = Array.length mat in
  let base = n / num in
  let extra = n mod num in
  Array.init num (fun i ->
      let size = base + if i < extra then 1 else 0 in
      let start = (i * base) + min i extra in
      Array.sub mat start size)

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

let split_int_job_s numworkers (job : int_job_s) =
  let { aint; scalar } = job in
  let a_chunks = split_matrix aint numworkers in
  IntJobSSplit
    (Array.init numworkers (fun i -> { aint = a_chunks.(i); scalar }))

let split_float_job_s numworkers (job : float_job_s) =
  let { afloat; scalar } = job in
  let a_chunks = split_matrix afloat numworkers in
  FloatJobSSplit
    (Array.init numworkers (fun i -> { afloat = a_chunks.(i); scalar }))

let construct_int_matrix row col = IntMatrix (Array.make row (Array.make col 0))

let fill_matrix_int (res : int array array) ((first_row, last_row) : 'a * 'b)
    (old_array : int array array) =
  Array.iteri
    (fun num_row matrix_row -> old_array.(num_row + first_row) <- matrix_row)
    res

let fill_matrix_float (res : float array array)
    ((first_row, last_row) : 'a * 'b) (old_array : float array array) =
  Array.iteri
    (fun num_row matrix_row -> old_array.(num_row + first_row) <- matrix_row)
    res

let construct_float_matrix row col =
  FloatMatrix (Array.make row (Array.make col 0.0))

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

(* reads int matrix from a file and returns the matrix, # rows, # columns,
   valuetype*)
let read_int_matrix_from_file pathname =
  try
    let csv = Csv.load pathname in
    let arrayrows =
      List.map (fun x -> Array.of_list (List.map int_of_string x)) csv
    in
    IntMatrix (Array.of_list arrayrows)
  with exn -> raise MatrixNotReadable

(* reads float matrix from a file, returns the matrix, # of rows, # of columsn,
   valuetype*)
let read_float_matrix_from_file pathname =
  try
    let csv = Csv.load pathname in
    let arrayrows =
      List.map (fun x -> Array.of_list (List.map float_of_string x)) csv
    in
    FloatMatrix (Array.of_list arrayrows)
  with exn -> raise MatrixNotReadable

(*prints a matrix, type checking against IntMatrix and FloatMatrix*)
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

let set_row_int i mat row =
  let row =
    Array.of_list (List.map int_of_string (String.split_on_char ',' row))
  in
  mat.(i) <- row

let set_row_float i mat row =
  let row =
    Array.of_list (List.map float_of_string (String.split_on_char ',' row))
  in
  mat.(i) <- row

(* first has to send value type (int, float, etc.), then # of rows, then # of columns*)
(* TODO: implement error checking for matrix size*)
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
