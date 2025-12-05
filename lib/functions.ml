(** A module type describing basic matrix operations over a generic element type
    [t]. Implementations of this signature must define how addition,
    subtraction, multiplication, scaling, transposition, and mapping behave for
    their particular value type. *)
module type MatrixOperations = sig
  type t
  (** The type of each element stored inside the matrix. *)

  val add : t array array -> t array array -> t array array
  (** [add m1 m2] returns the element-wise sum of matrices [m1] and [m2].
      Raises: [Invalid_argument] if the matrices do not have the same
      dimensions. *)

  val subtract : t array array -> t array array -> t array array
  (** [subtract m1 m2] returns the element-wise difference [m1 - m2]. Raises:
      [Invalid_argument] if the matrices do not have matching shapes. *)

  val multiply : t array array -> t array array -> t array array
  (** [multiply m1 m2] returns the matrix product [m1 × m2] using standard
      row-by-column multiplication. Requires: the number of columns in [m1]
      equals the number of rows in [m2]. Raises: [Invalid_argument] if
      dimensions are incompatible. *)

  val scale : t -> t array array -> t array array
  (** [scale k m] multiplies every element of matrix [m] by the scalar [k]. *)
end

exception InvalidDimensions of string
(** Raised when two matrices have incompatible dimensions for the requested
    operation. *)

(** An implementation of [MatrixOperations] where each matrix element is an
    integer. Provides standard arithmetic operations with runtime checks
    ensuring shape consistency. *)
module IntegerMatrixOperations : MatrixOperations with type t = int = struct
  type t = int

  (** [equal_length m1 m2] checks that [m1] and [m2] have identical row and
      column counts. Raises [Invalid_argument] if they differ. *)

  let equal_length a1 a2 =
    let row1 = Array.length a1 in
    let row2 = Array.length a2 in
    if row1 <> row2 then invalid_arg "Matrix sizes do not match"
    else
      for i = 0 to row1 - 1 do
        if Array.length a1.(i) <> Array.length a2.(i) then
          invalid_arg "Matrix sizes do not match"
      done

  (** [equal_row m] ensures every row in matrix [m] has the same length. Raises
      [Invalid_argument] if the matrix is jagged. *)
  let equal_row list =
    if list == [| [||] |] then ()
    else
      let length = Array.length list.(0) in
      for i = 1 to Array.length list - 1 do
        if length <> Array.length list.(i) then
          invalid_arg "Matrix sizes do not match"
      done

  (** [equal_length_mult m1 m2] checks that matrix multiplication is valid: the
      number of columns in [m1] must equal the number of rows in [m2]. *)
  let equal_length_mult a1 a2 =
    let () = equal_row a1 in
    let () = equal_row a2 in
    let row2 = Array.length a2 in
    let column1 = Array.length a1.(0) in
    if row2 <> column1 then invalid_arg "Matrix sizes do not match" else ()

  (** [add_rows r1 r2] returns an element-wise sum of two equal-length rows. *)
  let add_rows row1 row2 =
    let length = Array.length row1 in
    let elt i = row1.(i) + row2.(i) in
    Array.init length elt

  (** [subtract_rows r1 r2] returns an element-wise difference of two rows. *)
  let subtract_rows row1 row2 =
    let length = Array.length row1 in
    let elt i = row1.(i) - row2.(i) in
    Array.init length elt

  (** [add m1 m2] performs matrix addition. *)
  let add mat1 mat2 =
    let () = equal_length mat1 mat2 in
    let length = Array.length mat1 in
    let calc i = add_rows mat1.(i) mat2.(i) in
    Array.init length calc

  (** [subtract m1 m2] performs matrix subtraction. *)
  let subtract mat1 mat2 =
    let () = equal_length mat1 mat2 in
    let length = Array.length mat1 in
    let calc i = subtract_rows mat1.(i) mat2.(i) in
    Array.init length calc

  (** [extract_column j m] returns the [j]-th column of matrix [m] as a row. *)
  let extract_column j mat =
    let length = Array.length mat in
    let elt i = mat.(i).(j) in
    Array.init length elt

  (** [dot_product r1 r2] computes the sum of element-wise products of two
      equal-length rows. *)
  let dot_product row1 row2 =
    let sum = ref 0 in
    for i = 0 to Array.length row1 - 1 do
      sum := !sum + (row1.(i) * row2.(i))
    done;
    !sum

  (** [calc_row i m1 m2] computes the i-th row of the product [m1 × m2]. *)
  let calc_row i mat1 mat2 =
    let elt j = dot_product mat1.(i) (extract_column j mat2) in
    Array.init (Array.length mat2.(0)) elt

  (** [multiply m1 m2] performs integer matrix multiplication. *)
  let multiply mat1 mat2 =
    let () = equal_length_mult mat1 mat2 in
    Array.init (Array.length mat1) (fun i -> calc_row i mat1 mat2)

  (** [scale_row k r] multiplies each element of row [r] by [k]. *)
  let scale_row k row = Array.init (Array.length row) (fun i -> k * row.(i))

  (** [scale k m] multiplies all elements in [m] by integer [k]. *)
  let scale k mat1 =
    Array.init (Array.length mat1) (fun i -> scale_row k mat1.(i))
end

(** A floating-point implementation of [MatrixOperations], identical in
    structure to [IntegerMatrixOperations] but operating on [float] values and
    using floating-point arithmetic. *)
module FloatMatrixOperations : MatrixOperations with type t = float = struct
  type t = float

  (** [equal_length m1 m2] checks that [m1] and [m2] have identical row and
      column counts. Raises [Invalid_argument] if they differ. *)
  let equal_length a1 a2 =
    let row1 = Array.length a1 in
    let row2 = Array.length a2 in
    if row1 <> row2 then invalid_arg "Matrix sizes do not match"
    else
      for i = 0 to row1 - 1 do
        if Array.length a1.(i) <> Array.length a2.(i) then
          invalid_arg "Matrix sizes do not match"
      done

  (** [equal_row m] ensures every row in matrix [m] has the same length. Raises
      [Invalid_argument] if the matrix is jagged. *)
  let equal_row list =
    if list == [| [||] |] then ()
    else
      let length = Array.length list.(0) in
      for i = 1 to Array.length list - 1 do
        if length <> Array.length list.(i) then
          invalid_arg "Matrix sizes do not match"
      done

  (** [equal_length_mult m1 m2] checks that matrix multiplication is valid: the
      number of columns in [m1] must equal the number of rows in [m2]. *)
  let equal_length_mult a1 a2 =
    let () = equal_row a1 in
    let () = equal_row a2 in
    let row2 = Array.length a2 in
    let column1 = Array.length a1.(0) in
    if row2 <> column1 then invalid_arg "Matrix sizes do not match" else ()

  (** [add_rows r1 r2] returns an element-wise sum of two equal-length rows. *)
  let add_rows row1 row2 =
    let length = Array.length row1 in
    let elt i = row1.(i) +. row2.(i) in
    Array.init length elt

  (** [subtract_rows r1 r2] returns an element-wise difference of two rows. *)
  let subtract_rows row1 row2 =
    let length = Array.length row1 in
    let elt i = row1.(i) -. row2.(i) in
    Array.init length elt

  (** [add m1 m2] performs matrix addition. *)
  let add mat1 mat2 =
    let () = equal_length mat1 mat2 in
    let length = Array.length mat1 in
    let calc i = add_rows mat1.(i) mat2.(i) in
    Array.init length calc

  (** [subtract m1 m2] performs matrix subtraction. *)
  let subtract mat1 mat2 =
    let () = equal_length mat1 mat2 in
    let length = Array.length mat1 in
    let calc i = subtract_rows mat1.(i) mat2.(i) in
    Array.init length calc

  (** [extract_column j m] returns the [j]-th column of matrix [m] as a row. *)
  let extract_column j mat =
    let length = Array.length mat in
    let elt i = mat.(i).(j) in
    Array.init length elt

  (** [dot_product r1 r2] computes the sum of element-wise products of two
      equal-length rows. *)
  let dot_product row1 row2 =
    let sum = ref 0.0 in
    for i = 0 to Array.length row1 - 1 do
      sum := !sum +. (row1.(i) *. row2.(i))
    done;
    !sum

  (** [calc_row i m1 m2] computes the i-th row of the product [m1 × m2]. *)
  let calc_row i mat1 mat2 =
    let elt j = dot_product mat1.(i) (extract_column j mat2) in
    Array.init (Array.length mat2.(0)) elt

  (** [multiply m1 m2] performs floating point matrix multiplication. *)
  let multiply mat1 mat2 =
    let () = equal_length_mult mat1 mat2 in
    Array.init (Array.length mat1) (fun i -> calc_row i mat1 mat2)

  (** [scale_row k r] multiplies each element of row [r] by [k]. *)
  let scale_row k row = Array.init (Array.length row) (fun i -> k *. row.(i))

  (** [scale k m] multiplies all elements in [m] by floating-point [k]. *)
  let scale k mat1 =
    Array.init (Array.length mat1) (fun i -> scale_row k mat1.(i))
end
