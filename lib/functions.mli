(** An implementation of [MatrixOperations] where each matrix element is an
    integer. Provides standard arithmetic operations with runtime checks
    ensuring shape consistency. *)
module IntegerMatrixOperations : sig
  type t = int
  (** The type of each element stored inside an integer matrix. *)

  val add : t array array -> t array array -> t array array
  (** [add m1 m2] returns the element-wise sum of matrices [m1] and [m2].
      Raises: [Invalid_argument] if the matrices do not have the same
      dimensions. *)

  val subtract : t array array -> t array array -> t array array
  (** [subtract m1 m2] returns the element-wise difference [m1 - m2].
      Raises: [Invalid_argument] if the matrices do not have matching shapes. *)

  val multiply : t array array -> t array array -> t array array
  (** [multiply m1 m2] returns the matrix product [m1 × m2] using standard
      row-by-column multiplication.
      Requires: the number of columns in [m1] equals the number of rows in [m2].
      Raises: [Invalid_argument] if dimensions are incompatible. *)

  val scale : t -> t array array -> t array array
  (** [scale k m] multiplies every element of matrix [m] by the scalar [k]. *)
end

(** A floating-point implementation of [MatrixOperations], identical in
    structure to [IntegerMatrixOperations] but operating on [float] values and
    using floating-point arithmetic. *)
module FloatMatrixOperations : sig
  type t = float
  (** The type of each element stored inside a floating-point matrix. *)

  val add : t array array -> t array array -> t array array
  (** [add m1 m2] returns the element-wise sum of matrices [m1] and [m2].
      Raises: [Invalid_argument] if the matrices do not have the same
      dimensions. *)

  val subtract : t array array -> t array array -> t array array
  (** [subtract m1 m2] returns the element-wise difference [m1 - m2].
      Raises: [Invalid_argument] if the matrices do not have matching shapes. *)

  val multiply : t array array -> t array array -> t array array
  (** [multiply m1 m2] returns the matrix product [m1 × m2] using standard
      row-by-column multiplication.
      Requires: the number of columns in [m1] equals the number of rows in [m2].
      Raises: [Invalid_argument] if dimensions are incompatible. *)

  val scale : t -> t array array -> t array array
  (** [scale k m] multiplies every element of matrix [m] by the scalar [k]. *)
end
