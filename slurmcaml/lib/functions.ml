(* each module we make will be an instance of IntegerMatrix signature from
   matrices.ml *)
open Slurmcaml.Matrices

module type MatrixOperations = sig
  type t

  val add : t array array -> t array array -> t array array
  val subtract : t array array -> t array array -> t array array
  val multiply : t array array -> t array array -> t array array
  val scale : t -> 'a array array -> t array array
end

module IntegerMatrixOperations : MatrixOperations with type t = int = struct end
