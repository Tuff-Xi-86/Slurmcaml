open OUnit2
open Slurmcaml.Functions

let tests =
  "test suite"
  >::: [
         ( "testing matrixes addition when both arrays are the same" >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [| 2; 4; 6; 8; 10 |] |] );
       ]

let _ = run_test_tt_main tests

let equal_length a1 a2 =
  if Array.length a1.(0) <> Array.length a2.(0) then (
    print_endline "Length of Arrays are not the same and cannot be added!";
    exit 1)
  else ()
