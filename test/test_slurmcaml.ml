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
