open OUnit2
open Slurmcaml.Functions

let tests =
  "test suite"
  >::: [
         ( "testing addition of matrixes addition when both arrays are the same"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [| 2; 4; 6; 8; 10 |] |] );
         ( "testing addition of matrixes when the first matrix contains a \
            negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [| 0; 4; 6; 8; 10 |] |] );
         ( "testing addition of matrixes when the both matrix contains a \
            negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| -1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [| -2; 4; 6; 8; 10 |] |] );
         ( "testing addition of matrixes when the first matrix is all zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0; 0; 0; 0; 0 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [| 1; 2; 3; 4; 5 |] |] );
         ( "testing addition of matrixes when the both matrix is all zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0; 0; 0; 0; 0 |] |] in
              let array2 = [| [| 0; 0; 0; 0; 0 |] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [| 0; 0; 0; 0; 0 |] |] );
         ( "testing addition of matrixes when the both matrix are empty"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [||] |] in
              let array2 = [| [||] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [||] |] );
         ( "testing addition of matrixes when the rows are differernt lengths \
            are different lengths"
         >:: fun _ ->
           let array1 = [| [| 1 |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.add array1 array2) );
         ( "testing addition of matrixes when the number or rows are different"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1 |]; [| 1 |] |] in
              let array2 = [| [||] |] in
              IntegerMatrixOperations.add array1 array2)
             [| [||] |] );
         ( "testing addition of matrixes when there are multiple rows"
         >:: fun _ ->
           let array1 = [| [| 1 |]; [| 1 |]; [| 1 |]; [| 1 |] |] in
           let array2 = [| [| 1 |]; [| 1 |]; [| 1 |]; [| 1 |] |] in
           assert_equal
             (IntegerMatrixOperations.add array1 array2)
             [| [| 2 |]; [| 2 |]; [| 2 |]; [| 2 |] |] );
         ( "testing addition of float matrixes addition when both arrays are \
            the same"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1.0; 2.0; 3.0; 4.0; 5.0 |] |] in
              let array2 = [| [| 1.0; 2.0; 3.0; 4.0; 5.0 |] |] in
              FloatMatrixOperations.add array1 array2)
             [| [| 2.0; 4.0; 6.0; 8.; 10. |] |] );
         ( "testing addition of float matrixes when the first matrix contains \
            a negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1.; 2.; 3.; 4.; 5. |] |] in
              let array2 = [| [| 1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.add array1 array2)
             [| [| 0.; 4.; 6.; 8.; 10. |] |] );
         ( "testing addition of float  matrixes when the both matrix contains \
            a negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1.; 2.; 3.; 4.; 5. |] |] in
              let array2 = [| [| -1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.add array1 array2)
             [| [| -2.; 4.; 6.; 8.; 10. |] |] );
         ( "testing addition of float matrixes when the first matrix is all \
            zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0.; 0.; 0.; 0.; 0. |] |] in
              let array2 = [| [| 1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.add array1 array2)
             [| [| 1.; 2.; 3.; 4.; 5. |] |] );
         ( "testing addition of float matrixes when the both matrix is all zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0.; 0.; 0.; 0.; 0. |] |] in
              let array2 = [| [| 0.; 0.; 0.; 0.; 0. |] |] in
              FloatMatrixOperations.add array1 array2)
             [| [| 0.; 0.; 0.; 0.; 0. |] |] );
         ( "testing addition of float matrixes when the both matrix are empty"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [||] |] in
              let array2 = [| [||] |] in
              FloatMatrixOperations.add array1 array2)
             [| [||] |] );
         ( "testing addition of float matrixes when the rows are differernt \
            lengths are different lengths"
         >:: fun _ ->
           let array1 = [| [| 1. |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> FloatMatrixOperations.add array1 array2) );
         ( "testing addition of float matrixes when the number or rows are \
            different"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1. |]; [| 1. |] |] in
              let array2 = [| [||] |] in
              FloatMatrixOperations.add array1 array2)
             [| [||] |] );
         ( "testing addition of float matrixes when there are multiple rows"
         >:: fun _ ->
           let array1 = [| [| 1. |]; [| 1. |]; [| 1. |]; [| 1. |] |] in
           let array2 = [| [| 1. |]; [| 1. |]; [| 1. |]; [| 1. |] |] in
           assert_equal
             (FloatMatrixOperations.add array1 array2)
             [| [| 2. |]; [| 2. |]; [| 2. |]; [| 2. |] |] );
         ( "testing subtraction of matrixes addition when both arrays are the \
            same"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [| 0; 0; 0; 0; 0 |] |] );
         ( "testing subtraction of matrixes when the first matrix contains a \
            negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [| -2; 0; 0; 0; 0 |] |] );
         ( "testing subtraction of matrixes when the both matrix contains a \
            negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1; 2; 3; 4; 5 |] |] in
              let array2 = [| [| -1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [| 0; 0; 0; 0; 0 |] |] );
         ( "testing subtraction of matrixes when the first matrix is all zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0; 0; 0; 0; 0 |] |] in
              let array2 = [| [| 1; 2; 3; 4; 5 |] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [| -1; -2; -3; -4; -5 |] |] );
         ( "testing subtraction of matrixes when the both matrix is all zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0; 0; 0; 0; 0 |] |] in
              let array2 = [| [| 0; 0; 0; 0; 0 |] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [| 0; 0; 0; 0; 0 |] |] );
         ( "testing subtraction of matrixes when the both matrix are empty"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [||] |] in
              let array2 = [| [||] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [||] |] );
         ( "testing subtraction of matrixes when the rows are differernt \
            lengths are different lengths"
         >:: fun _ ->
           let array1 = [| [| 1 |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.subtract array1 array2) );
         ( "testing subtraction of matrixes when the number or rows are \
            different"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1 |]; [| 1 |] |] in
              let array2 = [| [||] |] in
              IntegerMatrixOperations.subtract array1 array2)
             [| [||] |] );
         ( "testing subtraction of matrixes when there are multiple rows"
         >:: fun _ ->
           let array1 = [| [| 1 |]; [| 1 |]; [| 1 |]; [| 1 |] |] in
           let array2 = [| [| 1 |]; [| 1 |]; [| 1 |]; [| 1 |] |] in
           assert_equal
             (IntegerMatrixOperations.subtract array1 array2)
             [| [| 0 |]; [| 0 |]; [| 0 |]; [| 0 |] |] );
         ( "testing subtraction of float matrixes addition when both arrays \
            are the same"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1.; 2.; 3.; 4.; 5. |] |] in
              let array2 = [| [| 1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [| 0.; 0.; 0.; 0.; 0. |] |] );
         ( "testing subtraction of float matrixes when the first matrix \
            contains a negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1.; 2.; 3.; 4.; 5. |] |] in
              let array2 = [| [| 1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [| -2.; 0.; 0.; 0.; 0. |] |] );
         ( "testing subtraction of float matrixes when the both matrix \
            contains a negative number"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| -1.; 2.; 3.; 4.; 5. |] |] in
              let array2 = [| [| -1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [| 0.; 0.; 0.; 0.; 0. |] |] );
         ( "testing subtraction of float matrixes when the first matrix is all \
            zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0.; 0.; 0.; 0.; 0. |] |] in
              let array2 = [| [| 1.; 2.; 3.; 4.; 5. |] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [| -1.; -2.; -3.; -4.; -5. |] |] );
         ( "testing subtraction of float matrixes when the both matrix is all \
            zeros"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 0.; 0.; 0.; 0.; 0. |] |] in
              let array2 = [| [| 0.; 0.; 0.; 0.; 0. |] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [| 0.; 0.; 0.; 0.; 0. |] |] );
         ( "testing subtraction of float matrixes when the both matrix are empty"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [||] |] in
              let array2 = [| [||] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [||] |] );
         ( "testing subtraction of float matrixes when the rows are differernt \
            lengths are different lengths"
         >:: fun _ ->
           let array1 = [| [| 1. |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> FloatMatrixOperations.subtract array1 array2) );
         ( "testing subtraction of float matrixes when the number or rows are \
            different"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1. |]; [| 1. |] |] in
              let array2 = [| [||] |] in
              FloatMatrixOperations.subtract array1 array2)
             [| [||] |] );
         ( "testing subtraction of float matrixes when there are multiple rows"
         >:: fun _ ->
           let array1 = [| [| 1. |]; [| 1. |]; [| 1. |]; [| 1. |] |] in
           let array2 = [| [| 1. |]; [| 1. |]; [| 1. |]; [| 1. |] |] in
           assert_equal
             (FloatMatrixOperations.subtract array1 array2)
             [| [| 0. |]; [| 0. |]; [| 0. |]; [| 0. |] |] );
       ]

let _ = run_test_tt_main tests
