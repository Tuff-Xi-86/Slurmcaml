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
           let array1 = [| [| 1 |]; [| 1 |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.add array1 array2) );
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
           let array1 = [| [| 1. |]; [| 1. |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> FloatMatrixOperations.add array1 array2) );
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
           let array1 = [| [| 1 |]; [| 1 |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.subtract array1 array2) );
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
           let array1 = [| [| 1. |]; [| 1. |] |] in
           let array2 = [| [||] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> FloatMatrixOperations.subtract array1 array2) );
         ( "testing subtraction of float matrixes when there are multiple rows"
         >:: fun _ ->
           let array1 = [| [| 1. |]; [| 1. |]; [| 1. |]; [| 1. |] |] in
           let array2 = [| [| 1. |]; [| 1. |]; [| 1. |]; [| 1. |] |] in
           assert_equal
             (FloatMatrixOperations.subtract array1 array2)
             [| [| 0. |]; [| 0. |]; [| 0. |]; [| 0. |] |] );
         ( "multiply 1x2 and 2x1 gives 1x1" >:: fun _ ->
           let a = [| [| 1; 2 |] |] in
           let b = [| [| 3 |]; [| 4 |] |] in
           assert_equal [| [| 11 |] |] (IntegerMatrixOperations.multiply a b) );
         ( "multiply 2x2 and 2x2" >:: fun _ ->
           let a = [| [| 1; 2 |]; [| 3; 4 |] |] in
           let b = [| [| 5; 6 |]; [| 7; 8 |] |] in
           assert_equal
             [| [| 19; 22 |]; [| 43; 50 |] |]
             (IntegerMatrixOperations.multiply a b) );
         ( "multiply rectangular 2x3 and 3x2" >:: fun _ ->
           let a = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           let b = [| [| 7; 8 |]; [| 9; 10 |]; [| 11; 12 |] |] in
           assert_equal
             [| [| 58; 64 |]; [| 139; 154 |] |]
             (IntegerMatrixOperations.multiply a b) );
         ( "multiply mismatched dims raises" >:: fun _ ->
           let a = [| [| 1; 2 |] |] in
           let b = [| [| 3; 4 |] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.multiply a b) );
         ( "multiply empty single-row matrices triggers equal_row special case"
         >:: fun _ ->
           let a = [| [||] |] in
           let b = [| [||] |] in

           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.multiply a b) );
         ( "multiply triggers equal_row row-length mismatch" >:: fun _ ->
           let a = [| [| 1; 2 |]; [| 3 |] |] in

           let b = [| [| 1 |]; [| 1 |] |] in

           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> IntegerMatrixOperations.multiply a b) );
         ( "float multiply 1x2 and 2x1 gives 1x1" >:: fun _ ->
           let a = [| [| 1.0; 2.0 |] |] in
           let b = [| [| 3.0 |]; [| 4.0 |] |] in
           assert_equal [| [| 11.0 |] |] (FloatMatrixOperations.multiply a b) );
         ( "float multiply 2x2 and 2x2" >:: fun _ ->
           let a = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
           let b = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |] in
           assert_equal
             [| [| 19.0; 22.0 |]; [| 43.0; 50.0 |] |]
             (FloatMatrixOperations.multiply a b) );
         ( "float multiply rectangular 2x3 and 3x2" >:: fun _ ->
           let a = [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |] |] in
           let b = [| [| 7.0; 8.0 |]; [| 9.0; 10.0 |]; [| 11.0; 12.0 |] |] in
           assert_equal
             [| [| 58.0; 64.0 |]; [| 139.0; 154.0 |] |]
             (FloatMatrixOperations.multiply a b) );
         ( "float multiply triggers equal_row row-length mismatch" >:: fun _ ->
           let a = [| [| 1.0; 2.0 |]; [| 3.0 |] |] in
           let b = [| [| 1.0 |]; [| 1.0 |] |] in

           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> FloatMatrixOperations.multiply a b) );
         ( "float multiply mismatched dims raises" >:: fun _ ->
           let a = [| [| 1.0; 2.0 |] |] in
           let b = [| [| 3.0; 4.0 |] |] in
           assert_raises (Invalid_argument "Matrix sizes do not match")
             (fun () -> FloatMatrixOperations.multiply a b) );
         ( "int scale simple 2x2" >:: fun _ ->
           let m = [| [| 1; 2 |]; [| 3; 4 |] |] in
           assert_equal
             [| [| 2; 4 |]; [| 6; 8 |] |]
             (IntegerMatrixOperations.scale 2 m) );
         ( "int scale by zero" >:: fun _ ->
           let m = [| [| 5; 6; 7 |] |] in
           assert_equal [| [| 0; 0; 0 |] |] (IntegerMatrixOperations.scale 0 m)
         );
         ( "int scale negative" >:: fun _ ->
           let m = [| [| 1; -2 |] |] in
           assert_equal [| [| -3; 6 |] |] (IntegerMatrixOperations.scale (-3) m)
         );
         ( "int scale empty row" >:: fun _ ->
           let m = [| [||] |] in
           assert_equal [| [||] |] (IntegerMatrixOperations.scale 10 m) );
         ( "float scale simple 2x2" >:: fun _ ->
           let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
           assert_equal
             [| [| 2.0; 4.0 |]; [| 6.0; 8.0 |] |]
             (FloatMatrixOperations.scale 2.0 m) );
         ( "float scale by zero" >:: fun _ ->
           let m = [| [| 3.5; 4.5 |] |] in
           assert_equal [| [| 0.0; 0.0 |] |] (FloatMatrixOperations.scale 0.0 m)
         );
         ( "float scale fractional" >:: fun _ ->
           let m = [| [| 2.0; 4.0 |] |] in
           assert_equal [| [| 1.0; 2.0 |] |] (FloatMatrixOperations.scale 0.5 m)
         );
         ( "float scale empty row" >:: fun _ ->
           let m = [| [||] |] in
           assert_equal [| [||] |] (FloatMatrixOperations.scale 5.0 m) );
       ]

let _ = run_test_tt_main tests
