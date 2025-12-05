open OUnit2
open Slurmcaml.Matrixutils

let tests =
  "test suite"
  >::: [
         ( "testing split matrix when it has a length\n\
           \   of even number and  divides equally"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] |] in
              split_matrix array1 1)
             [| [| [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] |] |] );
         ( "testing split matrix when it has a length of odd number\n\
           \   and  divides unequally"
         >:: fun _ ->
           assert_equal
             (let array1 = [| [| 1; 2; 3 |]; [| 4; 5 |]; [| 6; 7 |] |] in
              split_matrix array1 1)
             [| [| [| 1; 2; 3 |]; [| 4; 5 |]; [| 6; 7 |] |] |] );
         ( "test split_int_job_asm when the matrix doesnt split evenly"
         >:: fun _ ->
           let job =
             {
               aint = [| [| 1; 2; 3 |]; [| 4; 5 |]; [| 6; 7 |] |];
               bint = [| [| 10; 20; 30 |]; [| 40; 50 |]; [| 60; 70 |] |];
               opi = "add";
             }
           in
           let ans1 =
             {
               aint = [| [| 1; 2; 3 |] |];
               bint = [| [| 10; 20; 30 |] |];
               opi = "add";
             }
           in
           let ans2 =
             { aint = [| [| 4; 5 |] |]; bint = [| [| 40; 50 |] |]; opi = "add" }
           in
           let ans3 =
             { aint = [| [| 6; 7 |] |]; bint = [| [| 60; 70 |] |]; opi = "add" }
           in
           let expected = IntJobASMSplit [| ans1; ans2; ans3 |] in
           let actual = split_int_job_asm 3 job in
           assert_equal expected actual );
         ( "test split_int_job_asm when the matrix does split evenly"
         >:: fun _ ->
           let job =
             {
               aint = [| [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] |];
               bint = [| [| 10; 20 |]; [| 30; 40 |]; [| 50; 60 |] |];
               opi = "add";
             }
           in
           let ans1 =
             { aint = [| [| 1; 2 |] |]; bint = [| [| 10; 20 |] |]; opi = "add" }
           in
           let ans2 =
             { aint = [| [| 3; 4 |] |]; bint = [| [| 30; 40 |] |]; opi = "add" }
           in
           let ans3 =
             { aint = [| [| 5; 6 |] |]; bint = [| [| 50; 60 |] |]; opi = "add" }
           in
           let expected = IntJobASMSplit [| ans1; ans2; ans3 |] in
           let actual = split_int_job_asm 3 job in
           assert_equal expected actual );
         ( "test split_int_job_asm when the matrix does split evenly but its \
            multiplying"
         >:: fun _ ->
           let job =
             {
               aint = [| [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] |];
               bint = [| [| 10; 20; 30; 40; 50; 60 |] |];
               opi = "multiply";
             }
           in
           let ans1 =
             {
               aint = [| [| 1; 2 |] |];
               bint = [| [| 10; 20; 30; 40; 50; 60 |] |];
               opi = "multiply";
             }
           in
           let ans2 =
             {
               aint = [| [| 3; 4 |] |];
               bint = [| [| 10; 20; 30; 40; 50; 60 |] |];
               opi = "multiply";
             }
           in
           let ans3 =
             {
               aint = [| [| 5; 6 |] |];
               bint = [| [| 10; 20; 30; 40; 50; 60 |] |];
               opi = "multiply";
             }
           in
           let expected = IntJobASMSplit [| ans1; ans2; ans3 |] in
           let actual = split_int_job_asm 3 job in
           assert_equal expected actual );
         ( "test split_float_job_asm when the matrix does split evenly"
         >:: fun _ ->
           let job =
             {
               afloat = [| [| 1.; 2. |]; [| 3.; 4. |]; [| 5.; 6. |] |];
               bfloat = [| [| 10.; 20. |]; [| 30.; 40. |]; [| 50.; 60. |] |];
               opf = "add";
             }
           in
           let ans1 =
             {
               afloat = [| [| 1.; 2. |] |];
               bfloat = [| [| 10.; 20. |] |];
               opf = "add";
             }
           in
           let ans2 =
             {
               afloat = [| [| 3.; 4. |] |];
               bfloat = [| [| 30.; 40. |] |];
               opf = "add";
             }
           in
           let ans3 =
             {
               afloat = [| [| 5.; 6. |] |];
               bfloat = [| [| 50.; 60. |] |];
               opf = "add";
             }
           in
           let expected = FloatJobASMSplit [| ans1; ans2; ans3 |] in
           let actual = split_float_job_asm 3 job in
           assert_equal expected actual );
         ( "test split_float_job_asm when the matrix does split evenly when \
            multiplying"
         >:: fun _ ->
           let job =
             {
               afloat = [| [| 1.; 2. |]; [| 3.; 4. |]; [| 5.; 6. |] |];
               bfloat = [| [| 10.; 20.; 30.; 40.; 50.; 60. |] |];
               opf = "multiply";
             }
           in
           let ans1 =
             {
               afloat = [| [| 1.; 2. |] |];
               bfloat = [| [| 10.; 20.; 30.; 40.; 50.; 60. |] |];
               opf = "multiply";
             }
           in
           let ans2 =
             {
               afloat = [| [| 3.; 4. |] |];
               bfloat = [| [| 10.; 20.; 30.; 40.; 50.; 60. |] |];
               opf = "multiply";
             }
           in
           let ans3 =
             {
               afloat = [| [| 5.; 6. |] |];
               bfloat = [| [| 10.; 20.; 30.; 40.; 50.; 60. |] |];
               opf = "multiply";
             }
           in
           let expected = FloatJobASMSplit [| ans1; ans2; ans3 |] in
           let actual = split_float_job_asm 3 job in
           assert_equal expected actual );
         ( "testing split_int_job_s" >:: fun _ ->
           let job =
             { aint = [| [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] |]; scalar = 3 }
           in
           let ans1 = { aint = [| [| 1; 2 |] |]; scalar = 3 } in
           let ans2 = { aint = [| [| 3; 4 |] |]; scalar = 3 } in
           let ans3 = { aint = [| [| 5; 6 |] |]; scalar = 3 } in
           let expected = IntJobSSplit [| ans1; ans2; ans3 |] in
           let actual = split_int_job_s 3 job in
           assert_equal expected actual );
         ( "testing split_float_job_s" >:: fun _ ->
           let job =
             {
               afloat = [| [| 1.; 2. |]; [| 3.; 4. |]; [| 5.; 6. |] |];
               scalar = 3.;
             }
           in
           let ans1 = { afloat = [| [| 1.; 2. |] |]; scalar = 3. } in
           let ans2 = { afloat = [| [| 3.; 4. |] |]; scalar = 3. } in
           let ans3 = { afloat = [| [| 5.; 6. |] |]; scalar = 3. } in
           let expected = FloatJobSSplit [| ans1; ans2; ans3 |] in
           let actual = split_float_job_s 3 job in
           assert_equal expected actual );
         ( "testing construct_int_matrix" >:: fun _ ->
           let expected = IntMatrix [| [| 0 |]; [| 0 |]; [| 0 |] |] in
           let actual = construct_int_matrix 3 1 in
           assert_equal expected actual );
         ( "testing construct_float_matrix" >:: fun _ ->
           let expected = FloatMatrix [| [| 0. |]; [| 0. |]; [| 0. |] |] in
           let actual = construct_float_matrix 3 1 in
           assert_equal expected actual );
         ( "testing fill_matrix_int" >:: fun _ ->
           let input =
             [| [| 0; 0; 0 |]; [| 0; 0; 0 |]; [| 0; 0; 0 |]; [| 0; 0; 0 |] |]
           in
           let res = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           let () = fill_matrix_int res (1, 2) input in
           let result =
             [| [| 0; 0; 0 |]; [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 0; 0; 0 |] |]
           in
           assert_equal input result );
         ( "testing fill_matrix_float" >:: fun _ ->
           let input =
             [|
               [| 0.; 0.; 0. |];
               [| 0.; 0.; 0. |];
               [| 0.; 0.; 0. |];
               [| 0.; 0.; 0. |];
             |]
           in
           let res = [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |] |] in
           let () = fill_matrix_float res (1, 2) input in
           let result =
             [|
               [| 0.; 0.; 0. |];
               [| 1.; 2.; 3. |];
               [| 4.; 5.; 6. |];
               [| 0.; 0.; 0. |];
             |]
           in
           assert_equal input result );
         ( "testing split_first_space" >:: fun _ ->
           let expected = ("hello", "world program") in
           let actual = split_first_space "hello world program" in
           assert_equal expected actual );
       ]

let _ = run_test_tt_main tests
