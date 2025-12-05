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
         ( "test check_scalar_float with invalid argument" >:: fun _ ->
           assert_raises
             (InvalidMatrixArgument
                "Matrix scalar must be an float / match that of your matrix \
                 type") (fun () -> check_scalar_float "a") );
         ( "test check_scalar_float with valid argument" >:: fun _ ->
           let expected = () in
           let actual = check_scalar_float ".0" in
           assert_equal expected actual );
         ( "test check_scalar_int with invalid argument" >:: fun _ ->
           assert_raises
             (InvalidMatrixArgument
                "Matrix scalar must be an float / match that of your matrix \
                 type") (fun () -> check_scalar_float "a") );
         ( "test check_scalar_float with valid argument" >:: fun _ ->
           let expected = () in
           let actual = check_scalar_int "0" in
           assert_equal expected actual );
         ( "test determine assignments int job asm" >:: fun _ ->
           let job =
             IntJobASM
               {
                 aint = [| [| 1; 2 |]; [| 3; 4 |] |];
                 bint = [| [| 1; 2 |]; [| 3; 4 |] |];
                 opi = "add";
               }
           in
           let users = Hashtbl.create 10 in
           let () =
             Hashtbl.add users "127" ("dummy", Lwt_io.null, Lwt_io.null, true)
           in
           let () =
             Hashtbl.add users "1272" ("dummy2", Lwt_io.null, Lwt_io.null, true)
           in
           let expectedtbl = Hashtbl.create 10 in
           let () = Hashtbl.add expectedtbl "127" (0, 1) in
           let () = Hashtbl.add expectedtbl "1272" (1, 2) in
           let actual = determine_assignments job users in
           match actual with
           | kind, tbl, matrix, count ->
               assert_equal kind IntJobASMType;
               assert_equal (IntMatrix [| [| 0; 0 |]; [| 0; 0 |] |]) matrix;
               assert_equal count 0 );
         ( "test determine assignments" >:: fun _ ->
           let job =
             FloatJobASM
               {
                 afloat = [| [| 1.; 2. |]; [| 3.; 4. |] |];
                 bfloat = [| [| 1.; 2. |]; [| 3.; 4. |] |];
                 opf = "add";
               }
           in
           let users = Hashtbl.create 10 in
           let () =
             Hashtbl.add users "127" ("dummy", Lwt_io.null, Lwt_io.null, true)
           in
           let () =
             Hashtbl.add users "1272" ("dummy2", Lwt_io.null, Lwt_io.null, true)
           in
           let expectedtbl = Hashtbl.create 10 in
           let () = Hashtbl.add expectedtbl "127" (0, 1) in
           let () = Hashtbl.add expectedtbl "1272" (1, 2) in
           let actual = determine_assignments job users in
           match actual with
           | kind, tbl, matrix, count ->
               assert_equal kind FloatJobASMType;
               assert_equal
                 (FloatMatrix [| [| 0.; 0. |]; [| 0.; 0. |] |])
                 matrix;
               assert_equal count 0 );
         ( "test determine assignments int job scale" >:: fun _ ->
           let job =
             IntJobS { aint = [| [| 1; 2 |]; [| 3; 4 |] |]; scalar = 3 }
           in
           let users = Hashtbl.create 10 in
           let () =
             Hashtbl.add users "127" ("dummy", Lwt_io.null, Lwt_io.null, true)
           in
           let () =
             Hashtbl.add users "1272" ("dummy2", Lwt_io.null, Lwt_io.null, true)
           in
           let expectedtbl = Hashtbl.create 10 in
           let () = Hashtbl.add expectedtbl "127" (0, 1) in
           let () = Hashtbl.add expectedtbl "1272" (1, 2) in
           let actual = determine_assignments job users in
           match actual with
           | kind, tbl, matrix, count ->
               assert_equal kind IntJobSType;
               assert_equal (IntMatrix [| [| 0; 0 |]; [| 0; 0 |] |]) matrix;
               assert_equal count 0 );
         ( "test determine assignments float job scale" >:: fun _ ->
           let job =
             FloatJobS
               { afloat = [| [| 1.; 2. |]; [| 3.; 4. |] |]; scalar = 3. }
           in
           let users = Hashtbl.create 10 in
           let () =
             Hashtbl.add users "127" ("dummy", Lwt_io.null, Lwt_io.null, true)
           in
           let () =
             Hashtbl.add users "1272" ("dummy2", Lwt_io.null, Lwt_io.null, true)
           in
           let expectedtbl = Hashtbl.create 10 in
           let () = Hashtbl.add expectedtbl "127" (0, 1) in
           let () = Hashtbl.add expectedtbl "1272" (1, 2) in
           let actual = determine_assignments job users in
           match actual with
           | kind, tbl, matrix, count ->
               assert_equal kind FloatJobSType;
               assert_equal
                 (FloatMatrix [| [| 0.; 0. |]; [| 0.; 0. |] |])
                 matrix;
               assert_equal count 0 );
       ]

let _ = run_test_tt_main tests
