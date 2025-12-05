open OUnit2
open Lwt.Infix
open Slurmcaml.Functions
open Slurmcaml.Matrixutils

let tests =
  "test suite"
  >::: [
         ( "parse_matrix_type int" >:: fun _ ->
           let rest_string = ref "int addsdfasdgjkasdjg" in
           let res = parse_matrix_type rest_string in

           assert_equal "int" res;
           assert_equal "addsdfasdgjkasdjg" !rest_string );
         ( "parse_matrix_type float" >:: fun _ ->
           let rest_string = ref "float addsdfasdgjkasdjg" in
           let res = parse_matrix_type rest_string in

           assert_equal "float" res;
           assert_equal "addsdfasdgjkasdjg" !rest_string );
         ( "parse_matrix_type other" >:: fun _ ->
           let rest_string = ref "cheesenugget addsdfasdgjkasdjg" in
           assert_raises
             (InvalidMatrixArgument "unknown matrix type: must be int or float")
             (fun () -> parse_matrix_type rest_string) );
         ( "parse_scale valid" >:: fun _ ->
           let rest_string = ref "67 " in
           let res = parse_scale rest_string in

           assert_equal "67" res );
         ( "parse_op add valid" >:: fun _ ->
           let rest_string = ref "add " in
           let res = process_op rest_string in
           assert_equal "add" res;
           assert_equal "" !rest_string );
         ( "parse_op subtract valid" >:: fun _ ->
           let rest_string = ref "subtract " in
           let res = process_op rest_string in
           assert_equal "subtract" res;
           assert_equal "" !rest_string );
         ( "parse_op multiply valid" >:: fun _ ->
           let rest_string = ref "multiply " in
           let res = process_op rest_string in
           assert_equal "multiply" res;
           assert_equal "" !rest_string );
         ( "parse_op scale valid" >:: fun _ ->
           let rest_string = ref "scale " in
           let res = process_op rest_string in
           assert_equal "scale" res;
           assert_equal "" !rest_string );
         ( "parse_op transpose valid" >:: fun _ ->
           let rest_string = ref "transpose " in
           let res = process_op rest_string in
           assert_equal "transpose" res;
           assert_equal "" !rest_string );
       ]

let _ = run_test_tt_main tests
