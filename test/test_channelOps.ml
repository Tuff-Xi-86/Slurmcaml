open OUnit2
open Lwt.Infix
open Slurmcaml.Functions
open Slurmcaml.Matrixutils

let tests =
  "test suite"
  >::: [
         ( "testing read-Int_matrix_input from a channel" >:: fun _ ->
           let original_mat =
             IntMatrix [| [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; 9 |] |]
           in

           let mat =
             Lwt_main.run
               (let r, w = Lwt_io.pipe () in
                let writer =
                  let%lwt () = print_matrix original_mat w in
                  Lwt_io.close w
                in
                let reader = read_int_matrix_input r in
                let%lwt sheepy = Lwt.both writer reader in
                Lwt.return (snd sheepy))
           in

           let expected_mat =
             [| [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; 9 |] |]
           in
           assert_equal expected_mat mat );
         ( "testing read-Int_matrix_input negative numbers from a channel"
         >:: fun _ ->
           let original_mat =
             IntMatrix
               [| [| -1; -2; -3 |]; [| -4; -5; -6 |]; [| -7; -8; -9 |] |]
           in

           let mat =
             Lwt_main.run
               (let r, w = Lwt_io.pipe () in
                let writer =
                  let%lwt () = print_matrix original_mat w in
                  Lwt_io.close w
                in
                let reader = read_int_matrix_input r in
                let%lwt sheepy = Lwt.both writer reader in
                Lwt.return (snd sheepy))
           in

           let expected_mat =
             [| [| -1; -2; -3 |]; [| -4; -5; -6 |]; [| -7; -8; -9 |] |]
           in
           assert_equal expected_mat mat );
         ( "testing read-Float_matrix_input from a channel" >:: fun _ ->
           let original_mat =
             FloatMatrix
               [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9. |] |]
           in

           let mat =
             Lwt_main.run
               (let r, w = Lwt_io.pipe () in
                let writer =
                  let%lwt () = print_matrix original_mat w in
                  Lwt_io.close w
                in
                let reader = read_float_matrix_input r in
                let%lwt sheepy = Lwt.both writer reader in
                Lwt.return (snd sheepy))
           in

           let expected_mat =
             [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9. |] |]
           in
           assert_equal expected_mat mat );
         ( "testing read-Float_matrix_input negative numbers from a channel"
         >:: fun _ ->
           let original_mat =
             FloatMatrix
               [|
                 [| -1.; -2.; -3. |]; [| -4.; -5.; -6. |]; [| -7.; -8.; -9. |];
               |]
           in

           let mat =
             Lwt_main.run
               (let r, w = Lwt_io.pipe () in
                let writer =
                  let%lwt () = print_matrix original_mat w in
                  Lwt_io.close w
                in
                let reader = read_float_matrix_input r in
                let%lwt sheepy = Lwt.both writer reader in
                Lwt.return (snd sheepy))
           in

           let expected_mat =
             [| [| -1.; -2.; -3. |]; [| -4.; -5.; -6. |]; [| -7.; -8.; -9. |] |]
           in
           assert_equal expected_mat mat );
       ]

let _ = run_test_tt_main tests
