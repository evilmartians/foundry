open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "Foundry" >:::
  [Test_big_int_conv.suite]

let _ =
  run_test_tt_main suite
