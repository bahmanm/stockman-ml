open OUnit2
open Batteries

let () =
  print_endline "\n»» Test_CmdLine.suite_cmd";
  run_test_tt_main Test_CmdLine.suite_cmd
