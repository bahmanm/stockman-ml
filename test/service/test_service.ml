(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_dummy ctx =
  assert_equal 0 0

let suite_service =
  "suite_service">:::
  ["test_dummy">:: test_dummy]

let () =
  print_endline "█ test_service.suite_service";
  run_test_tt_main suite_service
