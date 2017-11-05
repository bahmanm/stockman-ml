(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_dummy ctx =
  assert_equal 0 0

let suite_domain =
  "suite_domain">:::
  ["test_dummy">:: test_dummy]

let () =
  print_endline "█ test_domain.suite_domain";
  run_test_tt_main suite_domain
