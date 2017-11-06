(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let () =
  print_endline "█ test_domain.suite_invoice";
  run_test_tt_main Test_invoice.suite_invoice
