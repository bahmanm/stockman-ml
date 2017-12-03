(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let () =
  print_endline "█ test_service.suite_inventory";
  run_test_tt_main Test_inventory.suite_inventory;
  print_endline "█ test_service.suite_invoice";
  run_test_tt_main Test_invoice.suite_invoice;
