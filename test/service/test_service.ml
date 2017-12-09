(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let () =
  print_endline "█ test_service.test_inventory";
  run_test_tt_main Test_inventory.suite_inventory;
  print_endline "█ test_service.test_invoice.save";
  run_test_tt_main Test_invoice.suite_save;
