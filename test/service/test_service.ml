(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let print_name s =
  print_endline ("\n»» test_service." ^ s)

let () =
  print_name Test_inventory.Stock_in.name;
  run_test_tt_main Test_inventory.Stock_in.suite;
  
  print_name Test_invoice.Save.name;
  run_test_tt_main Test_invoice.Save.suite
