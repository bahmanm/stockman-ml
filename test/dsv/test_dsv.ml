(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let print_name s =
  print_endline ("\n»» test_dsv." ^ s)

let () =
  print_name Test_product.Product_of_string.name;
  run_test_tt_main Test_product.Product_of_string.suite;

  print_name Test_invoice.Invoice_of_string.name;
  run_test_tt_main Test_invoice.Invoice_of_string.suite;

  print_name Test_invoice.Load_file.name;
  run_test_tt_main Test_invoice.Load_file.suite
