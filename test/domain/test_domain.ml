(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let print_name s =
  print_endline ("\n»» test_domain." ^ s)

let () =
  print_name Test_invoice.Validate_header.name;
  run_test_tt_main Test_invoice.Validate_header.suite;

  print_name Test_invoice.Validate_header_aggregate.name;
  run_test_tt_main Test_invoice.Validate_header_aggregate.suite;

  print_name Test_invoice.Validate_lines.name;
  run_test_tt_main Test_invoice.Validate_lines.suite;

  print_name Test_invoice.Validate_all.name;
  run_test_tt_main Test_invoice.Validate_all.suite
