open OUnit2
open Batteries

let () =
  print_endline "Test_Product.suite_product";
  run_test_tt_main Test_Product.suite_product;
  print_endline "Test_DsvLoader.suite_dsv";
  run_test_tt_main Test_DsvLoader.suite_dsv;
  print_endline "Test_TableFormatter.suite_table_formatter";
  run_test_tt_main Test_TableFormatter.suite_table_formatter;
  print_endline "Test_CmdLine.suite_cmd";
  run_test_tt_main Test_CmdLine.suite_cmd
