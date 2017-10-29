open OUnit2
open Batteries

module C = Stockman.Cmd
  
let test_cmd_all_options_present ctx =
  let argv = [|"app_name"; "-f"; "/home/bahman/t.tmp"; "-d"; ";"|] in
  match C.parse argv with
  | Ok opts ->
    assert_equal (Some ';') opts.C.field_delim;
    assert_equal (Some "/home/bahman/t.tmp") opts.C.file_path
  | Bad _ -> assert_failure "shouldn't happen";;

let test_cmd_only_file_path_options_present ctx =
  let argv = [|"app_name"; "-f"; "/home/bahman/t.tmp"|] in
  match C.parse argv with
  | Ok opts ->
    assert_equal (Some ',') opts.C.field_delim;
    assert_equal (Some "/home/bahman/t.tmp") opts.C.file_path
  | Bad _ -> assert_failure "shouldn't happen";;

let suite_cmd =
  "suite_cmd">:::
  ["test_cmd_all_options_present">:: test_cmd_all_options_present;
   "test_cmd_only_file_path_options_present">:: test_cmd_only_file_path_options_present];;

let () =
  run_test_tt_main Test_Product.suite_product;
  run_test_tt_main Test_DsvLoader.suite_dsv;
  run_test_tt_main Test_TableFormatter.suite_table_formatter;
  run_test_tt_main suite_cmd;;
