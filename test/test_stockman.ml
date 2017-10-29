open OUnit2
open Batteries

module D = Product.Db
module P = Product
module E = DsvLoader
module C = Stockman.Cmd
module TF = Stockman.TableFormatter
  
(******************************************************************************)
let test_table_formatter_single_row ctx =
  let product_list = [
    { P.name = "P1-2334"; P.qty = 12 }
  ] in
  let expected_result =
    "
+==================================================+==========================+
|         Product Name                             |      Available Qty       |
+==================================================+==========================+
| P1-2334                                          | 12                       |
+==================================================+==========================+
" in
  let actual_result = TF.format product_list in
  assert_equal
    ~printer:(fun s -> s)
    expected_result actual_result;;

let test_table_formatter_long_name ctx =
  let product_list = [
    { P.name = "P-0123456789-0123456789-0123456789-0123456789-0123456789-0123456789"; P.qty = 12 }
  ] in
  let expected_result =
    "
+==================================================+==========================+
|         Product Name                             |      Available Qty       |
+==================================================+==========================+
| P-0123456789-0123456789-0123456789-0123456789-01 | 12                       |
+==================================================+==========================+
" in
  let actual_result = TF.format product_list in
  assert_equal
    ~printer:(fun s -> s)
    expected_result actual_result;;

let test_table_formatter_multiple_rows ctx =
  let product_list = [
    { P.name = "P1-2334"; P.qty = 12 };
    { P.name = "AA90 (23.45)"; P.qty = 606 };
    { P.name = "P1-2337"; P.qty = 3811 };
    { P.name = "CVCD Lorem Ipsum"; P.qty = 8 };
    { P.name = "G2"; P.qty = 75 };
  ] in
  let expected_result =
    "
+==================================================+==========================+
|         Product Name                             |      Available Qty       |
+==================================================+==========================+
| P1-2334                                          | 12                       |
+--------------------------------------------------+--------------------------+
| AA90 (23.45)                                     | 606                      |
+--------------------------------------------------+--------------------------+
| P1-2337                                          | 3811                     |
+--------------------------------------------------+--------------------------+
| CVCD Lorem Ipsum                                 | 8                        |
+--------------------------------------------------+--------------------------+
| G2                                               | 75                       |
+==================================================+==========================+
" in
  let actual_result = TF.format product_list in
  assert_equal
    ~printer:(fun s -> s)
    expected_result actual_result;;

let suite_table_formatter =
  "suite_table_formatter">:::
  ["test_table_formatter_single_row">:: test_table_formatter_single_row;
   "test_table_formatter_long_name">:: test_table_formatter_long_name;
   "test_table_formatter_multiple_rows">:: test_table_formatter_multiple_rows];;

(******************************************************************************)
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
  run_test_tt_main suite_table_formatter;
  run_test_tt_main suite_cmd;;
