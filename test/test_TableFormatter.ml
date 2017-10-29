open OUnit2
open Batteries

module P = Product
module TF = TableFormatter

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
