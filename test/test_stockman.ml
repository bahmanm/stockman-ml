open OUnit2;;
open Batteries;;

module D = Product.Db;;
module P = Product;;
module E = Stockman.DsvLoader;;
module C = Stockman.Cmd;;
module TF = Stockman.TableFormatter;;
  
let test_product_db_add ctx =
  let db = D.empty in
  assert_equal 0 (D.size db);
  let db = db |> D.add { P.name="foo"; P.qty=1 } in
  assert_equal 1 (D.size db);
  let db = db |> D.add { P.name="bar"; P.qty=1 } in
  assert_equal 2 (D.size db);;
      
let test_product_db_empty ctx =
  assert_equal 0 (D.size D.empty);;

let suite_product =
  "suite_product">:::
  ["test_product_db_add">:: test_product_db_add;
   "test_product_db_empty">:: test_product_db_empty];;

(******************************************************************************)
let test_dsv_product_of_string_valid_input ctx =
  let () = match E.product_of_string ',' "p1,10" with
    | Ok { P.name="p1"; P.qty=10 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen" in
  let () = match E.product_of_string ';' "p1 ;10 " with
    | Ok { P.name="p1"; P.qty=10 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen" in
  match E.product_of_string '-' "   p1 -  10 " with
    | Ok { P.name="p1"; P.qty=10 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen"

let test_dsv_product_of_string_invalid_input ctx =
  let () = match E.product_of_string ',' ",10" with
    | Bad msg -> assert_equal msg "Invalid row: ,10"
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ';' "p1, " with
    | Bad msg -> assert_equal msg "Invalid row: p1, "
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ',' "p1" with
    | Bad msg -> assert_equal msg "Invalid row: p1"
    | _ -> assert_equal 0 1 in
  match E.product_of_string '-' "" with
  | Bad msg -> assert_equal msg "Empty row"
  | _ -> assert_equal 0 1;;

let test_dsv_db_of_file_valid_input ctx =
  let db = E.db_of_file "res/wp1-products__all-valid.csv" "#" ',' 1 in
  assert_equal 4 (D.size db);;

let test_dsv_db_of_file_invalid_input ctx =
  let db1 = E.db_of_file "res/wp1-products__lots-of-invalid-rows.csv" "#" ';' 1 in
  assert_equal 1 (D.size db1);
  let db2 = E.db_of_file "res/wp1-products__lots-of-invalid-rows.csv" "#" '~' 1 in
  assert_equal true (D.is_empty db2);;

let suite_dsv =
  "suite_dsv">:::
  ["test_dsv_product_of_string_valid_input">:: test_dsv_product_of_string_valid_input;
   "test_dsv_product_of_string_invalid_input">:: test_dsv_product_of_string_invalid_input;
   "test_dsv_db_of_file_valid_input">:: test_dsv_db_of_file_valid_input;
   "test_dsv_db_of_file_invalid_input">:: test_dsv_db_of_file_invalid_input];;

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
  run_test_tt_main suite_product;
  run_test_tt_main suite_dsv;
  run_test_tt_main suite_table_formatter;
  run_test_tt_main suite_cmd;;
