open OUnit2;;
open Batteries;;

module D = Stockman.Products.Db;;
module P = Stockman.Products;;
module E = Stockman.Etl;;
module C = Stockman.Cmd;;
  
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
let test_etl_product_of_string_valid_input ctx =
  let () = match E.product_of_string ',' "p1,10" with
    | Ok { P.name="p1"; P.qty=10 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen" in
  let () = match E.product_of_string ';' "p1 ;10 " with
    | Ok { P.name="p1"; P.qty=10 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen" in
  match E.product_of_string '-' "   p1 -  10 " with
    | Ok { P.name="p1"; P.qty=10 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen"

let test_etl_product_of_string_invalid_input ctx =
  let () = match E.product_of_string ',' ",10" with
    | Error msg -> assert_equal msg "Invalid row: ,10"
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ';' "p1, " with
    | Error msg -> assert_equal msg "Invalid row: p1, "
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ',' "p1" with
    | Error msg -> assert_equal msg "Invalid row: p1"
    | _ -> assert_equal 0 1 in
  match E.product_of_string '-' "" with
  | Error msg -> assert_equal msg "Empty row"
  | _ -> assert_equal 0 1;;

let test_etl_db_of_file_valid_input ctx =
  let db = E.db_of_file "res/wp1-products__all-valid.csv" "#" ',' 1 in
  assert_equal 4 (D.size db);;

let test_etl_db_of_file_invalid_input ctx =
  let db1 = E.db_of_file "res/wp1-products__lots-of-invalid-rows.csv" "#" ';' 1 in
  assert_equal 1 (D.size db1);
  let db2 = E.db_of_file "res/wp1-products__lots-of-invalid-rows.csv" "#" '~' 1 in
  assert_equal true (D.is_empty db2);;

let suite_etl =
  "suite_etl">:::
  ["test_etl_product_of_string_valid_input">:: test_etl_product_of_string_valid_input;
   "test_etl_product_of_string_invalid_input">:: test_etl_product_of_string_invalid_input;
   "test_etl_db_of_file_valid_input">:: test_etl_db_of_file_valid_input;
   "test_etl_db_of_file_invalid_input">:: test_etl_db_of_file_invalid_input];;

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
  run_test_tt_main suite_etl;
  run_test_tt_main suite_cmd;;
