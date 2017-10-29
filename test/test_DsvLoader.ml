(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2;;
open Batteries;;

module D = Product.Db;;
module P = Product;;
module E = DsvLoader;;

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
