(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module P = StkDomain.Product
module Dsv = StkDsv.Product
module Params = StkDsv.Params

let test_product_of_string_valid_input ctx =
  let () = match Dsv.product_of_string ',' "p1,10,10.00" with
    | Ok { P.name="p1"; P.qty=10; P.amt=10.0 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen" in
  let () = match Dsv.product_of_string ';' "p1 ;10; 10.00 " with
    | Ok { P.name="p1"; P.qty=10; P.amt=10.0 } -> assert_equal 1 1
    | _ -> assert_failure "shouldn't happen" in
  match Dsv.product_of_string '-' "   p1 -  10- 10.00 " with
  | Ok { P.name="p1"; P.qty=10; P.amt=10.0 } -> assert_equal 1 1
  | _ -> assert_failure "shouldn't happen"

let test_product_of_string_invalid_input ctx =
  let () = match Dsv.product_of_string ',' ",10" with
    | Bad msg -> assert_equal msg "Invalid row: ,10"
    | _ -> assert_equal 0 1 in
  let () = match Dsv.product_of_string ';' "p1, " with
    | Bad msg -> assert_equal msg "Invalid row: p1, "
    | _ -> assert_equal 0 1 in
  let () = match Dsv.product_of_string ',' "p1" with
    | Bad msg -> assert_equal msg "Invalid row: p1"
    | _ -> assert_equal 0 1 in
  match Dsv.product_of_string '-' "" with
  | Bad msg -> assert_equal msg "Empty row"
  | _ -> assert_equal 0 1;;

let test_load_file_valid_input ctx =
  let records = Dsv.load_file {
      Params.path="res/products__all-valid.csv";
      Params.comment_str="#";
      Params.delimiter=',';
      Params.n_header=1
    } in
  assert_equal 4 (BatEnum.count records);;

let test_load_file_invalid_input ctx =
  let records1 = Dsv.load_file {
      Params.path="res/products__lots-of-invalid-rows.csv";
      Params.comment_str="#";
      Params.delimiter=';';
      Params.n_header=1
    } in
  assert_equal 1 (BatEnum.count records1);
  let records2 = Dsv.load_file {
      Params.path="res/products__all-invalid.csv";
      Params.comment_str="#";
      Params.delimiter='~';
      Params.n_header=1
    } in
  assert_equal true (BatEnum.is_empty records2);;

let suite_dsv =
  "suite_dsv">:::
  ["test_product_of_string_valid_input">:: test_product_of_string_valid_input;
   "test_product_of_string_invalid_input">:: test_product_of_string_invalid_input;
   "test_load_file_valid_input">:: test_load_file_valid_input;
   "test_load_file_invalid_input">:: test_load_file_invalid_input];;

let () =
  print_endline "█ test_dsv.suite_dsv";
  run_test_tt_main suite_dsv;
