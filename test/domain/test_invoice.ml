(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module Invoice = StkDomain.Invoice

let test_validate_no_lines_amt_0 ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 0.0; lines = []
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_equal 0 0
  | Bad s -> assert_failure ("shouldn't happen: " ^ s)

let test_validate_no_lines_amt_not_0 ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 1.0; lines = []
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "amt"

let test_validate_lines_ok ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 50.0; lines = [
        {Invoice.line_no = 10; product = "p1"; price = 10.0; qty = 1};
        {Invoice.line_no = 20; product = "p2"; price = 20.0; qty = 2};
      ]
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_equal 0 0
  | Bad _ -> assert_failure "shouldn't happen"

let test_validate_invalid_lines_product ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 50.0; lines = [
        {Invoice.line_no = 10; product = ""; price = 10.0; qty = 1}
      ]
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "line.10.product"

let test_validate_invalid_lines_price ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 50.0; lines = [
        {Invoice.line_no = 10; product = "p1"; price = -10.0; qty = 1}
      ]
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "line.10.price"

let test_validate_invalid_lines_qty ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 50.0; lines = [
        {Invoice.line_no = 10; product = "p1"; price = 10.0; qty = -1}
      ]
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "line.10.qty"

let test_validate_invalid_header_doc_no ctx =
  let inv = {
    Invoice.doc_no = ""; date = "2018/01/01"; vendor = "v1";
    amt = 0.0; lines = []
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "doc_no"

let test_validate_invalid_header_date ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/0101"; vendor = "v1";
    amt = 0.0; lines = []
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "date"

let test_validate_invalid_header_vendor ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "";
    amt = 0.0; lines = []
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "vendor"

let test_validate_invalid_header_amt ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = -10.0; lines = []
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "amt"

let test_validate_invalid_header_amt_not_equal_sum_of_lines ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 40.0; lines = [
        {Invoice.line_no = 10; product = "p1"; price = 10.0; qty = 1};
        {Invoice.line_no = 20; product = "p2"; price = 20.0; qty = 2};
      ]
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_failure "shouldn't happen"
  | Bad s -> assert_equal s "amt"

let test_validate_all_ok ctx =
  let inv = {
    Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
    amt = 50.0; lines = [
        {Invoice.line_no = 10; product = "p1"; price = 10.0; qty = 1};
        {Invoice.line_no = 20; product = "p2"; price = 20.0; qty = 2};
      ]
  } in
  match Invoice.validate inv with
  | Ok _ -> assert_equal 0 0
  | Bad _ -> assert_failure "shouldn't happen"

let suite_invoice =
  "suite_invoice">:::
  ["test_validate_no_lines_amt_0">:: test_validate_no_lines_amt_0;
   "test_validate_no_lines_amt_not_0">:: test_validate_no_lines_amt_not_0;
   "test_validate_lines_ok">:: test_validate_lines_ok;
   "test_validate_invalid_lines_productn">:: test_validate_invalid_lines_product;
   "test_validate_invalid_lines_price">:: test_validate_invalid_lines_price;
   "test_validate_invalid_lines_qty">:: test_validate_invalid_lines_qty;
   "test_validate_invalid_header_doc_no">:: test_validate_invalid_header_doc_no;
   "test_validate_invalid_header_date">:: test_validate_invalid_header_date;
   "test_validate_invalid_header_vendor">:: test_validate_invalid_header_vendor;
   "test_validate_invalid_header_amt">:: test_validate_invalid_header_amt;
   "test_validate_invalid_header_amt_not_equal_sum_of_lines">:: test_validate_invalid_header_amt_not_equal_sum_of_lines;
   "test_validate_all_ok">:: test_validate_all_ok]
