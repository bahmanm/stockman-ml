(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let pass = assert true
let fail = assert false
let name = "test_invoice."
module Invoice = StkDomain.Invoice

(**********************************************************)
module Validate_lines = struct
  let name = name ^ "validate_lines"

  (************************)
  let test_ok ctx =
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

  (************************)
  let test_invalid_product ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
      amt = 50.0; lines = [
          {Invoice.line_no = 10; product = ""; price = 10.0; qty = 1}
        ]
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "line.10.product"

  (************************)
  let test_invalid_price ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
      amt = 50.0; lines = [
          {Invoice.line_no = 10; product = "p1"; price = -10.0; qty = 1}
        ]
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "line.10.price"

  (************************)
  let test_invalid_qty ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
      amt = 50.0; lines = [
          {Invoice.line_no = 10; product = "p1"; price = 10.0; qty = -1}
        ]
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "line.10.qty"

  (************************)
  let suite =
    name >:::
    ["invalid price should be reject"
     >:: test_invalid_price;
     "invalid qty should be rejected"
     >:: test_invalid_qty;
     "invalid product should be rejected"
     >:: test_invalid_product;
     "all valid"
     >:: test_ok]
end

(**********************************************************)
module Validate_header_aggregate = struct
  let name = name ^ "validate_header_aggrate"

  (************************)
  let test_no_lines_amt_0 ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
      amt = 0.0; lines = []
    } in
    match Invoice.validate inv with
    | Ok _ -> assert true
    | Bad s -> assert_failure ("shouldn't happen: " ^ s)

  (************************)
  let test_no_lines_amt_not_0 ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
      amt = 1.0; lines = []
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "amt"

  (************************)
  let test_amt_negative ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "v1";
      amt = -10.0; lines = []
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "amt"

  (************************)
  let test_amt_not_equal_sum_of_lines ctx =
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

  (************************)
  let suite =
    name >:::
    ["amt should match sum of line amts even if no lines"
     >:: test_no_lines_amt_0;
     "amt should be 0 if no lines"
     >:: test_no_lines_amt_not_0;
     "amt should match sum of line amts"
     >:: test_amt_not_equal_sum_of_lines;
     "amt should not be negative"
     >:: test_amt_negative]
end

module Validate_header = struct
  let name = name ^ "validate_header"

  (************************)
  let test_invalid_doc_no ctx =
    let inv = {
      Invoice.doc_no = ""; date = "2018/01/01"; vendor = "v1";
      amt = 0.0; lines = []
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "doc_no"

  (************************)
  let test_invalid_date ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/0101"; vendor = "v1";
      amt = 0.0; lines = []
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "date"

  (************************)
  let test_invalid_vendor ctx =
    let inv = {
      Invoice.doc_no = "pi123"; date = "2018/01/01"; vendor = "";
      amt = 0.0; lines = []
    } in
    match Invoice.validate inv with
    | Ok _ -> assert_failure "shouldn't happen"
    | Bad s -> assert_equal s "vendor"

  let suite =
    name >:::
    ["reject invalid doc_no"
     >:: test_invalid_doc_no;
     "reject invalid date"
     >:: test_invalid_date;
     "reject invalid vendor"
     >:: test_invalid_vendor]
end

module Validate_all = struct
  let name = name ^ "validate_all"

  (************************)
  let test_ok ctx =
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

  (************************)
  let suite =
    name >:::
    ["ok"
     >:: test_ok]
end
