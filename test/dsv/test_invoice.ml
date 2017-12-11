(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module I = StkDomain.Invoice
module Dsv = StkDsv.Invoice

let name = "test_invoice."

(**********************************************************)
module Invoice_of_string = struct
  let name = name ^ "invoice_of_string"

  (********************************************************)
  module Fixtures = struct
    (* DATE,LINE,DOCNO,PRODUCT,VENDOR,PRICE,QTY,TOTAL *)
    let delim = ','
    let empty = [
      ""
    ]
    let missing_field = [
      "2017/01/01,1,I1,P1,V1";
      "2017/01/01,1,I1,P1,V1,1.00,1.00"
    ]
    let invalid_date = [
      "2017/01,1,I1,P1,V1,1.00,1,1.00";
      "2017/01/0a,1,I1,P1,V1,1.00,1,1.00"
    ]
    let invalid_line_no = [
      "2017/01/01,a,I1,P1,V1,1.00,1,1.00";
      "2017/01/01,1,,P1,V1,1.00,1,1.00"
    ]
    let invalid_product = [
      "2017/01/01,1,I1,,V1,1.00,1,1.00"
    ]
    let invalid_vendor = [
      "2017/01/01,1,I1,P1,,1.00,1,1.00"
    ]
    let invalid_price = [
      "2017/01/01,1,I1,P1,V1,-1.00,1,1.00";
      "2017/01/01,1,I1,P1,V1,.00,1,1.00";
      "2017/01/01,1,I1,P1,V1,a.00,1,1.00";
      "2017/01/01,1,I1,P1,V1,a1.00,1,1.00"
    ]
    let invalid_qty = [
      "2017/01/01,1,I1,P1,V1,1.00,-1,1.00";
      "2017/01/01,1,I1,P1,V1,1.00,1a,1.00";
      "2017/01/01,1,I1,P1,V1,1.00,1.1,1.00"
    ]
    let invalid_amt = [
      "2017/01/01,1,I1,P1,V1,1.00,1,-1.00";
      "2017/01/01,1,I1,P1,V1,1.00,1,.00";
      "2017/01/01,1,I1,P1,V1,1.00,1,a.00";
      "2017/01/01,1,I1,P1,V1,1.00,1,a1.00"
    ]
    let valid = [
      "2017/01/01,1,I1,P1,V1,1.00,1,1.00"
    ]
  end

  (************************)
  let invoice_of_string = Dsv.invoice_of_string Fixtures.delim
  let run_input_list list =
    List.iter
      (fun input ->
         match (invoice_of_string input) with
         | Ok _ -> assert_failure "shouldn't happen"
         | Bad msg -> assert_equal msg ("Invalid row: " ^ input))
      list

  (************************)
  let test_empty_string ctx = Fixtures.empty |> run_input_list
  let test_missing_field ctx = Fixtures.missing_field |> run_input_list
  let test_invalid_date ctx = Fixtures.invalid_date |> run_input_list
  let test_invalid_line_no ctx = Fixtures.invalid_line_no |> run_input_list
  let test_invalid_product ctx = Fixtures.invalid_product |> run_input_list
  let test_invalid_vendor ctx = Fixtures.invalid_vendor |> run_input_list
  let test_invalid_price ctx = Fixtures.invalid_price |> run_input_list
  let test_invalid_qty ctx = Fixtures.invalid_qty |> run_input_list
  let test_invalid_amt ctx = Fixtures.invalid_amt |> run_input_list
  let test_valid ctx = Fixtures.valid |> run_input_list

  (************************)
  let suite =
    name >:::
    ["empty string"
     >:: test_empty_string;
     "missing field"
     >:: test_missing_field;
     "invalid date"
     >:: test_invalid_date;
     "invalid line no"
     >:: test_invalid_line_no;
     "invalid product"
     >:: test_invalid_product;
     "invalid vendor"
     >:: test_invalid_vendor;
     "invalid price"
     >:: test_invalid_vendor;
     "invalid price"
     >:: test_invalid_price;
     "invalid_qty"
     >:: test_invalid_qty;
     "invalid amt"
     >:: test_invalid_amt;
     "valid"
     >:: test_valid]
end

(**********************************************************)
module Load_file = struct
  let name = name ^ "load_file"

  (************************)
  let test_valid_input ctx =
    assert false

  (************************)
  let test_invalid_input ctx =
    assert false

  (************************)
  let suite =
    name >:::
    ["valid input"
     >:: test_valid_input;
     "invalid_input"
     >:: test_invalid_input]
end
