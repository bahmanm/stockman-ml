(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module I = StkDomain.Invoice
module IDb = StkDomainDb.InvoiceDb
module S = StkService.Invoice

(**********************************************************)
module Fixtures = struct
  let line0 = {
    I.line_no = 1;
    product = "P1";
    price = 10.;
    qty = 5
  }
  let line1 = {
    I.line_no = 2;
    product = "P2";
    price = 2.;
    qty = 3
  }
  let line2 = {
    I.line_no = 3;
    product = "P3";
    price = 11.5;
    qty = 1
  }
  let line3 = {
    I.line_no = 1;
    product = "P2";
    price = 2.;
    qty = 3
  }
end

(**********************************************************)
module Compare = struct
  let invoice_line l1 l2 ctx =
    assert_equal (l1.I.line_no) (l2.I.line_no);
    assert_equal (l1.I.product) (l2.I.product);
    assert_equal (l1.I.qty) (l2.I.qty);
    assert_equal (l1.I.price) (l2.I.price)
  
  let invoice inv1 inv2 ctx =
    assert_equal (inv1.I.doc_no) (inv2.I.doc_no);
    assert_equal (inv1.I.vendor) (inv2.I.vendor);
    assert_equal (inv1.I.date) (inv2.I.date);
    assert_equal (inv1.I.amt) (inv2.I.amt);
    assert_equal (List.length inv1.I.lines) (List.length inv2.I.lines);
    let lines1 = List.sort
        (fun e1 e2 -> e1.I.line_no - e2.I.line_no)
        (inv1.I.lines) in
    let lines2 = List.sort
        (fun e1 e2 -> e1.I.line_no - e2.I.line_no)
        (inv2.I.lines) in
    List.iter2
      (fun l1 l2 -> invoice_line l1 l2 ctx)
      lines1 lines2
end

(************************)
let test_new_invoice ctx =
  let inv = {
    I.doc_no = "I1";
    date = "2017/12/01";
    vendor = "VENDOR1";
    amt = 50.;
    lines = [Fixtures.line0]
  } in
  let db = (S.save inv IDb.empty) in
  match IDb.get "I1" db with
  | None -> assert false
  | Some i -> Compare.invoice i inv ctx

(************************)
let test_existing_invoice_new_line ctx =
  let inv_0 = {
    I.doc_no = "I1"; date = "2017/12/01";
    vendor = "VENDOR1"; amt = 50.;
    lines = [Fixtures.line0]
  } in
  let db_0 = (IDb.empty |> S.save inv_0) in
  let inv_1 = {
    inv_0 with
    I.lines = [Fixtures.line0; Fixtures.line1];
    I.amt = 56.0
  } in
  let db_1 = (db_0 |> S.save inv_1) in
  match (IDb.get "I1" db_1) with
  | None -> assert false
  | Some i -> Compare.invoice i inv_1 ctx

(************************)
let test_existing_invoice_new_single_line ctx =
  let inv_0 = {
    I.doc_no = "I1"; date = "2017/12/01";
    vendor = "VENDOR1"; amt = 50.;
    lines = [Fixtures.line0]
  } in
  let db_0 = (IDb.empty |> S.save inv_0) in
  let inv_1 = {
    I.doc_no = inv_0.I.doc_no; date = inv_0.I.date;
    vendor = inv_0.I.vendor; amt = 56.;
    lines = [Fixtures.line1]
  } in
  let db_1 = (db_0 |> S.save inv_1) in
  let expected = {
    inv_1 with
    I.lines = [Fixtures.line0; Fixtures.line1]
  } in
  match (IDb.get "I1" db_1) with
  | None -> assert false
  | Some i -> Compare.invoice i expected ctx

(************************)
let suite_invoice =
  "suite_invoice">:::
  ["test_new_invoice">:: test_new_invoice;
   "test_existing_invoice_new_line">:: test_existing_invoice_new_line;
   "test_existing_invoice_new_single_line">:: test_existing_invoice_new_single_line]
