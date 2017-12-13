(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module I = StkDomain.Invoice
module IDb = StkDomainDb.InvoiceDb
module S = StkService.Invoice

let pass = assert true
let fail = assert false
let name = "test_invoice."

(**********************************************************)
module Fixtures = struct
  let hdr0 = {
    I.date = "2017/01/01";
    I.doc_no = "I1";
    I.vendor = "V1";
    I.amt = 100.00;
    lines = []
  }
  let hdr0_1 = {
    hdr0 with I.date = "2017/01/02"
  }
  let hdr0_2 = {
    hdr0 with I.vendor = "V2"
  }
  let hdr0_3 = {
    hdr0 with I.amt = 110.00
  }

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

(**********************************************************)
module Save = struct
  let name = name ^ "save"

  (************************)
  let new_invoice ctx =
    let inv = {
      I.doc_no = "I1";
      date = "2017/12/01";
      vendor = "VENDOR1";
      amt = 50.;
      lines = [Fixtures.line0]
    } in
    match (S.save inv IDb.empty) with
    | Ok db -> begin
        match IDb.get "I1" db with
        | Some i -> Compare.invoice i inv ctx
        | _ -> fail
      end
    | _ -> fail

  (************************)
  let only_new_lines_are_saved ctx =
    let inv_0 = {
      I.doc_no = "I1"; date = "2017/12/01";
      vendor = "VENDOR1"; amt = 50.;
      lines = [Fixtures.line0]
    } in
    match (IDb.empty |> S.save inv_0) with
    | Ok db -> begin
        let inv_1 = {
          inv_0 with
          I.lines = [Fixtures.line0; Fixtures.line1];
          I.amt = 56.0
        } in
        match (db |> S.save inv_1) with
        | Ok db -> begin
            match (IDb.get "I1" db) with
            | Some i -> Compare.invoice i inv_1 ctx
            | _ -> fail
          end
        | _ -> fail
      end
    | _ -> fail

  (************************)
  let existing_lines_are_not_deleted ctx =
    let inv_0 = {
      I.doc_no = "I1"; date = "2017/12/01";
      vendor = "VENDOR1"; amt = 50.;
      lines = [Fixtures.line0]
    } in
    match (IDb.empty |> S.save inv_0) with
    | Ok db -> begin
        let inv_1 = {
          I.doc_no = inv_0.I.doc_no; date = inv_0.I.date;
          vendor = inv_0.I.vendor; amt = 56.;
          lines = [Fixtures.line1]
        } in
        match (db |> S.save inv_1) with
        | Ok db -> begin
            let expected = {
              inv_1 with
              I.lines = [Fixtures.line0; Fixtures.line1]
            } in
            match (db |> IDb.get "I1") with
            | Some i -> Compare.invoice i expected ctx
            | None -> fail
          end
        | _ -> fail
      end
    | _ -> fail


  (************************)
  let amt_is_automatically_adjusted ctx =
    let inv_0 = {
      I.doc_no = "I1"; date = "2017/12/01";
      vendor = "VENDOR1"; amt = 50.;
      lines = [Fixtures.line0]
    } in
    match (IDb.empty |> S.save inv_0) with
    | Ok db ->
      begin
        let inv_1 = {
          I.doc_no = inv_0.I.doc_no; date = inv_0.I.date;
          vendor = inv_0.I.vendor; amt = 6.;
          lines = [Fixtures.line1]
        } in
        match (db |> S.save inv_1) with
        | Ok db ->
          begin
            let expected = {
              inv_1 with
              I.amt = (inv_0.I.amt +. inv_1.I.amt);
              lines = [Fixtures.line0; Fixtures.line1]
            } in
            match (IDb.get "I1" db) with
            | Some i -> Compare.invoice i expected ctx
            | None -> fail
          end
        | _ -> fail
      end
    | _ -> fail

  (************************)
  let hdrs_not_match ctx =
    let module F = Fixtures in
    let inv_0 = { F.hdr0 with I.lines = [F.line0] } in
    match (IDb.empty |> S.save inv_0) with
    | Ok db ->
      begin
        (match (db |> S.save { F.hdr0_1 with I.lines = [F.line1] }) with
         | Bad S.HeaderMismatch -> pass
         | _ -> fail);
        (match (db |> S.save { F.hdr0_2 with I.lines = [F.line1] }) with
         | Bad S.HeaderMismatch -> pass
         | _ -> fail);
        (match (db |> S.save { F.hdr0_3 with I.lines = [F.line1] }) with
         | Bad S.HeaderMismatch -> pass
         | _ -> fail);
      end
    | _ -> fail

  let suite =
    name>:::
    ["new invoice"
     >:: new_invoice;
     "existing lines are not deleted"
     >:: existing_lines_are_not_deleted;
     "only new lines are saved"
     >:: only_new_lines_are_saved;
     "amt is automically adjusted"
     >:: amt_is_automatically_adjusted;
     "skip if headers don't match"
     >:: hdrs_not_match]
end
