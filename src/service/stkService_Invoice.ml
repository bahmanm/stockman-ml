(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module IDb = StkDomainDb.InvoiceDb
module I = StkDomain.Invoice

type save_error = HeaderMismatch | LineMismatch

(*******************************************************************************)
module Utils = struct
  let calc_amt lines =
    List.fold_left
      (fun amt line -> amt +. (line.I.qty |> float_of_int) *. line.I.price)
      0.
      lines

  (*****************************************)
  let header_equal inv1 inv2 =
    if inv1.I.doc_no = inv2.I.doc_no then
      inv1.I.date = inv2.I.date
      && inv1.I.doc_no = inv2.I.doc_no
      && inv1.I.vendor = inv2.I.vendor
      && inv1.I.amt = inv2.I.amt
    else
      true

  (*****************************************)
  let line_equal l1 l2 =
    if l1.I.line_no = l2.I.line_no then
      l1.I.product = l2.I.product
      && l1.I.qty = l2.I.qty
      && l1.I.price = l2.I.price
    else
      true

  (*****************************************)
  let lines_equal_by_line_no lines1 lines2 =
    let find_line lines line =
      List.find (fun l -> l.I.line_no = line.I.line_no) lines in
    List.exists
      (fun l1 -> l1 |> find_line lines2 |> line_equal l1 |> not)
      lines1

  (*****************************************)
  let combine_lines inv1 inv2 =
    let lines1 = inv1.I.lines in
    let lines2 = inv2.I.lines in
    if lines_equal_by_line_no lines1 lines2 then
      let lines_diff =
        List.filter
          (fun l1 ->
             List.exists
               (fun l2 -> l2.I.line_no = l1.I.line_no)
               lines2
             |> not)
          lines1 in
      Some(List.append lines2 lines_diff)
    else
      None
end

(*******************************************************************************)
let save inv db =
  match (db |> IDb.get inv.I.doc_no) with
  | None -> Ok (db |> IDb.save inv)
  | Some inv_db ->
      if Utils.header_equal inv inv_db then
        match Utils.combine_lines inv inv_db with
        | Some lines -> begin
            let amt = Utils.calc_amt lines in
            Ok (db |> IDb.save { inv with I.amt = amt; I.lines = lines })
          end
        | None -> Bad LineMismatch
      else
        Bad HeaderMismatch
