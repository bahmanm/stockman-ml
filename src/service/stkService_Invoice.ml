(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Purchase Invoice service. *)
open Batteries

module IDb = StkDomainDb.InvoiceDb
module I = StkDomain.Invoice

module Utils = struct
  let combine_lines inv1 inv2 =
    let lines1 = inv1.I.lines in
    let lines2 = inv2.I.lines in
    let lines_diff =
      List.filter
        (fun l1 ->
           List.exists
             (fun l2 -> l2.I.line_no = l1.I.line_no)
             lines2
           |> not)
        lines1 in
    List.append lines2 lines_diff

  let calc_amt lines =
    List.fold_left
      (fun amt line -> amt +. (line.I.qty |> float_of_int) *. line.I.price)
      0.
      lines
end

(** [save inv db] saves [inv] into [db]. In case an invoice already exists
    with the same `id`, it will combine the lines. *)
let save inv db =
  match (db |> IDb.get inv.I.doc_no) with
  | None -> IDb.save inv db
  | Some inv_db -> begin
      let lines = Utils.combine_lines inv inv_db in
      let amt = Utils.calc_amt lines in
      IDb.save
        { inv with I.amt = amt; I.lines = lines }
        db
    end
