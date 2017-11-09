(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

(** Purchase invoice line. *)
type invoice_line = {
  line_no : int;
  product : string;
  price : float;
  qty : int;
}

(** Purchase invoice. *)
type invoice = {
  doc_no : string;
  date : string;
  vendor : string;
  amt : float;
  lines : invoice_line list;
}

let validate pi =
  let validate_line l =
    if String.is_empty l.product then
      Some (l, "product")
    else if l.price < 0.0 then
      Some (l, "price")
    else if l.qty < 0 then
      Some (l, "qty")
    else
      None
  in
  try
    let (line, field) = BatList.find_map (fun l -> validate_line l) pi.lines in
    Bad ("line." ^ (string_of_int line.line_no) ^ "." ^ field)
  with Not_found ->
    if String.is_empty pi.doc_no then
      Bad "doc_no"
    else if String.is_empty pi.vendor then
      Bad "vendor"
    else if
      not (Str.string_match
             (Str.regexp "^[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]$")
             pi.date
             0)
    then
      Bad "date"
    else if
      let sum_lines = BatList.fold_left
          (fun acc line -> acc +. line.price *. (float_of_int line.qty))
          0.0
          pi.lines in
      pi.amt < 0.0 || pi.amt <> sum_lines
    then
      Bad "amt"
    else
      Ok pi
