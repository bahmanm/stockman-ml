(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Invoice DSV importer *)
(* TODO contents are dummy ones *)

open Batteries

module I = StkDomain.Invoice

let invoice_of_string delimiter s =
  Bad ("" ^ s)

let load_file params =
  Enum.ising { I.doc_no = ""; date = ""; amt = 0.; vendor = ""; lines = [] }
