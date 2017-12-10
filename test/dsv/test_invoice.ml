(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module I = StkDomain.Invoice
module Dsv = StkDsv.Invoice

(**********************************************************)
module Invoice_of_string = struct
  let test_empty_string =
    match (Dsv.invoice_of_string ',' "") with
    | Ok _ -> assert false
    | Bad s -> assert false
end
