(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module I = StkDomain.Invoice
module Dsv = StkDsv.Invoice

let name = "test_invoice."

(**********************************************************)
module Invoice_of_string = struct
  let name = name ^ "invoice_of_string"

  (************************)
  let test_empty_string ctx =
    match (Dsv.invoice_of_string ',' "") with
    | Ok _ -> assert false
    | Bad s -> assert false

  (************************)
  let suite =
    name >:::
    ["empty string"
     >:: test_empty_string]
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
