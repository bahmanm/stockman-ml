(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Invoice DSV importer *)

open Batteries

module I = StkDomain.Invoice

(** [invoice_of_string delimiter s] converts [s] into an [invoice].
    It does NOT validate the result. *)
val invoice_of_string : char -> string -> (I.invoice, string) result

(** [load_file params] reads and loads [params.path].
    It does NOT validate the result. *)
val load_file : StkDsv_Params.params -> I.invoice Enum.t
