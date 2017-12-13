(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Purchase Invoice service. *)
open Batteries

module IDb = StkDomainDb.InvoiceDb
module I = StkDomain.Invoice

type save_error =
  | HeaderMismatch (** header fields do not match *)
  | LineMismatch (** line fields (with the same line_no) do not match *)
(** errors returned by the [save] operation  *)

val save : I.invoice -> IDb.t -> (IDb.t, save_error) result
(** [save invoice db] saves [invoice] to the [db]. If an invoice with the same
    [id] as [invoice] exists, and if the header fields match, it combines the
    lines and saves the result. If [invoice] contains lines which already
    exist, it only succeeds if the lines match. *)
