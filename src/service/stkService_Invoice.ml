(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Purchase Invoice service. *)
open Batteries

module InvoiceDb = StkDomainDb.InvoiceDb
module Invoice = StkDomain.Product

(** [save inv db] saves [inv] into [db]. In case an invoice already exists
    with the same `id`, it will combine the lines. *)
let save inv db =
  InvoiceDb.save inv db
