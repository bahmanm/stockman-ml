(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Concrete database types for each domain type. *)

(** Product database *)
module ProductDb = Db.Make(struct
    module Product = StkDomain.Product
    type t = Product.product
    type id_t = string
    let id p = p.Product.name
    let validate p = Product.validate p
  end)

(** Invoice database *)
module InvoiceDb = Db.Make(struct
    module Invoice = StkDomain.Invoice
    type t = Invoice.invoice
    type id_t = string
    let id inv = inv.Invoice.doc_no
    let validate inv = Invoice.validate inv
  end)
