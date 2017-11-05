(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)

(** Stockman application context.
    The context is a storage for application-wide variables and composite types
    such as databases. *)

module Product = StkDomain.Product

(** Product database *)
module ProductDb = Db.Make(struct
    type t = Product.product
    type id_t = string
    let id p = p.Product.name
  end)
