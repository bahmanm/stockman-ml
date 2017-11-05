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
