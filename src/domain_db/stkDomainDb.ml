(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Concrete database types for each domain type. *)

(** Product database *)
module ProductDb = Db.Make(struct
    type t = StkDomain.Product.product
    type id_t = string
    let id p = p.StkDomain.Product.name
  end)
