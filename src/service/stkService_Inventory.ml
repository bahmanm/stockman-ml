(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Inventory service. *)
open Batteries

module ProductDb = StkDomainDb.ProductDb
module Product = StkDomain.Product
                     
let stock_in p_in db =
  if p_in.Product.qty <= 0 || p_in.Product.amt <= 0. then
    Bad "Invalid amount"
  else
    match ProductDb.get p_in.Product.name db with
    | Some p -> Ok (
        ProductDb.save {
          p_in with
          Product.qty = p.Product.qty + p_in.Product.qty;
          Product.amt = p.Product.amt +. p_in.Product.amt
        } db
      )
    | None -> Ok (ProductDb.save p_in db)

