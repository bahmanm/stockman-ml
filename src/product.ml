(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries;;

(** A [product] entity with [name] and [qty] fields. *)
type product = { name : string; qty : int }

(** Represents a database for the products. *)
module Db = struct
  type db_t = product list
  let add product db = BatList.cons product db
  let size db = BatList.length db
  let is_empty db = BatList.is_empty db
  let empty : db_t = []
end
