(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Db

(** A product entity *)
type product = { name : string; qty : int }

(** Product database *)
module Db = Db.Make(struct
    type t = product
    type id_t = string
    let id p = p.name
  end)
