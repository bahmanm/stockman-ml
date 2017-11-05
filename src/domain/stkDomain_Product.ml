(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

(** Product domain entity. *)
type product = {
  name : string;
  qty : int;
  amt : float
}

(** [validate p] checks if [p] is valid (formally). *)
let validate p =
  if String.length p.name = 0 then
    Bad "name"
  else if p.qty < 0 then
    Bad "qty"
  else if p.amt < 0.0 then
    Bad "amt"
  else
    Ok p

