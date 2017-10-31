(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

type product = { name : string; qty : int }
(** A product entity *)

(** What follows is a bit bigger than the scope of WP1, but I just couldn't
    help but doing it the proper way (as you can see through revision history,
    the previous versions of [Db] have all been but dumb and not actually doing
    anything --not even encapsulating the fact that internally they were using
    [list] to get things done).
    
    This is an attempt at a general database (for any type of element). *)
               
module type DbElemType = sig
  
  type t
  (** The type of the database elements. *)
    
end
(** Input type of the functor !{Product.MakeDb}. *)

module type DbType = sig

  type elt_t
  (** The type of the database elements. *)

  type t
  (** The type of database. *)

  val empty : t
  (** The empty database. *)

  val is_empty : t -> bool
  (** Tests whether a database is empty or not. *)

  val size : t -> int
  (** [size db] returns the number of elements of [db]. *)

  val add : elt_t -> t -> t
  (** [add x db] returns a database containing all elements of [db],
      plus [x]. *)

  val map_to_list : (elt_t -> 'b) -> t -> 'b list
  (** [map_to_list f db] returns a [list] containing all elements of [db], after
      applying [f] to each element. *)

  val sort : (elt_t -> elt_t -> int) -> t -> t
  (** [sort cmp db] returns a db containing all elements of [db] ordered using
      [cmp].*)

  val to_list : t -> elt_t list
  (** [to_list db] returns a list containing all elements of [db] in the exact
      same order as [db].*)

end
(** Output type of the functor !{Product.MakeDb}. *)

module MakeDb(ElemType : DbElemType) :
  DbType with type elt_t = ElemType.t
= struct

  type elt_t = ElemType.t
  type t = | Empty
           | Db of elt_t list

  let empty =
    Empty
    
  let is_empty db =
    match db with
    | Empty -> true
    | _ -> false
      
  let size db =
    match db with
    | Empty -> 0
    | Db ll -> BatList.length ll
                 
  let add x db =
    match db with
    | Empty -> Db [x]
    | Db ll -> Db (BatList.cons x ll)

  let map_to_list f db =
    match db with
    | Empty -> []
    | Db ll -> BatList.map f ll

  let sort cmp db =
    match db with
    | Empty -> db
    | Db ll -> Db (BatList.sort cmp ll)

  let to_list db =
    match db with
    | Empty -> []
    | Db ll -> ll
                 
end
(** Functor for creating a database of products. *)

module Db = MakeDb(struct type t = product end)
(** Product database *)
