(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)

(** Input type of the functor !{Db.Make}. *)
module type DbElemType = sig
  
  (** The type of the database elements. *)
  type t

  (** The type of the id field of the elements. *)
  type id_t

  (** Retrieves the id of an element. *)
  val id : t -> id_t
    
end

(** Output type of the functor !{Db.Make}. *)
module type DbType = sig

  (** The type of the database elements. *)
  type elt_t

  (** The type of the id field of the elements. *)
  type id_t

  (** The type of database. *)
  type t
  
  (** The empty database. *)
  val empty : t

  (** Tests whether a database is empty or not. *)
  val is_empty : t -> bool

  (** [size db] returns the number of elements of [db]. *)
  val size : t -> int

  (** [add x db] returns a database containing all elements of [db],
      plus [x]. *)
  val add : elt_t -> t -> t

  (** [map_to_list f db] returns a [list] containing all elements of [db], after
      applying [f] to each element. *)
  val map_to_list : (elt_t -> 'b) -> t -> 'b list

  (** [sort cmp db] returns a db containing all elements of [db] ordered using
      [cmp].*)
  val sort : (elt_t -> elt_t -> int) -> t -> t

  (** [to_list db] returns a list containing all elements of [db] in the exact
      same order as [db].*)
  val to_list : t -> elt_t list

  val id : elt_t -> id_t

end

(** Functor for creating a database. *)
module Make(Elem : DbElemType) :
  DbType with type elt_t = Elem.t and type id_t = Elem.id_t
= struct

  type elt_t = Elem.t
                 
  type id_t = Elem.id_t
                
  type t = | Empty
           | Db of elt_t list

  let id e =
    Elem.id e
      
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
