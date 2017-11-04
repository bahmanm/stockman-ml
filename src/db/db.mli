(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)

(** Input type of the functor !{Db.Make}. *)
module type DbElemType = sig
  
  (** The type of the database elements. *)
  type t
    
end

(** Output type of the functor !{Db.Make}. *)
module type DbType = sig

  (** The type of the database elements. *)
  type elt_t

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

end


(** Functor for creating a database. *)
module Make(ElemType : DbElemType) : DbType with type elt_t = ElemType.t
