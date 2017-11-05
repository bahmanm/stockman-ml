(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)

(** Input type of the functor !{Db.Make}. *)
module type DbElemType = sig
  
  (** The type of the database elements. *)
  type t

  (** The type of the id field of the elements. *)
  type id_t

  (** [id e] retrieves the id of an element. *)
  val id : t -> id_t

  (** [validate e] formally validates [e]. In case of invalidity, returns the
      name of the field violating constraints.
      This method should strictly do *formal* validation and should not rely
      on any information outside [e] itself. *)
  val validate : t -> (t, string) Batteries.result
    
end

(** Output type of the functor !{Db.Make}. *)
module type DbType = sig

  (** The type of the database elements. *)
  type elt_t

  (** The type of the id field of the elements. *)
  type id_t

  (** The type of database. *)
  type t

  (** [InvalidField (element, field_name)] denotes an exception where the
      the value of [field_name] in [element] violates a constraint. *)
  exception InvalidField of (elt_t * string)
  
  (** The empty database. *)
  val empty : t

  (***** Ops *****)
  
  (** [save x db] returns a database containing all elements of [db],
      plus [x]. If an element with id equal to that of [x] exists, it will
      replaced by [x]. *)
  val save : elt_t -> t -> t

  (** [delete id_value db] returns a database containing all elements of [db]
      except the one with id [id_value]. *)
  val delete : id_t -> t -> t

  (***** Query *****)
    
  (** [exists id_value db] returns true if [db] contains an element with id
      equal to [id_value], false otherwise. *)
  val contains : id_t -> t -> bool

  (** [get id_value db] returns the element whose id value matches
      [id_value]. *)
  val get : id_t -> t -> elt_t option

  (** [all db] returns all elements of [db] as a [list]. *)
  val all : t -> elt_t list

  (** [size db] returns the number of elements of [db]. *)
  val size : t -> int

end

(** Functor for creating a database. *)
module Make(Elem : DbElemType) :
  DbType with type elt_t = Elem.t and type id_t = Elem.id_t
= struct
  open Batteries
  
  type elt_t = Elem.t
                 
  type id_t = Elem.id_t
                
  type t = | Empty
           | Db of elt_t list

  exception InvalidField of (elt_t * string)

  let id e =
    Elem.id e
      
  let empty =
    Empty
      
  let size db =
    match db with
    | Empty -> 0
    | Db ll -> BatList.length ll
                 
  let save x db =
    match Elem.validate x with
    | Ok _ ->
      (match db with
       | Empty -> Db [x]
       | Db ll -> Db (BatList.cons x ll))
    | Bad field -> raise (InvalidField (x, field))

  let delete id_value db =
    match db with
    | Empty -> db
    | Db ll -> Db (BatList.filter (fun e -> (id e) != id_value) ll)

  let contains id_value db =
    match db with
    | Empty -> false
    | Db ll -> BatList.exists (fun e -> (id e) = id_value) ll

  let get id_value db =
    match db with
    | Empty -> None
    | Db ll ->
      try
        Some (BatList.find
                (fun e -> (id e) = id_value)
                ll)
      with Not_found -> None

  let all db =
    match db with
    | Empty -> []
    | Db ll -> ll
  
end
