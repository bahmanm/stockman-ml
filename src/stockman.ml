(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries;;

(** Contains all the types and operations related to "product". *)
module Products = struct
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
end
(******************************************************************************)

(** Represents the functionality for loading "delimiter separated values"
    formats for importing products from plain-text files. In reality this
    should be a very thin and abstract layer simply running the ad-hoc
    'converter' functions passed to it, e.g. `product_of_string`. However,
    due the very simple nature of WP1, those ad-hoc functions are part of
    the module. *)
module DsvLoader = struct
  (** Number of fields in each row. *)
  let n_fields = 2

  (** Loads a `product` from a given string where fields are delimited by
      `delimiter`. *)
  let product_of_string delimiter str =
    let is_valid_name s = String.length s > 0 in
    let is_valid_qty s = Str.string_match (Str.regexp "^[0-9]+$") s 0 in
    let get_fields s = s |> String.split_on_char delimiter
                       |> BatList.map String.trim in
    let is_valid_fields fields = BatList.length fields = n_fields &&
                                 BatList.at fields 0 |> is_valid_name &&
                                 BatList.at fields 1 |> is_valid_qty in
    if String.length str = 0 then
      Bad "Empty row"
    else
      let fields = get_fields str in
      if is_valid_fields fields then
        Ok { Products.name = BatList.at fields 0;
             Products.qty = BatList.at fields 1 |> int_of_string }
      else
        Bad ("Invalid row: " ^ str)

  (** Loads the contents of the file at `path` as a repository of `products`.
      The first `n_header` lines of the file are ignored. If a line starts
      with `comment_str` it is ignored. In each line the fields are seaparated
      by `delimiter`. *)
  let db_of_file path comment_str delimiter n_header =
    BatFile.lines_of path
    |> BatEnum.skip n_header
    |> BatEnum.filter_map (fun line ->
        if not (String.starts_with line comment_str) then
          match product_of_string delimiter line with
          | Ok p -> Some p
          | _ -> None
        else
          None
      )
    |> BatEnum.fold (fun db p -> Products.Db.add p db ) Products.Db.empty
end  
(******************************************************************************)
module TableFormatter = struct
  let string_of_product p =
    BatPrintf.sprintf
      "| %-48s | %-24d |" (BatString.left p.Products.name 48)  p.Products.qty

  let header = "
+==================================================+==========================+
|         Product Name                             |      Available Qty       |
+==================================================+==========================+
"
  let footer = "
+==================================================+==========================+
"

  let row_sep = "
+--------------------------------------------------+--------------------------+
"

  let format product_list =
    let rows product_list =
      BatList.map string_of_product product_list
      |> BatString.concat row_sep in
    BatString.concat "" [header; (rows product_list); footer]
    
end

(******************************************************************************)

(** Collection of operations and types on command line arguements. *)
module Cmd = struct
  (** Stockman options *)
  type cmd_opts_t = { mutable file_path : string option;
                      mutable field_delim : char option }

  (** Validates the options. *)
  let validate_opts cmd_opts =
    match cmd_opts.file_path with
    | Some _ ->
      (match cmd_opts.field_delim with
       | Some _ -> Ok cmd_opts
       | _ -> Bad "field delimiter cannot be empty.")
    | _ -> Bad "file path cannot be empty."

  (** Parses the [argv] as command line arguments. *)
  let parse argv =
    let validate_opts cmd_opts =
      match cmd_opts.file_path with
      | Some _ ->
        (match cmd_opts.field_delim with
         | Some _ -> Ok cmd_opts
         | _ -> Bad "field delimiter cannot be empty.")
      | _ -> Bad "file path cannot be empty." in
    let cmd_opts = { file_path = None; field_delim = Some ',' } in
    let file_path_spec =
      ("-f",
       Arg.String (fun arg -> cmd_opts.file_path <- Some arg),
       "path to file containing the records") in
    let field_delim_spec =
      ("-d",
       Arg.String (fun arg ->
           cmd_opts.field_delim <- (BatString.enum arg |> BatEnum.get)),
       "field delimiter character") in
    let specs = [file_path_spec; field_delim_spec] in
    let anon_f s = () in
    Arg.current := 0;
    Arg.parse_argv argv specs anon_f "Stockman";
    validate_opts cmd_opts
end

