(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Represents the functionality for loading "delimiter separated values"
    formats for importing products from plain-text files. In reality this
    should be a very thin and abstract layer simply running the ad-hoc
    'converter' functions passed to it, e.g. `product_of_string`. However,
    due the very simple nature of WP1, those ad-hoc functions are part of
    the module. *)
open Batteries

(** Number of fields in each row. *)
let n_fields = 2

(** Loads a `product` from a given string where fields are delimited by
    `delimiter`. *)
let product_of_string delimiter str =
  let is_valid_name s =
    String.length s > 0 in
  let is_valid_qty s =
    Str.string_match (Str.regexp "^[0-9]+$") s 0 in
  let get_fields s =
    s
    |> String.split_on_char delimiter
    |> BatList.map String.trim in
  let is_valid_fields fields =
    BatList.length fields = n_fields &&
    BatList.at fields 0 |> is_valid_name &&
    BatList.at fields 1 |> is_valid_qty in
  if String.length str = 0 then
    Bad "Empty row"
  else
    let fields = get_fields str in
    if is_valid_fields fields then
      Ok { Product.name = BatList.at fields 0;
           qty = BatList.at fields 1 |> int_of_string }
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
  |> BatEnum.fold (fun db p -> Product.Db.add p db) Product.Db.empty
