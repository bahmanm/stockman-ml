(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Represents the functionality for loading "delimiter separated values"
    formats for importing products from plain-text files. In reality this
    should be a very thin and abstract layer simply running the ad-hoc
    'converter' functions passed to it, e.g. `product_of_string`. However,
    due the very simple nature of WP1, those ad-hoc functions are part of
    the module. *)
open Batteries

module Enum = BatEnum
module Product = StkDomain.Product
module Params = StkDsv_Params
                   
let n_fields = 3

let is_valid_name s =
  String.length s > 0

let is_valid_qty s =
  Str.string_match (Str.regexp "^[0-9]+$") s 0

let is_valid_amt s =
  Str.string_match (Str.regexp "^[0-9]+\.[0-9][0-9]$") s 0

let get_fields delimiter s =
  s
  |> String.split_on_char delimiter
  |> BatList.map String.trim

let is_valid_fields fields =
  BatList.length fields = n_fields &&
  BatList.at fields 0 |> is_valid_name &&
  BatList.at fields 1 |> is_valid_qty &&
  BatList.at fields 2 |> is_valid_amt


let product_of_string delimiter str =
  if String.length str = 0 then
    Bad "Empty row"
  else
    let fields = get_fields delimiter str in
    if is_valid_fields fields then
      Ok { Product.name = BatList.at fields 0;
           Product.qty = BatList.at fields 1 |> int_of_string;
           Product.amt = BatList.at fields 2 |> float_of_string }
    else
      Bad ("Invalid row: " ^ str)

let load_file params =
  BatFile.lines_of params.Params.path
  |> BatEnum.skip params.Params.n_header
  |> BatEnum.filter_map (fun line ->
      if not (String.starts_with line params.Params.comment_str) then
        match product_of_string params.Params.delimiter line with
        | Ok p -> Some p
        | _ -> None
      else
        None)
