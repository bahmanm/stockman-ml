(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries;;

(** Contains all the types and operations related to "product". *)
module Products = struct

  (** Represents a product entity. *)
  class product = object (self)
    val mutable name = ""
    val mutable qty = 0

    method get_name = name
    method set_name _name = name <- _name

    method get_qty = qty
    method set_qty _qty = qty <- _qty
  end

  (** Represents a repository for products. The data source is a simple list. *)
  module Repo = struct
    (** Type of data source. *)
    type repo_t = { db : product list }
      
    let add product repo = { db = BatList.cons product repo.db }
    let size repo = BatList.length repo.db
    let is_empty repo = BatList.is_empty repo.db
    let empty = { db = BatList.make 0 new product }
    let to_list repo = repo.db
  end

  (** Represents the ETL module for im/exporting products. In reality this
      should be a very thin and abstract layer simply running the ad-hoc
      'converter' functions passed to it, e.g. `product_of_string`. However,
      due the very simple nature of WP1, those ad-hoc functions are part of
      the module. *)
  module Etl = struct
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
        Error("Empty row")
      else
        let fields = get_fields str in
        if is_valid_fields fields then
          let p = new product in
          BatList.at fields 0 |> p#set_name;
          BatList.at fields 1 |> int_of_string |> p#set_qty;
          Ok(p)
        else
          Error("Invalid row: " ^ str)

    (** Loads the contents of the file at `path` as a repository of `products`.
        The first `n_header` lines of the file are ignored. If a line starts
        with `comment_str` it is ignored. In each line the fields are seaparated
        by `delimiter`. *)
    let repo_of_file path comment_str delimiter n_header =
      BatFile.lines_of path
      |> BatEnum.skip n_header
      |> BatEnum.filter_map (fun line ->
          if not (String.starts_with line comment_str) then
            match product_of_string delimiter line with
            | Ok p -> Some(p)
            | _ -> None
          else
            None
        )
      |> BatEnum.fold (fun repo p -> Repo.add p repo ) Repo.empty
  end  

end

module Cmd = struct
  type cmd_opts_t = { mutable file_path : string option;
                      mutable field_delim : char option }

  let parse argv =
    let cmd_opts = { file_path = None; field_delim = Some ',' } in
    let file_path_spec = ("-f",
                          Arg.String (fun arg -> cmd_opts.file_path <- Some arg),
                          "path to file containing the records") in
    let field_delim_spec = ("-d",
                            Arg.String (fun arg -> cmd_opts.field_delim <- (BatString.enum arg |> BatEnum.get)),
                            "field delimiter character") in
    let specs = [file_path_spec; field_delim_spec] in
    let anon_f s = () in
    Arg.current := 0;
    Arg.parse_argv argv specs anon_f "Stockman";
    cmd_opts
end
