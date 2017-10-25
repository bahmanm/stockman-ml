open Batteries;;

module Products = struct

  class product = object (self)
    val mutable name = ""
    val mutable qty = 0

    method get_name = name
    method set_name _name = name <- _name

    method get_qty = qty
    method set_qty _qty = qty <- _qty
  end

  module Repo = struct    
    type db_t = product list
    let add product (db : db_t) = BatList.cons product db
    let size db = BatList.length db
    let is_empty db = BatList.is_empty db
    let empty = BatList.make 0 new product
  end

  module Etl = struct
    let n_fields = 2
      
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
  end  

end

