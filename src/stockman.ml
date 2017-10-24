open Batteries;;

module Products = struct

  class product = object (self)
    val mutable name = ""
    val mutable qty = 0
                    
    method get_name = name
    method set_name _name = name <- _name
                          
    method get_qty = qty
    method set_qty _qty = qty <- _qty
  end;;

  module Repo = struct    
    type db_t = product list
    let add product (db : db_t) = BatList.cons product db
    let size db = BatList.length db
    let is_empty db = BatList.is_empty db
    let empty = BatList.make 0 new product
  end;;
    
end;;
  
