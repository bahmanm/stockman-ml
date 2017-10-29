(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

let string_of_product p =
  BatPrintf.sprintf
    "| %-48s | %-24d |" (BatString.left p.Product.name 48)  p.Product.qty

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
