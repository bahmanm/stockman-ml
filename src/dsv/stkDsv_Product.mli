(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(** Represents the functionality for loading "delimiter separated values"
    formats for importing products from plain-text files. *)

open Batteries
    
module Enum = BatEnum
module Product = StkDomain.Product

(** [product_of_string delimiter s] converts [s] into a [product].
    It does NOT validate the result. *)
val product_of_string : char -> string -> (Product.product, string) result

(** [load_file params] reads and loads [params.path].
    It does NOT validate the result. *)
val load_file : StkDsv_Params.params -> Product.product Enum.t
