(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module C = CmdLine.CmdLine
module Dsv = StkDsv.Product
module Product = StkDomain.Product
module Tabulator = StkTabulator.Product

let () =
  match C.parse Sys.argv with
  | Ok {
      C.file_path = file_path;
      field_delim = field_delim;
      n_header = n_header;
      comment_marker = comment_marker
    } -> 
    Dsv.db_of_file file_path comment_marker field_delim n_header
    |> BatEnum.fold
      (fun db p -> Ctx.ProductDb.add p db)
      Ctx.ProductDb.empty
    |> Ctx.ProductDb.sort
      (fun p1 p2 -> - compare (p1.Product.qty) (p2.Product.qty))
    |> Ctx.ProductDb.to_list
    |> Tabulator.format
    |> print_endline
  | Bad s ->
    print_endline s

