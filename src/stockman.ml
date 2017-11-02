(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module C = CmdLine.CmdLine

let () =
  match C.parse Sys.argv with
  | Ok { C.file_path = file_path; field_delim = field_delim } -> 
    DsvLoader.db_of_file file_path "#" field_delim 1
    |> Product.Db.sort (fun p1 p2 -> - compare (p1.Product.qty) (p2.Product.qty))
    |> Product.Db.to_list
    |> TableFormatter.format
    |> print_endline
  | Bad s ->
    print_endline s;;
