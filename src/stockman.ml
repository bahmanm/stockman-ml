(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module C = CmdLine.CmdLine

let () =
  match C.parse Sys.argv with
  | Ok {
      C.file_path = file_path;
      field_delim = field_delim;
      n_header = n_header;
      comment_marker = comment_marker
    } -> 
    DsvLoader.db_of_file file_path comment_marker field_delim n_header
    |> Product.Db.sort (fun p1 p2 -> - compare (p1.Product.qty) (p2.Product.qty))
    |> Product.Db.to_list
    |> TableFormatter.format
    |> print_endline
  | Bad s ->
    print_endline s

