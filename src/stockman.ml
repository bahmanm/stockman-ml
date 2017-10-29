(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries


let () =
  match CmdLine.parse Sys.argv with
  | Ok { CmdLine.file_path = Some file_path; field_delim = Some field_delim } -> 
    DsvLoader.db_of_file file_path "#" field_delim 1
    |> BatList.sort (fun p1 p2 -> - compare (p1.Product.qty) (p2.Product.qty))
    |> TableFormatter.format
    |> print_endline
  | Ok { CmdLine.file_path = _; field_delim = _ } ->
    print_endline "Invalid command line args"
  | Bad s ->
    print_endline s;;
