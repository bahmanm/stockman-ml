(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

(** Collection of operations and types on command line arguements. *)
module Cmd = struct
  (** Stockman options *)
  type cmd_opts_t = { mutable file_path : string option;
                      mutable field_delim : char option }

  (** Validates the options. *)
  let validate_opts cmd_opts =
    match cmd_opts.file_path with
    | Some _ ->
      (match cmd_opts.field_delim with
       | Some _ -> Ok cmd_opts
       | _ -> Bad "field delimiter cannot be empty.")
    | _ -> Bad "file path cannot be empty."

  (** Parses the [argv] as command line arguments. *)
  let parse argv =
    let validate_opts cmd_opts =
      match cmd_opts.file_path with
      | Some _ ->
        (match cmd_opts.field_delim with
         | Some _ -> Ok cmd_opts
         | _ -> Bad "field delimiter cannot be empty.")
      | _ -> Bad "file path cannot be empty." in
    let cmd_opts = { file_path = None; field_delim = Some ',' } in
    let file_path_spec =
      ("-f",
       Arg.String (fun arg -> cmd_opts.file_path <- Some arg),
       "path to file containing the records") in
    let field_delim_spec =
      ("-d",
       Arg.String (fun arg ->
           cmd_opts.field_delim <- (BatString.enum arg |> BatEnum.get)),
       "field delimiter character") in
    let specs = [file_path_spec; field_delim_spec] in
    let anon_f s = () in
    Arg.current := 0;
    Arg.parse_argv argv specs anon_f "Stockman";
    validate_opts cmd_opts
end
  
let () =
  match Cmd.parse Sys.argv with
  | Ok { Cmd.file_path = Some file_path; field_delim = Some field_delim } -> 
    DsvLoader.db_of_file file_path "#" field_delim 1
    |> BatList.sort (fun p1 p2 -> - compare (p1.Product.qty) (p2.Product.qty))
    |> TableFormatter.format
    |> print_endline
  | Ok { Cmd.file_path = _; field_delim = _ } ->
    print_endline "Invalid command line args"
  | Bad s ->
    print_endline s;;
