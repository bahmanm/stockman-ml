(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

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
