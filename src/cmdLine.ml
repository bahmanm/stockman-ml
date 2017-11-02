(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module CmdLine : sig

  type cmd_opts_t = {
    file_path : string;
    field_delim : char;
    n_header : int;
    comment_marker : string;
  }
  (** Stockman command line options *)    

  val parse : string array -> (cmd_opts_t, string) result
  (** [parse argv] parses the command line arguments [argv]. *)

end = struct

  type cmd_opts_t = {
    file_path : string;
    field_delim : char;
    n_header : int;
    comment_marker : string;
  }

  type cmd_opts_wip_t = {
    mutable wip_file_path : string option;
    mutable wip_field_delim : char option;
    mutable wip_n_header : int option;
    mutable wip_comment_marker : string option;
  }
  (* command line options - during parsing and validation *)

  let specs opts = [
    ("-f",
     Arg.String (fun arg -> opts.wip_file_path <- Some arg),
     "path to file containing the records");
    ("-d",
     Arg.String (fun arg ->
         opts.wip_field_delim <- (BatString.enum arg |> BatEnum.get)),
     "field delimiter character");
    ("-i",
     Arg.Int (fun arg ->
         opts.wip_n_header <- Some arg),
     "field delimiter character");
    ("-c",
     Arg.String (fun arg ->
         opts.wip_comment_marker <- Some arg),
     "comment marker")
  ]

  let validate_opts cmd_opts =
    match cmd_opts with
    | { wip_file_path = None; _ } ->
      Bad "file path cannot be empty."
    | { wip_field_delim = None; _ } ->
      Bad "field delimiter cannot be empty."
    | { wip_comment_marker = None; _ } ->
      Bad "comment marker cannot be empty."
    | { wip_n_header = None; _ } ->
      Bad "number of header lines cannot be empty."
    | { wip_n_header = Some i; _ } when i < 0 ->
      Bad "number of header lines cannot be < 0."
    | { wip_file_path = Some file_path;
        wip_field_delim = Some field_delim;
        wip_n_header = Some n_header;
        wip_comment_marker = Some comment_marker
      } ->
      Ok {
        file_path = file_path; field_delim = field_delim;
        n_header = n_header; comment_marker = comment_marker
      }
  
  let parse argv =
    let cmd_opts = {
      wip_file_path = None;
      wip_field_delim = Some ',';
      wip_comment_marker = Some "#";
      wip_n_header = Some 1
    } in
    let anon_f s = () in
    Arg.current := 0;
    Arg.parse_argv argv (specs cmd_opts) anon_f "Stockman";
    validate_opts cmd_opts

end
