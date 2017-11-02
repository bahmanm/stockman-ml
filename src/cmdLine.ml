(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module CmdLine : sig

  type cmd_opts_t = {
    file_path : string;
    field_delim : char;
  }
  (** Stockman command line options *)    

  val parse : string array -> (cmd_opts_t, string) result
  (** [parse argv] parses the command line arguments [argv]. *)

end = struct

  type cmd_opts_t = {
    file_path : string;
    field_delim : char;
  }

  type cmd_opts_wip_t = {
    mutable wip_file_path : string option;
    mutable wip_field_delim : char option;
  }
  (* command line options - during parsing and validation *)

  let specs opts = [
    ("-f",
     Arg.String (fun arg -> opts.wip_file_path <- Some arg),
     "path to file containing the records");
    ("-d",
     Arg.String (fun arg ->
         opts.wip_field_delim <- (BatString.enum arg |> BatEnum.get)),
     "field delimiter character")
  ]

  let validate_opts cmd_opts =
    match cmd_opts.wip_file_path with
    | Some file_path ->
      (match cmd_opts.wip_field_delim with
       | Some field_delim -> Ok {
           file_path = file_path;
           field_delim = field_delim
         }
       | _ -> Bad "field delimiter cannot be empty.")
    | _ -> Bad "file path cannot be empty."
  
  let parse argv =
    let cmd_opts = { wip_file_path = None; wip_field_delim = Some ',' } in
    let anon_f s = () in
    Arg.current := 0;
    Arg.parse_argv argv (specs cmd_opts) anon_f "Stockman";
    validate_opts cmd_opts

end
