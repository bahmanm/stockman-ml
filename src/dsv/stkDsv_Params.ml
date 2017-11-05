(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)

(** Required parameters to read and load a DSV file. *)
type params = {
  (** Path to the file. *)
  path : string;

  (** Comment mark - lines starting with it will be ignored. *)
  comment_str : string;

  (** Delimiter character. *)
  delimiter : char;

  (** Number of header lines - to ignore. *)
  n_header : int
}
