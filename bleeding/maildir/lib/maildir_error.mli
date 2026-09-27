(** Maildir format and policy failures.

    {!Maildir.error} re-exports this type and documents each
    constructor. *)

type t =
  | Legacy_metadata of string
  | Not_a_directory of string
  | Non_native_path of string
  | Malformed_filename of string
  | Duplicate_identity of string
  | Unknown_letter of { file : string; letter : char }
  | Keyword_map of string
  | Unsupported_flag of Mail_flag.Imap_flag.t
  | Too_many_keywords of Mail_flag.Imap_flag.t
  | Unrepresentable_date of float
  | Target_exists of string
  | Vanished of string
(** The type for format and policy failures. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] prints a one-line description of [e] on [ppf]. *)
