(** Dovecot filename keyword mappings. *)
type t
val validate_flags : Mail_flag.Imap_flag.t list -> unit
val parse : string -> t
val encode : t -> string
val add : t -> Mail_flag.Imap_flag.t list -> t
val letters : t -> Mail_flag.Imap_flag.t list -> char list
val flags : t -> string -> Mail_flag.Imap_flag.t list
