(** IMAP mailbox names with their exact wire identity. Rev1 names use
    modified UTF-7 (RFC 3501 section 5.1.3). UTF-8 mode uses RFC 6855 and
    RFC 9051. Both modes reject malformed UTF-8 and the controls U+0000 to
    U+001F and U+007F. *)

type mode =
  | Rev1  (** Modified UTF-7. *)
  | Utf8  (** UTF-8 after [UTF8=ACCEPT] or under IMAP4rev2. *)

type t = private {
  raw : string;  (** The exact wire bytes. *)
  mode : mode;  (** The encoding [raw] was read in. *)
  utf8 : (string, string) result;
      (** The decoded name, or why [raw] does not decode in [mode]. *)
}
(** A received mailbox name. A malformed name remains available as
    [raw]. *)

val of_wire : mode:mode -> string -> t
(** [of_wire ~mode raw] is the received name [raw] read in [mode]. It never
    fails, and an undecodable [raw] has an [Error] in [utf8]. *)

val encode : mode:mode -> string -> (string, string) result
(** [encode ~mode name] is the UTF-8 [name] in its [mode] wire form, or an
    error when [name] is malformed UTF-8 or holds a control character. *)

val equal : t -> t -> bool
(** [equal a b] holds when [a] and [b] have the same [raw] bytes and
    [mode]. [INBOX] is not case-folded. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the decoded name, or [raw] as an OCaml string literal when
    it does not decode. *)
