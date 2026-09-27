(** IMAP mailbox names with their exact wire identity.

    A name travels in one of two encodings. Rev1 uses modified UTF-7, RFC
    3501 §5.1.3. UTF-8 mode, entered with [UTF8=ACCEPT] or under IMAP4rev2,
    sends UTF-8 as RFC 6855 and RFC 9051 describe. Both modes reject
    malformed UTF-8 and the controls U+0000 to U+001F and U+007F. A
    received name keeps its wire bytes even when they do not decode, so a
    malformed name can still be selected or reported. *)

type mode =
  | Rev1  (** Modified UTF-7. *)
  | Utf8  (** UTF-8 after [UTF8=ACCEPT] or under IMAP4rev2. *)
(** The type for mailbox name encodings. *)

type t = private {
  raw : string;  (** The exact wire bytes. *)
  mode : mode;  (** The encoding [raw] was read in. *)
  utf8 : (string, string) result;
      (** The decoded name, or why [raw] does not decode in [mode]. *)
}
(** The type for received mailbox names. *)

val of_wire : mode:mode -> string -> t
(** [of_wire ~mode raw] is the received name [raw] read in [mode]. An
    undecodable [raw] gives a name whose [utf8] field is an [Error]. *)

val encode : mode:mode -> string -> (string, string) result
(** [encode ~mode name] is the UTF-8 [name] in its [mode] wire form. The
    error says whether [name] is malformed UTF-8 or holds a control
    character. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same [raw] bytes and the
    same [mode]. [INBOX] is not case-folded. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf n] prints the decoded name of [n] on [ppf], or [raw] as an
    OCaml string literal when it does not decode. *)
