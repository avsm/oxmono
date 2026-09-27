(** SEARCH criteria, the RFC 9051 section 6.4.4 search keys and the
    extension keys this client sends. A typed criterion has no
    sequence-set key, so only [Raw] can name messages by sequence
    number. *)

type date = { day : int; month : int; year : int }
(** A calendar date. [month] runs from 1 to 12 and [year] from 1 to 9999. *)

type t =
  | All
  | Answered
  | Deleted
  | Draft
  | Flagged
  | Seen
  | Unanswered
  | Undeleted
  | Undraft
  | Unflagged
  | Unseen
  | Keyword of Mail_flag.Imap_flag.t
      (** The flag must be a keyword. A system flag has its own key. *)
  | Unkeyword of Mail_flag.Imap_flag.t
  | New  (** IMAP4rev1 only. *)
  | Old  (** IMAP4rev1 only. *)
  | Recent  (** IMAP4rev1 only. *)
  | Bcc of string
  | Cc of string
  | From of string
  | To of string
  | Subject of string
  | Body of string
  | Text of string
  | Header of string * string
      (** A header field name, which must be nonempty, and a substring. *)
  | Before of date
  | On of date
  | Since of date
  | Sentbefore of date
  | Senton of date
  | Sentsince of date
  | Larger of int64  (** A size in octets, at least 0. *)
  | Smaller of int64  (** A size in octets, at least 0. *)
  | Uid of Uid_set.t  (** A nonempty set. *)
  | Modseq of Modseq.t  (** RFC 7162. *)
  | Emailid of string  (** RFC 8474, an [objectid]. *)
  | Threadid of string  (** RFC 8474, an [objectid]. *)
  | Saved  (** The RFC 5182 saved result [$]. *)
  | Not of t
  | Or of t * t
  | And of t list  (** Every criterion must match. [And []] is [All]. *)
  | Raw of string
      (** Search-key syntax sent as given. It must be nonempty, free of
          control characters, balanced in parentheses and quotes, and must
          not end in a literal marker such as [{5}]. The server validates
          everything else. *)

type error =
  | Needs_utf8 of string
      (** A string argument is not ASCII and [utf8] is [false]. *)
  | Unquotable of string
      (** A string argument holds a control character or invalid UTF-8,
          which only a literal could carry. *)
  | Invalid_key of { key : string; reason : string }
      (** An argument is outside the syntax of the key [key]. *)
  | Invalid_raw of { raw : string; reason : string }

val error_to_string : error -> string
(** [error_to_string e] is [e] as one line naming the key or string at
    fault. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error] prints {!error_to_string}. *)

val date_of_internal_date : Internal_date.t -> date
(** [date_of_internal_date d] is the calendar date of [d] in its own zone. *)

val to_wire : utf8:bool -> t -> (string, error) result
(** [to_wire ~utf8 c] is the search-key syntax of [c]. Strings become
    quoted strings, and a non-ASCII string is an error unless [utf8] is
    [true]. Pass [true] once UTF8=ACCEPT is enabled or IMAP4rev2 is in
    effect. An operand of [Not] or [Or] that is a conjunction of several
    criteria or a [Raw] is parenthesized. *)

val capabilities : t -> Capability.t list
(** [capabilities c] is the extensions [c] needs, without repeats, in the
    order they first occur. [Modseq] needs [Condstore], [Saved] needs
    [Searchres], and [Emailid] and [Threadid] need [Objectid]. [Raw] needs
    none. *)

val uidonly_safe : t -> bool
(** [uidonly_safe c] is [false] when [c] contains a [Raw] whose first
    token is made only of digits, [:], [,] and [*], which RFC 9586
    forbids once UIDONLY is enabled, and [true] otherwise. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
(** [pp] prints [c] in search-key syntax without validating it. *)
