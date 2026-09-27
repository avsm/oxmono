(** Stored IMAP scalar and scope representations. *)

exception Scope_mismatch
(** Raised when the stored cursor for a mailbox key names a different raw
    name, encoding or mailbox ID than the requested scope. *)

val of_checked : string -> ('a -> ('b, string) result) -> 'a -> 'b
val uid : int64 -> Imap.Uid.t
val validity : int64 -> Imap.Uidvalidity.t
val modseq : int64 -> Imap.Modseq.t
val enc : Imap.Mailbox_name.mode -> string
val dec_enc : string -> Imap.Mailbox_name.mode
val phase : Imap.Mirror.phase -> int64
val mode : Imap.Mirror.mode -> int64
val scope_key : Imap.Mirror.scope -> Sqlite3.Data.t list

val mirror_error : Imap.Mirror.error -> string
(** [mirror_error e] is a one-line description of [e]. *)

val is_sha256_hex : string -> bool
(** [is_sha256_hex x] is true when [x] is 64 lowercase hexadecimal digits. *)

val current_cursor : Database.t -> Imap.Mirror.scope ->
  Imap.Mirror.cursor option
(** [current_cursor t scope] is the stored cursor for [scope], or
    [Mirror.initial scope] when none is stored. It is [None] when the
    stored cursor names a different raw name, encoding or mailbox ID. A
    corrupt row raises [Failure]. The caller holds the database lock. *)

val cursor_exn : Database.t -> Imap.Mirror.scope -> Imap.Mirror.cursor
(** [cursor_exn t scope] is {!current_cursor} but raises {!Scope_mismatch}
    instead of returning [None]. *)

val stale : Database.t -> Imap.Mirror.cursor -> bool
(** [stale t cursor] is true unless the current cursor for [cursor]'s
    scope has the revision and UIDVALIDITY of [cursor]. *)

val stale_revision : Database.t -> Imap.Mirror.scope -> revision:int64 ->
  bool
(** [stale_revision t scope ~revision] is true unless the current cursor
    for [scope] has [revision]. *)

val check_page_args : string -> Imap.Mirror.scope -> Imap.Mirror.cursor ->
  int -> unit
(** [check_page_args who scope cursor limit] raises [Invalid_argument]
    naming [who] unless [limit] is 1 to 10,000 and [cursor] belongs to
    [scope]. *)

val flag_text : Mail_flag.Imap_flag.t list -> string
(** [flag_text l] is the wire spellings of [l] in order, separated by single
    spaces. *)

val flags : Mail_flag.Imap_flag.t list -> Sqlite3.Data.t
(** [flags l] is {!flag_text} [l] as a TEXT value. *)

val dec_flags : string -> Sqlite3.Data.t -> Mail_flag.Imap_flag.t list
(** [dec_flags what v] is the list {!flags} stored as [v]. [what] names the
    column in a decoding failure. *)

val nullable_flags : Mail_flag.Imap_flag.t list option -> Sqlite3.Data.t
(** [nullable_flags l] is NULL for [None], so that an unknown list stays
    distinct from a known empty one. *)

val dec_nullable_flags : string -> Sqlite3.Data.t ->
  Mail_flag.Imap_flag.t list option
(** [dec_nullable_flags what v] is [None] for NULL and {!dec_flags}
    otherwise. *)
