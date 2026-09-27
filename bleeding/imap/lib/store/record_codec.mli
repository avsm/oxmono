(** Stored IMAP scalar and scope representations. *)

val of_checked : string -> ('a -> ('b, string) result) -> 'a -> 'b
val uid : int64 -> Imap.Proto.Uid.t
val validity : int64 -> Imap.Proto.Uidvalidity.t
val modseq : int64 -> Imap.Proto.Modseq.t
val enc : Imap.Mailbox_name.mode -> string
val dec_enc : string -> Imap.Mailbox_name.mode
val phase : Imap.Mirror.phase -> int64
val dec_phase : int64 -> Imap.Mirror.phase
val mode : Imap.Mirror.mode -> int64
val dec_mode : int64 -> Imap.Mirror.mode
val scope_key : Imap.Mirror.scope -> Sqlite3.Data.t list

val decode_cursor : Imap.Mirror.scope -> Sqlite3.Data.t array -> Imap.Mirror.cursor
(** [decode_cursor scope row] validates and restores a persisted cursor. *)
