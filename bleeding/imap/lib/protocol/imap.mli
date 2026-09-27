@@ portable

(** IMAP protocol values, codecs and synchronization planning.

    [Imap] is the pure half of the IMAP client. It checks identifiers,
    frames and parses server responses, encodes commands, types the
    extension vocabularies and plans mailbox scans and reconciliation. It
    performs no I/O. [Imap_eio] runs these values over a connection, and
    [Imap_sync] drives durable synchronization with them.

    Every function is portable, so a caller may frame, parse, encode and
    plan on any domain. Every type is immutable data that may be shared
    between domains, except the mutable {!Wire.t} and the stdlib-backed
    {!Capability.Set.t} and {!Mirror.snapshot}. *)

(** {1 Identifiers} *)

module Uid = Uid
(** Message unique identifiers. *)

module Uidvalidity = Uidvalidity
(** Mailbox UIDVALIDITY values. *)

module Modseq = Modseq
(** CONDSTORE modification sequences. *)

module Seq = Seq
(** Message sequence numbers. *)

module Uid_set = Uid_set
(** Finite UID sets and their wire form. *)

module Mailbox_name = Mailbox_name
(** Mailbox names in modified UTF-7 or UTF-8 with their wire identity. *)

module Internal_date = Internal_date
(** Validated INTERNALDATE and APPEND date-times. *)

(** {1 Wire and codecs} *)

module Wire = Wire
(** Incremental response framing. *)

module Response = Response
(** Typed server responses. *)

module Command = Command
(** Validated command encoders. *)

(** {1 Vocabularies} *)

module Capability = Capability
(** Capability tokens and sets. *)

module Search = Search
(** SEARCH criteria. *)

module Fetch_item = Fetch_item
(** Metadata FETCH data items. *)

module Status_item = Status_item
(** STATUS data items. *)

module Mailbox_list = Mailbox_list
(** LIST-EXTENDED selection and return options. *)

module Sort = Sort
(** SORT keys and ESORT return options. *)

module Thread = Thread
(** THREAD algorithms. *)

module Notify = Notify
(** NOTIFY filters and events. *)

module Metadata = Metadata
(** GETMETADATA options. *)

(** {1 Planning} *)

module Mirror = Mirror
(** Mailbox cursors and scan planning. *)

module Sync_policy = Sync_policy
(** Flag and deletion reconciliation policy for a paired message. *)
