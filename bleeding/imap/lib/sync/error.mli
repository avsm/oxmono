(** Errors of every [Imap_sync] operation.

    An error never means that a sent mutation was undone. An operation that
    reached the server or Maildir before the error stays pending in the
    journal until a later cycle or an operator repair settles it. *)

type t =
  | Client of Imap_eio.Error.t
      (** [Client e] is an IMAP connection or command failure. *)
  | Maildir of Maildir.error
      (** [Maildir e] is a Maildir format or policy failure. *)
  | Store_stale_revision
      (** [Store_stale_revision] is returned when the published cursor or a
          journal row changed between a read and its compare-and-swap. *)
  | Mirror of Imap.Mirror.error
      (** [Mirror e] is a SELECT response that no scan can be planned
          from. *)
  | Invalid_scope of string
      (** [Invalid_scope why] is a mailbox name or OBJECTID+ binding that no
          longer names the scope. *)
  | Incomplete of string
      (** [Incomplete why] is a server response that omitted part of an
          inventory or a message. *)
  | Limit of string
      (** [Limit why] is a scan, transfer or audit budget that is invalid or
          too small for the mailbox. *)
  | Uidvalidity_changed
      (** [Uidvalidity_changed] is returned when the selected UIDVALIDITY
          differs from the epoch a call must preserve. *)
  | Writer_busy
      (** [Writer_busy] is returned when another process holds the Maildir
          writer lease. *)
  | Missing_pair
      (** [Missing_pair] is an operation whose pair no longer exists. *)
  | Stale_pair
      (** [Stale_pair] is a pair whose revision, scope or identity differs
          from the one the operation was journaled against. *)
  | Missing_occurrence
      (** [Missing_occurrence] is a paired endpoint whose occurrence is
          absent. *)
  | Stale_inventory
      (** [Stale_inventory] is a live observation that contradicts the
          published complete inventory. *)
  | Identity_changed
      (** [Identity_changed] is a paired occurrence whose bytes, flags or
          MODSEQ changed since the operation was journaled. *)
  | Modified
      (** [Modified] is returned when an endpoint changed after it was read
          and before any write, so the operation was rejected unapplied. *)
  | Conditional_store_unavailable
      (** [Conditional_store_unavailable] is a remote flag write that lacks
          CONDSTORE or a message MODSEQ. *)
  | Permanent_flag_unavailable of Mail_flag.Imap_flag.t
      (** [Permanent_flag_unavailable f] is a remote change of [f] that
          SELECT's PERMANENTFLAGS does not permit. *)
  | Unsupported of string
      (** [Unsupported why] is a targeted deletion that the server or the
          configuration cannot perform. *)
  | Pending_operations of string list
      (** [Pending_operations ids] names journal operations that remain
          pending and block new work. *)
  | No_pending_operation
      (** [No_pending_operation] is a repair whose operation is unknown,
          finished or of another kind. *)
  | Bootstrap_requires_pairing
      (** [Bootstrap_requires_pairing] is returned when both endpoints hold
          unpaired messages and duplicate import was not allowed. *)
  | Source_vanished of Imap.Uid.t
      (** [Source_vanished uid] is a remote message expunged before its
          body was archived. *)
  | Local_source_changed of string
      (** [Local_source_changed id] is a Maildir occurrence that changed
          before or during its upload. *)
  | Content_mismatch of string
      (** [Content_mismatch pair_id] is a paired local body that differs
          from the paired digest. A durable content conflict records it. *)
  | Content_diverged of string
      (** [Content_diverged id] is a copy whose bytes differ from the
          journaled digest or length. *)
  | Flags_diverged of string
      (** [Flags_diverged id] is a copy whose flags differ from the
          journaled flags. *)
  | Date_diverged of string
      (** [Date_diverged id] is a copy whose INTERNALDATE differs from the
          journaled date. *)
  | Diverged of string
      (** [Diverged why] is an endpoint or journal state that disagrees with
          the operation in a way no other constructor names. *)
  | Invalid_operation of string
      (** [Invalid_operation why] is a journal operation or operator request
          that the call cannot act on. *)
  | Invalid_configuration of string
      (** [Invalid_configuration why] is an invalid argument, such as a
          negative budget or a missing spool directory. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] prints a one-line description of [e]. *)

val to_string : t -> string
(** [to_string e] is the text [pp] prints for [e]. *)
