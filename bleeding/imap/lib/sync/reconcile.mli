(** Conservative, read-only evidence for an uncertain APPEND.

    IMAP has no general idempotency key. An exact current body match, even a
    unique one, cannot prove that this operation created that occurrence:
    another client may have appended identical bytes. This module never
    confirms, rejects, or replays an intent. *)

type error =
  | Client of Imap_eio.Error.t
  | Invalid_intent of string
  | Invalid_scope of string
  | Incomplete of string
  | Limit of string

val pp_error : Format.formatter -> error -> unit

type candidate = {
  uid : Imap.Proto.Uid.t;
  length : int64;
  sha256 : string;
  flags_match : bool option;
}

type report =
  | Epoch_changed of {
      journal_uidvalidity : Imap.Proto.Uidvalidity.t;
      server_uidvalidity : Imap.Proto.Uidvalidity.t;
    }
  | Inspected of {
      uidvalidity : Imap.Proto.Uidvalidity.t;
      covered_upper : int64;
      examined : int;
      matches : candidate list;
    }
(** [Inspected] describes current server state only. Zero matches does not
    prove an APPEND was never committed; the message may have been expunged.
    One match does not prove ownership. Keep the intent pending until an
    application-specific reconciliation policy has stronger evidence. *)

val inspect_append :
  ?max_windows:int -> ?max_candidates:int -> ?max_bytes:int64 ->
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> id:string ->
  spool:_ Eio.Path.t -> unit -> (report, error) result
(** [spool] must be a unique path on a writable filesystem. It is created
    exclusively for each candidate and removed on all exits. Inspection
    requires a saved UIDVALIDITY and pre-send UID frontier. It scans only UIDs
    above that frontier, bounded by the selected UIDNEXT and the supplied
    budgets. A concurrent mailbox change may make the scan fail; reconnect and
    retry rather than treating that failure as evidence of absence. No durable
    state changes. *)
