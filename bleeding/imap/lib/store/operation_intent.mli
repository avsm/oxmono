(** Durable command intents and APPEND recovery metadata, documented in
    [Imap_store]. *)

type intent_kind =
  | Append of {
      message_id : string;
      content_digest : string;
      spool_ref : string;
      pre_send_uid_frontier : int64 option;
      expected_length : int64 option;
      expected_flags : Mail_flag.Imap_flag.t list option;
      expected_internal_date : string option;
    }
  | Other of string

type intent_state = Prepared | Sent | Ambiguous | Confirmed | Rejected

type intent = {
  id : string;
  scope : Imap.Mirror.scope;
  kind : intent_kind;
  state : intent_state;
  uidvalidity : Imap.Proto.Uidvalidity.t option;
  uid : Imap.Proto.Uid.t option;
}

val prepare_intent : Database.t -> intent -> unit
(** [prepare_intent t intent] commits [intent] as [Prepared], raising
    [Invalid_argument] for invalid APPEND metadata and [Sqlite3.SqliteError] for
    a reused ID. *)

val set_intent_state : Database.t -> id:string -> intent_state -> unit
(** [set_intent_state t ~id state] applies a legal state transition, or raises
    [Invalid_argument]. *)

val confirm_intent : Database.t -> id:string ->
  uidvalidity:Imap.Proto.Uidvalidity.t option ->
  uid:Imap.Proto.Uid.t option -> unit
(** [confirm_intent t ~id ~uidvalidity ~uid] resolves a sent or ambiguous intent
    and records its APPENDUID receipt, keeping the stored UIDVALIDITY when
    [uidvalidity] is [None]. *)

val pending_intents : Database.t -> scope:Imap.Mirror.scope -> intent list
val find_intent : Database.t -> id:string -> intent option
