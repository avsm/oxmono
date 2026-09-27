(** Durable command intents and APPEND recovery metadata. *)

type t = Database.t

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

val prepare_intent : t -> intent -> unit
(** The caller supplies a globally unique ID. [Prepared] is committed before
    the network command is sent. Reusing an ID fails. APPEND reconciliation
    metadata is immutable once prepared; [None] fields mark legacy unknown
    values, while [Some []] flags mean known empty flags. The frontier is the
    last published UID bound before send, not proof of server state at send.
    New APPEND intents require a 64-character lowercase SHA-256 digest and,
    when supplied, a valid unquoted IMAP date-time. A [uid] requires a
    [uidvalidity]. Invalid metadata raises [Invalid_argument] without
    inserting an intent. Existing legacy metadata remains readable for
    inspection and explicit recovery. A legacy row with no stored message
    ID, digest or spool reference reads that field as the empty string. *)

val set_intent_state : t -> id:string -> intent_state -> unit
(** Legal transitions are Prepared -> Sent/Ambiguous/Rejected and
    Sent -> Ambiguous/Confirmed/Rejected and Ambiguous -> Confirmed/Rejected.
    A missing ID or illegal transition raises [Invalid_argument]. *)

val confirm_intent : t -> id:string ->
  uidvalidity:Imap.Proto.Uidvalidity.t option ->
  uid:Imap.Proto.Uid.t option -> unit
(** Resolve a sent or ambiguous operation and record an optional UIDPLUS
    [APPENDUID] receipt in the same transaction. [uid] requires
    [uidvalidity]. [uidvalidity = None] keeps the stored UIDVALIDITY. *)

val pending_intents : t -> scope:Imap.Mirror.scope -> intent list
(** Returns Prepared, Sent and Ambiguous operations for reconciliation.
    Sending an APPEND after restart requires app-specific duplicate detection;
    the journal does not itself claim exactly-once delivery. *)

val find_intent : t -> id:string -> intent option
(** Retrieve a pending or resolved intent, including a persisted UIDPLUS
    receipt recorded by [confirm_intent]. *)

