(** A single Eio IMAP connection. Commands are serialized across fibers. *)
type t
type error = Error.t

val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string

val connect :
  sw:Eio.Switch.t -> ?auth:Auth.t -> Transport.t -> (t, error) result
(** Open a network connection, authenticate TLS, read the greeting, and
    authenticate. [Transport.v] chooses implicit TLS by default. *)

val of_flow :
  sw:Eio.Switch.t -> ?auth:Auth.t ->
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t ->
  (t, error) result
(** Take ownership of a connected test flow. *)

val capabilities : t -> string list
val enabled : t -> string list
val is_open : t -> bool
(** Whether the connection can still be reused. A successful protocol command
    may close it later, so check again when taking it from a pool. *)
val compress_deflate : t -> (unit, error) result
(** Explicit RFC 4978 COMPRESS DEFLATE activation after authentication.
    Requires COMPRESS=DEFLATE, runs under the connection command mutex, and
    switches only after tagged OK. NO/BAD preserves the plaintext transport
    and typed rejection code; a second activation is a state error.
    Compression wraps the current transport, including TLS, and lasts for the
    connection. Cancellation, malformed streams and lost framing close it;
    mutation outcomes remain uncertain after dispatch. Decompressed data is
    subject to the ordinary IMAP parser/command limits.
    This is opt-in: it is never enabled during credential exchange. Consider
    compression side channels when mixing secret and attacker-controlled data.
    Activate between mailbox leases; never call connection commands from
    inside [with_mailbox]. STARTTLS after compression is not supported. *)
val enable_uidonly : t -> (unit, error) result
(** Explicitly enables RFC 9586 mode before mailbox selection. UIDFETCH and
    VANISHED replace sequence-based updates. This mode cannot be disabled on
    the connection; callers should use a dedicated connection. *)
val enable_objectid_plus : t -> (unit, error) result
(** Explicitly activate the pinned OBJECTID+ draft through ENABLE before
    selection. The mode stays active on this connection and changes SELECT
    identity response codes to compound OBJECTID. It is separate from the
    RFC 8474 OBJECTID capability. *)
val pin_mailbox_objectid : t -> mailbox:string -> account_id:string ->
  mailbox_id:string -> (unit, error) result
(** Bind a mailbox name to a previously verified compound identity for this
    connection. Subsequent [with_mailbox] calls select by ID and reject a
    name fallback to another mailbox. APPEND checks the name with STATUS
    before sending message bytes. Rebinding to a different ID fails. *)
val list : t -> ?reference:string -> pattern:string ->
  unit -> (Imap.Response.list_result list, error) result
(** [reference] and [pattern] are UTF-8, with IMAP [*] and [%] wildcards in
    [pattern]. Outbound names use modified UTF-7 until UTF-8 mode is enabled.
    Returned [list_result.mailbox] is the exact wire name; decode it with
    [Imap.Mailbox_name.of_wire] using {!mailbox_mode}. *)
val lsub : t -> ?reference:string -> pattern:string ->
  unit -> (Imap.Response.list_result list, error) result
(** Legacy subscribed-mailbox discovery. Returned names remain exact wire
    bytes; LSUB rows may include unsubscribed hierarchy parents. *)
val namespace : t -> (Imap.Response.namespace, error) result
(** Requires NAMESPACE or IMAP4rev2. Prefixes remain exact wire names. *)
type discovery = {
  mailboxes : (Imap.Response.list_result * Imap.Response.mailbox_status option) list;
  unpaired_status : Imap.Response.mailbox_status list;
}
val list_extended : t -> ?reference:string -> patterns:string list ->
  ?selection:Imap.Command.list_selection list ->
  ?returns:Imap.Command.list_return list ->
  ?status:Imap.Command.status_item list -> unit -> (discovery, error) result
(** Negotiates LIST-EXTENDED, SPECIAL-USE and LIST-STATUS as requested.
    A selectable LIST row can lack STATUS even after tagged OK (RFC 5819);
    [None] is incomplete, never an empty status. Unpaired unsolicited STATUS
    rows remain visible. Names are exact wire bytes. *)
val mailbox_mode : t -> Imap.Mailbox_name.mode
val status : t -> mailbox:string -> items:Imap.Command.status_item list ->
  (Imap.Response.mailbox_status, error) result
(** The draft [Objectid] item requires prior [enable_objectid_plus]; its
    account and mailbox identifiers are in [mailbox_status.objectid]. *)
val get_jmap_access : t -> (string, error) result
(** Returns the server's advertised JMAP access data verbatim. A proxy must
    apply its own endpoint trust policy before using it. *)
val get_acl : t -> mailbox:string -> (Imap.Response.acl, error) result
val list_rights : t -> mailbox:string -> identifier:string ->
  (Imap.Response.list_rights, error) result
val my_rights : t -> mailbox:string -> (Imap.Response.my_rights, error) result
val set_acl : t -> mailbox:string -> identifier:string ->
  operation:[ `Add | `Remove | `Replace ] -> rights:string ->
  (unit, error) result
val delete_acl : t -> mailbox:string -> identifier:string ->
  (unit, error) result
(** ACL operations require the advertised ACL capability. Identifiers are sent
    verbatim as IMAP astrings; caller policy must handle identity preparation. *)
val get_quota : t -> root:string -> (Imap.Response.quota, error) result
val get_quota_root : t -> mailbox:string ->
  ((Imap.Response.quota_root * Imap.Response.quota list), error) result
val set_quota : t -> root:string -> limits:(string * int64) list ->
  (Imap.Response.quota option, error) result
(** Requires QUOTASET. [limits] replaces every limit on the quota root;
    omitted resources lose their limits. A returned QUOTA is the server's
    authoritative rounded/actual values when present. *)
type metadata_result = {
  responses : Imap.Response.metadata list;
  longentries : int64 option;
}
val get_metadata : t -> mailbox:string -> entries:string list ->
  ?maxsize:int64 -> ?depth:Imap.Command.metadata_depth -> unit ->
  (metadata_result, error) result
(** [longentries] reports RFC 5464 MAXSIZE truncation; when present the
    returned entries do not form a complete requested result. *)
val set_metadata : t -> mailbox:string ->
  values:(string * string option) list -> (unit, error) result
(** Empty [mailbox] refers to server metadata. This quoted-value path rejects
    values requiring a literal. METADATA-SERVER alone permits only that scope. *)
val notify_set : t -> ?status:bool -> groups:Imap.Command.notify_group list ->
  unit -> (Imap.Response.mailbox_status list, error) result
val notify_none : t -> (unit, error) result
(** Only non-selected NOTIFY filters can be installed via [Client]: calling
    this inside [with_mailbox] would violate the exclusive lease. Use a
    dedicated connection and reconcile after any notification overflow. *)
val create_mailbox : t -> string -> (unit, error) result
val create_mailbox_objectid : t -> string ->
  (Imap.Response.compound_object_id, error) result
val delete_mailbox : t -> string -> (unit, error) result
val rename_mailbox : t -> old_name:string -> new_name:string ->
  (unit, error) result
val rename_mailbox_objectid : t -> old_name:string -> new_name:string ->
  (Imap.Response.compound_object_id, error) result
(** The OBJECTID+ mutation methods require explicit activation and return the
    tagged account/mailbox identity. If the server omits either ID after a
    successful mutation, the connection closes and the outcome is uncertain
    for callers that need a durable identity; reconcile before retrying. *)
val subscribe_mailbox : t -> string -> (unit, error) result
val unsubscribe_mailbox : t -> string -> (unit, error) result
(** Mailbox mutations have uncertain outcomes on a lost tagged completion.
    A caller managing a durable mirror must reconcile identity and cursor
    scope after RENAME rather than assuming UID continuity. *)
val with_mailbox : t -> ?qresync:(int64 * int64) ->
  ?objectid:(string * string) ->
  mode:[ `Read_only | `Read_write ] -> string ->
  (Selected.t -> ('a, error) result) -> ('a, error) result
(** Holds an exclusive selection lease across [callback]. Calls on the same
    client from inside [callback] wait for that lease and therefore must be
    avoided. The selected handle becomes stale when [callback] returns. A
    normal exit sends UNSELECT where negotiated, otherwise closes safely; an
    exceptional exit closes the connection. Selected commands are serialized;
    join their fibers before returning. An escaped in-flight command closes the
    connection at lease exit. Mailbox arguments are UTF-8.
    [qresync] is a saved UIDVALIDITY and completed MODSEQ checkpoint, and is
    accepted only when QRESYNC was successfully enabled. [objectid] is the
    draft [(account_id, mailbox_id)] identity of the intended mailbox. The
    client requires prior OBJECTID+ activation and checks the SELECT response
    before invoking [callback], closing the connection if the server fell
    back to a different mailbox. *)
val append_flow : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source -> (unit, error) result
(** Sends exactly [length] octets. After any APPEND command byte is written,
    an I/O failure returns [Error.Uncertain]; the caller must reconcile before
    retrying. The client never replays APPEND automatically. *)
type append_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  uid : Imap.Proto.Uid.t;
}
val append_flow_receipt : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source -> (append_receipt option, error) result
(** A tagged OK without APPENDUID is successful but has unknown destination
    identity. [None] must be reconciled before any source deletion. *)
val append_binary_flow_receipt : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t -> length:int64 -> _ Eio.Flow.source ->
  (append_receipt option, error) result
(** RFC 3516 literal8 APPEND, gated by explicit BINARY capability; IMAP4rev2
    alone does not enable it. Uses the same destination identity guard, scoped
    command lock and uncertainty handling as [append_flow_receipt]. Sends
    exactly [length] octets and leaves any following source bytes unread.
    The server may transform content-transfer encodings while preserving
    decoded content, so the receipt proves UID identity, not stored byte
    equality. Do not publish the input digest as a canonical archived body:
    fetch and verify the stored representation first. This low-level operation
    does not journal or automatically retry. UNKNOWN-CTE is a typed rejection. *)
val append_binary_flow : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t -> length:int64 -> _ Eio.Flow.source ->
  (unit, error) result
(** Binary APPEND without retaining the optional destination UID receipt. *)
val close : t -> unit

type append_message
val append_message : ?flags:string list -> ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source -> append_message
(** [append_message source] describes a borrowed message stream. The source
    must remain usable until [append_messages] returns; it is not closed. *)

type multiappend_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  uids : Imap.Proto.Uid.t list;
}
val append_messages : t -> mailbox:string -> append_message list ->
  (multiappend_receipt option, error) result
(** Stream 1..1000 nonempty messages as one RFC 3502 atomic APPEND. Multiple
    messages require MULTIAPPEND; there is no sequential fallback. Advertised
    MESSAGELIMIT/SAVELIMIT caps are checked before dispatch. All syntax
    is validated before dispatch. Each stream supplies exactly its declared
    length; excess bytes remain unread. Literals use a fixed-size streaming buffer. Negotiated LITERAL-/LITERAL+
    or effective IMAP4rev2 permits non-synchronizing literals up to 4096
    octets; larger literals remain synchronizing.
    A rejection aborts the entire batch. Lost completion or invalid receipt
    returns Uncertain and closes the connection; cancellation also closes it.
    Receipt UIDs retain message order. None means success without UID evidence.
    This low-level operation does not journal or automatically replay a batch. *)

val noop : t -> (Imap.Response.t list, error) result
(** [noop t] sends a keepalive and returns unsolicited updates in wire order.
    Use between mailbox leases; use [Selected.noop] inside a lease. Updates
    are observations, not durable checkpoints. *)
val logout : t -> (unit, error) result
(** [logout t] waits for BYE and tagged completion, then closes the transport.
    Errors and cancellation also close it. Use between mailbox leases.
    Apply an Eio timeout externally when shutdown needs a deadline.
    [close] immediately releases the transport without a protocol exchange. *)
