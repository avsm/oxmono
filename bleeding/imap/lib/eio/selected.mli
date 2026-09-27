(** A mailbox lease. Commands on the handle are serialized across fibers.
    A handle expires when [Client.with_mailbox] returns. Join command fibers
    before returning: an in-flight command at lease exit closes the connection,
    and queued commands fail with [Error.State]. *)
type t
val create : Session.t -> int -> Imap.Response.select_metadata ->
  Imap.Response.t list -> t
val invalidate : t -> unit
val info : t -> (Imap.Response.select_metadata, Error.t) result
val select_updates : t -> (Imap.Response.t list, Error.t) result
(** Bounded SELECT/EXAMINE prelude, including QRESYNC FETCH and VANISHED
    responses, in wire order. Treat it as provisional until SELECT completed. *)
type saved_search
(** Opaque RFC 5182 server result, bound to its originating selected lease.
    The set shrinks as matching messages are expunged. It is not a snapshot
    or a durable UID inventory. Every subsequent ordinary UID SEARCH dispatch
    on this connection conservatively invalidates older handles, including raw
    RETURN (SAVE), rejected searches and new saved searches.
    Only [uid_search_saved] refinement preserves the handle.
    Handle validation and command dispatch share the selected command mutex. *)
val saved_search_count : saved_search -> int64
(** COUNT captured when SAVE completed, not the current live set size. *)
val uid_search_save : t -> criterion:string -> (saved_search, Error.t) result
(** Requires SEARCHRES. Requests SAVE and COUNT, with exactly one correlated
    UID ESEARCH COUNT before minting a handle. An empty saved set is valid.
    Raw criteria and UIDONLY restrictions follow [uid_search]. *)
val uid_search_saved : saved_search -> criterion:string -> (int64 list, Error.t) result
(** Search within the live saved set, preserving the handle for later use.
    The fixed ALL/COUNT command combines UID $ with a validated grouped
    criterion; RETURN/SAVE cannot be injected. Requires exactly one correlated
    UID ESEARCH result with consistent ALL/COUNT and no duplicate UIDs.
    At most 100,000 results are expanded. Ordinary [uid_search] remains a
    conservative invalidation boundary even when used with UID $. *)
val uid_fetch_saved : saved_search -> ?partial:(int64 * int64) ->
  items:string list -> unit -> (Imap.Response.fetch list, Error.t) result
(** Bounded metadata-only UID FETCH of the saved set. Always requests UID;
    allowed attributes are UID, FLAGS, INTERNALDATE, RFC822.SIZE, ENVELOPE,
    BODYSTRUCTURE and MODSEQ. MODSEQ requires CONDSTORE/QRESYNC; positional
    PARTIAL requires PARTIAL. Body literals are not supported here.
    Existing response-count/metadata budgets apply; MESSAGELIMIT partial
    results fail. Returned rows have UID but may include unsolicited FETCH
    updates: they do not independently prove membership in the saved set. *)
val uid_search : t -> string -> (int64 list, Error.t) result
(** [uid_search t criterion] is the explicit SEARCH result for [criterion].
    Exactly one SEARCH or matching UID ESEARCH response is required. A missing
    response is not an empty result. Expansion is bounded to 100,000 UIDs. *)
val uid_sort : t -> keys:(Imap.Command.sort_key * Imap.Command.sort_order) list ->
  charset:string -> criterion:string -> (int64 list, Error.t) result
(** RFC 5256 UID SORT. Requires a SORT-prefixed capability and returns at most
    100,000 distinct UIDs in server sort order. An explicit empty SORT result
    is [Ok []]; absent, repeated, malformed or MESSAGELIMIT partial results
    fail. [charset] is mandatory; [criterion] is raw SEARCH syntax, with no
    CHARSET prefix. Under UIDONLY it must not contain message sequence sets;
    use [ALL] or [UID ...]. The leading sequence-set guard is shared with
    [uid_search]; the server validates the remaining SEARCH grammar.
    A result describes current mailbox membership, not a durable snapshot. *)
type sort_result = {
  count : int64;
  first : int64 option;
  last : int64 option;
  uids : int64 list option;
  range : (int64 * int64) option;
}
val uid_sort_extended : t -> returns:Imap.Command.sort_return list ->
  keys:(Imap.Command.sort_key * Imap.Command.sort_order) list ->
  charset:string -> criterion:string -> (sort_result, Error.t) result
(** RFC 5267 ESORT. Requires ESORT; positive positional PARTIAL additionally
    requires CONTEXT=SORT (the separate PARTIAL capability is insufficient).
    An empty [returns] requests ALL; COUNT is always additionally requested.
    Exactly one correlated UID ESEARCH response must supply the requested
    fields consistently. [first]/[last] are MIN/MAX in sort order, not numeric
    UID extrema. [uids=None] means no UID list was returned, while [Some []]
    is a verified empty ALL or PARTIAL result. [range] retains the requested
    PARTIAL endpoints, including reversed endpoints.
    Expansion preserves comma-element order and expands each numeric range
    ascending, rejecting duplicates and more than 100,000 UIDs. COUNT alone
    may exceed that bound. Partial pages must match their clipped COUNT and
    position range. Raw criteria and UIDONLY restrictions follow [uid_sort].
    No UPDATE context is established; positions may shift between commands
    and results do not establish a durable snapshot. *)
val uid_thread : t -> algorithm:Imap.Command.thread_algorithm -> charset:string ->
  criterion:string -> (Imap.Response.thread list, Error.t) result
(** RFC 5256 UID THREAD, gated by the exact THREAD=algorithm capability.
    Preserves ordered parent/child relationships and dummy grouping nodes
    ([uid=None]). Bounds are 100,000 nodes and depth 100. Empty results must
    be explicit; absent, repeated, malformed and partial results fail.
    [charset] and raw [criterion] follow [uid_sort]'s rules, including UIDONLY.
    Thread trees are server-computed relationships, not stable JMAP thread
    identifiers or a durable mailbox snapshot. *)
val uid_search_partial : t -> range:(int64 * int64) -> criterion:string ->
  (Imap.Response.esearch, Error.t) result
(** One correlated RFC 9394 ESEARCH page. [partial] retains the requested
    result-position range and returned UID set (or NIL). Positions can shift
    between calls; pages alone do not prove a complete mailbox inventory. *)
type search_page = {
  uids : int64 list;
  complete : bool;
  limit : int64 option;
  resume_before : int64 option;
}
val uid_search_page : t -> ?before:int64 -> string ->
  (search_page, Error.t) result
(** RFC 9738 descending SEARCH page. After a partial page, use
    [resume_before] with the same criterion. A missing server boundary returns
    [Error.Limit]. Cross-page mailbox changes can still shift results; a
    durable inventory needs an independent membership/checkpoint strategy. *)
val uid_search_range : t -> first:int64 -> last:int64 ->
  (int64 list, Error.t) result
(** Search at most 1,000 UIDs, resuming RFC 9738 MESSAGELIMIT pages by the
    server's processed-UID boundary when advertised. Returns sorted distinct
    UIDs only after every requested UID has been processed. *)
val uid_fetch_partial : t -> set:string -> items:string list ->
  range:(int64 * int64) -> (Imap.Response.fetch list, Error.t) result
(** RFC 9394 positional FETCH page. Unsolicited FETCH rows can be interleaved;
    this is provisional page data, not a complete UID-set inventory. *)
val fetch_binary_to : t -> ?max_bytes:int64 -> ?partial:(int64 * int64) ->
  uid:int64 -> section:int list -> _ Eio.Flow.sink -> (int64 option, Error.t) result
(** Stream RFC 3516 decoded BINARY.PEEK leaf-part bytes without setting Seen.
    Requires BINARY or effective IMAP4rev2. Numeric [section] identifies a
    MIME leaf; the server validates its existence and body structure.
    [partial=(offset,count)] addresses decoded bytes and requires matching
    response origin; short reads at EOF are allowed. [max_bytes] defaults to
    1 GiB and caps literals and quoted strings, additionally bounded by count.
    The wire framer independently caps each literal at 1 GiB even if a larger
    [max_bytes] is supplied.
    [None] is explicit NIL; [Some 0L] is an empty string/literal. Missing UID
    is [Error.Missing_uid]; missing/wrong section, origin, UID or extra literals
    fail. UNKNOWN-CTE and other tagged failures propagate as rejections.
    Sink bytes are provisional until [Ok] confirms metadata and tagged success;
    discard them on any error or cancellation. Decoded parts are not the raw
    RFC 5322 message and must not replace archive/synchronization body bytes. *)
type binary_size_row = { uid : int64; size : int64 }
val uid_fetch_binary_sizes : t -> uids:int64 list -> section:int list -> unit ->
  (binary_size_row list, Error.t) result
(** Decoded sizes for at most 50 distinct UIDs, in ascending UID order.
    Uses the same BINARY/rev2 gate and leaf section rules as [fetch_binary_to].
    Missing rows can mean expunged messages; sizes are metadata, not proof of
    complete membership. Malformed, duplicate or unrequested results fail.
    Decoding sizes can be expensive on the server; fetch only when needed. *)
val fetch_to : t -> ?max_bytes:int64 -> uid:int64 ->
  _ Eio.Flow.sink -> (unit, Error.t) result
(** Streams into [sink] while parsing. Bytes in [sink] are provisional until
    [Ok ()] confirms a matching UID and body length after tagged completion.
    [max_bytes] caps provisional output and defaults to 1 GiB. A clean tagged
    success with no matching FETCH row returns [Error.Missing_uid] while
    keeping the selected connection usable. *)
val uid_fetch : t -> set:string -> items:string list ->
  (string list, Error.t) result
type envelope_row = {
  uid : int64;
  envelope : Imap.Response.envelope;
}
val uid_fetch_envelopes : t -> uids:int64 list -> unit ->
  (envelope_row list, Error.t) result
(** Fetch typed ENVELOPE data for 1..50 UIDs. Results retain requested UID
    order and omit messages expunged before FETCH. Malformed, duplicate or
    changing envelope data fails the call. *)
type bodystructure_row = {
  uid : int64;
  bodystructure : Imap.Response.bodystructure;
}
val uid_fetch_bodystructures : t -> uids:int64 list -> unit ->
  (bodystructure_row list, Error.t) result
(** Fetch typed RFC 3501/9051 BODYSTRUCTURE for 1..50 UIDs. Results retain
    requested UID order and omit expunged messages. Malformed or conflicting
    repeated structures fail the call. Each row has bounded nesting and size. *)
type preview_row = { uid : int64; preview : string option }
val uid_fetch_previews : t -> ?lazy_:bool -> uids:int64 list -> unit ->
  (preview_row list, Error.t) result
(** RFC 8970 PREVIEW for at most 50 UIDs per request. [None] is LAZY NIL;
    [Some ""] means the server found no meaningful preview. A non-LAZY NIL
    is a protocol error. Missing rows may have been expunged meanwhile. *)
type object_id_row = {
  uid : int64;
  email_id : string;
  thread_id : string option;
}
val uid_fetch_object_ids : t -> uids:int64 list -> unit ->
  (object_id_row list, Error.t) result
(** RFC 8474 OBJECTID for at most 50 distinct UIDs per request. Requires the
    exact OBJECTID capability and selected MAILBOXID; OBJECTID+ alone has a
    separate activation and grammar. [thread_id=None] is a reported NIL,
    while an omitted THREADID is an error. Missing UID rows may have been
    expunged. A proxy must still verify account scope and must not treat
    EMAILID as an occurrence or JMAP Email ID without that evidence. *)
type object_id_plus_row = {
  uid : int64;
  ids : Imap.Response.compound_object_id;
}
val uid_fetch_object_ids_plus : t -> uids:int64 list -> unit ->
  (object_id_plus_row list, Error.t) result
(** Pinned OBJECTID+ draft -06 compound FETCH for at most 50 distinct UIDs.
    Requires explicit [Client.enable_objectid_plus] and a selected compound
    identity with ACCOUNTID and MAILBOXID. Individual message identifiers
    are optional, including an empty compound response. The caller obtains
    the verified mailbox context from [info]. The draft mode is not silently
    substituted for RFC 8474. *)
val fetch_metadata_range : ?size:bool -> ?internal_date:bool ->
  t -> first:int64 -> last:int64 ->
  modseq:bool -> (Imap.Response.fetch list, Error.t) result
(** Fetches a finite UID range with UID, FLAGS and optionally MODSEQ. Rows
    without a UID or complete FLAGS are ignored as unsolicited partial updates;
    duplicate UID rows are resolved in wire order. A caller must separately
    reconcile complete membership before treating absence as an expunge.
    Advertised RFC 9738 MESSAGELIMIT partial successes are continued below
    the processed UID; missing or contradictory boundaries fail the call.
    [size=true] also requests RFC822.SIZE for bounded body inspection;
    [internal_date=true] requests a validated IMAP INTERNALDATE. *)

type store_receipt = {
  modified : Imap.Proto.Uid_set.t;
  updates : Imap.Response.fetch list;
}

val uid_store_saved : saved_search -> operation:[ `Add | `Remove | `Replace ] ->
  flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 -> unit ->
  (store_receipt, Error.t) result
(** STORE on a valid saved set, with the same conditional-write and writable
    mailbox checks as [uid_store_flags]. An identity reset or lost completion
    after dispatch returns [Error.Uncertain]; never automatically replay. *)
val uid_store_flags : t -> set:Imap.Proto.Uid_set.t ->
  operation:[ `Add | `Remove | `Replace ] ->
  flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 ->
  unit -> (store_receipt, Error.t) result
(** Conditional STORE requires CONDSTORE. [modified] is the server's RFC 7162
    conflict set. On an uncertain transport outcome, reconcile before retrying. *)

type copy_mapping = {
  source_first : Imap.Proto.Uid.t;
  destination_first : Imap.Proto.Uid.t;
  length : int64;
}

type copy_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  source : Imap.Proto.Uid_set.t;
  destination : Imap.Proto.Uid_set.t;
  mapping : copy_mapping list;
}

(** [mapping] preserves COPYUID correspondence in wire element order.
    Each range maps [source_first + i] to [destination_first + i] for
    [0 <= i < length]. Ranges stay compact even for large copies. The source
    and destination sets describe membership only, not positional pairing. *)

val uid_copy_saved : saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
val uid_move_saved : saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
val uid_expunge_saved : saved_search -> (unit, Error.t) result
(** Saved-set variants with the same capability, receipt and writable checks
    as their finite UID-set equivalents. Empty sets are valid. EXPUNGE/MOVE
    may shrink the saved set without invalidating its handle. These mutate
    remote state; uncertain outcomes require reconciliation, not replay. *)
val uid_copy : t -> set:Imap.Proto.Uid_set.t -> mailbox:string ->
  (copy_receipt option, Error.t) result
val uid_move : t -> set:Imap.Proto.Uid_set.t -> mailbox:string ->
  (copy_receipt option, Error.t) result
val uid_expunge : t -> set:Imap.Proto.Uid_set.t -> (unit, Error.t) result
(** MOVE and UID EXPUNGE require their advertised extensions; this API never
    falls back to mailbox-wide EXPUNGE. [mailbox] is UTF-8. *)

val wait_for_change : t -> (Imap.Response.t list, Error.t) result
(** Enters IDLE, waits for one unsolicited response, sends DONE, and waits for
    tagged completion. Use a dedicated client connection. If cancelled while
    waiting, the connection closes; reconnect and reconcile from durable state.
    A response is a wakeup hint, not a durable change receipt.
    NOTIFICATIONOVERFLOW is retained in the returned updates and means the
    server disabled NOTIFY registration. Reconcile before registering again. *)

val fetch_changes : t -> set:Imap.Proto.Uid_set.t ->
  since:Imap.Proto.Modseq.t -> vanished:bool ->
  (Imap.Response.t list, Error.t) result
(** CONDSTORE CHANGEDSINCE results in wire order. [vanished] requires enabled
    QRESYNC. The caller must also discover new UIDs and account for command
    boundaries before moving a durable checkpoint. *)

val fetch_changes_range : t -> first:int64 -> last:int64 ->
  since:Imap.Proto.Modseq.t -> (Imap.Response.fetch list, Error.t) result
(** Fetch changed UID/FLAGS/MODSEQ rows in a window of at most 1,000 UIDs.
    Advertised RFC 9738 MESSAGELIMIT partial replies are continued below the
    processed UID; a missing or contradictory boundary fails the call. This
    returns no VANISHED rows: verify complete membership independently before
    publishing absence or advancing a durable checkpoint. *)

val uid_batches : t -> ?range:(int64 * int64) -> size:int64 ->
  unit -> (Imap.Response.uidbatches, Error.t) result
(** RFC 10022 batch boundaries. They do not prove UID membership. The client
    permits one request per selected mailbox per connection, conservatively
    satisfying the RFC's reissue limit until it can track mailbox churn. *)

val notify_set : t -> ?status:bool -> groups:Imap.Command.notify_group list ->
  unit -> (Imap.Response.mailbox_status list, Error.t) result
val notify_none : t -> (unit, Error.t) result
(** RFC 5465 notification registration through the active selected lease.
    Registration is session state, so it also works for EXAMINE. A server
    NOTIFICATIONOVERFLOW cancels the watch; reconnect/reconcile as needed.
    Selected filters require a selected lease; other filters may be combined. *)

val noop : t -> (Imap.Response.t list, Error.t) result
(** [noop t] polls unsolicited updates under the selected command lease.
    Updates retain wire order and do not establish a durable checkpoint. *)
