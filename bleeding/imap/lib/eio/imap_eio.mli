(** Scoped Eio IMAP connections and mailbox commands. *)

module Auth : sig
  (** Password and bearer credentials. [Auto] prefers advertised SASL PLAIN
      over TLS, then CRAM-MD5, then LOGIN. On explicitly insecure transports it
      prefers CRAM-MD5 before LOGIN. An explicit SASL mechanism must be advertised;
      no authentication failure triggers fallback. TLS is required for PLAIN,
      OAUTHBEARER and LOGIN, including Auto's LOGIN fallback, unless
      [allow_insecure_transport] opts out for an isolated fixture. CRAM-MD5 does
      not protect subsequent mailbox traffic. *)
  type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
  type t
  val password : username:string -> password:string -> ?mechanism:mechanism ->
    ?allow_insecure_transport:bool -> unit -> t
  (** [password ~username ~password ?mechanism ?allow_insecure_transport ()]
      holds a fixed password. [mechanism] defaults to [`Auto] and
      [allow_insecure_transport] to [false]. It raises [Invalid_argument]
      for an empty, non-UTF-8 or control-character username, a password
      containing NUL, [`Oauthbearer], or [`Cram_md5] with a username
      containing whitespace. *)
  val refreshing : username:string -> ?mechanism:mechanism ->
    ?allow_insecure_transport:bool -> (unit -> string) -> t
  (** [refreshing ~username ?mechanism ?allow_insecure_transport get] calls
      [get] for the password at each authentication. It checks [username]
      and [mechanism] as {!password} does. An invalid password, or an
      exception from [get], fails authentication with
      [Error.State "invalid credentials"] before any secret is sent. *)
  val bearer : username:string -> token:string ->
    ?allow_insecure_transport:bool -> unit -> t
  (** [bearer ~username ~token ?allow_insecure_transport ()] holds a fixed
      OAUTHBEARER token. [allow_insecure_transport] defaults to [false]. It
      raises [Invalid_argument] for an invalid username, or a token that is
      empty, longer than 32 KiB or not an RFC 6750 b64token. *)
  val refreshing_bearer : username:string ->
    ?allow_insecure_transport:bool -> (unit -> string) -> t
  (** [refreshing_bearer ~username ?allow_insecure_transport get] calls
      [get] for the token at each authentication, with the failure rules of
      {!refreshing}. *)
  val username : t -> string
  val mechanism : t -> mechanism
  val allow_insecure_transport : t -> bool
end

module Error : sig
  (** Structured IMAP client failures. *)
  type t =
    | Closed
    | Protocol of string
    | Transport of string
    | Rejected of { tag : string; status : [ `No | `Bad ];
        code : Imap.Response.code option; text : string }
    | State of string
    | Missing_uid of int64
    | Limit of string
    | Uncertain of string

  (** [Rejected] retains the tagged response code separately from explanatory
      text. Known codes are typed; extension codes use [Imap.Response.Other_code].
      No code is represented by [None]. A rejection does not itself authorize
      retry: partial mutations may instead return [Uncertain]. Authentication
      failures retain only a whitelist of standard codes without payloads and
      replace server text with a fixed diagnostic, so echoed credentials cannot
      enter the public error through arbitrary code parameters or text. *)
end

module Transport : sig
  (** An IMAP endpoint and its network authority. *)

  type tls = [ `Implicit | `Required_starttls | `Plain ]
  type t

  val v :
    net:_ Eio.Net.t ->
    host:string ->
    ?port:int ->
    ?tls:tls ->
    ?authenticator:X509.Authenticator.t @ portable ->
    unit -> t
  (** [Plain] is for an explicitly trusted test server. *)

  val host : t -> string
  val port : t -> int
  val tls : t -> tls

end

module Selected : sig
  (** A mailbox lease. Commands on the handle are serialized across fibers.
      A handle expires when [Client.with_mailbox] returns. Join command fibers
      before returning: an in-flight command at lease exit closes the connection,
      and queued commands fail with [Error.State]. *)
  type t
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
  (** Requires SEARCHRES or IMAP4rev2. Requests SAVE and COUNT, with exactly
      one correlated UID ESEARCH COUNT before minting a handle. An empty saved
      set is valid. Raw criteria and UIDONLY restrictions follow
      [uid_search]. *)
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
  (** [uid_search t criterion] is the explicit SEARCH result for [criterion],
      sorted and without duplicates. Exactly one SEARCH response or one UID
      ESEARCH response tagged with this command is required. A missing
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
  (** RFC 9738 descending SEARCH page. [uids] are sorted. [complete] is false
      only when [resume_before] is [Some _]. After a partial page, use
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
      this is provisional page data, not a complete UID-set inventory. Body
      items such as [BODY[]] or [BINARY[]] are refused with [Error.State]. *)
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
      discard them on any error or cancellation. A payload row without UID is
      [Error.Protocol]. A failing [sink] closes the connection and is
      [Error.State]. Decoded parts are not the raw RFC 5322 message and must
      not replace archive/synchronization body bytes. *)
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
  (** Streams a literal body into [sink] while parsing. A body sent as a quoted
      string is written after tagged completion. Bytes in [sink] are
      provisional until [Ok ()] confirms a matching UID and body length after
      tagged completion. [max_bytes] caps provisional output and defaults to
      1 GiB. A clean tagged success with no matching FETCH row returns
      [Error.Missing_uid] while keeping the selected connection usable. A
      failing [sink] closes the connection and is [Error.State]. *)
  val uid_fetch : t -> set:string -> items:string list ->
    (string list, Error.t) result
  (** [uid_fetch t ~set ~items] is the raw text of each FETCH row in the
      response, including unsolicited rows. Body items such as [BODY[]] or
      [BINARY[]] are refused with [Error.State]. *)
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
  (** Fetches a finite UID range with UID, FLAGS and optionally MODSEQ, which
      requires CONDSTORE or QRESYNC. Rows
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
  (** MOVE requires MOVE or IMAP4rev2 and UID EXPUNGE requires UIDPLUS or
      IMAP4rev2. This API never falls back to mailbox-wide EXPUNGE. A COPYUID
      receipt naming a UID outside [set] is an invalid receipt. [mailbox] is
      UTF-8. *)

  val wait_for_change : t -> (Imap.Response.t list, Error.t) result
  (** Enters IDLE, waits for one unsolicited response, sends DONE, and waits for
      tagged completion. Requires IDLE or IMAP4rev2. A tagged NO or BAD leaves
      the connection open. Use a dedicated client connection. If cancelled while
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
      Rows without UID or FLAGS are ignored.
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

end

module Client : sig
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
      account and mailbox identifiers are in [mailbox_status.objectid].
      [Highestmodseq] requires CONDSTORE or QRESYNC, [Mailboxid] requires
      OBJECTID, [Size] requires STATUS=SIZE or IMAP4rev2, [Deleted] requires
      QUOTA or IMAP4rev2 and [Deleted_storage] requires QUOTA. A missing
      capability is [Error.State] and sends nothing. *)
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
      exceptional exit closes the connection and re-raises. If UNSELECT fails,
      the connection closes and the callback's result is still returned.
      Selected commands are serialized. Join their fibers before returning. An
      escaped in-flight command closes the connection at lease exit. Mailbox
      arguments are UTF-8.
      [qresync] is a saved UIDVALIDITY and completed MODSEQ checkpoint, and is
      accepted only when QRESYNC was successfully enabled. [objectid] is the
      draft [(account_id, mailbox_id)] identity of the intended mailbox. The
      client requires prior OBJECTID+ activation and checks the SELECT response
      before invoking [callback], closing the connection if the server fell
      back to a different mailbox. *)
  val append_flow : t -> mailbox:string -> ?flags:string list ->
    ?internal_date:Imap.Internal_date.t ->
    length:int64 -> _ Eio.Flow.source -> (unit, error) result
  (** Sends exactly [length] octets. Once the final CRLF is sent, any failure
      other than a tagged rejection returns [Error.Uncertain], and the caller
      must reconcile before retrying. An earlier failure keeps its own kind,
      since the server cannot have run the command, and closes the connection
      if bytes were sent. The client never replays APPEND automatically. *)
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

end

module Pool : sig
  (** A bounded Eio pool of authenticated IMAP connections.

      Connections belong to the creation switch. Closed or failed connections
      are replaced on the next checkout. Callers should dedicate a connection
      outside this pool for long IDLE waits when ordinary work must continue. *)

  type t

  val create :
    sw:Eio.Switch.t -> max_connections:int ->
    connect:(sw:Eio.Switch.t -> (Client.t, Error.t) result) -> t
  (** [connect] is called lazily, at most [max_connections] live clients at a
      time. A failed connection attempt returns its IMAP error from [use] and
      does not consume capacity. The pool closes its clients with [sw]. *)

  val max_connections : t -> int

  val use : t -> (Client.t -> ('a, Error.t) result) -> ('a, Error.t) result
  (** Borrow one client for a bounded operation. A protocol, transport or
      uncertain-outcome error closes the borrowed client before returning it to
      the pool; a tagged rejection leaves it reusable. Exceptions and
      cancellation close the client and propagate. Never retain [Client.t]
      beyond the callback. Once the pool's switch is released, [use] returns
      [Error.Closed], including for a caller already waiting for a client. *)
end
