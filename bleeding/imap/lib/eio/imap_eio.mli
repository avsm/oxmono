(** Scoped Eio IMAP connections and mailbox commands. *)

module Auth : sig
  (** Password and bearer credentials. [Auto] prefers advertised SASL PLAIN over
      TLS, then CRAM-MD5, then LOGIN. On explicitly insecure transports it
      prefers CRAM-MD5 before LOGIN. An explicit SASL mechanism must be
      advertised; no authentication failure triggers fallback. TLS is required
      for PLAIN, OAUTHBEARER and LOGIN, including Auto's LOGIN fallback, unless
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
    | Missing_uid of Imap.Uid.t
    | Limit of string
    | Uncertain of string
    | Unsupported of Imap.Capability.t
    | Not_enabled of Imap.Capability.t

  (** [Unsupported c] means the server neither advertises [c] nor has it
      folded into effective IMAP4rev2, and [Not_enabled c] means the server
      offers [c] but ENABLE has not confirmed it. Both are returned before
      anything is sent and leave the connection usable. A missing
      MESSAGELIMIT is [Unsupported (Other "MESSAGELIMIT")], since there is no
      limit to name. [State] reports a local precondition that is not about
      an extension. [Missing_uid u] means a completed FETCH returned no row
      for the requested UID [u].

      [Rejected] retains the tagged response code separately from explanatory
      text. Known codes are typed; extension codes use
      [Imap.Response.Other_code]. No code is represented by [None]. A rejection
      does not itself authorize retry: partial mutations may instead return
      [Uncertain]. Authentication failures retain only a whitelist of standard
      codes without payloads and replace server text with a fixed diagnostic, so
      echoed credentials cannot enter the public error through arbitrary code
      parameters or text. *)
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
  (** A mailbox lease. Commands on the handle are serialized across fibers. A
      handle expires when [Client.with_mailbox] returns. Join command fibers
      before returning: an in-flight command at lease exit closes the
      connection, and queued commands fail with [Error.State]. An operation
      that requires an extension [c] is [Error.Unsupported c] unless
      [Client.has] holds for [c], and one that requires an enabled mode is
      [Error.Not_enabled c] until ENABLE confirms it. *)

  type t
  val info : t -> (Imap.Response.select_metadata, Error.t) result
  val select_updates : t -> (Imap.Response.t list, Error.t) result
  (** Bounded SELECT/EXAMINE prelude, including QRESYNC FETCH and VANISHED
      responses, in wire order. Treat it as provisional until SELECT
      completed. *)

  type saved_search
  (** Opaque RFC 5182 server result, bound to its originating selected lease.
      The set shrinks as matching messages are expunged. It is not a snapshot or
      a durable UID inventory. Every subsequent ordinary UID SEARCH dispatch on
      this connection conservatively invalidates older handles, including raw
      RETURN (SAVE), rejected searches and new saved searches. Only
      [uid_search_saved] refinement preserves the handle. Handle validation and
      command dispatch share the selected command mutex. *)

  val saved_search_count : saved_search -> int64
  (** COUNT captured when SAVE completed, not the current live set size. *)

  type row = {
    uid : Imap.Uid.t;
    flags : Mail_flag.Imap_flag.t list option;
    internal_date : Imap.Internal_date.t option;
    size : int64 option;
    modseq : Imap.Modseq.t option;
    envelope : Imap.Response.envelope option;
    bodystructure : Imap.Response.bodystructure option;
    email_id : string option;
    thread_id : string option option;
    preview : string option option;
    objectid : Imap.Response.compound_object_id option;
    binary_sizes : (int list * int64) list;
  }
  (** The FETCH data of one message. A field is [None], and [binary_sizes]
      is empty, when its item was not requested or the server did not
      report it. [flags] is the last FLAGS reported and can include the
      transient [\Recent]. [size] is RFC822.SIZE. [thread_id = Some None]
      is an RFC 8474 THREADID NIL. [preview = Some None] is a LAZY PREVIEW
      NIL, and [Some (Some "")] means the server found no meaningful
      preview. [objectid] is the OBJECTID+ message identity, which never
      carries an account or mailbox identifier. [binary_sizes] pairs each
      requested BINARY.SIZE section with its decoded size in octets. *)

  val uid_search_save :
    t -> criteria:Imap.Search.t -> (saved_search, Error.t) result
  (** [uid_search_save t ~criteria] saves the UIDs matching [criteria] on
      the server and is a handle to them. It requires SEARCHRES or
      IMAP4rev2, requests SAVE and COUNT, and needs exactly one correlated
      UID ESEARCH COUNT before minting the handle. An empty saved set is
      valid. [criteria] follows the rules of [uid_search]. *)

  val uid_search_saved :
    saved_search -> criteria:Imap.Search.t -> (Imap.Uid.t list, Error.t) result
  (** [uid_search_saved saved ~criteria] is the UIDs of the live saved set
      that match [criteria], and keeps [saved] valid. The fixed command
      requests ALL and COUNT for [UID $] and the grouped [criteria], so a
      [Raw] criterion cannot inject RETURN or SAVE. Exactly one correlated
      UID ESEARCH result with consistent ALL and COUNT and no repeated UID
      is required. At most 100,000 results are expanded. [criteria] follows
      the rules of [uid_search]. An ordinary [uid_search] invalidates
      [saved] even when its criteria name [Saved]. *)

  val uid_fetch_saved : saved_search -> ?partial:(int64 * int64) ->
    items:Imap.Fetch_item.t list -> unit -> (row list, Error.t) result
  (** [uid_fetch_saved saved ~items ()] fetches [items] for the saved set
      under the row policy of [fetch], in ascending UID order. [partial]
      is omitted by default. When given it is an RFC 9394 position range
      and requires PARTIAL. Every reported UID is accepted because the
      saved set is not known locally, so rows can include unsolicited
      updates and do not prove membership. A MESSAGELIMIT partial result
      fails the call. *)

  val uid_search :
    t -> criteria:Imap.Search.t -> (Imap.Uid.t list, Error.t) result
  (** [uid_search t ~criteria] is the UIDs matching [criteria], sorted and
      without duplicates. Exactly one SEARCH response or one UID ESEARCH
      response tagged with this command is required. A missing response is
      not an empty result. Expansion is bounded to 100,000 UIDs.

      Every search in this module encodes its criteria with
      {!Imap.Search.to_wire}, allowing non-ASCII strings once UTF8=ACCEPT
      is enabled or IMAP4rev2 is in effect, and an encoding error is
      [Error.State]. The extensions {!Imap.Search.capabilities} lists are
      required, and QRESYNC satisfies CONDSTORE. Once UIDONLY is enabled,
      criteria that fail {!Imap.Search.uidonly_safe} are [Error.State].
      The server validates the grammar of a [Raw] criterion. *)

  val uid_sort :
    t -> keys:(Imap.Sort.key * Imap.Sort.order) list ->
    charset:string -> criteria:Imap.Search.t ->
    (Imap.Uid.t list, Error.t) result
  (** [uid_sort t ~keys ~charset ~criteria] is the RFC 5256 UID SORT of
      the messages matching [criteria], in server sort order. It requires
      SORT or SORT=DISPLAY and returns at most 100,000 distinct UIDs. An
      explicit empty SORT result is [Ok []]. An absent, repeated or
      malformed result and a MESSAGELIMIT partial result fail the call.
      [charset] is mandatory and [criteria] follows the rules of
      [uid_search]. The result describes current membership, not a durable
      snapshot. *)

  type sort_result = {
    count : int64;
    first : Imap.Uid.t option;
    last : Imap.Uid.t option;
    uids : Imap.Uid.t list option;
    range : (int64 * int64) option;
  }
  val uid_sort_extended : t -> returns:Imap.Sort.return list ->
    keys:(Imap.Sort.key * Imap.Sort.order) list ->
    charset:string -> criteria:Imap.Search.t -> (sort_result, Error.t) result
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
      position range. [charset] and [criteria] follow [uid_sort].
      No UPDATE context is established; positions may shift between commands
      and results do not establish a durable snapshot. *)

  type thread = { uid : Imap.Uid.t option; children : thread list }
  (** A UID THREAD node. [uid] is [None] for a dummy parent that groups its
      [children]. *)

  val uid_thread :
    t -> algorithm:Imap.Thread.algorithm -> charset:string ->
    criteria:Imap.Search.t -> (thread list, Error.t) result
  (** RFC 5256 UID THREAD, gated by the exact THREAD=algorithm capability.
      Preserves ordered parent/child relationships and dummy grouping nodes
      ([uid=None]). A number in the response outside the UID range is a
      [Protocol] error. Bounds are 100,000 nodes and depth 100. Empty results
      must be explicit. Absent, repeated, malformed and partial results fail.
      [charset] and [criteria] follow [uid_sort]. Thread trees are
      server-computed relationships, not stable JMAP thread identifiers or a
      durable mailbox snapshot. *)

  val uid_search_partial : t -> range:(int64 * int64) ->
    criteria:Imap.Search.t -> (Imap.Response.esearch, Error.t) result
  (** [uid_search_partial t ~range ~criteria] is one correlated RFC 9394
      ESEARCH page of the results at positions [range]. It requires
      PARTIAL. [partial] in the result keeps the requested range and the
      returned UID set, or NIL. Positions can shift between calls, so pages
      alone do not prove a complete mailbox inventory. [criteria] follows
      the rules of [uid_search]. *)

  type search_page = {
    uids : Imap.Uid.t list;
    complete : bool;
    limit : int64 option;
    resume_before : Imap.Uid.t option;
  }
  val uid_search_page : ?before:Imap.Uid.t -> t -> criteria:Imap.Search.t ->
    (search_page, Error.t) result
  (** [uid_search_page t ~criteria] is one RFC 9738 descending SEARCH page
      of the UIDs matching [criteria], below [before] when it is given.
      [before] is omitted by default. It requires MESSAGELIMIT. [uids] are
      sorted. [complete] is false only when [resume_before] is [Some _].
      After a partial page, call again with [resume_before] as [before]
      and the same criteria. A missing server boundary is [Error.Limit].
      Mailbox changes between pages can shift results, so a durable
      inventory needs an independent membership check. [criteria] follows
      the rules of [uid_search]. *)

  val uid_search_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
    (Imap.Uid.t list, Error.t) result
  (** [uid_search_range t ~first ~last] is the sorted distinct UIDs from
      [first] to [last] that exist, returned only after every UID in the
      window has been processed. RFC 9738 MESSAGELIMIT pages are resumed by
      the server's processed-UID boundary when advertised. A window with
      [last] below [first] or spanning more than 1,000 UIDs is
      [Error.State]. *)

  val uid_fetch_partial : t -> set:Imap.Uid_set.t ->
    items:Imap.Fetch_item.t list -> range:(int64 * int64) ->
    (row list, Error.t) result
  (** [uid_fetch_partial t ~set ~items ~range] is one RFC 9394 positional
      FETCH page of [set] under the row policy of [fetch], in ascending UID
      order. It requires PARTIAL, and an empty [set] is [Error.State].
      Positions can shift between pages, so pages do not prove a complete
      inventory of [set]. *)

  val fetch_binary_to : t -> ?max_bytes:int64 -> ?partial:(int64 * int64) ->
    uid:Imap.Uid.t -> section:int list -> _ Eio.Flow.sink ->
    (int64 option, Error.t) result
  (** Stream RFC 3516 decoded BINARY.PEEK leaf-part bytes without setting Seen.
      Requires BINARY or effective IMAP4rev2. Numeric [section] identifies a
      MIME leaf; the server validates its existence and body structure.
      [partial=(offset,count)] addresses decoded bytes and requires matching
      response origin; short reads at EOF are allowed. [max_bytes] defaults to
      1 GiB and caps literals and quoted strings, additionally bounded by count.
      The wire framer independently caps each literal at 1 GiB even if a larger
      [max_bytes] is supplied. [None] is explicit NIL; [Some 0L] is an empty
      string/literal. Missing UID is [Error.Missing_uid]; missing/wrong section,
      origin, UID or extra literals fail. UNKNOWN-CTE and other tagged failures
      propagate as rejections. Sink bytes are provisional until [Ok] confirms
      metadata and tagged success; discard them on any error or cancellation. A
      payload row without UID is [Error.Protocol]. A failing [sink] closes the
      connection and is [Error.State]. Decoded parts are not the raw RFC 5322
      message and must not replace archive/synchronization body bytes. *)

  val fetch_to : t -> ?max_bytes:int64 -> uid:Imap.Uid.t ->
    _ Eio.Flow.sink -> (unit, Error.t) result
  (** Streams a literal body into [sink] while parsing. A body sent as a quoted
      string is written after tagged completion. Bytes in [sink] are
      provisional until [Ok ()] confirms a matching UID and body length after
      tagged completion. [max_bytes] caps provisional output and defaults to
      1 GiB. A clean tagged success with no matching FETCH row returns
      [Error.Missing_uid] while keeping the selected connection usable. A
      failing [sink] closes the connection and is [Error.State]. *)

  val fetch : t -> uids:Imap.Uid.t list -> items:Imap.Fetch_item.t list ->
    (row list, Error.t) result
  (** [fetch t ~uids ~items] is one UID FETCH of [items] for [uids], with
      a row for each requested UID the server reported, in request order.
      UID and FLAGS are always requested. A repeated UID counts once, more
      than 1,000 distinct UIDs is [Error.State], and an empty [uids] is
      [Ok []] without a command. A UID with no row may have been expunged.
      The session's response budgets bound the reply. The extensions
      {!Imap.Fetch_item.capabilities} lists are required, with QRESYNC
      satisfying CONDSTORE and IMAP4rev2 satisfying BINARY, and OBJECTID+
      must be enabled. EMAILID and THREADID need a selected MAILBOXID, and
      OBJECTID a selected ACCOUNTID and MAILBOXID, else [Error.Protocol].

      Every FETCH of metadata in this module applies one row policy. A row
      for a UID that was not requested is unsolicited and ignored. Rows for
      one UID merge. FLAGS and MODSEQ are live state and take the last
      value reported, and a differing value for any other item is
      [Error.Protocol]. A row without a UID is ignored when it carries only
      FLAGS or MODSEQ and is [Error.Protocol] otherwise. A requested item
      missing from a row leaves its field empty. Malformed item data and a
      PREVIEW NIL without LAZY are [Error.Protocol]. Body items do not
      exist here. Stream bodies with [fetch_to] and [fetch_binary_to]. *)

  val fetch_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
    items:Imap.Fetch_item.t list -> (row list, Error.t) result
  (** [fetch_range t ~first ~last ~items] fetches [items] for the UIDs from
      [first] to [last] under the row policy of [fetch], in ascending UID
      order. A window with [last] below [first] or spanning more than 1,000
      UIDs is [Error.State]. Advertised RFC 9738 MESSAGELIMIT partial
      successes are continued below the processed UID, and a missing or
      contradictory boundary fails the call. Absence from the result does
      not prove an expunge, so reconcile membership separately before
      publishing it. *)

  type store_receipt = {
    modified : Imap.Uid_set.t;
    updates : Imap.Response.fetch list;
  }

  val uid_store_saved : saved_search ->
    operation:[ `Add | `Remove | `Replace ] ->
    flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 -> unit ->
    (store_receipt, Error.t) result
  (** STORE on a valid saved set, with the same conditional-write and writable
      mailbox checks as [uid_store_flags]. An identity reset or lost completion
      after dispatch returns [Error.Uncertain]; never automatically replay. *)

  val uid_store_flags : t -> set:Imap.Uid_set.t ->
    operation:[ `Add | `Remove | `Replace ] ->
    flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 ->
    unit -> (store_receipt, Error.t) result
  (** Conditional STORE requires CONDSTORE. [unchangedsince] is omitted by
      default. It is an RFC 7162 [mod-sequence-valzer] rather than an
      {!Imap.Modseq.t}, because 0 is valid there and no existing message
      satisfies it. [modified] is the server's RFC 7162 conflict set. On an
      uncertain transport outcome, reconcile before retrying. *)

  type copy_mapping = {
    source_first : Imap.Uid.t;
    destination_first : Imap.Uid.t;
    length : int64;
  }

  type copy_receipt = {
    uidvalidity : Imap.Uidvalidity.t;
    source : Imap.Uid_set.t;
    destination : Imap.Uid_set.t;
    mapping : copy_mapping list;
  }

  (** [mapping] preserves COPYUID correspondence in wire element order.
      Each range maps [source_first + i] to [destination_first + i] for
      [0 <= i < length]. Ranges stay compact even for large copies. The source
      and destination sets describe membership only, not positional pairing. *)

  val uid_copy_saved :
    saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
  val uid_move_saved :
    saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
  val uid_expunge_saved : saved_search -> (unit, Error.t) result
  (** Saved-set variants with the same capability, receipt and writable checks
      as their finite UID-set equivalents. Empty sets are valid. EXPUNGE/MOVE
      may shrink the saved set without invalidating its handle. These mutate
      remote state; uncertain outcomes require reconciliation, not replay. *)

  val uid_copy : t -> set:Imap.Uid_set.t -> mailbox:string ->
    (copy_receipt option, Error.t) result
  val uid_move : t -> set:Imap.Uid_set.t -> mailbox:string ->
    (copy_receipt option, Error.t) result
  val uid_expunge : t -> set:Imap.Uid_set.t -> (unit, Error.t) result
  (** MOVE requires MOVE or IMAP4rev2 and UID EXPUNGE requires UIDPLUS or
      IMAP4rev2. This API never falls back to mailbox-wide EXPUNGE. A COPYUID
      receipt naming a UID outside [set] is an invalid receipt. [mailbox] is
      UTF-8. *)

  val wait_for_change : t -> (Imap.Response.t list, Error.t) result
  (** Enters IDLE, waits for one unsolicited response, sends DONE, and waits
      for tagged completion. Requires IDLE or IMAP4rev2. A tagged NO or BAD
      leaves the connection open. Use a dedicated client connection. If
      cancelled while waiting, the connection closes; reconnect and reconcile
      from durable state. A response is a wakeup hint, not a durable change
      receipt. NOTIFICATIONOVERFLOW is retained in the returned updates and
      means the server disabled NOTIFY registration. Reconcile before
      registering again. *)

  val fetch_changes : t -> set:Imap.Uid_set.t ->
    since:Imap.Modseq.t -> vanished:bool ->
    (Imap.Response.t list, Error.t) result
  (** CONDSTORE CHANGEDSINCE results in wire order. [vanished] requires enabled
      QRESYNC. The caller must also discover new UIDs and account for command
      boundaries before moving a durable checkpoint. *)

  val fetch_changes_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
    since:Imap.Modseq.t -> (Imap.Response.fetch list, Error.t) result
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

  val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
    unit -> (Imap.Response.mailbox_status list, Error.t) result
  val notify_none : t -> (unit, Error.t) result
  (** RFC 5465 notification registration through the active selected lease.
      Registration is session state, so it also works for EXAMINE. A server
      NOTIFICATIONOVERFLOW cancels the watch; reconnect/reconcile as needed.
      Selected filters require a selected lease; other filters may be
      combined. *)  

  val noop : t -> (Imap.Response.t list, Error.t) result
  (** [noop t] polls unsolicited updates under the selected command lease.
      Updates retain wire order and do not establish a durable checkpoint. *)

end

module Client : sig
  (** A single Eio IMAP connection. Commands are serialized across fibers.
      An operation that requires an extension [c] is [Error.Unsupported c]
      unless {!has} holds for [c], and one that requires an enabled mode is
      [Error.Not_enabled c] until {!enable} confirms it. *)

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

  val capabilities : t -> Imap.Capability.Set.t
  (** [capabilities t] is the set the latest CAPABILITY response advertised.
      The client reads it after the greeting, after STARTTLS and after
      authentication. *)

  val enabled : t -> Imap.Capability.Set.t
  (** [enabled t] is every capability an ENABLED response confirmed on [t]. *)

  val has : t -> Imap.Capability.t -> bool
  (** [has t c] holds when [capabilities t] contains [c], or when [t] is in
      effective IMAP4rev2 and {!Imap.Capability.implied_by_rev2} [c] holds.
      Effective IMAP4rev2 means the server advertises IMAP4rev2 and either
      does not advertise IMAP4rev1 or confirmed ENABLE IMAP4rev2. Every
      extension gate in [Client] and [Selected] uses this predicate. *)

  val is_enabled : t -> Imap.Capability.t -> bool
  (** [is_enabled t c] is [Imap.Capability.Set.mem c (enabled t)]. *)

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
      mutation outcomes remain uncertain after dispatch. More than 16 MiB of
      compressed input without decoded output counts as a malformed stream.
      Decompressed data is subject to the ordinary IMAP parser/command limits.
      This is opt-in: it is never enabled during credential exchange. Consider
      compression side channels when mixing secret and attacker-controlled data.
      Activate between mailbox leases; never call connection commands from
      inside [with_mailbox]. STARTTLS after compression is not supported. *)

  val enable : t -> Imap.Capability.t list ->
    (Imap.Capability.t list, error) result
  (** [enable t caps] sends RFC 5161 ENABLE for those of [caps] not yet
      enabled and is the list the server's ENABLED responses confirmed. It
      sends nothing and is [Ok []] when [caps] is empty or already enabled.
      It is [Error.Unsupported Enable] unless [has t Enable] or the server
      advertises IMAP4rev2, and [Error.Unsupported c] for a [c] of [caps]
      with [not (has t c)]. It is [Error.State] while a mailbox is selected.
      A capability the server does not confirm is not an error. [connect]
      already enables IMAP4rev2 when both revisions are advertised, then
      UTF8=ACCEPT without effective IMAP4rev2, then QRESYNC, ignoring a
      rejection of each. *)

  val enable_uidonly : t -> (unit, error) result
  (** [enable_uidonly t] is [enable t [Uidonly]], and a [Protocol] error if
      the server does not confirm it. UIDFETCH and VANISHED replace
      sequence-based updates. This mode cannot be disabled on the connection,
      so use a dedicated connection. *)

  val enable_objectid_plus : t -> (unit, error) result
  (** [enable_objectid_plus t] is [enable t [Objectid_plus]], and a
      [Protocol] error if the server does not confirm it. The pinned
      OBJECTID+ draft stays active on this connection and changes SELECT
      identity response codes to compound OBJECTID. It is separate from the
      RFC 8474 OBJECTID capability. *)

  val pin_mailbox_objectid : t -> mailbox:string -> account_id:string ->
    mailbox_id:string -> (unit, error) result
  (** Bind a mailbox name to a previously verified compound identity for this
      connection. Subsequent [with_mailbox] calls select by ID and reject a
      name fallback to another mailbox. APPEND checks the name with STATUS
      before sending message bytes. Rebinding to a different ID fails. *)

  type mailbox_entry = {
    name : Imap.Mailbox_name.t;
        (** The row's mailbox name. [name.utf8] is the decoded form and
            [name.raw] the exact wire form. *)
    info : Imap.Response.list_result;  (** The LIST or LSUB row. *)
  }
  (** A listed mailbox, its name decoded in the connection's
      {!mailbox_mode} at the time of the response. *)

  val list : t -> ?reference:string -> pattern:string ->
    unit -> (mailbox_entry list, error) result
  (** [list t ~pattern ()] is every LIST row matching [pattern] under
      [reference], which defaults to [""]. [reference] and [pattern] are
      UTF-8, with IMAP [*] and [%] wildcards in [pattern]. Outbound names use
      modified UTF-7 until UTF-8 mode is enabled. *)

  val lsub : t -> ?reference:string -> pattern:string ->
    unit -> (mailbox_entry list, error) result
  (** [lsub t ~pattern ()] is legacy subscribed-mailbox discovery, with
      [reference] and [pattern] as in {!list}. LSUB rows may include
      unsubscribed hierarchy parents. *)

  val namespace : t -> (Imap.Response.namespace, error) result
  (** Requires NAMESPACE or IMAP4rev2. Prefixes remain exact wire names. *)

  type discovery = {
    mailboxes : (mailbox_entry * Imap.Response.mailbox_status option) list;
        (** Each LIST row with the STATUS row that followed it. *)
    unpaired_status : Imap.Response.mailbox_status list;
        (** STATUS rows that followed no LIST row. *)
  }
  (** The result of {!list_extended}. *)

  val list_extended : t -> ?reference:string -> patterns:string list ->
    ?selection:Imap.Mailbox_list.selection list ->
    ?returns:Imap.Mailbox_list.return list ->
    ?status:Imap.Status_item.t list -> unit -> (discovery, error) result
  (** [list_extended t ~patterns ()] negotiates LIST-EXTENDED, SPECIAL-USE
      and LIST-STATUS as requested. [reference] defaults to [""], and
      [selection] and [returns] default to none. [status] adds RFC 5819
      LIST-STATUS and is omitted by default. A selectable LIST row can lack
      STATUS even after tagged OK (RFC 5819). [None] is incomplete, never an
      empty status. Unpaired unsolicited STATUS rows remain visible, with
      their names as exact wire bytes. *)

  val mailbox_mode : t -> Imap.Mailbox_name.mode
  (** [mailbox_mode t] is [Utf8] when IMAP4rev2 or UTF8=ACCEPT is in effect
      on [t], and [Rev1] otherwise. *)

  val status : t -> mailbox:string -> items:Imap.Status_item.t list ->
    (Imap.Response.mailbox_status, error) result
  (** The draft [Objectid] item requires prior [enable_objectid_plus]; its
      account and mailbox identifiers are in [mailbox_status.objectid].
      [Highestmodseq] requires CONDSTORE or QRESYNC, [Mailboxid] requires
      OBJECTID, [Size] requires STATUS=SIZE or IMAP4rev2, [Deleted] requires
      QUOTA or IMAP4rev2 and [Deleted_storage] requires QUOTA. A missing
      capability is [Error.Unsupported] naming CONDSTORE, OBJECTID,
      STATUS=SIZE or QUOTA, and a missing OBJECTID+ activation is
      [Error.Not_enabled], and neither sends anything. *)

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
  (** ACL operations require the advertised ACL capability. Identifiers are
      sent verbatim as IMAP astrings; caller policy must handle identity
      preparation. *)

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
    ?maxsize:int64 -> ?depth:Imap.Metadata.depth -> unit ->
    (metadata_result, error) result
  (** [longentries] reports RFC 5464 MAXSIZE truncation; when present the
      returned entries do not form a complete requested result. *)

  val set_metadata : t -> mailbox:string ->
    values:(string * string option) list -> (unit, error) result
  (** Empty [mailbox] refers to server metadata. This quoted-value path
      rejects values requiring a literal. METADATA-SERVER alone permits only
      that scope. *)

  val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
    unit -> (Imap.Response.mailbox_status list, error) result
  val notify_none : t -> (unit, error) result
  (** Only non-selected NOTIFY filters can be installed via [Client]: calling
      this inside [with_mailbox] would violate the exclusive lease. Use a
      dedicated connection and reconcile after any notification overflow. *)

  val create_mailbox : t -> mailbox:string -> (unit, error) result
  (** [create_mailbox t ~mailbox] creates the UTF-8 name [mailbox]. Like
      every mailbox mutation, a lost tagged completion is
      [Error.Uncertain]. *)

  val create_mailbox_objectid : t -> mailbox:string ->
    (Imap.Response.compound_object_id, error) result
  (** [create_mailbox_objectid t ~mailbox] is {!create_mailbox} returning the
      tagged account and mailbox identity. It requires prior
      {!enable_objectid_plus}. If the server omits either ID after a
      successful CREATE, the connection closes and the result is
      [Error.Uncertain]. Reconcile before retrying. *)

  val delete_mailbox : t -> mailbox:string -> (unit, error) result
  (** [delete_mailbox t ~mailbox] deletes the UTF-8 name [mailbox]. *)

  val rename_mailbox : t -> old_name:string -> new_name:string ->
    (unit, error) result
  (** [rename_mailbox t ~old_name ~new_name] renames the UTF-8 name
      [old_name] to [new_name]. A caller managing a durable mirror must
      reconcile identity and cursor scope afterwards rather than assume UID
      continuity. *)

  val rename_mailbox_objectid : t -> old_name:string -> new_name:string ->
    (Imap.Response.compound_object_id, error) result
  (** [rename_mailbox_objectid t ~old_name ~new_name] is {!rename_mailbox}
      returning the tagged identity, under the conditions of
      {!create_mailbox_objectid}. *)

  val subscribe_mailbox : t -> mailbox:string -> (unit, error) result
  (** [subscribe_mailbox t ~mailbox] subscribes the UTF-8 name [mailbox]. *)

  val unsubscribe_mailbox : t -> mailbox:string -> (unit, error) result
  (** [unsubscribe_mailbox t ~mailbox] unsubscribes the UTF-8 name
      [mailbox]. *)

  val with_mailbox : t -> ?qresync:(Imap.Uidvalidity.t * Imap.Modseq.t) ->
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

  type append_receipt = {
    uidvalidity : Imap.Uidvalidity.t;
    uid : Imap.Uid.t;
  }
  (** The RFC 4315 APPENDUID of one stored message. *)

  type append_message
  (** One message for [append] or [append_many]. *)

  val append_message :
    ?flags:Mail_flag.Imap_flag.t list -> ?internal_date:Imap.Internal_date.t ->
    length:int64 -> _ Eio.Flow.source -> append_message
  (** [append_message ~length source] is a message of exactly [length]
      octets read from [source]. [flags] defaults to none and is sent in
      {!Mail_flag.Imap_flag.to_wire} spelling. [internal_date] is omitted by
      default, which lets the server choose the INTERNALDATE. [source] is
      borrowed and must stay usable until the APPEND returns. It is not
      closed, and bytes after [length] stay unread. *)

  val append : t -> mailbox:string -> ?binary:bool -> append_message ->
    (append_receipt option, error) result
  (** [append t ~mailbox message] stores [message] in [mailbox] with one
      APPEND and is its APPENDUID receipt. [None] is a tagged OK without
      APPENDUID, whose destination identity is unknown, so reconcile before
      deleting the source. Use [Result.map ignore] to discard the receipt.

      [binary] defaults to [false]. When [true] the message is an RFC 3516
      literal8, which requires the BINARY capability even under IMAP4rev2.
      The server may then transform content-transfer encodings while
      preserving decoded content, so the receipt proves UID identity and not
      stored byte equality. Fetch and verify the stored representation
      before publishing the input digest as an archived body. UNKNOWN-CTE is
      a typed rejection.

      A mailbox with a pinned OBJECTID+ identity is checked with STATUS
      before any byte is sent. Negotiated LITERAL-, LITERAL+ or effective
      IMAP4rev2 permits a non-synchronizing literal up to 4096 octets. Once
      the final CRLF is sent, any failure other than a tagged rejection is
      [Error.Uncertain], and the caller must reconcile before retrying. An
      APPENDUID naming several UIDs is [Error.Uncertain] and closes the
      connection. An earlier failure keeps its own kind, since the server
      cannot have run the command, and closes the connection if bytes were
      sent. The client never replays APPEND. *)

  val close : t -> unit

  type multiappend_receipt = {
    uidvalidity : Imap.Uidvalidity.t;
    uids : Imap.Uid.t list;
  }
  (** The APPENDUID of an RFC 3502 batch, with [uids] in message order. *)

  val append_many : t -> mailbox:string -> append_message list ->
    (multiappend_receipt option, error) result
  (** [append_many t ~mailbox messages] streams 1 to 1,000 nonempty
      messages as one RFC 3502 atomic APPEND. More than one message
      requires MULTIAPPEND, and there is no sequential fallback. Advertised
      MESSAGELIMIT and SAVELIMIT caps and all syntax are checked before
      dispatch. Literals use a fixed-size streaming buffer, and the literal
      and uncertainty rules of [append] apply. A rejection aborts the whole
      batch. A lost completion or an APPENDUID that does not name one UID per
      message is [Error.Uncertain] and closes the connection, and
      cancellation also closes it. [None] means success without UID
      evidence. The batch is not journalled or replayed. Binary literals are
      not supported here. *)

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
