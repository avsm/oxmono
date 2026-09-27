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
      connection, and queued commands fail with [Error.State].

      The values at this level need only IMAP4rev1, or gate each call on
      the typed criteria and items they are given. An operation that exists
      only because of an extension lives in a submodule named for it. The
      submodule's [require] checks the gate once and returns a witness that
      its operations take instead of the lease. A missing extension [c] is
      [Error.Unsupported c], and a mode that ENABLE has not confirmed is
      [Error.Not_enabled c]. Neither sends anything. [require] succeeds
      without further checks where effective IMAP4rev2 folds the extension
      in. A witness is bound to its lease and expires with it, so an
      operation on a witness whose lease has ended is [Error.State]. *)

  type t
  val info : t -> (Imap.Response.select_metadata, Error.t) result
  val select_updates : t -> (Imap.Response.t list, Error.t) result
  (** Bounded SELECT/EXAMINE prelude, including QRESYNC FETCH and VANISHED
      responses, in wire order. Treat it as provisional until SELECT
      completed. *)

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

  val uid_search_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
    (Imap.Uid.t list, Error.t) result
  (** [uid_search_range t ~first ~last] is the sorted distinct UIDs from
      [first] to [last] that exist, returned only after every UID in the
      window has been processed. RFC 9738 MESSAGELIMIT pages are resumed by
      the server's processed-UID boundary when advertised. A window with
      [last] below [first] or spanning more than 1,000 UIDs is
      [Error.State]. *)

  type sort_result = {
    count : int64;
    first : Imap.Uid.t option;
    last : Imap.Uid.t option;
    uids : Imap.Uid.t list option;
    range : (int64 * int64) option;
  }
  (** The result of {!Esort.uid_sort_extended}. *)

  type thread = { uid : Imap.Uid.t option; children : thread list }
  (** A UID THREAD node. [uid] is [None] for a dummy parent that groups its
      [children]. *)

  type search_page = {
    uids : Imap.Uid.t list;
    complete : bool;
    limit : int64 option;
    resume_before : Imap.Uid.t option;
  }
  (** One page of {!Messagelimit.uid_search_page}. *)

  val fetch_to : t -> ?max_bytes:int64 -> uid:Imap.Uid.t ->
    _ Eio.Flow.sink -> (unit, Error.t) result
  (** [fetch_to t ~uid sink] streams the message body of [uid] into [sink]
      while parsing. A body sent as a quoted string is written after tagged
      completion. Bytes in [sink] are provisional until [Ok ()] confirms a
      matching UID and body length after tagged completion. [max_bytes]
      caps provisional output and defaults to 1 GiB. A clean tagged success
      with no matching FETCH row returns [Error.Missing_uid] while keeping
      the selected connection usable. A failing [sink] closes the
      connection and is [Error.State]. *)

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
      exist here. Stream bodies with [fetch_to] and
      {!Binary.fetch_binary_to}. *)

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
  (** [modified] is the server's RFC 7162 conflict set, empty for an
      unconditional STORE. [updates] are the FETCH rows the STORE
      reported. *)

  val uid_store_flags : t -> set:Imap.Uid_set.t ->
    operation:[ `Add | `Remove | `Replace ] ->
    flags:Mail_flag.Imap_flag.t list -> (store_receipt, Error.t) result
  (** [uid_store_flags t ~set ~operation ~flags] applies [operation] with
      [flags] to [set] in one UID STORE. A read-only mailbox and an empty
      [set] are [Error.State]. On an uncertain transport outcome,
      reconcile before retrying. {!Condstore.uid_store_flags} is the
      conditional form. *)

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

  val uid_copy : t -> set:Imap.Uid_set.t -> mailbox:string ->
    (copy_receipt option, Error.t) result
  (** [uid_copy t ~set ~mailbox] copies [set] to the UTF-8 name [mailbox]
      and is its COPYUID receipt, or [None] when the server sent none. A
      COPYUID receipt naming a UID outside [set] is an invalid receipt. An
      uncertain outcome requires reconciliation, not replay. *)

  val noop : t -> (Imap.Response.t list, Error.t) result
  (** [noop t] polls unsolicited updates under the selected command lease.
      Updates retain wire order and do not establish a durable checkpoint. *)

  module Condstore : sig
    (** RFC 7162 CONDSTORE. [require] holds when the server offers
        CONDSTORE or QRESYNC. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Condstore]. *)

    val uid_store_flags : t -> set:Imap.Uid_set.t ->
      operation:[ `Add | `Remove | `Replace ] ->
      flags:Mail_flag.Imap_flag.t list -> unchangedsince:int64 ->
      (store_receipt, Error.t) result
    (** [uid_store_flags t ~set ~operation ~flags ~unchangedsince] is
        {!Selected.uid_store_flags} with UNCHANGEDSINCE. [unchangedsince]
        is an RFC 7162 [mod-sequence-valzer] rather than an
        {!Imap.Modseq.t}, because 0 is valid there and no existing message
        satisfies it. [modified] in the receipt lists the UIDs the server
        refused to change. *)

    val fetch_changes_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
      since:Imap.Modseq.t -> (Imap.Response.fetch list, Error.t) result
    (** [fetch_changes_range t ~first ~last ~since] is the UID, FLAGS and
        MODSEQ rows changed after [since] in a window of at most 1,000
        UIDs. Rows without UID or FLAGS are ignored. Advertised RFC 9738
        MESSAGELIMIT partial replies are continued below the processed UID,
        and a missing or contradictory boundary fails the call. It returns
        no VANISHED rows, so verify complete membership independently
        before publishing absence or advancing a durable checkpoint. *)
  end

  module Qresync : sig
    (** RFC 7162 QRESYNC. [require] holds once ENABLE has confirmed
        QRESYNC. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s]. It is [Error.Unsupported Qresync]
        when the server does not offer QRESYNC and [Error.Not_enabled
        Qresync] when ENABLE has not confirmed it. *)

    val fetch_changes : t -> set:Imap.Uid_set.t ->
      since:Imap.Modseq.t -> vanished:bool ->
      (Imap.Response.t list, Error.t) result
    (** [fetch_changes t ~set ~since ~vanished] is the CHANGEDSINCE FETCH,
        VANISHED and HIGHESTMODSEQ responses for [set] in wire order, with
        VANISHED requested when [vanished] holds. An empty [set] is
        [Error.State]. The caller must also discover new UIDs and account
        for command boundaries before moving a durable checkpoint. *)
  end

  module Uidplus : sig
    (** RFC 4315 UIDPLUS, folded into IMAP4rev2. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Uidplus]. *)

    val uid_expunge : t -> set:Imap.Uid_set.t -> (unit, Error.t) result
    (** [uid_expunge t ~set] expunges exactly the messages of [set] marked
        [\Deleted]. It never falls back to mailbox-wide EXPUNGE. A
        read-only mailbox and an empty [set] are [Error.State]. An
        uncertain outcome requires reconciliation, not replay. *)
  end

  module Move : sig
    (** RFC 6851 MOVE, folded into IMAP4rev2. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Move]. *)

    val uid_move : t -> set:Imap.Uid_set.t -> mailbox:string ->
      (copy_receipt option, Error.t) result
    (** [uid_move t ~set ~mailbox] moves [set] to the UTF-8 name [mailbox]
        under the receipt rules of {!Selected.uid_copy}. A read-only
        mailbox is [Error.State]. *)
  end

  module Binary : sig
    (** RFC 3516 BINARY FETCH. IMAP4rev2 folds in the FETCH side of
        BINARY, so [require] holds there too. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Binary]. *)

    val fetch_binary_to : t -> ?max_bytes:int64 ->
      ?partial:(int64 * int64) -> uid:Imap.Uid.t -> section:int list ->
      _ Eio.Flow.sink -> (int64 option, Error.t) result
    (** [fetch_binary_to t ~uid ~section sink] streams the decoded
        BINARY.PEEK bytes of the MIME leaf [section] of [uid] into [sink]
        without setting [\Seen], and is their length. The server validates
        that [section] exists. [partial] is omitted by default. When given
        as [(offset, count)] it addresses decoded bytes and requires a
        matching response origin, and a short read at the end is allowed.
        [max_bytes] defaults to 1 GiB and caps literals and quoted strings,
        further bounded by [count]. The wire framer caps each literal at
        1 GiB whatever [max_bytes] is.

        [None] is an explicit NIL, and [Some 0L] an empty string or literal.
        A missing UID is [Error.Missing_uid]. A missing or wrong section,
        origin or UID, or an extra literal, fails the call, and a payload
        row without UID is [Error.Protocol]. UNKNOWN-CTE and other tagged
        failures are rejections. Bytes in [sink] are provisional until [Ok]
        confirms metadata and tagged success, so discard them on any error
        or cancellation. A failing [sink] closes the connection and is
        [Error.State]. Decoded parts are not the raw RFC 5322 message and
        must not replace archived body bytes. *)
  end

  module Searchres : sig
    (** RFC 5182 SEARCHRES, folded into IMAP4rev2. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Searchres]. *)

    type saved_search
    (** An opaque server-side saved result, bound to the lease it was saved
        on. The set shrinks as matching messages are expunged. It is not a
        snapshot or a durable UID inventory. Every later ordinary UID
        SEARCH on the connection invalidates older handles, including raw
        RETURN (SAVE), rejected searches and new saved searches. Only
        [uid_search_saved] refinement preserves the handle. A stale handle
        is [Error.State]. Handle validation and command dispatch share the
        selected command mutex. *)

    val uid_search_save :
      t -> criteria:Imap.Search.t -> (saved_search, Error.t) result
    (** [uid_search_save t ~criteria] saves the UIDs matching [criteria] on
        the server and is a handle to them. It requests SAVE and COUNT and
        needs exactly one correlated UID ESEARCH COUNT before minting the
        handle. An empty saved set is valid. [criteria] follows the rules
        of {!Selected.uid_search}. *)

    val uid_search_saved : saved_search -> criteria:Imap.Search.t ->
      (Imap.Uid.t list, Error.t) result
    (** [uid_search_saved saved ~criteria] is the UIDs of the live saved
        set that match [criteria], and keeps [saved] valid. The fixed
        command requests ALL and COUNT for [UID $] and the grouped
        [criteria], so a [Raw] criterion cannot inject RETURN or SAVE.
        Exactly one correlated UID ESEARCH result with consistent ALL and
        COUNT and no repeated UID is required. At most 100,000 results are
        expanded. [criteria] follows the rules of {!Selected.uid_search}.
        An ordinary [uid_search] invalidates [saved] even when its criteria
        name [Saved]. *)

    val uid_fetch_saved : saved_search -> ?partial:(int64 * int64) ->
      items:Imap.Fetch_item.t list -> unit -> (row list, Error.t) result
    (** [uid_fetch_saved saved ~items ()] fetches [items] for the saved set
        under the row policy of {!Selected.fetch}, in ascending UID order.
        [partial] is omitted by default. When given it is an RFC 9394
        position range and is [Error.Unsupported Partial] unless the
        server offers PARTIAL. Every reported UID is accepted because the
        saved set is not known locally, so rows can include unsolicited
        updates and do not prove membership. A MESSAGELIMIT partial result
        fails the call. *)

    val uid_store_saved : saved_search ->
      operation:[ `Add | `Remove | `Replace ] ->
      flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 -> unit ->
      (store_receipt, Error.t) result
    (** [uid_store_saved saved ~operation ~flags ()] is a STORE on the
        saved set with the checks of {!Selected.uid_store_flags}.
        [unchangedsince] is omitted by default. When given it makes the
        STORE conditional as in {!Condstore.uid_store_flags} and is
        [Error.Unsupported Condstore] without CONDSTORE or QRESYNC. An
        identity reset or lost completion after dispatch is
        [Error.Uncertain], and the call is never replayed. *)

    val uid_copy_saved :
      saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
    (** [uid_copy_saved saved ~mailbox] is {!Selected.uid_copy} of the saved
        set. An empty set is valid. *)

    val uid_move_saved :
      saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
    (** [uid_move_saved saved ~mailbox] is {!Move.uid_move} of the saved
        set, and is [Error.Unsupported Move] unless the server offers MOVE.
        The move can shrink the saved set without invalidating the
        handle. *)

    val uid_expunge_saved : saved_search -> (unit, Error.t) result
    (** [uid_expunge_saved saved] is {!Uidplus.uid_expunge} of the saved
        set, and is [Error.Unsupported Uidplus] unless the server offers
        UIDPLUS. The expunge can shrink the saved set without invalidating
        the handle. *)

    val saved_search_count : saved_search -> int64
    (** [saved_search_count saved] is the COUNT captured when SAVE
        completed, not the current size of the live set. *)
  end

  module Sort : sig
    (** RFC 5256 SORT. [require] holds when the server offers SORT or
        SORT=DISPLAY. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Sort]. *)

    val uid_sort : t -> keys:(Imap.Sort.key * Imap.Sort.order) list ->
      charset:string -> criteria:Imap.Search.t ->
      (Imap.Uid.t list, Error.t) result
    (** [uid_sort t ~keys ~charset ~criteria] is the UID SORT of the
        messages matching [criteria], in server sort order. It returns at
        most 100,000 distinct UIDs. An explicit empty SORT result is
        [Ok []]. An absent, repeated or malformed result and a MESSAGELIMIT
        partial result fail the call. [charset] is mandatory and [criteria]
        follows the rules of {!Selected.uid_search}. The result describes
        current membership, not a durable snapshot. *)
  end

  module Esort : sig
    (** RFC 5267 ESORT. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Esort]. *)

    val uid_sort_extended : t -> returns:Imap.Sort.return list ->
      keys:(Imap.Sort.key * Imap.Sort.order) list ->
      charset:string -> criteria:Imap.Search.t ->
      (sort_result, Error.t) result
    (** [uid_sort_extended t ~returns ~keys ~charset ~criteria] is the ESORT
        summary of {!Sort.uid_sort}. An empty [returns] requests ALL, and
        COUNT is always requested. A positional PARTIAL is
        [Error.Unsupported (Context `Sort)] unless the server offers
        CONTEXT=SORT, which the PARTIAL capability does not replace.
        Exactly one correlated UID ESEARCH response must supply the
        requested fields consistently. [first] and [last] are MIN and MAX
        in sort order, not numeric UID extrema. [uids = None] means no UID
        list was returned, while [Some []] is a verified empty ALL or
        PARTIAL result. [range] keeps the requested PARTIAL endpoints,
        including reversed ones.

        Expansion keeps comma-element order and expands each numeric range
        ascending, rejecting duplicates and more than 100,000 UIDs. COUNT
        alone may exceed that bound. Partial pages must match their clipped
        COUNT and position range. [charset] and [criteria] follow
        {!Sort.uid_sort}. No UPDATE context is established, so positions
        may shift between commands and results are not a durable
        snapshot. *)
  end

  module Thread : sig
    (** RFC 5256 THREAD for one algorithm. *)

    type selected := t
    type t

    val require : selected -> Imap.Thread.algorithm -> (t, Error.t) result
    (** [require s algorithm] is a witness for [s] and [algorithm], or
        [Error.Unsupported (Thread algorithm)] unless the server offers
        THREAD=[algorithm]. *)

    val uid_thread : t -> charset:string -> criteria:Imap.Search.t ->
      (thread list, Error.t) result
    (** [uid_thread t ~charset ~criteria] is the UID THREAD forest of the
        messages matching [criteria] under the witness's algorithm. It
        keeps ordered parent and child relationships and dummy grouping
        nodes. A number outside the UID range is [Error.Protocol]. Bounds
        are 100,000 nodes and depth 100. An empty result must be explicit,
        and absent, repeated, malformed and partial results fail the call.
        [charset] and [criteria] follow {!Sort.uid_sort}. Thread trees are
        server-computed relationships, not stable JMAP thread identifiers
        or a durable mailbox snapshot. *)
  end

  module Partial : sig
    (** RFC 9394 PARTIAL. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Partial]. *)

    val uid_search_partial : t -> range:(int64 * int64) ->
      criteria:Imap.Search.t -> (Imap.Response.esearch, Error.t) result
    (** [uid_search_partial t ~range ~criteria] is one correlated ESEARCH
        page of the results at positions [range]. [partial] in the result
        keeps the requested range and the returned UID set, or NIL.
        Positions can shift between calls, so pages alone do not prove a
        complete mailbox inventory. [criteria] follows the rules of
        {!Selected.uid_search}. *)

    val uid_fetch_partial : t -> set:Imap.Uid_set.t ->
      items:Imap.Fetch_item.t list -> range:(int64 * int64) ->
      (row list, Error.t) result
    (** [uid_fetch_partial t ~set ~items ~range] is one positional FETCH
        page of [set] under the row policy of {!Selected.fetch}, in
        ascending UID order. An empty [set] is [Error.State]. Positions can
        shift between pages, so pages do not prove a complete inventory of
        [set]. *)
  end

  module Messagelimit : sig
    (** RFC 9738 MESSAGELIMIT. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported (Other
        "MESSAGELIMIT")] when the server advertises no limit. *)

    val uid_search_page : ?before:Imap.Uid.t -> t -> criteria:Imap.Search.t ->
      (search_page, Error.t) result
    (** [uid_search_page t ~criteria] is one descending SEARCH page of the
        UIDs matching [criteria], below [before] when it is given.
        [before] is omitted by default. [uids] are sorted. [complete] is
        false only when [resume_before] is [Some _]. After a partial page,
        call again with [resume_before] as [before] and the same criteria.
        A missing server boundary is [Error.Limit]. Mailbox changes between
        pages can shift results, so a durable inventory needs an
        independent membership check. [criteria] follows the rules of
        {!Selected.uid_search}. *)
  end

  module Uidbatches : sig
    (** RFC 10022 UIDBATCHES. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Uidbatches]. *)

    val uid_batches : t -> ?range:(int64 * int64) -> size:int64 ->
      unit -> (Imap.Response.uidbatches, Error.t) result
    (** [uid_batches t ~size ()] is the server's UID batch boundaries for
        batches of [size] messages, restricted to the batch indexes [range]
        when it is given. [range] is omitted by default. Boundaries do not
        prove UID membership. A second request for the same mailbox on one
        connection is [Error.State], which satisfies the RFC's reissue
        limit without tracking mailbox churn. *)
  end

  module Notify : sig
    (** RFC 5465 NOTIFY through the active lease. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Notify]. *)

    val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
      unit -> (Imap.Response.mailbox_status list, Error.t) result
    (** [notify_set t ~groups ()] registers [groups] and is the STATUS rows
        the server sent with the registration, which [status] requests.
        [status] defaults to [false]. Registration is session state, so it
        also works for EXAMINE, and selected filters may be combined with
        others. A NOTIFICATIONOVERFLOW in the reply is [Error.Limit],
        since the server cancelled the registration. *)

    val notify_none : t -> (unit, Error.t) result
    (** [notify_none t] cancels every registration on the connection. *)
  end

  module Idle : sig
    (** RFC 2177 IDLE, folded into IMAP4rev2. *)

    type selected := t
    type t

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Idle]. *)

    val wait_for_change : t -> (Imap.Response.t list, Error.t) result
    (** [wait_for_change t] enters IDLE, waits for one unsolicited
        response, sends DONE and waits for tagged completion, and is the
        responses that ended the wait. A tagged NO or BAD leaves the
        connection open. Use a dedicated connection. Cancellation while
        waiting closes the connection, so reconnect and reconcile from
        durable state. A response is a wakeup hint, not a durable change
        receipt. A NOTIFICATIONOVERFLOW is kept in the result and means
        the server cancelled NOTIFY registration, so reconcile before
        registering again. *)
  end
end

module Client : sig
  (** A single Eio IMAP connection. Commands are serialized across fibers.

      The values at this level are IMAP4rev1 commands, or extensions that
      IMAP4rev2 folds in and that each call gates on its arguments. An
      operation that exists only because of an extension lives in a
      submodule named for it, whose [require] or [enable] checks the gate
      once and returns a witness that its operations take instead of the
      client. A missing extension [c] is [Error.Unsupported c] unless
      {!has} holds for [c], and a mode that {!enable} has not confirmed is
      [Error.Not_enabled c]. Neither sends anything. *)

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
  (** [status t ~mailbox ~items] is the STATUS of the UTF-8 name
      [mailbox] for [items]. Each item is gated on its extension.
      [Highestmodseq] needs CONDSTORE or QRESYNC, [Mailboxid] OBJECTID,
      [Size] STATUS=SIZE or IMAP4rev2, [Deleted] QUOTA or IMAP4rev2 and
      [Deleted_storage] QUOTA, and a missing one is [Error.Unsupported]
      naming CONDSTORE, OBJECTID, STATUS=SIZE or QUOTA. The draft
      [Objectid] item is [Error.Not_enabled Objectid_plus] until
      {!Objectid_plus.enable} succeeds, and its identifiers are in
      [mailbox_status.objectid]. *)

  val get_jmap_access : t -> (string, error) result
  (** Returns the server's advertised JMAP access data verbatim. A proxy must
      apply its own endpoint trust policy before using it. *)

  type metadata_result = {
    responses : Imap.Response.metadata list;
    longentries : int64 option;
  }
  (** The result of {!Metadata.get_metadata}. [longentries] reports RFC 5464
      MAXSIZE truncation, and when present the returned entries do not form
      the complete requested result. *)

  val create_mailbox : t -> mailbox:string -> (unit, error) result
  (** [create_mailbox t ~mailbox] creates the UTF-8 name [mailbox]. Like
      every mailbox mutation, a lost tagged completion is
      [Error.Uncertain]. *)

  val delete_mailbox : t -> mailbox:string -> (unit, error) result
  (** [delete_mailbox t ~mailbox] deletes the UTF-8 name [mailbox]. *)

  val rename_mailbox : t -> old_name:string -> new_name:string ->
    (unit, error) result
  (** [rename_mailbox t ~old_name ~new_name] renames the UTF-8 name
      [old_name] to [new_name]. A caller managing a durable mirror must
      reconcile identity and cursor scope afterwards rather than assume UID
      continuity. *)

  val subscribe_mailbox : t -> mailbox:string -> (unit, error) result
  (** [subscribe_mailbox t ~mailbox] subscribes the UTF-8 name [mailbox]. *)

  val unsubscribe_mailbox : t -> mailbox:string -> (unit, error) result
  (** [unsubscribe_mailbox t ~mailbox] unsubscribes the UTF-8 name
      [mailbox]. *)

  val with_mailbox : t -> ?qresync:(Imap.Uidvalidity.t * Imap.Modseq.t) ->
    ?objectid:(string * string) ->
    mode:[ `Read_only | `Read_write ] -> string ->
    (Selected.t -> ('a, error) result) -> ('a, error) result
  (** [with_mailbox t ~mode mailbox callback] holds an exclusive selection
      lease on the UTF-8 name [mailbox] across [callback] and is its result.
      A [Client] command on [t], including a nested [with_mailbox], called
      from [callback] or from a fiber that [callback] forked is
      [Error.State "call inside with_mailbox on the same connection"] at
      once and sends nothing. Use the [Selected] operations on the lease
      instead. A [Client] call on another connection is unaffected.

      The selected handle becomes stale when [callback] returns. A normal
      exit sends UNSELECT where negotiated and otherwise closes the
      connection. An exceptional exit closes the connection and re-raises.
      If UNSELECT fails, the connection closes and the callback's result is
      still returned. Selected commands are serialized. Join their fibers
      before returning, since an in-flight command closes the connection at
      lease exit.

      [qresync] is a saved UIDVALIDITY and completed MODSEQ checkpoint, and is
      accepted only when QRESYNC was successfully enabled. [objectid] is the
      draft [(account_id, mailbox_id)] identity of the intended mailbox. It
      is [Error.Not_enabled Objectid_plus] until {!Objectid_plus.enable}
      succeeds. The client checks the SELECT response before invoking
      [callback], closing the connection if the server fell back to a
      different mailbox. [qresync] and [objectid] are omitted by default. *)

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

  val noop : t -> (Imap.Response.t list, error) result
  (** [noop t] sends a keepalive and returns unsolicited updates in wire order.
      Use between mailbox leases; use [Selected.noop] inside a lease. Updates
      are observations, not durable checkpoints. *)

  val logout : t -> (unit, error) result
  (** [logout t] waits for BYE and tagged completion, then closes the transport.
      Errors and cancellation also close it. Use between mailbox leases.
      Apply an Eio timeout externally when shutdown needs a deadline.
      [close] immediately releases the transport without a protocol exchange. *)

  module Acl : sig
    (** RFC 4314 access control lists. Identifiers are sent verbatim as IMAP
        astrings, so caller policy must handle identity preparation. *)

    type client := t
    type t

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported Acl]. *)

    val get_acl : t -> mailbox:string -> (Imap.Response.acl, error) result
    (** [get_acl t ~mailbox] is the access control list of [mailbox]. *)

    val list_rights : t -> mailbox:string -> identifier:string ->
      (Imap.Response.list_rights, error) result
    (** [list_rights t ~mailbox ~identifier] is the rights [identifier] can
        be granted on [mailbox]. *)

    val my_rights : t -> mailbox:string ->
      (Imap.Response.my_rights, error) result
    (** [my_rights t ~mailbox] is the rights of the authenticated user on
        [mailbox]. *)

    val set_acl : t -> mailbox:string -> identifier:string ->
      operation:[ `Add | `Remove | `Replace ] -> rights:string ->
      (unit, error) result
    (** [set_acl t ~mailbox ~identifier ~operation ~rights] adds, removes or
        replaces the [rights] of [identifier] on [mailbox]. An empty
        [rights] with [`Add] or [`Remove] is [Error.State]. *)

    val delete_acl : t -> mailbox:string -> identifier:string ->
      (unit, error) result
    (** [delete_acl t ~mailbox ~identifier] removes every right of
        [identifier] on [mailbox]. *)
  end

  module Quota : sig
    (** RFC 9208 QUOTA. [require] holds when the server offers QUOTA or
        any QUOTA=RES-* resource. *)

    type client := t
    type t

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported Quota]. *)

    val get_quota : t -> root:string -> (Imap.Response.quota, error) result
    (** [get_quota t ~root] is the usage and limits of the quota root
        [root]. *)

    val get_quota_root : t -> mailbox:string ->
      ((Imap.Response.quota_root * Imap.Response.quota list), error) result
    (** [get_quota_root t ~mailbox] is the quota roots of [mailbox] with the
        QUOTA of each root the server reported. *)

    val set_quota : t -> root:string -> limits:(string * int64) list ->
      (Imap.Response.quota option, error) result
    (** [set_quota t ~root ~limits] replaces every limit on [root] with
        [limits], so an omitted resource loses its limit. It is
        [Error.Unsupported Quotaset] unless the server offers QUOTASET. A
        returned QUOTA is the server's rounded or actual values. *)
  end

  module Metadata : sig
    (** RFC 5464 METADATA. [require] holds when the server offers METADATA
        or METADATA-SERVER. *)

    type client := t
    type t

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported
        Metadata_server] when the server offers neither. *)

    val get_metadata : t -> mailbox:string -> entries:string list ->
      ?maxsize:int64 -> ?depth:Imap.Metadata.depth -> unit ->
      (metadata_result, error) result
    (** [get_metadata t ~mailbox ~entries ()] is the values of [entries] on
        [mailbox], where an empty [mailbox] names server metadata.
        [maxsize] and [depth] are omitted by default. A nonempty [mailbox]
        is [Error.Unsupported Metadata] when the server offers only
        METADATA-SERVER. *)

    val set_metadata : t -> mailbox:string ->
      values:(string * string option) list -> (unit, error) result
    (** [set_metadata t ~mailbox ~values] sets each entry of [values] on
        [mailbox], removing it for [None], with [mailbox] scoped as in
        {!get_metadata}. A value that needs a literal is [Error.State]. *)
  end

  module Notify : sig
    (** RFC 5465 NOTIFY outside a lease. Selected filters need
        {!Selected.Notify}, and a selected filter here is [Error.State]. Use
        a dedicated connection and reconcile after any notification
        overflow. *)

    type client := t
    type t

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported Notify]. *)

    val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
      unit -> (Imap.Response.mailbox_status list, error) result
    (** [notify_set t ~groups ()] registers [groups] as
        {!Selected.Notify.notify_set} does. [status] defaults to
        [false]. *)

    val notify_none : t -> (unit, error) result
    (** [notify_none t] cancels every registration on the connection. *)
  end

  module Multiappend : sig
    (** RFC 3502 MULTIAPPEND. *)

    type client := t
    type t

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported
        Multiappend]. *)

    val append_many : t -> mailbox:string -> append_message list ->
      (multiappend_receipt option, error) result
    (** [append_many t ~mailbox messages] streams 1 to 1,000 nonempty
        messages as one atomic APPEND, with no sequential fallback.
        Advertised MESSAGELIMIT and SAVELIMIT caps and all syntax are
        checked before dispatch. Literals use a fixed-size streaming
        buffer, and the literal and uncertainty rules of {!append} apply.
        A rejection aborts the whole batch. A lost completion or an
        APPENDUID that does not name one UID per message is
        [Error.Uncertain] and closes the connection, and cancellation also
        closes it. [None] means success without UID evidence. The batch is
        not journalled or replayed. Binary literals are not supported
        here. *)
  end

  module Compress : sig
    (** RFC 4978 COMPRESS=DEFLATE. *)

    type client := t
    type t

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported (Compress
        `Deflate)]. *)

    val activate : t -> (unit, error) result
    (** [activate t] sends COMPRESS DEFLATE and switches the connection to
        compression after the tagged OK. A NO or BAD keeps the plaintext
        transport and the typed rejection code, and a second activation is
        [Error.State]. Compression wraps the current transport, including
        TLS, and lasts for the connection. Cancellation, malformed streams
        and lost framing close it, and mutation outcomes stay uncertain
        after dispatch. More than 16 MiB of compressed input without
        decoded output counts as a malformed stream. Decompressed data is
        subject to the ordinary parser and command limits. Compression is
        never active during credential exchange. Consider compression side
        channels when mixing secret and attacker-controlled data. STARTTLS
        after compression is not supported. *)
  end

  module Objectid_plus : sig
    (** The OBJECTID+ draft mode. It is separate from the RFC 8474 OBJECTID
        capability and stays active for the connection once enabled. *)

    type client := t
    type t

    val enable : client -> (t, error) result
    (** [enable c] is [Client.enable c [Objectid_plus]] returning a witness,
        and [Error.Protocol] if the server does not confirm it. Enabling
        changes SELECT identity response codes to compound OBJECTID. *)

    val pin_mailbox : t -> mailbox:string -> account_id:string ->
      mailbox_id:string -> (unit, error) result
    (** [pin_mailbox t ~mailbox ~account_id ~mailbox_id] binds [mailbox] to
        a previously verified compound identity for the connection. Later
        {!with_mailbox} calls select by that identity and reject a name
        fallback to another mailbox, and {!append} checks the name with
        STATUS before sending message bytes. Rebinding to a different
        identity and pinning during a lease are [Error.State]. *)

    val create_mailbox : t -> mailbox:string ->
      (Imap.Response.compound_object_id, error) result
    (** [create_mailbox t ~mailbox] is {!Client.create_mailbox} returning
        the tagged account and mailbox identity. If the server omits either
        identifier after a successful CREATE, the connection closes and
        the result is [Error.Uncertain], so reconcile before retrying. *)

    val rename_mailbox : t -> old_name:string -> new_name:string ->
      (Imap.Response.compound_object_id, error) result
    (** [rename_mailbox t ~old_name ~new_name] is {!Client.rename_mailbox}
        returning the tagged identity, under the conditions of
        {!create_mailbox}. *)

    val status : t -> mailbox:string -> items:Imap.Status_item.t list ->
      (Imap.Response.mailbox_status, error) result
    (** [status t ~mailbox ~items] is {!Client.status}, where the
        [Objectid] item is always allowed. *)
  end

  module Uidonly : sig
    (** The RFC 9586 UIDONLY mode. UIDFETCH and VANISHED replace
        sequence-based updates, and the mode cannot be disabled, so use a
        dedicated connection. *)

    type client := t
    type t

    val enable : client -> (t, error) result
    (** [enable c] is [Client.enable c [Uidonly]] returning a witness, and
        [Error.Protocol] if the server does not confirm it. The witness is
        the proof a caller passes to code that needs the mode. *)
  end
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
