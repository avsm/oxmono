(** Eio IMAP connections, mailbox leases and extension witnesses.

    A {!Client.t} is one authenticated connection. The switch passed to
    {!Client.connect} owns it, and releasing that switch closes it.
    Commands on a connection are serialized across fibers.

    {!Client.with_mailbox} selects a mailbox and passes its callback a
    {!Selected.t} lease. The lease expires when the callback returns, and
    every witness and saved search obtained from it expires with it. An
    operation on an expired lease or witness is [Error.State]. A [Client]
    call on the same connection from inside the callback, or from a fiber
    the callback forked, is [Error.State] and sends nothing.

    Cancelling a fiber while its command runs closes the connection, since
    later replies can no longer be matched to their commands. Any error
    other than [Error.Rejected] that arises while a reply is being read also
    closes it. Once a mutating command has been sent, a failure other than
    a tagged rejection is [Error.Uncertain]. The server may or may not have
    applied the command, so the caller reconciles before retrying. The
    client never replays a command.

    Every extension is named by an {!Imap.Capability.t}. An operation that
    exists only because of an extension lives in a submodule named for it.
    The submodule's [require], or [enable] for a mode, checks the gate once
    and is a witness that the submodule's operations take in place of the
    lease or the client. A capability the server does not offer is
    [Error.Unsupported c], and one it offers that ENABLE has not confirmed
    is [Error.Not_enabled c]. Neither sends anything or affects the
    connection. {!Client.has} folds in the extensions that IMAP4rev2 makes
    part of the base protocol, so their [require] succeeds on an
    IMAP4rev2 server that does not advertise them. *)

(** {1 Credentials} *)

module Auth : sig
  (** IMAP login credentials.

      A credential holds a username, a secret or a function that fetches
      one, a mechanism and a transport policy. [`Auto] chooses SASL PLAIN
      when the server advertises it and the transport is TLS, then CRAM-MD5
      when the server advertises it, then LOGIN. An explicit SASL mechanism
      the server does not advertise is [Error.Unsupported (Auth name)]. A
      rejected mechanism never falls back to another.

      PLAIN, OAUTHBEARER and LOGIN, including the LOGIN that [`Auto] falls
      back to, need a TLS transport unless [allow_insecure_transport]
      holds, and are [Error.State] otherwise. LOGIN is also [Error.State]
      when the server advertises LOGINDISABLED. CRAM-MD5 runs without TLS
      and leaves the mailbox traffic that follows it unprotected. A
      transport is TLS when it is [`Implicit], or once [`Required_starttls]
      has upgraded it. A flow given to {!Client.of_flow} is never TLS.

      A secret the chosen mechanism cannot send, such as an empty password
      under PLAIN, fails authentication with
      [Error.State "invalid credentials"] before any secret is sent.

      The constructors and accessors below are portable, so a credential
      may be built and inspected on any domain. *)

  type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
  (** The type for authentication mechanisms. [`Auto] chooses among PLAIN,
      CRAM-MD5 and LOGIN as described above. *)

  type t
  (** The type for credentials. *)

  val password : username:string -> password:string -> ?mechanism:mechanism ->
    ?allow_insecure_transport:bool -> unit -> t @@ portable
  (** [password ~username ~password ~mechanism ~allow_insecure_transport ()]
      is a credential holding the fixed [password] for [username].
      [mechanism] defaults to [`Auto]. [allow_insecure_transport] defaults
      to [false].

      @raise Invalid_argument if [username] is empty, is not UTF-8 or
      contains a control character, if [password] contains NUL, if
      [mechanism] is [`Oauthbearer], or if [mechanism] is [`Cram_md5] and
      [username] contains a space or a tab. *)

  val refreshing : username:string -> ?mechanism:mechanism ->
    ?allow_insecure_transport:bool -> (unit -> string) -> t @@ portable
  (** [refreshing ~username ~mechanism ~allow_insecure_transport get] is a
      credential for [username] that calls [get] for the password at each
      authentication. [mechanism] defaults to [`Auto].
      [allow_insecure_transport] defaults to [false]. A password from [get]
      that contains NUL, or an exception from [get] other than
      cancellation, fails authentication with
      [Error.State "invalid credentials"] before any secret is sent.

      @raise Invalid_argument if {!password} would reject [username] or
      [mechanism]. *)

  val bearer : username:string -> token:string ->
    ?allow_insecure_transport:bool -> unit -> t @@ portable
  (** [bearer ~username ~token ~allow_insecure_transport ()] is an
      OAUTHBEARER credential holding the fixed [token] for [username].
      [allow_insecure_transport] defaults to [false].

      @raise Invalid_argument if {!password} would reject [username], or
      if [token] is empty, longer than 32,768 bytes or not an RFC 6750
      b64token. *)

  val refreshing_bearer : username:string ->
    ?allow_insecure_transport:bool -> (unit -> string) -> t @@ portable
  (** [refreshing_bearer ~username ~allow_insecure_transport get] is an
      OAUTHBEARER credential for [username] that calls [get] for the token
      at each authentication. [allow_insecure_transport] defaults to
      [false]. A token that {!bearer} would reject, or an exception from
      [get] other than cancellation, fails authentication with
      [Error.State "invalid credentials"] before any secret is sent.

      @raise Invalid_argument if {!password} would reject [username]. *)

  val username : t -> string @@ portable
  (** [username t] is the username of [t]. *)

  val mechanism : t -> mechanism @@ portable
  (** [mechanism t] is the mechanism of [t]. It is [`Oauthbearer] for a
      bearer credential. *)

  val allow_insecure_transport : t -> bool @@ portable
  (** [allow_insecure_transport t] holds when [t] may use PLAIN,
      OAUTHBEARER or LOGIN without TLS. *)
end

(** {1 Errors} *)

module Error : sig
  (** IMAP client failures. *)

  type t =
    | Closed  (** The connection is closed. *)
    | Protocol of string
        (** The server's reply violates the protocol or contradicts the
            request. *)
    | Transport of string  (** The network or TLS layer failed. *)
    | Rejected of { tag : string; status : [ `No | `Bad ];
        code : Imap.Response.code option; text : string }
        (** The server completed the command with a tagged NO or BAD.
            [code] is the tagged response
            code, typed when known and {!Imap.Response.Other_code}
            otherwise. [text] is the server's explanation. A rejected
            authentication keeps only a standard code that carries no
            payload, and its [text] is a fixed diagnostic, so credentials
            the server echoes cannot enter the error. A rejection does not
            by itself make a retry safe, since a partially applied
            mutation is [Uncertain] instead. *)
    | State of string
        (** A local precondition failed, such as an expired lease, a
            nested call, an argument the command cannot encode or a
            read-only mailbox. *)
    | Missing_uid of Imap.Uid.t
        (** A completed FETCH returned no row for the requested UID. *)
    | Limit of string
        (** A client bound or an advertised limit was exceeded. *)
    | Uncertain of string
        (** The server may have applied a mutation whose outcome was lost.
            The connection is closed. *)
    | Unsupported of Imap.Capability.t
        (** The server neither advertises the capability nor has it folded
            into effective IMAP4rev2. A missing MESSAGELIMIT is
            [Unsupported (Other "MESSAGELIMIT")]. Nothing was sent. *)
    | Not_enabled of Imap.Capability.t
        (** The server offers the capability but ENABLE has not confirmed
            it. Nothing was sent. *)
  (** The type for client errors. An error is immutable data, so it may be
      shared between domains. *)
end

(** {1 Endpoints} *)

module Transport : sig
  (** IMAP endpoints and their network authority. *)

  type tls = [ `Implicit | `Required_starttls | `Plain ]
  (** The type for transport security. [`Implicit] is TLS from the first
      byte. [`Required_starttls] connects in plaintext and upgrades with
      STARTTLS before authentication. [`Plain] never uses TLS and is only
      for an explicitly trusted test server. *)

  type t
  (** The type for endpoints. An endpoint holds a network, an address and
      a TLS configuration, and no connection. *)

  val v :
    net:_ Eio.Net.t ->
    host:string ->
    ?port:int ->
    ?tls:tls ->
    ?authenticator:X509.Authenticator.t @ portable ->
    unit -> t
  (** [v ~net ~host ~port ~tls ~authenticator ()] is the endpoint [host] on
      [port], reached through [net]. [tls] defaults to [`Implicit]. [port]
      defaults to 993 when [tls] is [`Implicit] and to 143 otherwise.
      [authenticator] checks the server certificate and defaults to the
      system trust store, which [v] loads unless [tls] is [`Plain]. [v]
      resolves and connects nothing.

      @raise Invalid_argument if [host] is empty, if [port] is outside
      1 to 65535, or if [tls] is not [`Plain] and [host] is neither an IP
      address nor a valid host name.

      @raise Failure if [authenticator] is omitted, [tls] is not [`Plain]
      and the system trust store cannot be loaded. *)

  val host : t -> string
  (** [host t] is the host name or address of [t]. *)

  val port : t -> int
  (** [port t] is the TCP port of [t]. *)

  val tls : t -> tls
  (** [tls t] is the transport security of [t]. *)

end

(** {1 Mailbox leases} *)

module Selected : sig
  (** Selected mailbox leases.

      {!Client.with_mailbox} creates a lease and passes it to its callback.
      Commands on a lease are serialized across fibers. The lease expires
      when the callback returns. A command still running at that moment
      closes the connection, so a callback that forks fibers must join
      them before it returns.

      The values at this level need only IMAP4rev1, or gate each call on
      the typed criteria or items they are given. The submodules hold the
      operations that exist only because of an extension. *)

  type t
  (** The type for leases on a selected mailbox. *)

  val info : t -> (Imap.Response.select_metadata, Error.t) result
  (** [info t] is the metadata of the SELECT or EXAMINE that created
      [t]. *)

  val select_updates : t -> (Imap.Response.t list, Error.t) result
  (** [select_updates t] is the untagged responses of the SELECT or EXAMINE
      that created [t], in wire order, including the FETCH and VANISHED
      responses of a QRESYNC selection. *)

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
  (** The type for the FETCH data of one message. A field is [None], and
      [binary_sizes] is empty, when its item was not requested or the
      server did not report it. [flags] is the last FLAGS reported and can
      include the transient [\Recent]. [size] is RFC822.SIZE.
      [thread_id = Some None] is an RFC 8474 THREADID NIL.
      [preview = Some None] is a LAZY PREVIEW NIL, and [Some (Some "")]
      means the server found no meaningful preview. [objectid] is the
      OBJECTID+ message identity, which never carries an account or
      mailbox identifier. [binary_sizes] pairs each requested BINARY.SIZE
      section with its decoded size in octets. *)

  val uid_search :
    t -> criteria:Imap.Search.t -> (Imap.Uid.t list, Error.t) result
  (** [uid_search t ~criteria] is the UIDs matching [criteria], sorted and
      without duplicates. The reply must hold exactly one SEARCH response
      or one UID ESEARCH response correlated with the command, and a
      missing response is [Error.Protocol], not an empty result. More than
      100,000 UIDs is [Error.Limit].

      Every search in this module encodes its criteria with
      {!Imap.Search.to_wire}, which allows non-ASCII strings once
      UTF8=ACCEPT is enabled or IMAP4rev2 is in effect. An encoding error
      is [Error.State]. The capabilities {!Imap.Search.capabilities} lists
      for the criteria are required, and QRESYNC satisfies CONDSTORE. Once
      UIDONLY is enabled, criteria that fail {!Imap.Search.uidonly_safe}
      are [Error.State]. The server validates the grammar of a [Raw]
      criterion. *)

  val uid_search_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
    (Imap.Uid.t list, Error.t) result
  (** [uid_search_range t ~first ~last] is the sorted distinct UIDs from
      [first] to [last] that exist, returned only after the server has
      processed every UID of the window. When the server advertises
      RFC 9738 MESSAGELIMIT, the search resumes below each processed-UID
      boundary, and more than 1,000 pages is [Error.Limit]. A UID outside
      the window is [Error.Protocol]. A window with [last] below [first]
      or of more than 1,000 UIDs is [Error.State]. *)

  type sort_result = {
    count : int64;
    first : Imap.Uid.t option;
    last : Imap.Uid.t option;
    uids : Imap.Uid.t list option;
    range : (int64 * int64) option;
  }
  (** The type for the result of {!Esort.uid_sort_extended}. [count] is the
      number of matching messages. [first] and [last] are MIN and MAX in
      sort order, not numeric UID extrema. [uids = None] means no UID list
      was returned, and [Some []] is a verified empty ALL or PARTIAL
      result. [range] is the requested PARTIAL range as given, including a
      reversed one. *)

  type thread = { uid : Imap.Uid.t option; children : thread list }
  (** The type for UID THREAD nodes. [uid] is [None] for a dummy parent
      that groups its [children]. *)

  type search_page = {
    uids : Imap.Uid.t list;
    complete : bool;
    limit : int64 option;
    resume_before : Imap.Uid.t option;
  }
  (** The type for pages of {!Messagelimit.uid_search_page}. [uids] are
      sorted. [complete] is [false] exactly when [resume_before] is
      [Some _], the UID below which the next page continues. [limit] is the
      MESSAGELIMIT a partial reply reported, and [None] after a reply that
      was not partial. *)

  val fetch_to : t -> ?max_bytes:int64 -> uid:Imap.Uid.t ->
    _ Eio.Flow.sink -> (unit, Error.t) result
  (** [fetch_to t ~max_bytes ~uid sink] writes the full message of [uid] to
      [sink] with UID FETCH BODY.PEEK[], which leaves [\Seen] unchanged. A
      literal body is written as it arrives, and a quoted body after tagged
      completion. Bytes in [sink] are provisional until [Ok ()] confirms
      the UID and the body length. [max_bytes] defaults to 1 GiB and
      bounds the body, and a larger body is [Error.Limit]. A negative
      [max_bytes] is [Error.State]. A tagged success with no body for
      [uid] is [Error.Missing_uid] and keeps the connection usable. A reply
      that does not match [uid] and the written length closes the
      connection and is [Error.Protocol]. A failing [sink] closes the
      connection and is [Error.State]. *)

  val fetch : t -> uids:Imap.Uid.t list -> items:Imap.Fetch_item.t list ->
    (row list, Error.t) result
  (** [fetch t ~uids ~items] is the rows of one UID FETCH of [items] for
      [uids], with a row for each requested UID the server reported, in
      request order. UID and FLAGS are always requested. A repeated UID
      counts once, more than 1,000 distinct UIDs is [Error.State], and an
      empty [uids] is [Ok []] without a command. A UID with no row may
      have been expunged. The capabilities {!Imap.Fetch_item.capabilities}
      lists for [items] are required, with QRESYNC satisfying CONDSTORE and
      IMAP4rev2 satisfying BINARY, and OBJECTID+ must be enabled. EMAILID
      and THREADID need a MAILBOXID in the selection, and OBJECTID an
      ACCOUNTID and a MAILBOXID, else [Error.Protocol].

      Every FETCH of metadata in this module applies one row policy. A row
      for a UID that was not requested is unsolicited and ignored. Rows for
      one UID merge. FLAGS and MODSEQ are live state and take the last
      value reported, and a differing value for any other item is
      [Error.Protocol]. A row without a UID is ignored when it carries only
      FLAGS or MODSEQ and is [Error.Protocol] otherwise. A requested item
      missing from a row leaves its field empty. Malformed item data and a
      PREVIEW NIL without LAZY are [Error.Protocol]. Body items do not
      exist here. {!fetch_to} and {!Binary.fetch_binary_to} stream
      bodies. *)

  val fetch_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
    items:Imap.Fetch_item.t list -> (row list, Error.t) result
  (** [fetch_range t ~first ~last ~items] is the rows of [items] for the
      UIDs from [first] to [last], under the row policy and gates of
      {!fetch}, in ascending UID order. A window with [last] below [first]
      or of more than 1,000 UIDs is [Error.State]. An RFC 9738 MESSAGELIMIT
      partial success is continued below the processed UID. A missing or
      contradictory boundary fails the call, and more than 1,000 pages is
      [Error.Limit]. Absence from the result does not prove an expunge, so
      membership needs a separate check before it is published. *)

  type store_receipt = {
    modified : Imap.Uid_set.t;
    updates : Imap.Response.fetch list;
  }
  (** The type for STORE receipts. [modified] is the server's RFC 7162
      conflict set, empty for an unconditional STORE. [updates] are the
      FETCH rows the STORE reported. *)

  val uid_store_flags : t -> set:Imap.Uid_set.t ->
    operation:[ `Add | `Remove | `Replace ] ->
    flags:Mail_flag.Imap_flag.t list -> (store_receipt, Error.t) result
  (** [uid_store_flags t ~set ~operation ~flags] adds, removes or replaces
      [flags] on the messages of [set], as [operation] says, with one UID
      STORE. A read-only mailbox and an empty [set] are [Error.State]. A
      receipt that cannot be decoded after tagged success closes the
      connection and is [Error.Uncertain]. {!Condstore.uid_store_flags} is
      the conditional form. *)

  type copy_mapping = {
    source_first : Imap.Uid.t;
    destination_first : Imap.Uid.t;
    length : int64;
  }
  (** The type for one COPYUID range. It maps [source_first + i] to
      [destination_first + i] for [0 <= i < length]. *)

  type copy_receipt = {
    uidvalidity : Imap.Uidvalidity.t;
    source : Imap.Uid_set.t;
    destination : Imap.Uid_set.t;
    mapping : copy_mapping list;
  }
  (** The type for RFC 4315 COPYUID receipts. [uidvalidity] is the
      destination's. [mapping] keeps the correspondence in wire element
      order, in compact ranges. [source] and [destination] describe
      membership only, not positional pairing. *)

  val uid_copy : t -> set:Imap.Uid_set.t -> mailbox:string ->
    (copy_receipt option, Error.t) result
  (** [uid_copy t ~set ~mailbox] copies the messages of [set] to the UTF-8
      name [mailbox] and is the COPYUID receipt, or [None] when the server
      sent none. An empty [set] is [Error.State]. A COPYUID that names a
      UID outside [set], repeats a UID or pairs sets of different sizes
      closes the connection and is [Error.Uncertain]. An uncertain copy
      needs reconciliation, not replay. *)

  val noop : t -> (Imap.Response.t list, Error.t) result
  (** [noop t] sends NOOP on the lease and is the untagged responses it
      returned, in wire order. They report changes and establish no
      durable checkpoint. *)

  (** {2 Extensions} *)

  module Condstore : sig
    (** RFC 7162 CONDSTORE. [require] holds when the server offers
        CONDSTORE or QRESYNC. *)

    type selected := t

    type t
    (** The type for CONDSTORE witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Condstore]. *)

    val uid_store_flags : t -> set:Imap.Uid_set.t ->
      operation:[ `Add | `Remove | `Replace ] ->
      flags:Mail_flag.Imap_flag.t list -> unchangedsince:int64 ->
      (store_receipt, Error.t) result
    (** [uid_store_flags t ~set ~operation ~flags ~unchangedsince] is
        {!Selected.uid_store_flags} with UNCHANGEDSINCE [unchangedsince].
        [unchangedsince] is an RFC 7162 [mod-sequence-valzer] rather than
        an {!Imap.Modseq.t}, because 0 is valid there and no existing
        message satisfies it. [modified] in the receipt lists the UIDs the
        server refused to change. *)

    val fetch_changes_range : t -> first:Imap.Uid.t -> last:Imap.Uid.t ->
      since:Imap.Modseq.t -> (Imap.Response.fetch list, Error.t) result
    (** [fetch_changes_range t ~first ~last ~since] is the UID, FLAGS and
        MODSEQ rows changed after [since] for the UIDs from [first] to
        [last], in ascending UID order. A window with [last] below [first]
        or of more than 1,000 UIDs is [Error.State]. Rows without FLAGS,
        and rows without a UID in the window, are ignored. An RFC 9738
        MESSAGELIMIT partial reply is continued below the processed UID. A
        missing or contradictory boundary fails the call, and more than
        1,000 pages is [Error.Limit]. No VANISHED rows are returned, so
        complete membership needs an independent check before absence is
        published or a durable checkpoint advances. *)
  end

  module Qresync : sig
    (** RFC 7162 QRESYNC. [require] holds once ENABLE has confirmed
        QRESYNC. *)

    type selected := t

    type t
    (** The type for QRESYNC witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s]. It is [Error.Unsupported Qresync]
        when the server does not offer QRESYNC and [Error.Not_enabled
        Qresync] when ENABLE has not confirmed it. *)

    val fetch_changes : t -> set:Imap.Uid_set.t ->
      since:Imap.Modseq.t -> vanished:bool ->
      (Imap.Response.t list, Error.t) result
    (** [fetch_changes t ~set ~since ~vanished] is the FETCH, VANISHED and
        HIGHESTMODSEQ responses of a UID FETCH CHANGEDSINCE [since] for
        [set], in wire order, with VANISHED requested when [vanished]
        holds. An empty [set] is [Error.State]. The caller also discovers
        new UIDs and accounts for command boundaries before it moves a
        durable checkpoint. *)
  end

  module Uidplus : sig
    (** RFC 4315 UIDPLUS. [require] holds when the server offers UIDPLUS or
        is in effective IMAP4rev2. *)

    type selected := t

    type t
    (** The type for UIDPLUS witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Uidplus]. *)

    val uid_expunge : t -> set:Imap.Uid_set.t -> (unit, Error.t) result
    (** [uid_expunge t ~set] expunges the messages of [set] that are marked
        [\Deleted], and no others. It never falls back to a mailbox-wide
        EXPUNGE. A read-only mailbox and an empty [set] are [Error.State].
        An uncertain expunge needs reconciliation, not replay. *)
  end

  module Move : sig
    (** RFC 6851 MOVE. [require] holds when the server offers MOVE or is in
        effective IMAP4rev2. *)

    type selected := t

    type t
    (** The type for MOVE witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Move]. *)

    val uid_move : t -> set:Imap.Uid_set.t -> mailbox:string ->
      (copy_receipt option, Error.t) result
    (** [uid_move t ~set ~mailbox] moves the messages of [set] to the UTF-8
        name [mailbox] under the receipt rules of {!Selected.uid_copy}. A
        read-only mailbox is [Error.State]. *)
  end

  module Binary : sig
    (** RFC 3516 BINARY FETCH. [require] holds when the server offers
        BINARY or is in effective IMAP4rev2, which folds in the FETCH side
        of BINARY. *)

    type selected := t

    type t
    (** The type for BINARY FETCH witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Binary]. *)

    val fetch_binary_to : t -> ?max_bytes:int64 ->
      ?partial:(int64 * int64) -> uid:Imap.Uid.t -> section:int list ->
      _ Eio.Flow.sink -> (int64 option, Error.t) result
    (** [fetch_binary_to t ~max_bytes ~partial ~uid ~section sink] writes
        the decoded BINARY.PEEK bytes of the MIME leaf [section] of [uid]
        to [sink], leaving [\Seen] unchanged, and is their length. [None]
        is an explicit NIL, and [Some 0L] an empty string or literal. The
        server validates that [section] exists.

        [partial] is omitted by default. When given as [(offset, count)] it
        addresses decoded bytes, the response origin must match [offset],
        and a short read at the end is allowed. [max_bytes] defaults to
        1 GiB and bounds the body together with [count], and a larger body
        is [Error.Limit]. A negative [max_bytes] is [Error.State]. The wire
        framer bounds each literal at 1 GiB whatever [max_bytes] is.

        A tagged success with no row for [uid] is [Error.Missing_uid]. A
        missing or wrong section, origin or UID, or an extra literal,
        closes the connection and is [Error.Protocol]. UNKNOWN-CTE and
        other tagged failures are [Error.Rejected]. Bytes in [sink] are
        provisional until [Ok] confirms the metadata and tagged success,
        and are discarded on any error or cancellation. A failing [sink]
        closes the connection and is [Error.State]. Decoded parts are not
        the raw RFC 5322 message and never replace archived body
        bytes. *)
  end

  module Searchres : sig
    (** RFC 5182 SEARCHRES. [require] holds when the server offers
        SEARCHRES or is in effective IMAP4rev2. *)

    type selected := t

    type t
    (** The type for SEARCHRES witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Searchres]. *)

    type saved_search
    (** The type for server-side saved search results. A saved search
        belongs to the lease it was saved on. Its set shrinks as matching
        messages are expunged, so it is neither a snapshot nor a durable
        UID inventory. Any later ordinary UID SEARCH on the connection
        makes older saved searches stale, including a rejected search, a
        [Raw] RETURN (SAVE) and a new saved search. So does a response that
        resets the mailbox identity. Only {!uid_search_saved} refinement
        keeps a saved search valid. An operation on a stale saved search is
        [Error.State]. A saved search is checked and its command sent under
        the lease's command serialization, so a concurrent search cannot
        make it stale in between. *)

    val uid_search_save :
      t -> criteria:Imap.Search.t -> (saved_search, Error.t) result
    (** [uid_search_save t ~criteria] saves the UIDs matching [criteria] on
        the server and is the saved search. The command requests SAVE and
        COUNT, and the reply must hold exactly one correlated UID ESEARCH
        with COUNT, else [Error.Protocol]. An empty saved set is valid.
        [criteria] follows the rules of {!Selected.uid_search}. *)

    val uid_search_saved : saved_search -> criteria:Imap.Search.t ->
      (Imap.Uid.t list, Error.t) result
    (** [uid_search_saved saved ~criteria] is the UIDs of the live saved
        set of [saved] that match [criteria], and keeps [saved] valid. The
        command requests ALL and COUNT for [UID $] and the grouped
        [criteria], so a [Raw] criterion cannot inject RETURN or SAVE. The
        reply must hold exactly one correlated UID ESEARCH result whose
        ALL, COUNT and saved COUNT agree, with no repeated UID, else
        [Error.Protocol]. More than 100,000 UIDs is [Error.Limit].
        [criteria] follows the rules of {!Selected.uid_search}. An ordinary
        [uid_search] makes [saved] stale even when its criteria name the
        saved result. *)

    val uid_fetch_saved : saved_search -> ?partial:(int64 * int64) ->
      items:Imap.Fetch_item.t list -> unit -> (row list, Error.t) result
    (** [uid_fetch_saved saved ~partial ~items ()] is the rows of [items]
        for the saved set of [saved] under the row policy of
        {!Selected.fetch}, in ascending UID order. [partial] is omitted by
        default. When given it is an RFC 9394 position range and is
        [Error.Unsupported Partial] unless the server offers PARTIAL. Every
        reported UID is accepted because the saved set is not known
        locally, so rows can include unsolicited updates and do not prove
        membership. A MESSAGELIMIT partial result is [Error.Limit]. *)

    val uid_store_saved : saved_search ->
      operation:[ `Add | `Remove | `Replace ] ->
      flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 -> unit ->
      (store_receipt, Error.t) result
    (** [uid_store_saved saved ~operation ~flags ~unchangedsince ()] is a
        STORE of [flags] on the saved set of [saved] with the checks of
        {!Selected.uid_store_flags}. [unchangedsince] is omitted by
        default. When given it makes the STORE conditional as in
        {!Condstore.uid_store_flags}, and is [Error.Unsupported Condstore]
        without CONDSTORE or QRESYNC. A mailbox identity reset or a lost
        completion after dispatch is [Error.Uncertain]. *)

    val uid_copy_saved :
      saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
    (** [uid_copy_saved saved ~mailbox] is {!Selected.uid_copy} of the saved
        set of [saved] to [mailbox]. An empty saved set is valid. *)

    val uid_move_saved :
      saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
    (** [uid_move_saved saved ~mailbox] is {!Move.uid_move} of the saved set
        of [saved] to [mailbox], and is [Error.Unsupported Move] unless the
        server offers MOVE or is in effective IMAP4rev2. The move shrinks
        the saved set without making [saved] stale. *)

    val uid_expunge_saved : saved_search -> (unit, Error.t) result
    (** [uid_expunge_saved saved] is {!Uidplus.uid_expunge} of the saved set
        of [saved], and is [Error.Unsupported Uidplus] unless the server
        offers UIDPLUS or is in effective IMAP4rev2. The expunge shrinks
        the saved set without making [saved] stale. *)

    val saved_search_count : saved_search -> int64
    (** [saved_search_count saved] is the COUNT reported when [saved] was
        saved, not the current size of its live set. *)
  end

  module Sort : sig
    (** RFC 5256 SORT. [require] holds when the server offers SORT or
        SORT=DISPLAY. *)

    type selected := t

    type t
    (** The type for SORT witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Sort]. *)

    val uid_sort : t -> keys:(Imap.Sort.key * Imap.Sort.order) list ->
      charset:string -> criteria:Imap.Search.t ->
      (Imap.Uid.t list, Error.t) result
    (** [uid_sort t ~keys ~charset ~criteria] is the UIDs of the messages
        matching [criteria] in the server's order for [keys], with strings
        compared in [charset]. An explicit empty SORT result is [Ok []]. A
        missing or repeated result, a malformed one and a repeated UID are
        [Error.Protocol], and more than 100,000 UIDs and a MESSAGELIMIT
        partial result are [Error.Limit]. [criteria] follows the rules of
        {!Selected.uid_search}. The result describes current membership,
        not a durable snapshot. *)
  end

  module Esort : sig
    (** RFC 5267 ESORT. [require] holds when the server offers ESORT. *)

    type selected := t

    type t
    (** The type for ESORT witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Esort]. *)

    val uid_sort_extended : t -> returns:Imap.Sort.return list ->
      keys:(Imap.Sort.key * Imap.Sort.order) list ->
      charset:string -> criteria:Imap.Search.t ->
      (sort_result, Error.t) result
    (** [uid_sort_extended t ~returns ~keys ~charset ~criteria] is the ESORT
        summary of {!Sort.uid_sort} [~keys ~charset ~criteria] with the
        fields [returns] names. An empty [returns] requests ALL, and COUNT
        is always requested. A positional PARTIAL is
        [Error.Unsupported (Context `Sort)] unless the server offers
        CONTEXT=SORT, which the PARTIAL capability does not replace. The
        reply must hold exactly one correlated UID ESEARCH response that
        supplies the requested fields consistently, else [Error.Protocol].

        Expansion keeps comma-element order and expands each range in
        ascending order. A repeated UID is [Error.Protocol], and more than
        100,000 UIDs is [Error.Limit], though COUNT alone may exceed that
        bound. A PARTIAL page must match its clipped COUNT and position
        range. No UPDATE context is established, so positions may shift
        between commands and results are not a durable snapshot. *)
  end

  module Thread : sig
    (** RFC 5256 THREAD for one algorithm. [require] holds when the server
        offers THREAD for that algorithm. *)

    type selected := t

    type t
    (** The type for THREAD witnesses, each bound to one algorithm. *)

    val require : selected -> Imap.Thread.algorithm -> (t, Error.t) result
    (** [require s algorithm] is a witness for [s] and [algorithm], or
        [Error.Unsupported (Thread algorithm)] unless the server offers
        THREAD=[algorithm]. *)

    val uid_thread : t -> charset:string -> criteria:Imap.Search.t ->
      (thread list, Error.t) result
    (** [uid_thread t ~charset ~criteria] is the UID THREAD forest of the
        messages matching [criteria] under the witness's algorithm, with
        strings compared in [charset]. It keeps the order of parents and
        children and the dummy grouping nodes. A number outside the UID
        range and a depth over 100 are [Error.Protocol], and more than
        100,000 nodes is [Error.Limit]. An empty result must be explicit. A
        missing, repeated or malformed result is [Error.Protocol], and a
        partial one [Error.Limit]. [criteria] follows the rules of
        {!Selected.uid_search}. Thread trees are server-computed
        relationships, not stable JMAP thread identifiers or a durable
        mailbox snapshot. *)
  end

  module Partial : sig
    (** RFC 9394 PARTIAL. [require] holds when the server offers
        PARTIAL. *)

    type selected := t

    type t
    (** The type for PARTIAL witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Partial]. *)

    val uid_search_partial : t -> range:(int64 * int64) ->
      criteria:Imap.Search.t -> (Imap.Response.esearch, Error.t) result
    (** [uid_search_partial t ~range ~criteria] is the correlated ESEARCH
        page of the results at positions [range]. [partial] in the result
        holds the requested range and the returned UID set, or NIL. A reply
        without exactly one such page is [Error.Protocol]. Positions can
        shift between calls, so pages alone do not prove a complete mailbox
        inventory. [criteria] follows the rules of
        {!Selected.uid_search}. *)

    val uid_fetch_partial : t -> set:Imap.Uid_set.t ->
      items:Imap.Fetch_item.t list -> range:(int64 * int64) ->
      (row list, Error.t) result
    (** [uid_fetch_partial t ~set ~items ~range] is the rows of [items] for
        the messages of [set] at positions [range], under the row policy
        of {!Selected.fetch}, in ascending UID order. An empty [set] is
        [Error.State]. Positions can shift between pages, so pages do not
        prove a complete inventory of [set]. *)
  end

  module Messagelimit : sig
    (** RFC 9738 MESSAGELIMIT. [require] holds when the server advertises a
        MESSAGELIMIT. *)

    type selected := t

    type t
    (** The type for MESSAGELIMIT witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported (Other
        "MESSAGELIMIT")]. *)

    val uid_search_page : ?before:Imap.Uid.t -> t -> criteria:Imap.Search.t ->
      (search_page, Error.t) result
    (** [uid_search_page ~before t ~criteria] is one page of the UIDs
        matching [criteria], below [before] when it is given. [before] is
        omitted by default. The server processes UIDs downwards and may
        stop at its limit. The next page is [uid_search_page] with
        [resume_before] as [before] and the same [criteria]. A partial
        reply without a processed-UID boundary is [Error.Limit], and one
        that contradicts its boundary is [Error.Protocol]. Mailbox changes
        between pages can shift results, so a durable inventory needs an
        independent membership check. [criteria] follows the rules of
        {!Selected.uid_search}. *)
  end

  module Uidbatches : sig
    (** UIDBATCHES, RFC 10022. [require] holds when the server offers
        UIDBATCHES. *)

    type selected := t

    type t
    (** The type for UIDBATCHES witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported
        Uidbatches]. *)

    val uid_batches : t -> ?range:(int64 * int64) -> size:int64 ->
      unit -> (Imap.Response.uidbatches, Error.t) result
    (** [uid_batches t ~range ~size ()] is the server's UID boundaries for
        batches of [size] messages, restricted to the batch indexes [range]
        when it is given. [range] is omitted by default. Boundaries do not
        prove UID membership. A request for a mailbox whose UIDBATCHES
        request this connection already completed with a tagged OK is
        [Error.State] and sends nothing, which keeps within the RFC's
        reissue limit. A rejected request does not count. The reply must
        hold exactly one correlated UIDBATCHES response, else
        [Error.Protocol]. *)
  end

  module Notify : sig
    (** RFC 5465 NOTIFY through a lease. [require] holds when the server
        offers NOTIFY. *)

    type selected := t

    type t
    (** The type for NOTIFY witnesses on a lease. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Notify]. *)

    val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
      unit -> (Imap.Response.mailbox_status list, Error.t) result
    (** [notify_set t ~status ~groups ()] registers [groups] for the
        connection and is the STATUS rows the server sent with the
        registration, which [status] requests. [status] defaults to
        [false]. Selected filters are allowed here, also for EXAMINE, and
        may be combined with others. A NOTIFICATIONOVERFLOW in the reply is
        [Error.Limit], since the server cancelled the registration. *)

    val notify_none : t -> (unit, Error.t) result
    (** [notify_none t] cancels every registration on the connection. *)
  end

  module Idle : sig
    (** RFC 2177 IDLE. [require] holds when the server offers IDLE or is in
        effective IMAP4rev2. *)

    type selected := t

    type t
    (** The type for IDLE witnesses. *)

    val require : selected -> (t, Error.t) result
    (** [require s] is a witness for [s], or [Error.Unsupported Idle]. *)

    val wait_for_change : t -> (Imap.Response.t list, Error.t) result
    (** [wait_for_change t] enters IDLE, waits for an untagged response,
        sends DONE and waits for tagged completion, and is the untagged
        responses that arrived. A tagged NO or BAD is [Error.Rejected] and
        leaves the connection open. It has no timeout, and cancelling it
        closes the connection, so it needs a dedicated connection and a
        reconciliation from durable state after cancellation. A response
        is a wakeup hint, not a durable change receipt. A
        NOTIFICATIONOVERFLOW is kept in the result and means the server
        cancelled the NOTIFY registration, which needs reconciliation
        before it is registered again. *)
  end
end

(** {1 Connections} *)

module Client : sig
  (** Authenticated IMAP connections.

      A client belongs to the switch given to {!connect} or {!of_flow}, and
      releasing the switch closes it. Commands on a client are serialized
      across fibers. Every mailbox argument is a UTF-8 name that the client
      encodes for the wire in {!mailbox_mode}, and a name it cannot encode
      is [Error.State]. A command that expects one response of a kind is
      [Error.Protocol] when the reply holds none or several.

      Every command is bounded. A command line longer than 64 KiB is
      [Error.Limit] before it is sent. More than 10,000 untagged responses
      to one command, a response longer than 16 MiB, or more than 64 MiB of
      response data for one command is [Error.Limit] and closes the
      connection. A PREVIEW literal longer than 1,024 bytes is
      [Error.Limit] too. Bodies streamed to a sink count against the
      caller's [max_bytes] instead.

      The values at this level are IMAP4rev1 commands, or extensions that
      each call gates on its arguments. The submodules hold the operations
      that exist only because of an extension. *)

  type t
  (** The type for connections. *)

  type error = Error.t
  (** The type for client errors. *)

  val pp_error : Format.formatter -> error -> unit @@ portable
  (** [pp_error ppf e] prints a one-line description of [e] on [ppf]. It
      is portable, so it may run on any domain. *)

  val error_to_string : error -> string @@ portable
  (** [error_to_string e] is the text {!pp_error} prints for [e]. It is
      portable, so it may run on any domain. *)

  val connect :
    sw:Eio.Switch.t -> ?auth:Auth.t -> Transport.t -> (t, error) result
  (** [connect ~sw ~auth transport] connects to [transport], negotiates the
      TLS it requires and checks the server certificate, reads the
      greeting, authenticates with [auth], and is the client, which [sw]
      owns. [auth] is omitted by default, which succeeds only on a PREAUTH
      greeting and is [Error.State] otherwise. [`Required_starttls] is
      [Error.State] on a PREAUTH greeting and [Error.Unsupported Starttls]
      when the server does not offer STARTTLS.

      Once authenticated, [connect] enables IMAP4rev2 when the server
      advertises both IMAP4rev1 and IMAP4rev2, then UTF8=ACCEPT when it is
      offered and IMAP4rev2 is not in effect, then QRESYNC when it is
      offered, each only when ENABLE is available and ignoring a rejection.
      IMAP4rev2 and UTF8=ACCEPT make {!mailbox_mode} [Utf8], and QRESYNC
      makes the server report expunges as VANISHED instead of EXPUNGE.

      A network, name resolution or TLS failure is [Error.Transport]. No
      error leaves a connection open. *)

  val of_flow :
    sw:Eio.Switch.t -> ?auth:Auth.t ->
    [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t ->
    (t, error) result
  (** [of_flow ~sw ~auth flow] is {!connect} over the connected [flow],
      without STARTTLS. The client takes ownership of [flow] and closes it.
      [flow] is never treated as TLS, so a credential that needs TLS also
      needs [allow_insecure_transport]. [auth] is omitted by default, as in
      {!connect}. *)

  val capabilities : t -> Imap.Capability.Set.t
  (** [capabilities t] is the set the latest CAPABILITY response
      advertised. The client sends CAPABILITY after the greeting, after
      STARTTLS and after authentication. *)

  val enabled : t -> Imap.Capability.Set.t
  (** [enabled t] is every capability an ENABLED response confirmed on
      [t]. *)

  val has : t -> Imap.Capability.t -> bool
  (** [has t c] holds when [capabilities t] contains [c], or when [t] is in
      effective IMAP4rev2 and {!Imap.Capability.implied_by_rev2} [c] holds.
      Effective IMAP4rev2 means the server advertises IMAP4rev2 and either
      does not advertise IMAP4rev1 or confirmed ENABLE IMAP4rev2. Every
      extension gate of this library uses this predicate. *)

  val is_enabled : t -> Imap.Capability.t -> bool
  (** [is_enabled t c] is [Imap.Capability.Set.mem c (enabled t)]. *)

  val is_open : t -> bool
  (** [is_open t] holds while [t] is not closed. A later command can still
      close it, so a pool checks again at each checkout. *)

  val enable : t -> Imap.Capability.t list ->
    (Imap.Capability.t list, error) result
  (** [enable t caps] sends RFC 5161 ENABLE for those of [caps] not yet
      enabled and is the list the server's ENABLED responses confirmed. It
      sends nothing and is [Ok []] when [caps] is empty or already enabled.
      It is [Error.Unsupported Enable] unless [has t Enable] holds or the
      server advertises IMAP4rev2, and [Error.Unsupported c] for a [c] of
      [caps] for which [has t c] does not hold. A capability the server
      does not confirm is not an error. {!connect} already enables what it
      can of IMAP4rev2, UTF8=ACCEPT and QRESYNC. *)

  type mailbox_entry = {
    name : Imap.Mailbox_name.t;
        (** The row's mailbox name. [name.utf8] is the decoded form and
            [name.raw] the exact wire form. *)
    info : Imap.Response.list_result;  (** The LIST or LSUB row. *)
  }
  (** The type for listed mailboxes. The name is decoded in the connection's
      {!mailbox_mode} at the time of the response. *)

  val list : t -> ?reference:string -> pattern:string ->
    unit -> (mailbox_entry list, error) result
  (** [list t ~reference ~pattern ()] is every LIST row matching [pattern]
      under [reference]. [reference] defaults to [""]. [pattern] may hold
      the IMAP wildcards [*] and [%]. *)

  val lsub : t -> ?reference:string -> pattern:string ->
    unit -> (mailbox_entry list, error) result
  (** [lsub t ~reference ~pattern ()] is every LSUB row matching [pattern]
      under [reference], with the arguments of {!list}. LSUB rows may
      include unsubscribed hierarchy parents. *)

  val namespace : t -> (Imap.Response.namespace, error) result
  (** [namespace t] is the server's NAMESPACE response. It is
      [Error.Unsupported Namespace] unless {!has} holds for NAMESPACE.
      Prefixes stay exact wire names. *)

  type discovery = {
    mailboxes : (mailbox_entry * Imap.Response.mailbox_status option) list;
        (** Each LIST row with the STATUS row that directly followed it. *)
    unpaired_status : Imap.Response.mailbox_status list;
        (** STATUS rows that followed no LIST row for their mailbox, with
            their names as exact wire bytes. *)
  }
  (** The type for the result of {!list_extended}. *)

  val list_extended : t -> ?reference:string -> patterns:string list ->
    ?selection:Imap.Mailbox_list.selection list ->
    ?returns:Imap.Mailbox_list.return list ->
    ?status:Imap.Status_item.t list -> unit -> (discovery, error) result
  (** [list_extended t ~reference ~patterns ~selection ~returns ~status ()]
      is the rows of an RFC 5258 LIST matching [patterns] under
      [reference], with the selection options [selection] and the return
      options [returns]. [reference] defaults to [""], and [selection] and
      [returns] default to empty. [status] is omitted by default. When
      given it requests RFC 5819 LIST-STATUS for those items, gated as in
      {!status}.

      Any number of [patterns] other than one, and any option, need
      LIST-EXTENDED. A SPECIAL-USE option needs SPECIAL-USE, and [status]
      needs LIST-STATUS. Each missing one is [Error.Unsupported]. Past
      those gates an empty [patterns] is [Error.State]. A mailbox listed
      twice is [Error.Protocol]. A selectable row can lack STATUS even
      after tagged OK, so [None] means incomplete, never empty. *)

  val mailbox_mode : t -> Imap.Mailbox_name.mode
  (** [mailbox_mode t] is [Utf8] when IMAP4rev2 or UTF8=ACCEPT is in effect
      on [t], and [Rev1] otherwise. *)

  val status : t -> mailbox:string -> items:Imap.Status_item.t list ->
    (Imap.Response.mailbox_status, error) result
  (** [status t ~mailbox ~items] is the STATUS of [mailbox] for [items]. An
      empty [items] is [Error.State]. Each item is gated on its extension.
      [Highestmodseq] needs CONDSTORE or QRESYNC, [Mailboxid] OBJECTID,
      [Size] STATUS=SIZE or IMAP4rev2, [Deleted] a QUOTA capability or
      IMAP4rev2, and [Deleted_storage] a QUOTA capability, where a QUOTA
      capability is QUOTA or any QUOTA=RES-* token. A missing one is
      [Error.Unsupported] naming CONDSTORE, OBJECTID, STATUS=SIZE or QUOTA.
      The draft [Objectid] item is [Error.Not_enabled Objectid_plus] until
      {!Objectid_plus.enable} succeeds, and its identifiers are in
      [mailbox_status.objectid]. *)

  val get_jmap_access : t -> (string, error) result
  (** [get_jmap_access t] is the JMAP access data the server returns for
      GETJMAPACCESS, verbatim. It is [Error.Unsupported Jmapaccess] unless
      the server offers JMAPACCESS. A proxy applies its own endpoint trust
      policy before using it. *)

  type metadata_result = {
    responses : Imap.Response.metadata list;
    longentries : int64 option;
  }
  (** The type for the result of {!Metadata.get_metadata}. [longentries]
      reports RFC 5464 MAXSIZE truncation. When it is present the entries
      do not form the complete requested result. *)

  val create_mailbox : t -> mailbox:string -> (unit, error) result
  (** [create_mailbox t ~mailbox] creates [mailbox]. *)

  val delete_mailbox : t -> mailbox:string -> (unit, error) result
  (** [delete_mailbox t ~mailbox] deletes [mailbox]. *)

  val rename_mailbox : t -> old_name:string -> new_name:string ->
    (unit, error) result
  (** [rename_mailbox t ~old_name ~new_name] renames [old_name] to
      [new_name]. A caller keeping a durable mirror reconciles identity and
      cursor scope afterwards rather than assume UID continuity. *)

  val subscribe_mailbox : t -> mailbox:string -> (unit, error) result
  (** [subscribe_mailbox t ~mailbox] subscribes [mailbox]. *)

  val unsubscribe_mailbox : t -> mailbox:string -> (unit, error) result
  (** [unsubscribe_mailbox t ~mailbox] unsubscribes [mailbox]. *)

  val with_mailbox : t -> ?qresync:(Imap.Uidvalidity.t * Imap.Modseq.t) ->
    ?objectid:(string * string) ->
    mode:[ `Read_only | `Read_write ] -> string ->
    (Selected.t -> ('a, error) result) -> ('a, error) result
  (** [with_mailbox t ~qresync ~objectid ~mode mailbox callback] selects
      [mailbox], with SELECT for [`Read_write] and EXAMINE for
      [`Read_only], and is the result of [callback] applied to the lease.
      The lease is read-only when [mode] is [`Read_only] or the server
      reports READ-ONLY. The lease holds [t] exclusively, so a [Client]
      call from another fiber waits until it ends. A [Client] call on [t]
      from [callback] or a fiber it forked, including a nested
      [with_mailbox], is [Error.State "call inside with_mailbox on the same
      connection"] and sends nothing. A call on another connection is
      unaffected.

      [qresync] is omitted by default. When given it is a saved
      UIDVALIDITY and completed MODSEQ checkpoint for QRESYNC, and is
      [Error.Not_enabled Qresync] until ENABLE has confirmed QRESYNC.
      Without [qresync] the selection requests CONDSTORE when the server
      offers CONDSTORE or QRESYNC. [objectid] is omitted by default, and
      then a pin from {!Objectid_plus.pin_mailbox} applies. When given it
      is the draft [(account_id, mailbox_id)] identity of the intended
      mailbox, is [Error.State] when it differs from a pin, and is
      [Error.Not_enabled Objectid_plus] until {!Objectid_plus.enable}
      succeeds.

      Before [callback] runs, the client checks the selection. Invalid
      SELECT metadata closes the connection and is [Error.Protocol]. An
      OBJECTID+ identity other than the requested one, and a UIDNOTSTICKY
      mailbox, whose UIDs cannot be persisted, close the connection and
      are [Error.State].

      When [callback] returns, the lease expires. The client sends UNSELECT
      when {!has} holds for UNSELECT and closes the connection otherwise.
      If UNSELECT fails, the connection closes and the result of
      [callback] still stands. An exception from [callback], including
      cancellation, closes the connection and is re-raised. *)

  type append_receipt = {
    uidvalidity : Imap.Uidvalidity.t;
    uid : Imap.Uid.t;
  }
  (** The type for the RFC 4315 APPENDUID of one stored message. *)

  type append_message
  (** The type for messages to {!append} and {!Multiappend.append_many}. *)

  val append_message :
    ?flags:Mail_flag.Imap_flag.t list -> ?internal_date:Imap.Internal_date.t ->
    length:int64 -> _ Eio.Flow.source -> append_message
  (** [append_message ~flags ~internal_date ~length source] is a message of
      exactly [length] octets read from [source]. [flags] defaults to none
      and is sent in {!Mail_flag.Imap_flag.to_wire} spelling.
      [internal_date] is omitted by default, which lets the server choose
      the INTERNALDATE. [source] is borrowed and stays usable until the
      APPEND returns. It is not closed, and bytes after [length] stay
      unread. *)

  val append : t -> mailbox:string -> ?binary:bool -> append_message ->
    (append_receipt option, error) result
  (** [append t ~mailbox ~binary message] stores [message] in [mailbox]
      with one APPEND and is its APPENDUID receipt. [None] is a tagged OK
      without APPENDUID, whose destination identity is unknown, so the
      caller reconciles before deleting the source.

      [binary] defaults to [false]. When [true] the message is an RFC 3516
      literal8, which is [Error.Unsupported Binary] unless the server
      offers BINARY, even under IMAP4rev2. The server may then transform
      content-transfer encodings while preserving decoded content, so the
      receipt proves UID identity and not stored byte equality. The stored
      representation needs fetching and verifying before the input digest
      is published as an archived body. UNKNOWN-CTE is an
      [Error.Rejected].

      A mailbox with an OBJECTID+ pin is checked with STATUS before any
      byte is sent, and a different identity is [Error.State]. LITERAL-,
      LITERAL+ or effective IMAP4rev2 allows a non-synchronizing literal
      of up to 4,096 octets. A [source] that fails or ends early is
      [Error.State]. Once the final CRLF is sent, any failure other than a
      tagged rejection is [Error.Uncertain], and so is an APPENDUID that
      names several UIDs. An earlier failure keeps its own kind, since the
      server cannot have run the command, and closes the connection if
      bytes were sent. *)

  val close : t -> unit
  (** [close t] closes the transport of [t] at once, without LOGOUT.
      Closing a closed client has no effect. *)

  type multiappend_receipt = {
    uidvalidity : Imap.Uidvalidity.t;
    uids : Imap.Uid.t list;
  }
  (** The type for the APPENDUID of an RFC 3502 batch, with [uids] in
      message order. *)

  val noop : t -> (Imap.Response.t list, error) result
  (** [noop t] sends NOOP and is the untagged responses, in wire order.
      They report changes and establish no durable checkpoint. Inside
      {!with_mailbox}, {!Selected.noop} is the equivalent. *)

  val logout : t -> (unit, error) result
  (** [logout t] sends LOGOUT, waits for BYE and tagged completion, and
      closes the transport, also when the exchange fails or is cancelled. A
      completion without BYE is [Error.Protocol]. It has no timeout, so a
      caller that needs a deadline applies its own, for instance with
      [Eio.Time.with_timeout]. *)

  (** {2 Extensions} *)

  module Acl : sig
    (** RFC 4314 access control lists. [require] holds when the server
        offers ACL. Identifiers are sent verbatim as IMAP astrings, so the
        caller prepares identities. *)

    type client := t

    type t
    (** The type for ACL witnesses. *)

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
        replaces, as [operation] says, the [rights] of [identifier] on
        [mailbox]. Invalid [rights], and an empty [rights] with [`Add] or
        [`Remove], are [Error.State]. *)

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
    (** The type for QUOTA witnesses. *)

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
        returned QUOTA holds the server's rounded or actual values. *)
  end

  module Metadata : sig
    (** RFC 5464 METADATA. [require] holds when the server offers METADATA
        or METADATA-SERVER. *)

    type client := t

    type t
    (** The type for METADATA witnesses. *)

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported
        Metadata_server] when the server offers neither. *)

    val get_metadata : t -> mailbox:string -> entries:string list ->
      ?maxsize:int64 -> ?depth:Imap.Metadata.depth -> unit ->
      (metadata_result, error) result
    (** [get_metadata t ~mailbox ~entries ~maxsize ~depth ()] is the values
        of [entries] on [mailbox], where an empty [mailbox] names server
        metadata. [maxsize] and [depth] are omitted by default. A nonempty
        [mailbox] is [Error.Unsupported Metadata] when the server offers
        only METADATA-SERVER. *)

    val set_metadata : t -> mailbox:string ->
      values:(string * string option) list -> (unit, error) result
    (** [set_metadata t ~mailbox ~values] sets each entry of [values] on
        [mailbox], removing it for [None], with [mailbox] scoped as in
        {!get_metadata}. An empty [values], a repeated entry, and a value
        with a control character or invalid UTF-8 are [Error.State]. *)
  end

  module Notify : sig
    (** RFC 5465 NOTIFY outside a lease. [require] holds when the server
        offers NOTIFY. NOTIFY needs a dedicated connection and a
        reconciliation after any notification overflow. *)

    type client := t

    type t
    (** The type for NOTIFY witnesses on a connection. *)

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported Notify]. *)

    val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
      unit -> (Imap.Response.mailbox_status list, error) result
    (** [notify_set t ~status ~groups ()] registers [groups] as
        {!Selected.Notify.notify_set} does. [status] defaults to [false]. A
        selected filter in [groups] is [Error.State] here and needs
        {!Selected.Notify}. *)

    val notify_none : t -> (unit, error) result
    (** [notify_none t] cancels every registration on the connection. *)
  end

  module Multiappend : sig
    (** RFC 3502 MULTIAPPEND. [require] holds when the server offers
        MULTIAPPEND. *)

    type client := t

    type t
    (** The type for MULTIAPPEND witnesses. *)

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported
        Multiappend]. *)

    val append_many : t -> mailbox:string -> append_message list ->
      (multiappend_receipt option, error) result
    (** [append_many t ~mailbox messages] stores [messages] in [mailbox]
        with one atomic APPEND and is its APPENDUID receipt. [None] means
        success without UID evidence. It needs 1 to 1,000 [messages], each
        nonempty, and is [Error.State] otherwise. A count above an
        advertised MESSAGELIMIT or SAVELIMIT is [Error.Limit], and a
        malformed advertised limit is [Error.Protocol]. The command syntax
        of each message must stay within 65,000 bytes and that of the batch
        within 1 MiB, else [Error.State]. All of this is checked before
        dispatch. Message bytes are streamed, and the literal and
        uncertainty rules of {!append} apply. A rejection aborts the whole
        batch, and there is no sequential fallback. An APPENDUID that does
        not name one UID per message is [Error.Uncertain]. Binary literals
        are not supported. *)
  end

  module Compress : sig
    (** RFC 4978 COMPRESS=DEFLATE. [require] holds when the server offers
        COMPRESS=DEFLATE. *)

    type client := t

    type t
    (** The type for COMPRESS=DEFLATE witnesses. *)

    val require : client -> (t, error) result
    (** [require c] is a witness for [c], or [Error.Unsupported (Compress
        `Deflate)]. *)

    val activate : t -> (unit, error) result
    (** [activate t] sends COMPRESS DEFLATE and compresses the connection
        after the tagged OK. A NO or BAD is [Error.Rejected] and keeps the
        plaintext transport, and a second activation is [Error.State].
        Compression wraps the current transport, including TLS, and lasts
        for the connection. Cancellation, a malformed stream and lost
        framing close the connection. More than 16 MiB of compressed input
        without decoded output counts as a malformed stream. Decompressed
        data is subject to the ordinary response bounds. Compression is
        never active during credential exchange, and STARTTLS after
        compression is not supported. Compression side channels matter
        when secret and attacker-controlled data share the connection. *)
  end

  module Objectid_plus : sig
    (** The OBJECTID+ draft mode. [enable] holds once ENABLE has confirmed
        OBJECTID+. The mode is separate from the RFC 8474 OBJECTID
        capability and lasts for the connection. *)

    type client := t

    type t
    (** The type for OBJECTID+ witnesses. *)

    val enable : client -> (t, error) result
    (** [enable c] is {!Client.enable} [c [Objectid_plus]] as a witness,
        and [Error.Protocol] if the server does not confirm the mode.
        Enabling it changes the SELECT identity response codes to compound
        OBJECTID. *)

    val pin_mailbox : t -> mailbox:string -> account_id:string ->
      mailbox_id:string -> (unit, error) result
    (** [pin_mailbox t ~mailbox ~account_id ~mailbox_id] binds [mailbox] to
        the verified compound identity [(account_id, mailbox_id)] for the
        connection. Later {!with_mailbox} calls select by that identity and
        refuse a fallback to another mailbox, and {!append} checks the
        identity with STATUS before it sends message bytes. Binding
        [mailbox] again to a different identity is [Error.State]. *)

    val create_mailbox : t -> mailbox:string ->
      (Imap.Response.compound_object_id, error) result
    (** [create_mailbox t ~mailbox] is {!Client.create_mailbox} and is the
        tagged account and mailbox identity. A successful CREATE whose
        completion omits either identifier closes the connection and is
        [Error.Uncertain]. *)

    val rename_mailbox : t -> old_name:string -> new_name:string ->
      (Imap.Response.compound_object_id, error) result
    (** [rename_mailbox t ~old_name ~new_name] is {!Client.rename_mailbox}
        and is the tagged identity, under the conditions of
        {!create_mailbox}. *)

    val status : t -> mailbox:string -> items:Imap.Status_item.t list ->
      (Imap.Response.mailbox_status, error) result
    (** [status t ~mailbox ~items] is {!Client.status}, where the
        [Objectid] item is always allowed. *)
  end

  module Uidonly : sig
    (** The RFC 9586 UIDONLY mode. [enable] holds once ENABLE has confirmed
        UIDONLY. UIDFETCH and VANISHED replace sequence-number updates, and
        the mode cannot be disabled, so it needs a dedicated connection. *)

    type client := t

    type t
    (** The type for UIDONLY witnesses. A witness is the proof a caller
        passes to code that needs the mode. *)

    val enable : client -> (t, error) result
    (** [enable c] is {!Client.enable} [c [Uidonly]] as a witness, and
        [Error.Protocol] if the server does not confirm the mode. *)
  end
end

(** {1 Portable mailbox operations} *)

module Mailbox : sig
  (** Mailbox operations that choose their commands from the server's
      extensions.

      The layer serves mail user agents and proxies that must work across
      servers. Each operation reports the strategy it chose, so a caller
      can log or assert it. A durable syncer that needs exact commands
      uses {!Selected} directly. A [t] comes from {!of_selected} inside
      {!Client.with_mailbox}. It is the lease under another interface and
      expires with it. *)

  type t
  (** The type for strategy views of a lease. *)

  val of_selected : Selected.t -> t
  (** [of_selected s] is the strategy view of the lease [s]. *)

  type ('a, 's) outcome = { strategy : 's; result : ('a, Error.t) result }
  (** The type for the result of an operation and the strategy that
      produced it. [strategy] is reported on failure too. It names the
      strategy chosen, or for {!move} how far the strategy got. *)

  val search : t -> criteria:Imap.Search.t ->
    (Imap.Uid.t list, [ `Search ]) outcome
  (** [search t ~criteria] is {!Selected.uid_search}. When [criteria] needs
      an extension the server lacks, the result is [Error.Unsupported]
      naming the first such extension in {!Imap.Search.capabilities}
      order, and nothing is sent. *)

  val fetch : ?drop_unsupported:bool -> t -> uids:Imap.Uid.t list ->
    items:Imap.Fetch_item.t list ->
    (Selected.row list, [ `Fetch of int ]) outcome
  (** [fetch ~drop_unsupported t ~uids ~items] is {!Selected.fetch} over
      any number of UIDs. It removes repeated UIDs, sends one UID FETCH per
      1,000 distinct UIDs in request order and concatenates their rows,
      and [`Fetch n] reports the [n] round trips. An empty [uids] is
      [Ok []] with [`Fetch 0]. [drop_unsupported] defaults to [false], and
      then the first item of [items] whose extension the server lacks is
      the error {!Selected.fetch} would give, before anything is sent. When
      [true] such items are left out and their fields stay empty. A failed
      round trip ends the call, discards the rows of earlier ones and
      counts in [n]. *)

  val store : ?unchangedsince:int64 -> t -> set:Imap.Uid_set.t ->
    operation:[ `Add | `Remove | `Replace ] ->
    flags:Mail_flag.Imap_flag.t list ->
    (Selected.store_receipt, [ `Conditional | `Unconditional ]) outcome
  (** [store ~unchangedsince t ~set ~operation ~flags] is
      {!Selected.uid_store_flags} of [flags] on [set] as [operation] says,
      reported as [`Unconditional]. [unchangedsince] is omitted by default.
      When given the STORE is {!Selected.Condstore.uid_store_flags},
      reported as [`Conditional], and a server without CONDSTORE or QRESYNC
      is [Error.Unsupported Condstore]. A conditional STORE never falls
      back to an unconditional one. *)

  type move_strategy = [
    | `Move
    | `Copy_then_expunge
    | `Copy_then_flag
    | `Copied of Selected.copy_receipt option
    | `Copied_and_flagged of Selected.copy_receipt option ]
  (** The type for how {!move} ran. [`Move] is one UID MOVE.
      [`Copy_then_expunge] is UID COPY, a STORE adding [\Deleted] and a
      UIDPLUS UID EXPUNGE of the same set. [`Copy_then_flag] is UID COPY
      and a STORE adding [\Deleted], with no EXPUNGE. [`Copied r] and
      [`Copied_and_flagged r] come only with an error. Each means the copy
      succeeded with receipt [r], and [`Copied_and_flagged] that the STORE
      did too, before the next command failed. *)

  val move : t -> set:Imap.Uid_set.t -> mailbox:string ->
    (Selected.copy_receipt option, move_strategy) outcome
  (** [move t ~set ~mailbox] moves the messages of [set] to the UTF-8 name
      [mailbox] and is the COPYUID receipt, or [None] when the server sent
      none. It uses [`Move] when the server offers MOVE, else
      [`Copy_then_expunge] when it offers UIDPLUS, else [`Copy_then_flag].
      The fallbacks check that the mailbox is writable before copying.

      [`Copy_then_flag] leaves the source messages marked [\Deleted] and
      never sends a mailbox-wide EXPUNGE, which would also remove every
      other message already marked. The caller expunges deliberately. A
      failure of the first command is reported with the strategy chosen.
      A failure after the copy is reported as [`Copied] or
      [`Copied_and_flagged], so the caller can reconcile the destination
      and the source's [\Deleted] flags. *)

  type change =
    | Flags of Imap.Uid.t * Mail_flag.Imap_flag.t list
        (** The message's current flags. *)
    | Vanished of Imap.Uid_set.t  (** UIDs the server reported expunged. *)
    | New of Imap.Uid.t  (** A message at or above the caller's UIDNEXT. *)
  (** The type for mailbox changes. *)

  val changes_since : ?uidnext:Imap.Uid.t -> t -> Imap.Modseq.t option ->
    (change list, [ `Qresync | `Condstore | `Full ]) outcome
  (** [changes_since ~uidnext t since] is the changes after the MODSEQ
      [since].

      With [Some m] and QRESYNC enabled it is [`Qresync], one
      {!Selected.Qresync.fetch_changes} with VANISHED over every UID, in
      wire order. With [Some m] and CONDSTORE or QRESYNC offered but
      QRESYNC not enabled it is [`Condstore], a
      {!Selected.Condstore.fetch_changes_range} per window. It reports no
      [Vanished], so the absence of a message needs a separate check, for
      instance with {!search}. Otherwise it is [`Full], a
      {!Selected.fetch_range} of FLAGS per window, and every message is a
      [Flags] change. Windows span 1,000 UIDs from UID 1 to below the
      UIDNEXT that SELECT reported, so a message delivered during the
      lease waits for the next call.

      [uidnext] is omitted by default. When given it is the UIDNEXT the
      caller recorded with [since], and a change for a UID at or above it
      is [New uid] rather than [Flags]. A row without FLAGS is left
      out. *)

  val list_with_status : Client.t -> ?reference:string -> pattern:string ->
    Imap.Status_item.t list ->
    ((Client.mailbox_entry * Imap.Response.mailbox_status option) list,
     [ `List_status | `List_then_status ]) outcome
  (** [list_with_status c ~reference ~pattern items] is every LIST row
      matching [pattern] under [reference], each paired with the STATUS of
      [items] for its mailbox. [reference] defaults to [""]. It uses
      [`List_status], one {!Client.list_extended} with RFC 5819 STATUS,
      when {!Client.has} holds for LIST-EXTENDED and LIST-STATUS.
      Otherwise it uses [`List_then_status], one {!Client.list} and then
      one {!Client.status} per selectable row. [None] means no STATUS.
      Under [`List_then_status] that is a row that is not selectable,
      whose name does not decode, or whose STATUS the server answered with
      NO, and any other failure ends the call. An empty [items] is
      [Error.State].

      It takes the connection, not a lease. A call inside
      {!Client.with_mailbox} on the same connection is [Error.State] and
      sends nothing. *)

  val wait : t -> clock:_ Eio.Time.clock -> poll_seconds:float ->
    (Imap.Response.t list, [ `Idle | `Poll ]) outcome
  (** [wait t ~clock ~poll_seconds] blocks until the server reports a
      change and is the responses of the round that reported it, in wire
      order. It uses [`Idle], repeated {!Selected.Idle.wait_for_change},
      when the server offers IDLE, and otherwise [`Poll], a
      {!Selected.noop} after each sleep of [poll_seconds] on [clock]. A
      change is an EXISTS, EXPUNGE, FETCH or VANISHED response, or an
      untagged status response with a response code. A round without one,
      such as a bare [* OK] keepalive, is dropped and the wait continues,
      and a bare [* OK] is removed from the result.

      [wait] has no timeout, so the caller applies its own, for instance
      with [Eio.Time.with_timeout]. Cancelling [`Idle] closes the
      connection, as {!Selected.Idle.wait_for_change} documents, and needs
      a reconnection and reconciliation afterwards. Cancelling [`Poll]
      during a sleep leaves the connection usable. [wait] needs a
      dedicated connection. *)
end

(** {1 Connection pools} *)

module Pool : sig
  (** Bounded pools of authenticated IMAP connections.

      The pool's switch owns its connections. A closed or failed
      connection is replaced at the next checkout. A long IDLE wait belongs
      on a connection outside the pool when ordinary work must
      continue. *)

  type t
  (** The type for connection pools. *)

  val create :
    sw:Eio.Switch.t -> max_connections:int ->
    connect:(sw:Eio.Switch.t -> (Client.t, Error.t) result) -> t
  (** [create ~sw ~max_connections ~connect] is a pool of at most
      [max_connections] live clients, each opened on demand by [connect],
      which receives [sw]. Releasing [sw] closes the clients. A failed
      [connect] is returned by {!use} and does not consume capacity.

      @raise Invalid_argument if [max_connections] is less than 1. *)

  val max_connections : t -> int
  (** [max_connections t] is the capacity of [t]. *)

  val use : t -> (Client.t -> ('a, Error.t) result) -> ('a, Error.t) result
  (** [use t callback] is the result of [callback] applied to a client
      borrowed from [t]. It waits while every client is in use. A
      [Protocol], [Transport] or [Uncertain] error closes the borrowed
      client before it returns to the pool, and a tagged rejection leaves
      it reusable. An exception from [callback], including cancellation,
      closes the client and is re-raised. The client must not outlive
      [callback]. Once the switch of [t] is released, [use] is
      [Error.Closed], also for a caller already waiting for a client. *)
end
