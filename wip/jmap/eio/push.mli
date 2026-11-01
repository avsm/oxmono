(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP push over the event source.

    A JMAP server that lets clients hold a connection open publishes a
    [text/event-stream] resource, the event source of
    {{:https://www.rfc-editor.org/rfc/rfc8620#section-7.3} RFC 8620 §7.3}. It is
    one long running HTTP GET over which the server appends a [state] event
    carrying a {!Jmap.Proto.Push.State_change.t} whenever data the client asked
    about changes, plus a [ping] event at an agreed interval so that a client
    can tell a live connection from a stalled one.

    A state change carries new state strings and never the data itself, so the
    loop is to listen and, on each change whose state differs from the one held,
    fetch the difference with the matching [/changes] method.

    {!listen} is one connection and {!subscribe} is a fiber that keeps
    reconnecting. Both are [Fetch.Sse] over the client's credentialed, origin
    scoped HTTP client. What this module adds to it is the expansion of the
    [eventSourceUrl] template, the decoding of the [state] and [ping] payloads,
    and the polling mode a server such as Cyrus needs. *)

(** {1 Events} *)

(** The type for the events of the stream. *)
type event =
  | State_change of Jmap.Proto.Push.State_change.t
      (** The [state] event of RFC 8620 §7.3, whose data is the StateChange
          object of §7.1. For each account the user can see it carries the new
          state string of each type that changed. *)
  | Ping of { interval : int64 }
      (** The [ping] event, whose [interval] is the number of seconds the server
          is using between pings. It may differ from the requested value. RFC
          8620 §7.3 lets a server clamp the interval, but not to a minimum above
          30 or a maximum below 300 seconds. A ping never advances the last
          event id. *)
  | Unknown of string * string
      (** [Unknown (name, data)] is an event this module does not model, which
          is an extension event or a [state] or [ping] event whose data does not
          decode. [name] is the event name, ["message"] when the server sent
          none, and [data] the concatenated payload lines. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event ppf e] prints a one line summary of [e]. Server-supplied names,
    states, and payloads are escaped, so a control byte cannot affect the
    terminal or break the line. *)

(** {1 Connecting} *)

val event_source_url :
  Client.t ->
  ?types:string list ->
  ?close_after:[ `State | `No ] ->
  ?ping:int ->
  unit ->
  string
(** [event_source_url client ~types ~close_after ~ping ()] is the session's
    [eventSourceUrl] template (RFC 8620 §7.3) resolved against the session URL
    and expanded as an RFC 6570 level 1 template.

    [types] names the types to be notified about, joined with commas. It
    defaults to ["*"], every type, which is also the meaning of [Some []].
    [close_after] is [`State] to have the server end the response after the
    first state event, which is what a client behind a buffering proxy needs,
    and defaults to [`No], which keeps the connection open. [ping] is the
    requested ping interval in seconds and defaults to [0], which asks for no
    ping events at all.

    The expansion percent encodes, so a two element [types] arrives as
    [Email%2CMailbox] and ["*"] as [%2A]. Both are the same values to a server
    that decodes its query string.

    @raise Invalid_argument
      if [ping] is outside the JMAP [UnsignedInt] range, 0 to 2{^ 53}-1, or the
      endpoint template cannot be expanded into an allowed URL. *)

val decode : Fetch.Sse.event -> event
(** [decode event] is the JMAP event [event] carries. The [state] event of RFC
    8620 §7.3 is a {!State_change}, [ping] is a {!Ping}, and anything else, data
    that fails to decode included, is {!Unknown}. It is what {!listen} and
    {!subscribe} apply to every event they read, and is exposed for a caller
    that drives [Fetch.Sse] itself to see the event ids the server sends. *)

val listen :
  Client.t ->
  ?types:string list ->
  ?close_after:[ `State | `No ] ->
  ?ping:int ->
  ?last_event_id:string ->
  (event -> [ `Continue | `Stop ]) ->
  (unit, Client.error) result
(** [listen client ~types ~close_after ~ping ~last_event_id f] opens the event
    source with {!event_source_url} and calls [f] on each event as it arrives.
    It is [Ok ()] when [f] answers [`Stop] or the server ends the response. The
    body is parsed incrementally, so [f] sees an event as soon as its
    terminating blank line does. Nothing waits for the stream to end, which for
    [~close_after:`No] never happens.

    [last_event_id] is sent as the [Last-Event-ID] request header and defaults
    to none. RFC 8620 §7.3 has the server send an event id encoding the whole
    visible state after each state event, so replaying the last id seen lets a
    reconnecting client be told about the changes it missed.

    Answering [`Stop] closes the response, and with it the connection, before
    [listen] returns. So does raising out of [f], and so does cancelling the
    fiber. An exception from [f] propagates unchanged; in particular, an
    [Eio.Io] or [Unix.Unix_error] raised by the callback is not misreported as a
    transport failure.

    The request carries the client's credentials, scoped as every other request
    is. A non-2xx status is {!Client.Http_error} or {!Client.Jmap_error} without
    any event being delivered, with the body bounded at one mebibyte rather than
    by the client's own limit. A transport failure is
    {!Client.constructor-Transport}, and so is an event larger than one
    mebibyte, carrying [Fetch.Decode_failure] with [Media.Too_large], which
    {!val-result} treats as fatal. A transport diagnostic identifies whether
    opening, reading or closing the event source failed. JMAP adds only the
    endpoint origin to the existing context. A failure while reading a refused
    response preserves its HTTP status and reports the body-read failure as its
    diagnostic.

    The framing is the [text/event-stream] parsing of the
    {{:https://html.spec.whatwg.org/multipage/server-sent-events.html} HTML
     server-sent events} specification, which RFC 8620 §7.3 defers to, and is
    [Fetch.Sse.decode]. Each event reaches [f] through {!val-decode}. A [ping]
    outside the JMAP [UnsignedInt] range is returned as
    [Client.Transport (Fetch.Invalid_request _, _)] before connecting. *)

(** {1 Subscriptions}

    {!listen} is one connection. It returns when the server ends the response
    and reports a dropped connection as an error. A client that wants to be told
    about changes for as long as it runs has to reconnect, remember the last
    event id so that the server can replay what it missed (RFC 8620 §7.3), and
    back off when the server is unreachable. {!subscribe} is that loop, running
    in a fiber of its own and handing events to the rest of the program over an
    [Eio.Stream.t]. *)

type item = [ `Event of event | `End ]
(** The type for the items of a subscription's stream. [`End], when present, is
    the final item. It marks a subscription closed with {!close} or one that
    gave up on an error that cannot be retried, and {!val-result} says which. A
    teardown may omit it when the stream is full; see {!close}. *)

type subscription
(** The type for reconnecting event source subscriptions. A value of this type
    is the fiber that runs the loop and the stream it writes to. *)

val subscribe :
  sw:Eio.Switch.t ->
  Client.t ->
  ?types:string list ->
  ?ping:int ->
  ?poll:float ->
  ?last_event_id:string ->
  ?backoff_initial:float ->
  ?backoff_max:float ->
  ?capacity:int ->
  unit ->
  subscription
(** [subscribe ~sw client ~types ~ping ~poll ~last_event_id ~backoff_initial
     ~backoff_max ~capacity ()] forks a fiber under [sw] that keeps an event
    source connection to the [eventSourceUrl] of [client] and adds every event
    it reads to the stream {!val-events} returns.

    The fiber is a daemon. It does not hold [sw] open and it is cancelled when
    [sw] finishes. Until then, each time a connection ends, because the server
    closed it, because [poll] expired or because it failed, the loop opens
    another quoting the last event id it saw as [Last-Event-ID] so that the
    server replays the changes made while it was away.

    [types] names the types to be notified about, as in {!event_source_url}, and
    defaults to every type. [ping] is the ping interval to ask for in seconds
    and defaults to [0], which asks for no pings. It must be in the JMAP
    [UnsignedInt] range. A ping is the only way to tell a live connection from a
    stalled one, so a held open subscription should ask for them.

    [poll] polls rather than holding the connection open. It bounds each
    connection attempt and response together to this many seconds and sets the
    default gap between connections to the same duration. It defaults to holding
    the connection open. Without it one connection is held open
    ([closeafter=no]) and events arrive on it as the server sends them, which is
    the shape RFC 8620 §7.3 describes. With it each connection asks for
    [closeafter=state]. At the deadline, a pending connection attempt is
    cancelled or an open response is closed, including when the event queue is
    full. Buffered events remain available. A full queue delays reconnection
    until the consumer makes room. Changes made between connections are replayed
    on the next connection. This mode allows mail delivery on Cyrus IMAP, where
    a held event source can stop delivery to the account.

    [last_event_id] is the id to quote on the first connection, from
    {!val-last_event_id} of an earlier subscription. It defaults to sending
    none, which asks the server for changes from now on. Resuming requires
    reconciliation from the caller's saved data state as described by
    {!val-last_event_id}.

    [backoff_initial] and [backoff_max] are the seconds to wait after a
    connection fails or ends with nothing read, doubling from the first up to
    the second and reset to the first as soon as a connection delivers an event.
    They default to [1.] and [60.]. Growth is deterministic, with no jitter, one
    client reconnecting to one server having nothing to spread out. With [poll]
    they are replaced by [poll]. A server [retry] field replaces the delay,
    bounded above by the effective maximum and below by [0.1] seconds, or the
    effective maximum if it is smaller.

    [capacity] is how many events the stream buffers and defaults to [64]. The
    subscription blocks rather than dropping an event, so a reading fiber that
    cannot keep up stalls the connection and eventually has the server drop it.
    A consumer that cannot keep up should hold a small capacity and let that
    happen rather than buffer without limit. The connection reads a few events
    beyond the ones the stream holds, so {!val-last_event_id} may already be
    ahead of what the reader has taken.

    @raise Invalid_argument
      if the transport of [client] carries no monotonic clock, if [poll] is not
      finite and positive or is too small or too large for the monotonic clock,
      if [backoff_initial] and [backoff_max] are not finite with
      [0. < backoff_initial <= backoff_max], if the effective backoffs are
      outside the nonzero range of [Duration.t], if [ping] is outside 0 to
      2{^ 53}-1, if [capacity] is below 1, or if the endpoint template cannot be
      expanded into an allowed URL. All are checked when the subscription is
      made rather than when the loop first needs them. *)

val next : subscription -> item
(** [next t] is the next buffered event of [t], waiting for an event or for the
    subscription to finish. Buffered events are returned before [`End]. Once the
    subscription has finished and its buffer is empty, every call returns
    [`End], including when teardown could not enqueue a sentinel. The reason for
    termination is available from {!val-result}. *)

val events : subscription -> item Eio.Stream.t
(** [events t] is the bounded stream the fiber of [t] writes to. {!next} reads
    it while observing termination. Direct [Eio.Stream.take] calls can block
    after termination because teardown may omit [`End] from a full stream.
    [Eio.Stream.take_nonblocking] may be used together with {!val-result} to
    poll it. *)

val last_event_id : subscription -> string option
(** [last_event_id t] is the subscription's current resume cursor, initialized
    from [last_event_id] of {!subscribe} and then replaced by each valid [id]
    field the connection reads. It is what the next connection quotes as
    [Last-Event-ID] (RFC 8620 §7.3). It can be ahead of events the consumer has
    taken or applied, so persisting it alone does not preserve unread changes. A
    restarting client must reconcile with [/changes] from its last applied data
    state before waiting for further notifications. An id-only event block may
    change the cursor without producing an item. A ping with no id does not
    change it. *)

val result : subscription -> (unit, Client.error) result Eio.Promise.t
(** [result t] is resolved when the fiber of [t] stops. It is [Ok ()] if [t] was
    stopped by {!close} or by its switch and [Error e] if it gave up. It is
    resolved on every path, the switch cancelling the fiber in the middle of a
    connection or while it waits for room in a full stream included, so a fiber
    outside that switch may await it.

    It retries a connection failure, a protocol error, an exhausted redirect
    walk, and a refusal with status 429 or 5xx. It gives up on anything else,
    since reconnecting cannot re-authenticate, re-resolve the event-source URL,
    or rewrite the request. This module reads a fatal refusal's body before
    stopping, so it arrives as {!Client.Http_error} or {!Client.Jmap_error}
    rather than as a status alone.

    [Denied] is the one worth naming among the fatal errors. It is what an
    unreadable credential file, an [http://] event source under
    [allow_insecure:false] and an [eventSourceUrl] outside the credential scope
    all produce, and none of them will be different on the next connection. An
    event larger than one mebibyte is fatal for the same reason: the next
    connection would be answered with the same one. A fatal transport diagnostic
    identifies the subscription operation and includes only the endpoint origin.
    A failure while reading a refused response preserves its HTTP status and
    reports the body-read failure as its diagnostic. *)

val close : subscription -> unit
(** [close t] stops [t]. The connection, if one is open, is dropped, no further
    one is made, and [`End] is added to the stream when it has room. It returns
    without waiting for the fiber, including when the stream is full, and
    calling it twice is harmless, a subscription being driven from one domain.

    A subscription is also stopped by its switch finishing, which cancels the
    fiber. {!val-result} is resolved either way. A teardown must not park
    waiting for room, so a stream that is full when [close] is called or its
    switch finishes may never receive the sentinel. {!next} still returns [`End]
    after draining its remaining events. A consumer that has stopped reading can
    observe {!val-result}. *)

val wait_for_state :
  subscription ->
  ?timeout:float ->
  ?since:string ->
  type_:string ->
  account_id:Jmap.Proto.Id.t ->
  unit ->
  string option
(** [wait_for_state t ~timeout ~since ~type_ ~account_id ()] takes events from
    [t] until one is a StateChange (RFC 8620 §7.1) naming [type_] for
    [account_id], and is the new state string it reports. Fetch the changes
    using the caller's previously applied state as [sinceState], and advance
    that saved state only after applying the returned changes. It is [None] if
    [t] ends first or has already ended, or if [timeout] seconds pass. [timeout]
    defaults to waiting for ever.

    [since] is a state the caller already holds. A StateChange reporting it for
    [type_] is skipped like any other event, so the wait ends on the next state
    rather than on the one already in hand. It defaults to no state being
    skipped. A server pushes the current state of every type it is asked about
    when a connection opens, so a caller that drains [/changes] and then waits
    again returns at once without it.

    Events taken on the way, StateChanges for other accounts or types included,
    are discarded. *)
