(** This module provides a portable HTTP client capability.

    A {!type-t} is the authority to make HTTP requests. Consumers can narrow it
    to selected origins, paths, or methods, and attach scoped credentials
    without gaining access to the underlying unrestricted client.

    {[
    let github ~token cap =
      let gh = [ "https://api.github.com" ] in
      let open Fetch in
      cap |> restrict ~under:gh
      |> with_headers ~mode:`If_absent
           Header.
             [
               (user_agent, "my-app/1.0");
               (accept, [ pref "application/vnd.github+json" ]);
             ]
      |> with_credentials ~scope:gh Credential.[ Bearer (fun () -> token) ]
    ]}

    HTTP error statuses, including 4xx and 5xx, are returned as responses. Only
    invalid input, transport, protocol, decoding, and policy failures raise an
    {!type-error}. Bound a request with [Eio.Time.with_timeout]; cancellation
    propagates through the client and aborts the exchange. *)

(** {1 Clients} *)

type 'tag ty = [ `Fetch | `Platform of 'tag ]
(** [ty] is the resource-interface row of a client. [`Platform] carries a
    backend tag that keys backend-specific operations, following the pattern
    used by [Eio.Net]. *)

type 'a t = 'a Eio.Resource.t constraint 'a = [> [> `Generic ] ty ]
(** ['a t] is a client with authority to make HTTP requests and resource
    interfaces ['a]. *)

type plain = [ `Generic ] ty Eio.Resource.t
(** [plain] is a client with no backend-specific interface. *)

type body = Middleware.body =
  | Empty  (** [Empty] is a request with no body. *)
  | String of string  (** [String data] is a replayable in-memory body. *)
  | Stream of {
      length : int64 option;
      flow : Eio.Flow.source_ty Eio.Resource.t;
    }
      (** [Stream body] is a one-shot body read from [body.flow]. It is sent
          with [Content-Length] when [body.length] is known and chunked
          otherwise. Use {!stream} to construct one. *)

type response = Middleware.response
(** [response] is a response head and one-shot body. See {!section-responses}.
*)

module Header = Header
(** [Header] is the module providing typed HTTP header values. *)

module Redirect = Redirect
(** [Redirect] configures redirect following and credential-scope extension. *)

(** {1 Making requests} *)

val get :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response
(** [get ~sw client url] is the response to a GET request for [url]. *)

val head :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response
(** [head ~sw client url] is the response to a HEAD request for [url]. Its body
    is empty. *)

val post :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  body:body ->
  _ t ->
  string ->
  response
(** [post ~sw client ~body url] is the response to submitting [body] to [url]
    with POST. *)

val put :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  body:body ->
  _ t ->
  string ->
  response
(** [put ~sw client ~body url] is the response to replacing [url] with [body]
    using PUT. *)

val delete :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response
(** [delete ~sw client url] is the response to a DELETE request for [url]. *)

val patch :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  body:body ->
  _ t ->
  string ->
  response
(** [patch ~sw client ~body url] is the response to applying the partial update
    [body] to [url] with PATCH. *)

val options :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response
(** [options ~sw client url] is the response to an OPTIONS request asking which
    methods [url] supports. *)

val read : ?limit:int -> _ t -> string -> string
(** [read client url] is the body of [GET url] as a string, regardless of the
    response status. [limit] defaults to 16 MiB and bounds the body size. It
    raises [Eio.Buf_read.Buffer_limit_exceeded] for a larger body. *)

val fetch :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?body:body ->
  ?redirects:int ->
  ?allow_downgrade:bool ->
  ?redirect:Redirect.config ->
  ?sensitive:string list ->
  _ t ->
  Http.Method.t ->
  string ->
  response
(** [fetch ~sw client meth url] is the response to requesting [url] with [meth]
    and following redirects under [redirect] (default {!Redirect.default}).
    [redirects] and [allow_downgrade] are shorthands that override the
    corresponding policy fields. [headers] defaults to none and [body] to
    [Empty]. Per
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-15.4}RFC 9110 §15.4}, 303
    (and 301 or 302 on POST) convert the method to GET and drop the body, while
    307 and 308 re-send it. Re-sending a [Stream] body raises
    [Body_not_replayable].

    The policy sees each redirect response with its body unread. [Stop] returns
    it; [Follow_within_scope] offers the target origin to
    [with_credentials ~extend:true]. An https-to-http hop raises [Denied]
    unless allowed by the policy. A hop that changes origin drops caller-set
    [Authorization], [Cookie],
    [Proxy-Authorization] and any header named in [sensitive], except on an
    http-to-https upgrade of the same host. The body itself is not dropped:
    a cross-origin 307 or 308 re-sends a replayable body to the new origin
    once those headers have been stripped, as browsers do. A body that must
    not reach an unexpected origin needs a redirect policy that stops the
    hop, not a header rule.

    It raises [Eio.Io] with [E (Invalid_request _)] before network access if
    [meth] or a header name is not a token, a header value contains a control
    character, [meth] is [CONNECT], which needs a tunnel this library does not
    open, or the request sets one of the headers the backend derives:
    [Host], [Content-Length] and [Transfer-Encoding], which frame the message,
    and [Connection], [Expect], [TE] and [Upgrade], which govern a connection
    the request does not own. *)

val with_response :
  ?headers:Header.headers ->
  ?body:body ->
  ?redirects:int ->
  ?allow_downgrade:bool ->
  ?redirect:Redirect.config ->
  ?sensitive:string list ->
  _ t ->
  Http.Method.t ->
  string ->
  (response -> 'a) ->
  'a
(** [with_response client meth url f] is [f response], where [response] is
    obtained as by {!fetch}. The response and its body remain valid only while
    [f] runs and are closed when [f] returns or raises. *)

val stream : ?length:int64 -> _ Eio.Flow.source -> body
(** [stream flow] is a one-shot request body read from [flow]. [length], when
    supplied, is the exact number of bytes the backend must send; without it,
    the size is unknown until the flow ends. A negative [length] raises
    [Invalid_argument]. *)

exception Idle_timeout of float
(** [Idle_timeout seconds] means a source wrapped by {!with_idle_timeout}
    produced no result from one read for [seconds]. *)

val with_idle_timeout :
  clock:_ Eio.Time.clock ->
  seconds:float ->
  _ Eio.Flow.source ->
  Eio.Flow.source_ty Eio.Resource.t
(** [with_idle_timeout ~clock ~seconds source] is [source] with an independent
    deadline around every read. A transfer may take longer than [seconds] in
    total while reads keep completing; a stalled read raises {!Idle_timeout}.
    The wrapper deliberately offers no optimized [copy], so a sink cannot
    bypass the per-read deadline.

    [seconds] must be finite and non-negative. The wrapper does not close
    [source], whose lifetime remains its owner's responsibility. *)

module Form = Form
(** [Form] is the module constructing request bodies for HTML form endpoints:
    {!Form.urlencoded} for a form without files and {!Form.multipart} for one
    with them. {!Form.urlencoded} is the encoder of {!Media.form}, so a request
    it builds and a response that codec decodes are the same encoding read in
    the two directions. *)

(** {1:typed Typed bodies}

    A {!Media.t} codec pairs a media type with an encoder and a decoder for
    one OCaml type. What the server says is a value: a status outside the
    2xx range is returned as the response for the caller to examine. What
    the server does wrong is an exception: a body whose media type is not
    the one the codec expects, or which the codec rejects, raises
    [Decode_failure] like any other protocol failure.

    {!Media.form} is the codec for [application/x-www-form-urlencoded], which
    an OAuth token endpoint answers in, so [read_as client Media.form url]
    reads one without a line of parsing. {!Form.urlencoded} builds a request
    in the same encoding. *)

module Media = Httpz.Media
(** [Media] is the module of typed media codecs, from {!Httpz.Media}.
    Codec values are portable and can be defined once and captured by
    portable closures.
    {!Json} and {!Markdown} provide the batteries-included JSON, JSON Lines,
    CommonMark, and HTML codecs. *)

module Json = Httpz.Json
(** [Json] is the bounded Jsont codec module from {!Httpz.Json}. The response
    byte limit of {!Fetch.decode} independently bounds the complete body. *)

module Markdown : sig
  (** This module provides CommonMark document codecs. *)

  val markdown :
    ?strict:bool -> ?max_bracket_depth:int -> unit -> Cmarkit.Doc.t Media.t
  (** [markdown ()] decodes [text/markdown] and [text/x-markdown], and
      encodes with [Cmarkit_commonmark].

      [strict] defaults to [false]. [max_bracket_depth] defaults to 16 and
      rejects excessive literal bracket nesting before parsing. Backslashes
      escape the next character; code spans are not interpreted by this
      lexical restriction. It is not a bound on parser work. Decoding untrusted
      Markdown requires Cmarkit's upstream nested-link parser correction;
      the development test wrapper selects the prepared local build.
      @raise Stdlib.Invalid_argument if [max_bracket_depth] is not positive. *)

  val html : ?safe:bool -> unit -> Cmarkit.Doc.t Media.t
  (** [html ()] encodes [text/html]. [safe] defaults to [true], dropping raw
      HTML and links whose schemes remain unsafe after percent-decoding and
      removing ASCII whitespace/control obfuscation. This conservative guard
      is not a substitute for a dedicated HTML sanitizer. *)
end

val encode : 'a Media.t -> 'a -> Header.headers * body
(** [encode codec v] is the Content-Type header and body of a request carrying
    [v] encoded by [codec], ready to pass to {!post} and its siblings. The
    body is a replayable [String]. *)

val decode : ?limit:int -> 'a Media.t -> response -> 'a
(** [decode codec r] is [r]'s body decoded by [codec]. The body is read as
    it arrives, bounded by [limit] (default 16 MiB). It raises [Eio.Io] with
    [E (Decode_failure _)] when the response Content-Type is not one [codec]
    accepts, when the decoder rejects the body, or with
    [Media.Too_large limit] when the body exceeds the bound. The status is not
    examined. *)

val get_as :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  'a Media.t ->
  string ->
  ('a, response) result
(** [get_as ~sw client codec url] is [Ok v] when a GET for [url] answers with
    a 2xx status whose body decodes to [v], and [Error r] with the response
    [r] for any other status, its body unread so that it may be decoded with
    another codec. An [Accept] header naming the codec's media type is sent
    unless [headers] carries one. Decoding failures raise as in {!decode}. *)

val read_as : ?limit:int -> _ t -> 'a Media.t -> string -> ('a, response) result
(** [read_as client codec url] is {!get_as} without a switch, as {!read} is to
    {!get}. On [Error r] the body has been read within [limit] into memory, so
    [r] remains usable after the call, including with {!decode}. *)

exception Rejected of response
(** [Rejected r] is raised by {!expect} for a response outside the 2xx
    range. *)

val expect : ('a, response) result -> 'a
(** [expect result] is the value of an [Ok], and raises {!Rejected} for an
    [Error]. It is for callers that treat any status outside the 2xx range as
    fatal. *)

val decode_seq : ?max_line:int -> 'a Media.seq -> response -> 'a Seq.t
(** [decode_seq codec r] is the sequence of values in [r]'s body, decoded one
    line at a time as the sequence is consumed. It must be consumed while
    [r]'s switch is live. A Content-Type the codec does not accept raises
    [Decode_failure] at once, and a line that exceeds [max_line] (default
    1 MiB), or does not decode, raises it when that element is reached. *)

module Sse : sig
  (** This module consumes Server-Sent Events and maintains reconnecting
      subscriptions. *)

  type event = {
    name : string;
    data : string;
    id : string option;
    retry : int option;
  }
  (** An [event] is one dispatched event block. [name] defaults to
      ["message"], [data] joins its data fields with newlines, and [id] and
      [retry] are the last valid fields in the block when present. *)

  val media_type : string
  (** [media_type] is ["text/event-stream"]. *)

  val decode : ?max_event:int -> response -> event Seq.t
  (** [decode response] is the events in [response]'s body, produced as they
      arrive. It must be consumed while the request switch remains live.
      [max_event] defaults to 1 MiB and bounds the complete block between
      dispatch boundaries. A wrong Content-Type or an oversized event raises
      [Decode_failure]. A partial final block is dropped.

      One leading UTF-8 byte-order mark is removed, as
      {{:https://html.spec.whatwg.org/multipage/server-sent-events.html}WHATWG
      "parsing an event stream"} requires; a second, and one anywhere later, is
      data. Nothing else is decoded: [name], [data] and [id] are the field bytes
      as they arrived, with no UTF-8 validation and no replacement of ill-formed
      sequences, so a caller that needs valid text validates them itself. *)

  val connect :
    sw:Eio.Switch.t ->
    ?headers:Header.headers ->
    ?last_event_id:string ->
    ?max_event:int ->
    _ t ->
    string ->
    (event Seq.t, response) result
  (** [connect ~sw client url] opens an event stream. It sends
      [Accept: text/event-stream] unless [headers] already carries Accept, and
      sends [Last-Event-ID] when supplied and representable as an HTTP field
      value. An ID containing another control byte remains visible to the
      decoder but is omitted from a request rather than making reconnection
      fail. A 2xx response is decoded; any other status is returned with its
      body unread. *)

  type subscription
  (** A [subscription] reconnects an event stream in a daemon fiber. *)

  val subscribe :
    sw:Eio.Switch.t ->
    clock:_ Eio.Time.Mono.t ->
    ?headers:Header.headers ->
    ?last_event_id:string ->
    ?max_event:int ->
    ?backoff_initial:float ->
    ?backoff_max:float ->
    ?capacity:int ->
    ?retryable:(exn -> bool) ->
    _ t ->
    string ->
    subscription
  (** [subscribe ~sw ~clock client url] reconnects until closed or until a
      fatal error. The last valid event ID is sent on every reconnection. A
      valid event-stream ID that contains a byte forbidden in an HTTP field is
      retained in {!last_event_id} but omitted from the reconnect request. A
      server retry field overrides exponential backoff, which otherwise starts
      at [backoff_initial] (default one second), doubles to [backoff_max]
      (default 60 seconds), and resets after a delivered event. A retry field
      is honoured only within [0.1] seconds and [backoff_max] and is clamped
      into that range, so neither a zero nor an astronomical value from the
      server governs the reconnection. [capacity] defaults to 64; a full event
      stream blocks its producer. Backoff must satisfy
      [0 < backoff_initial <= backoff_max], and capacity must be positive.

      [retryable] defaults to connection and protocol failures, exhausted
      redirect walks, and rejected 429 or 5xx responses. Decode, TLS, and
      policy failures are fatal. *)

  val events : subscription -> [ `Event of event | `End ] Eio.Stream.t
  (** [events subscription] is its bounded event stream. [`End] is its final
      item, added during shutdown only if the stream has room for it at that
      moment: the subscription never waits for a consumer to make room, so a
      consumer that stops draining a full stream sees no [`End] but does not
      hold the owning switch open either. Drain the stream, or observe
      {!result}, to see a subscription end in that case. *)

  val last_event_id : subscription -> string option
  (** [last_event_id subscription] is the most recent valid event ID, including
      one from a block that dispatched no event. *)

  val result : subscription -> (unit, exn) result Eio.Promise.t
  (** [result subscription] resolves on every termination path. *)

  val close : subscription -> unit
  (** [close subscription] stops reconnecting and ends the subscription. *)
end

val encode_seq : 'a Media.seq -> 'a Seq.t -> Header.headers * body
(** [encode_seq codec items] is the Content-Type header and body of a request
    carrying [items], encoded as each one is sent. The body is a one-shot
    [Stream], so it is neither retried nor re-sent on a redirect. *)

(** {1:responses Responses} *)

val status : response -> int
(** [status r] is [r]'s HTTP status code. *)

val headers : response -> Http.Header.t
(** [headers r] is [r]'s header fields. *)

val body : response -> Eio.Flow.source_ty Eio.Resource.t
(** [body r] is [r]'s body as a one-shot flow, closed when the request's switch
    finishes. *)

val url : response -> string
(** [url r] is the effective URL [r] was fetched from, after any redirects.
    Its fragment is retained even though fragments are not sent in HTTP
    requests. *)

val close : response -> unit
(** [close response] promptly releases its exchange without draining its body.
    It is idempotent and cancellation-protected. Do not read the body afterwards.
    The owning switch also releases the exchange when it ends. Middleware must
    close discarded responses before issuing a successor. *)

val scope : response -> string list
(** [scope r] is the credential scope in force when [r] was fetched, including
    origins approved by [Redirect.Follow_within_scope] when the credential
    wrapper opted into extension. Entries are URL prefixes as {!restrict}
    describes. *)

type version = [ Http.Version.t | `HTTP_2 ]
(** [version] is [Http.Version.t] extended with HTTP/2. *)

val version : response -> version
(** [version r] is the HTTP version [r] arrived over. *)

val trailers : response -> Http.Header.t option
(** [trailers r] is [r]'s trailer fields once its body has been fully read, if
    the backend surfaces them. *)

val header : 'a Header.t -> response -> 'a option
(** [header h r] is [h]'s value parsed from [r], or [None] if it is absent or
    malformed. *)

val pp_response : response Fmt.t
(** [pp_response ppf response] is a formatting of [response]'s status and URL.
*)

(** {1 Narrowing and appending requests} *)

val restrict :
  ?under:string list ->
  ?methods:Http.Method.t list ->
  ?filter:(Middleware.request -> [ `Allow | `Reject of string ]) ->
  _ t ->
  plain
(** [restrict client] is a client that allows only requests matching every axis
    given in the arguments. An absent axis is unrestricted, and stacking
    intersects. A rejected request raises [Eio.Io (E (Denied _), _)].

    [under] is a list of URL prefixes, the same language every [scope] below is
    written in. A request is under an entry when their origins agree, scheme,
    host and port alike, and the entry's path segments are a prefix of the
    request's. The match is by whole segment, so ["https://h/v3"] covers
    ["https://h/v3/x"] but not ["https://h/v3x"], and ["https://h"] and
    ["https://h/"] both mean the whole origin.

    An entry matches one origin. A rule over a set of them, any subdomain of a
    host say, goes in [filter], which sees the parsed URL through
    {!Middleware.Url}.

    Both axes compare the canonical host {!Middleware.Url} stores: a lowercase
    A-label without a trailing dot, and an IPv4 address as a dotted quad
    whatever spelling arrived, so a rule naming ["http://127.0.0.1"] also
    covers ["http://127.1"] and ["http://2130706433"]. It cannot cover the
    address a name resolves to: an allowed name may point at loopback, and an
    IPv4-mapped IPv6 literal such as [::ffff:127.0.0.1] is a distinct string
    from its IPv4 form. A backend's [~connect] is where a resolved address is
    checked.

    It raises [Invalid_argument] if an entry is not an HTTP or HTTPS URL, or
    carries a query. *)

val read_only : _ t -> plain
(** [read_only client] is a client that allows only GET, HEAD, and OPTIONS, the
    safe methods of
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-9.2.1}RFC 9110 §9.2.1}
    less TRACE, which reflects credentials. Any other method is denied with
    [Denied], by whichever entry point it arrives through. *)

val with_headers :
  ?scope:string list ->
  ?mode:[ `Set | `Add | `If_absent ] ->
  Header.headers ->
  _ t ->
  plain
(** [with_headers bs client] is a client that adds the headers [bs] to each
    request under [scope] (default all), whose entries are the URL prefixes
    {!restrict} describes. [mode] resolves a clash with a caller's header and
    [`Set] (default) replaces it, [`If_absent] keeps it, [`Add] appends.

    A secret under a name of your own, such as [X-Api-Key], belongs in
    {!with_credentials} instead, which scopes it, redacts it and drops it on a
    cross-origin hop.

    It raises [Invalid_argument] if [bs] binds [Authorization], [Cookie], or
    [Proxy-Authorization], or if a [scope] entry is not an HTTP or HTTPS URL, or
    carries a query. *)

module Credential = Credential
(** [Credential] is the module describing how scoped credentials travel. *)

val with_credentials :
  scope:string list ->
  ?allow_insecure:bool ->
  ?extend:bool ->
  Credential.t list ->
  _ t ->
  plain
(** [with_credentials ~scope cs client] is a client that attaches each
    credential of [cs], in order, to each request under [scope], replacing any
    caller value. The entries are the URL prefixes {!restrict} describes, so a
    scope of ["https://api.example.com/v3/"] leaves a request beside [/v3]
    untouched. Every redirect hop re-enters the wrapper, so a hop out of [scope]
    is never given a credential by default. With [extend] set to [true], a
    redirect policy's [Redirect.Follow_within_scope] decision adds the target
    origin for the remainder of that request chain.

    A [Header] credential is marked sensitive, so {!Middleware.pp_request}
    redacts it and a cross-origin hop drops a caller copy. [Authorization],
    which [Bearer] and [Basic] set, is sensitive already. A [Query] credential
    names its parameters in [request.sensitive_query], so
    {!Middleware.pp_request} and every wrapper that prints through
    {!Middleware.pp_url} replace their values; it stays out of traces, error
    context, and the response's {!val-url}.

    An in-scope [http://] request raises [Denied] unless [allow_insecure]
    (default [false]).

    It raises [Invalid_argument] if a [Header] name is not a token, is one the
    backend derives ([Host], [Content-Length], [Transfer-Encoding],
    [Connection], [Expect], [TE], [Upgrade]), or is
    [Cookie], which a jar manages (see [Fetch_cookies]); if a [Query] parameter
    name is empty; or if a [scope] entry is not an HTTP or HTTPS URL, or carries
    a query. *)

(** {1 Rate limits and retries} *)

val with_limits :
  clock:_ Eio.Time.Mono.t ->
  ?scope:string list ->
  ?min_interval:float ->
  ?max_concurrent:int ->
  _ t ->
  plain
(** [with_limits ~clock client] is a client that bounds the request rate per
    origin (scheme, host, port), for requests under [scope] (default all), whose
    entries are the URL prefixes {!restrict} describes. Two entries on one host
    share a budget, whatever their paths. [min_interval] is the minimum spacing
    in seconds between request starts, with concurrent fibers queueing at that
    rate. [max_concurrent] caps requests in flight, each slot held until the
    backend returns the response. It is a politeness bound on an origin, not a
    backend's connection pool size, which governs connection reuse and is set on
    the backend. It raises [Invalid_argument] if [max_concurrent] is below 1,
    [min_interval] is negative, infinite, or NaN, or a [scope] entry is not an
    HTTP or HTTPS URL or carries a query. *)

module Retry = Retry
(** [Retry] is the module configuring {!with_retry}. *)

val with_retry :
  clock:_ Eio.Time.Mono.t ->
  random:_ Eio.Flow.source ->
  ?wall:_ Eio.Time.clock ->
  ?config:Retry.config ->
  _ t ->
  plain
(** [with_retry ~clock ~random client] is a client that reissues requests that
    fail with a retryable status or a connection failure, per [config] (default
    {!Retry.default}), honouring [Retry-After] up to [config.backoff_max].
    Retries require a remaining retry budget, a replayable body, an allowed
    method, and approval by [config.retry_request] when configured. Under
    {!fetch} each redirect hop is retried on its own. The request predicate
    narrows all retry reasons, including connection failures and the additive
    retry hooks; returning [false] still permits the initial exchange.
    Discarded responses are closed before backoff and before the successor
    starts; their bodies are not drained.

    The request predicate runs once before the first attempt for the canonical
    request at this middleware boundary, and separately for each rewritten
    redirect hop. It is skipped when [config.max_retries] is zero, the body is
    streamed, or the method is disallowed. A veto skips both [retry_response]
    and [retry_exception]. A predicate exception propagates before the inner
    client is called. See {!Retry.config} for callback guidance.

    [Retry-After] has two forms. The delta-seconds form is honoured always. The
    HTTP-date form needs a wall clock to subtract from, so it is honoured only
    when [wall] is given (pass [env#clock]); without one the configured backoff
    applies as if the field were absent. A date already past waits no time at
    all, and a date far ahead is still capped by [config.backoff_max].

    [random] feeds the jitter. Pass [env#secure_random], or a deterministic flow
    to make backoff reproducible in a test. *)

(** {1 Errors} *)

type error = Middleware.error =
  | Invalid_url of string
      (** [Invalid_url reason] means the URL is not a valid HTTP or HTTPS URL
          for [reason]. *)
  | Invalid_request of string
      (** [Invalid_request reason] means the method, a header field, or the body
          was rejected for [reason] (see {!fetch}), or a backend refused the
          request. *)
  | Denied of string
      (** [Denied reason] means a policy wrapper rejected the request for
          [reason]. *)
  | Connection_failure of Eio.Net.connection_failure
      (** [Connection_failure reason] means the backend could not connect to the
          origin for [reason]. *)
  | Tls_failure of string
      (** [Tls_failure reason] means TLS setup or certificate validation failed
          for [reason]. *)
  | Protocol_error of string
      (** [Protocol_error reason] means the peer sent a malformed or oversized
          response described by [reason]. *)
  | Too_many_redirects
      (** [Too_many_redirects] means the request exhausted its redirect
          allowance. *)
  | Body_not_replayable
      (** [Body_not_replayable] means a redirect or retry needed to re-send a
          streaming body. *)
  | Decode_failure of { media : string; error : Media.error }
      (** [Decode_failure { media; error }] means a body could not be read as
          the media type [media] that a codec expected, for [error]. See
          {!section-typed}. *)

type Eio.Exn.err +=
  | E of error  (** Errors are raised as [Eio.Io (E e, context)]. *)

val err : error -> exn
(** [err e] is the [Eio.Io] exception carrying [e]. *)

(** {1 Extending} *)

module Middleware = Middleware
(** [Middleware] is the extension API for wrappers and backends. It exposes
    {!Middleware.type-request}, exchange functions, and {!Middleware.Pi}.
    Applications normally use the rest of this module instead. *)
