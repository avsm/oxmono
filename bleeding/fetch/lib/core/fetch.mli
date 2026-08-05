(** A portable HTTP client capability.

    A {!type-t} is the authority to make HTTP requests, which can
    be further restricted by consumers.

    {[
      let github ~token cap =
        let gh = [ "https://api.github.com" ] in
        let open Fetch in
        cap
        |> restrict ~under:gh
        |> with_headers ~mode:`If_absent
             Header.[ user_agent, "my-app/1.0";
                      accept, [ pref "application/vnd.github+json" ] ]
        |> with_credentials ~scope:gh Credential.[ Bearer (fun () -> token) ]
    ]}

    - 4xx and 5xx statuses are returned as data. Only transport,
      protocol and policy failures raise an {!error}.
    - Timeouts are Eio cancellation via [Eio.Time.with_timeout]
      rather than a request parameter.
*)

(** {1 Clients} *)

type 'tag ty = [ `Fetch | `Platform of 'tag ]
(** The row of a client. [`Platform] carries a backend's tag, keying
    interfaces beyond this one, as [Eio.Net] does. *)

type 'a t = 'a Eio.Resource.t constraint 'a = [> [> `Generic ] ty ]
(** A client has the authority to make HTTP requests via fetch. *)

type plain = [ `Generic ] ty Eio.Resource.t
(** A plain client with no backend tag. *)

type body = Middleware.body =
  | Empty
  | String of string
  | Stream of { length : int64 option; flow : Eio.Flow.source_ty Eio.Resource.t }
      (** Sent with [Content-Length] when [length] is known and chunked
          otherwise. Can be sent at most once. *)

type response = Middleware.response
(** A response. See {!section-responses}. *)

module Header = Header
(** Typed HTTP header values *)

(** {1 Making requests} *)

val get :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int ->
  _ t -> string -> response
(** [get ~sw t url] fetches [url]. *)

val head :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int ->
  _ t -> string -> response
(** [head ~sw t url] fetches [url]'s headers without its body. *)

val post :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int -> body:body ->
  _ t -> string -> response
(** [post ~sw t ~body url] submits [body] to [url]. *)

val put :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int -> body:body ->
  _ t -> string -> response
(** [put ~sw t ~body url] replaces [url] with [body]. *)

val delete :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int ->
  _ t -> string -> response
(** [delete ~sw t url] deletes [url]. *)

val patch :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int -> body:body ->
  _ t -> string -> response
(** [patch ~sw t ~body url] applies the partial update [body] to
    [url]. *)

val options :
  sw:Eio.Switch.t -> ?headers:Header.headers -> ?redirects:int ->
  _ t -> string -> response
(** [options ~sw t url] asks which methods [url] supports. *)

val read : ?limit:int -> _ t -> string -> string
(** [read t url] is the body of [GET url] as a string, whatever the
    status. [limit] (default 16 MiB) bounds the body size.
    @raise Eio.Buf_read.Buffer_limit_exceeded on a larger body. *)

val fetch :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?body:body ->
  ?redirects:int ->
  ?allow_downgrade:bool ->
  ?sensitive:string list ->
  _ t -> Http.Method.t -> string -> response
(** [fetch ~sw t meth url] requests [url] with [meth], following up to
    [redirects] hops. Per
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-15.4}RFC 9110
    §15.4}, 303 (and 301 or 302 on POST) convert the method to GET and
    drop the body, while 307 and 308 re-send it. Re-sending a [Stream]
    body raises [Body_not_replayable].

    An https-to-http hop raises [Denied] unless [allow_downgrade]. A hop
    that changes origin drops caller-set [Authorization], [Cookie],
    [Proxy-Authorization] and any header named in [sensitive], except on
    an http-to-https upgrade of the same host.

    @raise Eio.Io with [E (Invalid_request _)], before any network
      access, if [meth] or a header name is not a token, if a header
      value holds a control character, or if the request sets [Host],
      [Content-Length] or [Transfer-Encoding], which the backend sets
      itself. *)

val with_response :
  ?headers:Header.headers ->
  ?body:body ->
  ?redirects:int ->
  ?allow_downgrade:bool ->
  ?sensitive:string list ->
  _ t -> Http.Method.t -> string -> (response -> 'a) -> 'a
(** [with_response t meth url fn] is {!fetch} scoped to [fn]. The
    response is closed when [fn] returns. *)

val stream : ?length:int64 -> _ Eio.Flow.source -> body
(** [stream flow] is a body read from [flow]. *)

module Form = Form
(** Request bodies for HTML-form endpoints. *)

(** {1:responses Responses} *)

val status : response -> int
(** [status r] is [r]'s HTTP status code. *)

val headers : response -> Http.Header.t
(** [headers r] is [r]'s header fields. *)

val body : response -> Eio.Flow.source_ty Eio.Resource.t
(** [body r] is [r]'s body as a one-shot flow, closed when the request's
    switch finishes. *)

val url : response -> string
(** [url r] is the URL [r] was fetched from, after any redirects. *)

type version = [ Http.Version.t | `HTTP_2 ]
(** [Http.Version.t] extended with HTTP/2. *)

val version : response -> version
(** [version r] is the HTTP version [r] arrived over. *)

val trailers : response -> Http.Header.t option
(** [trailers r] is [r]'s trailer fields once its body has been fully
    read, if the backend surfaces them. *)

val header : 'a Header.t -> response -> 'a option
(** [header h r] is [h]'s value parsed from [r], or [None] if it is
    absent or malformed. *)

val pp_response : response Fmt.t
(** [pp_response] prints the status and URL. *)

(** {1 Narrowing and appending requests} *)

val restrict :
  ?under:string list ->
  ?methods:Http.Method.t list ->
  ?filter:(Middleware.request -> [ `Allow | `Reject of string ]) ->
  _ t -> plain
(** [restrict t] allows only requests matching every axis given in the
    arguments. An absent axis is unrestricted, and stacking intersects.
    A rejected request raises [Eio.Io (E (Denied _), _)].

    [under] is a list of URL prefixes, the same language every [scope]
    below is written in. A request is under an entry when their origins
    agree, scheme, host and port alike, and the entry's path segments
    are a prefix of the request's. The match is by whole segment, so
    ["https://h/v3"] covers ["https://h/v3/x"] but not ["https://h/v3x"],
    and ["https://h"] and ["https://h/"] both mean the whole origin.

    An entry matches one origin. A rule over a set of them, any
    subdomain of a host say, goes in [filter], which sees the parsed URL
    through {!Middleware.Url}.

    @raise Invalid_argument if an entry is not an http or https URL, or
      carries a query. *)

val read_only : _ t -> plain
(** [read_only t] allows only GET, HEAD and OPTIONS, the safe methods
    of {{:https://www.rfc-editor.org/rfc/rfc9110#section-9.2.1}RFC 9110
    §9.2.1} less TRACE, which reflects credentials. Any other method is
    denied with [Denied], by whichever entry point it arrives
    through. *)

val with_headers :
  ?scope:string list ->
  ?mode:[ `Set | `Add | `If_absent ] ->
  Header.headers ->
  _ t -> plain
(** [with_headers bs t] adds the headers [bs] to each request under
    [scope] (default all), whose entries are the URL prefixes
    {!restrict} describes. [mode] resolves a clash with a caller's
    header and [`Set] (default) replaces it, [`If_absent] keeps it,
    [`Add] appends.

    A secret under a name of your own, such as [X-Api-Key], belongs in
    {!with_credentials} instead, which scopes it, redacts it and drops
    it on a cross-origin hop.

    @raise Invalid_argument if [bs] binds [Authorization], [Cookie] or
      [Proxy-Authorization], or if a [scope] entry is not an http or
      https URL, or carries a query. *)

module Credential = Credential
(** How a credential travels. *)

val with_credentials :
  scope:string list ->
  ?allow_insecure:bool ->
  Credential.t list ->
  _ t -> plain
(** [with_credentials ~scope cs t] attaches each credential of [cs], in
    order, to each request under [scope], replacing any caller value.
    The entries are the URL prefixes {!restrict} describes, so a scope
    of ["https://api.example.com/v3/"] leaves a request beside [/v3]
    untouched. Every redirect hop re-enters the wrapper, so a hop out
    of [scope] is never given a credential.

    A [Header] credential is marked sensitive, so {!Middleware.pp_request}
    redacts it and a cross-origin hop drops a caller copy.
    [Authorization], which [Bearer] sets, is sensitive already. A
    [Query] credential stays out of the URL that traces and error
    context report, and appears only in the response's {!val-url}.

    An in-scope [http://] request raises [Denied] unless
    [allow_insecure] (default [false]).

    @raise Invalid_argument if a [Header] name is not a token, is one
      the backend derives ([Host], [Content-Length],
      [Transfer-Encoding]), or is [Cookie], which is a jar's to manage
      (see [Fetch_cookies]), if a [Query] parameter name is empty, or if
      a [scope] entry is not an http or https URL, or carries a
      query. *)

(** {1 Rate limits and retries} *)

val with_limits :
  clock:_ Eio.Time.Mono.t ->
  ?scope:string list ->
  ?min_interval:float ->
  ?max_concurrent:int ->
  _ t -> plain
(** [with_limits ~clock t] bounds the request rate per origin (scheme,
    host, port), for requests under [scope] (default all), whose entries
    are the URL prefixes {!restrict} describes. Two entries on one host
    share a budget, whatever their paths. [min_interval] is the minimum
    spacing in seconds between request starts, with concurrent fibers
    queueing at that rate. [max_concurrent] caps requests in flight,
    each slot held until the backend returns the response. It is a
    politeness bound on an origin, not a backend's connection pool
    size, which governs connection reuse and is set on the backend.
    @raise Invalid_argument if [max_concurrent] is below 1, if
      [min_interval] is negative, infinite or NaN, or if a [scope] entry
      is not an http or https URL, or carries a query. *)

module Retry = Retry
(** Configuration for {!with_retry}. *)

val with_retry :
  clock:_ Eio.Time.Mono.t ->
  random:_ Eio.Flow.source ->
  ?config:Retry.config ->
  _ t -> plain
(** [with_retry ~clock ~random t] re-issues requests that fail with a
    retryable status or a connection failure, per [config] (default
    {!Retry.default}), honouring [Retry-After] up to
    [config.backoff_max]. Only replayable bodies and allowed methods are
    retried, and under {!fetch} each redirect hop is retried on its
    own.

    [random] feeds the jitter. Pass [env#secure_random], or a
    deterministic flow to make backoff reproducible in a test. *)

(** {1 Errors} *)

type error = Middleware.error =
  | Invalid_url of string  (** The URL string did not validate. *)
  | Invalid_request of string
      (** The method or a header field was rejected (see {!fetch}), or a
          backend refused the request. *)
  | Denied of string  (** A policy wrapper rejected the request. *)
  | Connection_failure of Eio.Net.connection_failure
  | Tls_failure of string
  | Protocol_error of string  (** A malformed or oversized response. *)
  | Too_many_redirects
  | Body_not_replayable
      (** A redirect or retry needed to re-send a streaming body. *)

type Eio.Exn.err += E of error
(** Errors are raised as [Eio.Io (E e, context)]. *)

val err : error -> exn
(** [err e] is the [Eio.Io] exception carrying [e]. *)

(** {1 Extending} *)

module Middleware = Middleware
(** The extension API: the {!Middleware.type-request} record and the
    exchange functions a wrapper is written with, and {!Middleware.Pi},
    which a backend implements. Not for use by applications. *)
