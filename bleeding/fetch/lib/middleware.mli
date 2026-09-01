(** This module provides the extension API for middleware and backends.

    Applications normally use {!Fetch} instead. This module exposes the request
    and response representation needed to implement policy wrappers and
    transports.

    A middleware is the general form of every narrowing and appending combinator
    {!Fetch} offers. It may rewrite the request, call the inner handler any
    number of times, and transform the response. It must propagate
    [Eio.Cancel.Cancelled], check {!body_replayable} before reissuing a request,
    and {!close} any response it discards before issuing another exchange.
    Failures should be reported as [Eio.Io] with an {!type-error}. A transport
    implements {!Pi.CLIENT} and exposes it with {!Pi.client}. *)

(** {1 Types} *)

type url = Url.t
(** [url] is a validated, canonical HTTP or HTTPS URL. Accessors are in {!Url}.
*)

type error =
  | Invalid_url of string
      (** [Invalid_url reason] means the URL is not a valid HTTP or HTTPS URL
          for [reason]. *)
  | Invalid_request of string
      (** [Invalid_request reason] means the method, headers, or body cannot
          form a valid request for [reason]. *)
  | Denied of string
      (** [Denied reason] means a policy wrapper denied the request for
          [reason]. *)
  | Connection_failure of Eio.Net.connection_failure
      (** [Connection_failure reason] means the transport could not connect to
          the origin for [reason]. *)
  | Tls_failure of string
      (** [Tls_failure reason] means TLS setup or validation failed for
          [reason]. *)
  | Protocol_error of string
      (** [Protocol_error reason] means the peer sent a malformed or oversized
          response described by [reason]. *)
  | Too_many_redirects
      (** [Too_many_redirects] means the request exhausted its redirect
          allowance. *)
  | Body_not_replayable
      (** [Body_not_replayable] means a redirect or retry required a one-shot
          streaming body again. *)
  | Decode_failure of { media : string; error : Httpz.Media.error }
      (** [Decode_failure { media; error }] means a body could not be read as
          the media type [media] a codec expected, for [error]. *)

type Eio.Exn.err +=
  | E of error  (** Errors are raised as [Eio.Io (E e, context)]. *)

val err : error -> exn
(** [err e] is the [Eio.Io] exception carrying [e], which is how a middleware
    reports a failure. *)

type body =
  | Empty  (** [Empty] is a request with no body. *)
  | String of string  (** [String data] is a replayable in-memory body. *)
  | Stream of {
      length : int64 option;
          (** [length] is the exact byte count when it is known. *)
      flow : Eio.Flow.source_ty Eio.Resource.t;
          (** [flow] is the source supplying the one-shot body. *)
    }

type request = {
  meth : Http.Method.t;  (** [meth] is the request method. *)
  url : url;  (** [url] is the validated, canonical target URL. *)
  headers : Http.Header.t;
      (** [headers] is the request field block before backend-derived framing
          and [Host] fields are added. *)
  body : body;  (** [body] is the request body. *)
  sensitive : string list;
      (** [sensitive] is the list of case-insensitive header names that carry
          secrets, extending the built-in [Authorization], [Cookie], and
          [Proxy-Authorization] set. A redirect hop that changes origin drops
          them, and {!pp_request} redacts their values. *)
  sensitive_query : string list;
      (** [sensitive_query] is the list of case-sensitive query parameter names
          whose values carry secrets. {!pp_request} and the trace span print the
          URL with those values replaced by ["<redacted>"].
          {!Fetch.with_credentials} fills it in for a [Credential.Query]. *)
}
(** [request] is one validated exchange presented to a backend or policy
    wrapper. Its URL is already canonical, but {!val-request} still validates
    its method, body length, and headers before dispatch. A standard method
    spelling such as ["HEAD"] must use its standard constructor rather than
    [`Other], so every backend makes the same method-dependent framing
    decision. *)

type version = [ Http.Version.t | `HTTP_2 ]
(** [version] is an HTTP/1.0, HTTP/1.1, or HTTP/2 version. *)

type response
(** [response] is a response head and one-shot body returned by a backend. *)

type 'tag ty = [ `Fetch | `Platform of 'tag ]
(** [ty] is the resource interface row for clients. [`Platform] carries a
    backend tag for backend-specific operations. *)

type 'a t = 'a Eio.Resource.t constraint 'a = [> [> `Generic ] ty ]
(** ['a t] is an HTTP client capability with resource interfaces ['a]. *)

type plain = [ `Generic ] ty Eio.Resource.t
(** [plain] is a client with no backend-specific interface. *)

type handler = sw:Eio.Switch.t -> request -> response
(** [handler] is the function that performs one exchange. *)

type middleware = handler -> handler
(** [middleware] is a transformation of one exchange function into another. *)

(** {1 Exchanges} *)

val request : sw:Eio.Switch.t -> _ t -> request -> response
(** [request ~sw client req] is the response to the single exchange [req]. It
    never follows redirects. *)

val pp_url : request -> url Fmt.t
(** [pp_url request ppf url] prints [url] with the values of the query
    parameters named in [request.sensitive_query] replaced by ["<redacted>"], so
    a wrapper's own diagnostics need not republish a query credential. The
    result is for reading rather than for parsing back. *)

val pp_request : request Fmt.t
(** [pp_request ppf request] is a formatting of [request]'s method, URL, and
    headers. It redacts the built-in credential headers and names listed in
    [request.sensitive], and the values of the query parameters listed in
    [request.sensitive_query]. The URL it prints is therefore for reading
    rather than for parsing back. *)

val status : response -> int
(** [status response] is [response]'s HTTP status code. *)

val headers : response -> Http.Header.t
(** [headers response] is [response]'s header fields. *)

val version : response -> version
(** [version response] is the HTTP version over which [response] arrived. *)

val body : response -> Eio.Flow.source_ty Eio.Resource.t
(** [body response] is [response]'s body as a one-shot flow. *)

val close : response -> unit
(** [close response] releases its exchange without draining the body. It is
    idempotent, including across metadata copies, and cancellation-protected.
    The body must not be used afterwards. A backend must also release resources
    when its owning switch ends. *)

val sensitive : response -> string list
(** [sensitive response] lists lowercase credential header names accumulated
    through middleware for this exchange. It contains names only. Redirect
    middleware must carry these names into the next request's stripping policy. *)

val url : response -> string
(** [url response] is the effective URL [response] was fetched from, after any
    redirects. Its fragment is retained even though fragments are not sent in
    HTTP requests. *)

val scope : response -> string list
(** [scope response] is the credential scope reported by wrappers for this
    request, including origins added by its redirect walk. *)

val trailers : response -> Http.Header.t option
(** [trailers response] is [response]'s trailer fields after its body has been
    read, if the backend exposes them. *)

val pp_response : response Fmt.t
(** [pp_response ppf response] is a formatting of [response]'s status and URL.
    The URL is the one {!val-url} reports, fragment included. *)

val handler : _ t -> handler
(** [handler client] is [client]'s exchange function, without the tracing and
    error context {!val-request} adds. *)

val of_handler : handler -> plain
(** [of_handler handler] is a client that dispatches to [handler]. *)

val middleware : middleware -> _ t -> plain
(** [middleware wrapper client] is [client] transformed by [wrapper]. *)

val body_length : body -> int64 option
(** [body_length body] is [body]'s size in bytes, if known. *)

val body_replayable : body -> bool
(** [body_replayable body] is [true] unless [body] is a [Stream]. *)

(** {1 Header names} *)

val is_token : string -> bool
(** [is_token string] is [true] if [string] is a non-empty
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2}RFC 9110 token}, the
    required syntax for methods and header names. *)

val is_field_value : string -> bool
(** [is_field_value string] is [true] if [string] holds only bytes an
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.5}RFC 9110 field value}
    may carry, that is anything but a control byte other than tab. *)

val reserved_headers : string list
(** [reserved_headers] is the list of lowercase names the backend derives, so a
    request that sets one is rejected: the framing and authority fields
    [Host], [Content-Length] and [Transfer-Encoding], and the hop-by-hop fields
    [Connection], [Expect], [TE] and [Upgrade], which belong to a connection the
    request does not own.

    This is a fixed set, not the
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1}RFC 9110 section
     7.6.1} connection options an inbound Connection field can nominate at
    run time. An application forwarding a request through {!Fetch}, acting as
    an intermediary, must still compute and drop those nominated fields
    itself before forwarding the rest; passing the inbound Connection field
    straight through is rejected here rather than forwarded, since
    [Connection] is always in this list. *)

val sensitive_headers : string list
(** [sensitive_headers] is the list of lowercase names that always carry a
    credential. A cross-origin redirect hop strips them and {!pp_request}
    redacts their values. *)

(** {1 Scopes, URLs and backends} *)

(** [Scope.t] is a parsed URL-prefix scope for middleware that offers a [scope]
    argument. *)
module Scope : sig
  type t

  val v : caller:string -> string -> t
  (** [v ~caller string] is [string] parsed as an origin-and-path prefix. A
      request matches the scope when its origin is equal and its path starts
      with the prefix on a segment boundary. It raises [Invalid_argument],
      naming [caller], if [string] is not an HTTP or HTTPS URL, or carries a
      query or fragment. *)

  val list : caller:string -> ?what:string -> string list -> t list
  (** [list ~caller entries] is {!v} applied to each item in [entries]. [what]
      (default ["scope"]) is what the failure message calls an entry. *)

  val matches : t -> url -> bool
  (** [matches s u] is [true] if [u] is under [s]. *)

  val to_string : t -> string
  (** [to_string scope] is its canonical URL-prefix representation. *)
end

module Url = Url
(** [Url] is the module providing validated, canonical HTTP and HTTPS URLs for
    policy filters and backends. *)

(** [Pi] is the backend provider interface. *)
module Pi : sig
  module type CLIENT = sig
    type t
    (** [t] is the backend's internal client state. *)

    type tag
    (** [tag] is the identifier for the backend's resource interface. *)

    val request : t -> sw:Eio.Switch.t -> request -> response
    (** [request t ~sw req] is the response to one exchange. A backend must
        never follow redirects, and must serialize {!Url.to_string} (or
        {!Url.to_uri}) rather than reparsing a URL string. *)
  end

  val client :
    (module CLIENT with type t = 't and type tag = 'tag) ->
    ('t, 'tag ty) Eio.Resource.handler
  (** [client (module X)] is the resource handler for backend [X], which with
      the backend's state makes an [Eio.Resource.t] that {!val-request} and the
      wrappers dispatch to.

      To offer backend-specific operations as well, keyed on the [`Platform]
      element of the row, extend this handler rather than replace it:

      {[
      let handler =
        Eio.Resource.handler
          (Eio.Resource.H (My_extras, (module X))
          :: Eio.Resource.bindings (Fetch.Middleware.Pi.client (module X)))
      ]}

      Those operations are then reachable through [Eio.Resource.get] on the
      backend's own client, and unreachable once it has been wrapped, since a
      wrapper types as {!Fetch.plain}. *)

  val response :
    status:int ->
    headers:Http.Header.t ->
    version:version ->
    body:Eio.Flow.source_ty Eio.Resource.t ->
    close:(unit -> unit) ->
    ?trailers:(unit -> Http.Header.t option) ->
    ?scope:string list ->
    ?sensitive:string list ->
    url:url ->
    unit ->
    response
  (** [response ~status ~headers ~version ~body ~close ~url ()] is a response
      for a backend to return. Breaking change: [close] is required. It must
      promptly abort an unfinished exchange, release transport resources and
      remain safe after EOF. In-memory backends can supply [fun () -> ()].
      The provider guarantees that the callback runs at most once under
      cancellation protection. [scope] and [sensitive] default to [[]]. *)

  val with_metadata :
    ?url:url -> ?scope:string list -> ?sensitive:string list -> response -> response
  (** [with_metadata response] is [response] with wrapper-owned URL or
      credential-scope metadata replaced. [sensitive] adds case-insensitive
      names to the existing set; it never removes names. Its head, body and
      close operation retain shared ownership. *)
end
