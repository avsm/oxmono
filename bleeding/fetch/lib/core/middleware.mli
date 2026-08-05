(** API for use by middleware and backends. This module is not intended
    for use by applications.

    A middleware is the general form of every narrowing and appending
    combinator {!Fetch} offers. It may rewrite the request, call the
    inner handler any number of times, and transform the response. It
    must pass [Eio.Cancel.Cancelled] through, check {!body_replayable}
    before re-issuing a request, drain any response it discards, and
    report failures as [Eio.Io] with a {!Fetch.error}. This module also
    carries the provider interface {!Pi}, which a backend implements to
    become a client. *)

(** {1 Types} *)

type url = Url.t
(** A validated, canonical http(s) URL. Accessors are in {!Url}. *)

(** Documented at {!Fetch.type-error}. *)
type error =
  | Invalid_url of string
  | Invalid_request of string
  | Denied of string
  | Connection_failure of Eio.Net.connection_failure
  | Tls_failure of string
  | Protocol_error of string
  | Too_many_redirects
  | Body_not_replayable

type Eio.Exn.err += E of error
(** Errors are raised as [Eio.Io (E e, context)]. *)

val err : error -> exn
(** [err e] is the [Eio.Io] exception carrying [e], which is how a
    middleware reports a failure. *)

(** Documented at {!Fetch.type-body}. *)
type body =
  | Empty
  | String of string
  | Stream of { length : int64 option; flow : Eio.Flow.source_ty Eio.Resource.t }

type request = {
  meth : Http.Method.t;
  url : url;
  headers : Http.Header.t;
  body : body;
  sensitive : string list;
      (** Names (case-insensitive) of headers in [headers] that carry
          secrets, extending the built-in [Authorization], [Cookie] and
          [Proxy-Authorization]. A redirect hop that changes origin
          drops them, and {!pp_request} redacts their values. *)
}
(** [request] is used by backends and policy wrappers. *)

type version = [ Http.Version.t | `HTTP_2 ]
(** Documented at {!Fetch.type-version}. *)

type response
(** Documented at {!Fetch.type-response}. *)

type 'tag ty = [ `Fetch | `Platform of 'tag ]
(** Documented at {!Fetch.type-ty}. *)

type 'a t = 'a Eio.Resource.t constraint 'a = [> [> `Generic ] ty ]
(** Documented at {!Fetch.type-t}. *)

type plain = [ `Generic ] ty Eio.Resource.t
(** Documented at {!Fetch.type-plain}. *)

type handler = sw:Eio.Switch.t -> request -> response
(** One exchange. This is the general form of a backend or a wrapped
    client. *)

type middleware = handler -> handler
(** A transformation of one exchange function into another. *)

(** {1 Exchanges} *)

val request : sw:Eio.Switch.t -> _ t -> request -> response
(** [request ~sw t req] performs the single exchange [req], never
    following redirects. *)

val pp_request : request Fmt.t
(** [pp_request] prints the method, URL and headers with their values,
    redacting the value of a sensitive header, meaning the built-in
    credential trio and the request's own [sensitive] names (see
    {!type-request}). *)

val status : response -> int
(** [status r] is [r]'s HTTP status code. Documented at
    {!Fetch.val-status}. *)

val headers : response -> Http.Header.t
(** [headers r] is [r]'s header fields. Documented at
    {!Fetch.val-headers}. *)

val version : response -> version
(** [version r] is the HTTP version [r] arrived over. Documented at
    {!Fetch.val-version}. *)

val body : response -> Eio.Flow.source_ty Eio.Resource.t
(** [body r] is [r]'s body as a one-shot flow. Documented at
    {!Fetch.val-body}. *)

val url : response -> string
(** [url r] is the URL [r] was fetched from. Documented at
    {!Fetch.val-url}. *)

val trailers : response -> Http.Header.t option
(** [trailers r] is [r]'s trailer fields. Documented at
    {!Fetch.val-trailers}. *)

val pp_response : response Fmt.t
(** [pp_response] prints the status and URL. Documented at
    {!Fetch.val-pp_response}. *)

val handler : _ t -> handler
(** [handler t] is [t]'s exchange function, without the tracing and
    error context {!val-request} adds. *)

val of_handler : handler -> plain
(** [of_handler h] is a client dispatching to [h]. *)

val middleware : middleware -> _ t -> plain
(** [middleware m t] is [of_handler (m (handler t))]. *)

val map_request : (request -> request) -> _ t -> plain
(** [map_request fn t] applies [fn] to every request. The result still
    passes [t]'s own restrictions, so [fn] cannot amplify authority. *)

val map_response : (response -> response) -> _ t -> plain
(** [map_response fn t] applies [fn] to every response. *)

val body_length : body -> int64 option
(** [body_length b] is [b]'s size in bytes, if known. *)

val body_replayable : body -> bool
(** [body_replayable b] is [true] unless [b] is a [Stream]. *)

(** {1 Header names} *)

val is_token : string -> bool
(** [is_token s] is [true] if [s] is a non-empty RFC 9110 token, which
    is what a method and a header name must be. *)

val reserved_headers : string list
(** [reserved_headers] are the lowercase names the backend derives, so
    a request that sets one is rejected. *)

val sensitive_headers : string list
(** [sensitive_headers] are the lowercase names that always carry a
    credential. A cross-origin redirect hop strips them and
    {!pp_request} redacts their values. *)

(** {1 Scopes, URLs and backends} *)

(** A parsed scope entry, for middleware offering a [scope] argument
    of its own. *)
module Scope : sig
  type t

  val v : caller:string -> string -> t
  (** [v ~caller s] parses the scope entry [s] as {!Fetch.restrict}
      parses an entry of [under].
      @raise Invalid_argument, naming [caller], if [s] is not an http
        or https URL, or carries a query. *)

  val list : caller:string -> ?what:string -> string list -> t list
  (** [list ~caller entries] is {!v} applied to each of [entries].
      [what] (default ["scope"]) is what the failure message calls an
      entry. *)

  val matches : t -> url -> bool
  (** [matches s u] is [true] if [u] is under [s]. *)
end

module Url = Url
(** A validated, canonical http(s) URL: what a policy filter inspects
    and a backend serializes. *)

(** Provider interface *)
module Pi : sig
  module type CLIENT = sig
    type t
    type tag

    val request : t -> sw:Eio.Switch.t -> request -> response
    (** [request t ~sw req] performs one exchange. A backend must never
        follow redirects, and must serialize {!Url.to_string} (or
        {!Url.to_uri}) rather than re-parsing a URL string. *)
  end

  val client :
    (module CLIENT with type t = 't and type tag = 'tag) ->
    ('t, 'tag ty) Eio.Resource.handler
  (** [client (module X)] is the resource handler for backend [X],
      which with the backend's state makes an [Eio.Resource.t] that
      {!val-request} and the wrappers dispatch to.

      To offer backend-specific operations as well, keyed on the
      [`Platform] element of the row, extend this handler rather than
      replace it:

      {[
        let handler =
          Eio.Resource.handler
            (Eio.Resource.H (My_extras, (module X))
             :: Eio.Resource.bindings
                  (Fetch.Middleware.Pi.client (module X)))
      ]}

      Those operations are then reachable through [Eio.Resource.get]
      on the backend's own client, and unreachable once it has been
      wrapped, since a wrapper types as {!Fetch.plain}. *)

  val response :
    status:int ->
    headers:Http.Header.t ->
    version:version ->
    body:Eio.Flow.source_ty Eio.Resource.t ->
    ?trailers:(unit -> Http.Header.t option) ->
    url:url ->
    unit -> response
  (** [response ~status ~headers ~version ~body ~url ()] is a response
      for a backend to return. *)
end
