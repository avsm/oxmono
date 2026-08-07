(** Declarative HTTP serving, independent of any HTTP implementation.

    A handler describes a response, it does not write one. A backend consumes
    the description and owns the wire. The core here depends on the stdlib
    alone, so it holds no sockets and no buffers.

    {2 Quick start}

    {[
      open Proffer
      open Proffer.Route

      type env = { greet : string -> string }

      let site =
        Site.of_routes
          [
            get nil (fun _env _req -> Resp.text "index");
            get (s "hello" / str /? nil) (fun who env _req ->
                Resp.html (env.greet who));
            post (s "hello" /? nil) (fun _env req ->
                match Req.form_param req "who" with
                | Some who -> Resp.see_other ("/hello/" ^ who)
                | None -> Resp.bad_request ());
          ]

      let compiled = Compiled.compile site
    ]}

    A backend turns a parsed request into {!Req.t}, calls {!Serve.handle} and
    writes the {!Serve.outcome}. Dispatch, conditional GET and HEAD are decided
    there, once, so every backend and [proffer.mock] agree.

    {2 Modes}

    Route constructors take their handler at [portable]. A handler therefore
    cannot capture domain-bound state, and a {!Compiled.t} is portable by
    construction. State a handler needs reaches it through the ['env]
    argument, which the mode system does not constrain. Build that value as a
    record of closures, one per domain. *)

(** {1 Protocol vocabulary} *)

module Method : sig
  type t =
    [ `GET
    | `HEAD
    | `POST
    | `PUT
    | `DELETE
    | `PATCH
    | `OPTIONS
    | `Other of string ]

  val to_string : t -> string @@ portable
  (** [to_string m] is the method token as it appears on the request line. *)

  val of_string : string -> t @@ portable
  (** [of_string s] is the method [s] names. An unrecognised token becomes
      [`Other s], with no case folding: method names are case-sensitive. *)

  val equal : t -> t -> bool @@ portable
  (** [equal a b] compares the wire spelling, so [`Other "GET"] and [`GET] are
      the same method. *)
end

module Status : sig
  type t =
    [ `OK
    | `Created
    | `No_content
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Bad_request
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Conflict
    | `Length_required
    | `Payload_too_large
    | `Unsupported_media_type
    | `Internal_server_error
    | `Not_implemented ]

  val code : t -> int @@ portable
  (** [code s] is the three-digit status code. *)

  val reason : t -> string @@ portable
  (** [reason s] is the reason phrase for the status line. *)
end

module Headers : sig
  (** Header fields, keyed case-insensitively.

      Names are lowercased when a value is built, so a lookup is a plain string
      compare. Both {!Req} and {!Resp} speak this type. *)

  type t

  val empty : t @@ portable
  val of_list : (string * string) list -> t @@ portable

  val to_list : t -> (string * string) list @@ portable
  (** [to_list t] is the fields in the order they were given, with lowercase
      names. *)

  val find : t -> string -> string option @@ portable
  (** [find t name] is the first value under [name], matched
      case-insensitively. Repeated fields are not joined. *)

  val add : t -> string -> string -> t @@ portable
  (** [add t name value] appends a field. It does not replace an existing one
      of the same name. *)

  val mem : t -> string -> bool @@ portable
end

(** {1 Requests} *)

module Req : sig
  type t

  val v :
    meth:Method.t ->
    target:string ->
    ?headers:(string * string) list ->
    ?body:string ->
    unit ->
    t
    @@ portable
  (** [v ~meth ~target ()] is a request. [target] is the origin-form request
      target, for example ["/contact/avsm?tab=emails"]. Backends and tests
      construct requests with it. The path and query are decoded once, here. *)

  val meth : t -> Method.t @@ portable
  val target : t -> string @@ portable

  val path : t -> string @@ portable
  (** [path t] is the part of the target before any ['?'], still encoded. *)

  val segments : t -> string list @@ portable
  (** [segments t] is the path split on ['/'], with empty segments dropped and
      each remaining one percent-decoded. Routes match against it. *)

  val query : t -> (string * string) list @@ portable
  (** [query t] is the query string decoded, with ['+'] read as a space. A
      parameter given without a value has the empty string. *)

  val query_param : t -> string -> string option @@ portable
  (** [query_param t name] is the first value of [name] in {!query}. *)

  val headers : t -> Headers.t @@ portable
  val header : t -> string -> string option @@ portable
  (** [header t name] is the first field named [name], matched
      case-insensitively. *)

  val body : t -> string @@ portable
  (** [body t] is the request body, or [""] when there is none. *)

  val form : t -> (string * string) list @@ portable
  (** [form t] is the body decoded as [application/x-www-form-urlencoded]. It
      is [[]] when the body is empty or the Content-Type is another media
      type. *)

  val form_param : t -> string -> string option @@ portable
  (** [form_param t name] is the first value of [name] in {!form}. *)

  val forwarded_for : t -> string option @@ portable
  (** [forwarded_for t] is the first entry of X-Forwarded-For, which the
      nearest proxy sets to the client it saw. Whether that proxy is trusted is
      the deployment's business, not this library's. *)

  val forwarded_proto : t -> string option @@ portable
  (** [forwarded_proto t] is X-Forwarded-Proto, lowercased. *)
end

(** {1 Responses} *)

module Cache_control : sig
  (** Cache policy as data. The header value is built when the policy is
      described, not once per response. *)

  type span = [ `Secs of int | `Hours of int | `Days of int ]

  type t : immutable_data
  (** The kind is declared so a policy may be defined once at the top level and
      still be reachable from a portable handler. An abstract type without one
      reads as contended there. *)

  val no_store : t @@ portable
  (** [no_store] forbids any storage of the response. *)

  val private' : ?max_age:span -> unit -> t @@ portable
  (** [private' ()] allows storage by the client alone. *)

  val public :
    max_age:span ->
    ?s_maxage:int ->
    ?stale_while_revalidate:int ->
    ?must_revalidate:bool ->
    ?immutable:bool ->
    unit ->
    t
    @@ portable
  (** [public ~max_age ()] allows shared caches to store the response.
      [s_maxage] and [stale_while_revalidate] are in seconds. *)

  val to_string : t -> string @@ portable
  (** [to_string t] is the Cache-Control field value. *)
end

module Body : sig
  module Sink : sig
    (** The output path a backend lends to a streaming body. *)

    type t

    val write : t -> string -> unit @@ portable
    (** [write t s] emits [s]. It is valid only during the [Body.Stream] write
        callback and must not escape it. A backend builds the sink with
        {!Serve.sink}. *)
  end

  type t =
    | Empty
    | String of string
    | Delayed of { length : int64 option; gen : unit -> string }
        (** Generated on demand. [gen] is never run for a HEAD or a 304, so
            [length] is what those report when it is known. *)
    | Stream of { length : int64 option; write : Sink.t -> unit }
        (** Written incrementally. A backend sends it chunked when [length] is
            [None]. *)

  val string : string -> t @@ portable
end

module Etag : sig
  type t = [ `Strong of string | `Weak of string ]
  (** An entity-tag, held without its quotes. The value must not contain a
      double quote. *)

  val to_string : t -> string @@ portable
  (** [to_string t] is the ETag field value, quoted and prefixed with ["W/"]
      when weak. *)

  val weak_equal : t -> t -> bool @@ portable
  (** [weak_equal a b] is the weak comparison of RFC 9110 section 8.8.3.2: the
      opaque values match and the strength is ignored. It is the comparison a
      conditional GET uses. *)
end

module Resp : sig
  type t

  val v :
    ?status:Status.t ->
    ?headers:(string * string) list ->
    ?etag:Etag.t ->
    ?last_modified:float ->
    ?cache:Cache_control.t ->
    ?content_type:string ->
    Body.t ->
    t
    @@ portable
  (** [v body] is a response, [`OK] unless [status] says otherwise.

      [last_modified] is seconds since the epoch. Each of [content_type],
      [cache], [etag] and [last_modified] adds its header, so [headers] should
      not repeat them. [etag] and [last_modified] are also what {!Serve.handle}
      evaluates a conditional request against. *)

  val html :
    ?status:Status.t -> ?etag:Etag.t -> ?cache:Cache_control.t -> string -> t
    @@ portable
  (** [html s] is [s] as [text/html; charset=utf-8]. *)

  val text : ?status:Status.t -> string -> t @@ portable
  (** [text s] is [s] as [text/plain; charset=utf-8]. *)

  val media :
    ?status:Status.t ->
    ?etag:Etag.t ->
    ?cache:Cache_control.t ->
    string ->
    string ->
    t
    @@ portable
  (** [media ct s] is [s] with Content-Type [ct]. *)

  val see_other : string -> t @@ portable
  (** [see_other location] is the 303 that follows a successful form post. *)

  val redirect : ?permanent:bool -> string -> t @@ portable
  (** [redirect location] is a 302, or a 301 when [permanent] is [true]. *)

  val not_found : ?html:string -> unit -> t @@ portable
  val bad_request : ?html:string -> unit -> t @@ portable
  val status : t -> Status.t @@ portable

  val headers : t -> (string * string) list @@ portable
  (** [headers t] is the field block as it goes on the wire, including
      Content-Type, Cache-Control, ETag and Last-Modified. Content-Length is
      the backend's business, since only it knows what it will send. *)

  val body : t -> Body.t @@ portable
end

(** {1 Routes and sites} *)

module Route : sig
  (** Path patterns in a final encoding, so a capture arrives as a curried
      handler argument rather than a tuple element.

      {[
        get nil (fun env _req -> ...)
        (* env -> Req.t -> Resp.t *)

        get (s "contact" / str /? nil) (fun handle env req -> ...)
        (* string -> env -> Req.t -> Resp.t *)

        get (s "a" / str / s "b" / int' /? nil) (fun x n env req -> ...)
        (* string -> int -> env -> Req.t -> Resp.t *)

        get (s "static" /* rest) (fun segs env req -> ...)
        (* string list -> env -> Req.t -> Resp.t *)
      ]}

      A pattern is a chain of fragments joined by [( / )], closed with
      [( /? ) nil] or, to capture everything left, [( /* ) rest]. All three
      operators associate to the left, so the chain reads in path order.

      A GET route also answers HEAD. {!Serve.handle} suppresses the body. *)

  type 'env handler = 'env -> Req.t -> Resp.t

  type ('f, 'r) pat
  (** A complete pattern. ['f] is the handler type it demands and ['r] what is
      left once every capture has been applied, which a route constructor fixes
      to ['env handler]. *)

  type ('f, 'r) frag
  (** A pattern prefix, which [( / )] extends. Separate from {!pat} so that
      nothing can follow {!rest}. *)

  type 'env t
  (** One route: a method, a pattern and a handler. *)

  val nil : ('r, 'r) pat @@ portable
  (** [nil] matches the end of the path and captures nothing. *)

  val rest : (string list -> 'r, 'r) pat @@ portable
  (** [rest] captures every remaining segment, decoded. It is reached only
      through [( /* )]. *)

  val s : string -> ('r, 'r) frag @@ portable
  (** [s name] matches the literal segment [name]. *)

  val str : (string -> 'r, 'r) frag @@ portable
  (** [str] captures one segment as it is. *)

  val int' : (int -> 'r, 'r) frag @@ portable
  (** [int'] captures one segment that parses as an integer. *)

  val conv :
    name:string -> (string -> 'a option) @ portable -> ('a -> 'r, 'r) frag
    @@ portable
  (** [conv ~name parse] captures one segment for which [parse] returns a
      value. [name] describes the converter and appears nowhere on the wire. *)

  val ( / ) : ('f, 'g) frag -> ('g, 'r) frag -> ('f, 'r) frag @@ portable
  val ( /? ) : ('f, 'g) frag -> ('g, 'r) pat -> ('f, 'r) pat @@ portable
  val ( /* ) : ('f, 'g) frag -> ('g, 'r) pat -> ('f, 'r) pat @@ portable

  val get : ('f, 'env handler) pat -> 'f @ portable -> 'env t @@ portable
  (** [get pat handler] answers GET, and HEAD, at [pat]. [handler] is taken at
      [portable], so the compiler rejects one that captures domain-bound state
      here, where the fix belongs. *)

  val post : ('f, 'env handler) pat -> 'f @ portable -> 'env t @@ portable

  val route :
    Method.t -> ('f, 'env handler) pat -> 'f @ portable -> 'env t
    @@ portable
  (** [route meth pat handler] is the general form, for a method without its
      own constructor. *)
end

module Site : sig
  type 'env t

  val of_routes : 'env Route.t list -> 'env t @@ portable
  (** [of_routes routes] is a site matching [routes] in order. Its fallback is
      a plain 404 text response. *)

  val with_fallback : ('env -> Req.t -> Resp.t) @ portable -> 'env t -> 'env t
    @@ portable
  (** [with_fallback handler site] answers with [handler] when no route matches
      the path. A path that matches a route under another method gets 405
      instead, and never reaches the fallback. *)
end

module Compiled : sig
  type 'env t : value mod portable
  (** A site ready to serve. It holds data and portable handlers only, so the
      kind is declared: a compiled site defined once at the top level stays
      reachable from portable code. *)

  val compile : 'env Site.t -> 'env t @@ portable
end

(** {1 The backend interface} *)

module Serve : sig
  (** What every backend shares, so that conditional requests and HEAD are
      decided in one place and [proffer.mock] tests the code a socket backend
      runs. *)

  type outcome = {
    status : Status.t;
    headers : (string * string) list;
        (** Fully rendered, including validators, Cache-Control and
            Content-Type. Content-Length is the backend's job. *)
    body :
      [ `Empty
      | `String of string
      | `Stream of int64 option * (Body.Sink.t -> unit) ];
    content_length : int64 option;
        (** The length the response would have, kept accurate for HEAD and 304
            so a backend can send Content-Length without a body. [None] means
            unknown, which for a stream means chunked. *)
  }

  val handle :
    ?on_error:(exn -> unit) -> 'env Compiled.t -> 'env -> Req.t -> outcome
    @@ portable
  (** [handle compiled env req] dispatches [req] and applies the protocol
      mechanics that do not need a socket, in this order.

      - The method and the decoded segments select a route. HEAD matches a GET
        route. A path that matches only under other methods gives 405 with an
        Allow field. No route at all gives the site's fallback. An exception
        from a handler goes to [on_error] and gives a plain 500.
      - A successful GET or HEAD whose response carries an ETag is checked
        against If-None-Match, per RFC 9110: a comma-separated list, the [*]
        form, weak comparison. On a match the outcome is 304 carrying only the
        response's ETag, Last-Modified, Cache-Control and Vary fields, an empty
        body and no length.
      - Failing that, and only when If-None-Match is absent, a Last-Modified
        response is checked against If-Modified-Since. The dates are compared
        at whole-second resolution, which is all an IMF-fixdate can express. A
        date that does not parse leaves the request unconditional.
      - HEAD empties the body and keeps [content_length].
      - A {!Body.Delayed} generator runs once, at [handle] time, so the outcome
        of a sent body is always [`String]. It never runs for a HEAD or a
        304. *)

  val sink : (string -> unit) -> Body.Sink.t @@ portable
  (** [sink f] is how a backend wraps its writer for a [Body.Stream]. *)
end
