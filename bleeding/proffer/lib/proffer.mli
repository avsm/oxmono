(** Declarative HTTP serving, independent of any HTTP implementation.

    A handler describes a response, it does not write one. A backend consumes
    the description and owns the wire. The core here depends on the stdlib
    alone, so it holds no sockets and no buffers.

    A handler is handed a {!Resp.respond} and calls it. Nothing that describes
    a response travels back up, so the description, its header block and the
    backend's outcome all live in the region {!Backend.handle} runs the handler
    in, at [local], and answering a request allocates on the heap only what the
    body itself is made of.

    {2 Quick start}

    {[
      open Proffer
      open Proffer.Route

      type env = { greet : string -> string }

      let site =
        Site.of_routes
          [
            get nil (fun _env _req respond -> Resp.text respond "index");
            get (s "hello" / str /? nil) (fun who env _req respond ->
                Resp.html respond (env.greet who));
            post (s "hello" /? nil) (fun _env req respond ->
                match Req.form_param req "who" with
                | Some who -> Resp.see_other respond ("/hello/" ^ who)
                | None -> Resp.bad_request respond ());
          ]

      let compiled = Compiled.compile site
    ]}

    A backend turns a parsed request into {!Req.t}, calls {!Backend.handle} and
    writes the {!Backend.outcome}. Dispatch, conditional GET and HEAD are
    decided there, once, so every backend and [proffer.mock] agree. A site never
    calls that module.

    {2 Modes}

    Route constructors take their handler at [portable]. A handler therefore
    cannot capture domain-bound state, and a {!Compiled.t} is portable by
    construction. State a handler needs reaches it through the ['env]
    argument, which the mode system does not constrain. Build that value as a
    record of closures, one per domain. *)

(** {1 Protocol vocabulary} *)

module Method : sig
  (** Request methods, which are httpz's.

      httpz's parser accepts the methods it names and rejects anything else,
      so a method that reaches a handler is always one of them. There is no
      [`Other] case and no wire spelling to compare: a method this set lacks
      is added to httpz rather than modelled around here. *)

  type t = Httpz.Method.t
  (** The methods httpz names. *)

  val to_string : t -> string @@ portable
  (** [to_string m] is the method token as it appears on the request line. *)

  val equal : t -> t -> bool @@ portable
  (** [equal a b] is whether [a] and [b] are the same method. *)
end

module Status : sig
  (** Response status codes, which are httpz's.

      A backend writes one straight onto the status line, so there is nothing
      to convert between describing a response and sending it. *)

  type t = Httpz.Res.status
  (** The statuses httpz names. *)

  val code : t -> int @@ portable
  (** [code s] is the three-digit status code. *)

  val reason : t -> string @@ portable
  (** [reason s] is the reason phrase for the status line. *)

  val of_code : int -> t option @@ portable
  (** [of_code n] is the status [n] names, if httpz names one. *)
end

module Headers : sig
  (** A field block.

      A field name is httpz's constructor. httpz already enumerates the fields
      its parser recognises, and a server layer that kept its own copy would
      spend every request and every response translating between two spellings
      of the same idea. Comparison is therefore constructor equality, a known
      name needs no validation because it is a token by construction, and a
      backend hands its parse over with nothing to convert.

      httpz names an unrecognised field {!Httpz.Header_name.Other} and keeps
      its spelling elsewhere, because a parsed name is a span into the read
      buffer. A described field has no buffer to point at, so {!field} carries
      the spelling directly. For a name httpz knows that is the canonical one,
      which costs nothing.

      A block travels the response path at [local], so its cells are stack
      allocated and a response costs no heap for them. [spelling] and [value]
      are [global_] because a string is boxed and a socket write needs it at
      global; [name] needs no modality, since it is read only to compare and
      to hand back to httpz's writer. Every operation below takes the block at
      [local], which a global block also satisfies. *)

  type name = Httpz.Header_name.t
  (** A field name. *)

  type field = {
    name : name;
    global_ spelling : string;
        (** The wire spelling: canonical for a name httpz knows, as given for
            an {!Httpz.Header_name.Other}. *)
    global_ value : string;
  }
  (** One field. *)

  type t = field list
  (** A block, in the order it goes on the wire. *)

  val h : name -> string -> field @@ portable
  (** [h name value] is the field [name: value], spelled canonically. It is
      what a handler passes in [~headers]. *)

  val other : string -> string -> field @@ portable
  (** [other spelling value] is a field httpz does not name, such as a site's
      own [X-] field, spelled as given. A spelling httpz does name resolves to
      its constructor, so a block holds the same field under one name however
      it was built. *)

  val h_local : name -> string -> field @ local @@ portable
  (** [h_local name value] is {!h} allocated in the caller's region.

      [stack_] on a list literal covers its cons cells and not the calls
      inside it, so [stack_ [ h n v ]] still puts every record on the heap.
      This is what a block on a path that answers every request should be
      built from. Everywhere else {!h} is simpler and the words do not
      matter. *)

  val other_local : string -> string -> field @ local @@ portable
  (** [other_local] is {!other} allocated in the caller's region. *)

  val empty : t @@ portable
  (** [empty] is the block with no fields. *)

  val of_string : string -> name @@ portable
  (** [of_string s] is the name [s] spells, matched case-insensitively, or
      {!Httpz.Header_name.Other} for one httpz does not name. *)

  val of_list : (string * string) list -> t @@ portable
  (** [of_list l] is [l] as a block, each name recognised where httpz names
      it. *)

  val to_list : t @ local -> (string * string) list @@ portable
  (** [to_list t] is the fields in order, each under its wire spelling. The
      result is a heap list, so this is a copy out of the block. *)

  val to_string : field -> string @@ portable
  (** [to_string f] is [f]'s wire spelling. *)

  val find : t @ local -> name -> string option @@ portable
  (** [find t name] is the first value under [name]. It is always [None] for
      {!Httpz.Header_name.Other}, which names no particular field; use
      {!find_other}. Repeated fields are not joined. *)

  val find_other : t @ local -> string -> string option @@ portable
  (** [find_other t spelling] is the first value under a field httpz does not
      name, matched case-insensitively. *)

  val mem : t @ local -> name -> bool @@ portable
  (** [mem t name] is whether [t] has a field named [name]. *)

  val same_name : name @ local -> name @ local -> bool @@ portable
  (** [same_name a b] is whether [a] and [b] are the same constructor. Every
      {!Httpz.Header_name.Other} is the same constructor, so this does not
      distinguish two differently spelled custom fields. *)

  val iter :
    (name -> string -> string -> unit) -> t @ local -> unit
    @@ portable
  (** [iter f t] applies [f] to each field's name, spelling and value, in
      order. It exists because [List.iter] takes a global list. *)

  val exists :
    (name -> string -> string -> bool) -> t @ local -> bool
    @@ portable
  (** [exists p t] is whether some field satisfies [p]. *)

  val cat : t @ local -> t @ local -> t @ local @@ portable
  (** [cat a b] is [a] then [b], allocated in the caller's region. A wrapper
      that adds a field to a block on its way past uses it, and pays no
      heap. *)

  val vary : t @ local -> string -> t @ local @@ portable
  (** [vary t name] is [t] with [name] added to its Vary field, rewriting that
      field rather than repeating it, and allocated in the caller's region. *)
end

module Mime : sig
  (** Content types by filename extension. *)

  val of_path : string -> string @@ portable
  (** [of_path name] is the Content-Type for [name], from its extension with
      case folded, or ["application/octet-stream"] when it is absent or
      unknown. *)
end

(** {1 Requests} *)

module Req : sig
  (** One request, decoded. A backend builds it from the wire and a test builds
      it directly. Every accessor reads a field that was decoded when the
      request was made, so none of them repeats work. *)

  type t
  (** A request. *)

  val v :
    meth:Method.t ->
    target:string ->
    ?path:string ->
    ?query:string ->
    ?headers:Headers.t ->
    ?body:string ->
    unit ->
    t @ local
    @@ portable
  (** [v ~meth ~target ()] is a request. [path] and [query] let a wire backend
      provide components already parsed from an absolute-form target while
      preserving [target] exactly as received; otherwise they are split from
      [target]. [headers] is a block rather than an association list, so a
      backend that already has one built from its own parse hands it over
      without a second copy. Use {!Headers.of_list} for a literal. *)

  val meth : t @ local -> Method.t @@ portable
  (** [meth t] is the request method. *)

  val target : t @ local -> string @@ portable
  (** [target t] is the request target as it arrived, undecoded. *)

  val path : t @ local -> string @@ portable
  (** [path t] is the part of the target before any ['?'], still encoded. *)

  val segments : t @ local -> string list @@ portable
  (** [segments t] is the path split on ['/'], with empty segments dropped and
      each remaining one percent-decoded. Routes match against it. *)

  val query : t @ local -> (string * string) list @@ portable
  (** [query t] is the query string decoded, with ['+'] read as a space. A
      parameter given without a value has the empty string. *)

  val query_param : t @ local -> string -> string option @@ portable
  (** [query_param t name] is the first value of [name] in {!query}. *)

  val headers : t @ local -> Headers.t @ local @@ portable
  (** [headers t] is the request's field block. *)

  val header : t @ local -> Headers.name -> string option @@ portable
  (** [header t name] is the first value under [name]. It is always [None] for
      {!Httpz.Header_name.Other}; use {!header_other}. *)

  val header_other : t @ local -> string -> string option @@ portable
  (** [header_other t spelling] is the first value under a field httpz does not
      name, matched case-insensitively. *)

  val body : t @ local -> string @@ portable
  (** [body t] is the request body, or [""] when there is none. *)

  val form : t @ local -> (string * string) list @@ portable
  (** [form t] is the body decoded as [application/x-www-form-urlencoded]. It
      is [[]] when the body is empty or the Content-Type is another media
      type. *)

  val form_param : t @ local -> string -> string option @@ portable
  (** [form_param t name] is the first value of [name] in {!form}. *)

  val forwarded_for : t @ local -> string option @@ portable
  (** [forwarded_for t] is the first entry of X-Forwarded-For, which the
      nearest proxy sets to the client it saw. Whether that proxy is trusted is
      the deployment's business, not this library's. *)

  val forwarded_proto : t @ local -> string option @@ portable
  (** [forwarded_proto t] is X-Forwarded-Proto, lowercased. *)
end

(** {1 Responses} *)

module Cache_control : sig
  (** Cache policy as data. The header value is built when the policy is
      described, not once per response. *)

  type span = [ `Secs of int | `Hours of int | `Days of int ]
  (** A freshness lifetime. Every form is written to the header in seconds. *)

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
  (** What a response carries, described rather than written. A backend decides
      the framing from the shape it is given. *)

  module Sink : sig
    (** The output path a backend lends to a streaming body. *)

    type t

    val write : t -> string -> unit @@ portable
    (** [write t s] emits [s]. It is valid only during the [Body.Stream] write
        callback and must not escape it. A backend builds the sink with
        {!Backend.sink}.

        The sink is a heap value, not a local one, and stays that way: a
        producer driving an encoder has to capture it in the closure the
        encoder writes through, and those take a global function. Lending one
        costs 3 words per streamed response. *)

    val write_sub : t -> bytes -> off:int -> len:int -> unit @@ portable
    (** [write_sub t b ~off ~len] emits that range of [b], which is not
        retained past the call.

        This is the way in for a producer that already holds bytes, which is
        every encoder that writes through a buffer: it hands over the
        encoder's own slice rather than making a string for each one. A
        backend that can only take strings pays a copy per slice here, so a
        producer that has a string should call {!write} instead. *)
  end

  type t =
    | Empty  (** No body, and a Content-Length of zero. *)
    | String of string @@ global  (** A body already in memory. *)
    | Delayed of { length : int64 option; gen : (unit -> string) @@ global }
        (** Generated on demand. [gen] is never run for HEAD or a status that
            cannot carry content, so [length] is what HEAD and 304 report when
            it is known. *)
    | Stream of { length : int64 option; write : (Sink.t -> unit) @@ global }
        (** Written incrementally. A backend sends it chunked when [length] is
            [None]. *)
  (** A response body. *)
end

module Etag : sig
  (** Entity-tags, the validator a conditional request is answered from. *)

  type t : immutable_data
  (** An entity-tag. The opaque value must not contain a double quote, a CR,
      an LF or a NUL, which {!Resp.v} enforces.

      The kind is declared because the type is abstract. A site that builds a
      tag once at the top level and serves it from a portable handler, which
      is what a static asset does, needs it to cross into that closure, and an
      abstract type carries no kind unless it says so.

      Abstract, because a tag carries its wire form alongside its opaque
      value and renders it once, when it is built. A memoised page builds its
      tag when it fills the cache and answers from it thereafter, so putting
      the quotes on costs nothing per request. *)

  val strong : string -> t @@ portable
  (** [strong s] is the strong entity-tag with opaque value [s]. *)

  val weak : string -> t @@ portable
  (** [weak s] is the weak entity-tag with opaque value [s]. *)

  val opaque : t -> string @@ portable
  (** [opaque t] is the value without quotes or a [W/] prefix. *)

  val is_weak : t -> bool @@ portable
  (** [is_weak t] is whether [t] was declared weak. *)

  val to_string : t -> string @@ portable
  (** [to_string t] is the ETag field value, quoted and prefixed with ["W/"]
      when weak. *)

  val weak_equal : t -> t -> bool @@ portable
  (** [weak_equal a b] is the weak comparison of RFC 9110 section 8.8.3.2: the
      opaque values match and the strength is ignored. It is the comparison a
      conditional GET uses. *)
end

module Resp : sig
  (** One response, described. A handler is handed a {!respond} and calls it,
      rather than returning a value.

      That is what keeps the response path off the heap. A returned record
      would have to outlive the frame that built it, or need [exclave_] at
      every frame between the constructor and the backend, and every
      combinator that transforms a response would need it too. Passing a
      responder down means nothing has to travel back up, so the description,
      its header block and the backend's outcome all live in the region
      {!Backend.handle} runs the handler in.

      A responder is always used at [local]. A backend builds one per request,
      and a handler that stashed one would hold a closure over a connection
      about to be reused. The mode is what stops that.

      Respond once, and respond last. A second call is dropped and reported to
      {!Backend.handle}'s [on_error], since the first response is already on
      the wire. A handler that returns without responding is reported the same
      way and answered 500. *)

  type description = {
    status : Status.t;
    headers : Headers.t;
    global_ etag : Etag.t option;
    global_ last_modified : float option;
    global_ cache : Cache_control.t option;
    global_ content_type : string or_null;
    body : Body.t;
  }
  (** What a handler describes. Only a backend and {!Backend.handle} read it:
      a handler builds one through the constructors below.

      It is passed at [local], so it costs no heap. [headers] is left at the
      record's own mode, because the block is the part worth keeping on the
      stack. Every other field holds a heap value that has to be readable at
      global to reach a socket, so each is [global_]. *)

  type respond = description @ local -> unit
  (** What a backend hands a handler.

      One record argument rather than a run of labelled ones: currying and
      locality do not mix, since a curried function used at [local] groups its
      arrows and an application then reads as complete after the first
      argument. *)

  val v :
    respond @ local ->
    ?status:Status.t ->
    headers:Headers.t @ local ->
    ?etag:Etag.t ->
    ?last_modified:float ->
    ?cache:Cache_control.t ->
    content_type:string or_null ->
    Body.t @ local ->
    unit
    @@ portable
  (** [v respond ~headers ~content_type body] responds with [body], [`OK]
      unless [status] says otherwise.

      The body is taken at [local], so a caller that writes
      [stack_ (Body.String s)] pays nothing for the block naming it. The
      string inside stays global, which is where a socket needs it. Every
      constructor below does this, so it matters only to a caller reaching
      for [v] directly.

      [headers] and [content_type] are required rather than optional, and that
      is what keeps both off the heap. An optional argument's payload arrives
      [local], so a local block cannot cross into it and a local string cannot
      reach the [global_] field a header value has to live in. The
      constructors below take [?headers] for convenience and put the block on
      the heap whenever one is given. [content_type] is [or_null] rather than
      an option because a value that cannot be null needs no box to say so, so
      naming a content type here costs nothing at all.

      Two things are needed to keep a block off the heap, and neither is
      visible at a call site that omits them. The block must be written
      [stack_], since [local] on a parameter permits stack allocation but does
      not cause it. And the call must not be in tail position, since a local
      argument cannot be passed in a tail call, so it is written
      [let () = ... in ()]. On a path that answers every request, that is
      worth doing:

      {[
        let () =
          Resp.v respond ~etag ~content_type
            ~headers:(stack_ [ Resp.h Httpz.Header_name.X_cache "hit" ])
            (Body.String page)
        in
        ()
      ]}

      Everywhere else the convenience is worth more than the words, and a
      constructor below with no [~headers] at all allocates nothing for the
      block either way, since the default is a constant.

      [etag], [cache] and [last_modified] are still optional, and each costs
      a couple of words when given, because an optional argument's payload
      arrives [local] and the [global_] field it lands in forces the [Some]
      onto the heap. They earn it back on a route that is revalidated, since
      {!Backend.handle} renders them only once it knows the response is being
      sent, so a 304 pays for no block at all. On a route that is never
      conditional, naming the field in the block instead is cheaper.

      [last_modified] is seconds since the epoch. Each of [content_type],
      [cache], [etag] and [last_modified] adds its header and owns that field,
      so [headers] may not name it as well. [etag] and [last_modified] are also
      what {!Backend.handle} evaluates a conditional request against, and the
      fields they own are rendered there, once it is known whether the
      response is being sent at all.

      Every other constructor in this module goes through [v], so the checks
      below cover them all.

      @raise Invalid_argument
        if a field would not survive the wire: a header name that is not a
        non-empty RFC 9110 token, a header value or [content_type] holding a
        CR, an LF or a NUL, an [etag] whose opaque value holds a double quote
        or one of those three, or a [last_modified] that is not a finite time
        between the years 0 and 9999.
      @raise Invalid_argument
        if [headers] names a field one of [content_type], [cache], [etag] or
        [last_modified] already sets. Emitting both would leave the copy a
        client reads first disagreeing with the one a conditional request is
        evaluated against.
      @raise Invalid_argument
        if [headers] names Content-Length, Transfer-Encoding or Connection,
        which the backend derives from the response body, request method and
        connection lifecycle.

      The message names the field. A handler runs under {!Backend.handle}'s
      guard, so either mistake becomes a 500 reported to [on_error] rather
      than a split, duplicated or truncated response. *)

  val h : Headers.name -> string -> Headers.field @@ portable

  val other : string -> string -> Headers.field @@ portable
  (** [other spelling value] is {!Headers.other}, re-exported. *)

  val h_local : Headers.name -> string -> Headers.field @ local @@ portable
  (** [h_local] is {!Headers.h_local}, re-exported: the constructor to build a
      block from on a path that answers every request. *)
  (** [h name value] is {!Headers.h}, re-exported so that a handler passing
      [~headers] needs only this module in scope. *)

  val html :
    respond @ local ->
    ?status:Status.t ->
    ?etag:Etag.t ->
    ?cache:Cache_control.t ->
    ?headers:Headers.t @ local ->
    string ->
    unit
    @@ portable
  (** [html respond s] responds with [s] as [text/html; charset=utf-8]. *)

  val text :
    respond @ local ->
    ?status:Status.t ->
    ?headers:Headers.t @ local ->
    string ->
    unit
    @@ portable
  (** [text respond s] responds with [s] as [text/plain; charset=utf-8]. *)

  val media :
    respond @ local ->
    ?status:Status.t ->
    ?etag:Etag.t ->
    ?cache:Cache_control.t ->
    ?headers:Headers.t @ local ->
    string ->
    string ->
    unit
    @@ portable
  (** [media respond ct s] responds with [s] under Content-Type [ct]. *)

  val stream :
    respond @ local ->
    ?status:Status.t ->
    ?cache:Cache_control.t ->
    ?headers:Headers.t @ local ->
    ?length:int64 ->
    string ->
    (Body.Sink.t -> unit) ->
    unit
    @@ portable
  (** [stream respond ct write] responds under Content-Type [ct] with whatever
      [write] emits onto the sink it is given.

      This is the constructor for a body that is produced rather than held: an
      encoder writing through a buffer hands each slice straight to the socket
      and the finished body never exists as a string. On a route answering a
      megabyte that is the difference between one copy and none.

      Omit [length] unless it is known before [write] runs, which for an
      encoder it is not. Without it the backend frames the body chunked, so
      the response carries no Content-Length. [write] is not run for HEAD or
      a status that cannot carry content; HEAD then reports no length either.

      [write] runs after the response head is on the wire, so a failure part
      way through it cannot become an error status. It is reported to
      {!Backend.handle}'s [on_error] and the body is truncated. Do not put a
      computation that can fail on this path. There is no [etag] argument for
      the same reason: a validator would have to be known before the bytes it
      validates. *)

  val empty :
    respond @ local ->
    ?status:Status.t ->
    ?headers:Headers.t @ local ->
    unit ->
    unit
    @@ portable
  (** [empty respond ()] responds with no body. *)

  val see_other : respond @ local -> string -> unit @@ portable
  (** [see_other respond location] is the 303 that follows a successful form
      post. *)

  val redirect :
    respond @ local -> ?permanent:bool -> string -> unit @@ portable
  (** [redirect respond location] is a 302, or a 301 when [permanent] is
      [true]. *)

  val not_found : respond @ local -> ?html:string -> unit -> unit @@ portable
  (** [not_found respond ()] is a 404 carrying [html], or a minimal page. *)

  val bad_request :
    respond @ local -> ?html:string -> unit -> unit @@ portable
  (** [bad_request respond ()] is a 400 carrying [html], or a minimal page. *)
end

(** {1 Routes and sites} *)

module Route : sig
  (** Path patterns in a final encoding, so a capture arrives as a curried
      handler argument rather than a tuple element.

      {[
        get nil (fun env _req respond -> ...)
        (* env -> Req.t -> Resp.respond @ local -> unit *)

        get (s "contact" / str /? nil) (fun handle env req respond -> ...)
        (* string -> env -> Req.t -> Resp.respond @ local -> unit *)

        get (s "a" / str / s "b" / int /? nil) (fun x n env req respond -> ...)
        (* string -> int -> env -> ... *)

        get (s "static" /* rest) (fun segs env req respond -> ...)
        (* string list -> env -> ... *)
      ]}

      A pattern is a chain of fragments joined by [( / )], closed with
      [( /? ) nil] or, to capture everything left, [( /* ) rest]. All three
      operators associate to the left, so the chain reads in path order.

      A GET route also answers HEAD. {!Backend.handle} suppresses the body. *)

  type 'env handler =
    'env -> Req.t @ local -> Resp.respond @ local -> unit
  (** What a route runs. The ['env] argument carries whatever state the handler
      needs, since a portable closure cannot capture it. The responder is taken
      at [local]: it is built per request in the region {!Backend.handle} runs
      the handler in, and a handler that stashed one would hold a closure over
      a connection about to be reused. *)

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
      through [( /* )].

      The segments are percent-decoded, so one of them may be [".."] or may
      itself contain a ['/'] that arrived as [%2F]. A handler that turns them
      into a filesystem path must therefore reject those cases itself, or it
      serves any file the process can read. Match on the segments, do not
      concatenate them and open the result. *)

  val s : string -> ('r, 'r) frag @@ portable
  (** [s name] matches the literal segment [name]. *)

  val str : (string -> 'r, 'r) frag @@ portable
  (** [str] captures one segment as it is. *)

  val int : (int -> 'r, 'r) frag @@ portable
  (** [int] captures one segment that parses as an integer. *)

  val conv :
    name:string -> (string -> 'a option) @ portable -> ('a -> 'r, 'r) frag
    @@ portable
  (** [conv ~name parse] captures one segment for which [parse] returns a
      value. [name] describes the converter and appears nowhere on the wire. *)

  val ( / ) : ('f, 'g) frag -> ('g, 'r) frag -> ('f, 'r) frag @@ portable
  (** [p / q] matches [p] then [q]. *)

  val ( /? ) : ('f, 'g) frag -> ('g, 'r) pat -> ('f, 'r) pat @@ portable
  (** [p /? nil] closes [p], matching a path with nothing after it. *)

  val ( /* ) : ('f, 'g) frag -> ('g, 'r) pat -> ('f, 'r) pat @@ portable
  (** [p /* rest] closes [p], capturing every segment after it. *)

  val get : ('f, 'env handler) pat -> 'f @ portable -> 'env t @@ portable
  (** [get pat handler] answers GET, and HEAD, at [pat]. [handler] is taken at
      [portable], so the compiler rejects one that captures domain-bound state
      here, where the fix belongs. *)

  val post : ('f, 'env handler) pat -> 'f @ portable -> 'env t @@ portable
  (** [post pat handler] answers POST at [pat]. *)

  val route :
    Method.t -> ('f, 'env handler) pat -> 'f @ portable -> 'env t
    @@ portable
  (** [route meth pat handler] is the general form, for a method without its
      own constructor. *)

  val moved : ('env handler, 'env handler) pat -> string -> 'env t @@ portable
  (** [moved pat location] answers GET, and HEAD, at [pat] with a 301 to
      [location]. The pattern captures nothing, so [location] is fixed. A
      location built from a capture needs a {!get} returning
      {!Resp.redirect}. *)

  val found : ('env handler, 'env handler) pat -> string -> 'env t @@ portable
  (** [found pat location] is {!moved} with a 302 instead. *)
end

module Site : sig
  (** A set of routes and what to do when none of them matches. *)

  type 'env t
  (** A site, before it is compiled. *)

  val of_routes : 'env Route.t list -> 'env t @@ portable
  (** [of_routes routes] is a site matching [routes] in order. Its fallback is
      a plain 404 text response. *)

  val with_fallback : 'env Route.handler @ portable -> 'env t -> 'env t
    @@ portable
  (** [with_fallback handler site] answers with [handler] when no route matches
      the path. A path that matches a route under another method gets 405
      instead, and never reaches the fallback. *)

  val with_headers : (string * string) list -> 'env t -> 'env t @@ portable
  (** [with_headers extra site] adds [extra] to every response [site] gives,
      whether a route wrote it or the library did. It is how a site sets
      security headers once.

      The fields are appended to the block on its way past the wrapper, so a
      name a handler already set is the copy a client reads first, and the
      joined block is built in the responder's region rather than the heap.
      The names and values are checked once here, as {!Resp.v} checks them,
      which is what stops a decorator injecting a response split.

      @raise Invalid_argument if a name or a value is unwritable. *)

  val with_auth :
    scope:string list list ->
    realm:string ->
    check:(string option -> bool) @ portable ->
    'env t ->
    'env t
    @@ portable
  (** [with_auth ~scope ~realm ~check site] gates every path under a prefix in
      [scope] behind [check], which is given the Authorization field of the
      request, or [None] when there is none. A failed check answers 401 with
      [WWW-Authenticate: Basic realm=...] naming [realm]. A path under no
      prefix in [scope] is served unchanged, and the empty prefix [[]] gates
      the whole site.

      The gate is what answers under [scope], so a request that would have got
      a 404 or a 405 there gets the 401 instead. A caller without credentials
      therefore cannot tell which paths under [scope] name a route.

      @raise Invalid_argument
        if [realm] holds a double quote or a backslash, which a quoted-string
        cannot carry unescaped, or if [scope] is empty. An empty [scope] gates
        no path at all, which would serve the site open behind what reads as
        a gate. Pass [[[]]] to gate the whole site. *)

  val mount : at:string list -> 'env t -> 'env t -> 'env t @@ portable
  (** [mount ~at sub site] adds the routes of [sub] to [site] under the path
      prefix [at]. A request whose path starts with [at] and whose remainder
      matches a route of [sub] is answered by that route. Only the routes of
      [sub] are taken, so its fallback stays behind and [site]'s answers a
      path [sub] does not match.

      @raise Invalid_argument
        if [sub] has been through {!with_auth} or {!with_headers}. Those wrap
        a site rather than its routes, and mounting takes the routes alone, so
        a mounted gate would be dropped and the sub-site served open. Wrap the
        result of [mount] instead. *)
end

module Negotiate : sig
  (** Choosing a response variant from the request's Accept header. *)

  type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]
  (** A media type this library can negotiate. [`Other] carries a full type
      such as ["image/png"]. *)

  val of_accept : string option -> media list @@ portable
  (** [of_accept accept] is the media types [accept] asks for, most preferred
      first, with q-values honoured and a missing q taken as 1. It is [[]] when
      [accept] is absent or empty. A type this library does not name becomes
      [`Other]. *)

  val v :
    (media * 'env Route.handler) list @ portable ->
    'env Route.handler @ portable
    @@ portable
  (** [v variants] is a handler that answers with the variant the client most
      prefers, which is the first media type its Accept header ranks that
      [variants] offers. The client's order decides, not the order [variants]
      are listed in. The first entry of [variants] is the fallback, taken when
      the client accepts none of them or sends no Accept header. The chosen
      response gains [Vary: Accept], since it depends on that header. An empty
      [variants] leaves nothing to answer with and gives a 404. [variants] is
      taken at [portable] because the handler it yields captures it, and a
      route stores that handler in a portable closure. *)
end

module Compiled : sig
  (** A site prepared for dispatch, which is what a backend serves. *)

  type 'env t : value mod portable
  (** A site ready to serve. It holds data and portable handlers only, so the
      kind is declared: a compiled site defined once at the top level stays
      reachable from portable code. *)

  val compile : 'env Site.t -> 'env t @@ portable
  (** [compile site] prepares [site] for dispatch. Compile once, at startup,
      and serve the result from every domain. *)
end

module Static : sig
  (** Serving a directory of files, described as data. *)

  val confine : string list -> string option @@ portable
  (** [confine segs] is [segs] joined with ['/'] when every segment names
      something directly under a root, and [None] otherwise. A segment that is
      empty, ["."] or [".."], or that holds a ['/'] or a NUL, is refused, so
      the result can never leave the subtree. A backend that resolves the
      result against a filesystem must still open it under a confining root,
      since [confine] cannot see symlinks. *)

  type t : immutable_data
  (** A served directory. It holds a label and a cache policy, not a filesystem
      handle, so a backend resolves [root] against its own capability. *)

  val v : root:string -> ?cache:Cache_control.t -> unit -> t @@ portable
  (** [v ~root ()] serves files under [root], a name the backend resolves. Each
      file's Content-Type comes from {!Mime.of_path} and its response carries
      [cache] when given. *)

  val root : t -> string @@ portable
  (** [root t] is the label [t] was built with. *)

  val cache : t -> Cache_control.t option @@ portable
  (** [cache t] is the policy [t] applies to each file, if any. *)
end

(** {1 Caching} *)

module Cache : sig
  (** A memoization cache keyed by string, holding a rendered body and its
      entity-tag. It crosses domains, so a policy built once at startup is
      reachable from every domain's handlers. *)

  type t : value mod portable contended
  (** A cache. The kind is declared so a cache created once at startup stays
      reachable from a portable handler. An abstract type without one reads as
      contended there, and a cache that names only [portable] is unusable from
      the handlers it exists to serve. *)

  val create : ttl:float -> t @@ portable
  (** [create ~ttl] is an empty cache whose entries live [ttl] seconds. *)

  val memoize :
    t -> now:float -> key:string -> (unit -> string) -> string * Etag.t
    @@ portable
  (** [memoize t ~now ~key gen] is the body under [key] and an entity-tag over
      it. It runs [gen] and stores the result when [key] is absent or its entry
      is older than the cache's [ttl] at [now], and returns the stored body
      otherwise. [now] is seconds since the epoch, passed in so the core reads
      no clock. [gen] runs on the calling domain and is not stored, so it may
      capture domain-bound state. Two domains racing on a miss both run [gen]
      and one result wins, which is the right trade for memoization.

      An entry is replaced on the next miss for its key, and every miss also
      drops the entries that have expired, whatever key they are under.
      Nothing is reclaimed while no miss occurs, so a cache serving hits alone
      keeps what it holds. What the cache costs is therefore set by the
      distinct keys asked for within one [ttl], which is what to bound when
      the key comes from the request. *)

  val stats : t -> int * int @@ portable
  (** [stats t] is the hit and miss counts since [t] was created. *)
end

(** {1 For backend authors}

    Everything above describes a site. What follows is the machinery a backend
    needs to serve one, and an application never calls it. *)

module Backend : sig
  (** What every backend shares, so that conditional requests and HEAD are
      decided in one place and [proffer.mock] tests the code a socket backend
      runs.

      Use this module only when writing a backend. To test a site, drive it
      through [proffer.mock] instead. *)

  type body =
    | Empty
    | String of string @@ global
    | Stream of {
        length : int64 option;
        write : (Body.Sink.t -> unit) @@ global;
      }
  (** The body a backend is asked to write, with the choice already made:
      there is no [Delayed], because {!handle} has run the generator, and HEAD
      or a status that cannot carry content arrives as {!Empty}.

      The payloads carry [global], not the block. A socket write needs the
      string and the writer at global; it does not need the block holding
      them, so the block is built in the region and costs nothing. *)

  type outcome = {
    status : Status.t;
    headers : Headers.t;
        (** Fully rendered, including validators, Cache-Control and
            Content-Type. Content-Length is the backend's job. *)
    body : body;
    content_length : int64 option;
        (** The length the response would have, kept accurate for HEAD and
            304 so a backend can send Content-Length without a body. It is
            zero for 205 and absent for 1xx and 204. [None] otherwise means
            unknown, which for a stream means chunked. *)
  }
  (** One response, decided but not yet written. Every field is at the
      record's own mode, so a backend that reads it and writes it costs no
      heap at all: the length is an [int64 option] built in the region rather
      than on it, and {!body} keeps its payloads global without the block
      being global too. *)

  type writer = outcome @ local -> unit
  (** What a backend gives {!handle} to write one response with. It is called
      exactly once per request. *)

  val handle :
    ?on_error:(exn -> unit) ->
    'env Compiled.t ->
    'env ->
    Req.t @ local ->
    writer @ local ->
    unit
    @@ portable
  (** [handle compiled env req write] dispatches [req], applies the protocol
      mechanics that do not need a socket, and calls [write] once with the
      outcome, in this order.

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
      - HEAD empties the body and keeps [content_length]. Statuses that cannot
        carry content also empty it without running a stream or generator;
        205 declares zero length, while 1xx and 204 omit framing.
      - A {!Body.Delayed} generator runs once, here, so the outcome of a sent
        body is always [`String]. It never runs for HEAD or a contentless
        status, and an exception it raises goes to [on_error] and gives a 500
        like any other handler failure.

      A handler that raises, including one whose response is rejected as
      unwritable, is answered 500, so [handle] itself does not raise.

      Three cases a handler can get wrong are decided here rather than left to
      a backend. A handler that returns without responding is reported to
      [on_error] and answered 500. A handler that responds twice has the second
      call reported and dropped, since the first is already written. A handler
      that raises after responding is reported and nothing further is written,
      because the bytes have gone. *)

  val run :
    ?on_error:(exn -> unit) ->
    Req.t @ local ->
    (Resp.respond @ local -> unit) @ local ->
    writer @ local ->
    unit
    @@ portable
  (** [run req describe write] gives [describe] a responder and writes what it
      responds with, applying the same conditional-request, HEAD and
      {!Body.Delayed} mechanics {!handle} does and reporting the same three
      handler mistakes. {!handle} is this plus dispatch. A test reaches it
      through [proffer.mock] to exercise one response without a site. *)

  val sink :
    ?emit_sub:(bytes -> int -> int -> unit) ->
    (string -> unit) ->
    Body.Sink.t
    @@ portable
  (** [sink f] is how a backend wraps its writer for a [Body.Stream].

      A backend that can write a range of bytes without making a string of it
      should pass [emit_sub] as well, so that a producer streaming through a
      buffer costs nothing per slice. Without it {!Body.Sink.write_sub} copies
      the range into a string and calls [f], which costs no more to build:
      the fallback is written at the use site rather than made as a defaulting
      closure, so a sink is 3 words either way. *)
end
