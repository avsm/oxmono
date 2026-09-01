(** This module provides declarative HTTP servers with interchangeable backends.

    Proffer represents routes, handlers, responses, and cache policy without
    binding them to a network stack. A handler receives a {!Resp.respond}
    function and calls it exactly once to describe its response. The selected
    backend then writes that description to the client.

    Nothing that describes a response travels back up, so the description, its
    header block and the backend's outcome all live in the region
    {!Backend.handle} runs the handler in, at [local]. Answering a request
    allocates on the heap only what the body itself is made of.

    {2 Quick start}

    {[
    open Proffer
    open Proffer.Route

    type env = { greet : string -> string }

    let site =
      Site.of_routes
        [
          get root (fun _env _req respond -> Resp.text respond "index");
          get (s "hello" / str) (fun who env _req respond ->
              Resp.text respond (env.greet who));
          post (s "hello") (fun _env req respond ->
              match Req.form_param req "who" with
              | Some who -> Resp.see_other respond ("/hello/" ^ who)
              | None -> Resp.bad_request respond ());
        ]

    ]}

    Application state is passed to each handler through its ['env] argument.
    Dispatch, conditional requests, and HEAD handling are shared by all
    backends, including [proffer.mock].

    {2 Modes}

    Route constructors take their handler at [portable]. A handler therefore
    cannot capture domain-bound state, and a {!Site.t} is portable by
    construction. Portable {!Media.t} codecs, including those built from
    Jsont descriptions, can be defined once and captured directly. Mutable or
    otherwise domain-bound application state reaches a handler through the
    ['env] argument, which the mode system does not constrain. Build that
    state as a record of closures, one per domain. *)

(** {1 Protocol vocabulary} *)

module Media = Httpz.Media
(** [Media] is the module of typed media codecs from {!Httpz.Media}. A codec
    pairs a media type with portable encoder and decoder closures for one
    OCaml type. Codec values are portable and may be captured directly by
    portable routes.
    {!Json} and {!Markdown} provide the batteries-included JSON, JSON Lines,
    CommonMark, and HTML codecs.
    {!Req.decode} reads a request body with one, {!Resp.encode} responds with
    one, {!Route.with_body} turns a decoding failure into a 415 or 400, and
    {!Negotiate.encode} chooses between several by the Accept field. *)

module Json = Httpz.Json
(** [Json] is the bounded Jsont codec module from {!Httpz.Json}. The request
    body limit independently bounds the complete body. *)

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
      It raises [Invalid_argument] if [max_bracket_depth] is not positive. *)

  val html : ?safe:bool -> unit -> Cmarkit.Doc.t Media.t
  (** [html ()] encodes [text/html]. [safe] defaults to [true], dropping raw
      HTML and links whose schemes remain unsafe after percent-decoding and
      removing ASCII whitespace/control obfuscation. This conservative guard
      is not a substitute for a dedicated HTML sanitizer. *)
end

module Method : sig
  (** This module provides the HTTP request methods supported by {!Httpz}. *)

  type t = Httpz.Method.t
  (** A [t] is a method that httpz names. *)

  val to_string : t @ local -> string @@ portable
  (** [to_string m] is the method token as it appears on the request line. *)

  val equal : t -> t -> bool @@ portable
  (** [equal a b] is whether [a] and [b] are the same method. *)
end

module Status : sig
  (** This module provides the HTTP response statuses supported by {!Httpz}. *)

  type t = Httpz.Res.status
  (** A [t] is a response status that httpz names. *)

  val code : t -> int @@ portable
  (** [code s] is the three-digit status code. *)

  val reason : t -> string @@ portable
  (** [reason s] is the reason phrase for the status line. *)

  val of_code : int -> t option @@ portable
  (** [of_code n] is the status [n] names, if httpz names one. *)
end

module Headers : sig
  (** This module provides ordered HTTP field blocks.

      Known field names use {!Httpz.Header_name.t}. Unknown fields retain their
      original spelling and use {!Httpz.Header_name.Other}. Field-name matching
      is case-insensitive, as required by
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.1}RFC 9110 section
       5.1}.

      A block travels the response path at [local], so its cells are stack
      allocated and a response costs no heap for them. [spelling] and [value]
      are [global_] because a string is boxed and a socket write needs it at
      global. [name] needs no modality, since it is read only to compare and to
      hand back to httpz's writer. Every operation below takes the block at
      [local], which a global block also satisfies. *)

  type name = Httpz.Header_name.t
  (** A [name] identifies an HTTP field. *)

  type field = private {
    name : name;
    spelling : string;
        (** [spelling] is the canonical spelling of a known name, or the
            supplied spelling of an unknown name. *)
    value : string;
  }
  (** A [field] is one HTTP field. Its components may be inspected. Use {!h}
      or {!other} to construct one. *)

  type t = field list
  (** A [t] is a field block in wire order. *)

  val h : name -> string -> field @@ portable
  (** [h name value] is a field with the canonical spelling of [name]. It
      rejects {!Httpz.Header_name.Other}. Use {!other} for a custom name. *)

  val other : string -> string -> field @@ portable
  (** [other spelling value] is a custom field with the supplied [spelling]. A
      known spelling is instead resolved to its [name] constructor and canonical
      spelling. *)

  val[@zero_alloc] h_local : name -> string @ local -> field @ local @@ portable
  (** [h_local name value] is {!h} allocated in the caller's region.

      [stack_] on a list literal covers its cons cells and not the calls
      inside it, so [stack_ [ h n v ]] still puts every record on the heap.
      This is what a block on a path that answers every request should be
      built from. Everywhere else {!h} is simpler and the words do not
      matter. *)

  val[@zero_alloc] other_local :
    string @ local -> string @ local -> field @ local @@ portable
  (** [other_local] is {!other} allocated in the caller's region. *)

  val empty : t @@ portable
  (** [empty] is the block with no fields. *)

  val of_string : string -> name @@ portable
  (** [of_string s] is the name [s] spells, matched case-insensitively, or
      {!Httpz.Header_name.Other} for one httpz does not name. *)

  val of_list : (string * string) list -> t @@ portable
  (** [of_list l] is [l] as a block. Names recognised by httpz use their typed
      constructor and canonical spelling, and other spellings are retained. *)

  val to_list : t @ local -> (string * string) list @ local @@ portable
  (** [to_list t] is the fields of [t] in wire order, built in the caller's
      region. *)

  val find : t @ local -> name -> string option @ local @@ portable
  (** [find t name] is the first value under [name]. It is always [None] for
      {!Httpz.Header_name.Other}, which names no particular field; use
      {!find_other}. Repeated fields are not joined. *)

  val find_other :
    t @ local -> string @ local -> string option @ local @@ portable
  (** [find_other t spelling] is the first value under a field httpz does not
      name, matched case-insensitively. *)

  val[@zero_alloc] mem : t @ local -> name -> bool @@ portable
  (** [mem t name] is whether [t] has a field named [name]. It is always [false]
      for {!Httpz.Header_name.Other}. Use {!find_other} to look up a custom
      name. *)

  val[@zero_alloc] same_name : name @ local -> name @ local -> bool @@ portable
  (** [same_name a b] is whether [a] and [b] are the same constructor. Every
      {!Httpz.Header_name.Other} is the same constructor, so this does not
      distinguish two differently spelled custom fields. *)

  val iter :
    (name -> string @ local -> string @ local -> unit) @ local ->
    t @ local ->
    unit
    @@ portable
  (** [iter f t] is [()] after applying [f] to each name, spelling, and value
      in order. It exists because [List.iter] takes a global list. *)

  val cat : t @ local -> t @ local -> t @ local @@ portable
  (** [cat a b] is [a] then [b], allocated in the caller's region. A wrapper
      that adds a field to a block on its way past uses it, and pays no
      heap. *)

  val vary : t @ local -> string -> t @ local @@ portable
  (** [vary t name] is [t] with [name] added to its Vary field, allocated in
      the caller's region. Repeated Vary fields are combined, names are
      compared case-insensitively, and a wildcard remains a wildcard. *)
end

module Mime : sig
  (** This module maps filename extensions to content types. *)

  val of_path : string -> string @@ portable
  (** [of_path name] is the Content-Type for [name], from its extension with
      case folded, or ["application/octet-stream"] when it is absent or unknown.
      A file-serving response should also send [X-Content-Type-Options:
      nosniff]; active formats such as HTML, SVG, and Markdown need an
      application-appropriate Content-Security-Policy when their contents are
      not fully trusted. *)
end

(** {1 Requests} *)

module Req : sig
  (** This module provides parsed HTTP requests. A backend builds a request from
      the wire, while tests may construct one directly with {!v}.

      Every string read out of a request is local to it. A backend builds them
      in the request's region, and a handler that keeps one past the request
      copies it with {!globalize}. *)

  type t
  (** A [t] is an HTTP request. *)

  val v :
    meth:Method.t ->
    target:string ->
    ?version:Httpz.Version.t ->
    ?connection_upgrade:bool ->
    ?path:string ->
    ?query:string ->
    ?headers:Headers.t ->
    ?body:string ->
    unit ->
    t @ local
    @@ portable
  (** [v ~meth ~target ()] is a request with the supplied method and raw target.
      [path] and [query] override the values normally derived from [target].
      [version] defaults to HTTP/1.1. [connection_upgrade] defaults to [false]
      and records whether a validated Connection field offered [upgrade].
      [headers] defaults to {!Headers.empty}, and [body] defaults to [""].
      [headers] is a block rather than an association list, so a backend that
      already has one built from its own parse hands it over without a second
      copy. Use {!Headers.of_list} for a literal. *)

  val meth : t @ local -> Method.t @@ portable
  (** [meth t] is the request method. *)

  val version : t @ local -> Httpz.Version.t @@ portable
  (** [version t] is the request's HTTP version. *)

  val connection_upgrade : t @ local -> bool @@ portable
  (** [connection_upgrade t] is whether the request's Connection field named
      [upgrade]. It is false for synthetic requests unless supplied to {!v}. *)

  val target : t @ local -> string @ local @@ portable
  (** [target t] is the request target as it arrived, undecoded. A client may
      send it in absolute-form, so it can begin with a scheme and an authority
      of the client's choosing. Building a Location from it is how an open
      redirect is written. Use {!path} for that. *)

  val path : t @ local -> string @ local @@ portable
  (** [path t] is the still-encoded path used for routing. It is normally the
      part of the target before any ['?'], unless {!v} supplied an override. *)

  val segments : t @ local -> string list @@ portable
  (** [segments t] is the path split on ['/'], with empty segments dropped and
      each remaining one percent-decoded. An invalid escape is preserved. Routes
      use the same decoding rules. *)

  val query : t @ local -> (string * string) list @@ portable
  (** [query t] is the query string decoded, with ['+'] read as a space. A
      parameter given without a value has the empty string, and an invalid
      percent escape is preserved. *)

  val query_param : t @ local -> string -> string option @@ portable
  (** [query_param t name] is the first value of [name] in {!query}. *)

  val headers : t @ local -> Headers.t @ local @@ portable
  (** [headers t] is the request's field block. *)

  val header : t @ local -> Headers.name -> string option @ local @@ portable
  (** [header t name] is the first value under [name]. It is always [None] for
      {!Httpz.Header_name.Other}; use {!header_other}. *)

  val header_other :
    t @ local -> string @ local -> string option @ local @@ portable
  (** [header_other t spelling] is the first value under a field httpz does not
      name, matched case-insensitively. *)

  val body : t @ local -> string @ local @@ portable
  (** [body t] is the request body, or [""] when there is none. *)

  val globalize : string @ local -> string @@ portable
  (** [globalize s] is a heap copy of [s]. Every string read out of a request
      is local to it, so a handler that keeps one past the request copies it
      with this. *)

  val is_form : t @ local -> bool @@ portable
  (** [is_form t] is whether the Content-Type is
      [application/x-www-form-urlencoded]. Its parameters, [charset] among
      them, are ignored. *)

  val form_result :
    t @ local -> ((string * string) list, Media.error) result @@ portable
  (** [form_result t] is the body decoded as
      [application/x-www-form-urlencoded] by {!Media.form}. It is
      [Error (Unsupported ct)] when {!is_form} is false, which is how a
      handler tells a body of another media type from an empty form.

      No charset is applied: the body is decoded as bytes, and a browser
      posting from a UTF-8 page sends UTF-8. *)

  val form : t @ local -> (string * string) list @@ portable
  (** [form t] is {!form_result} with a body of another media type read as
      [[]]. Order and repeated names are preserved. *)

  val form_param : t @ local -> string -> string option @@ portable
  (** [form_param t name] is the first value of [name] in {!form}, found by
      scanning the body rather than by building the list. It is [None] when
      {!is_form} is false. *)

  val forwarded_for : t @ local -> string option @@ portable
  (** [forwarded_for t] is the first X-Forwarded-For entry. The field is a
      chain the client writes the head of, so the first entry is whatever the
      client put there. It names the peer only if the trusted proxy in front
      of this server {e strips} any client-supplied X-Forwarded-For before
      appending the address it saw. A proxy that merely appends leaves the
      value attacker-controlled, and rate limits or audit logs keyed on it are
      forgeable. *)

  val forwarded_proto : t @ local -> string option @@ portable
  (** [forwarded_proto t] is X-Forwarded-Proto, lowercased. *)

  val decode : 'a Media.t -> t @ local -> ('a, Media.error) result @@ portable
  (** [decode codec t] is the body decoded by [codec]. It is
      [Error (Unsupported ct)] when the Content-Type [ct], or its absence, is
      not one [codec] accepts, and [Error (Malformed error)] when the decoder
      rejects the body. A bad body is the client's mistake, so it is a value
      for the handler to answer rather than an exception. {!Route.with_body}
      answers it with 415 or 400. *)

  val decode_seq :
    'a Media.seq -> t @ local -> ('a list, Media.error) result @@ portable
  (** [decode_seq codec t] is every value in the body, as {!decode}. *)
end

module Multipart : sig
  (** This module reads [multipart/form-data] request bodies, the encoding a
      browser uses for a form carrying a file.

      Each control arrives as a body part framed by a delimiter built from the
      [boundary] parameter of the Content-Type field, named by a
      Content-Disposition of [form-data], and optionally given a filename. The
      framing is that of
      {{:https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.1}RFC 2046
       section 5.1.1}, the form conventions are
      {{:https://www.rfc-editor.org/rfc/rfc7578.html}RFC 7578}, and an extended
      [filename*] parameter is read as
      {{:https://www.rfc-editor.org/rfc/rfc8187.html}RFC 8187} defines it.
      Line endings must be CRLF, and a part whose content type is
      [multipart/mixed] is left unexpanded.

      A body reaches a handler entire and in memory, so the backend's request
      cap is the upload limit: [proffer-httpz] holds a whole request in about
      32 KiB and answers a larger one with 413, which admits a small avatar or
      text file and nothing more. An upload of real size needs a backend that
      streams. *)

  type part = Httpz.Multipart.part = {
    name : string;  (** [name] is the [name] parameter of the part. *)
    filename : string option;
        (** [filename] is the [filename*] parameter when present and otherwise
            the [filename] parameter. It is [None] for a part that is not a
            file, which is how a field is told from an upload. *)
    content_type : string option;
        (** [content_type] is the part's Content-Type, or [None]. RFC 7578
            leaves that to mean [text/plain]. It is the client's claim about
            the bytes, not a fact about them. *)
    headers : (string * string) list;
        (** [headers] holds every part header in order, lowercased name and
            trimmed value, including those read into the other members. *)
    off : int;  (** [off] is the first content byte within {!Req.body}. *)
    len : int;  (** [len] is the content length in bytes. *)
  }
  (** A [part] is one body part. Its content stays in the request body rather
      than being copied; use {!content}. *)

  val of_req :
    ?max_parts:int -> Req.t @ local -> (part list, Media.error) result
    @@ portable
  (** [of_req req] is the parts of [req]'s body, in order. [max_parts] bounds
      how many are accepted and defaults to 256. Zero accepts only a multipart
      body with no parts. A negative value raises [Invalid_argument].

      It is [Error (Unsupported ct)] unless the Content-Type is
      [multipart/form-data] with a valid boundary, and
      [Error (Malformed error)] when the part count, framing, or a part header
      is rejected, [error]'s message naming the reason. The message is a
      diagnostic, not text to return to a client. *)

  val content : Req.t @ local -> part -> string @@ portable
  (** [content req p] is a copy of the content of [p], which must be a part
      {!of_req} returned for [req]. *)

  val field : Req.t @ local -> part list -> string -> string option @@ portable
  (** [field req parts name] is the content of the first part named [name]
      that has no filename. *)

  val file : part list -> string -> part option @@ portable
  (** [file parts name] is the first part named [name] that has a filename. *)

  val fields : Req.t @ local -> part list -> (string * string) list @@ portable
  (** [fields req parts] is every part without a filename, in order, as name
      and content pairs, in the shape {!Req.form} returns. *)
end

(** {1 Responses} *)

module Cache_control : sig
  (** This module constructs Cache-Control policies for responses. See
      {{:https://www.rfc-editor.org/rfc/rfc9111#section-5.2}RFC 9111 section
       5.2} for the standard directives. *)

  type span = [ `Secs of int | `Hours of int | `Days of int ]
  (** A [span] is a freshness lifetime. Every form is written to the header in
      seconds. *)

  type t : immutable_data
  (** A [t] is an immutable cache policy. The kind is declared so a policy may
      be defined once at the top level and still be reachable from a portable
      handler. An abstract type without one reads as contended there. *)

  val no_store : t @@ portable
  (** [no_store] is a policy that forbids any storage of the response. *)

  val private' : ?max_age:span -> unit -> t @@ portable
  (** [private' ?max_age ()] is a policy that permits storage by a private
      cache, such as a browser cache, but not by a shared cache. It raises
      [Invalid_argument] if a duration is negative or too large to express in
      seconds. *)

  val public :
    max_age:span ->
    ?s_maxage:int ->
    ?stale_while_revalidate:int ->
    ?must_revalidate:bool ->
    ?immutable:bool ->
    unit ->
    t
    @@ portable
  (** [public ~max_age ()] is a policy that permits shared caches to store the
      response. [s_maxage] overrides [max_age] for shared caches.
      [stale_while_revalidate] permits stale reuse while a cache revalidates in
      the background, as specified by
      {{:https://www.rfc-editor.org/rfc/rfc5861#section-3}RFC 5861 section 3}.
      [must_revalidate] forbids stale reuse without successful validation.
      [immutable] states that the representation will not change while fresh, as
      specified by {{:https://www.rfc-editor.org/rfc/rfc8246.html}RFC 8246}.
      Durations must be nonnegative. The function raises [Invalid_argument] if a
      duration is negative or too large to express in seconds. *)

  val to_string : t -> string @@ portable
  (** [to_string t] is the Cache-Control field value. *)
end

module Body : sig
  (** This module describes response bodies. A backend chooses HTTP framing from
      the body form and its optional declared length. *)

  module Sink : sig
    (** This module is the output path a backend lends to a streaming body. *)

    type t

    val write : t -> string -> unit @@ portable
    (** [write t s] is [()] after emitting [s]. The sink is valid only during
        its {!Body.Stream} callback and must not escape it. The backend neither
        mutates nor retains [s] after [write] returns.

        The sink is a heap value, not a local one, and stays that way: a
        producer driving an encoder has to capture it in the closure the
        encoder writes through, and those take a global function. Lending one
        costs 3 words per streamed response. *)

    val write_sub : t -> bytes -> off:int -> len:int -> unit @@ portable
    (** [write_sub t b ~off ~len] is [()] after emitting [len] bytes of [b]
        starting at [off]. The backend neither mutates nor retains [b] after
        the call. It raises [Invalid_argument] for an invalid byte range.

        This is the way in for a producer that already holds bytes, which is
        every encoder that writes through a buffer: it hands over the
        encoder's own slice rather than making a string for each one. A
        backend that can only take strings pays a copy per slice here, so a
        producer that has a string should call {!write} instead. *)
  end

  module Socket : sig
    (** A bidirectional connection transferred out of HTTP framing. *)

    type t

    val read : t -> bytes -> off:int -> len:int -> int @@ portable
    (** [read socket buffer ~off ~len] reads at most [len] bytes, first from
        bytes that arrived in the same packet as the HTTP request head. It
        returns zero at end of input. *)

    val write : t -> string -> unit @@ portable
    (** [write socket string] writes all of [string]. *)

    val write_sub : t -> bytes -> off:int -> len:int -> unit @@ portable
    (** [write_sub socket buffer ~off ~len] writes that byte range in full. *)

    val shutdown : t -> unit @@ portable
    (** [shutdown socket] shuts down both directions of the connection. *)
  end

  type handoff_kind =
    | Tunnel
    | Upgrade of string @@ global
  (** [Tunnel] follows a successful CONNECT response. [Upgrade protocol]
      follows a 101 response selecting [protocol]. *)

  (** A [t] is a response body. *)
  type t =
    | Empty  (** [Empty] is no body and a Content-Length of zero. *)
    | String of string @@ global
        (** [String s] is the body [s], already in memory. *)
    | Delayed of { length : int64 option; gen : (unit -> string) @@ global }
        (** [Delayed { length; gen }] is a body generated on demand. [gen] is
            not called for HEAD or a status that cannot carry content. A
            declared [length] must be between zero and [max_int] and equal the
            generated string's byte length. *)
    | Stream of {
        length : int64 option;
        write : (Sink.t -> unit) @@ global;
        trailers : Headers.t;
      }
        (** [Stream { length; write; trailers }] is a body written
            incrementally. Each
            backend chooses framing when [length] is [None]. [proffer-httpz]
            uses chunked transfer coding for HTTP/1.1 and closes HTTP/1.0
            connections after the body. A declared [length] must be between zero
            and [max_int] and equal the number of emitted bytes. Non-empty
            [trailers] force chunked framing in HTTP/1.1. *)
    | Handoff of {
        kind : handoff_kind;
        run : (Socket.t -> unit) @@ global;
      }
        (** [Handoff { kind; run }] sends a framing-free CONNECT or 101
            response head, then calls [run] with ownership of the connection.
            Prefer {!Resp.tunnel} or {!Resp.upgrade} to construct one. *)
end

module Etag : sig
  (** This module constructs entity-tags for cache validation. See
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3}RFC 9110 section
       8.8.3}. *)

  type t : immutable_data
  (** A [t] is an entity-tag. {!Resp.v} rejects opaque values containing bytes
      outside the [etagc] syntax defined by RFC 9110. Spaces, controls, double
      quotes, and DEL are therefore invalid. Visible non-quote ASCII and bytes
      at or above [0x80] are valid.

      The kind is declared because the type is abstract. A site that builds a
      tag once at the top level and serves it from a portable handler, which
      is what a static asset does, needs it to cross into that closure, and an
      abstract type carries no kind unless it says so.

      A tag carries its wire form alongside its opaque value and renders it
      once, when it is built. A memoised page builds its tag when it fills the
      cache and answers from it thereafter, so putting the quotes on costs
      nothing per request. *)

  val strong : string -> t @@ portable
  (** [strong s] is the strong entity-tag with opaque value [s]. *)

  val weak : string -> t @@ portable
  (** [weak s] is the weak entity-tag with opaque value [s]. *)

  val opaque : t @ local -> string @@ portable
  (** [opaque t] is the value without quotes or a [W/] prefix. *)

  val is_weak : t @ local -> bool @@ portable
  (** [is_weak t] is whether [t] was declared weak. *)

  val to_string : t @ local -> string @@ portable
  (** [to_string t] is the ETag field value, quoted and prefixed with ["W/"]
      when weak. *)

  val weak_equal : t @ local -> t @ local -> bool @@ portable
  (** [weak_equal a b] is [true] when [a] and [b] have equal opaque values,
      regardless of whether either tag is weak. Conditional GET uses this
      comparison as specified by
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3.2}RFC 9110 section
       8.8.3.2}. *)
end

module Resp : sig
  (** This module constructs HTTP response descriptions.

      A handler must call its responder exactly once and must not retain it.
      Returning without responding produces a 500 response. A second response is
      ignored because the first may already be on the wire. Both mistakes are
      reported through the backend's [on_error] callback.

      A responder is always used at [local]. A backend builds one per request,
      and a handler that stashed one would hold a closure over a connection
      about to be reused. The mode is what stops that. *)

  type description = private {
    status : Status.t;
    headers : Headers.t;
    etag : Etag.t option;
    last_modified : float option;
    cache : Cache_control.t option;
    content_type : string or_null;
    body : Body.t;
  }
  (** A [description] is a complete response consumed synchronously by a
      backend. Its fields may be inspected, and handlers construct one through
      {!v}.

      It is passed at [local], so it costs no heap. [headers] is left at the
      record's own mode, because the block is the part worth keeping on the
      stack. Every other field holds a heap value that has to be readable at
      global to reach a socket, so each is [global_]. *)

  type respond = description @ local -> unit
  (** A [respond] accepts one complete response description.

      One record argument rather than a run of labelled ones: currying and
      locality do not mix, since a curried function used at [local] groups its
      arrows and an application then reads as complete after the first
      argument. *)

  val v :
    respond @ local ->
    ?status:Status.t ->
    headers:Headers.t @ local ->
    ?etag:Etag.t @ local ->
    ?last_modified:float @ local ->
    ?cache:Cache_control.t @ local ->
    content_type:string or_null @ local ->
    Body.t @ local ->
    unit
    @@ portable
  (** [v respond ~headers ~content_type body] is [()] after passing [respond] a
      response with [body] and the supplied metadata. [status] defaults to 200
      OK.

      The body is taken at [local], so a caller that writes
      [stack_ (Body.String s)] pays nothing for the block naming it. The
      string inside stays global, which is where a socket needs it. Every
      constructor below does this, so it matters only to a caller reaching
      for [v] directly.

      [headers] and [content_type] are required rather than optional.
      [content_type] is [or_null] rather than an option because a value that
      cannot be null needs no box to say so, so naming a content type here
      costs nothing at all.

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
            ~headers:(stack_ [ Resp.h_local Httpz.Header_name.X_cache "hit" ])
            (Body.String page)
        in
        ()
      ]}

      Everywhere else the convenience is worth more than the words, and a
      constructor below with no [~headers] at all allocates nothing for the
      block either way, since the default is a constant.

      [etag], [cache] and [last_modified] are optional and arrive at [local],
      so naming one costs nothing on the heap. {!Backend.handle} renders them
      only once it knows the response is being sent, so a 304 pays for no
      block at all.

      [last_modified] is seconds since the Unix epoch. [content_type], [cache],
      [etag], and [last_modified] each add their corresponding HTTP field and
      must not also be supplied in [headers]. Validators are used to answer
      conditional requests according to
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-13}RFC 9110 section 13}.

      It raises [Invalid_argument] if a field would not survive the wire: a
      header name that is not a non-empty RFC 9110 token, a header value or
      [content_type] containing a forbidden control byte, an [etag] whose
      opaque value is outside RFC 9110's [etagc] syntax, or a [last_modified]
      outside the finite times in years 1 through 9999. It also raises
      [Invalid_argument] if [headers] duplicates a field supplied by a typed
      argument, if it names Content-Length, Transfer-Encoding, Connection, or
      Trailer, which the backend owns; supplies Upgrade on a status other than
      426; or if a declared body length is negative or greater than [max_int].

      A 206 Partial Content response must say which bytes it carries. It
      raises [Invalid_argument] unless [headers] holds a Content-Range in the
      [bytes] unit meeting
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-14.4}RFC 9110 section
       14.4}, as ["bytes first-last/complete"] or ["bytes first-last/*"] with
      [first <= last] and [last < complete], or unless the content type is
      [multipart/byteranges] with a [boundary] parameter and no top-level
      Content-Range, each part carrying its own instead
      ({{:https://www.rfc-editor.org/rfc/rfc9110#section-14.6}RFC 9110
       section 14.6}). ["bytes */complete"] belongs on a 416 and is refused
      here. Without the field a cache stores the fragment as the whole
      representation.

      Statuses whose meaning depends on a response field must carry it: 401
      needs WWW-Authenticate, 405 needs Allow, 407 needs Proxy-Authenticate,
      416 needs a ["bytes */complete-length"] Content-Range, and 426 needs
      Upgrade ({{:https://www.rfc-editor.org/rfc/rfc9110#section-15.5}RFC 9110
       section 15.5}). [v] raises [Invalid_argument] when one is absent (or
      when the 416 Content-Range has the wrong shape), since a client cannot
      act on the response without it.

      Informational 1xx statuses cannot be returned as final responses. The
      sole exception is the 101 handoff built by {!upgrade}; constructing a
      bare 101, or any other final 1xx response, raises [Invalid_argument].

      Invalid response descriptions raised by a handler are reported through
      [on_error] and become a 500 response. *)

  val h : Headers.name -> string -> Headers.field @@ portable
  (** [h name value] is {!Headers.h}, re-exported for response construction. *)

  val other : string -> string -> Headers.field @@ portable
  (** [other spelling value] is {!Headers.other}, re-exported. *)

  val h_local : Headers.name -> string -> Headers.field @ local @@ portable
  (** [h_local] is {!Headers.h_local}, re-exported: the constructor to build a
      block from on a path that answers every request. *)

  val html :
    respond @ local ->
    ?status:Status.t ->
    ?etag:Etag.t @ local ->
    ?cache:Cache_control.t @ local ->
    ?headers:Headers.t @ local ->
    string ->
    unit
    @@ portable
  (** [html respond s] is [()] after responding with [s] as
      [text/html; charset=utf-8]. [status] defaults to 200 OK. [headers] are
      appended, while [etag] and [cache] add their corresponding fields. *)

  val text :
    respond @ local ->
    ?status:Status.t ->
    ?headers:Headers.t @ local ->
    string ->
    unit
    @@ portable
  (** [text respond s] is [()] after responding with [s] as
      [text/plain; charset=utf-8]. [status] defaults to 200 OK, and [headers]
      are appended to the response. *)

  val media :
    respond @ local ->
    ?status:Status.t ->
    ?etag:Etag.t @ local ->
    ?cache:Cache_control.t @ local ->
    ?headers:Headers.t @ local ->
    string ->
    string ->
    unit
    @@ portable
  (** [media respond ct s] is [()] after responding with [s] under Content-Type
      [ct]. [status] defaults to 200 OK. [headers] are appended, while [etag]
      and [cache] add their corresponding fields. *)

  val stream :
    respond @ local ->
    ?status:Status.t ->
    ?cache:Cache_control.t @ local ->
    ?headers:Headers.t @ local ->
    ?length:int64 ->
    ?trailers:Headers.t @ local ->
    string ->
    (Body.Sink.t -> unit) ->
    unit
    @@ portable
  (** [stream respond ct write] is [()] after responding under Content-Type
      [ct] with the bytes that [write] emits to its sink.

      This is the constructor for a body that is produced rather than held: an
      encoder writing through a buffer hands each slice straight to the socket
      and the finished body never exists as a string. On a route answering a
      megabyte that is the difference between one copy and none.

      [status] defaults to 200 OK. [headers] are appended, and [cache] adds a
      Cache-Control field. Omit [length] unless it is known before [write] runs,
      which for an encoder is commonly not the case. A supplied length must be
      between zero and [max_int] and must equal the number of emitted bytes.
      [trailers] defaults to an empty block. A non-empty block is declared in
      the response's Trailer field and emitted separately after [write], using
      chunked framing even when [length] is known. Fields that RFC 9110 forbids
      in trailers are rejected. Without a declared length or trailers, the
      backend chooses framing. [proffer-httpz] uses chunked transfer coding for
      HTTP/1.1 and closes HTTP/1.0 connections after the body. [write] is not
      called for HEAD or a status that cannot carry content.

      Backends choose when to invoke [write]. [proffer-httpz] invokes it after
      the response head is on the wire, so a failure part way through truncates
      the body and is reported through [on_error]. [stream] has no [etag]
      argument. Use {!v} with {!Body.Stream} to supply a validator computed
      before responding. *)

  val tunnel :
    respond @ local ->
    ?status:Status.t ->
    ?headers:Headers.t @ local ->
    (Body.Socket.t -> unit) ->
    unit
    @@ portable
  (** [tunnel respond run] sends a successful CONNECT response with no HTTP
      content framing, then calls [run] with the connection, including bytes
      already buffered after the request head. [status] defaults to 200 and
      must be in the 2xx class. The request itself must use CONNECT. *)

  val upgrade :
    respond @ local ->
    ?headers:Headers.t @ local ->
    protocol:string ->
    (Body.Socket.t -> unit) ->
    unit
    @@ portable
  (** [upgrade respond ~protocol run] sends 101, Connection: Upgrade, and the
      selected Upgrade field before calling [run] with the connection.
      [protocol] is a protocol-name token with an optional [/version] token.
      The request must be HTTP/1.1, its validated Connection metadata must name
      [upgrade], and an Upgrade field must offer [protocol]; otherwise no
      callback runs and the invalid response becomes a 500. *)

  val encode :
    respond @ local ->
    ?status:Status.t ->
    ?etag:Etag.t @ local ->
    ?cache:Cache_control.t @ local ->
    ?headers:Headers.t @ local ->
    'a Media.t ->
    'a ->
    unit
    @@ portable
  (** [encode respond codec value] encodes [value] under the codec's
      Content-Type. The encoded string is produced before responding, so the
      backend knows its length and may suppress it for HEAD or revalidation. *)

  val encode_seq :
    respond @ local ->
    ?status:Status.t ->
    ?cache:Cache_control.t @ local ->
    ?headers:Headers.t @ local ->
    'a Media.seq ->
    'a Seq.t ->
    unit
    @@ portable
  (** [encode_seq respond codec items] streams [items] one at a time under the
      sequence codec's Content-Type. Writer-backed codecs pass their byte
      slices directly to the response sink without constructing an encoded
      string for each item. *)

  val empty :
    respond @ local ->
    ?status:Status.t ->
    ?headers:Headers.t @ local ->
    unit ->
    unit
    @@ portable
  (** [empty respond ()] is [()] after responding with no body. [status]
      defaults to 200 OK, and [headers] are appended to the response. *)

  val see_other : respond @ local -> string -> unit @@ portable
  (** [see_other respond location] is [()] after responding with 303 See Other
      and a Location field containing [location]. *)

  val redirect :
    respond @ local -> ?permanent:bool -> string -> unit @@ portable
  (** [redirect respond location] is [()] after responding with 302 Found, or
      301 Moved Permanently when [permanent] is [true]. *)

  val not_found : respond @ local -> ?html:string -> unit -> unit @@ portable
  (** [not_found respond ()] is a 404 carrying [html], or a minimal page. *)

  val bad_request :
    respond @ local -> ?html:string -> unit -> unit @@ portable
  (** [bad_request respond ()] is a 400 carrying [html], or a minimal page. *)
end

(** {1 Server-Sent Events} *)

module Sse : sig
  (** This module constructs streaming [text/event-stream] responses. *)

  type sink
  (** A [sink] writes one server-sent event stream. It is valid only during the
      callback passed to {!respond}. *)

  val send : sink -> ?name:string -> ?id:string -> string -> unit @@ portable
  (** [send sink data] writes one event. Newlines in [data] become separate
      data fields. [name] and [id] must not contain newlines, and [id] must not
      contain NUL. *)

  val comment : sink -> string -> unit @@ portable
  (** [comment sink text] writes a comment block suitable for a keep-alive. *)

  val retry : sink -> int -> unit @@ portable
  (** [retry sink milliseconds] writes a reconnect delay. Negative values are
      rejected. *)

  val respond :
    Resp.respond @ local -> ?retry:int -> (sink -> unit) -> unit @@ portable
  (** [respond respond write] describes a 200 [text/event-stream] response
      with [Cache-Control: no-store] and an unknown-length streaming body.
      [retry], when supplied, is written before [write] runs. *)
end

(** {1 Routes and sites} *)

module Route : sig
  (** This module provides typed paths and request handlers.

      Each captured path segment becomes a curried handler argument.

      {[
        get root (fun env _req respond -> ...)
        (* env -> Req.t -> Resp.respond @ local -> unit *)

        get (s "contact" / str) (fun handle env req respond -> ...)
        (* string -> env -> Req.t -> Resp.respond @ local -> unit *)

        get (s "a" / str / s "b" / int) (fun x n env req respond -> ...)
        (* string -> int -> env -> ... *)

        get (s "static" / rest) (fun segs env req respond -> ...)
        (* string list -> env -> ... *)
      ]}

      Join segments with [( / )], which associates to the left and therefore
      reads in path order. A path matches the whole request path unless it ends
      in {!rest}, which captures whatever remains and can only come last.

      Matching normalizes the request path: empty segments are skipped, so
      [/a//b] and [/a/b/] both reach the route for [/a/b]. A front proxy
      authorizing by path prefix sees the unnormalized target and must
      normalize the same way, or a rule on [/admin/] will not cover
      [//admin/]. Dot segments are not resolved: [.] and [..] remain ordinary
      decoded segments and normally fail to match a literal route. A front
      proxy that resolves them must do so before applying security policy and
      forward that same normalized target.

      A GET route also answers HEAD. {!Backend.handle} suppresses the body. *)

  type 'env handler =
    'env -> (Req.t @ local -> Resp.respond @ local -> unit) @ local
  (** A ['env handler] is what a route runs. The ['env] argument carries
      application state, since a portable closure cannot capture it. The
      request and the responder are built per request in the region
      {!Backend.handle} runs the handler in, and neither may be retained. The
      closure left after ['env] is local for the same reason, which is what
      lets a handler take a captured segment. *)

  type open_
  (** [open_] indexes a path that [( / )] can extend. *)

  type closed
  (** [closed] indexes a path that ends in {!rest}. *)

  type ('f, 'r, 'k) path
  (** A [('f, 'r, 'k) path] records its captured handler arguments and whether
      it remains open for another segment. *)

  type 'env t
  (** An ['env t] is one route: a method, a pattern, and a handler. *)

  val root : ('r, 'r, open_) path @@ portable
  (** [root] is the empty path, which matches [/] on its own. *)

  val rest : (string list @ local -> 'r @ local, 'r, closed) path @@ portable
  (** [rest] captures every remaining percent-decoded segment, and possibly
      none. It can only end a path. The list and its strings live in the
      request's region.

      The segments are percent-decoded, so one of them may be [".."] or may
      itself contain a ['/'] that arrived as [%2F]. A handler that turns them
      into a filesystem path must reject unsafe values. {!Static.confine}
      performs this lexical validation. *)

  val s : string -> ('r, 'r, open_) path @@ portable
  (** [s name] matches a percent-decoded segment equal to [name]. *)

  val str : (string @ local -> 'r @ local, 'r, open_) path @@ portable
  (** [str] captures one percent-decoded segment as a string built in the
      request's region. A handler that keeps it copies it with
      {!Req.globalize}. *)

  val int : (int -> 'r @ local, 'r, open_) path @@ portable
  (** [int] captures one percent-decoded segment that is a decimal integer,
      optionally preceded by ['-']. Leading zeroes (except the value [0]) and
      negative zero do not match. Other spellings OCaml would read, such as
      [0x1f], [1_000] and [+3], do not match, so one resource has one path. A
      value too large for an [int] does not match either. *)

  val conv :
    name:string ->
    (string -> 'a option) @ portable ->
    ('a -> 'r @ local, 'r, open_) path
    @@ portable
  (** [conv ~name parse] passes one percent-decoded segment to [parse] and
      captures its result when that is [Some value]. [parse] receives a heap
      copy of the segment. *)

  val ( / ) :
    ('f, 'g, open_) path -> ('g, 'r, 'k) path -> ('f, 'r, 'k) path
    @@ portable
  (** [p / q] matches [p] then [q]. [p] must be open, so nothing can follow
      {!rest}. *)

  val get :
    ('f, 'env handler, _) path -> 'f @ portable -> 'env t @@ portable
  (** [get path handler] is a route that answers GET, and HEAD, at [path].
      [handler] is taken at [portable], so the compiler rejects one that
      captures domain-bound state here, where the fix belongs. *)

  val post :
    ('f, 'env handler, _) path -> 'f @ portable -> 'env t @@ portable
  (** [post path handler] is a route that answers POST at [path]. *)

  val route :
    Method.t -> ('f, 'env handler, _) path -> 'f @ portable -> 'env t
    @@ portable
  (** [route meth path handler] is the general form. *)

  val moved :
    ('env handler, 'env handler, _) path -> string -> 'env t @@ portable
  (** [moved path location] is a route that answers GET, and HEAD, at [path]
      with a 301 to [location]. The path captures nothing, so [location] is
      fixed. A location built from a capture needs a {!get} returning
      {!Resp.redirect}.
  *)

  val found :
    ('env handler, 'env handler, _) path -> string -> 'env t @@ portable
  (** [found path location] is {!moved} with a 302 instead. *)

  val with_body :
    ('env -> 'a Media.t) @ portable ->
    ('a -> 'env handler) @ portable ->
    'env handler @ portable
    @@ portable
  (** [with_body codec_of_env f] obtains a codec from the environment, decodes
      the request body, and passes the value to [f]. Since {!Media.t} is
      portable, [codec_of_env] may simply return a captured module-level codec;
      the environment form also permits per-request or per-domain selection.
      An unsupported Content-Type gets 415 and a malformed body gets 400. *)
end

module Site : sig
  (** This module provides ordered route sets with fallback and response
      wrappers. A site is handed directly to a backend. *)

  type 'env t : value mod portable
  (** An ['env t] is a site ready to serve. It holds data and portable handlers
      only, so a site defined once remains usable by every domain. *)

  val of_routes : 'env Route.t list -> 'env t @@ portable
  (** [of_routes routes] is a site matching [routes] in order. Its fallback is a
      plain 404 text response. *)

  val with_fallback : 'env Route.handler @ portable -> 'env t -> 'env t
    @@ portable
  (** [with_fallback handler site] is [site] with [handler] used when no route
      matches the path. A path available under another method still receives 405
      Method Not Allowed. *)

  val with_headers : (string * string) list -> 'env t -> 'env t @@ portable
  (** [with_headers extra site] is [site] with [extra] added to every response,
      including fallback and library-generated responses that pass through this
      wrapper. Existing response fields come first when a name repeats.

      Wrappers applied later run outside earlier wrappers. In particular, apply
      [with_headers] after {!with_auth} when authentication challenges also need
      [extra].

      It raises [Invalid_argument] if a name is empty or not an HTTP token, if a
      value contains a forbidden control byte, or if a name is Content-Length,
      Transfer-Encoding, Connection, or Trailer, which the backend owns. Once
      the wrapped response is known it also refuses a field that collides with
      typed response metadata or a generated Upgrade field, or otherwise makes
      the response invalid. *)

  val with_auth :
    scope:string list list ->
    realm:string ->
    check:(string option @ local -> bool) @ portable ->
    'env t ->
    'env t
    @@ portable
  (** [with_auth ~scope ~realm ~check site] is [site] with every path below a
      prefix protected in [scope]. [check] receives the Authorization field at
      [local], or [None] when it is absent. A failed check responds with 401
      Unauthorized
      and a Basic authentication challenge naming [realm]. The empty prefix [[]]
      protects the whole site.

      The gate is what answers under [scope], so a request that would have got a
      404 or a 405 there gets the 401 instead. A caller without credentials
      therefore cannot tell which paths under [scope] name a route.

      A request carrying more than one Authorization field is rejected before
      [check] runs, including repetitions whose field-name case differs.

      [scope] names paths in [site] as it stands. Mounting a gated site under
      a prefix would leave every prefix in [scope] naming a path that no longer
      exists, and the gate matching nothing: {!mount} refuses a gated sub-site
      for that reason. Mount first and gate the result, with [scope] written in
      the mounted paths.

      It raises [Invalid_argument] if [realm] is not unescaped quoted text as
      defined by
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4}RFC 9110 section
       5.6.4}, or if [scope] is empty. Double quotes, backslashes, controls
      other than horizontal tab, and DEL are rejected. A scope segment must be
      nonempty, must not be [.] or [..], and must contain neither slash nor
      backslash nor an ASCII control byte. Scope segments are already-decoded
      values: a literal [%2F] therefore names a request segment written
      [%252F], while a request [%2F] decodes to the rejected slash segment.
      Pass [[[]]] to protect the whole site. *)

  val mount : at:string list -> 'env t -> 'env t -> 'env t @@ portable
  (** [mount ~at sub site] is [site] with the routes of [sub] added beneath the
      path prefix [at]. Existing routes remain first and may shadow mounted
      routes. Only routes are mounted, and [sub]'s fallback is not.

      It raises [Invalid_argument] if [sub] has been wrapped with {!with_auth}
      or {!with_headers}. Apply wrappers after mounting so they cannot be
      silently discarded, and so that a {!with_auth} scope is not left naming
      paths the mount has moved. *)
end

module Negotiate : sig
  (** This module provides simple response selection from the Accept request
      field. Base media types are matched after parameters other than [q] are
      discarded; a range such as [text/*] or [*/*] matches any type under it.
      See
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-12.5.1}RFC 9110 section
       12.5.1} for the field syntax and preference semantics. *)

  type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]
  (** A [media] is a media type this library can negotiate. [`Other] carries a
      full type such as ["image/png"]. Other media types are matched without
      regard to ASCII case. *)

  val of_accept : string option @ local -> media list @@ portable
  (** [of_accept accept] is the base media types in [accept], most preferred
      first, ordered by q-value with a missing q taken as 1. Media parameters
      other than [q] are discarded. A q-value is the
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-12.4.2}RFC 9110 section
       12.4.2} qvalue: a number from zero to one with at most three decimals.
      Zero-quality ranges are dropped, since the client is saying it will not
      take them, and so is any member spelling its q some other way. Equally
      ranked ranges keep their wire order. It is [[]] when [accept] is absent
      or empty. A type or wildcard this library does not name becomes
      [`Other]. *)

  val v :
    (media * 'env Route.handler) list @ portable ->
    'env Route.handler @ portable
    @@ portable
  (** [v variants] is a handler that invokes the first match in the client's
      preference order, comparing [`Other] strings without regard to ASCII
      case. Each variant takes its quality from its most-specific matching
      range, so an explicit [q=0] refusal overrides a wildcard. A client that
      sends no Accept field gets the first variant, the server's own
      preference. A client that sends one no variant satisfies
      gets 406 Not Acceptable with the available types as a plain-text list,
      as {{:https://www.rfc-editor.org/rfc/rfc9110#section-15.5.7}RFC 9110
       section 15.5.7} allows. The response gains [Vary: Accept]. An empty list
      responds with 404 Not Found. [variants] is taken at [portable] because
      the handler it yields captures it, and a route stores that handler in a
      portable closure. *)

  val select :
    'a Media.t list -> Req.t @ local -> 'a Media.t @@ portable
  (** [select codecs req] is the first codec, in the client's order of
      preference, whose media type falls within an accepted range. It falls
      back to the first codec and raises [Invalid_argument] for an empty list.
      Wildcards are honoured; a [q=0] range refuses a representation when it
      is the most-specific match, even if a wildcard also matches. *)

  val select_opt :
    'a Media.t list -> Req.t @ local -> 'a Media.t option @@ portable
  (** [select_opt codecs req] is {!select}, except that it is [None] rather
      than the first codec when the client stated what it accepts and no codec
      falls within it. {!encode} uses it to answer 406. It raises
      [Invalid_argument] if [codecs] is empty. *)

  val encode :
    ?status:Status.t ->
    ?etag:Etag.t @ local ->
    ?cache:Cache_control.t @ local ->
    ?headers:Headers.t @ local ->
    Resp.respond @ local ->
    Req.t @ local ->
    'a Media.t list ->
    'a ->
    unit
    @@ portable
  (** [encode respond req codecs value] is [()] after responding with [value]
      encoded by {!select_opt}[ codecs req], as {!Resp.encode} does, with
      [Vary: Accept] added to [headers]. A client whose Accept field no codec
      satisfies gets 406 Not Acceptable instead. The codec list is portable
      and may be captured directly by a portable handler. One value, several
      representations:

      {[
        Negotiate.encode respond req
          [ Markdown.html (); Markdown.markdown () ] page
      ]} *)
end

module Static : sig
  (** This module provides static-file serving descriptors for backend authors.

      The shipped backends do not interpret these descriptors directly. *)

  val confine : string list -> string option @@ portable
  (** [confine segs] is [segs] joined with ['/'] when every segment names
      something directly under a root, and [None] otherwise. A segment that is
      empty, ["."] or [".."], or that holds a slash, backslash, or NUL is
      refused. A backend must still resolve the result beneath a directory
      capability because lexical checks cannot detect symlink traversal. *)

  type t : immutable_data
  (** A [t] is a directory label and optional cache policy. A backend resolves
      the label against its filesystem capability. *)

  val v : root:string -> ?cache:Cache_control.t -> unit -> t @@ portable
  (** [v ~root ()] is a static-file description rooted at [root], a name the
      backend resolves. A backend can use {!Mime.of_path} for each file's
      Content-Type and apply [cache] to its response. *)

  val root : t -> string @@ portable
  (** [root t] is the label [t] was built with. *)

  val cache : t -> Cache_control.t option @@ portable
  (** [cache t] is the policy [t] applies to each file, if any. *)
end

(** {1 Caching} *)

module Cache : sig
  (** This module provides a concurrent memoization cache of rendered bodies and
      entity-tags. *)

  type t : value mod portable contended
  (** A [t] is a cache that may be created once at startup and shared by
      handlers. The kind is declared so it stays reachable from a portable
      handler. An abstract type without one reads as contended there, and a
      cache that names only [portable] is unusable from the handlers it exists
      to serve. *)

  val create : ?max_entries:int -> ttl:float -> unit -> t @@ portable
  (** [create ~ttl ()] is an empty cache whose entries live [ttl] seconds and
      which holds at most [max_entries] of them, 1024 by default. A cache is a
      fixed budget rather than a table that grows with whatever keys arrive,
      so a request-derived key cannot exhaust memory. It raises
      [Invalid_argument] unless [ttl] is finite and nonnegative and
      [max_entries] is positive. *)

  val memoize :
    t -> now:float -> key:string -> (unit -> string) -> string * Etag.t
    @@ portable
  (** [memoize t ~now ~key gen] is the cached body under [key] and its
      entity-tag. It calls [gen] when the key is absent or expired at [now],
      measured in seconds from a clock used consistently for every call. [gen]
      runs on the calling domain and is not stored, so it may capture
      domain-bound state. Concurrent misses may call [gen] more than once, and
      one generated value is retained. A miss removes every expired entry, and
      when the cache is already at [max_entries] it evicts the least recently
      used one to make room. It raises [Invalid_argument] unless [now] is
      finite. *)

  val stats : t -> int * int @@ portable
  (** [stats t] is the hit and miss counts since [t] was created. *)
end

(** {1 Backend interface} *)

module Backend : sig
  (** This module provides shared dispatch and response processing for backend
      implementations. Applications normally use a concrete backend or
      [proffer.mock] instead. *)

  (** A [body] is ready for a backend to write. Delayed generators have already
      run, while HEAD and contentless responses use {!Empty}.

      The payloads carry [global], not the block. A socket write needs the
      string and the writer at global. It does not need the block holding them,
      so the block is built in the region and costs nothing. *)
  type body =
    | Empty
    | String of string @@ global
    | Stream of {
        length : int64 option;
        write : (Body.Sink.t -> unit) @@ global;
        trailers : Headers.t;
      }
    | Handoff of {
        kind : Body.handoff_kind;
        run : (Body.Socket.t -> unit) @@ global;
      }

  type outcome = {
    status : Status.t;
    headers : Headers.t;
        (** [headers] is fully rendered, including the entity-tag,
            Cache-Control, and Content-Type. Last-Modified and Content-Length
            are the backend's job. *)
    last_modified : float option;
        (** [last_modified] is the response's Last-Modified time. The backend
            renders it as an IMF-fixdate field after [headers]. *)
    body : body;
    content_length : int64 option;
        (** [content_length] is the declared body length when one applies. HEAD
            and an explicitly described 304 preserve it; a 304 produced by
            conditional request processing has [None]. It is zero for 205 and
            absent for 1xx and 204. [None] otherwise means unknown, which for a
            stream normally means chunked transfer coding. *)
  }
  (** An [outcome] is a response after shared protocol processing and before
      wire encoding. *)

  type writer = outcome @ local -> unit
  (** A [writer] is what a backend gives {!handle} to write one response. It is
      called exactly once per request. *)

  val[@zero_alloc] request :
    meth:Method.t ->
    version:Httpz.Version.t ->
    connection_upgrade:bool ->
    target:string @ local ->
    path:string @ local ->
    query:string @ local ->
    Headers.t @ local ->
    body:string @ local ->
    Req.t @ local
    @@ portable
  (** [request ~meth ~target ~path ~query headers ~body] is the request handed
      to {!handle}, built in the caller's region. Unlike {!Req.v}, every
      component is required, including the version and validated Connection
      upgrade option needed for safe 101 negotiation. Strings and the field
      block arrive at [local]. *)

  val handle :
    ?on_error:(exn -> unit) ->
    ?now:float ->
    'env Site.t ->
    'env ->
    Req.t @ local ->
    writer @ local ->
    unit
    @@ portable
  (** [handle site env req write] is [()] after dispatching [req], applying
      protocol processing that does not require a socket, and calling [write]
      once.

      - The method and the decoded segments select a route. HEAD matches a GET
        route. A path that matches only under other methods gives 405 with an
        Allow field. No route at all gives the site's fallback. An exception
        from a handler goes to [on_error] and gives a plain 500.
      - GET and HEAD responses are checked against request preconditions in the
        order {{:https://www.rfc-editor.org/rfc/rfc9110#section-13.2.2}RFC 9110
         section 13.2.2} fixes:

        + If-Match, compared strongly; [*] matches any current representation,
          and a tag list matches nothing when the response carries no ETag. A
          failure gives 412 Precondition Failed.
        + Otherwise If-Unmodified-Since, against Last-Modified at whole-second
          resolution. A failure gives 412. A date that does not parse is
          ignored.
        + If-None-Match, compared weakly, with the same [*] and list handling.
          A match gives 304 Not Modified.
        + Otherwise If-Modified-Since. A match gives 304. A date that does not
          parse is ignored, and so is one later than [now], which is why [now]
          exists: without it a client can pin 304 responses with a future date.
          Pass the current time in seconds since the epoch, from the same clock
          the responses' Last-Modified values come from.

        A 304 carries only the response's ETag, Last-Modified, Cache-Control,
        Content-Location, Expires, and Vary fields, an empty body, and no
        length, as specified by
        {{:https://www.rfc-editor.org/rfc/rfc9110#section-15.4.5}RFC 9110
         section 15.4.5}. A 412 carries a plain-text body. If-Range is not
        evaluated; this library does not serve ranges.

        For every other method, If-Match, If-None-Match, or a valid singleton
        If-Unmodified-Since is conservatively answered with 412 before the
        handler runs. The generic handler interface does not expose a
        representation's pre-mutation validators, so Proffer declines
        conditional mutation rather than compare post-state or run an effect
        before rejecting it. If-Modified-Since, malformed dates, and repeated
        date fields are ignored.
      - HEAD empties the body and keeps [content_length]. Statuses that cannot
        carry content also empty it without running a stream or generator; 205
        declares zero length, while 1xx and 204 omit framing.
      - A {!Body.Delayed} generator runs once, here, so the outcome of a sent
        body is always [String]. It never runs for HEAD or a contentless status,
        and an exception it raises goes to [on_error] and gives a 500 like any
        other handler failure.

      Handler, delayed-body, and writer exceptions, along with invalid response
      descriptions, are reported to [on_error]. They produce 500 Internal Server
      Error when the writer has not yet been invoked. Returning without
      responding and responding more than once are also reported. [handle] does
      not propagate these errors. *)

  val[@zero_alloc] handle_unboxed :
    on_error:(exn -> unit) ->
    now:float# ->
    'env Site.t ->
    'env ->
    Req.t @ local ->
    writer @ local ->
    unit
    @@ portable
  (** [handle_unboxed ~on_error ~now site env req write] is {!handle} with a
      required error callback and an unboxed time. It is checked free of heap
      allocation outside the callbacks it runs and route captures. *)

  val run :
    ?on_error:(exn -> unit) ->
    ?now:float ->
    Req.t @ local ->
    (Resp.respond @ local -> unit) @ local ->
    writer @ local ->
    unit
    @@ portable
  (** [run req describe write] is [()] after giving [describe] a responder and
      writing its response with the same conditional-request, HEAD,
      delayed-body, and error handling as {!handle}, but without route
      dispatch. A test reaches it through [proffer.mock] to exercise one
      response without a site. *)

  val sink :
    ?emit_sub:(bytes -> int -> int -> unit) ->
    (string -> unit) ->
    Body.Sink.t
    @@ portable
  (** [sink emit] is a stream sink that calls [emit] for strings. When
      [emit_sub] is supplied, byte slices are passed directly to it, otherwise
      {!Body.Sink.write_sub} copies each slice to a string before calling
      [emit]. The fallback is written at the use site rather than made as a
      defaulting closure, so a sink is 3 words either way. *)

  val socket :
    read:(bytes -> int -> int -> int) ->
    write:(bytes -> int -> int -> unit) ->
    shutdown:(unit -> unit) ->
    Body.Socket.t
    @@ portable
  (** [socket] constructs the handoff capability supplied by a concrete
      backend. Application code consumes it through {!Body.Socket}. *)
end
