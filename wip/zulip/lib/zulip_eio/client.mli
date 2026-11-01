(** Zulip REST requests over an injectable Fetch capability.

    Operations return structured API, HTTP, JSON, transport and deadline errors.
    Eio cancellation propagates. Authentication is scoped to the configured
    Zulip origin. GET API requests follow only same-origin redirects. Other API
    methods do not follow redirects. *)

type t
(** The type for Zulip REST clients. *)

type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]
(** The type for supported HTTP request methods. *)

val create :
  ?timeout:float ->
  ?max_body:int ->
  ?allow_insecure:bool ->
  ?user_agent:string ->
  transport:Transport.t ->
  auth:Auth.t ->
  unit ->
  (t, Error.t) result
(** [create ~timeout ~max_body ~allow_insecure ~user_agent ~transport ~auth ()]
    is a client borrowing [transport] and [auth]. [timeout] defaults to 30
    seconds when the transport has a clock. It defaults to no client deadline
    for a clockless transport. An explicit timeout requires a clock and must be
    finite and positive. [max_body] defaults to 16 MiB and bounds JSON and
    error-response bodies. It must be positive and less than [max_int].
    Downloads and request bodies are not subject to this limit.

    [allow_insecure] defaults to [false]. An HTTP site requires [true] before
    credentials can be sent. [user_agent] defaults to [ocaml-zulip/fetch] and
    must be nonempty without control characters. Invalid configuration returns
    [Error.Invalid_request]. Retry and concurrency policy belongs to
    {!Transport.v}. *)

val transport : t -> Transport.t
(** [transport client] is its borrowed transport. *)

val auth : t -> Auth.t
(** [auth client] is its credential set, including the secret API key. *)

val site : t -> string
(** [site client] is its normalized Zulip site URL. *)

val user_agent : t -> string
(** [user_agent client] is the value sent in the User-Agent header. *)

val path_segment : string -> string
(** [path_segment text] is [text] percent-encoded as one URL path segment. Use
    it for a variable segment before constructing an endpoint path. *)

val request :
  t ->
  method_:meth ->
  path:string ->
  ?params:(string * string) list ->
  ?timeout:float ->
  ?longpoll:bool ->
  unit ->
  (Jsont.json, Error.t) result
(** [request client ~method_ ~path ~params ~timeout ~longpoll ()] is the
    successful JSON response from [path]. The path is relative to the API root
    or starts with [/api/v1/]. Query strings and fragments in [path], or paths
    escaping that root, return [Error.Invalid_request]. [params] defaults to
    [[]]. GET places parameters in the query. Other methods send a form body,
    including DELETE. Values are unescaped strings. JSON-valued parameters must
    be serialized once before this call. Query or form escaping is then applied
    once.

    [timeout] defaults to the client's deadline and uses seconds. [longpoll]
    defaults to [false]. When true, the independent polling transport is used.
    An oversized response returns [Error.Http]. Malformed successful JSON
    returns [Error.Json]. *)

val request_typed :
  t ->
  method_:meth ->
  path:string ->
  ?params:(string * string) list ->
  ?timeout:float ->
  ?longpoll:bool ->
  codec:'a Jsont.t ->
  unit ->
  ('a, Error.t) result
(** [request_typed client ~method_ ~path ~params ~timeout ~longpoll ~codec ()]
    is the response decoded with [codec]. Parameters, deadlines, and polling
    defaults are those of {!request}. Codec failures return [Error.Json] with
    their locations. *)

val multipart :
  t ->
  path:string ->
  ?method_:meth ->
  ?params:(string * string) list ->
  ?timeout:float ->
  Fetch.Form.part list ->
  (Jsont.json, Error.t) result
(** [multipart client ~path ~method_ ~params ~timeout parts] is the response to
    a multipart request. [method_] defaults to POST, [params] to [[]], and
    [timeout] to the client's deadline in seconds. Parameters become text fields
    before [parts]. Stream parts remain caller-owned and may be consumed before
    failure. They cannot be replayed without a fresh source and are not closed
    by this call. Invalid multipart metadata returns [Error.Invalid_request]. *)

val request_json :
  t ->
  method_:meth ->
  path:string ->
  ?params:(string * Jsont.json option) list ->
  ?timeout:float ->
  ?longpoll:bool ->
  unit ->
  (Jsont.json, Error.t) result
(** [request_json client ~method_ ~path ~params ~timeout ~longpoll ()] is the
    response with JSON-valued parameter conversion. [params] defaults to [[]].
    [None] omits a parameter, JSON strings supply their text, and other values
    are serialized once. [Some (Jsont.Json.null ())] sends the literal JSON
    value [null]. Deadline and polling defaults are those of {!request}. *)

val download : t -> url:string -> _ Eio.Flow.sink -> (unit, Error.t) result
(** [download client ~url sink] streams a successful response into [sink]. [url]
    must resolve to the configured origin. Redirects to any HTTPS origin are
    allowed, with credentials retained only for the Zulip origin. HTTPS-to-HTTP
    redirects are refused. When insecure HTTP is enabled, redirects from an HTTP
    origin may remain on HTTP. The client deadline applies.

    The caller owns [sink]. It is neither closed nor rewound. Source or sink
    failures return [Error.Transport] and may leave a prefix of the download in
    [sink]. Cancellation also leaves any written prefix and propagates. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf client] formats its site and account identity without the API key.
*)
