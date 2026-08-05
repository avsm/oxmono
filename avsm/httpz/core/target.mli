(** Request-target parsing.

    A request-target is validated against the
    {{:https://datatracker.ietf.org/doc/html/rfc3986}RFC 3986} grammar and
    split into components, in place. Nothing is copied and nothing is decoded:
    every component is a {!Span.t} into the parse buffer, and percent-encodings
    are left as they stand.

    The path span {b includes} the leading slash: ["/foo/bar?a=1"] yields a
    path of ["/foo/bar"].

    Functions that may fail return unboxed tuples [#(success, result)] instead
    of [option] to avoid allocation. *)

type form =
  | Origin     (** ["/where?q=now"] *)
  | Absolute   (** ["http://host:8080/where?q=now"], sent to proxies *)
  | Authority  (** ["host:8080"], CONNECT only *)
  | Asterisk   (** ["*"], OPTIONS only *)
  | Invalid    (** not a request-target; see {!error_offset} *)

(** Parsed target. Components absent from the target are zero-length spans;
    use {!form} to tell them apart from present-but-empty ones. *)
type t =
  #{ form : form
   ; path : Span.t   (** including the leading slash; empty unless [Origin] or [Absolute] *)
   ; query : Span.t  (** excluding the ['?'] *)
   ; scheme : Span.t (** [Absolute] only, excluding the [':'] *)
   ; host : Span.t   (** [Absolute] and [Authority]; an IP-literal excludes its brackets *)
   ; port : int      (** [-1] when absent *)
   ; err : int       (** offset of the first invalid byte, [-1] when valid *)
   }

val parse : local_ bytes -> Span.t -> t @@ portable
(** [parse buf target] classifies and splits [target]. Allocation-free.

    An invalid target yields [form = Invalid] rather than an exception; it is
    for the caller to turn that into a 400. {!Httpz.parse} does so already, so
    a target reaching a handler has been validated.

    Beyond the RFC 3986 grammar, a target is rejected if it carries a fragment
    or a userinfo, neither of which RFC 9112 permits, or if an authority-form
    has no port. Absolute-form requires a non-empty authority, so ["host:8080"]
    reads as authority-form and ["http:/x"] is rejected outright. *)

val form : t -> form @@ portable
val path : t -> Span.t @@ portable
val query : t -> Span.t @@ portable
val scheme : t -> Span.t @@ portable
val host : t -> Span.t @@ portable

val port : t -> int @@ portable
(** [-1] when the target names no port. An absolute-form target does not
    imply the scheme's default. *)

val is_valid : t -> bool @@ portable
val has_query : t -> bool @@ portable

val error_offset : t -> int @@ portable
(** Offset into the parse buffer of the first byte that failed validation, or
    [-1] when the target is valid. *)

(** {1 Path Segment Matching}

    Match path segments against expected patterns without allocation.
    Functions return unboxed tuples to avoid option allocation. *)

val match_segment : local_ bytes -> Span.t -> string -> #(bool * Span.t) @@ portable
(** [match_segment buf path expected] is [#(matched, rest)], where [rest] is
    [path] past the first segment and its separator. [rest] is undefined when
    [matched] is [false]. *)

val match_param : local_ bytes -> Span.t -> #(bool * Span.t * Span.t) @@ portable
(** [match_param buf path] is [#(matched, segment, rest)], capturing the first
    segment whatever it is. Both spans are undefined when [matched] is
    [false]. *)

val is_empty : Span.t -> bool @@ portable
(** Whether a path span is exhausted, i.e. the match was complete. *)

(** {1 Query Parameter Lookup}

    A parameter runs to the next ['&'] or to the end of the query, and its
    first ['='] splits key from value. A parameter with no ['='] has a
    zero-length value span, indistinguishable from ["k="]; a trailing ['&']
    introduces one final parameter with an empty key. Neither key nor value is
    percent-decoded. *)

val find_query_param : local_ bytes -> Span.t -> string -> #(bool * Span.t) @@ portable
(** [find_query_param buf query name] is [#(found, value)] for the first
    parameter whose key is exactly [name]. [value] is undefined when [found]
    is [false]. *)

val fold_query_params :
  local_ bytes ->
  Span.t ->
  init:'a ->
  f:('a -> Span.t -> Span.t -> 'a) ->
  'a @@ portable
(** [fold_query_params buf query ~init ~f] applies [f acc key value] to each
    parameter in order. *)

val query_to_string_pairs : local_ bytes -> Span.t -> (string * string) list @@ portable
(** Allocates one string per key and per value. *)
