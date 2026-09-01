(** This module provides HTTP request-target parsing and matching.

    A target is classified into one of the four forms defined by
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-3.2}RFC 9112, Section
     3.2}. URI components are validated against
    {{:https://www.rfc-editor.org/rfc/rfc3986.html}RFC 3986} and returned as
    borrowed spans. Percent-encoded bytes remain encoded.

    {!parse} classifies syntax only. {!Httpz.parse} additionally enforces which
    target forms are allowed for each request method.

    Functions that may fail return unboxed tuples [#(success, result)] rather
    than an option, so that no result is heap allocated. *)

(** A [form] is the wire form of a request-target. *)
type form =
  | Origin
      (** [Origin] means a path and optional query, such as [/items?page=2]. *)
  | Absolute  (** [Absolute] means an absolute URI, normally sent to a proxy. *)
  | Authority
      (** [Authority] means a host and required port, used by CONNECT. *)
  | Asterisk
      (** [Asterisk] means the literal [*], used by server-wide OPTIONS. *)
  | Invalid
      (** [Invalid] means input that is not an accepted request-target. *)

type t =
  #{ form : form  (** [form] is the request-target form. *)
   ; path : Span.t  (** [path] is the path, including any leading slash. *)
   ; query : Span.t  (** [query] is the query without [?]. *)
   ; scheme : Span.t  (** [scheme] is the absolute-form scheme without [:]. *)
   ; host : Span.t
       (** [host] is the host; brackets around an IP literal are excluded. *)
   ; port : int  (** [port] is the numeric port, or [-1] when absent. *)
   ; err : int
       (** [err] is the first invalid buffer offset, or [-1] when valid. *)
   }
(** A [t] is a classified request-target. Components not present in the selected
    form are empty spans. *)

val[@zero_alloc opt] parse : local_ bytes -> Span.t -> t @@ portable
(** [parse buf target] is [target] classified and split into its components.
    Invalid input produces [form = Invalid] and an error offset rather than
    raising. Absolute-form requires a non-empty authority and rejects userinfo,
    fragments, and ports above 65535. Authority-form requires a non-empty host
    and decimal port in the range 0 through 65535. *)

val form : t -> form @@ portable
(** [form target] is [target]'s request-target form. *)

val path : t -> Span.t @@ portable
(** [path target] is [target]'s path span. *)

val query : t -> Span.t @@ portable
(** [query target] is [target]'s query without [?]. Absence and an explicitly
    empty query both produce an empty span. *)

val scheme : t -> Span.t @@ portable
(** [scheme target] is the absolute-form scheme without [:], or an empty span.
*)

val host : t -> Span.t @@ portable
(** [host target] is the host without IP-literal brackets, or an empty span. *)

val port : t -> int @@ portable
(** [port target] is the explicit numeric port, or [-1]. A scheme does not imply
    its default port. *)

val is_valid : t -> bool @@ portable
(** [is_valid target] is [false] exactly when [target.form = Invalid]. *)

val has_query : t -> bool @@ portable
(** [has_query target] is [true] when [target] has a non-empty query. It does
    not distinguish no [?] from a trailing empty [?]. *)

val is_absolute : t -> bool @@ portable
(** [is_absolute target] is [true] exactly when [target.form = Absolute]. *)

val error_offset : t -> int @@ portable
(** [error_offset target] is the absolute buffer offset of the first invalid
    byte, or [-1] for a valid target. *)

val valid_host : local_ bytes -> Span.t -> bool @@ portable
(** [valid_host buf value] is [true] when [value] is a non-empty [uri-host] with
    an optional non-empty decimal port from 0 through 65535. It rejects an empty
    host, userinfo, and commas, so a Host field cannot name nothing and cannot
    be interpreted as a list. *)

val authority_matches : local_ bytes -> t -> Span.t -> bool @@ portable
(** [authority_matches buf target value] is [true] when the Host field [value]
    names the same authority as [target]. The host is compared ignoring ASCII
    case and the port exactly. A port absent on one side and present on the
    other does not match, and a scheme default port is never supplied. It is
    meaningful only for an absolute-form [target]. *)

val match_segment :
  local_ bytes -> Span.t -> string -> #(bool * Span.t) @@ portable
(** [match_segment buf path expected] is [(true, rest)] when the first
    slash-delimited segment equals [expected], where [rest] begins after the
    following slash. On failure, the second value is a placeholder. A leading
    slash therefore represents an initial empty segment and is not stripped
    automatically. *)

val match_param :
  local_ bytes -> Span.t -> #(bool * Span.t * Span.t) @@ portable
(** [match_param buf path] is [(true, segment, rest)] for the first
    slash-delimited segment of a non-empty [path]. For an empty [path], both
    spans are placeholders. A leading slash produces an empty first segment. *)

val is_empty : Span.t -> bool @@ portable
(** [is_empty path] is [true] when [path] has length zero. *)

val find_query_param :
  local_ bytes -> Span.t -> string -> #(bool * Span.t) @@ portable
(** [find_query_param buf query name] is [(true, value)] for the first
    case-sensitive key equal to [name]. It is [(false, placeholder)] when no key
    matches. Keys and values remain percent-encoded; a missing value and an
    empty value both produce an empty span. *)

val fold_query_params :
  local_ bytes
  -> Span.t
  -> init:'a
  -> f:('a -> Span.t -> Span.t -> 'a)
  -> 'a
  @@ portable
(** [fold_query_params buf query ~init ~f] is [f] folded over key-value spans in
    arrival order. Parameters are separated at [&] and at their first [=]. An
    empty query has no parameters; consecutive or trailing [&] delimiters
    produce empty keys. *)

val query_to_string_pairs :
  local_ bytes -> Span.t -> (string * string) list @@ portable
(** [query_to_string_pairs buf query] is the query parameters copied into
    strings in arrival order. It allocates one string for each key and value. *)
