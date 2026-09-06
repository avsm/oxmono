(** This module represents recognized HTTP field names.

    Known names have dedicated variants for fast matching. {!Other} represents an
    extension or unrecognized name; its spelling remains available through
    {!Header.name_span}. Field names are matched case-insensitively as required by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-5.1} RFC 9110, Section 5.1}. *)

(** A [t] is an HTTP field name recognized by Httpz. *)
type t =
  | Cache_control
  | Connection
  | Date
  | Transfer_encoding
  | Trailer
  | Te (** [Te] means TE. *)
  | Upgrade
  | Via
  | Accept
  | Accept_charset
  | Accept_encoding
  | Accept_language
  | Accept_ranges
  | Authorization
  | Proxy_authorization
  | Proxy_authenticate
  | Cookie
  | Expect
  | Host
  | Max_forwards
  | If_match
  | If_modified_since
  | If_none_match
  | If_range
  | If_unmodified_since
  | Range
  | Referer
  | User_agent
  | Age
  | Etag (** [Etag] means ETag. *)
  | Location
  | Retry_after
  | Server
  | Set_cookie
  | Www_authenticate (** [Www_authenticate] means WWW-Authenticate. *)
  | Allow
  | Content_disposition
  | Content_encoding
  | Content_language
  | Content_length
  | Content_location
  | Content_range
  | Content_type
  | Expires
  | Last_modified
  | X_forwarded_for
  | X_forwarded_proto
  | X_forwarded_host
  | X_request_id
  | Vary
  | X_correlation_id
  | X_cache
  | Depth (** [Depth] means WebDAV Depth. *)
  | Destination (** [Destination] means WebDAV Destination. *)
  | Overwrite (** [Overwrite] means WebDAV Overwrite. *)
  | Lock_token (** [Lock_token] means WebDAV Lock-Token. *)
  | Dav (** [Dav] means WebDAV DAV. *)
  | If (** [If] means WebDAV If. *)
  | Access_control_allow_origin
  | Access_control_allow_methods
  | Access_control_allow_headers
  | Other (** [Other] means an unrecognized field name. *)

(** [canonical name] is the conventional title-cased spelling of [name], or ["(unknown)"]
    for {!Other}. *)
val canonical : t -> string @@ portable

(** [equal a b] is [true] when [a] and [b] are the same field name. It is an immediate
    comparison, and {!Other} equals {!Other} regardless of the wire spellings the two
    names were parsed from. *)
val equal : t -> t -> bool @@ portable

(** [of_span buf span] is the known name matching [span], ignoring ASCII case, or {!Other}
    when no variant matches. *)
val of_span : local_ bytes -> Span.t -> t @@ portable

(** [pp formatter name] is the formatter operation that prints the canonical spelling of
    [name]. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable
