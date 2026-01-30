(** HTTP header name enumeration. *)

type t =
  | Cache_control
  | Connection
  | Date
  | Transfer_encoding
  | Upgrade
  | Via
  | Accept
  | Accept_charset
  | Accept_encoding
  | Accept_language
  | Accept_ranges
  | Authorization
  | Cookie
  | Expect
  | Host
  | If_match
  | If_modified_since
  | If_none_match
  | If_range
  | If_unmodified_since
  | Range
  | Referer
  | User_agent
  | Age
  | Etag
  | Location
  | Retry_after
  | Server
  | Set_cookie
  | Www_authenticate
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
  | X_correlation_id
  | Other
  (** [Other] indicates an unknown header; the actual name is stored in the header's
      [name_span] field. *)

(** Canonical display name for known headers. Returns ["(unknown)"] for [Other].
    Use this for response writing when you don't need buffer access. *)
val canonical : t -> string

(** Lowercase canonical name for known headers. Returns [""] for [Other]. *)
val lowercase : t -> string

(** Parse header name from span. *)
val of_span : local_ Base_bigstring.t -> Span.t -> t

(** Pretty-print header name. *)
val pp : Stdlib.Format.formatter -> t -> unit
