(** This module represents recognized HTTP field names.

    Known names have dedicated variants for fast matching. {!Other} represents an
    extension or unrecognized name; its spelling remains available through
    {!Header.name_span}. Field names are matched case-insensitively as required by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-5.1} RFC 9110, Section 5.1}. *)

(** A [t] is an HTTP field name recognized by Httpz. *)
type t =
  | Cache_control (** [Cache_control] means Cache-Control. *)
  | Connection (** [Connection] means Connection. *)
  | Date (** [Date] means Date. *)
  | Transfer_encoding (** [Transfer_encoding] means Transfer-Encoding. *)
  | Trailer (** [Trailer] means Trailer. *)
  | Te (** [Te] means TE. *)
  | Upgrade (** [Upgrade] means Upgrade. *)
  | Via (** [Via] means Via. *)
  | Accept (** [Accept] means Accept. *)
  | Accept_charset (** [Accept_charset] means Accept-Charset. *)
  | Accept_encoding (** [Accept_encoding] means Accept-Encoding. *)
  | Accept_language (** [Accept_language] means Accept-Language. *)
  | Accept_ranges (** [Accept_ranges] means Accept-Ranges. *)
  | Authorization (** [Authorization] means Authorization. *)
  | Proxy_authorization (** [Proxy_authorization] means Proxy-Authorization. *)
  | Proxy_authenticate (** [Proxy_authenticate] means Proxy-Authenticate. *)
  | Cookie (** [Cookie] means Cookie. *)
  | Expect (** [Expect] means Expect. *)
  | Host (** [Host] means Host. *)
  | Max_forwards (** [Max_forwards] means Max-Forwards. *)
  | If_match (** [If_match] means If-Match. *)
  | If_modified_since (** [If_modified_since] means If-Modified-Since. *)
  | If_none_match (** [If_none_match] means If-None-Match. *)
  | If_range (** [If_range] means If-Range. *)
  | If_unmodified_since (** [If_unmodified_since] means If-Unmodified-Since. *)
  | Range (** [Range] means Range. *)
  | Referer (** [Referer] means Referer. *)
  | User_agent (** [User_agent] means User-Agent. *)
  | Age (** [Age] means Age. *)
  | Etag (** [Etag] means ETag. *)
  | Location (** [Location] means Location. *)
  | Retry_after (** [Retry_after] means Retry-After. *)
  | Server (** [Server] means Server. *)
  | Set_cookie (** [Set_cookie] means Set-Cookie. *)
  | Www_authenticate (** [Www_authenticate] means WWW-Authenticate. *)
  | Allow (** [Allow] means Allow. *)
  | Content_disposition (** [Content_disposition] means Content-Disposition. *)
  | Content_encoding (** [Content_encoding] means Content-Encoding. *)
  | Content_language (** [Content_language] means Content-Language. *)
  | Content_length (** [Content_length] means Content-Length. *)
  | Content_location (** [Content_location] means Content-Location. *)
  | Content_range (** [Content_range] means Content-Range. *)
  | Content_type (** [Content_type] means Content-Type. *)
  | Expires (** [Expires] means Expires. *)
  | Last_modified (** [Last_modified] means Last-Modified. *)
  | X_forwarded_for (** [X_forwarded_for] means X-Forwarded-For. *)
  | X_forwarded_proto (** [X_forwarded_proto] means X-Forwarded-Proto. *)
  | X_forwarded_host (** [X_forwarded_host] means X-Forwarded-Host. *)
  | X_request_id (** [X_request_id] means X-Request-Id. *)
  | Vary (** [Vary] means Vary. *)
  | X_correlation_id (** [X_correlation_id] means X-Correlation-Id. *)
  | X_cache (** [X_cache] means X-Cache. *)
  | Depth (** [Depth] means WebDAV Depth. *)
  | Destination (** [Destination] means WebDAV Destination. *)
  | Overwrite (** [Overwrite] means WebDAV Overwrite. *)
  | Lock_token (** [Lock_token] means WebDAV Lock-Token. *)
  | Dav (** [Dav] means WebDAV DAV. *)
  | If (** [If] means WebDAV If. *)
  | Access_control_allow_origin
  (** [Access_control_allow_origin] means Access-Control-Allow-Origin. *)
  | Access_control_allow_methods
  (** [Access_control_allow_methods] means Access-Control-Allow-Methods. *)
  | Access_control_allow_headers
  (** [Access_control_allow_headers] means Access-Control-Allow-Headers. *)
  | Other (** [Other] means an unrecognized field name. *)

(** [canonical name] is the conventional title-cased spelling of [name], or ["(unknown)"]
    for {!Other}. *)
val canonical : t -> string @@ portable

(** [of_span buf span] is the known name matching [span], ignoring ASCII case, or {!Other}
    when no variant matches. *)
val of_span : local_ bytes -> Span.t -> t @@ portable

(** [pp formatter name] is the formatter operation that prints the canonical spelling of
    [name]. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable
