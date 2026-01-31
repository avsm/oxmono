(** HTTP header name enumeration.

    Common HTTP headers are represented as enum variants for fast pattern
    matching without string comparison. Unknown headers use the {!Other}
    variant, with the actual name stored in the header's [name_span] field.

    {2 Usage}

    {[
      match hdr.Header.name with
      | Header_name.Content_type -> (* handle content type *)
      | Header_name.Accept -> (* handle accept *)
      | Header_name.Other ->
        (* Check name_span for custom headers *)
        if Span.equal_caseless buf hdr.name_span "x-custom" then ...
      | _ -> ()
    ]} *)

(** {1 Types} *)

type t =
  | Cache_control      (** Cache-Control *)
  | Connection         (** Connection *)
  | Date               (** Date *)
  | Transfer_encoding  (** Transfer-Encoding *)
  | Upgrade            (** Upgrade *)
  | Via                (** Via *)
  | Accept             (** Accept *)
  | Accept_charset     (** Accept-Charset *)
  | Accept_encoding    (** Accept-Encoding *)
  | Accept_language    (** Accept-Language *)
  | Accept_ranges      (** Accept-Ranges *)
  | Authorization      (** Authorization *)
  | Cookie             (** Cookie *)
  | Expect             (** Expect *)
  | Host               (** Host *)
  | If_match           (** If-Match *)
  | If_modified_since  (** If-Modified-Since *)
  | If_none_match      (** If-None-Match *)
  | If_range           (** If-Range *)
  | If_unmodified_since (** If-Unmodified-Since *)
  | Range              (** Range *)
  | Referer            (** Referer *)
  | User_agent         (** User-Agent *)
  | Age                (** Age *)
  | Etag               (** ETag *)
  | Location           (** Location *)
  | Retry_after        (** Retry-After *)
  | Server             (** Server *)
  | Set_cookie         (** Set-Cookie *)
  | Www_authenticate   (** WWW-Authenticate *)
  | Allow              (** Allow *)
  | Content_disposition (** Content-Disposition *)
  | Content_encoding   (** Content-Encoding *)
  | Content_language   (** Content-Language *)
  | Content_length     (** Content-Length *)
  | Content_location   (** Content-Location *)
  | Content_range      (** Content-Range *)
  | Content_type       (** Content-Type *)
  | Expires            (** Expires *)
  | Last_modified      (** Last-Modified *)
  | X_forwarded_for    (** X-Forwarded-For *)
  | X_forwarded_proto  (** X-Forwarded-Proto *)
  | X_forwarded_host   (** X-Forwarded-Host *)
  | X_request_id       (** X-Request-Id *)
  | X_correlation_id   (** X-Correlation-Id *)
  | Other
      (** Unknown header. Check [Header.name_span] for the actual name. *)
(** HTTP header name. *)

(** {1 String Conversion} *)

val canonical : t -> string
(** [canonical name] returns the canonical display name.

    Returns the properly-cased header name for known headers
    (e.g., "Content-Type", "Accept-Encoding").
    Returns ["(unknown)"] for {!Other}.

    {[
      Header_name.canonical Content_type  (* "Content-Type" *)
      Header_name.canonical Other         (* "(unknown)" *)
    ]} *)

val lowercase : t -> string
(** [lowercase name] returns the lowercase header name.

    Returns [""] for {!Other}. Useful for HTTP/2 where headers are lowercase. *)

(** {1 Parsing} *)

val of_span : local_ bytes -> Span.t -> t
(** [of_span buf span] parses a header name from a buffer span.

    Returns the matching variant for known headers, or {!Other} for
    unrecognized names. Case-insensitive matching. *)

(** {1 Pretty Printing} *)

val pp : Stdlib.Format.formatter -> t -> unit
(** Pretty-print header name. *)
