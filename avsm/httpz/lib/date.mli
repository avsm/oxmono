(** HTTP-date parsing and formatting per RFC 7231 Section 7.1.1.1.

    HTTP uses a restricted subset of date formats. This module parses all
    three accepted formats and generates the preferred IMF-fixdate format.

    {2 Accepted Formats}

    - IMF-fixdate: [Sun, 06 Nov 1994 08:49:37 GMT] (preferred)
    - RFC 850: [Sunday, 06-Nov-94 08:49:37 GMT] (obsolete)
    - ANSI C asctime: [Sun Nov  6 08:49:37 1994] (obsolete)

    {2 Usage}

    {[
      (* Parse Last-Modified or If-Modified-Since header *)
      let #(status, timestamp) = Date.parse buf header_value_span in
      match status with
      | Date.Valid -> (* timestamp is valid float# *)
      | Date.Invalid -> (* Invalid date format *)

      (* Write Date header in response *)
      let off = Date.write_date_header buf ~off timestamp in
    ]}

    @see <https://datatracker.ietf.org/doc/html/rfc7231#section-7.1.1.1> RFC 7231 Section 7.1.1.1 *)

(** {1 Types} *)

(** Parse status. *)
type status =
  | Valid    (** Successfully parsed *)
  | Invalid  (** Invalid date format *)

(** {1 Parsing} *)

(** Parse HTTP-date from span.
    Accepts all three formats (IMF-fixdate, RFC 850, asctime).
    Returns (status, timestamp) where timestamp is Unix seconds since epoch.
    Only valid if status = Valid. *)
val parse : local_ bytes -> Span.t -> #(status * float#)

(** {1 Formatting} *)

(** Format Unix timestamp as IMF-fixdate string (allocates).
    Example: ["Sun, 06 Nov 1994 08:49:37 GMT"] *)
val format : float# -> string

(** {1 Response Writing} *)

(** Write [Date: <timestamp>\r\n] header. Returns new offset. *)
val write_date_header : bytes -> off:int16# -> float# -> int16#

(** Write [Last-Modified: <timestamp>\r\n] header. Returns new offset. *)
val write_last_modified : bytes -> off:int16# -> float# -> int16#

(** Write [Expires: <timestamp>\r\n] header. Returns new offset. *)
val write_expires : bytes -> off:int16# -> float# -> int16#

(** Write formatted HTTP-date at offset (no header name, no CRLF).
    Returns new offset. Used internally by header writers. *)
val write_http_date : bytes -> off:int16# -> float# -> int16#

(** {2 Comparison Helpers} *)

(** Check if resource was modified since the given date.
    [is_modified_since ~last_modified ~if_modified_since] returns [true]
    if the resource has been modified after the if_modified_since date. *)
val is_modified_since : last_modified:float# -> if_modified_since:float# -> bool

(** Check if resource was not modified since the given date.
    [is_unmodified_since ~last_modified ~if_unmodified_since] returns [true]
    if the resource has not been modified after the if_unmodified_since date. *)
val is_unmodified_since : last_modified:float# -> if_unmodified_since:float# -> bool
