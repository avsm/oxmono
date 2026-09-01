(** This module parses [multipart/form-data] bodies.

    A form submitted with the [multipart/form-data] encoding carries each
    control as a separate body part, framed by a delimiter line derived from the
    [boundary] parameter of the Content-Type field. The framing is the MIME
    multipart framing of
    {{:https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.1}RFC 2046,
     Section 5.1.1} and the use of Content-Disposition to name a part is
    {{:https://www.rfc-editor.org/rfc/rfc7578.html}RFC 7578}. An extended
    [filename*] parameter is read as
    {{:https://www.rfc-editor.org/rfc/rfc8187.html}RFC 8187} defines it.

    {!parse} works on a body already held in one string, which is how a server
    that caps a request by size delivers it, and it copies nothing: a part
    records the offset and length of its content within that string. The
    request cap of the enclosing server is therefore the upload limit.

    Line endings must be CRLF, as RFC 2046 requires. A bare LF or CR in the
    framing is rejected rather than repaired, because a parser that accepts both
    disagrees with the next parser in the chain about where a part ends.

    A part whose content type is [multipart/mixed] is not expanded; RFC 7578,
    Section 4.3 deprecates that nesting, so it is left as one part whose
    content type says what it holds. *)

type part = {
  name : string;  (** [name] is the [name] parameter of Content-Disposition. *)
  filename : string option;
      (** [filename] is the [filename*] parameter when present and otherwise the
          [filename] parameter, or [None] when the part is not a file. It is
          untrusted display metadata, not a safe filesystem path: it can
          contain separators, dot segments, or NUL after percent-decoding. *)
  content_type : string option;
      (** [content_type] is the part's Content-Type field value, or [None] when
          it has none. RFC 7578 leaves that to mean [text/plain]. *)
  headers : (string * string) list;
      (** [headers] holds every part header in order, as a lowercased field name
          and its value with surrounding whitespace removed. The fields read
          into the other members appear here too. *)
  off : int;  (** [off] is the first content byte within the parsed body. *)
  len : int;  (** [len] is the content length in bytes. *)
}
(** A [part] is one body part. Its content is not copied; use {!content}. *)

val content : string @ local -> part -> string @@ portable
(** [content body p] is a copy of the content of [p], which must be a part
    {!parse} returned for [body]. *)

val boundary_of_content_type :
  ?media_type:string @ local -> string @ local -> string option @@ portable
(** [boundary_of_content_type ct] is the [boundary] parameter of [ct] when [ct]
    is a [multipart/form-data] content type carrying one. [media_type] selects
    another exact base type and defaults to [multipart/form-data]. The media type is
    compared without case, the parameter name without case, and the value is a
    token or a quoted string with its quoted pairs unescaped. It is [None] for
    another media type, duplicate parameter names, a malformed or unconsumed
    parameter suffix, an absent parameter, or a boundary that is not one to
    seventy RFC 2046 [bchars] not ending in a space. *)

val[@zero_alloc] has_boundary :
  ?media_type:string @ local -> string @ local -> bool @@ portable
(** [has_boundary ct] validates the complete parameter suffix and reports
    whether [ct] has one unambiguous, syntactically valid boundary. It avoids
    copying the boundary and is intended for response validation. Other
    parameter names may repeat because their values do not affect framing. *)

val parse :
  ?max_parts:int -> boundary:string -> string @ local -> (part list, string) result
  @@ portable
(** [parse ~boundary body] is the parts of [body], in order. [max_parts] bounds
    how many parts are accepted and defaults to 256. Zero accepts only a body
    with no parts. A negative value raises [Invalid_argument].

    Any preamble before the first delimiter and any epilogue after the closing
    delimiter are ignored. A part must carry a Content-Disposition field of
    [form-data] with one [name] parameter; duplicate parameter names are
    rejected. A part header line is a field name and value; an obsolete folded
    line, a control byte in a value, more than
    thirty-two fields, or a line above 8 KiB is rejected.

    The error is a short reason, suited to a diagnostic rather than to a client:
    an invalid boundary, no delimiter, a bare LF or CR where CRLF is required, a
    part missing its Content-Disposition, a body truncated before the closing
    delimiter, or more than [max_parts] parts. *)
