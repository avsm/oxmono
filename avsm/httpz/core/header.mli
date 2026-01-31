(** HTTP header representation.

    Headers are stored as records with a typed {!Name.t} discriminant and
    span references into the parse buffer. This enables fast lookups for
    known headers while preserving custom header names.

    {2 Known vs Unknown Headers}

    For known headers (Content-Type, Accept, etc.), the [name] field contains
    the specific variant and [name_span] can be ignored:

    {[
      match hdr.name with
      | Name.Content_type -> handle_content_type buf hdr.value
      | Name.Accept -> handle_accept buf hdr.value
      | _ -> ()
    ]}

    For custom headers ([Name.Other]), use [name_span] to check the name:

    {[
      if hdr.name = Name.Other
         && Span.equal_caseless buf hdr.name_span "x-request-id"
      then
        let request_id = Span.to_string buf hdr.value in
        ...
    ]} *)

(** Header name enumeration. See {!Header_name} for details. *)
module Name = Header_name

(** {1 Types} *)

type t =
  { name : Name.t
      (** Parsed header name (known headers) or {!Name.Other} *)
  ; name_span : Span.t
      (** Span of header name in buffer. Meaningful only when [name = Other]. *)
  ; value : Span.t
      (** Span of header value in buffer (trimmed of leading/trailing whitespace). *)
  }
(** Parsed HTTP header.

    Headers are stored in a local list during parsing, enabling stack
    allocation with no heap overhead. *)

(** {1 Lookup} *)

val find : t list @ local -> Name.t -> t option @ local
(** [find headers name] returns the first header with the given name.

    Only matches known headers (not {!Name.Other}). For custom headers,
    use {!find_string}.

    {[
      match Header.find headers Name.Content_type with
      | Some hdr -> Span.to_string buf hdr.value
      | None -> "application/octet-stream"
    ]} *)

val find_string : bytes -> t list @ local -> string -> t option @ local
(** [find_string buf headers name] finds a header by string name.

    Case-insensitive comparison. Works for both known and custom headers.

    {[
      match Header.find_string buf headers "x-request-id" with
      | Some hdr -> Some (Span.to_string buf hdr.value)
      | None -> None
    ]} *)

(** {1 Conversion} *)

val to_string_pair : bytes -> t -> string * string
(** [to_string_pair buf hdr] converts a header to [(name, value)] strings.

    Allocates two strings. *)

val to_string_pairs : bytes -> t list -> (string * string) list
(** [to_string_pairs buf headers] converts all headers to string pairs.

    Allocates a list and strings for each header. *)

(** {1 Pretty Printing} *)

val pp : Stdlib.Format.formatter -> t -> unit
(** [pp fmt hdr] prints the header structure (name variant, spans). *)

val pp_with_buf : bytes -> Stdlib.Format.formatter -> t -> unit
(** [pp_with_buf buf fmt hdr] prints the header with actual name and value. *)
