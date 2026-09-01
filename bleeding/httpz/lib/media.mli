(** This module provides typed media codecs.

    A codec pairs a media type with an encoder and a decoder for one OCaml
    type, so that a request or response body is built from, and read back as,
    a typed value. [fetch] and [proffer] both accept codecs wherever they
    otherwise take a content type and a string.

    A codec reads from and writes to the streams of [Bytesrw], which is what
    streaming decoders such as Jsont consume directly. A codec whose format
    is handled as a string is built with {!of_strings}.

    {[
    type todo = { id : int; title : string }

    let todo : todo Media.t =
      Media.of_strings "application/json"
        ~encode:(fun t -> Printf.sprintf {|{"id":%d,"title":%S}|} t.id t.title)
        ~decode:(fun s -> parse_todo s)
    ]}

    {!Httpz.Json} provides bounded Jsont and JSON Lines codecs. The main
    [fetch] and [proffer] libraries expose it as their [Json] module alongside
    CommonMark and HTML adapters. *)

(** {1 Errors} *)

module Loc : sig
  type t : immutable_data =
    { first_byte : int
    ; last_byte : int
    ; first_line : int
    ; first_col : int
    ; last_line : int
    ; last_col : int
    }
  (** A [t] locates an inclusive byte range in a body. Byte offsets are
      zero-based; lines and columns are one-based. *)

  (** [pp ppf loc] formats [loc] for a diagnostic. *)
  val pp : Format.formatter -> t -> unit @@ portable

  (** [v ~first_byte ~last_byte ~first_line ~last_line] builds a location from
      byte offsets and [(line_number, line_start_byte)] positions. It derives
      one-based byte columns from the line starts. *)
  val v
    :  first_byte:int
    -> last_byte:int
    -> first_line:int * int
    -> last_line:int * int
    -> t
    @@ portable
end

type detail = ..
(** A [detail] preserves a codec-specific decoding error. *)

type detail += No_detail
(** [No_detail] means the decoder supplied only a message. *)

type malformed =
  { message : string
  ; loc : Loc.t option
  ; detail : detail
  }
(** A [malformed] body has a diagnostic [message], an optional location, and
    codec-specific [detail]. *)

type error =
  | Unsupported of string option
  (** [Unsupported ct] means the body carried the media type [ct], or none
          when [ct] is [None], and the codec decodes neither. *)
  | Malformed of malformed
  (** [Malformed error] means the body has the right media type but the
          decoder rejected it with [error]. *)
  | Too_large of int
  (** [Too_large limit] means the body exceeded [limit] bytes before it
          could be decoded. *)

(** An [error] is why a body could not be decoded. *)

(** [pp_error ppf e] formats [e] as a sentence. *)
val pp_error : Format.formatter -> error -> unit @@ portable

(** [error_to_string e] is [e] formatted by {!pp_error}. *)
val error_to_string : error -> string @@ portable

(** [sanitize_diagnostic s] preserves printable text while replacing C0,
    DEL, and C1 control characters with visible ASCII escape notation. It is
    suitable for an untrusted decoder diagnostic that may reach a log or
    terminal. Well-formed UTF-8 remains well formed. *)
val sanitize_diagnostic : string -> string @@ portable

(** [malformed message] constructs a malformed-body diagnostic. [detail]
    defaults to {!No_detail}. *)
val malformed : ?loc:Loc.t -> ?detail:detail -> string -> malformed @@ portable

(** {1 Codecs} *)

(** An ['a t] is a portable codec between a media type and values of type
    ['a]. Its encoder and decoder closures must themselves be portable, so a
    codec can be defined once and captured directly by portable handlers. *)
type 'a t : value mod portable contended

(** [v media ~encode ~decode] is a codec for the media type [media], written as
    [type/subtype] such as ["application/json"]. Case is folded.

    [encode v w] writes the encoding of [v] to [w] and must not write
    [Bytesrw.Bytes.Slice.eod]. It should raise [Invalid_argument] for a value
    it cannot represent. [decode s] decodes one complete string and returns a
    message for a body it rejects. If [decode_reader] is supplied, both
    {!decode_reader} and {!decode} use it to preserve a structured error;
    otherwise a reader is buffered and passed to [decode].

    [accept] lists further media types the decoder accepts, in addition to
    [media]. An entry may be a type such as ["text/x-markdown"], a range such
    as ["text/*"] or ["*/*"], or a structured-suffix range such as
    ["application/*+json"].

    [params] are sent with the media type in a Content-Type field, as in
    [[ ("charset", "utf-8") ]]. Token values are emitted directly; other
    values are quoted with quote and backslash escaping. They play no part in
    matching.

    It raises [Invalid_argument] if [media] or an [accept] entry is not a
    media type or range, if a parameter name is not a token, or if a parameter
    value contains a control byte. *)
val v
  :  ?accept:string list
  -> ?params:(string * string) list
  -> ?decode_reader:(Bytesrw.Bytes.Reader.t -> ('a, malformed) result) @ portable
  -> string
  -> encode:('a -> Bytesrw.Bytes.Writer.t -> unit) @ portable
  -> decode:(string -> ('a, string) result) @ portable
  -> 'a t
  @@ portable

(** [v_reader media ~encode ~decode] is a streaming codec whose decoder
    directly returns the structured {!type-malformed} error. String decoding
    wraps its input in a reader and uses the same function, so no redundant
    string decoder is required. Media and parameter validation, and the
    encoder's contract, are as in {!v}. *)
val v_reader
  :  ?accept:string list
  -> ?params:(string * string) list
  -> string
  -> encode:('a -> Bytesrw.Bytes.Writer.t -> unit) @ portable
  -> decode:(Bytesrw.Bytes.Reader.t -> ('a, malformed) result) @ portable
  -> 'a t
  @@ portable

(** [of_strings media ~encode ~decode] is {!v} for a format handled as a
    string. The body is buffered before [decode] sees it. *)
val of_strings
  :  ?accept:string list
  -> ?params:(string * string) list
  -> string
  -> encode:('a -> string) @ portable
  -> decode:(string -> ('a, string) result) @ portable
  -> 'a t
  @@ portable

(** [encoder media f] is a codec that only encodes, with [f]. It is suited to
    a rendered representation such as HTML produced from a document.
    Decoding with it raises [Invalid_argument]. *)
val encoder
  :  ?params:(string * string) list
  -> string
  -> ('a -> string) @ portable
  -> 'a t
  @@ portable

(** [decoder media f] is a codec that only decodes, with [f]. Encoding with it
    raises [Invalid_argument]. *)
val decoder
  :  ?accept:string list
  -> string
  -> (string -> ('a, string) result) @ portable
  -> 'a t
  @@ portable

(** [map ~decode ~encode t] is [t] carried to another type. [decode] may
    reject a value with a message, which becomes {!Malformed}. *)
val map
  :  decode:('a -> ('b, string) result) @ portable
  -> encode:('b -> 'a) @ portable
  -> 'a t
  -> 'b t
  @@ portable

(** [media_type t] is the media type, as [type/subtype] in lowercase. *)
val[@zero_alloc] media_type : _ t -> string @@ portable

(** [content_type t] is the Content-Type field value: the media type followed
    by any parameters. *)
val content_type : _ t -> string @@ portable

(** [accepts t ct] is whether [t] decodes a body whose Content-Type is [ct].
    Parameters in [ct] are ignored and case is folded. An absent Content-Type
    is accepted only by a codec whose [accept] list holds ["*/*"]. *)
val[@zero_alloc] accepts : _ t -> string option @ local -> bool @@ portable

module Syntax : sig
  (** Media-type and media-range syntax shared by field codecs and negotiation.
      Slice lengths count bytes, and bounds are checked without copying the
      input. Only SP and HTAB are accepted as surrounding whitespace. *)

  val[@zero_alloc] valid_type : local_ string -> pos:int -> len:int -> bool @@ portable
  (** [valid_type s ~pos ~len] checks a type/subtype without parameters or
      wildcards in the slice. Invalid slice bounds return [false]. *)

  val[@zero_alloc] valid_range : local_ string -> pos:int -> len:int -> bool @@ portable
  (** [valid_range s ~pos ~len] also permits [*/*], [type/*] and
      [type/*+suffix]. Other uses of [*] and parameters are rejected. Invalid
      slice bounds return [false]. *)

  val[@zero_alloc] specificity :
    range:local_ string -> pos:int -> len:int -> local_ string -> int @@ portable
  (** [specificity ~range ~pos ~len media] is [-1] for malformed input or no
      match, [0] for [*/*], [1] for [type/*], and [2] for an exact subtype or
      structured suffix. The range slice excludes parameters; parameters on
      [media] are ignored. Comparison folds ASCII case. Invalid slice bounds
      return [-1]. *)
end

(** [matches ~range media] is whether the media type [media] falls within
    [range], which may be a type, a range with a wildcard subtype or type,
    or a structured-suffix range such as ["application/*+json"]. Both are
    compared in lowercase without parameters. *)
val[@zero_alloc] matches : range:local_ string -> local_ string -> bool @@ portable

(** [accept_header medias] is an Accept field value listing [medias] in order
    of preference, first preferred, as in
    [accept_header [ media_type json; media_type text ]]. Preferences have
    strictly decreasing nonzero quality values, with at most three decimal
    places. Up to 1000 entries are supported.
    @raise Stdlib.Invalid_argument if more than 1000 entries are supplied. *)
val accept_header : string list -> string @@ portable

(** [can_encode t] is whether [t] has an encoder. *)
val[@zero_alloc] can_encode : _ t -> bool @@ portable

(** [can_decode t] is whether [t] has a decoder. *)
val[@zero_alloc] can_decode : _ t -> bool @@ portable

(** [encode t v] is [v] encoded as a string. It raises [Invalid_argument] if
    [t] cannot encode. *)
val encode : 'a t -> 'a -> string @@ portable

(** [encode_writer t v w] writes the encoding of [v] to [w] without an end of
    data. It raises [Invalid_argument] if [t] cannot encode. *)
val encode_writer : 'a t -> 'a -> Bytesrw.Bytes.Writer.t -> unit @@ portable

(** [decode t s] is the value encoded by [s]. It does not check a media type;
    use {!accepts} for that. It raises [Invalid_argument] if [t] cannot
    decode. *)
val decode : 'a t -> string -> ('a, error) result @@ portable

(** [decode_reader t r] is {!decode} reading from [r]. *)
val decode_reader :
  'a t -> Bytesrw.Bytes.Reader.t -> ('a, error) result @@ portable

(** {1 Built-in codecs} *)

(** [text] is [text/plain; charset=utf-8], carrying the string as it is. *)
val text : string t @@ portable

(** [html] is [text/html; charset=utf-8], carrying the string as it is. *)
val html : string t @@ portable

(** [octets] is [application/octet-stream]. It accepts any media type, and an
    absent one, so it reads any body as bytes. *)
val octets : string t @@ portable

val form : (string * string) list t @@ portable
(** [form] is [application/x-www-form-urlencoded], the encoding of an HTML form
    submitted without files and of an OAuth token endpoint. It carries the name
    and value pairs of {!Urlencoded}, so order and repeated names survive a
    round trip. Parameters of the received content type, [charset] among them,
    are ignored: the body is decoded as bytes. *)

(** {1 Sequences}

    A sequence codec describes a body made of many values, one per line, such
    as JSON Lines. A client decodes the items as they arrive, and a server
    writes them as they are produced. *)

(** An ['a seq] is a portable codec for a body holding a sequence of ['a]. *)
type 'a seq : value mod portable contended

(** [lines media item] is a sequence codec for [media] in which each line
    holds one value encoded by [item]. [item] must encode without a newline.
    A blank line is skipped when decoding, and a trailing carriage return is
    removed from each line. [accept] and [params] are as in {!v}. *)
val lines
  :  ?accept:string list
  -> ?params:(string * string) list
  -> string
  -> 'a t
  -> 'a seq
  @@ portable

(** [item s] is the codec for one element of [s]. *)
val[@zero_alloc] item : 'a seq -> 'a t @@ portable

(** [seq_media_type s] is the media type of the sequence, as {!media_type}. *)
val[@zero_alloc] seq_media_type : _ seq -> string @@ portable

(** [seq_content_type s] is the Content-Type value, as {!content_type}. *)
val seq_content_type : _ seq -> string @@ portable

(** [seq_accepts s ct] is whether [s] decodes a body of Content-Type [ct], as
    {!accepts}. *)
val[@zero_alloc] seq_accepts : _ seq -> string option @ local -> bool @@ portable

(** [encode_item s v] is one item, framed. For {!lines} it ends with a
    newline. *)
val encode_item : 'a seq -> 'a -> string @@ portable

(** [decode_item s line] is the value in one frame. For {!lines} a trailing
    carriage return is removed first. *)
val decode_item : 'a seq -> string -> ('a, error) result @@ portable

(** [encode_items s items] is every item of [items] framed in order. *)
val encode_items : 'a seq -> 'a Seq.t -> string @@ portable

(** [decode_items s body] is every value in [body], stopping at the first
    frame that does not decode. *)
val decode_items : 'a seq -> string -> ('a list, error) result @@ portable
