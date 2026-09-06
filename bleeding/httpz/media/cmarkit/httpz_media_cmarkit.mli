(** CommonMark document codecs integrated with {!Httpz_media}. *)

val markdown :
  ?strict:bool -> ?max_bracket_depth:int -> unit -> Cmarkit.Doc.t Httpz_media.t
(** [markdown ()] decodes [text/markdown] and [text/x-markdown], and
    encodes with [Cmarkit_commonmark].

    [strict] defaults to [false]. [max_bracket_depth] defaults to 16 and
    rejects excessive literal bracket nesting before parsing. Backslashes
    escape the next character; code spans are not interpreted by this
    lexical restriction. It is not a bound on parser work. Decoding untrusted
    Markdown requires Cmarkit's upstream nested-link parser correction.
    @raise Stdlib.Invalid_argument if [max_bracket_depth] is not positive. *)

val html : ?safe:bool -> unit -> Cmarkit.Doc.t Httpz_media.t
(** [html ()] encodes [text/html]. [safe] defaults to [true], dropping raw
    HTML and links whose schemes remain unsafe after percent-decoding and
    removing ASCII whitespace/control obfuscation. This conservative guard
    is not a substitute for a dedicated HTML sanitizer. *)
