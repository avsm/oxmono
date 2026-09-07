(** Typed media codecs with JSON, JSON Lines, Markdown and HTML support. *)

include module type of Media

module Json : sig
  (** JSON readers and codecs with bounded nesting.

      Input passes through an incremental structural scan before Jsont sees
      each slice. The scan tracks strings and escapes across slices without
      decoding tokens twice; Jsont checks the complete JSON grammar.

      A decode failure can quote the offending body context. Applications that
      log one may therefore disclose fragments of that body to their logs. *)

  val default_max_depth : int
  (** [default_max_depth] is [128]. *)

  val decode' :
    ?layout:bool ->
    ?locs:bool ->
    ?file:Jsont.Textloc.fpath ->
    ?max_depth:int ->
    'a Jsont.t ->
    Bytesrw.Bytes.Reader.t ->
    ('a, Jsont.Error.t) result
  (** [decode' t reader] is [Jsont_bytesrw.decode'] with JSON arrays and
      objects bounded by [max_depth], which defaults to {!default_max_depth}.
      The outermost container has depth one and zero accepts scalars only.
      A negative bound raises [Invalid_argument].

      The bound is applied by a structural pre-scan that reads every byte, so it
      holds over the complete input rather than only the prefix a lexer could
      read. *)

  val decode_string' :
    ?layout:bool ->
    ?locs:bool ->
    ?file:Jsont.Textloc.fpath ->
    ?max_depth:int ->
    'a Jsont.t ->
    string ->
    ('a, Jsont.Error.t) result
  (** [decode_string'] is {!decode'} over a string. *)

  type detail += Error of Jsont.Error.t
  (** [Error error] preserves the structured JSON decoding error. *)

  val v :
    ?media:string ->
    ?accept:string list ->
    ?format:Jsont.format ->
    ?locs:bool ->
    ?max_depth:int ->
    'a Jsont.t ->
    'a t
  (** [v t] is a codec for [application/json] carrying values described by [t].
      [media] names another type and [accept] defaults to
      [["application/*+json"]]. [locs] defaults to [true] so ordinary decoding
      failures carry source locations. Synthetic nesting-limit failures have
      no source location.

      [max_depth] bounds arrays and objects before Jsont's recursive decoder
      sees them, counting the outermost container as depth one. It defaults to
      {!default_max_depth}; zero accepts scalars only and a negative value raises
      [Invalid_argument].

      Jsont maps an out-of-range JSON number such as [1e999] to an infinite
      float; encoding that value emits JSON [null]. Use a more restrictive Jsont
      number description when that lossy mapping is unacceptable. Jsont's
      floating-point number codec also decodes JSON [null] as NaN.

      Encoding does not escape [</], [&], U+2028 or U+2029; do not embed the
      resulting JSON directly in an HTML [script] element. *)

  val json : Jsont.json t
  (** [json] is {!v} for generic JSON values. *)

  val lines :
    ?media:string ->
    ?accept:string list ->
    ?max_depth:int ->
    'a Jsont.t ->
    'a seq
  (** [lines t] is a sequence codec for JSON Lines. It defaults to
      [application/jsonl] and accepts the common NDJSON spellings,
      [application/ndjson] and [application/x-ndjson] among them. Each line is
      independently bounded by [max_depth], on the terms of {!v}. *)
end

module Markdown : sig
  (** Markdown decoding and Markdown/HTML encoding. *)

  val markdown :
    ?strict:bool -> ?max_bracket_depth:int -> unit -> Cmarkit.Doc.t t
  (** [markdown ()] decodes [text/markdown] and [text/x-markdown], and
      encodes with [Cmarkit_commonmark].

      [strict] defaults to [false]. [max_bracket_depth] defaults to 16 and
      rejects excessive literal bracket nesting before parsing. Backslashes
      escape the next character; code spans are not interpreted by this
      lexical restriction. It is not a bound on parser work. Decoding untrusted
      Markdown requires Cmarkit's upstream nested-link parser correction.
      @raise Stdlib.Invalid_argument if [max_bracket_depth] is not positive. *)

  val html : ?safe:bool -> unit -> Cmarkit.Doc.t t
  (** [html ()] encodes [text/html]. [safe] defaults to [true], dropping raw
      HTML and links whose schemes remain unsafe after percent-decoding and
      removing ASCII whitespace/control obfuscation. This conservative guard
      is not a substitute for a dedicated HTML sanitizer. *)
end
