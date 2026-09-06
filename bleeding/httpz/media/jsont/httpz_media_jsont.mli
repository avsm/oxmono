(** Bounded Jsont codecs integrated with {!Httpz_media}.

    Encoding and decoding stream through [Jsont_bytesrw]. Jsont applies no
    nesting-depth cap of its own, so these codecs first pass input through
    the incremental structural scan in {!Httpz_jsont} and reject excessive
    nesting before Jsont sees it.

    A decode failure can quote the offending body context. Applications that
    log one may therefore disclose fragments of that body to their logs. *)

type Httpz_media.detail += Jsont of Jsont.Error.t
(** [Jsont error] preserves a structured Jsont decoding error behind a
    {!Httpz_media.Malformed} failure. *)

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
(** [decode' t reader] is {!Httpz_jsont.decode'} with JSON arrays and
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
(** [decode_string'] is {!Httpz_jsont.decode_string'}. *)

val v :
  ?media:string ->
  ?accept:string list ->
  ?format:Jsont.format ->
  ?locs:bool ->
  ?max_depth:int ->
  'a Jsont.t ->
  'a Httpz_media.t
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
    number description when that lossy mapping is unacceptable. *)

val json : Jsont.json Httpz_media.t
(** [json] is {!v} for generic JSON values. *)

val lines :
  ?media:string ->
  ?accept:string list ->
  ?max_depth:int ->
  'a Jsont.t ->
  'a Httpz_media.seq
(** [lines t] is a sequence codec for JSON Lines. It defaults to
    [application/jsonl] and accepts the common NDJSON spellings,
    [application/ndjson] and [application/x-ndjson] among them. Each line is
    independently bounded by [max_depth], on the terms of {!v}. *)
