(** Bounded Jsont codecs integrated with {!Media}.

    Encoding and decoding stream through [Jsont_bytesrw]. Jsont applies no
    nesting-depth cap of its own, so these codecs first pass input through
    Jsonm's incremental parser and reject excessive nesting before Jsont sees
    it.

    A decode failure can quote the offending body context. Applications that
    log one may therefore disclose fragments of that body to their logs. *)

type Media.detail += Jsont of Jsont.Error.t
(** [Jsont error] preserves a structured Jsont decoding error behind a
    {!Media.Malformed} failure. *)

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
    A negative bound raises [Invalid_argument]. *)

val decode_string' :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  'a Jsont.t ->
  string ->
  ('a, Jsont.Error.t) result
(** [decode_string'] is {!decode'} over a string. *)

val v :
  ?media:string ->
  ?accept:string list ->
  ?format:Jsont.format ->
  ?locs:bool ->
  ?max_depth:int ->
  'a Jsont.t ->
  'a Media.t
(** [v t] is a codec for [application/json] carrying values described by [t].
    [media] names another type and [accept] defaults to
    [["application/*+json"]]. [locs] defaults to [true] so structured
    failures carry source locations.

    [max_depth] bounds arrays and objects before Jsont's recursive decoder
    sees them, counting the outermost container as depth one. It defaults to
    {!default_max_depth}; zero accepts scalars only and a negative value raises
    [Invalid_argument].

    Jsont maps an out-of-range JSON number such as [1e999] to an infinite
    float; encoding that value emits JSON [null]. Use a more restrictive Jsont
    number description when that lossy mapping is unacceptable. *)

val json : Jsont.json Media.t
(** [json] is {!v} for generic JSON values. *)

val lines :
  ?media:string ->
  ?accept:string list ->
  ?max_depth:int ->
  'a Jsont.t ->
  'a Media.seq
(** [lines t] is a sequence codec for JSON Lines. It defaults to
    [application/jsonl] and accepts the common NDJSON spellings. Each line is
    independently bounded by [max_depth], on the terms of {!v}. *)
