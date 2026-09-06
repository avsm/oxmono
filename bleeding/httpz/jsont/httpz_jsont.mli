(** Jsont readers with bounded JSON nesting.

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
