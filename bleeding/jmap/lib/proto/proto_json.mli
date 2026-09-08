@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP JSON.

    JMAP uses the I-JSON profile. Text is UTF-8, strings contain neither
    surrogate code points nor Unicode noncharacters, and object member names are
    unique. General JSON numbers may be fractional; fields specified as JMAP
    [Int] or [UnsignedInt] use the stricter {!Jmap.Proto.Int53} codecs.

    @canonical Jmap.Proto.Json *)

val check : string -> (unit, Jsont.Error.t) result
(** [check s] is [Ok ()] if [s] is syntactically valid I-JSON. It additionally
    rejects a number outside binary64 magnitude, which Jsont could not retain
    without loss. *)

val check_value : Jsont.Json.t -> (unit, Jsont.Error.t) result
(** [check_value json] is [Ok ()] if [json] can be encoded as I-JSON without
    replacing a non-finite number or emitting an invalid string. Nested object
    members must be unique. *)

val decode :
  ?locs:bool ->
  ?max_depth:int ->
  'a Jsont.t ->
  string ->
  ('a, Jsont.Error.t) result
(** [decode jsont s] is the value encoded by [s] according to [jsont]. It checks
    the I-JSON constraints of {!check} and rejects excessive nesting. Numeric
    range checks imposed by [jsont], such as the Int53 bound, are applied in
    addition. [max_depth] defaults to {!Httpz_media.Json.default_max_depth}.

    [locs] defaults to [false] and gives the errors of the typed decode the
    position of the offending text. The I-JSON constraints are checked in a
    first pass over [s], so a syntax error or an I-JSON violation is reported
    without a position whatever [locs] is.

    @raise Stdlib.exception-Invalid_argument if [max_depth] is negative. *)

val encode :
  ?format:Jsont.format -> 'a Jsont.t -> 'a -> (string, Jsont.Error.t) result
(** [encode jsont value] is the I-JSON text for [value] according to [jsont]. It
    first encodes to a generic value and applies {!check_value}, which matters
    for caller-supplied [Jsont.json] values. [format] defaults to
    [Jsont.Minify]. *)

val pp : 'a Jsont.t -> Format.formatter -> 'a -> unit
(** [pp jsont ppf v] prints the JSON [jsont] encodes [v] as, indented over
    several lines. Members are printed one per line and a short array is kept on
    one line, which is the layout of [Jsont.pp_json] rather than that of an
    indenting encoder. A value [jsont] cannot encode is printed as the JSON
    string of the encoding error, so the output is always JSON text. *)

val media :
  ?media:string ->
  ?accept:string list ->
  ?format:Jsont.format ->
  ?locs:bool ->
  ?max_depth:int ->
  'a Jsont.t ->
  'a Httpz_media.t
(** [media jsont] is an HTTP media codec using {!decode} and {!encode}. [media]
    defaults to [application/json]. [accept] defaults to [application/*+json].
    [locs] defaults to [true]. *)
