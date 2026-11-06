(** Codec assertions shared by the JSContact tests. *)

val decode : 'a Jsont.t -> string -> ('a, Jsont.Error.t) result
(** [decode t s] decodes [s] with [t]. *)

val encode : 'a Jsont.t -> 'a -> (string, Jsont.Error.t) result
(** [encode t v] encodes [v] with [t]. *)

val enc_ok : string -> 'a Jsont.t -> 'a -> string
(** [enc_ok name t v] is the encoding of [v], failing test [name] on error. *)

val dec_ok : string -> 'a Jsont.t -> string -> 'a
(** [dec_ok name t s] is the decoding of [s], failing test [name] on error. *)

val dec_fails : string -> 'a Jsont.t -> string -> unit
(** [dec_fails name t s] fails test [name] unless [s] is rejected by [t]. *)

val roundtrip : string -> 'a Jsont.t -> string -> string -> unit
(** [roundtrip name t s expected] checks that decoding [s] and encoding the
    result gives [expected], and that a second cycle changes nothing. *)
