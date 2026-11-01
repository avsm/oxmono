(** Location-preserving JSON conversion for endpoint implementations. *)

val encode : 'a Jsont.t -> 'a -> (string, Error.t) result
(** [encode codec value] is the JSON text for [value]. Codec failures return
    [Error.Json]. *)

val decode : 'a Jsont.t -> Jsont.json -> ('a, Error.t) result
(** [decode codec json] is the value represented by [json]. Codec failures
    return [Error.Json]. *)

val decode_string : 'a Jsont.t -> string -> ('a, Error.t) result
(** [decode_string codec text] is the value represented by [text]. Parsing and
    codec failures return [Error.Json]. *)
