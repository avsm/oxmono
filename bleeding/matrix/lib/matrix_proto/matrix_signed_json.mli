@@ portable

(** Canonical JSON, the encoding Matrix signatures cover.

    A signature is computed over the object encoded as UTF-8 with no
    insignificant whitespace and with object members sorted by their byte-wise
    key, after the [signatures] and [unsigned] members have been removed.

    @see <https://spec.matrix.org/v1.11/appendices/#canonical-json>
      Canonical JSON *)

val canonical_json_result : Jsont.json -> (string, [> `Msg of string ]) result
(** [canonical_json_result j] validates [j] and returns its Matrix canonical
    JSON encoding. It rejects fractions, non-finite numbers, numbers outside the
    interoperable integer range, invalid UTF-8 and duplicate object names.
    Negative zero is rendered as [0]. *)

val canonical_json : Jsont.json -> string
(** [canonical_json j] is {!canonical_json_result} with the historical raising
    API: an invalid value raises [Invalid_argument]. *)

val json_for_signing : Jsont.json -> Jsont.json
(** [json_for_signing j] is [j] with any [signatures] and [unsigned] members
    removed. A value that is not an object is returned unchanged. *)
