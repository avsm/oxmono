@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Codecs for the JSON shapes
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} builds on.

    The {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.3.4} [@type]}
    member every JSContact object carries, the
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.2} UnsignedInt}
    data type, and the JSON objects that Section 1.4.1 uses as maps and sets
    each have a codec below.

    @canonical Jscontact.Json *)

(** {1:type The [@type] member} *)

val type_mem :
  string -> ('o, unit -> 'a) Jsont.Object.map -> ('o, 'a) Jsont.Object.map
(** [type_mem name map] adds the [@type] member of a JSContact object whose type
    name is [name].
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.3.4} Section 1.3.4}
    makes [@type] optional wherever the type is implied by the property the
    object is the value of, which is everywhere in a Card but its topmost
    object. Decoding therefore accepts an absent member and errors on one that
    names a different type. Encoding omits it. The object constructor takes a
    leading [()] argument for the member. *)

val type_mem_required :
  string -> ('o, unit -> 'a) Jsont.Object.map -> ('o, 'a) Jsont.Object.map
(** [type_mem_required name map] is {!type_mem} for an object whose [@type] is
    mandatory, which Section 1.3.4 makes of an object that is not the value of a
    property. Decoding errors on an absent member and encoding always writes it.
    Section 2.1.1 makes the [@type] of a Card mandatory. *)

val type_mem_partial :
  string -> ('o, unit -> 'a) Jsont.Object.map -> ('o, 'a) Jsont.Object.map
(** [type_mem_partial name map] is {!type_mem} on decoding and
    {!type_mem_required} on encoding. An absent member is accepted, a mismatched
    one errors, and the member is always written. It serves a context that
    returns a subset of an object's properties rather than the whole object,
    where the mandatory [@type] of Section 2.1.1 may be missing on the way in
    but must be present on the way out. See {!Jscontact.Card.partial_jsont}. *)

(** {1:unsigned UnsignedInt} *)

val unsigned : kind:string -> int Jsont.t
(** [unsigned ~kind] is the codec for an UnsignedInt property named [kind], as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.2} Section 1.4.2}.
    Decoding and encoding error on a value outside the range 0 to 2{^ 53}-1 and
    decoding errors on a number that is not an integer. *)

(** {1:maps JSON objects used as maps} *)

(** JSON objects used as maps.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} types many
    properties [Id[T]] or [String[Boolean]], a JSON object whose member names
    are the keys of a map.
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.1} Section 1.4.1}
    explains the construction. Such an object is an unordered set whose entries
    are named, so that a patch can address one entry and an object can refer to
    another by its key.

    Decoding orders the association list by key, in the octet order of
    [String.compare], whatever the order of the members on the wire. Encoding
    writes the pairs in the order the list holds them. *)
module Map : sig
  val of_string : 'a Jsont.t -> (string * 'a) list Jsont.t
  (** [of_string value] is the codec for a [String[T]] map whose values are
      coded by [value]. Decoding errors on an object that names the same member
      twice. Encoding errors on a list that binds the same key twice, which no
      JSON object can express. *)

  val of_id : 'a Jsont.t -> (Jscontact_id.t * 'a) list Jsont.t
  (** [of_id value] is {!of_string} with keys decoded as {!Jscontact.Id.t}.

      {[
        { "e1": { "address": "jdoe@example.com" } }
      ]} *)

  val of_key :
    kind:string ->
    to_string:('k -> string) @ portable ->
    of_string:(string -> 'k) @ portable ->
    'v Jsont.t ->
    ('k * 'v) list Jsont.t
  (** [of_key ~kind ~to_string ~of_string value] is {!of_string} with keys read
      and written by the given functions, for a map whose keys are an enumerated
      value rather than free text, such as the [sortAs] of a Name. [kind] names
      the property in error messages. *)

  val bool_set :
    kind:string ->
    to_string:('a -> string) @ portable ->
    of_string:(string -> 'a) @ portable ->
    ?show:('a -> string) @ portable ->
    unit ->
    'a list Jsont.t
  (** [bool_set ~kind ~to_string ~of_string] is the codec for a
      [String[Boolean]] set such as the [contexts] of a phone number, whose keys
      are read with [of_string] and written with [to_string].
      {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} requires every
      value of such an object to be [true], so decoding errors on [false] and
      encoding writes [true] throughout. [kind] names the property in error
      messages. [show] names a key in a decoding error and defaults to
      [to_string]. A [to_string] that rejects a value it cannot write needs a
      [show] that does not, so that a decoding error reports what the wire
      actually held.

      {[
        { "work": true, "private": true }
        (* decodes to [[`Private; `Work]] *)
      ]} *)

  val string_set : kind:string -> string list Jsont.t
  (** [string_set ~kind] is {!bool_set} over unconstrained strings, for the
      [keywords] and [members] properties of a Card, whose keys are free text
      and uids rather than enumerated values. *)

  val equal :
    key:('k -> 'k -> int) ->
    ('v -> 'v -> bool) ->
    ('k * 'v) list ->
    ('k * 'v) list ->
    bool
  (** [equal ~key eq a b] is [true] if the maps [a] and [b] bind the same keys,
      ordered by [key], to values equal by [eq], whatever the order of their
      entries. *)
end
