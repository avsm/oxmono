@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON objects used as maps.

    RFC 8620 and RFC 8621 type many properties [String[T]] or [Id[T]], a JSON
    object whose member names are the keys of a map. The codecs below convert
    such an object to and from an association list.

    Decoding orders the association list by key, in the octet order of
    [String.compare], whatever the order of the members on the wire. Encoding
    writes the pairs in the order the list holds them.

    @canonical Jmap.Proto.Json_map *)

val of_string : 'a Jsont.t -> (string * 'a) list Jsont.t
(** [of_string value] is the codec for a [String[T]] map whose values are coded
    by [value].

    {[
      { "en": "Hello", "fr": "Bonjour" }
      (* decodes to [("en", "Hello"); ("fr", "Bonjour")] *)
    ]}

    Decoding errors on an object that names the same member twice. Encoding
    errors on a list that binds the same key twice, which no JSON object can
    express. *)

val of_id : 'a Jsont.t -> (Proto_id.t * 'a) list Jsont.t
(** [of_id value] is {!of_string} with keys decoded as JMAP Ids. A creation
    reference is rejected, which is what every response side map wants.

    {[
      { "Mdc123": { ... }, "Mdc456": { ... } }
    ]} *)

val of_creation : 'v Jsont.t -> ('r Proto_id.creation * 'v) list Jsont.t
(** [of_creation value] is {!of_string} with keys decoded as typed creation ids.
    A creation reference with a [#] prefix is rejected. *)

val of_id_or_creation : 'a Jsont.t -> (Proto_id.t * 'a) list Jsont.t
(** [of_id_or_creation value] is {!of_id} for request argument positions, where
    a key may also be a creation reference of the form ["#cid"] as defined by
    RFC 8620 Section 5.3. Use it for the [update] map of a [/set] call and for
    foreign key maps such as [mailboxIds]. Never use it for a response side map.
    A bare ["#"] key is rejected. *)

val id_to_bool : (Proto_id.t * bool) list Jsont.t
(** [id_to_bool] is the codec for an [Id[Boolean]] map such as the [mailboxIds]
    of an Email. Keys may be creation references, since such a map appears as a
    foreign key in [/set] arguments. The same codec serves responses.

    {[
      { "Mbox1": true, "Mbox2": true }
    ]} *)

val string_to_bool : (string * bool) list Jsont.t
(** [string_to_bool] is the codec for a [String[Boolean]] map such as the
    [keywords] of an Email.

    {[
      { "$seen": true, "$flagged": true }
    ]} *)

(** {1 Nullable members} *)

val nullable_mem :
  string ->
  'a Jsont.t ->
  enc:('o -> 'a option) @ portable ->
  ('o, 'a option -> 'm) Jsont.Object.map ->
  ('o, 'm) Jsont.Object.map
(** [nullable_mem name t ~enc map] declares the member [name] of [map], whose
    JMAP type is [T|null]. Decoding maps both an absent member and an explicit
    JSON [null] to [None]. Encoding omits the member for [None].

    [Jsont.Object.opt_mem] accepts an absent member but rejects an explicit
    [null], and so cannot code the [T|null] properties that RFC 8620 and RFC
    8621 define throughout. *)

val nullable_mem_null :
  string ->
  'a Jsont.t ->
  enc:('o -> 'a option) @ portable ->
  ('o, 'a option -> 'm) Jsont.Object.map ->
  ('o, 'm) Jsont.Object.map
(** [nullable_mem_null] is {!nullable_mem} except that [None] encodes as an
    explicit [null] member rather than being omitted. Use it for the arguments
    to which the specification gives [null] a meaning distinct from absence,
    such as the [ids] argument of a [/get] call, where [null] means all. *)
