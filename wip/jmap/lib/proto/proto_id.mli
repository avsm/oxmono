@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP identifiers.

    An Id is a string of 1 to 255 octets drawn from the alphabet [A-Za-z0-9_-],
    as defined by
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-1.2} RFC 8620
     Section 1.2}.

    @canonical Jmap.Proto.Id *)

type t : immutable_data
(** The type for identifiers. A value is either an Id or a creation reference.
    See {{!section-creation}creation references}. *)

val of_string : string -> (t, string) result
(** [of_string s] is the identifier [s]. The error holds a human readable
    message when [s] is empty, longer than 255 octets, or holds an octet outside
    the Section 1.2 alphabet. *)

val of_string_received : string -> (t, string) result
(** [of_string_received s] is the identifier [s] as a server sent it, checking
    only that [s] is 1 to 255 octets and does not start with a ["#"], which
    Section 5.3 gives to creation references.

    RFC 8620 Section 1.2 restricts an Id to the alphabet {!of_string} enforces,
    but that is a rule for the server that assigns the id, not a licence for a
    client to refuse a record it can otherwise read. Cyrus sets the id of a
    ContactCard to the card's uid, which is a URN and so holds colons, and a
    client that rejects it cannot read a [ContactCard/get] response at all.
    {!jsont} decodes with this function and encodes verbatim; use {!of_string}
    wherever the client is the one choosing the id. *)

val of_string_exn : string -> t
(** [of_string_exn s] is {!of_string}.

    @raise Invalid_argument if [s] is not a well formed Id. *)

(** {1:creation Creation references}

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.3} RFC 8620
     Section 5.3} says that "the client refers to the new record using its
    creation id prefixed with a [#]". The [#] octet is outside the Section 1.2
    alphabet, so a creation reference is not a well formed Id and {!of_string}
    rejects it, as does {!jsont} when decoding a server response.

    A creation reference is legal only where the specification says that a
    client argument may name a record that does not exist yet. Those are the
    keys of a [/set] [update] map, the entries of a [/set] [destroy] list, and
    the foreign key properties of a created or updated object, such as
    [mailboxIds] in [Email/set] or [emailId] in [EmailSubmission/set]. A server
    never returns one. *)

val of_creation_id : string -> (t, string) result
(** [of_creation_id cid] is the creation reference ["#" ^ cid] for the client
    chosen creation id [cid]. The error holds a human readable message when
    [cid] is not a well formed Id. *)

val of_creation_id_exn : string -> t
(** [of_creation_id_exn cid] is {!of_creation_id}.

    @raise Invalid_argument if [cid] is not a well formed Id. *)

val of_string_or_creation : string -> (t, string) result
(** [of_string_or_creation s] is the identifier [s] if [s] is a well formed Id,
    and the creation reference [s] if [s] is a [#] followed by a well formed Id.
    The error holds a human readable message otherwise, with any octet position
    given relative to the start of [s]. A bare ["#"] names no creation id and is
    rejected. *)

val is_creation_ref : t -> bool
(** [is_creation_ref t] is [true] if [t] is a [#] prefixed creation reference
    rather than an Id. *)

val to_creation_id : t -> string option
(** [to_creation_id t] is the creation id of [t] if [t] is a creation reference,
    and [None] if [t] is an Id. *)

(** {2 Typed creation tokens} *)

type 'a creation
(** The type for the creation ids of RFC 8620 Section 5.3. The parameter is the
    type of the record the creation id names, so a token made for a Mailbox does
    not typecheck where an Email is created. A token bound at top level is bound
    by an application and so falls under the value restriction, which gives it a
    weak type parameter that its first use fixes. *)

val creation : string -> 'a creation
(** [creation cid] is the creation id [cid].

    @raise Invalid_argument if [cid] is not a well formed Id. *)

val creation_id : 'a creation -> t
(** [creation_id c] is [c] as the key of a [/set] [create] map, which RFC 8620
    Section 5.3 writes without a ["#"]. *)

val creation_ref : 'a creation -> t
(** [creation_ref c] is [c] as a creation reference, which is the form an
    argument naming a record created in the same request takes. It is
    {!of_creation_id_exn} of the creation id of [c]. *)

val pp_creation : Format.formatter -> 'a creation -> unit
(** [pp_creation ppf c] prints the creation id of [c] on [ppf]. *)

val to_string : t -> string
(** [to_string t] is [t] as a string, [#] prefix included for a creation
    reference. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same octet sequence. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by octet sequence. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] on [ppf] as {!to_string} does. *)

val of_string_or_creation_received : string -> (t, string) result
(** [of_string_or_creation_received s] is {!of_string_or_creation} for a value a
    server sent: a creation reference is still held to the Section 1.2 alphabet,
    since the client chose that token itself, and anything else is
    {!of_string_received}. *)

(** {1 Codecs} *)

val jsont : t Jsont.t
(** [jsont] is the codec for an Id. Decoding is {!of_string_received}, which
    takes any string of 1 to 255 octets that is not a creation reference rather
    than enforcing the Section 1.2 alphabet, so that a record whose id a server
    spelled outside that alphabet can still be read. Encoding is {!to_string},
    so a value built by {!of_creation_id} encodes to its [#] prefixed form and
    may be used in the argument positions that admit one. *)

val jsont_or_creation : t Jsont.t
(** [jsont_or_creation] is {!jsont} except that decoding also accepts a creation
    reference. Encoding is the same as {!jsont}. *)

(** {2 Where each codec is used}

    The asymmetry follows the direction of travel of the value rather than the
    property it sits in.

    Client arguments that may name a record created earlier in the same request
    use [jsont_or_creation]. Those are the [destroy] list of a [/set] call and
    the keys of an [update] or foreign key map, for which see
    [Proto_json_map.of_id_or_creation].

    Everything a server sends back uses {!jsont}. RFC 8620 Section 5.3 resolves
    every creation reference before the response is built, so a server never
    returns one and accepting one there would hide a server bug.

    Objects with a single codec shared between the create argument and the
    response keep the strict {!jsont} on decode. Those are [Mailbox] for
    [parentId], [EmailSubmission] for [emailId] and [identityId], and [Email],
    whose [mailboxIds] values are keys and so already go through
    [Proto_json_map.of_id_or_creation]. A client still builds such a create
    object with {!of_creation_id}, which encodes to the [#] prefixed form. Only
    decoding a [#] back out of one of those properties is refused, and only a
    nonconformant server produces that. *)
