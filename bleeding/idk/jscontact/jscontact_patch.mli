@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** PatchObjects.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.3} RFC 9553
     Section 1.4.3} types a PatchObject [String[*]] and describes it as "an
    unordered set of patches on a JSON object. Each key is a path represented in
    a subset of the JSON Pointer format [RFC6901]. The paths have an implicit
    leading ["/"], so each key is prefixed with ["/"] before applying the JSON
    Pointer evaluation algorithm."

    A patch value of [null] removes the property. Any other value sets it.

    The [localizations] property of a Card holds one PatchObject per language
    tag. See {!Jscontact.Card.localize}.

    @canonical Jscontact.Patch *)

(** The type for the value of an entry. *)
type entry =
  | Remove  (** The property is removed. Its wire value is [null]. *)
  | Set of Jsont.json  (** The property is set to the value. *)

type t : immutable_data
(** The type for a PatchObject. *)

val empty : t
(** [empty] is the patch with no entries, which leaves a value unchanged. *)

val of_list : (string * entry) list -> (t, string) result
(** [of_list entries] is the patch applying each of [entries], whose keys are
    JSON Pointer paths with the implicit leading ["/"] omitted, such as
    ["name/components/0/value"]. The error holds a message if a key is empty,
    holds a ["~"] not followed by ["0"] or ["1"], or appears twice. *)

val v : (string * entry) list -> t
(** [v entries] is {!of_list}.

    @raise Invalid_argument if {!of_list} would error. *)

val to_list : t -> (string * entry) list
(** [to_list p] are the entries of [p], ordered by key. *)

val is_empty : t -> bool
(** [is_empty p] is [true] if [p] has no entries. *)

val find : t -> string -> entry option
(** [find p key] is the entry [p] holds for [key], or [None] if [p] does not
    bind [key]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] hold the same entries. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] formats the keys of [p] on [ppf]. *)

val validate : t -> t Jscontact_valid.t
(** [validate p] checks the rules of Section 1.4.3 that do not depend on the
    value being patched. Every key is well formed JSON Pointer syntax naming at
    least one token, which includes its reference tokens being valid UTF-8. No
    key is a prefix of another, compared token by token, so ["a/b"] does not
    prefix ["a/bc"]. The remaining rules, that the parent of each path already
    exists and that an array member is not removed, are checked by {!apply}
    against the value at hand. *)

val apply : t -> Jsont.json -> (Jsont.json, string) result
(** [apply p json] is [json] with every entry of [p] applied, or an error naming
    the first entry that could not be. Section 1.4.3 requires a patch to be
    rejected in its entirety if any of its entries is invalid. [json] is
    returned unchanged on error rather than partially patched.

    A [Remove] entry removes the property, and does nothing if the property is
    absent. A [Set] entry sets the property to its value, creating it if the
    object it belongs to already exists. An entry errors if a token before the
    last does not resolve, or if the value being patched is neither a JSON
    object nor a JSON array.

    Within an array a [Set] replaces the member at an index that already exists,
    which Section 1.4.3 permits. An index that does not exist, the append token
    ["-"], and a [Remove] are each an error, since a PatchObject may not change
    the length of an array. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a PatchObject. Decoding orders the entries by key
    and does not check them. {!validate} does. *)
