@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** PatchObjects.

    A PatchObject is the value of a [/set] [update] map.
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.3} RFC 8620
     Section 5.3} types it [String[*]] and describes it as "an unordered set of
    patches. The keys are a path in JSON Pointer format [RFC6901], with an
    implicit leading [/] (i.e., prefix each key with [/] before applying the
    JSON Pointer evaluation algorithm)."

    A patch value of [null] means "set to the default value if specified for
    this property; otherwise, remove the property from the patched object".
    Anything else is "the value to set for this property". An entry value is
    therefore [Jsont.json option], where [None] is JSON [null].

    {2 Example}

    File an Email in an archive Mailbox and take it out of the inbox in one
    patch.
    {[
    let patch =
      Patch.v
        [
          Patch.pointer "keywords/$seen" (Some (Jsont.Json.bool true));
          Patch.pointer ("mailboxIds/" ^ archive) (Some (Jsont.Json.bool true));
          Patch.pointer ("mailboxIds/" ^ inbox) None;
        ]
    in
    Chain.email_set ~account_id ~update:[ (email_id, patch) ] ()
    ]}

    @canonical Jmap.Proto.Patch *)

(** {1 Entries} *)

type entry : immutable_data
(** The type for one patch. A value is a path and the value to set at it, [None]
    standing for JSON [null]. *)

val path : string list -> Jsont.json option -> entry
(** [path tokens v] is the patch setting [v] at [tokens], a list of unescaped
    JSON Pointer reference tokens. The escaping of ["~"] and ["/"] within a
    token is applied when the patch is serialised, so
    [path ["keywords"; "a/b"] (Some j)] has the key ["keywords/a~1b"].

    @raise Invalid_argument
      if [tokens] is empty or is one empty token, since neither names a
      property, or if a token is not valid UTF-8. *)

val pointer : string -> Jsont.json option -> entry
(** [pointer key v] is {!path} for a [key] already in JSON Pointer syntax with
    the implicit leading ["/"] omitted, such as ["keywords/$seen"].

    @raise Invalid_argument
      if [key] is empty, is not valid UTF-8, or is not valid JSON Pointer
      syntax, that is if it holds a ["~"] not followed by ["0"] or ["1"]. *)

val entry_key : entry -> string
(** [entry_key e] is the JSON Pointer key of [e], escaped, without the implicit
    leading ["/"]. *)

val entry_value : entry -> Jsont.json option
(** [entry_value e] is the value [e] sets, [None] for JSON [null]. *)

val set_field : string -> Jsont.json -> entry
(** [set_field name v] is the patch replacing the top level property [name] with
    [v].

    @raise Invalid_argument if [name] is empty or is not valid UTF-8. *)

val remove_field : string -> entry
(** [remove_field name] is the patch setting the top level property [name] to
    [null], which RFC 8620 Section 5.3 reads as "set to the default value if
    specified for this property; otherwise, remove the property from the patched
    object".

    @raise Invalid_argument if [name] is empty or is not valid UTF-8. *)

(** {1 Patch objects} *)

type t
(** The type for validated PatchObjects. *)

val empty : t @@ nonportable
(** [empty] is the patch that changes nothing, [{}]. *)

val is_empty : t -> bool
(** [is_empty p] is [true] if [p] has no entries. *)

val of_entries : entry list -> (t, string) result
(** [of_entries es] is the patch made of [es]. The error holds a human readable
    message when [es] breaks one of the restrictions RFC 8620 Section 5.3 places
    on a PatchObject, which the server would otherwise reject with an
    [invalidPatch] error.

    The first is that "the pointer MUST NOT reference inside an array (i.e., you
    MUST NOT insert/delete from an array; the array MUST be replaced in its
    entirety instead)". This restriction is checked by the server. A numeric
    token or ["-"] can also name an object member, including a JMAP Id, so its
    meaning depends on the object being patched.

    The second is that "there MUST NOT be two patches in the PatchObject where
    the pointer of one is the prefix of the pointer of the other". Two equal
    paths fall under the same rule, since a JSON object cannot carry the same
    member name twice.

    The remaining restriction, that "all parts prior to the last MUST already
    exist on the object being patched", depends on the current server side value
    of the record and cannot be checked here. *)

val v : entry list -> t
(** [v es] is {!of_entries}.

    @raise Invalid_argument
      if [es] breaks a restriction of RFC 8620 Section 5.3. *)

val add : entry -> t -> (t, string) result
(** [add e p] is [p] with [e] appended. The error holds a human readable message
    when [e] conflicts with an entry already in [p] under the rules of
    {!of_entries}. *)

val of_json : Jsont.json -> (t, string) result
(** [of_json j] is the patch [j], for a PatchObject already built as JSON. RFC
    8620 Section 5.3 notes that "this patch definition is designed such that an
    entire Foo object is also a valid PatchObject", so a whole record may be
    sent as one. [j] must be a JSON object and its members are checked as by
    {!of_entries}. *)

val to_json : t -> Jsont.json
(** [to_json p] is the JSON object of [p]. An entry value of [None] is written
    as an explicit JSON [null]. *)

val to_list : t -> (string * Jsont.json option) list
(** [to_list p] is the entries of [p] as escaped key and value pairs, in the
    order they were added. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a PatchObject. Decoding checks the members as
    {!of_json} does. *)
