@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Semantic validation.

    A codec of this library enforces the shape of a JSContact object. It checks
    the type of each property and the presence of the mandatory ones. It does
    not enforce the rules a value must meet beyond its shape, such as "at least
    one of the name and units properties MUST be set" or "the value MUST be in
    the range of 1 to 100". Those rules live in the [validate] function of each
    type, so that a Card a server sends still decodes into a usable value and an
    application decides for itself when to hold it to
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7} RFC 9553 Section
     1.7}. The combinators below build those [validate] functions.

    @canonical Jscontact.Valid *)

type 'a t = ('a, string) result
(** The type for the outcome of validating a value of type ['a]. The error holds
    a human readable message naming the property at fault. *)

val ok : 'a -> 'a t
(** [ok v] is [Ok v]. *)

val error : ('a, Format.formatter, unit, 'b t) format4 -> 'a
(** [error fmt ...] is an [Error] whose message is formatted by [fmt]. *)

val check : bool -> ('a, Format.formatter, unit, unit t) format4 -> 'a
(** [check cond fmt ...] is [Ok ()] if [cond] holds and otherwise the [Error] of
    {!error}. The message is formatted only when [cond] is [false]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let*] sequences validations, stopping at the first error. *)

val in_ : string -> 'a t -> 'a t
(** [in_ prop r] prefixes the error message of [r] with [prop] and a colon, so
    that a failure deep in a Card names the path that reaches it. *)

val opt : ('a -> 'a t) -> 'a option -> 'a option t
(** [opt validate v] is [Ok None] on [None] and applies [validate] otherwise. *)

val list : ('a -> 'a t) -> 'a list -> 'a list t
(** [list validate l] is [Ok l] if [validate] holds of every element of [l], and
    otherwise the [Error] of the first element that fails, its message prefixed
    by its index. *)

val entries :
  ('a -> 'a t) -> (Jscontact_id.t * 'a) list -> (Jscontact_id.t * 'a) list t
(** [entries validate m] is [Ok m] if [validate] holds of every value in the Id
    map [m], and otherwise the [Error] of the first value that fails, its
    message prefixed by its key. *)

val string_entries : ('a -> 'a t) -> (string * 'a) list -> (string * 'a) list t
(** [string_entries validate m] is {!entries} for a map with string keys. *)
