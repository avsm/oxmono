@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unknown and vendor-specific object members.

    Every JSContact object keeps the members its codec does not define, so a
    decoded value re-encodes with those members intact.
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.4} RFC 9553
     Section 1.7.4} requires an implementation to preserve properties it does
    not know, and
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.8.1} Section 1.8.1}
    requires the same of vendor-specific properties.

    @canonical Jscontact.Unknown *)

type t : immutable_data
(** The type for sets of unknown members. A set is a JSON object whose member
    names are unique, in the order they were decoded or added. *)

val empty : t
(** [empty] is the set with no members. *)

val of_list : (string * Jsont.json) list -> t
(** [of_list mems] is the set holding [mems], in order. A name bound twice keeps
    its last binding. *)

val to_list : t -> (string * Jsont.json) list
(** [to_list u] are the members of [u], in order. *)

val to_json : t -> Jsont.json
(** [to_json u] is [u] as a JSON object. *)

val is_empty : t -> bool
(** [is_empty u] is [true] if [u] has no members. *)

val find : t -> string -> Jsont.json option
(** [find u name] is the value of the member [name] of [u], or [None] if [u] has
    no such member. *)

val names : t -> string list
(** [names u] are the member names of [u], in order. *)

val add : t -> string -> Jsont.json -> t
(** [add u name v] is [u] with the member [name] bound to [v] at the end,
    replacing any member of that name. *)

val remove : t -> string -> t
(** [remove u name] is [u] without the member [name], and [u] itself if it has
    no such member. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] hold the same members, whatever their
    order. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf u] formats the member names of [u] on [ppf]. *)

val mems : (t, t, Jsont.mem list) Jsont.Object.Mems.map
(** [mems] is the member map that collects the members an object codec does not
    define. It is passed to [Jsont.Object.keep_unknown]. Decoding errors on an
    object that names the same member twice, which
    {{:https://www.rfc-editor.org/rfc/rfc7493.html#section-2.3} RFC 7493 Section
     2.3} forbids. *)

val validate : ?in_type:string -> t -> t Jscontact_valid.t
(** [validate ~in_type u] is [Ok u] if every member name of [u] is either
    syntactically a name that IANA could register, per
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.2} RFC 9553
     Section 1.7.2}, or a vendor-specific name, per
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.8.1} Section 1.8.1}.
    Section 1.7.4 allows no other spelling. [in_type] names the JSContact object
    type the members belong to, so that a name
    {{:https://www.rfc-editor.org/rfc/rfc9610.html} RFC 9610} reserves in that
    type alone is also rejected. [in_type] defaults to absent, in which case
    only names reserved in every object are rejected. See
    {!Jscontact.Registry.reserved_in}. *)
