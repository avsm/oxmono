@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Typed ParticipantIdentity properties. [None] represents a property omitted
    by a projection or returned as null. Unrecognised fields are retained in
    [unknown], including fields of nested extensible objects. [meta] locates the
    original object for {!Proto_response.source_fragment}. *)

type t = {
  meta : Jsont.Meta.t;
  id : Proto_id.t option;
  name : string option;
  calendar_address : string option;
  is_default : bool option;
  unknown : Proto_unknown.t;
}

val jsont : t Jsont.t
(** [jsont] codes ParticipantIdentity properties and retains unrecognised
    members. *)

val empty : t
(** [empty] has no properties set. *)

type property = [ `Id | `Name | `Calendar_address | `Is_default ]

val property_to_string : [< property ] -> string
