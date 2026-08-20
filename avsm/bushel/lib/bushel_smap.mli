(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Build-once lookup tables keyed by string.

    A table that a portable function captures and reads has to have a type
    whose kind the compiler knows to be immutable. Neither [Hashtbl.t] nor a
    stdlib [Map.S.t] declares such a kind, and [Map.MakePortable] does not
    change that, so a record field holding either cannot be read from a
    portable closure. This module is a balanced binary search tree over string
    keys. It is built once from a list and then read only, which is what every
    caller in this tree needs. *)

type 'a t : immutable_data with 'a
(** A finite map from strings to ['a]. The kind is what lets a value of this
    type be read after it crosses into a portable closure. *)

val empty : 'a t
(** [empty] is the map with no bindings. *)

val of_list : (string * 'a) list -> 'a t
(** [of_list l] is the map holding the bindings of [l], balanced so that a
    lookup costs time logarithmic in the length of [l]. A key bound more than
    once in [l] keeps the binding that appears last, as repeated
    [Hashtbl.replace] would. *)

val find_opt : string -> 'a t -> 'a option
(** [find_opt k m] is the value bound to [k] in [m], or [None] if there is
    none. *)

val find : string -> 'a t -> 'a
(** [find k m] is the value bound to [k] in [m].

    @raise Not_found if [k] is not bound. *)

val bindings : 'a t -> (string * 'a) list
(** [bindings m] is the bindings of [m] in increasing key order. *)
