(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Immutable string maps. *)

type 'a t : immutable_data with 'a
(** A finite map from strings to ['a]. *)

val empty : 'a t @@ portable
(** [empty] is the map with no bindings. *)

val of_list : (string * 'a) list -> 'a t @@ portable
(** [of_list l] is the map holding the bindings of [l], balanced so that a
    lookup is logarithmic. Repeated keys keep their last binding. *)

val find_opt : string -> 'a t -> 'a option @@ portable
(** [find_opt k m] is the value bound to [k] in [m], or [None] if there is
    none. *)

val find : string -> 'a t -> 'a @@ portable
(** [find k m] is the value bound to [k] in [m].

    @raise Not_found if [k] is not bound. *)

val bindings : 'a t -> (string * 'a) list @@ portable
(** [bindings m] is the bindings of [m] in increasing key order. *)

val depth : 'a t -> int
(** [depth m] is the longest path from the root of [m]. *)
