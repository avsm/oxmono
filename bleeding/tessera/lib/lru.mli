(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bounded least recently used caches.

    A cache holds at most [capacity] bindings. Adding one past that
    evicts the least recently used binding. A lookup that hits counts as
    a use, so a binding that is read stays.

    The table and the recency list both live in the value, so two caches
    share nothing and nothing here is global.

    Keys go through the polymorphic hash and equality of [Hashtbl], so a
    key holding a closure raises and a key holding a float treats [nan]
    as different from itself. Tessera keys tiles with integers and
    constant constructors, which both compare structurally. *)

type ('k, 'v) t
(** The type for caches binding ['k] to ['v]. *)

val create : capacity:int -> ('k, 'v) t
(** [create ~capacity] is an empty cache holding at most [capacity]
    bindings.

    @raise Invalid_argument if [capacity] is not positive. *)

val capacity : ('k, 'v) t -> int
(** [capacity t] is the bound [t] was created with. *)

val length : ('k, 'v) t -> int
(** [length t] is the number of bindings [t] holds, never above
    {!capacity}. *)

val find_opt : ('k, 'v) t -> 'k -> 'v option
(** [find_opt t k] is the value bound to [k], or [None]. A hit makes [k]
    the most recently used binding. *)

val add : ('k, 'v) t -> 'k -> 'v -> unit
(** [add t k v] binds [k] to [v] and makes it the most recently used
    binding, replacing any earlier binding of [k]. When that takes [t]
    past {!capacity} the least recently used binding is dropped. *)

val clear : ('k, 'v) t -> unit
(** [clear t] drops every binding of [t]. *)
