(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML mapping (object) values with metadata *)

type ('k, 'v) t

val make :
  ?anchor:string ->
  ?tag:string ->
  ?implicit:bool ->
  ?style:Layout_style.t ->
  ('k * 'v) list ->
  ('k, 'v) t
(** Create a mapping *)

(** {2 Accessors} *)

val members : ('k, 'v) t -> ('k * 'v) list
val anchor : ('k, 'v) t -> string option
val tag : ('k, 'v) t -> string option
val implicit : ('k, 'v) t -> bool
val style : ('k, 'v) t -> Layout_style.t

(** {2 Modifiers} *)

val with_anchor : string -> ('k, 'v) t -> ('k, 'v) t
val with_tag : string -> ('k, 'v) t -> ('k, 'v) t
val with_style : Layout_style.t -> ('k, 'v) t -> ('k, 'v) t

(** {2 Operations} *)

val map_keys : ('k -> 'k2) -> ('k, 'v) t -> ('k2, 'v) t
val map_values : ('v -> 'v2) -> ('k, 'v) t -> ('k, 'v2) t
val map : ('k -> 'v -> 'k2 * 'v2) -> ('k, 'v) t -> ('k2, 'v2) t
val length : ('k, 'v) t -> int
val is_empty : ('k, 'v) t -> bool
val find : ('k -> bool) -> ('k, 'v) t -> 'v option
val find_key : ('k -> bool) -> ('k, 'v) t -> ('k * 'v) option
val mem : ('k -> bool) -> ('k, 'v) t -> bool
val keys : ('k, 'v) t -> 'k list
val values : ('k, 'v) t -> 'v list
val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val fold : ('a -> 'k -> 'v -> 'a) -> 'a -> ('k, 'v) t -> 'a

(** {2 Comparison} *)

val pp :
  (Format.formatter -> 'k -> unit) ->
  (Format.formatter -> 'v -> unit) ->
  Format.formatter ->
  ('k, 'v) t ->
  unit

val equal :
  ('k -> 'k -> bool) -> ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool

val compare :
  ('k -> 'k -> int) -> ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int
