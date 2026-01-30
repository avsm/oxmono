(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML sequence (array) values with metadata *)

type 'a t

val make :
  ?anchor:string ->
  ?tag:string ->
  ?implicit:bool ->
  ?style:Layout_style.t ->
  'a list ->
  'a t
(** Create a sequence *)

(** {2 Accessors} *)

val members : 'a t -> 'a list
val anchor : 'a t -> string option
val tag : 'a t -> string option
val implicit : 'a t -> bool
val style : 'a t -> Layout_style.t

(** {2 Modifiers} *)

val with_anchor : string -> 'a t -> 'a t
val with_tag : string -> 'a t -> 'a t
val with_style : Layout_style.t -> 'a t -> 'a t

(** {2 Operations} *)

val map : ('a -> 'b) -> 'a t -> 'b t
val length : 'a t -> int
val is_empty : 'a t -> bool
val nth : 'a t -> int -> 'a
val nth_opt : 'a t -> int -> 'a option
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** {2 Comparison} *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
