(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON-compatible YAML value representation *)

type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list ]

(** {2 Constructors} *)

val null : t
val bool : bool -> t
val int : int -> t
val float : float -> t
val string : string -> t
val list : ('a -> t) -> 'a list -> t
val obj : (string * t) list -> t

(** {2 Type Name} *)

val type_name : t -> string
(** Get the type name for error messages *)

(** {2 Safe Accessors} *)

val as_null : t -> unit option
val as_bool : t -> bool option
val as_float : t -> float option
val as_string : t -> string option
val as_list : t -> t list option
val as_assoc : t -> (string * t) list option
val as_int : t -> int option

(** {2 Unsafe Accessors} *)

val to_null : t -> unit
val to_bool : t -> bool
val to_float : t -> float
val to_string : t -> string
val to_list : t -> t list
val to_assoc : t -> (string * t) list
val to_int : t -> int

(** {2 Object Access} *)

val mem : string -> t -> bool
val find : string -> t -> t option
val get : string -> t -> t
val keys : t -> string list
val values : t -> t list

(** {2 Combinators} *)

val combine : t -> t -> t
val map : (t -> t) -> t -> t
val filter : (t -> bool) -> t -> t

(** {2 Comparison} *)

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val compare : t -> t -> int
