(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Bonsai_term

val wrap : width:int -> string -> string list
val text : ?attrs:Attr.t list -> string -> View.t
val fit : width:int -> height:int -> View.t -> View.t
