(* SPDX-License-Identifier: ISC *)

type at_uri = { did : string; collection : string; rkey : string }

val parse_at_uri : string -> at_uri option
(** [parse_at_uri text] validates a complete record URI. *)

val make_at_uri : did:string -> collection:string -> rkey:string -> string
val pp_at_uri : at_uri Fmt.t
