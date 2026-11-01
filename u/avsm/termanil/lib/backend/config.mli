(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
type carddav = {
  url : string;
  collection : string option;
  username : string;
  password_file : string;
}

type t = {
  path : string;
  demo_root : string option;
  draft_root : string option;
  identity : string option;
  signature : string option;
  profile : string option;
  account : string option;
  service : string option;
  sortal_root : string option;
  vcard_root : string option;
  carddav : carddav option;
  dooit_config : string option;
  dooit_root : string option;
  capture_tags : string list;
}

val parse : path:string -> string -> t

val load : ?path:string -> unit -> t
(** [load ?path ()] resolves XDG paths without creating directories. *)

val init : ?path:string -> unit -> string
(** [init ?path ()] creates a private example config, refusing replacement. *)

val example : string
val replies : t -> string
val demo : ?root:string -> unit -> t
