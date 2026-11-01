(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val local : string -> Termanil_model.contact list * string list
(** [local root] reads a migrated store or a directory of individual vCards.
    Invalid files and duplicate UIDs are reported individually. Reading never
    writes files. YAML stores must first use [sortal carddav migrate]. *)

val card : Sortal_carddav.Remote.card -> Termanil_model.contact

val remote :
  ?collection:string -> Sortal_carddav.Remote.t -> Termanil_model.contact list
(** [remote dav] discovers and reads the selected CardDAV address book. *)

val combine :
  Termanil_model.contact list ->
  Termanil_model.contact list ->
  Termanil_model.contact list
(** [combine local remote] retains separate source identities, including equal
    names and email addresses. No implicit merge or write occurs. *)

val for_addresses :
  string list -> Termanil_model.contact list -> Termanil_model.contact list
