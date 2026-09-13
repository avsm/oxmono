(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val cards_dir : string -> string
(** [cards_dir root] locates the editable cards in a native store, or uses
    [root] itself for a directory of vCard files. *)
