(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val fields : Mapping.property list -> (string * string) list
(** [fields properties] presents standard and extension properties as labelled
    metadata. Embedded photo bytes stay in the card and are represented by a
    description. Unknown fields and parameters remain visible. *)
