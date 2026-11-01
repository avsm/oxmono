(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = No_dune_project | Refused of string | Active of { merlin : bool }

let note ?(flatten = Fun.id) = function
  | No_dune_project ->
      "okit: no dune tools, since this workspace has no dune-project"
  | Refused e -> "okit: no dune tools. " ^ flatten e
  | Active { merlin = false } ->
      "okit: dune tools active, and no ocamlmerlin to answer for the workspace"
  | Active { merlin = true } -> "okit: dune tools active, ocamlmerlin found"
