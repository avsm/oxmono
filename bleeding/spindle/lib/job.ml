(* SPDX-License-Identifier: ISC *)

type step = Metadata | Command of string list
type t = { name : string; steps : step list }

let inspect =
  { name = "inspect"; steps = [Metadata; Command ["ls"; "-la"; "--"]] }
