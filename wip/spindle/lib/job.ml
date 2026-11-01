(* SPDX-License-Identifier: ISC *)
type kind = Push | Pull_request | Manual

type context = {
  repo : string;
  actor : string;
  commit : string;
  kind : kind;
  ref_ : string option;
  changed_files : string list;
  default_ref : bool;
  request : Jsont.json;
}

type step = Metadata | Command of string list
type t = { name : string; steps : step list; accepts : context -> bool }

let v ?(accepts = fun _ -> true) name steps = { name; steps; accepts }
let inspect = v "inspect" [ Metadata; Command [ "ls"; "-la"; "--" ] ]
