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

type step = { name : string; command : string; env : (string * string) list }

type t = {
  name : string;
  steps : step list;
  accepts : context -> bool;
  env : (string * string) list;
  clone : bool;
  definition : Jsont.json option;
  yaml : string option;
}

let step ?(env = []) name command = { name; command; env }

(* Like the reference spindle's default pull request types, a closed or
   merged pull request runs only workflows that ask for it. *)
let routine (c : context) =
  c.kind <> Pull_request
  ||
  match Option.bind (Json.field "trigger" c.request) (Json.field "action") with
  | Some (Jsont.String (("closed" | "merged"), _)) -> false
  | _ -> true

let v ?(accepts = routine) name steps =
  {
    name;
    steps;
    accepts;
    env = [];
    clone = true;
    definition = None;
    yaml = None;
  }

let workflow ~env ~clone ~definition ~yaml name steps =
  {
    name;
    steps;
    accepts = (fun _ -> true);
    env;
    clone;
    definition = Some definition;
    yaml = Some yaml;
  }

let inspect = v "inspect" [ step "list files" "ls -la" ]
