(* SPDX-License-Identifier: ISC *)
type files = { list : string -> string list; read : string -> string option }

type request = {
  pipeline : string;
  workflow : string;
  repo : string;
  source : string;
  commit : string;
  clone : bool;
  metadata : Jsont.json;
  definition : Jsont.json option;
  yaml : string option;
  env : (string * string) list;
  variables : (string * string) list;
  secrets : (string * string) list;
  steps : Job.step list;
  files : files;
  labels : unit -> string list;
}

type kind = [ `User | `System ]

type outcome = {
  workflow : string;
  status : [ `Success | `Failed | `Cancelled | `Timeout ];
  error : string option;
  log : string;
}

type progress = {
  workflow : string;
  status : [ `Pending | `Running | `Success | `Failed | `Cancelled | `Timeout ];
  step : string option;
  error : string option;
}

type context = {
  step : ?kind:kind -> ?command:string -> string -> (unit -> unit) -> unit;
  log : [ `Stdout | `Stderr ] -> string -> unit;
  spawn : ?env:(string * string) list -> string -> Job.step list -> unit;
  await_spawned : unit -> outcome list;
  spawned : ?since:progress list -> unit -> progress list;
  finished : unit -> outcome list;
  queued : 'a. (unit -> 'a) -> 'a;
}

type t = context -> request -> unit
