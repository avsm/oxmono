(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Event = Event
include Task_core

type level = Task_core.level
type state = [ `Running | `Succeeded | `Failed | `Cancelled ]
type progress = [ `Spin | `Items of int * int | `Bytes of int64 * int64 ]

(* A transient task that succeeded leaves its place to the tasks under it. *)
let rec children t =
  List.concat_map
    (fun c ->
      if transient c && outcome c = Some `Succeeded then children c else [ c ])
    (Task_core.children t)

let root ?label ~now () = Task_core.root ?label ~now ()

let start ?key ?log_lines ?transient parent label =
  fst (Task_core.start ?key ?log_lines ?transient parent label)

let progress (t : t) : progress =
  match metric t with
  | Spin -> `Spin
  | Count (c, n) -> `Items (c, n)
  | Bytes (c, n) -> `Bytes (c, n)

let output t =
  List.rev
    (Queue.fold (fun acc (l : Task_core.line) -> l.text :: acc) [] (output t))

let rate t = match rate t with r when r > 0. -> Some r | _ -> None
