(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Source spans representing ranges in input *)

type t = { start : Position.t; stop : Position.t }

let make ~start ~stop = { start; stop }
let point pos = { start = pos; stop = pos }

let merge a b =
  let start =
    if Position.compare a.start b.start <= 0 then a.start else b.start
  in
  let stop = if Position.compare a.stop b.stop >= 0 then a.stop else b.stop in
  { start; stop }

let extend span pos = { span with stop = pos }

let pp fmt t =
  if t.start.line = t.stop.line then
    Format.fprintf fmt "line %d, columns %d-%d" t.start.line t.start.column
      t.stop.column
  else Format.fprintf fmt "lines %d-%d" t.start.line t.stop.line

let to_string t = Format.asprintf "%a" pp t

let compare a b =
  let c = Position.compare a.start b.start in
  if c <> 0 then c else Position.compare a.stop b.stop

let equal a b = Position.equal a.start b.start && Position.equal a.stop b.stop
