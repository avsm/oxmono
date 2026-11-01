(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  look : Look.t;
  width : int option;
  height : int option;
  time : float;
}

let look c = c.look
let width c = c.width
let height c = c.height
let time c = c.time
let v ?width ?height ?(time = 0.) look = { look; width; height; time }
let default = v Look.default
let natural c = { c with width = None; height = None }
let with_room ~width ~height c = { c with width; height }
let with_width width c = { c with width; height = None }
let still c = { c with time = 0. }

let same_room a b =
  Look.equal a.look b.look
  && Option.equal Int.equal a.width b.width
  && Option.equal Int.equal a.height b.height
