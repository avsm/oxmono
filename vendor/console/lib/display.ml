(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Display_core.t

let v ?size ?delay ~now ppf view = Display_core.v ?size ?delay ~now ppf view
let set_view = Display_core.set_view
let print = Display_core.print
let with_output = Display_core.with_output
let redraw = Display_core.redraw
let close = Display_core.close
let pp = Display_core.pp
