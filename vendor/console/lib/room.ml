(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Ctx.t

let width = Ctx.width
let height = Ctx.height
let time = Ctx.time
let theme r = (Ctx.look r).theme
let utf_8 r = (Ctx.look r).utf_8
