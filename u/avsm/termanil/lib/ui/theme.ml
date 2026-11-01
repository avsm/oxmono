(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Bonsai_term

let rgb r g b = Attr.Color.rgb ~r ~g ~b
let background = rgb 51 51 51
let foreground = rgb 238 232 213
let sand = rgb 240 230 140
let green = rgb 152 251 152
let sky = rgb 109 206 235
let red = rgb 255 160 160
let muted = rgb 174 174 166
let selection = rgb 80 78 65
let normal = [ Attr.fg foreground; Attr.bg background ]
let heading = [ Attr.fg sand; Attr.bg background; Attr.bold ]
let selected = [ Attr.bg selection; Attr.bold ]
