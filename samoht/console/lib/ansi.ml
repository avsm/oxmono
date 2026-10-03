(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let dim = "\027[2m"
let bold = "\027[1m"
let reset_code = "\027[0m"
let color_code color = "\027[" ^ Color.to_fg_code color ^ "m"
let styled code s = String.concat "" [ code; s; reset_code ]
let dimmed s = if s = "" then "" else styled dim s

let should_style ppf =
  match Fmt.style_renderer ppf with `Ansi_tty -> true | `None -> false
