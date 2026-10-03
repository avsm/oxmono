(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let sanitize = Render.sanitize
let sanitize_styles = Render.sanitize_styles

let style_renderer ?renderer ~getenv ~is_tty () =
  match renderer with
  | Some renderer -> renderer
  | None -> (
      match (getenv "NO_COLOR", getenv "TERM") with
      | Some value, _ when value <> "" -> `None
      | _, (None | Some ("" | "dumb")) -> `None
      | _ -> if is_tty then `Ansi_tty else `None)

module Color = Color
module Style = Style
module Gradient = Gradient
module Width = Width
module Span = Span
module Border = Border
module Guide = Guide
module Anim = Anim
module Spinner = Spinner
module Theme = Theme
module Bar = Bar
module Panel = Panel
module Table = Table
module Tree = Tree
module Rule = Rule
module Layout = Layout
module Canvas = Canvas
module Display = Display
module Input = Input
