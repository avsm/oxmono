(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  style : Style.t option;
  glyph : string;
  align : View_core.align;
  label : View_core.t;
}

let v ?style ?(glyph = "─") ?(align = `Center) ?(label = View_core.empty) () =
  let glyph = Console_text.sanitize ~keep_newlines:false glyph in
  if Width.string_width glyph <> 1 then
    invalid_arg "Console.Rule.view: glyph must occupy one cell";
  { style; glyph; align; label = Widget.one_line label }

let space = View_core.text " "

let line look t width =
  let style = Option.value t.style ~default:(Widget.muted look) in
  let glyph = Widget.line look t.glyph in
  let fill n = View_core.ink style (Widget.fill n glyph) in
  let label = Widget.truncate look width t.label in
  let available = width - View_core.width label in
  if View_core.width label = 0 then fill width
  else if available = 0 then label
  else
    match t.align with
    | `Left -> View_core.(label ++ space ++ fill (available - 1))
    | `Right -> View_core.(fill (available - 1) ++ space ++ label)
    | `Center when available = 1 -> View_core.(label ++ space)
    | `Center ->
        let left = (available - 2) / 2 in
        let right = available - 2 - left in
        View_core.(fill left ++ space ++ label ++ space ++ fill right)

(* At its natural size, a label has two glyphs a side, and a line without
   one is four glyphs. *)
let natural t = match View_core.width t.label with 0 -> 4 | w -> w + 6

let view ?style ?glyph ?align ?label () =
  let t = v ?style ?glyph ?align ?label () in
  View_core.deferred ~grow:1
    ~kind:(fun look -> View_core.kind t.label look)
    (fun c ->
      let label = View_core.resolve (Ctx.natural c) t.label in
      let t = { t with label } in
      let width =
        match Ctx.width c with Some w -> max 0 w | None -> natural t
      in
      line (Ctx.look c) t width)
