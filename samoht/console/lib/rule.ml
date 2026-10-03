(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type align = [ `Left | `Center | `Right ]
type t = Span.t

let repeat glyph n = String.concat "" (List.init n (fun _ -> glyph))

let v ?(char_width = Width.default_char_width) ?theme ?style ?(glyph = "─")
    ?(align = `Center) ?label ~width () =
  if width < 0 then invalid_arg "Console.Rule.v: negative width";
  String.iter
    (fun ch ->
      let code = Char.code ch in
      if code < 0x20 || code = 0x7f then
        invalid_arg "Console.Rule.v: control character in glyph")
    glyph;
  if Width.string_width ~char_width glyph <> 1 then
    invalid_arg "Console.Rule.v: glyph must occupy one cell";
  let style =
    match (style, theme) with
    | Some style, _ -> style
    | None, Some theme -> Style.fg (Theme.accent theme)
    | None, None -> Style.faint
  in
  let fill n = Span.styled style (repeat glyph n) in
  match label with
  | None -> fill width
  | Some label -> (
      let label = Span.sanitize ~keep_newlines:false label in
      let label_width = Span.width ~char_width label in
      if label_width > width then
        invalid_arg "Console.Rule.v: label wider than rule";
      let available = width - label_width in
      if available = 0 then label
      else
        match align with
        | `Left -> Span.(label ++ space ++ fill (available - 1))
        | `Right -> Span.(fill (available - 1) ++ space ++ label)
        | `Center when available = 1 -> Span.(label ++ space)
        | `Center ->
            let left = (available - 2) / 2 in
            let right = available - 2 - left in
            Span.(fill left ++ space ++ label ++ space ++ fill right))

let pp = Span.pp
let to_string = Span.to_string
let to_ansi_string t = Span.to_ansi_string t
let anim t = Anim.const (to_ansi_string t)
