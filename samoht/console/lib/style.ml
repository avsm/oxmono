(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type paint = Solid of Color.t | Gradient of Gradient.t

type t = {
  bold : bool;
  faint : bool;
  italic : bool;
  underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  fg : paint option;
  bg : paint option;
}

let none =
  {
    bold = false;
    faint = false;
    italic = false;
    underline = false;
    blink = false;
    reverse = false;
    strikethrough = false;
    fg = None;
    bg = None;
  }

let bold = { none with bold = true }
let faint = { none with faint = true }
let italic = { none with italic = true }
let underline = { none with underline = true }
let blink = { none with blink = true }
let reverse = { none with reverse = true }
let strikethrough = { none with strikethrough = true }
let fg color = { none with fg = Some (Solid color) }
let bg color = { none with bg = Some (Solid color) }
let fg_gradient g = { none with fg = Some (Gradient g) }
let bg_gradient g = { none with bg = Some (Gradient g) }
let merge_opt left right = match right with Some _ -> right | None -> left

let ( + ) left right =
  {
    bold = left.bold || right.bold;
    faint = left.faint || right.faint;
    italic = left.italic || right.italic;
    underline = left.underline || right.underline;
    blink = left.blink || right.blink;
    reverse = left.reverse || right.reverse;
    strikethrough = left.strikethrough || right.strikethrough;
    fg = merge_opt left.fg right.fg;
    bg = merge_opt left.bg right.bg;
  }

let merge = List.fold_left ( + ) none

(* Active boolean attributes paired with their ANSI code and human name.
   [bool_attrs] and [bool_attr_names] are filters over this list. *)
let active_bool_attrs style =
  let pick flag code name = if flag then Some (code, name) else None in
  List.filter_map
    (fun x -> x)
    [
      pick style.bold "1" "bold";
      pick style.faint "2" "faint";
      pick style.italic "3" "italic";
      pick style.underline "4" "underline";
      pick style.blink "5" "blink";
      pick style.reverse "7" "reverse";
      pick style.strikethrough "9" "strikethrough";
    ]

let bool_attrs style = List.map fst (active_bool_attrs style)

let resolve ~row ~column = function
  | Solid c -> c
  | Gradient g -> Gradient.cell g ~row ~column

let is_gradient = function Some (Gradient _) -> true | _ -> false
let has_gradient style = is_gradient style.fg || is_gradient style.bg

let shows_on_space style =
  style.bg <> None || style.reverse || style.underline || style.strikethrough

let at ~row ~column style =
  let solid = Option.map (fun p -> Solid (resolve ~row ~column p)) in
  { style with fg = solid style.fg; bg = solid style.bg }

let foreground style = Option.map (resolve ~row:0 ~column:0) style.fg
let background style = Option.map (resolve ~row:0 ~column:0) style.bg

let map_paint f = function
  | Solid c -> Solid (f c)
  | Gradient g -> Gradient (Gradient.map f g)

let map_color ?(fg = Fun.id) ?(bg = Fun.id) style =
  {
    style with
    fg = Option.map (map_paint fg) style.fg;
    bg = Option.map (map_paint bg) style.bg;
  }

let to_ansi style =
  let codes = bool_attrs style in
  let codes =
    match foreground style with
    | Some c -> codes @ [ Color.to_fg_code c ]
    | None -> codes
  in
  let codes =
    match background style with
    | Some c -> codes @ [ Color.to_bg_code c ]
    | None -> codes
  in
  match codes with
  | [] -> ""
  | _ -> Fmt.str "\027[%sm" (String.concat ";" codes)

let reset = "\027[0m"

let styled style pp ppf value =
  let ansi = to_ansi style in
  if ansi = "" || Fmt.style_renderer ppf = `None then pp ppf value
  else (
    Fmt.pf ppf "%s" ansi;
    pp ppf value;
    Fmt.pf ppf "%s" reset)

let is_none style =
  (not style.bold) && (not style.faint) && (not style.italic)
  && (not style.underline) && (not style.blink) && (not style.reverse)
  && (not style.strikethrough) && style.fg = None && style.bg = None

let opt_equal eq left right =
  match (left, right) with
  | None, None -> true
  | Some l, Some r -> eq l r
  | _ -> false

let equal_paint a b =
  match (a, b) with
  | Solid a, Solid b -> Color.equal a b
  | Gradient a, Gradient b -> Gradient.equal a b
  | _ -> false

let equal left right =
  Bool.equal left.bold right.bold
  && Bool.equal left.faint right.faint
  && Bool.equal left.italic right.italic
  && Bool.equal left.underline right.underline
  && Bool.equal left.blink right.blink
  && Bool.equal left.reverse right.reverse
  && Bool.equal left.strikethrough right.strikethrough
  && opt_equal equal_paint left.fg right.fg
  && opt_equal equal_paint left.bg right.bg

let bool_attr_names style = List.map snd (active_bool_attrs style)

let pp ppf style =
  if is_none style then Fmt.pf ppf "none"
  else
    let parts = bool_attr_names style in
    let pp_paint ppf = function
      | Solid c -> Color.pp ppf c
      | Gradient g -> Gradient.pp ppf g
    in
    let parts =
      match style.fg with
      | Some p -> parts @ [ Fmt.str "fg(%a)" pp_paint p ]
      | None -> parts
    in
    let parts =
      match style.bg with
      | Some p -> parts @ [ Fmt.str "bg(%a)" pp_paint p ]
      | None -> parts
    in
    Fmt.pf ppf "%s" (String.concat " + " parts)
