(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ansi =
  [ `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `Bright_black
  | `Bright_red
  | `Bright_green
  | `Bright_yellow
  | `Bright_blue
  | `Bright_magenta
  | `Bright_cyan
  | `Bright_white ]

type color = Ansi of ansi | Rgb of int * int * int | Palette of int

type 'c style = {
  bold : bool;
  faint : bool;
  italic : bool;
  underline : bool;
  reverse : bool;
  strikethrough : bool;
  fg : 'c option;
  bg : 'c option;
}

let ansi_position = function
  | `Black | `Bright_black -> 0
  | `Red | `Bright_red -> 1
  | `Green | `Bright_green -> 2
  | `Yellow | `Bright_yellow -> 3
  | `Blue | `Bright_blue -> 4
  | `Magenta | `Bright_magenta -> 5
  | `Cyan | `Bright_cyan -> 6
  | `White | `Bright_white -> 7

let is_bright = function
  | `Bright_black | `Bright_red | `Bright_green | `Bright_yellow | `Bright_blue
  | `Bright_magenta | `Bright_cyan | `Bright_white ->
      true
  | _ -> false

let code ~normal ~bright ~extended = function
  | Ansi c ->
      string_of_int ((if is_bright c then bright else normal) + ansi_position c)
  | Rgb (r, g, b) ->
      String.concat ";" (List.map string_of_int [ extended; 2; r; g; b ])
  | Palette index ->
      String.concat ";" (List.map string_of_int [ extended; 5; index ])

let attributes s =
  List.filter_map
    (fun (set, code, name) -> if set then Some (code, name) else None)
    [
      (s.bold, "1", "bold");
      (s.faint, "2", "faint");
      (s.italic, "3", "italic");
      (s.underline, "4", "underline");
      (s.reverse, "7", "reverse");
      (s.strikethrough, "9", "strikethrough");
    ]

let start s =
  let colour ~normal ~bright ~extended c =
    Option.to_list (Option.map (code ~normal ~bright ~extended) c)
  in
  match
    List.map fst (attributes s)
    @ colour ~normal:30 ~bright:90 ~extended:38 s.fg
    @ colour ~normal:40 ~bright:100 ~extended:48 s.bg
  with
  | [] -> ""
  | codes -> "\027[" ^ String.concat ";" codes ^ "m"

let reset = "\027[0m"
