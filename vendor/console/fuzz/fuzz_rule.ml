(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Cell text whose bytes, clusters and cells do not line up. *)
let pool =
  [|
    "x";
    "abc";
    "\xe7\x95\x8c\xe7\x95\x8c";
    "e\xcc\x81";
    "\xf0\x9f\x8e\x89 ok";
    "a b c d e f";
    "src/lib/a.ml";
    "\xff\xfe";
    "\t\007bell";
    "";
    "a long cell that will not fit in a narrow column at all";
  |]

let pick s i =
  let s = s ^ "\000" in
  pool.((Char.code s.[i mod String.length s] + i) mod Array.length pool)

(* What [pp] prints at [margin], in colour, as lines without their escapes. *)
let render pp margin v =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Fmt.set_style_renderer ppf `Ansi_tty;
  pp ppf v;
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buf in
  (raw, List.map Console_text.strip (String.split_on_char (Char.chr 10) raw))

let cells s = Width.string_width s

(* Every style a widget turns on, it turns off before the line ends. *)
let check_closed raw =
  List.iter
    (fun l ->
      match String.rindex_opt l (Char.chr 27) with
      | None -> ()
      | Some i ->
          let rest = String.sub l i (String.length l - i) in
          if
            not
              (String.starts_with ~prefix:"\027[0m" rest
              || String.starts_with ~prefix:"\027]8;;\027" rest)
          then failf "style left on: %S" l)
    (String.split_on_char (Char.chr 10) raw)

(* A rule is as wide as the margin; a glyph that is not one cell, its
   controls made spaces, is refused. *)
let test_rule s n =
  let margin = 1 + (abs n mod 80) in
  let glyph = pick s 0 and label = View.text (pick s 1) in
  match Rule.view ~glyph ~label () with
  | r ->
      let raw, lines = render View.pp margin r in
      check_closed raw;
      check_eq ~pp:Fmt.int margin (cells (List.hd lines))
  | exception Invalid_argument _ ->
      check (cells (Console_text.sanitize ~keep_newlines:false glyph) <> 1)

let suite =
  ("rule", [ test_case "as wide as the margin" [ bytes; int ] test_rule ])
