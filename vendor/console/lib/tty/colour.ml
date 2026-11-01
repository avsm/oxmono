(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The rules of cmdliner and fmt: NO_COLOR set and not empty, or a TERM that
   is unset or dumb, turn colour off; otherwise a terminal gets colour. *)
let dumb ~getenv =
  match getenv "TERM" with None | Some ("" | "dumb") -> true | Some _ -> false

let renderer ~getenv ~is_tty =
  match getenv "NO_COLOR" with
  | Some v when v <> "" -> `None
  | _ -> if dumb ~getenv || not is_tty then `None else `Ansi_tty

let has ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  at 0

let depth ~getenv =
  match (getenv "COLORTERM", getenv "TERM") with
  | Some ("truecolor" | "24bit"), _ -> `True_color
  | _, Some term when String.ends_with ~suffix:"-direct" term -> `True_color
  | _, Some term when has ~affix:"256color" term -> `Ansi_256
  | _ -> `Ansi_16

(* The locale's character set is that of the first of these variables that
   is set and not empty, as setlocale(3) reads them. *)
let utf_8 ~getenv =
  let set name = match getenv name with Some "" -> None | v -> v in
  match List.find_map set [ "LC_ALL"; "LC_CTYPE"; "LANG" ] with
  | None -> false
  | Some locale ->
      let l = String.lowercase_ascii locale in
      has ~affix:"utf-8" l || has ~affix:"utf8" l
