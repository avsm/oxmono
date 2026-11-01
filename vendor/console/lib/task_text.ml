(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB" |]

(* [n] bytes as a number below 1024 of the unit [units.(i)]. *)
let scaled n =
  let rec pick i v =
    if v < 1024. || i = Array.length units - 1 then (v, i)
    else pick (i + 1) (v /. 1024.)
  in
  pick 0 (Float.max 0. (Int64.to_float n))

let bytes n =
  let v, i = scaled n in
  if i = 0 then Fmt.str "%d B" (int_of_float (Float.round v))
  else
    let hundredths v = int_of_float (Float.round (v *. 100.)) in
    (* A count that rounds to 1024.00 of a unit is 1.00 of the next. *)
    let h, i =
      let h = hundredths v in
      if h < 102_400 || i = Array.length units - 1 then (h, i)
      else (hundredths (v /. 1024.), i + 1)
    in
    Fmt.str "%d.%02d %s" (h / 100) (h mod 100) units.(i)

let bytes_short n =
  let v, i = scaled n in
  let v, i =
    if v >= 999.5 && i < Array.length units - 1 then (v /. 1024., i + 1)
    else (v, i)
  in
  let unit = units.(i).[0] in
  if i = 0 then Fmt.str "%d%c" (int_of_float v) unit
  else if v < 9.95 then Fmt.str "%.1f%c" v unit
  else Fmt.str "%.0f%c" v unit

(* Every duration under a minute, made once: a display shows one at each
   frame for each task. *)
let tenths =
  Array.init 600 (fun t ->
      string_of_int (t / 10) ^ "." ^ string_of_int (t mod 10) ^ "s")

let two n = if n < 10 then "0" ^ string_of_int n else string_of_int n

let duration secs =
  let secs =
    if Float.is_nan secs then 0. else Float.min 1e15 (Float.max 0. secs)
  in
  let t = Float.round (secs *. 10.) in
  if t < 600. then tenths.(int_of_float t)
  else
    let s = int_of_float (Float.round secs) in
    if s < 3600 then
      String.concat "" [ string_of_int (s / 60); "m"; two (s mod 60); "s" ]
    else
      let m = int_of_float (Float.round (secs /. 60.)) in
      String.concat "" [ string_of_int (m / 60); "h"; two (m mod 60); "m" ]

(* [duration] is six wide at its widest below a hundred hours ("59m59s",
   "23h59m"). A longer run takes the columns it needs rather than losing a
   digit. *)
let duration_cells = 6
let clean = Console_text.sanitize ~keep_newlines:false
let kept_bytes = 4096

(* ASCII dots, so that a line of ASCII cut stays ASCII, which the
   sanitizing of each redraw passes over at once. *)
let ellipsis = "..."

exception Cut of int

let capped s =
  if String.length s <= kept_bytes then s
  else
    let room = kept_bytes - String.length ellipsis in
    let cut =
      try
        Glyph.String.iter_graphemes
          (fun ~offset ~len ->
            if offset + len > room then raise_notrace (Cut offset))
          s;
        room
      with Cut i -> i
    in
    String.sub s 0 cut ^ ellipsis

let label s = capped (clean s)

let last_line_cells s =
  match String.rindex_opt s '\n' with
  | Some i ->
      Width.string_width (String.sub s (i + 1) (String.length s - i - 1))
  | None -> Width.string_width s

(* [s] sanitized, a tab gone to the next stop of eight columns from the
   start of its line. A tab cannot be inside a character or a line end, so
   the text between two is sanitized alone. *)
let expand_tabs s =
  match String.split_on_char '\t' s with
  | [] -> ""
  | first :: rest ->
      let buf = Buffer.create (String.length s + 16) in
      let first = Console_text.sanitize first in
      Buffer.add_string buf first;
      ignore
        (List.fold_left
           (fun col piece ->
             let stop = 8 - (col mod 8)
             and piece = Console_text.sanitize piece in
             Buffer.add_string buf (String.make stop ' ');
             Buffer.add_string buf piece;
             if String.contains piece '\n' then last_line_cells piece
             else col + stop + Width.string_width piece)
           (last_line_cells first) rest);
      Buffer.contents buf

let lines s =
  if s = "" then []
  else
    let lines = String.split_on_char '\n' (expand_tabs s) in
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
