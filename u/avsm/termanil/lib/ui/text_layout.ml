(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
module M = Termanil_model
module S = Stdlib
module Buffer = Stdlib.Buffer
module Uchar = Stdlib.Uchar

let wrap ~width text =
  let width = max 1 width in
  let text = M.safe_text text in
  let lines = ref [] and b = Buffer.create width and column = ref 0 in
  let flush () =
    lines := Buffer.contents b :: !lines;
    Buffer.clear b;
    column := 0
  in
  let add u =
    let w = max 0 (View.uchar_tty_width u) in
    if !column > 0 && !column + w > width then flush ();
    Buffer.add_utf_8_uchar b u;
    column := !column + w
  in
  let rec loop i =
    if i < String.length text then (
      let d = S.String.get_utf_8_uchar text i in
      let u = Uchar.utf_decode_uchar d in
      (match Uchar.to_int u with
      | 10 -> flush ()
      | 9 ->
          for _ = 1 to 4 do
            add (Uchar.of_char ' ')
          done
      | _ -> add u);
      loop (i + Uchar.utf_decode_length d))
  in
  loop 0;
  flush ();
  List.rev !lines

let one_line s =
  M.safe_text s |> String.map ~f:(function '\n' | '\r' | '\t' -> ' ' | c -> c)

let text ?(attrs = []) s = View.text ~attrs (one_line s)

let fit ~width ~height view =
  let width = max 0 width and height = max 0 height in
  let view =
    View.crop
      ~r:(max 0 (View.width view - width))
      ~b:(max 0 (View.height view - height))
      view
  in
  View.zcat [ view; View.transparent_rectangle ~width ~height ]
