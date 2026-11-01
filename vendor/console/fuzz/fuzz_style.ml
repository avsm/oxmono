(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

let style n =
  let c = Color.ansi_256 (n land 255) in
  let parts =
    [
      Style.bold;
      Style.faint;
      Style.italic;
      Style.underline;
      Style.reverse;
      Style.strikethrough;
      Style.fg c;
      Style.bg c;
    ]
  in
  List.fold_left
    (fun acc (bit, s) ->
      if n land (1 lsl (bit + 8)) <> 0 then Style.(acc + s) else acc)
    Style.none
    (List.mapi (fun i s -> (i, s)) parts)

(* [none] is a unit of [+], [+] is associative, and the colours of its right
   argument win. *)
let test_laws a b c =
  let a = style a and b = style b and c = style c in
  check (Style.equal a Style.(a + none));
  check (Style.equal a Style.(none + a));
  check (Style.equal Style.(a + (b + c)) Style.(a + b + c));
  check (Style.equal Style.(a + b + b) Style.(a + b));
  Fmt.kstr ignore "%a" Style.pp a

(* [compare] is a total order that agrees with [equal], on styles and on
   their colours. *)
let test_order a b c =
  let order compare equal a b c =
    check (Bool.equal (compare a b = 0) (equal a b));
    check
      (Int.equal (Int.compare (compare a b) 0) (-Int.compare (compare b a) 0));
    if compare a b <= 0 && compare b c <= 0 then check (compare a c <= 0)
  in
  order Style.compare Style.equal (style a) (style b) (style c);
  let color n =
    match n mod 3 with
    | 0 -> Color.ansi_256 (n land 255)
    | 1 -> Color.rgb (n land 255) ((n lsr 8) land 255) ((n lsr 16) land 255)
    | _ -> List.nth Color.[ red; blue; bright_green ] ((n lsr 2) mod 3)
  in
  order Color.compare Color.equal (color a) (color b) (color c)

let suite =
  ( "style",
    [
      test_case "composition laws" [ int; int; int ] test_laws;
      test_case "compare agrees with equal" [ int; int; int ] test_order;
    ] )
