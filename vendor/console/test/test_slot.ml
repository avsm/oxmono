(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let fresh () = Format.formatter_of_buffer (Buffer.create 16)
let check_find = Alcotest.(check (option int))

(* A slot holds one value per formatter, the last one set, apart from the
   other slots. *)
let test_find () =
  let a = Slot.v () and b = Slot.v () in
  let ppf = fresh () and other = fresh () in
  check_find "empty" None (Slot.find a ppf);
  Slot.set a ppf 1;
  check_find "set" (Some 1) (Slot.find a ppf);
  Slot.set a ppf 2;
  check_find "set again" (Some 2) (Slot.find a ppf);
  check_find "another slot" None (Slot.find b ppf);
  Slot.set b ppf 3;
  check_find "both" (Some 2) (Slot.find a ppf);
  check_find "another formatter" None (Slot.find a other)

(* A formatter like another, as Fmt.with_buffer makes it, keeps its values. *)
let test_like () =
  let s = Slot.v () and ppf = fresh () in
  Slot.set s ppf 1;
  let like = Fmt.with_buffer ~like:ppf (Buffer.create 16) in
  check_find "carried over" (Some 1) (Slot.find s like)

(* Only Fmt and the slots that are settings change what [unset] sees, in
   whichever order they are set. *)
let test_unset () =
  let unset what want ppf = Alcotest.(check bool) what want (Slot.unset ppf) in
  let setting = Slot.v () and other = Slot.v ~setting:false () in
  unset "fresh" true (fresh ());
  let ppf = fresh () in
  Slot.set other ppf 1;
  unset "not a setting" true ppf;
  Slot.set setting ppf 1;
  unset "a setting" false ppf;
  let ppf = fresh () in
  Fmt.set_style_renderer ppf `None;
  unset "a style renderer" false ppf;
  Slot.set other ppf 1;
  unset "a style renderer under" false ppf;
  let ppf = fresh () in
  Slot.set other ppf 1;
  Fmt.set_utf_8 ppf false;
  unset "UTF-8 over" false ppf

let suite =
  ( "slot",
    [
      Alcotest.test_case "find" `Quick test_find;
      Alcotest.test_case "like" `Quick test_like;
      Alcotest.test_case "unset" `Quick test_unset;
    ] )
