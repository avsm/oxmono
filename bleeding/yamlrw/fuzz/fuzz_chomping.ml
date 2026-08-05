(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fuzz tests for Chomping module *)

open Crowbar

(** Test of_char/to_char roundtrip for valid chars *)
let () =
  add_test ~name:"chomping: of_char/to_char roundtrip" [ uint8 ] @@ fun n ->
  let c = Char.chr n in
  match Yamlrw.Chomping.of_char c with
  | None -> check true (* Invalid char, that's fine *)
  | Some chomping -> (
      match Yamlrw.Chomping.to_char chomping with
      | None ->
          (* Clip has no char representation *)
          if chomping <> Yamlrw.Chomping.Clip then
            fail "non-Clip chomping should have char"
          else check true
      | Some c' ->
          if c <> c' then fail "roundtrip mismatch"
          else check true)

(** Test that to_string never crashes *)
let () =
  add_test ~name:"chomping: to_string Strip" [ const () ] @@ fun () ->
  let _ = Yamlrw.Chomping.to_string Yamlrw.Chomping.Strip in
  check true

let () =
  add_test ~name:"chomping: to_string Clip" [ const () ] @@ fun () ->
  let _ = Yamlrw.Chomping.to_string Yamlrw.Chomping.Clip in
  check true

let () =
  add_test ~name:"chomping: to_string Keep" [ const () ] @@ fun () ->
  let _ = Yamlrw.Chomping.to_string Yamlrw.Chomping.Keep in
  check true

(** Test pp never crashes *)
let () =
  add_test ~name:"chomping: pp" [ range 3 ] @@ fun n ->
  let chomping =
    match n with
    | 0 -> Yamlrw.Chomping.Strip
    | 1 -> Yamlrw.Chomping.Clip
    | _ -> Yamlrw.Chomping.Keep
  in
  let _ = Format.asprintf "%a" Yamlrw.Chomping.pp chomping in
  check true

(** Test equality is reflexive *)
let () =
  add_test ~name:"chomping: equal reflexive" [ range 3 ] @@ fun n ->
  let chomping =
    match n with
    | 0 -> Yamlrw.Chomping.Strip
    | 1 -> Yamlrw.Chomping.Clip
    | _ -> Yamlrw.Chomping.Keep
  in
  if not (Yamlrw.Chomping.equal chomping chomping) then
    fail "chomping not equal to itself"
  else check true

(** Test specific valid indicators *)
let () =
  add_test ~name:"chomping: strip indicator '-'" [ const () ] @@ fun () ->
  match Yamlrw.Chomping.of_char '-' with
  | Some Yamlrw.Chomping.Strip -> check true
  | _ -> fail "'-' should parse as Strip"

let () =
  add_test ~name:"chomping: keep indicator '+'" [ const () ] @@ fun () ->
  match Yamlrw.Chomping.of_char '+' with
  | Some Yamlrw.Chomping.Keep -> check true
  | _ -> fail "'+' should parse as Keep"

(** Test invalid chars return None *)
let () =
  add_test ~name:"chomping: invalid chars" [ const () ] @@ fun () ->
  let invalid_chars = [ 'a'; 'z'; '0'; '9'; ' '; '\n'; '#' ] in
  List.iter
    (fun c ->
      match Yamlrw.Chomping.of_char c with
      | None -> ()
      | Some _ -> fail (Printf.sprintf "char '%c' should not be valid" c))
    invalid_chars;
  check true

let run () = ()
