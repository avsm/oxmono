(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fuzz tests for Tag module *)

open Crowbar
open Fuzz_common

(** Test that of_string never crashes on arbitrary input *)
let () =
  add_test ~name:"tag: of_string crash safety" [ bytes ] @@ fun buf ->
  let _ = Yamlrw.Tag.of_string buf in
  check true

(** Test of_string/to_string roundtrip *)
let () =
  add_test ~name:"tag: of_string/to_string roundtrip" [ bytes ] @@ fun buf ->
  match Yamlrw.Tag.of_string buf with
  | None -> check true (* Invalid tag, that's fine *)
  | Some tag ->
      let s = Yamlrw.Tag.to_string tag in
      (* Re-parse should succeed *)
      (match Yamlrw.Tag.of_string s with
      | None -> fail "re-parse of to_string output failed"
      | Some tag' ->
          if not (Yamlrw.Tag.equal tag tag') then fail "roundtrip mismatch"
          else check true)

(** Test to_uri never crashes for valid tags *)
let () =
  add_test ~name:"tag: to_uri after of_string" [ bytes ] @@ fun buf ->
  match Yamlrw.Tag.of_string buf with
  | None -> check true
  | Some tag ->
      let _ = Yamlrw.Tag.to_uri tag in
      check true

(** Test pp never crashes *)
let () =
  add_test ~name:"tag: pp" [ bytes ] @@ fun buf ->
  match Yamlrw.Tag.of_string buf with
  | None -> check true
  | Some tag ->
      let _ = Format.asprintf "%a" Yamlrw.Tag.pp tag in
      check true

(** Test equality is reflexive *)
let () =
  add_test ~name:"tag: equal reflexive" [ bytes ] @@ fun buf ->
  match Yamlrw.Tag.of_string buf with
  | None -> check true
  | Some tag ->
      if not (Yamlrw.Tag.equal tag tag) then fail "tag not equal to itself"
      else check true

(** Test compare is antisymmetric *)
let () =
  add_test ~name:"tag: compare antisymmetric" [ bytes; bytes ]
  @@ fun buf1 buf2 ->
  match (Yamlrw.Tag.of_string buf1, Yamlrw.Tag.of_string buf2) with
  | Some t1, Some t2 ->
      let cmp1 = Yamlrw.Tag.compare t1 t2 in
      let cmp2 = Yamlrw.Tag.compare t2 t1 in
      if cmp1 > 0 && cmp2 >= 0 then fail "compare not antisymmetric"
      else if cmp1 < 0 && cmp2 <= 0 then fail "compare not antisymmetric"
      else if cmp1 = 0 && cmp2 <> 0 then fail "compare not antisymmetric"
      else check true
  | _ -> check true

(** Test make function *)
let () =
  add_test ~name:"tag: make" [ ident_string; ident_string ]
  @@ fun handle suffix ->
  let tag = Yamlrw.Tag.make ~handle ~suffix in
  let _ = Yamlrw.Tag.to_string tag in
  let _ = Yamlrw.Tag.to_uri tag in
  check true

(** Test standard tags exist and have expected properties *)
let () =
  add_test ~name:"tag: standard tags" [ const () ] @@ fun () ->
  let tags =
    [
      (Yamlrw.Tag.null, Yamlrw.Tag.is_null);
      (Yamlrw.Tag.bool, Yamlrw.Tag.is_bool);
      (Yamlrw.Tag.int, Yamlrw.Tag.is_int);
      (Yamlrw.Tag.float, Yamlrw.Tag.is_float);
      (Yamlrw.Tag.str, Yamlrw.Tag.is_str);
      (Yamlrw.Tag.seq, Yamlrw.Tag.is_seq);
      (Yamlrw.Tag.map, Yamlrw.Tag.is_map);
    ]
  in
  List.iter
    (fun (tag, pred) ->
      if not (pred tag) then fail "standard tag predicate failed"
      else
        let _ = Yamlrw.Tag.to_string tag in
        let _ = Yamlrw.Tag.to_uri tag in
        ())
    tags;
  check true

(** Test tag predicates are mutually exclusive for standard tags *)
let () =
  add_test ~name:"tag: predicates mutually exclusive" [ const () ] @@ fun () ->
  let tags =
    [
      Yamlrw.Tag.null;
      Yamlrw.Tag.bool;
      Yamlrw.Tag.int;
      Yamlrw.Tag.float;
      Yamlrw.Tag.str;
      Yamlrw.Tag.seq;
      Yamlrw.Tag.map;
    ]
  in
  let predicates =
    [
      Yamlrw.Tag.is_null;
      Yamlrw.Tag.is_bool;
      Yamlrw.Tag.is_int;
      Yamlrw.Tag.is_float;
      Yamlrw.Tag.is_str;
      Yamlrw.Tag.is_seq;
      Yamlrw.Tag.is_map;
    ]
  in
  List.iter
    (fun tag ->
      let count = List.fold_left (fun n p -> if p tag then n + 1 else n) 0 predicates in
      if count <> 1 then fail "tag matched multiple predicates")
    tags;
  check true

let run () = ()
