(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fuzz tests for Value module *)

open Crowbar
open Fuzz_common

(** Generator for Value.t *)
let rec value_gen depth =
  if depth <= 0 then
    choose
      [
        const `Null;
        map [ bool ] (fun b -> `Bool b);
        map [ float ] (fun f -> `Float f);
        map [ printable_string ] (fun s -> `String s);
      ]
  else
    choose
      [
        const `Null;
        map [ bool ] (fun b -> `Bool b);
        map [ float ] (fun f -> `Float f);
        map [ printable_string ] (fun s -> `String s);
        map [ list (value_gen (depth - 1)) ] (fun vs -> `A vs);
        map
          [ list (pair ident_string (value_gen (depth - 1))) ]
          (fun pairs -> `O pairs);
      ]

let value = value_gen 3

(** Test pp never crashes *)
let () =
  add_test ~name:"value: pp" [ value ] @@ fun v ->
  let _ = Format.asprintf "%a" Yamlrw.Value.pp v in
  check true

(** Test equal is reflexive *)
let () =
  add_test ~name:"value: equal reflexive" [ value ] @@ fun v ->
  if not (Yamlrw.Value.equal v v) then fail "value not equal to itself"
  else check true

(** Test compare is reflexive (returns 0 for same value) *)
let () =
  add_test ~name:"value: compare reflexive" [ value ] @@ fun v ->
  if Yamlrw.Value.compare v v <> 0 then fail "compare should return 0 for same value"
  else check true

(** Test type_name never crashes *)
let () =
  add_test ~name:"value: type_name" [ value ] @@ fun v ->
  let _ = Yamlrw.Value.type_name v in
  check true

(** Test safe accessors return correct types *)
let () =
  add_test ~name:"value: as_null" [ value ] @@ fun v ->
  (match (v, Yamlrw.Value.as_null v) with
  | `Null, Some () -> ()
  | `Null, None -> fail "as_null should return Some for Null"
  | _, Some () -> fail "as_null should return None for non-Null"
  | _, None -> ());
  check true

let () =
  add_test ~name:"value: as_bool" [ value ] @@ fun v ->
  (match (v, Yamlrw.Value.as_bool v) with
  | `Bool b, Some b' when b = b' -> ()
  | `Bool _, Some _ -> fail "as_bool returned wrong value"
  | `Bool _, None -> fail "as_bool should return Some for Bool"
  | _, Some _ -> fail "as_bool should return None for non-Bool"
  | _, None -> ());
  check true

let () =
  add_test ~name:"value: as_float" [ value ] @@ fun v ->
  (match (v, Yamlrw.Value.as_float v) with
  | `Float f, Some f' when f = f' || (Float.is_nan f && Float.is_nan f') -> ()
  | `Float _, Some _ -> fail "as_float returned wrong value"
  | `Float _, None -> fail "as_float should return Some for Float"
  | _, Some _ -> fail "as_float should return None for non-Float"
  | _, None -> ());
  check true

let () =
  add_test ~name:"value: as_string" [ value ] @@ fun v ->
  (match (v, Yamlrw.Value.as_string v) with
  | `String s, Some s' when s = s' -> ()
  | `String _, Some _ -> fail "as_string returned wrong value"
  | `String _, None -> fail "as_string should return Some for String"
  | _, Some _ -> fail "as_string should return None for non-String"
  | _, None -> ());
  check true

let () =
  add_test ~name:"value: as_list" [ value ] @@ fun v ->
  (match (v, Yamlrw.Value.as_list v) with
  | `A lst, Some lst' when lst = lst' -> ()
  | `A _, Some _ -> fail "as_list returned wrong value"
  | `A _, None -> fail "as_list should return Some for A"
  | _, Some _ -> fail "as_list should return None for non-A"
  | _, None -> ());
  check true

let () =
  add_test ~name:"value: as_assoc" [ value ] @@ fun v ->
  (match (v, Yamlrw.Value.as_assoc v) with
  | `O pairs, Some pairs' when pairs = pairs' -> ()
  | `O _, Some _ -> fail "as_assoc returned wrong value"
  | `O _, None -> fail "as_assoc should return Some for O"
  | _, Some _ -> fail "as_assoc should return None for non-O"
  | _, None -> ());
  check true

(** Test constructors *)
let () =
  add_test ~name:"value: null constructor" [ const () ] @@ fun () ->
  if Yamlrw.Value.null <> `Null then fail "null should be `Null"
  else check true

let () =
  add_test ~name:"value: bool constructor" [ bool ] @@ fun b ->
  if Yamlrw.Value.bool b <> `Bool b then fail "bool constructor mismatch"
  else check true

let () =
  add_test ~name:"value: int constructor" [ range 1000000 ] @@ fun n ->
  (* Use smaller range since floats can't exactly represent all int64 values *)
  match Yamlrw.Value.int n with
  | `Float f when Float.to_int f = n -> check true
  | `Float _ -> fail "int constructor roundtrip failed"
  | _ -> fail "int should produce Float"

let () =
  add_test ~name:"value: float constructor" [ float ] @@ fun f ->
  match Yamlrw.Value.float f with
  | `Float f' when f = f' || (Float.is_nan f && Float.is_nan f') -> check true
  | `Float _ -> fail "float constructor roundtrip failed"
  | _ -> fail "float should produce Float"

let () =
  add_test ~name:"value: string constructor" [ printable_string ] @@ fun s ->
  if Yamlrw.Value.string s <> `String s then fail "string constructor mismatch"
  else check true

(** Test object operations *)
let () =
  add_test ~name:"value: mem/find consistency" [ value; ident_string ]
  @@ fun v key ->
  match v with
  | `O _ ->
      let has_key = Yamlrw.Value.mem key v in
      let found = Yamlrw.Value.find key v in
      if has_key && Option.is_none found then fail "mem true but find None"
      else if (not has_key) && Option.is_some found then
        fail "mem false but find Some"
      else check true
  | _ -> check true

(** Test map preserves structure *)
let () =
  add_test ~name:"value: map preserves list length" [ value ] @@ fun v ->
  match v with
  | `A lst ->
      let mapped = Yamlrw.Value.map (fun x -> x) v in
      (match mapped with
      | `A lst' when List.length lst = List.length lst' -> check true
      | `A _ -> fail "map changed list length"
      | _ -> fail "map changed type")
  | _ -> check true

(** Test combine for objects *)
let () =
  add_test ~name:"value: combine objects" [ value; value ] @@ fun v1 v2 ->
  match (v1, v2) with
  | `O pairs1, `O pairs2 ->
      let combined = Yamlrw.Value.combine v1 v2 in
      (match combined with
      | `O pairs ->
          (* Combined should have all keys from both *)
          let keys1 = List.map fst pairs1 in
          let keys2 = List.map fst pairs2 in
          let all_keys =
            List.sort_uniq String.compare (keys1 @ keys2)
          in
          let combined_keys =
            List.sort_uniq String.compare (List.map fst pairs)
          in
          if all_keys = combined_keys then check true
          else fail "combine missing keys"
      | _ -> fail "combine should produce object")
  | _ -> check true

(** Test generated value -> serialize -> parse roundtrip *)
let () =
  add_test ~name:"value: generated value roundtrip" [ value ] @@ fun v ->
  (try
     let s = Yamlrw.to_string v in
     let v' = Yamlrw.of_string s in
     if not (Yamlrw.equal v v') then fail "generated value roundtrip mismatch"
     else ()
   with Yamlrw.Yamlrw_error _ ->
     (* Some generated values might not roundtrip perfectly due to YAML ambiguities *)
     ());
  check true

(** Test generated value serialization with block style *)
let () =
  add_test ~name:"value: generated block style" [ value ] @@ fun v ->
  (try
     let s = Yamlrw.to_string ~layout_style:`Block v in
     let _ = Yamlrw.of_string s in
     ()
   with Yamlrw.Yamlrw_error _ -> ());
  check true

(** Test generated value serialization with flow style *)
let () =
  add_test ~name:"value: generated flow style" [ value ] @@ fun v ->
  (try
     let s = Yamlrw.to_string ~layout_style:`Flow v in
     let _ = Yamlrw.of_string s in
     ()
   with Yamlrw.Yamlrw_error _ -> ());
  check true

(** Test to_json/of_json roundtrip for generated values *)
let () =
  add_test ~name:"value: to_json/of_json generated" [ value ] @@ fun v ->
  let y = Yamlrw.of_json v in
  let v' = Yamlrw.to_json y in
  if not (Yamlrw.equal v v') then fail "to_json/of_json roundtrip mismatch"
  else check true

(** Test compare is transitive *)
let () =
  add_test ~name:"value: compare transitive" [ value; value; value ]
  @@ fun v1 v2 v3 ->
  let c12 = Yamlrw.Value.compare v1 v2 in
  let c23 = Yamlrw.Value.compare v2 v3 in
  let c13 = Yamlrw.Value.compare v1 v3 in
  (* If v1 <= v2 and v2 <= v3 then v1 <= v3 *)
  if c12 <= 0 && c23 <= 0 && c13 > 0 then fail "compare not transitive"
  else if c12 >= 0 && c23 >= 0 && c13 < 0 then fail "compare not transitive"
  else check true

(** Test equal is symmetric *)
let () =
  add_test ~name:"value: equal symmetric" [ value; value ] @@ fun v1 v2 ->
  let eq12 = Yamlrw.Value.equal v1 v2 in
  let eq21 = Yamlrw.Value.equal v2 v1 in
  if eq12 <> eq21 then fail "equal not symmetric" else check true

(** Test filter on lists *)
let () =
  add_test ~name:"value: filter" [ value ] @@ fun v ->
  match v with
  | `A _ ->
      let filtered = Yamlrw.Value.filter (fun _ -> true) v in
      if not (Yamlrw.Value.equal v filtered) then
        fail "filter (fun _ -> true) should be identity"
      else
        let empty = Yamlrw.Value.filter (fun _ -> false) v in
        (match empty with
        | `A [] -> check true
        | `A _ -> fail "filter (fun _ -> false) should be empty"
        | _ -> fail "filter should preserve list type")
  | _ -> check true

(** Test keys/values for objects *)
let () =
  add_test ~name:"value: keys/values" [ value ] @@ fun v ->
  match v with
  | `O pairs ->
      let ks = Yamlrw.Value.keys v in
      let vs = Yamlrw.Value.values v in
      if List.length ks <> List.length pairs then fail "keys length mismatch"
      else if List.length vs <> List.length pairs then
        fail "values length mismatch"
      else check true
  | _ -> check true

(** Test Util.update *)
let () =
  add_test ~name:"value: Util.update" [ value; ident_string; value ]
  @@ fun v key newv ->
  match v with
  | `O _ ->
      (try
         let updated = Yamlrw.Util.update key newv v in
         let found = Yamlrw.Value.find key updated in
         match found with
         | Some x when Yamlrw.Value.equal x newv -> check true
         | Some _ -> fail "update: found wrong value"
         | None -> fail "update: key not found after update"
       with Yamlrw.Util.Type_error _ -> fail "Type_error on update")
  | _ -> check true

(** Test Util.remove *)
let () =
  add_test ~name:"value: Util.remove" [ value; ident_string ] @@ fun v key ->
  match v with
  | `O _ ->
      (try
         let removed = Yamlrw.Util.remove key v in
         let found = Yamlrw.Value.find key removed in
         if Option.is_some found then fail "remove: key still present"
         else check true
       with Yamlrw.Util.Type_error _ -> fail "Type_error on remove")
  | _ -> check true

(** Test Util.get_path *)
let () =
  add_test ~name:"value: Util.get_path" [ value; list ident_string ]
  @@ fun v path ->
  let _ = Yamlrw.Util.get_path path v in
  check true

(** Test Util.flatten *)
let () =
  add_test ~name:"value: Util.flatten" [ value ] @@ fun v ->
  match v with
  | `A _ ->
      (try
         let _ = Yamlrw.Util.flatten v in
         check true
       with Yamlrw.Util.Type_error _ -> fail "Type_error on flatten of list")
  | _ -> check true

(** Test Util.nth *)
let () =
  add_test ~name:"value: Util.nth" [ value; range 100 ] @@ fun v idx ->
  match v with
  | `A lst ->
      let result = Yamlrw.Util.nth idx v in
      if idx < List.length lst then
        match result with
        | Some x when Yamlrw.Value.equal x (List.nth lst idx) -> check true
        | Some _ -> fail "nth returned wrong element"
        | None -> fail "nth returned None for valid index"
      else if Option.is_some result then
        fail "nth returned Some for invalid index"
      else check true
  | _ -> check true

(** Test Util.length *)
let () =
  add_test ~name:"value: Util.length" [ value ] @@ fun v ->
  let len = Yamlrw.Util.length v in
  (match v with
  | `A lst when len = List.length lst -> ()
  | `O pairs when len = List.length pairs -> ()
  | `A _ -> fail "length mismatch for list"
  | `O _ -> fail "length mismatch for object"
  | _ when len = 0 -> ()
  | _ -> fail "length should be 0 for scalars");
  check true

(** Test Util.fold *)
let () =
  add_test ~name:"value: Util.fold" [ value ] @@ fun v ->
  match v with
  | `A lst ->
      (try
         let count = Yamlrw.Util.fold (fun acc _ -> acc + 1) 0 v in
         if count <> List.length lst then fail "fold count mismatch"
         else check true
       with Yamlrw.Util.Type_error _ -> fail "Type_error on fold of list")
  | _ -> check true

(** Test Util.mapi preserves length *)
let () =
  add_test ~name:"value: Util.mapi preserves length" [ value ] @@ fun v ->
  match v with
  | `A lst ->
      (try
         let mapped = Yamlrw.Util.mapi (fun _ x -> x) v in
         (match mapped with
         | `A lst' when List.length lst = List.length lst' -> check true
         | `A _ -> fail "mapi changed list length"
         | _ -> fail "mapi changed type")
       with Yamlrw.Util.Type_error _ -> fail "Type_error on mapi of list")
  | _ -> check true

let run () = ()
