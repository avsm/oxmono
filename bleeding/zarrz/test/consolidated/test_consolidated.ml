(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Zarrz

(* Root documents are built here rather than read from a fixture, so
   that a malformed map is as easy to write as a well formed one. *)

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.fail m

let string_of_json j =
  match Jsont_bytesrw.encode_string Jsont.json j with
  | Ok s -> s
  | Error m -> Alcotest.fail m

let group_doc = {|{"zarr_format":3,"node_type":"group"}|}
let typeless_doc = {|{"zarr_format":3}|}

let array_doc =
  {|{"zarr_format":3,"node_type":"array","shape":[8],|}
  ^ {|"data_type":"uint16",|}
  ^ {|"chunk_grid":{"name":"regular","configuration":{"chunk_shape":[4]}},|}
  ^ {|"chunk_key_encoding":{"name":"default"},"fill_value":0,|}
  ^ {|"codecs":[{"name":"bytes","configuration":{"endian":"little"}}]}|}

let mem n v = (Jsont.Json.name n, v)

let root ?(kind = "inline") ?(with_member = true) nodes =
  let cm =
    Jsont.Json.object'
      [
        mem "kind" (Jsont.Json.string kind);
        mem "must_understand" (Jsont.Json.bool false);
        mem "metadata"
          (Jsont.Json.object'
             (List.map (fun (p, d) -> mem p (json_of_string d)) nodes));
      ]
  in
  Jsont.Json.object'
    ([ mem "zarr_format" (Jsont.Json.int 3);
       mem "node_type" (Jsont.Json.string "group") ]
    @ if with_member then [ mem "consolidated_metadata" cm ] else [])

let group j =
  match Metadata.group_of_json j with
  | Ok g -> g
  | Error m -> Alcotest.fail m

let some j =
  match Consolidated.of_group (group j) with
  | Some c -> c
  | None -> Alcotest.fail "no consolidated metadata"

(* Deliberately not in alphabetical order, so that a test of document
   order cannot pass by accident. *)
let store_nodes =
  [
    ("utm30", group_doc);
    ("utm30/band", array_doc);
    ("utm30/embeddings", array_doc);
    ("global_rgb", group_doc);
    ("global_rgb/0", typeless_doc);
  ]

let strings = Alcotest.(list string)

let kinds =
  let pp ppf (n, k) =
    Format.fprintf ppf "%s:%s" n
      (match k with `Array -> "array" | `Group -> "group")
  in
  Alcotest.testable (Format.pp_print_list pp) ( = )

(* -- Presence of the member ------------------------------------------ *)

let test_absent_member () =
  Alcotest.(check bool)
    "a root without the member has no map" true
    (Consolidated.of_group (group (root ~with_member:false [])) = None)

let test_kind_mismatch () =
  Alcotest.(check bool)
    "only the inline kind is readable" true
    (Consolidated.of_group (group (root ~kind:"hierarchical" store_nodes))
    = None)

let test_empty_map () =
  Alcotest.check strings "an inline map may hold nothing" []
    (Consolidated.paths (some (root [])))

(* -- Paths ----------------------------------------------------------- *)

let test_paths_keep_document_order () =
  Alcotest.check strings "document order, not sorted"
    [ "utm30"; "utm30/band"; "utm30/embeddings"; "global_rgb"; "global_rgb/0" ]
    (Consolidated.paths (some (root store_nodes)))

let test_paths_lose_slashes () =
  Alcotest.check strings "as written, without the slashes"
    [ "utm30"; "utm30/band" ]
    (Consolidated.paths
       (some (root [ ("/utm30", group_doc); ("utm30/band/", array_doc) ])))

let test_repeated_path_keeps_the_first () =
  let c =
    some (root [ ("a", group_doc); ("a", array_doc); ("b", array_doc) ])
  in
  Alcotest.check strings "one entry a path" [ "a"; "b" ] (Consolidated.paths c);
  Alcotest.check kinds "the first document of a path wins"
    [ ("a", `Group); ("b", `Array) ]
    (Consolidated.children c "")

(* -- Node lookup ----------------------------------------------------- *)

let test_node_lookup () =
  let c = some (root store_nodes) in
  Alcotest.(check bool)
    "a node without a leading slash" true
    (Consolidated.node c "utm30/band" <> None);
  Alcotest.(check bool)
    "a leading slash is tolerated" true
    (Consolidated.node c "/utm30/band" <> None);
  Alcotest.(check bool)
    "so is a trailing one" true
    (Consolidated.node c "/utm30/band/" <> None);
  Alcotest.(check bool)
    "an absent node" true
    (Consolidated.node c "utm31" = None);
  Alcotest.(check bool)
    "the root is not in the map" true
    (Consolidated.node c "/" = None)

let test_node_json_is_kept_raw () =
  let c = some (root store_nodes) in
  let j = Option.get (Consolidated.node c "utm30/band") in
  Alcotest.(check string)
    "the document is the one that was written" array_doc (string_of_json j);
  match Metadata.array_of_json j with
  | Ok m -> Alcotest.(check int) "and it parses" 8 m.Metadata.shape.(0)
  | Error m -> Alcotest.fail m

(* -- Children -------------------------------------------------------- *)

let test_children_of_root () =
  Alcotest.check kinds "the two top level nodes, in document order"
    [ ("utm30", `Group); ("global_rgb", `Group) ]
    (Consolidated.children (some (root store_nodes)) "")

let test_children_spellings_of_root () =
  let c = some (root store_nodes) in
  Alcotest.check kinds "an empty path and a slash name the same node"
    (Consolidated.children c "")
    (Consolidated.children c "/")

let test_children_are_tagged () =
  let c = some (root store_nodes) in
  Alcotest.check kinds "an array is tagged as one"
    [ ("band", `Array); ("embeddings", `Array) ]
    (Consolidated.children c "utm30");
  Alcotest.check kinds "a document with no node_type is a group"
    [ ("0", `Group) ]
    (Consolidated.children c "/global_rgb/")

let test_children_of_a_leaf () =
  Alcotest.check kinds "an array has none" []
    (Consolidated.children (some (root store_nodes)) "utm30/band")

let test_children_below_a_missing_parent () =
  (* A map that skipped the intermediate group still enumerates below
     it, which is what a reader walking a partial map needs. *)
  let c = some (root [ ("a/b/c", array_doc) ]) in
  Alcotest.check kinds "the child is found" [ ("c", `Array) ]
    (Consolidated.children c "a/b");
  Alcotest.check kinds "the skipped group is not invented" []
    (Consolidated.children c "")

let suite =
  [
    ("absent member", `Quick, test_absent_member);
    ("kind mismatch", `Quick, test_kind_mismatch);
    ("empty map", `Quick, test_empty_map);
    ("paths keep document order", `Quick, test_paths_keep_document_order);
    ("paths lose slashes", `Quick, test_paths_lose_slashes);
    ("repeated path", `Quick, test_repeated_path_keeps_the_first);
    ("node lookup", `Quick, test_node_lookup);
    ("node json is raw", `Quick, test_node_json_is_kept_raw);
    ("children of the root", `Quick, test_children_of_root);
    ("children of the root, spelled two ways", `Quick,
     test_children_spellings_of_root);
    ("children are tagged", `Quick, test_children_are_tagged);
    ("children of a leaf", `Quick, test_children_of_a_leaf);
    ("children below a missing parent", `Quick,
     test_children_below_a_missing_parent);
  ]

let () = Alcotest.run "consolidated" [ ("consolidated", suite) ]
