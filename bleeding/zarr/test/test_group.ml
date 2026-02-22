(** Tests for group operations *)

open Alcotest
open Zarr_sync

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype

let test_create_group () =
  let store = Memory_store.create () in
  match Memory_group.create store ~path:"mygroup" () with
  | Error _ -> fail "should create group"
  | Ok group ->
    check string "path" "mygroup" (Memory_group.path group);
    check bool "metadata exists"
      true (Memory_store.exists store "mygroup/zarr.json")

let test_open_group () =
  let store = Memory_store.create () in
  (match Memory_group.create store ~path:"mygroup" () with
  | Error _ -> fail "should create group"
  | Ok _ -> ());

  match Memory_group.open_ store ~path:"mygroup" with
  | Error _ -> fail "should open group"
  | Ok group ->
    check string "path" "mygroup" (Memory_group.path group)

let test_group_not_found () =
  let store = Memory_store.create () in
  match Memory_group.open_ store ~path:"nonexistent" with
  | Error (`Not_found _) -> ()
  | Error _ -> fail "wrong error type"
  | Ok _ -> fail "should not find group"

let test_group_attributes () =
  let store = Memory_store.create () in
  match Memory_group.create store ~path:"mygroup"
    ~attributes:(Some (`Assoc [("foo", `String "bar")])) () with
  | Error _ -> fail "should create group"
  | Ok group ->
    let attrs = Memory_group.attrs group in
    match attrs with
    | `Assoc [("foo", `String "bar")] -> ()
    | _ -> fail "wrong attributes"

let test_group_set_attributes () =
  let store = Memory_store.create () in
  match Memory_group.create store ~path:"mygroup" () with
  | Error _ -> fail "should create group"
  | Ok group ->
    Memory_group.set_attrs group (`Assoc [("key", `Int 42)]);
    (* Reopen and check *)
    match Memory_group.open_ store ~path:"mygroup" with
    | Error _ -> fail "should open group"
    | Ok group2 ->
      let attrs = Memory_group.attrs group2 in
      match attrs with
      | `Assoc [("key", `Int 42)] -> ()
      | _ -> fail "wrong attributes after set"

let test_group_children () =
  let store = Memory_store.create () in

  (* Create parent group *)
  (match Memory_group.create store ~path:"parent" () with
  | Error _ -> fail "should create parent"
  | Ok _ -> ());

  (* Create child array *)
  (match Memory_array.create store
    ~path:"parent/child_array"
    ~shape:[|10|]
    ~chunks:[|10|]
    ~dtype:D.Int32
    () with
  | Error _ -> fail "should create child array"
  | Ok _ -> ());

  (* Create child group *)
  (match Memory_group.create store ~path:"parent/child_group" () with
  | Error _ -> fail "should create child group"
  | Ok _ -> ());

  (* List children *)
  match Memory_group.open_ store ~path:"parent" with
  | Error _ -> fail "should open parent"
  | Ok group ->
    let children = Memory_group.children group in
    check bool "has child_array" true (List.mem "child_array" children);
    check bool "has child_group" true (List.mem "child_group" children)

let test_group_child_type () =
  let store = Memory_store.create () in

  (* Create parent group *)
  (match Memory_group.create store ~path:"parent" () with
  | Error _ -> fail "should create parent"
  | Ok _ -> ());

  (* Create child array *)
  (match Memory_array.create store
    ~path:"parent/arr"
    ~shape:[|10|]
    ~chunks:[|10|]
    ~dtype:D.Int32
    () with
  | Error _ -> fail "should create child array"
  | Ok _ -> ());

  (* Create child group *)
  (match Memory_group.create store ~path:"parent/grp" () with
  | Error _ -> fail "should create child group"
  | Ok _ -> ());

  match Memory_group.open_ store ~path:"parent" with
  | Error _ -> fail "should open parent"
  | Ok group ->
    check (option (testable (fun fmt -> function
      | `Array -> Format.pp_print_string fmt "Array"
      | `Group -> Format.pp_print_string fmt "Group") (=)))
      "arr is array" (Some `Array) (Memory_group.child_type group "arr");
    check (option (testable (fun fmt -> function
      | `Array -> Format.pp_print_string fmt "Array"
      | `Group -> Format.pp_print_string fmt "Group") (=)))
      "grp is group" (Some `Group) (Memory_group.child_type group "grp");
    check (option (testable (fun fmt -> function
      | `Array -> Format.pp_print_string fmt "Array"
      | `Group -> Format.pp_print_string fmt "Group") (=)))
      "nonexistent" None (Memory_group.child_type group "nonexistent")

let test_hierarchy_walk () =
  let store = Memory_store.create () in

  (* Create a hierarchy *)
  (match Memory_group.create store ~path:"" () with
  | Error _ -> fail "should create root"
  | Ok _ -> ());
  (match Memory_group.create store ~path:"group1" () with
  | Error _ -> fail "should create group1"
  | Ok _ -> ());
  (match Memory_array.create store ~path:"group1/array1" ~shape:[|10|] ~chunks:[|10|] ~dtype:D.Int32 () with
  | Error _ -> fail "should create array1"
  | Ok _ -> ());

  let nodes = ref [] in
  Memory_hierarchy.walk store (fun path node_type ->
    nodes := (path, node_type) :: !nodes
  );

  check int "num nodes" 3 (List.length !nodes);
  check bool "has root" true (List.exists (fun (p, t) -> p = "/" && t = `Group) !nodes);
  check bool "has group1" true (List.exists (fun (p, t) -> p = "/group1" && t = `Group) !nodes);
  check bool "has array1" true (List.exists (fun (p, t) -> p = "/group1/array1" && t = `Array) !nodes)

let test_hierarchy_exists () =
  let store = Memory_store.create () in

  (match Memory_group.create store ~path:"mygroup" () with
  | Error _ -> fail "should create group"
  | Ok _ -> ());

  check bool "exists" true (Memory_hierarchy.exists store "mygroup");
  check bool "not exists" false (Memory_hierarchy.exists store "other")

let tests = [
  "create group", `Quick, test_create_group;
  "open group", `Quick, test_open_group;
  "group not found", `Quick, test_group_not_found;
  "group attributes", `Quick, test_group_attributes;
  "group set attributes", `Quick, test_group_set_attributes;
  "group children", `Quick, test_group_children;
  "group child type", `Quick, test_group_child_type;
  "hierarchy walk", `Quick, test_hierarchy_walk;
  "hierarchy exists", `Quick, test_hierarchy_exists;
]
