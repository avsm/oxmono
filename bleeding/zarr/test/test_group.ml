(** Tests for group operations *)

open Alcotest
open Zarr_sync

let none = Jsont.Meta.none
let jstr s = Jsont.String (s, none)
let jint i = Jsont.Number (float_of_int i, none)
let jobj ms = Jsont.Object (ms, none)
let jmem n v : Jsont.mem = ((n, none), v)

let test_create_group () =
  let store = Memory_store.create () in
  let group = Memory_group.create store ~path:"mygroup" () in
  check string "path" "mygroup" (Memory_group.path group);
  check bool "metadata exists"
    true (Memory_store.exists store "mygroup/zarr.json")

let test_open_group () =
  let store = Memory_store.create () in
  let _ = Memory_group.create store ~path:"mygroup" () in
  let group = Memory_group.open_ store ~path:"mygroup" in
  check string "path" "mygroup" (Memory_group.path group)

let test_group_not_found () =
  let store = Memory_store.create () in
  (try
    let _ = Memory_group.open_ store ~path:"nonexistent" in
    fail "should not find group"
  with Failure _ -> ())

let test_group_attributes () =
  let store = Memory_store.create () in
  let group = Memory_group.create store ~path:"mygroup"
    ~attributes:(jobj [jmem "foo" (jstr "bar")]) () in
  let attrs = Memory_group.attrs group in
  match attrs with
  | Jsont.Object ([(("foo", _), Jsont.String ("bar", _))], _) -> ()
  | _ -> fail "wrong attributes"

let test_group_set_attributes () =
  let store = Memory_store.create () in
  let group = Memory_group.create store ~path:"mygroup" () in
  Memory_group.set_attrs group (jobj [jmem "key" (jint 42)]);
  (* Reopen and check *)
  let group2 = Memory_group.open_ store ~path:"mygroup" in
  let attrs = Memory_group.attrs group2 in
  match attrs with
  | Jsont.Object ([(("key", _), Jsont.Number (42., _))], _) -> ()
  | _ -> fail "wrong attributes after set"

let test_group_children () =
  let store = Memory_store.create () in

  (* Create parent group *)
  let _ = Memory_group.create store ~path:"parent" () in

  (* Create child array *)
  let _ = Memory_array.create store
    ~path:"parent/child_array"
    ~shape:[|10|]
    ~chunks:[|10|]
    ~dtype:Zarr.Dtype.Int32
    () in

  (* Create child group *)
  let _ = Memory_group.create store ~path:"parent/child_group" () in

  (* List children *)
  let group = Memory_group.open_ store ~path:"parent" in
  let children = Memory_group.children group in
  check bool "has child_array" true (List.mem "child_array" children);
  check bool "has child_group" true (List.mem "child_group" children)

let test_group_child_type () =
  let store = Memory_store.create () in

  (* Create parent group *)
  let _ = Memory_group.create store ~path:"parent" () in

  (* Create child array *)
  let _ = Memory_array.create store
    ~path:"parent/arr"
    ~shape:[|10|]
    ~chunks:[|10|]
    ~dtype:Zarr.Dtype.Int32
    () in

  (* Create child group *)
  let _ = Memory_group.create store ~path:"parent/grp" () in

  let group = Memory_group.open_ store ~path:"parent" in
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
  let _ = Memory_group.create store ~path:"" () in
  let _ = Memory_group.create store ~path:"group1" () in
  let _ = Memory_array.create store ~path:"group1/array1" ~shape:[|10|] ~chunks:[|10|] ~dtype:Zarr.Dtype.Int32 () in

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

  let _ = Memory_group.create store ~path:"mygroup" () in

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
