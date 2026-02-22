(** Tests for memory store *)

open Alcotest
open Zarr_sync

let test_create () =
  let store = Memory_store.create () in
  check int "empty store" 0 (Memory_store.length store)

let test_get_set () =
  let store = Memory_store.create () in
  check (option bytes) "get missing" None (Memory_store.get store "key1");

  Memory_store.set store "key1" (Bytes.of_string "value1");
  check (option bytes) "get existing"
    (Some (Bytes.of_string "value1"))
    (Memory_store.get store "key1");

  Memory_store.set store "key1" (Bytes.of_string "value2");
  check (option bytes) "overwrite"
    (Some (Bytes.of_string "value2"))
    (Memory_store.get store "key1")

let test_exists () =
  let store = Memory_store.create () in
  check bool "not exists" false (Memory_store.exists store "key1");

  Memory_store.set store "key1" (Bytes.of_string "value1");
  check bool "exists" true (Memory_store.exists store "key1")

let test_erase () =
  let store = Memory_store.create () in
  Memory_store.set store "key1" (Bytes.of_string "value1");
  check bool "exists before erase" true (Memory_store.exists store "key1");

  Memory_store.erase store "key1";
  check bool "not exists after erase" false (Memory_store.exists store "key1");

  (* Erasing non-existent key should not fail *)
  Memory_store.erase store "nonexistent"

let test_erase_prefix () =
  let store = Memory_store.create () in
  Memory_store.set store "foo/bar" (Bytes.of_string "1");
  Memory_store.set store "foo/baz" (Bytes.of_string "2");
  Memory_store.set store "other" (Bytes.of_string "3");

  Memory_store.erase_prefix store "foo/";

  check bool "foo/bar erased" false (Memory_store.exists store "foo/bar");
  check bool "foo/baz erased" false (Memory_store.exists store "foo/baz");
  check bool "other not erased" true (Memory_store.exists store "other")

let test_list () =
  let store = Memory_store.create () in
  Memory_store.set store "a" (Bytes.of_string "1");
  Memory_store.set store "b" (Bytes.of_string "2");
  Memory_store.set store "c/d" (Bytes.of_string "3");

  let keys = Memory_store.list store in
  check int "num keys" 3 (List.length keys);
  check bool "has a" true (List.mem "a" keys);
  check bool "has b" true (List.mem "b" keys);
  check bool "has c/d" true (List.mem "c/d" keys)

let test_list_prefix () =
  let store = Memory_store.create () in
  Memory_store.set store "foo/a" (Bytes.of_string "1");
  Memory_store.set store "foo/b" (Bytes.of_string "2");
  Memory_store.set store "bar/c" (Bytes.of_string "3");

  let keys = Memory_store.list_prefix store "foo/" in
  check int "num keys" 2 (List.length keys);
  check bool "has foo/a" true (List.mem "foo/a" keys);
  check bool "has foo/b" true (List.mem "foo/b" keys);
  check bool "no bar/c" false (List.mem "bar/c" keys)

let test_list_dir () =
  let store = Memory_store.create () in
  Memory_store.set store "a" (Bytes.of_string "1");
  Memory_store.set store "foo/b" (Bytes.of_string "2");
  Memory_store.set store "foo/c" (Bytes.of_string "3");
  Memory_store.set store "bar/d" (Bytes.of_string "4");

  let (keys, prefixes) = Memory_store.list_dir store "" in
  check int "root keys" 1 (List.length keys);
  check bool "has a" true (List.mem "a" keys);
  check int "root prefixes" 2 (List.length prefixes);
  check bool "has foo/" true (List.mem "foo/" prefixes);
  check bool "has bar/" true (List.mem "bar/" prefixes)

let test_get_partial () =
  let store = Memory_store.create () in
  Memory_store.set store "key1" (Bytes.of_string "Hello, World!");

  match Memory_store.get_partial store "key1" [(0, Some 5); (7, Some 5)] with
  | None -> fail "should get partial"
  | Some parts ->
    check int "num parts" 2 (List.length parts);
    check bytes "first part" (Bytes.of_string "Hello") (List.nth parts 0);
    check bytes "second part" (Bytes.of_string "World") (List.nth parts 1)

let test_get_partial_to_end () =
  let store = Memory_store.create () in
  Memory_store.set store "key1" (Bytes.of_string "Hello, World!");

  match Memory_store.get_partial store "key1" [(7, None)] with
  | None -> fail "should get partial"
  | Some parts ->
    check bytes "to end" (Bytes.of_string "World!") (List.nth parts 0)

let test_set_partial () =
  let store = Memory_store.create () in
  Memory_store.set store "key1" (Bytes.of_string "Hello, World!");

  Memory_store.set_partial store [("key1", 7, Bytes.of_string "OCaml")];

  check (option bytes) "after partial set"
    (Some (Bytes.of_string "Hello, OCaml!"))
    (Memory_store.get store "key1")

let test_clear () =
  let store = Memory_store.create () in
  Memory_store.set store "a" (Bytes.of_string "1");
  Memory_store.set store "b" (Bytes.of_string "2");

  Memory_store.clear store;
  check int "cleared" 0 (Memory_store.length store)

let tests = [
  "create", `Quick, test_create;
  "get/set", `Quick, test_get_set;
  "exists", `Quick, test_exists;
  "erase", `Quick, test_erase;
  "erase_prefix", `Quick, test_erase_prefix;
  "list", `Quick, test_list;
  "list_prefix", `Quick, test_list_prefix;
  "list_dir", `Quick, test_list_dir;
  "get_partial", `Quick, test_get_partial;
  "get_partial_to_end", `Quick, test_get_partial_to_end;
  "set_partial", `Quick, test_set_partial;
  "clear", `Quick, test_clear;
]
