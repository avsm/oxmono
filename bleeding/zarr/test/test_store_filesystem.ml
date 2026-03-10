(** Tests for filesystem store *)

open Alcotest
open Zarr_sync

let test_root = "/tmp/zarr_test_" ^ string_of_int (Random.int 100000)

let setup () =
  if Sys.file_exists test_root then
    ignore (Sys.command ("rm -rf " ^ test_root));
  Unix.mkdir test_root 0o755

let teardown () =
  if Sys.file_exists test_root then
    ignore (Sys.command ("rm -rf " ^ test_root))

let test_create () =
  setup ();
  let store = Filesystem_store.create test_root in
  check string "root" test_root (Filesystem_store.root store);
  teardown ()

let test_get_set () =
  setup ();
  let store = Filesystem_store.create test_root in

  check (option bytes) "get missing" None (Filesystem_store.get store "key1");

  Filesystem_store.set store "key1" (Bytes.of_string "value1");
  check (option bytes) "get existing"
    (Some (Bytes.of_string "value1"))
    (Filesystem_store.get store "key1");

  Filesystem_store.set store "key1" (Bytes.of_string "value2");
  check (option bytes) "overwrite"
    (Some (Bytes.of_string "value2"))
    (Filesystem_store.get store "key1");

  teardown ()

let test_nested_paths () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "foo/bar/baz" (Bytes.of_string "nested");
  check (option bytes) "get nested"
    (Some (Bytes.of_string "nested"))
    (Filesystem_store.get store "foo/bar/baz");

  teardown ()

let test_exists () =
  setup ();
  let store = Filesystem_store.create test_root in

  check bool "not exists" false (Filesystem_store.exists store "key1");

  Filesystem_store.set store "key1" (Bytes.of_string "value1");
  check bool "exists" true (Filesystem_store.exists store "key1");

  teardown ()

let test_erase () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "key1" (Bytes.of_string "value1");
  check bool "exists before erase" true (Filesystem_store.exists store "key1");

  Filesystem_store.erase store "key1";
  check bool "not exists after erase" false (Filesystem_store.exists store "key1");

  (* Erasing non-existent key should not fail *)
  Filesystem_store.erase store "nonexistent";

  teardown ()

let test_erase_prefix () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "foo/bar" (Bytes.of_string "1");
  Filesystem_store.set store "foo/baz" (Bytes.of_string "2");
  Filesystem_store.set store "other" (Bytes.of_string "3");

  Filesystem_store.erase_prefix store "foo";

  check bool "foo/bar erased" false (Filesystem_store.exists store "foo/bar");
  check bool "foo/baz erased" false (Filesystem_store.exists store "foo/baz");
  check bool "other not erased" true (Filesystem_store.exists store "other");

  teardown ()

let test_list () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "a" (Bytes.of_string "1");
  Filesystem_store.set store "b" (Bytes.of_string "2");
  Filesystem_store.set store "c/d" (Bytes.of_string "3");

  let keys = Filesystem_store.list store in
  check int "num keys" 3 (List.length keys);
  check bool "has a" true (List.mem "a" keys);
  check bool "has b" true (List.mem "b" keys);
  check bool "has c/d" true (List.mem "c/d" keys);

  teardown ()

let test_list_prefix () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "foo/a" (Bytes.of_string "1");
  Filesystem_store.set store "foo/b" (Bytes.of_string "2");
  Filesystem_store.set store "bar/c" (Bytes.of_string "3");

  let keys = Filesystem_store.list_prefix store "foo/" in
  check int "num keys" 2 (List.length keys);
  check bool "has foo/a" true (List.mem "foo/a" keys);
  check bool "has foo/b" true (List.mem "foo/b" keys);
  check bool "no bar/c" false (List.mem "bar/c" keys);

  teardown ()

let test_list_dir () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "a" (Bytes.of_string "1");
  Filesystem_store.set store "foo/b" (Bytes.of_string "2");
  Filesystem_store.set store "foo/c" (Bytes.of_string "3");
  Filesystem_store.set store "bar/d" (Bytes.of_string "4");

  let (keys, prefixes) = Filesystem_store.list_dir store "" in
  check int "root keys" 1 (List.length keys);
  check bool "has a" true (List.mem "a" keys);
  check int "root prefixes" 2 (List.length prefixes);
  check bool "has foo/" true (List.mem "foo/" prefixes);
  check bool "has bar/" true (List.mem "bar/" prefixes);

  teardown ()

let test_get_partial () =
  setup ();
  let store = Filesystem_store.create test_root in

  Filesystem_store.set store "key1" (Bytes.of_string "Hello, World!");

  match Filesystem_store.get_partial store "key1" [(0, Some 5); (7, Some 5)] with
  | None -> fail "should get partial"
  | Some parts ->
    check int "num parts" 2 (List.length parts);
    check bytes "first part" (Bytes.of_string "Hello") (List.nth parts 0);
    check bytes "second part" (Bytes.of_string "World") (List.nth parts 1);

  teardown ()

let test_binary_data () =
  setup ();
  let store = Filesystem_store.create test_root in

  (* Test with binary data including null bytes *)
  let binary = Bytes.of_string "\x00\x01\x02\xff\xfe\xfd" in
  Filesystem_store.set store "binary" binary;

  check (option bytes) "binary roundtrip" (Some binary) (Filesystem_store.get store "binary");

  teardown ()

let tests = [
  "create", `Quick, test_create;
  "get/set", `Quick, test_get_set;
  "nested paths", `Quick, test_nested_paths;
  "exists", `Quick, test_exists;
  "erase", `Quick, test_erase;
  "erase_prefix", `Quick, test_erase_prefix;
  "list", `Quick, test_list;
  "list_prefix", `Quick, test_list_prefix;
  "list_dir", `Quick, test_list_dir;
  "get_partial", `Quick, test_get_partial;
  "binary data", `Quick, test_binary_data;
]
