(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Filesystem store tests. The golden values come from the zarrs
   oracle, see ../fixtures/README.md for the provenance of every
   fixture. The conformance executable is checked by the diff rules in
   ./dune rather than from here. *)

module Arr = Zarrz.Arr
module Byte_range = Zarrz.Byte_range
module Dtype = Zarrz.Dtype
module Error = Zarrz.Error
module Fill_value = Zarrz.Fill_value
module Group = Zarrz.Group
module Node = Zarrz.Node
module Slab = Zarrz.Slab
module Store = Zarrz.Store
module Subset = Zarrz.Subset
module Fu = Stdlib_upstream_compatible.Float_u
module I32u = Stdlib_upstream_compatible.Int32_u
module F32u = Stdlib_stable.Float32_u
module Ia = Stdlib_stable.Iarray

(* {1 Helpers} *)

(* Without this a failing test reports a bare [Zarrz__Error.E(_)]. *)
let () =
  Printexc.register_printer (function
    | Error.E e -> Some ("Zarrz.Error.E: " ^ Error.to_string e)
    | _ -> None)

let bs = Base_bigstring.of_string
let str = Base_bigstring.to_string

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error e -> Alcotest.failf "decoding %S: %s" s e

let json = Alcotest.testable Jsont.Json.pp Jsont.Json.equal
let range = Alcotest.testable Byte_range.pp ( = )

let whole a =
  let s = Arr.shape a in
  {
    Subset.start = Ia.of_array (Array.map (fun _ -> 0) s);
    shape = Ia.of_array s;
  }

let sub start shape =
  { Subset.start = Ia.of_array start; shape = Ia.of_array shape }

let f32 slab i =
  Int32.float_of_bits (I32u.to_int32 (F32u.to_bits (Slab.F32.get slab i)))

let f64 slab i = Fu.to_float (Slab.F64.get slab i)

(* A fresh empty directory below the directory the test runs in. *)
let tmp cwd name =
  let p = Eio.Path.(cwd / name) in
  Eio.Path.rmtree ~missing_ok:true p;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p;
  p

(* A store that records what it was asked for, so that a test can say
   which requests a read made and not only what it returned. *)
type calls = {
  mutable gets : string list;
  mutable ranges : (string * Byte_range.t) list;
  mutable batches : (string * int) list;
  mutable sizes : int;
}

let counting (s : Store.t) =
  let c = { gets = []; ranges = []; batches = []; sizes = 0 } in
  let t =
    {
      s with
      Store.get =
        (fun ~key ->
          c.gets <- c.gets @ [ key ];
          s.Store.get ~key);
      get_range =
        (fun ~key r ->
          c.ranges <- c.ranges @ [ (key, r) ];
          s.Store.get_range ~key r);
      get_ranges =
        (fun ~key rs ->
          c.batches <- c.batches @ [ (key, List.length rs) ];
          s.Store.get_ranges ~key rs);
      size =
        (fun ~key ->
          c.sizes <- c.sizes + 1;
          s.Store.size ~key);
    }
  in
  (t, c)

(* {1 Store semantics} *)

let test_read_write cwd () =
  let root = tmp cwd "tmp_store" in
  let s = Zarrz_eio.store ~writable:true root in
  let set = Option.get s.Store.set in
  let get k = Option.map str (s.Store.get ~key:k) in
  set ~key:"zarr.json" (bs "{}");
  set ~key:"a/b/c/0/0" (bs "0123456789");
  Alcotest.(check (option string)) "get" (Some "0123456789") (get "a/b/c/0/0");
  Alcotest.(check (option string)) "get shallow" (Some "{}") (get "zarr.json");
  Alcotest.(check (option string)) "missing" None (get "nope");
  Alcotest.(check (option string)) "missing deep" None (get "no/such/key");
  (* A chunk directory is not a chunk. *)
  Alcotest.(check (option string)) "directory" None (get "a/b");
  Alcotest.(check (option int)) "directory size" None (s.Store.size ~key:"a/b");
  Alcotest.(check (option int))
    "size" (Some 10)
    (s.Store.size ~key:"a/b/c/0/0");
  Alcotest.(check (option int)) "size missing" None (s.Store.size ~key:"nope");
  Alcotest.(check bool) "ranged" true s.Store.ranged;
  (* set replaces rather than appends. *)
  set ~key:"zarr.json" (bs "[]");
  Alcotest.(check (option string)) "replaced" (Some "[]") (get "zarr.json");
  let erase = Option.get s.Store.erase in
  erase ~key:"zarr.json";
  Alcotest.(check (option string)) "erased" None (get "zarr.json");
  erase ~key:"zarr.json";
  Alcotest.(check (option string)) "erasing twice" None (get "zarr.json");
  (* A directory is not an object, so erasing one fails rather than
     passing for an already absent key. *)
  match erase ~key:"a/b" with
  | () -> Alcotest.fail "erasing a directory succeeded"
  | exception Error.E (Error.Store _) -> ()

let test_absent_root cwd () =
  let root = Eio.Path.(cwd / "tmp_absent") in
  Eio.Path.rmtree ~missing_ok:true root;
  let s = Zarrz_eio.store root in
  Alcotest.(check (option string))
    "get" None
    (Option.map str (s.Store.get ~key:"zarr.json"));
  Alcotest.(check (option int)) "size" None (s.Store.size ~key:"zarr.json");
  Alcotest.(check (option string))
    "range" None
    (Option.map str (s.Store.get_range ~key:"zarr.json" (Byte_range.Suffix 2)));
  Alcotest.(check (list string))
    "list" []
    ((Option.get s.Store.list) ~prefix:"")

let test_ranges cwd () =
  let root = tmp cwd "tmp_ranges" in
  let s = Zarrz_eio.store ~writable:true root in
  (Option.get s.Store.set) ~key:"o" (bs "0123456789");
  let r x = Option.map str (s.Store.get_range ~key:"o" x) in
  Alcotest.(check (option string))
    "from start" (Some "234")
    (r (Byte_range.From_start { off = 2; len = Some 3 }));
  Alcotest.(check (option string))
    "to the end" (Some "6789")
    (r (Byte_range.From_start { off = 6; len = None }));
  Alcotest.(check (option string))
    "truncated" (Some "89")
    (r (Byte_range.From_start { off = 8; len = Some 5 }));
  Alcotest.(check (option string))
    "beyond the end" (Some "")
    (r (Byte_range.From_start { off = 20; len = Some 3 }));
  Alcotest.(check (option string))
    "suffix" (Some "789")
    (r (Byte_range.Suffix 3));
  Alcotest.(check (option string))
    "suffix longer than the object" (Some "0123456789")
    (r (Byte_range.Suffix 30));
  Alcotest.(check (option string))
    "missing" None
    (Option.map str (s.Store.get_range ~key:"nope" (Byte_range.Suffix 3)));
  let many =
    s.Store.get_ranges ~key:"o"
      [
        Byte_range.Suffix 2;
        Byte_range.From_start { off = 0; len = Some 2 };
        Byte_range.From_start { off = 4; len = Some 1 };
      ]
  in
  Alcotest.(check (option (list string)))
    "one buffer per range, in order"
    (Some [ "89"; "01"; "4" ])
    (Option.map (List.map str) many);
  Alcotest.(check (option (list string)))
    "missing batch" None
    (Option.map (List.map str)
       (s.Store.get_ranges ~key:"nope" [ Byte_range.Suffix 2 ]))

let test_list cwd () =
  let root = tmp cwd "tmp_list" in
  let s = Zarrz_eio.store ~writable:true root in
  let set = Option.get s.Store.set in
  List.iter
    (fun k -> set ~key:k (bs "x"))
    [ "zarr.json"; "a/zarr.json"; "a/c/0/0"; "a/c/0/1"; "ab/zarr.json" ];
  let list = Option.get s.Store.list in
  Alcotest.(check (list string))
    "everything, sorted"
    [ "a/c/0/0"; "a/c/0/1"; "a/zarr.json"; "ab/zarr.json"; "zarr.json" ]
    (list ~prefix:"");
  Alcotest.(check (list string))
    "one node" [ "a/c/0/0"; "a/c/0/1"; "a/zarr.json" ] (list ~prefix:"a/");
  (* A prefix cuts a name in half, it does not stop at a separator. *)
  Alcotest.(check (list string))
    "a partial name" [ "a/c/0/0"; "a/c/0/1"; "a/zarr.json"; "ab/zarr.json" ]
    (list ~prefix:"a");
  Alcotest.(check (list string))
    "a chunk" [ "a/c/0/1" ]
    (list ~prefix:"a/c/0/1");
  Alcotest.(check (list string)) "nothing" [] (list ~prefix:"z/");
  Alcotest.(check (list string)) "an absent node" [] (list ~prefix:"a/nope/");
  (* A read follows a symlink, so the listing does too, and a dangling
     one is nothing to either. *)
  Eio.Path.symlink ~link_to:"0" Eio.Path.(root / "a/c/0/2");
  Eio.Path.symlink ~link_to:"nope" Eio.Path.(root / "a/c/0/3");
  Alcotest.(check (list string))
    "a symlinked chunk"
    [ "a/c/0/0"; "a/c/0/1"; "a/c/0/2" ]
    (list ~prefix:"a/c/");
  Alcotest.(check (option string))
    "reading through the symlink" (Some "x")
    (Option.map str (s.Store.get ~key:"a/c/0/2"));
  Alcotest.(check (option string))
    "reading a dangling symlink" None
    (Option.map str (s.Store.get ~key:"a/c/0/3"))

let test_bad_keys cwd () =
  let root = tmp cwd "tmp_keys" in
  let s = Zarrz_eio.store ~writable:true root in
  let raises what k =
    Alcotest.check_raises what
      (Error.E
         (Error.Store (Printf.sprintf "get: %S is not a relative path" k)))
      (fun () -> ignore (s.Store.get ~key:k))
  in
  raises "absolute" "/etc/passwd";
  raises "parent" "../secret";
  raises "parent inside" "a/../../secret";
  raises "here" "./a";
  raises "empty component" "a//b";
  raises "trailing separator" "a/";
  Alcotest.check_raises "empty"
    (Error.E (Error.Store "get: the empty key names no object"))
    (fun () -> ignore (s.Store.get ~key:""));
  Alcotest.check_raises "a bad prefix"
    (Error.E (Error.Store "list: \"../a\" is not a relative path"))
    (fun () -> ignore ((Option.get s.Store.list) ~prefix:"../a"));
  (* Nothing escaped: the directory is still empty. *)
  Alcotest.(check (list string))
    "untouched" []
    ((Option.get s.Store.list) ~prefix:"")

let test_read_only cwd () =
  let root = tmp cwd "tmp_ro" in
  let w = Zarrz_eio.store ~writable:true root in
  (Option.get w.Store.set) ~key:"o" (bs "hello");
  let s = Zarrz_eio.store root in
  Alcotest.(check bool) "no set" true (Option.is_none s.Store.set);
  Alcotest.(check bool) "no erase" true (Option.is_none s.Store.erase);
  Alcotest.(check bool)
    "list is still there" true
    (Option.is_some s.Store.list);
  Alcotest.(check (option string))
    "reads work" (Some "ell")
    (Option.map str
       (s.Store.get_range ~key:"o"
          (Byte_range.From_start { off = 1; len = Some 3 })));
  Alcotest.check_raises "creating a group"
    (Error.E (Error.Store "the store does not support writing"))
    (fun () -> ignore (Group.create s ~path:"/g"));
  Alcotest.check_raises "creating an array"
    (Error.E (Error.Store "the store does not support writing"))
    (fun () ->
      ignore
        (Arr.create ~shape:[| 2 |] ~chunk_shape:[| 2 |] ~dtype:Dtype.Uint8
           ~fill_value:(Fill_value.of_bytes "\000")
           s ~path:"/a"))

(* {1 Golden arrays} *)

(* The 10 by 10 float32 fixtures hold [a.(i).(j) = 10 * i + j] whatever
   their codec chain and whichever writer produced them. *)
let check_10x10 fixtures dir =
  let s = Zarrz_eio.store Eio.Path.(fixtures / dir) in
  let a = Arr.open_ s ~path:"/" in
  Alcotest.(check (array int)) (dir ^ " shape") [| 10; 10 |] (Arr.shape a);
  Alcotest.(check string) (dir ^ " dtype") "float32" (Dtype.name (Arr.dtype a));
  Alcotest.(check (array int))
    (dir ^ " chunk shape") [| 5; 5 |] (Arr.chunk_shape a);
  let slab = Arr.read a (whole a) in
  for i = 0 to 9 do
    for j = 0 to 9 do
      Alcotest.(check (float 0.))
        (Printf.sprintf "%s [%d][%d]" dir i j)
        (float_of_int ((10 * i) + j))
        (f32 slab ((i * 10) + j))
    done
  done

let test_v3 fixtures () =
  List.iter
    (fun d -> check_10x10 fixtures ("v3/" ^ d ^ ".zarr"))
    [ "array_none"; "array_gzip"; "array_zstd" ]

let test_zarr_python fixtures () =
  check_10x10 fixtures "v3_zarr_python/array_none.zarr"

(* The sharded fixture is the one that must not be fetched whole. The
   chain is a bare [sharding_indexed], so every shard is served by one
   ranged read of the index and one batch of the inner chunks the
   subset needs. *)
let test_sharded fixtures () =
  let base =
    Zarrz_eio.store Eio.Path.(fixtures / "sharded_array_write_read.zarr")
  in
  let s, c = counting base in
  let a = Arr.open_ s ~path:"/group/array" in
  Alcotest.(check (array int)) "shape" [| 8; 8 |] (Arr.shape a);
  Alcotest.(check string) "dtype" "uint16" (Dtype.name (Arr.dtype a));
  Alcotest.(check (array int)) "shard shape" [| 4; 8 |] (Arr.chunk_shape a);
  (* Opening read the metadata. The read is what is under test. *)
  c.gets <- [];
  c.sizes <- 0;
  let slab = Arr.read a (whole a) in
  for i = 0 to 7 do
    for j = 0 to 7 do
      Alcotest.(check int)
        (Printf.sprintf "[%d][%d]" i j)
        ((8 * i) + j)
        (Stdlib_stable.Int16_u.to_int (Slab.U16.get slab ((i * 8) + j))
        land 0xffff)
    done
  done;
  Alcotest.(check (list string)) "no shard fetched whole" [] c.gets;
  Alcotest.(check int) "one size per shard" 2 c.sizes;
  Alcotest.(check (list string))
    "one index read per shard"
    [ "group/array/c/0/0"; "group/array/c/1/0" ]
    (List.map fst c.ranges);
  List.iter
    (fun (_, r) ->
      Alcotest.check range "the index is a 36 byte suffix"
        (Byte_range.Suffix 36) r)
    c.ranges;
  Alcotest.(check (list (pair string int)))
    "one batch of two inner chunks per shard"
    [ ("group/array/c/0/0", 2); ("group/array/c/1/0", 2) ]
    c.batches

(* {1 Hierarchies} *)

let test_hierarchy fixtures () =
  let s = Zarrz_eio.store Eio.Path.(fixtures / "hierarchy.zarr") in
  let group what path =
    match Node.open_ s ~path with
    | `Group g -> g
    | `Array _ -> Alcotest.failf "%s is an array" what
  in
  let array what path =
    match Node.open_ s ~path with
    | `Array a -> a
    | `Group _ -> Alcotest.failf "%s is a group" what
  in
  let root = group "the root" "/" in
  Alcotest.(check (option (list string)))
    "the children of the root"
    (Some [ "a"; "b" ])
    (Group.children root);
  Alcotest.(check (option json)) "the root has no attributes" None
    (Group.attributes root);
  let a = group "a" "/a" in
  Alcotest.(check (option (list string)))
    "the children of a"
    (Some [ "baz"; "foo" ])
    (Group.children a);
  let b = group "b" "/b" in
  Alcotest.(check (option json))
    "the attributes of b"
    (Some (json_of_string {|{"test_key": "test_value"}|}))
    (Group.attributes b);
  Alcotest.(check (option (list string)))
    "a group with no children"
    (Some []) (Group.children b);
  let foo = array "a/foo" "/a/foo" in
  Alcotest.(check (array int)) "shape" [| 10000; 1000 |] (Arr.shape foo);
  Alcotest.(check (array int)) "chunk shape" [| 1000; 100 |]
    (Arr.chunk_shape foo);
  Alcotest.(check (array int)) "grid shape" [| 10; 10 |] (Arr.grid_shape foo);
  Alcotest.(check string) "dtype" "float64" (Dtype.name (Arr.dtype foo));
  Alcotest.(check (option (list (option string))))
    "dimension names"
    (Some [ Some "rows"; Some "columns" ])
    (Arr.dimension_names foo);
  Alcotest.(check (option json))
    "attributes"
    (Some (json_of_string {|{"foo": 42, "bar": "apples", "baz": [1,2,3,4]}|}))
    (Arr.attributes foo);
  Alcotest.(check string) "chunk key" "a/foo/c/3/4"
    (Arr.chunk_key foo [| 3; 4 |]);
  (* No chunk is stored, so every element is the fill value. *)
  let slab = Arr.read foo (sub [| 0; 0 |] [| 2; 3 |]) in
  for i = 0 to 5 do
    Alcotest.(check bool)
      (Printf.sprintf "fill value %d" i)
      true
      (Float.is_nan (f64 slab i))
  done;
  Alcotest.(check bool) "an absent chunk" true
    (Option.is_none (Arr.read_chunk_opt foo [| 0; 0 |]))

(* The final state of the oracle's [array_write_read] example, read out
   of the fixture chunks. Chunk [0, 0] is erased at the end of the
   example, so the top left quarter is the fill value. *)
let awr_expected =
  [|
    [| nan; nan; nan; nan; 0.1; 0.1; -0.6; 0.1 |];
    [| nan; nan; nan; nan; 0.1; 0.1; -1.6; 0.1 |];
    [| nan; nan; nan; nan; 0.1; 0.1; -2.6; 0.1 |];
    [| nan; nan; nan; nan; -3.4; -3.5; -3.6; 0.1 |];
    [| 1.0; 1.0; 1.0; -4.3; -4.4; -4.5; -4.6; 1.1 |];
    [| 1.0; 1.0; 1.0; -5.3; -5.4; -5.5; -5.6; 1.1 |];
    [| 1.0; 1.0; 1.0; 1.0; 1.1; 1.1; -6.6; 1.1 |];
    [| 1.0; 1.0; 1.0; 1.0; -7.4; -7.5; -7.6; -7.7 |];
  |]

let test_array_write_read fixtures () =
  let s = Zarrz_eio.store Eio.Path.(fixtures / "array_write_read.zarr") in
  (match Node.open_ s ~path:"/group" with
  | `Group g ->
      Alcotest.(check (option json))
        "the attributes of the group"
        (Some (json_of_string {|{"foo": "bar"}|}))
        (Group.attributes g);
      Alcotest.(check (option (list string)))
        "the children of the group"
        (Some [ "array" ])
        (Group.children g)
  | `Array _ -> Alcotest.fail "/group is an array");
  let a = Arr.open_ s ~path:"/group/array" in
  Alcotest.(check (array int)) "shape" [| 8; 8 |] (Arr.shape a);
  Alcotest.(check (array int)) "chunk shape" [| 4; 4 |] (Arr.chunk_shape a);
  Alcotest.(check string) "dtype" "float32" (Dtype.name (Arr.dtype a));
  Alcotest.(check (option (list (option string))))
    "dimension names"
    (Some [ Some "y"; Some "x" ])
    (Arr.dimension_names a);
  Alcotest.(check bool) "the erased chunk is absent" true
    (Option.is_none (Arr.read_chunk_opt a [| 0; 0 |]));
  Alcotest.(check bool) "a stored chunk is present" true
    (Option.is_some (Arr.read_chunk_opt a [| 1; 1 |]));
  let slab = Arr.read a (whole a) in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let want = awr_expected.(i).(j) in
      let got = f32 slab ((i * 8) + j) in
      let what = Printf.sprintf "[%d][%d]" i j in
      if Float.is_nan want then
        Alcotest.(check bool) (what ^ " is the fill value") true
          (Float.is_nan got)
      else Alcotest.(check (float 1e-6)) what want got
    done
  done

(* {1 Writing} *)

let test_round_trip cwd () =
  let root = tmp cwd "tmp_write" in
  let s = Zarrz_eio.store ~writable:true root in
  ignore (Group.create s ~path:"/");
  let dtype = Dtype.Float64 in
  let a =
    Arr.create ~shape:[| 4; 6 |] ~chunk_shape:[| 2; 3 |] ~dtype
      ~fill_value:(Fill_value.of_bytes (String.make 8 '\000'))
      s ~path:"/data"
  in
  let want = Slab.create dtype (Ia.of_array [| 4; 6 |]) in
  for k = 0 to 23 do
    Slab.F64.set want k (Fu.of_float (float_of_int k +. 0.5))
  done;
  Arr.write a (sub [| 0; 0 |] [| 4; 6 |]) want;
  (* A store built again from the directory, so nothing survives in
     memory between the write and the read. *)
  let s2 = Zarrz_eio.store root in
  let a2 = Arr.open_ s2 ~path:"/data" in
  Alcotest.(check (array int)) "shape" [| 4; 6 |] (Arr.shape a2);
  Alcotest.(check string) "dtype" "float64" (Dtype.name (Arr.dtype a2));
  let got = Arr.read a2 (whole a2) in
  Alcotest.(check string) "byte exact"
    (str (Slab.bigstring want))
    (str (Slab.bigstring got));
  Alcotest.(check (list string))
    "one file per chunk, plus the metadata"
    [
      "data/c/0/0";
      "data/c/0/1";
      "data/c/1/0";
      "data/c/1/1";
      "data/zarr.json";
      "zarr.json";
    ]
    ((Option.get s2.Store.list) ~prefix:"");
  (* A subset that does not cover a chunk reads the chunk back first. *)
  let patch = Slab.create dtype (Ia.of_array [| 1; 2 |]) in
  Slab.F64.set patch 0 (Fu.of_float 100.0);
  Slab.F64.set patch 1 (Fu.of_float 101.0);
  Arr.write a (sub [| 1; 2 |] [| 1; 2 |]) patch;
  let a3 = Arr.open_ (Zarrz_eio.store root) ~path:"/data" in
  let got = Arr.read a3 (whole a3) in
  for k = 0 to 23 do
    let want =
      if k = 8 then 100.0 else if k = 9 then 101.0 else float_of_int k +. 0.5
    in
    Alcotest.(check (float 0.)) (Printf.sprintf "element %d" k) want
      (f64 got k)
  done

(* {1 Suite} *)

let () =
  Eio_main.run @@ fun env ->
  (* [cwd] confines to the directory the test runs in and so cannot
     reach the fixtures beside it. Both live under [fs]. *)
  let cwd = Eio.Stdenv.fs env in
  let fixtures = Eio.Path.(cwd / "../fixtures") in
  let open Alcotest in
  run "zarrz eio"
    [
      ( "store",
        [
          test_case "read and write" `Quick (test_read_write cwd);
          test_case "an absent root" `Quick (test_absent_root cwd);
          test_case "byte ranges" `Quick (test_ranges cwd);
          test_case "listing" `Quick (test_list cwd);
          test_case "rejected keys" `Quick (test_bad_keys cwd);
          test_case "read only" `Quick (test_read_only cwd);
        ] );
      ( "fixtures",
        [
          test_case "zarrs v3" `Quick (test_v3 fixtures);
          test_case "zarr python v3" `Quick (test_zarr_python fixtures);
          test_case "sharded" `Quick (test_sharded fixtures);
          test_case "hierarchy" `Quick (test_hierarchy fixtures);
          test_case "array_write_read" `Quick (test_array_write_read fixtures);
        ] );
      ("write", [ test_case "round trip" `Quick (test_round_trip cwd) ]);
    ]
