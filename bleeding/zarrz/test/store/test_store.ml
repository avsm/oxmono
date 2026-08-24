(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Store, array, group and node tests.

   The codecs here are test codecs injected through a [Codec.resolver],
   so nothing in this file depends on which built-in codecs exist. They
   are the smallest chain shapes the array paths distinguish: a raw
   array to bytes codec, the same one with a partial decoder, and a
   size preserving bytes to bytes codec. *)

module Arr = Zarrz.Arr
module Byte_range = Zarrz.Byte_range
module Byte_source = Zarrz.Byte_source
module Chunk_key = Zarrz.Chunk_key
module Codec = Zarrz.Codec
module Dtype = Zarrz.Dtype
module Error = Zarrz.Error
module Ext = Zarrz.Ext
module Fill_value = Zarrz.Fill_value
module Group = Zarrz.Group
module Metadata = Zarrz.Metadata
module Node = Zarrz.Node
module Slab = Zarrz.Slab
module Store = Zarrz.Store
module Subset = Zarrz.Subset
module Ia = Stdlib_stable.Iarray
module I32u = Stdlib_upstream_compatible.Int32_u

let product a = Array.fold_left ( * ) 1 a

(* Int32 elements throughout: one element names one integer, so a
   failure prints the value rather than a bit pattern. *)

let set32 s i v = Slab.I32.set s i (I32u.of_int32 (Int32.of_int v))
let get32 s i = Int32.to_int (I32u.to_int32 (Slab.I32.get s i))

let i32_bytes v =
  let s = Slab.create Dtype.Int32 [: 1 :] in
  set32 s 0 v;
  Base_bigstring.to_string (Slab.bigstring s)

let fill_of v = Fill_value.of_bytes (i32_bytes v)
let ints_of_slab s = List.init (Slab.num_elements s) (get32 s)

let slab_of_ints shape l =
  let s = Slab.create Dtype.Int32 (Ia.of_array shape) in
  List.iteri (set32 s) l;
  s

let bs = Base_bigstring.of_string
let str = Base_bigstring.to_string
let sub_of ~start ~shape =
  { Subset.start = Ia.of_array start; shape = Ia.of_array shape }

(* Test codecs *)

let raw_bytes (r : Codec.repr) = Dtype.size r.dtype * product r.shape

let raw_encode s = Base_bigstring.copy (Slab.bigstring s)

let raw_decode b (r : Codec.repr) =
  if Base_bigstring.length b <> raw_bytes r then
    Error.raise_ (Error.Codec "test.raw: wrong encoded size");
  Slab.of_bigstring r.dtype (Ia.of_array r.shape) (Base_bigstring.copy b)

(* The raw layout is the decoded layout, so one contiguous run of the
   wanted subset is one byte range of the stored object. *)
let raw_partial (src : Byte_source.t) (r : Codec.repr) (sub : Subset.t) =
  let esz = Dtype.size r.dtype in
  let outer = Ia.of_array r.shape in
  let out = Slab.create r.dtype sub.shape in
  let dst_buf = Slab.bigstring out in
  Subset.iter_runs ~outer sub ~f:(fun ~src:soff ~dst ~len ->
      let b =
        src.Byte_source.read
          (Byte_range.From_start { off = soff * esz; len = Some (len * esz) })
      in
      Base_bigstring.blit ~src:b ~src_pos:0 ~dst:dst_buf ~dst_pos:(dst * esz)
        ~len:(len * esz));
  out

let raw_codec =
  {
    Codec.name = "test.raw";
    encoded_size = (fun r -> Codec.Fixed (raw_bytes r));
    encode = raw_encode;
    decode = raw_decode;
    partial_decode = None;
  }

let raw_partial_codec =
  {
    raw_codec with
    Codec.name = "test.raw_partial";
    partial_decode = Some raw_partial;
  }

let xor_map b =
  let n = Base_bigstring.length b in
  let o = Base_bigstring.create n in
  for i = 0 to n - 1 do
    Base_bigstring.set_uint8_exn o ~pos:i
      (Base_bigstring.get_uint8 b ~pos:i lxor 0xa5)
  done;
  o

let xor_codec =
  {
    Codec.name = "test.xor";
    encoded_size = (fun s -> s);
    encode = xor_map;
    decode = (fun b ~decoded_size:_ -> xor_map b);
  }

let resolver : Codec.resolver =
 fun ext ~dtype:_ ~fill_value:_ ->
  match ext.Ext.name with
  | "test.raw" -> Some (Ok (Codec.A2b raw_codec))
  | "test.raw_partial" -> Some (Ok (Codec.A2b raw_partial_codec))
  | "test.xor" -> Some (Ok (Codec.B2b xor_codec))
  | _ -> None

let raw_c = [ Ext.v "test.raw" ]
let raw_partial_c = [ Ext.v "test.raw_partial" ]
let xor_c = [ Ext.v "test.raw"; Ext.v "test.xor" ]

(* A store that counts what an array asks of it. *)

type counts = {
  mutable get : int;
  mutable get_range : int;
  mutable get_ranges : int;
  mutable size : int;
  mutable set : int;
}

let counting (s : Store.t) =
  let c = { get = 0; get_range = 0; get_ranges = 0; size = 0; set = 0 } in
  let t =
    {
      s with
      Store.get = (fun ~key -> c.get <- c.get + 1; s.Store.get ~key);
      get_range =
        (fun ~key r ->
          c.get_range <- c.get_range + 1;
          s.Store.get_range ~key r);
      get_ranges =
        (fun ~key rs ->
          c.get_ranges <- c.get_ranges + 1;
          s.Store.get_ranges ~key rs);
      size = (fun ~key -> c.size <- c.size + 1; s.Store.size ~key);
      set =
        Option.map
          (fun f ~key b -> c.set <- c.set + 1; f ~key b)
          s.Store.set;
    }
  in
  (t, c)

let reset c =
  c.get <- 0;
  c.get_range <- 0;
  c.get_ranges <- 0;
  c.size <- 0;
  c.set <- 0

(* Helpers *)

let put (s : Store.t) key v = (Option.get s.Store.set) ~key (bs v)
let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.failf "test JSON is invalid: %s" m

let json = Alcotest.testable Jsont.Json.pp Jsont.Json.equal
let ints = Alcotest.(list int)

let create ?attributes ?dimension_names ?(codecs = raw_c) ?chunk_key_encoding
    ?(path = "/a") ?(fill = -1) store ~shape ~chunk_shape =
  Arr.create ~resolver ?attributes ?dimension_names ~codecs ?chunk_key_encoding
    ~shape ~chunk_shape ~dtype:Dtype.Int32 ~fill_value:(fill_of fill) store
    ~path

(* [seq shape] holds its own C-order index at every element. *)
let seq shape =
  let s = Slab.create Dtype.Int32 (Ia.of_array shape) in
  for i = 0 to Slab.num_elements s - 1 do
    set32 s i i
  done;
  s

let write_all a shape =
  let start = Array.map (fun _ -> 0) shape in
  Arr.write a (sub_of ~start ~shape) (seq shape)

let raises_error name f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Error.E" name
  | exception Error.E e -> e

let check_msg name needle e =
  let s = Error.to_string e in
  let found = ref false in
  let n = String.length needle in
  for i = 0 to String.length s - n do
    if String.equal (String.sub s i n) needle then found := true
  done;
  if not !found then
    Alcotest.failf "%s: %S does not mention %S" name s needle

(* Memory store *)

let test_memory_get_set () =
  let s = Store.memory () in
  Alcotest.(check (option string)) "absent" None
    (Option.map str (s.Store.get ~key:"a"));
  Alcotest.(check (option int)) "absent size" None (s.Store.size ~key:"a");
  Alcotest.(check bool) "ranged" true s.Store.ranged;
  let b = bs "hello" in
  put s "a" "hello";
  Base_bigstring.set_uint8_exn b ~pos:0 (Char.code 'H');
  (Option.get s.Store.set) ~key:"b" b;
  Base_bigstring.set_uint8_exn b ~pos:0 (Char.code 'X');
  Alcotest.(check string) "set copies" "Hello"
    (str (Option.get (s.Store.get ~key:"b")));
  let g = Option.get (s.Store.get ~key:"a") in
  Base_bigstring.set_uint8_exn g ~pos:0 (Char.code 'J');
  Alcotest.(check string) "get copies" "hello"
    (str (Option.get (s.Store.get ~key:"a")));
  Alcotest.(check (option int)) "size" (Some 5) (s.Store.size ~key:"a");
  put s "a" "bye";
  Alcotest.(check string) "replace" "bye"
    (str (Option.get (s.Store.get ~key:"a")))

let test_memory_ranges () =
  let s = Store.memory () in
  put s "a" "hello";
  let r k = Option.map str (s.Store.get_range ~key:"a" k) in
  Alcotest.(check (option string))
    "from start" (Some "ell")
    (r (Byte_range.From_start { off = 1; len = Some 3 }));
  Alcotest.(check (option string))
    "to end" (Some "ello")
    (r (Byte_range.From_start { off = 1; len = None }));
  Alcotest.(check (option string))
    "suffix" (Some "llo")
    (r (Byte_range.Suffix 3));
  Alcotest.(check (option string))
    "suffix past start" (Some "hello") (r (Byte_range.Suffix 99));
  Alcotest.(check (option string))
    "off past end" (Some "")
    (r (Byte_range.From_start { off = 9; len = Some 2 }));
  Alcotest.(check (option string))
    "len past end" (Some "lo")
    (r (Byte_range.From_start { off = 3; len = Some 9 }));
  Alcotest.(check (option string))
    "absent" None
    (Option.map str (s.Store.get_range ~key:"z" (Byte_range.Suffix 1)));
  let rs =
    s.Store.get_ranges ~key:"a"
      [ Byte_range.Suffix 2; Byte_range.From_start { off = 0; len = Some 1 } ]
  in
  Alcotest.(check (option (list string)))
    "get_ranges" (Some [ "lo"; "h" ])
    (Option.map (List.map str) rs);
  Alcotest.(check (option (list string)))
    "get_ranges absent" None
    (Option.map (List.map str) (s.Store.get_ranges ~key:"z" []))

let test_memory_list_erase () =
  let s = Store.memory () in
  let list = Option.get s.Store.list in
  let erase = Option.get s.Store.erase in
  put s "a" "1";
  put s "p/q" "2";
  put s "p/r" "3";
  put s "pz" "4";
  Alcotest.(check (list string)) "prefix" [ "p/q"; "p/r" ] (list ~prefix:"p/");
  Alcotest.(check (list string))
    "prefix p" [ "p/q"; "p/r"; "pz" ] (list ~prefix:"p");
  Alcotest.(check (list string))
    "all" [ "a"; "p/q"; "p/r"; "pz" ] (list ~prefix:"");
  Alcotest.(check (list string)) "none" [] (list ~prefix:"zz");
  erase ~key:"p/q";
  erase ~key:"nope";
  Alcotest.(check (list string)) "erased" [ "p/r" ] (list ~prefix:"p/")

let test_get_json () =
  let s = Store.memory () in
  put s "k" {|{"a":1}|};
  Alcotest.check json "json" (json_of_string {|{"a":1}|})
    (Store.get_json s ~key:"k");
  check_msg "absent" "not found" (raises_error "absent" (fun () ->
      Store.get_json s ~key:"z"));
  put s "bad" "{oops";
  (match raises_error "bad" (fun () -> Store.get_json s ~key:"bad") with
  | Error.Metadata _ -> ()
  | e -> Alcotest.failf "bad json: %s" (Error.to_string e))

(* Creating *)

let test_create_metadata () =
  let s = Store.memory () in
  let a =
    create s ~shape:[| 4; 6 |] ~chunk_shape:[| 2; 3 |]
      ~attributes:(json_of_string {|{"a":1}|})
      ~dimension_names:[ Some "x"; None ]
  in
  let want =
    json_of_string
      {|{"zarr_format":3,"node_type":"array","shape":[4,6],
         "data_type":"int32",
         "chunk_grid":{"name":"regular",
                       "configuration":{"chunk_shape":[2,3]}},
         "chunk_key_encoding":{"name":"default",
                               "configuration":{"separator":"/"}},
         "fill_value":-1,"codecs":["test.raw"],
         "attributes":{"a":1},"dimension_names":["x",null]}|}
  in
  Alcotest.check json "document" want (Store.get_json s ~key:"a/zarr.json");
  Alcotest.(check (array int)) "shape" [| 4; 6 |] (Arr.shape a);
  Alcotest.(check (array int)) "chunk shape" [| 2; 3 |] (Arr.chunk_shape a);
  Alcotest.(check (array int)) "grid shape" [| 2; 2 |] (Arr.grid_shape a);
  Alcotest.(check string) "dtype" "int32" (Dtype.name (Arr.dtype a));
  Alcotest.(check string) "fill" (i32_bytes (-1))
    (Fill_value.to_bytes (Arr.fill_value a));
  Alcotest.(check string) "path" "/a" (Arr.path a);
  Alcotest.(check string) "chunk key" "a/c/1/0" (Arr.chunk_key a [| 1; 0 |]);
  (* Reopening sees the same array. *)
  let b = Arr.open_ ~codecs:resolver s ~path:"/a" in
  Alcotest.(check (array int)) "reopened shape" [| 4; 6 |] (Arr.shape b);
  Alcotest.check json "reopened attributes" (json_of_string {|{"a":1}|})
    (Option.get (Arr.attributes b));
  Alcotest.(check bool) "dimension names" true
    (Arr.dimension_names b = Some [ Some "x"; None ])

let test_create_defaults () =
  let s = Store.memory () in
  (* The default chain is the little endian [bytes] codec. It is
     resolved here by the test codec so that the assertion is about the
     metadata, not about which built-ins exist. *)
  let bytes_resolver : Codec.resolver =
   fun ext ~dtype:_ ~fill_value:_ ->
    match ext.Ext.name with
    | "bytes" -> Some (Ok (Codec.A2b raw_codec))
    | _ -> None
  in
  let _ =
    Arr.create ~resolver:bytes_resolver ~shape:[| 2 |] ~chunk_shape:[| 2 |]
      ~dtype:Dtype.Int32 ~fill_value:(fill_of 0) s ~path:"/d"
  in
  let m =
    match Metadata.array_of_json (Store.get_json s ~key:"d/zarr.json") with
    | Ok m -> m
    | Error e -> Alcotest.failf "metadata: %s" e
  in
  Alcotest.(check int) "one codec" 1 (List.length m.codecs);
  let c = List.hd m.codecs in
  Alcotest.(check string) "name" "bytes" c.Ext.name;
  Alcotest.check json "endian" (json_of_string {|"little"|})
    (Option.get (Ext.config_mem c "endian"))

let test_create_errors () =
  let s = Store.memory () in
  check_msg "rank" "chunk dimensions"
    (raises_error "rank" (fun () ->
         create s ~shape:[| 4 |] ~chunk_shape:[| 2; 2 |]));
  check_msg "zero chunk" "positive"
    (raises_error "zero chunk" (fun () ->
         create s ~shape:[| 4 |] ~chunk_shape:[| 0 |]));
  check_msg "names" "dimension names"
    (raises_error "names" (fun () ->
         create s ~shape:[| 4 |] ~chunk_shape:[| 2 |]
           ~dimension_names:[ Some "x"; None ]));
  check_msg "fill width" "fill value"
    (raises_error "fill width" (fun () ->
         Arr.create ~resolver ~codecs:raw_c ~shape:[| 4 |] ~chunk_shape:[| 2 |]
           ~dtype:Dtype.Int32
           ~fill_value:(Fill_value.of_bytes "ab")
           s ~path:"/x"));
  check_msg "unknown codec" "unknown codec"
    (raises_error "unknown codec" (fun () ->
         create s ~shape:[| 4 |] ~chunk_shape:[| 2 |]
           ~codecs:[ Ext.v "test.raw"; Ext.v "nope" ]));
  let ro = { (Store.memory ()) with Store.set = None } in
  check_msg "read only" "writing"
    (raises_error "read only" (fun () ->
         create ro ~shape:[| 4 |] ~chunk_shape:[| 2 |]))

(* Opening *)

let meta_str ?(node = "array") ?(dt = {|"int32"|}) ?(codecs = {|["test.raw"]|})
    ?(grid = {|{"name":"regular","configuration":{"chunk_shape":[2,2]}}|})
    ?(extra = "") () =
  Printf.sprintf
    {|{"zarr_format":3,"node_type":"%s","shape":[4,4],"data_type":%s,
       "chunk_grid":%s,"chunk_key_encoding":{"name":"default"},
       "fill_value":0,"codecs":%s%s}|}
    node dt grid codecs extra

let open_with doc =
  let s = Store.memory () in
  put s "a/zarr.json" doc;
  fun () -> Arr.open_ ~codecs:resolver s ~path:"/a"

let test_open_missing () =
  let s = Store.memory () in
  let e = raises_error "missing" (fun () -> Arr.open_ s ~path:"/a") in
  check_msg "missing" "not found" e;
  (match e with
  | Error.Store _ -> ()
  | _ -> Alcotest.failf "missing: %s" (Error.to_string e));
  put s "a/zarr.json" "{ not json";
  match raises_error "parse" (fun () -> Arr.open_ s ~path:"/a") with
  | Error.Metadata _ -> ()
  | e -> Alcotest.failf "parse: %s" (Error.to_string e)

let test_open_errors () =
  check_msg "node_type" "node_type"
    (raises_error "node_type" (open_with (meta_str ~node:"group" ())));
  check_msg "unknown dtype" "unknown data type"
    (raises_error "dtype" (open_with (meta_str ~dt:{|"quux"|} ())));
  check_msg "dtype config" "configuration"
    (raises_error "dtype config"
       (open_with
          (meta_str ~dt:{|{"name":"int32","configuration":{"n":1}}|} ())));
  check_msg "dtype must_understand" "must_understand"
    (raises_error "dtype mu"
       (open_with
          (meta_str ~dt:{|{"name":"int32","must_understand":false}|} ())));
  check_msg "grid" "unsupported name"
    (raises_error "grid"
       (open_with
          (meta_str ~grid:{|{"name":"rectangular","configuration":{}}|} ())));
  check_msg "transformer" "storage transformer"
    (raises_error "transformer"
       (open_with
          (meta_str ~extra:{|,"storage_transformers":[{"name":"x"}]|} ())));
  check_msg "codec" "unknown codec"
    (raises_error "codec"
       (open_with (meta_str ~codecs:{|["test.raw","nope"]|} ())));
  match
    raises_error "no a2b" (open_with (meta_str ~codecs:{|["test.xor"]|} ()))
  with
  | Error.Codec _ -> ()
  | e -> Alcotest.failf "no a2b: %s" (Error.to_string e)

let test_open_ignored () =
  (* [must_understand] false makes both an unknown storage transformer
     and an unknown codec skippable. *)
  let a =
    (open_with
       (meta_str
          ~extra:
            {|,"storage_transformers":[{"name":"x","must_understand":false}]|}
          ~codecs:{|["test.raw",{"name":"nope","must_understand":false}]|} ()))
      ()
  in
  Alcotest.(check (array int)) "shape" [| 4; 4 |] (Arr.shape a)

(* Chunks *)

let test_chunks () =
  let s = Store.memory () in
  let a = create s ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |] in
  Alcotest.(check (array int)) "grid" [| 3; 3 |] (Arr.grid_shape a);
  Alcotest.(check bool) "absent" true (Arr.read_chunk_opt a [| 0; 0 |] = None);
  Alcotest.check ints "fill chunk" (List.init 16 (fun _ -> -1))
    (ints_of_slab (Arr.read_chunk a [| 0; 0 |]));
  let c = slab_of_ints [| 4; 4 |] (List.init 16 (fun i -> i + 100)) in
  Arr.write_chunk a [| 1; 1 |] c;
  Alcotest.check ints "round trip"
    (List.init 16 (fun i -> i + 100))
    (ints_of_slab (Option.get (Arr.read_chunk_opt a [| 1; 1 |])));
  Alcotest.(check bool) "key" true
    (Option.is_some (s.Store.get ~key:"a/c/1/1"));
  let bad name f = match f () with
    | _ -> Alcotest.failf "%s: expected Invalid_argument" name
    | exception Invalid_argument _ -> ()
  in
  bad "rank" (fun () -> Arr.read_chunk a [| 0 |]);
  bad "range" (fun () -> Arr.read_chunk a [| 3; 0 |]);
  bad "negative" (fun () -> Arr.read_chunk a [| -1; 0 |]);
  bad "slab shape" (fun () ->
      Arr.write_chunk a [| 0; 0 |] (slab_of_ints [| 2; 2 |] [ 1; 2; 3; 4 ]))

let test_edge_chunk () =
  let s = Store.memory () in
  let a = create s ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |] in
  write_all a [| 10; 10 |];
  (* The far corner chunk holds four array elements. The twelve beyond
     the array keep the fill value. *)
  let want =
    List.concat_map
      (fun i ->
        List.map
          (fun j ->
            if 8 + i < 10 && 8 + j < 10 then ((8 + i) * 10) + 8 + j else -1)
          [ 0; 1; 2; 3 ])
      [ 0; 1; 2; 3 ]
  in
  Alcotest.check ints "corner" want (ints_of_slab (Arr.read_chunk a [| 2; 2 |]))

(* Reading *)

let test_read_2d () =
  let s = Store.memory () in
  let a = create s ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |] in
  write_all a [| 10; 10 |];
  Alcotest.check ints "whole array" (List.init 100 Fun.id)
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 10; 10 |])));
  Alcotest.check ints "four chunks" [ 33; 34; 43; 44 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 3; 3 |] ~shape:[| 2; 2 |])));
  Alcotest.check ints "one whole chunk"
    [ 44; 45; 46; 47; 54; 55; 56; 57; 64; 65; 66; 67; 74; 75; 76; 77 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 4; 4 |] ~shape:[| 4; 4 |])));
  Alcotest.check ints "row across chunks"
    (List.init 10 (fun j -> 50 + j))
    (ints_of_slab (Arr.read a (sub_of ~start:[| 5; 0 |] ~shape:[| 1; 10 |])));
  Alcotest.check ints "column across chunks"
    (List.init 10 (fun i -> (i * 10) + 7))
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 7 |] ~shape:[| 10; 1 |])));
  Alcotest.check ints "empty" []
    (ints_of_slab (Arr.read a (sub_of ~start:[| 1; 1 |] ~shape:[| 0; 3 |])));
  let bad name f = match f () with
    | _ -> Alcotest.failf "%s: expected Invalid_argument" name
    | exception Invalid_argument _ -> ()
  in
  bad "past the end" (fun () ->
      Arr.read a (sub_of ~start:[| 8; 8 |] ~shape:[| 4; 4 |]));
  bad "rank" (fun () -> Arr.read a (sub_of ~start:[| 0 |] ~shape:[| 1 |]))

let test_read_missing () =
  let s = Store.memory () in
  let a = create s ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |] in
  Alcotest.check ints "all absent"
    (List.init 25 (fun _ -> -1))
    (ints_of_slab (Arr.read a (sub_of ~start:[| 2; 2 |] ~shape:[| 5; 5 |])));
  (* One chunk written, the rest absent. *)
  Arr.write_chunk a [| 0; 0 |]
    (slab_of_ints [| 4; 4 |] (List.init 16 (fun i -> i)));
  Alcotest.check ints "half absent" [ 10; 11; -1; 14; 15; -1; -1; -1; -1 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 2; 2 |] ~shape:[| 3; 3 |])))

let test_read_3d () =
  let s = Store.memory () in
  let a = create s ~shape:[| 3; 4; 5 |] ~chunk_shape:[| 2; 2; 2 |] in
  write_all a [| 3; 4; 5 |];
  let at i j k = (i * 20) + (j * 5) + k in
  Alcotest.check ints "whole" (List.init 60 Fun.id)
    (ints_of_slab
       (Arr.read a (sub_of ~start:[| 0; 0; 0 |] ~shape:[| 3; 4; 5 |])));
  let want =
    List.concat_map
      (fun i ->
        List.concat_map
          (fun j -> List.map (fun k -> at (1 + i) (1 + j) (3 + k)) [ 0; 1 ])
          [ 0; 1 ])
      [ 0; 1 ]
  in
  Alcotest.check ints "straddling"
    want
    (ints_of_slab
       (Arr.read a (sub_of ~start:[| 1; 1; 3 |] ~shape:[| 2; 2; 2 |])))

let test_zero_dim () =
  let s = Store.memory () in
  let a = create s ~shape:[||] ~chunk_shape:[||] in
  Alcotest.(check (array int)) "grid" [||] (Arr.grid_shape a);
  Alcotest.(check string) "key" "a/c" (Arr.chunk_key a [||]);
  Alcotest.check ints "fill" [ -1 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[||] ~shape:[||])));
  Arr.write a (sub_of ~start:[||] ~shape:[||]) (slab_of_ints [||] [ 7 ]);
  Alcotest.(check bool) "stored" true (Option.is_some (s.Store.get ~key:"a/c"));
  Alcotest.check ints "read back" [ 7 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[||] ~shape:[||])));
  Alcotest.check ints "chunk" [ 7 ] (ints_of_slab (Arr.read_chunk a [||]))

(* Writing *)

let test_write_rmw () =
  let s = Store.memory () in
  let a = create s ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |] in
  write_all a [| 10; 10 |];
  Arr.write a
    (sub_of ~start:[| 3; 3 |] ~shape:[| 2; 2 |])
    (slab_of_ints [| 2; 2 |] [ 900; 901; 902; 903 ]);
  let want =
    List.init 100 (fun i ->
        match i with
        | 33 -> 900
        | 34 -> 901
        | 43 -> 902
        | 44 -> 903
        | i -> i)
  in
  Alcotest.check ints "neighbours kept" want
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 10; 10 |])));
  (* Writing into an edge chunk leaves the region beyond the array at
     the fill value. *)
  Arr.write a
    (sub_of ~start:[| 9; 9 |] ~shape:[| 1; 1 |])
    (slab_of_ints [| 1; 1 |] [ 42 ]);
  let corner = ints_of_slab (Arr.read_chunk a [| 2; 2 |]) in
  Alcotest.(check int) "written" 42 (List.nth corner 5);
  Alcotest.(check int) "beyond the array" (-1) (List.nth corner 15);
  match
    Arr.write a
      (sub_of ~start:[| 0; 0 |] ~shape:[| 2; 2 |])
      (slab_of_ints [| 1; 4 |] [ 1; 2; 3; 4 ])
  with
  | () -> Alcotest.fail "slab shape: expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let test_write_full_chunk_skips_read () =
  let m = Store.memory () in
  let s, c = counting m in
  let a = create s ~shape:[| 8; 8 |] ~chunk_shape:[| 4; 4 |] in
  reset c;
  write_all a [| 8; 8 |];
  Alcotest.(check int) "no read back" 0 c.get;
  Alcotest.(check int) "four chunks written" 4 c.set;
  reset c;
  (* A partly covered chunk must be read first. *)
  Arr.write a
    (sub_of ~start:[| 0; 0 |] ~shape:[| 2; 2 |])
    (slab_of_ints [| 2; 2 |] [ 1; 2; 3; 4 ]);
  Alcotest.(check int) "read modify write" 1 c.get;
  Alcotest.(check int) "one chunk written" 1 c.set;
  Alcotest.check ints "merged"
    [ 1; 2; 2; 3; 4; 5; 6; 7 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 1; 8 |])))

(* Codec chains and the partial path *)

let test_chain_b2b () =
  let s = Store.memory () in
  let a = create s ~shape:[| 2; 2 |] ~chunk_shape:[| 2; 2 |] ~codecs:xor_c in
  write_all a [| 2; 2 |];
  Alcotest.check ints "round trip" [ 0; 1; 2; 3 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 2; 2 |])));
  let stored = str (Option.get (s.Store.get ~key:"a/c/0/0")) in
  let plain = Base_bigstring.to_string (Slab.bigstring (seq [| 2; 2 |])) in
  Alcotest.(check string) "encoded bytes"
    (String.map (fun ch -> Char.chr (Char.code ch lxor 0xa5)) plain)
    stored

let test_partial_path () =
  let m = Store.memory () in
  let s, c = counting m in
  let a =
    create s ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |]
      ~codecs:raw_partial_c
  in
  write_all a [| 10; 10 |];
  reset c;
  let got = Arr.read a (sub_of ~start:[| 3; 3 |] ~shape:[| 2; 2 |]) in
  Alcotest.check ints "values" [ 33; 34; 43; 44 ] (ints_of_slab got);
  Alcotest.(check int) "no whole fetch" 0 c.get;
  Alcotest.(check bool) "ranged reads" true (c.get_range > 0);
  Alcotest.(check bool) "sized" true (c.size > 0);
  (* Without a partial decoder the same read fetches whole chunks. *)
  let m2 = Store.memory () in
  let s2, c2 = counting m2 in
  let b = create s2 ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |] in
  write_all b [| 10; 10 |];
  reset c2;
  let got2 = Arr.read b (sub_of ~start:[| 3; 3 |] ~shape:[| 2; 2 |]) in
  Alcotest.check ints "values" [ 33; 34; 43; 44 ] (ints_of_slab got2);
  Alcotest.(check int) "no ranged reads" 0 c2.get_range;
  Alcotest.(check int) "four whole fetches" 4 c2.get;
  (* A store that is not ranged never takes the partial path. *)
  let m3 = Store.memory () in
  let s3, c3 = counting m3 in
  let s3 = { s3 with Store.ranged = false } in
  let d =
    create s3 ~shape:[| 10; 10 |] ~chunk_shape:[| 4; 4 |]
      ~codecs:raw_partial_c
  in
  write_all d [| 10; 10 |];
  reset c3;
  ignore (Arr.read d (sub_of ~start:[| 3; 3 |] ~shape:[| 2; 2 |]));
  Alcotest.(check int) "no ranged reads" 0 c3.get_range;
  Alcotest.(check int) "four whole fetches" 4 c3.get

let test_partial_missing () =
  (* A chunk that is absent still reads as the fill value on the ranged
     path. *)
  let s = Store.memory () in
  let a =
    create s ~shape:[| 8; 8 |] ~chunk_shape:[| 4; 4 |] ~codecs:raw_partial_c
  in
  Arr.write_chunk a [| 0; 0 |]
    (slab_of_ints [| 4; 4 |] (List.init 16 (fun i -> i)));
  Alcotest.check ints "mixed" [ 10; 11; -1; 14; 15; -1; -1; -1; -1 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 2; 2 |] ~shape:[| 3; 3 |])))

(* Groups and nodes *)

let test_group () =
  let s = Store.memory () in
  let attributes = json_of_string {|{"g":true}|} in
  let g = Group.create s ~path:"/" ~attributes in
  Alcotest.check json "document"
    (json_of_string
       {|{"zarr_format":3,"node_type":"group","attributes":{"g":true}}|})
    (Store.get_json s ~key:"zarr.json");
  Alcotest.(check string) "path" "/" (Group.path g);
  let h = Group.open_ s ~path:"/" in
  Alcotest.check json "attributes" (json_of_string {|{"g":true}|})
    (Option.get (Group.attributes h));
  let plain = Group.create s ~path:"/g" in
  Alcotest.(check bool) "no attributes" true (Group.attributes plain = None);
  check_msg "missing" "not found"
    (raises_error "missing" (fun () -> Group.open_ s ~path:"/nope"));
  put s "na/zarr.json" (meta_str ());
  check_msg "not a group" "node_type"
    (raises_error "not a group" (fun () -> Group.open_ s ~path:"/na"))

let test_children () =
  let s = Store.memory () in
  let root = Group.create s ~path:"/" in
  let g = Group.create s ~path:"/g" in
  let _ = Group.create s ~path:"/g/x" in
  let a = create s ~shape:[| 4 |] ~chunk_shape:[| 2 |] ~path:"/arr" in
  write_all a [| 4 |];
  Alcotest.(check (list string))
    "root" [ "arr"; "g" ]
    (Option.get (Group.children root));
  Alcotest.(check (list string))
    "nested" [ "x" ]
    (Option.get (Group.children g));
  let no_list = { s with Store.list = None } in
  Alcotest.(check bool) "no list" true
    (Group.children (Group.open_ no_list ~path:"/") = None)

let test_node () =
  let s = Store.memory () in
  let _ = Group.create s ~path:"/g" in
  let _ = create s ~shape:[| 4 |] ~chunk_shape:[| 2 |] ~path:"/a" in
  (match Node.open_ ~codecs:resolver s ~path:"/a" with
  | `Array a -> Alcotest.(check (array int)) "shape" [| 4 |] (Arr.shape a)
  | `Group _ -> Alcotest.fail "expected an array");
  (match Node.open_ s ~path:"/g" with
  | `Group g -> Alcotest.(check string) "path" "/g" (Group.path g)
  | `Array _ -> Alcotest.fail "expected a group");
  check_msg "missing" "not found"
    (raises_error "missing" (fun () -> Node.open_ s ~path:"/z"));
  put s "n/zarr.json" {|{"zarr_format":3,"node_type":"chunk"}|};
  check_msg "node_type" "unknown node_type"
    (raises_error "node_type" (fun () -> Node.open_ s ~path:"/n"));
  put s "n/zarr.json" {|{"zarr_format":3}|};
  check_msg "no node_type" "no node_type"
    (raises_error "no node_type" (fun () -> Node.open_ s ~path:"/n"));
  put s "n/zarr.json" {|[1,2]|};
  check_msg "not an object" "not a JSON object"
    (raises_error "not an object" (fun () -> Node.open_ s ~path:"/n"))

let () =
  Alcotest.run "zarrz store"
    [
      ( "memory",
        [
          ("get and set", `Quick, test_memory_get_set);
          ("ranges", `Quick, test_memory_ranges);
          ("list and erase", `Quick, test_memory_list_erase);
          ("get_json", `Quick, test_get_json);
        ] );
      ( "create",
        [
          ("metadata", `Quick, test_create_metadata);
          ("defaults", `Quick, test_create_defaults);
          ("errors", `Quick, test_create_errors);
        ] );
      ( "open",
        [
          ("missing", `Quick, test_open_missing);
          ("errors", `Quick, test_open_errors);
          ("ignored extensions", `Quick, test_open_ignored);
        ] );
      ( "chunks",
        [
          ("round trip", `Quick, test_chunks);
          ("edge chunk", `Quick, test_edge_chunk);
        ] );
      ( "read",
        [
          ("2-d", `Quick, test_read_2d);
          ("missing chunks", `Quick, test_read_missing);
          ("3-d", `Quick, test_read_3d);
          ("0-d", `Quick, test_zero_dim);
        ] );
      ( "write",
        [
          ("read modify write", `Quick, test_write_rmw);
          ("full chunks", `Quick, test_write_full_chunk_skips_read);
        ] );
      ( "codecs",
        [
          ("bytes to bytes chain", `Quick, test_chain_b2b);
          ("partial path", `Quick, test_partial_path);
          ("partial with absent chunks", `Quick, test_partial_missing);
        ] );
      ( "hierarchy",
        [
          ("group", `Quick, test_group);
          ("children", `Quick, test_children);
          ("node", `Quick, test_node);
        ] );
    ]
