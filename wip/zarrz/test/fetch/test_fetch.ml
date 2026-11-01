(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for the HTTP store. Every exchange goes through [Fetch_mock],
   so the assertions are on the exact bytes the store puts on the wire
   and on what it makes of the answers it gets back. *)

module Arr = Zarrz.Arr
module Byte_range = Zarrz.Byte_range
module Dtype = Zarrz.Dtype
module Error = Zarrz.Error
module Ext = Zarrz.Ext
module Fill_value = Zarrz.Fill_value
module Group = Zarrz.Group
module Node = Zarrz.Node
module Slab = Zarrz.Slab
module Store = Zarrz.Store
module Subset = Zarrz.Subset
module Url = Fetch.Middleware.Url
module Ia = Stdlib_stable.Iarray
module I32u = Stdlib_upstream_compatible.Int32_u

(* Without this a failing test reports a bare [Zarrz__Error.E(_)]. *)
let () =
  Printexc.register_printer (function
    | Error.E e -> Some ("Zarrz.Error.E: " ^ Error.to_string e)
    | _ -> None)

let base = "http://zarr.example/data"
let run f = Eio_main.run (fun _env -> f ())
let str = Base_bigstring.to_string

(* {1 The mock origin} *)

(* One request as the origin saw it. [range] is the raw header value,
   which is what the range assertions compare. *)
type entry = { meth : string; url : string; range : string option }

type origin = {
  objs : (string, string) Hashtbl.t;
  forced : (string, int) Hashtbl.t;  (** A status to answer [key] with. *)
  mutable log : entry list;  (** Newest first. *)
  mutable ranged_gets : int;
  mutable vanish_after : int;
      (** Answer 404 once this many ranged GETs have been served.
          [max_int] never vanishes. *)
  mutable in_flight : int;
  mutable peak : int;
}

let origin () =
  {
    objs = Hashtbl.create 16;
    forced = Hashtbl.create 4;
    log = [];
    ranged_gets = 0;
    vanish_after = max_int;
    in_flight = 0;
    peak = 0;
  }

let put o key body = Hashtbl.replace o.objs key body
let log o = List.rev o.log
let urls o = List.map (fun e -> e.url) (log o)
let ranges o = List.filter_map (fun e -> e.range) (log o)

(* The key a request names, which is its path under the base URL. *)
let key_of_url u =
  let p = Url.path_and_query u in
  let prefix = "/data/" in
  if String.starts_with ~prefix p then
    String.sub p (String.length prefix) (String.length p - String.length prefix)
  else Alcotest.failf "request outside the base URL: %s" p

type spec = Rng of int * int option | Sfx of int

let parse_range s =
  let prefix = "bytes=" in
  if not (String.starts_with ~prefix s) then None
  else
    let s = String.sub s 6 (String.length s - 6) in
    match String.index_opt s '-' with
    | None -> None
    | Some i -> (
        let a = String.sub s 0 i in
        let b = String.sub s (i + 1) (String.length s - i - 1) in
        match (a, b) with
        | "", n -> Some (Sfx (int_of_string n))
        | f, "" -> Some (Rng (int_of_string f, None))
        | f, l -> Some (Rng (int_of_string f, Some (int_of_string l))))

(* The span a spec denotes in an object of [size] bytes, truncated the
   way RFC 9110 truncates, and [None] when it is unsatisfiable. *)
let span ~size = function
  | Rng (first, _) when first >= size -> None
  | Rng (first, None) -> Some (first, size - first)
  | Rng (first, Some last) ->
      Some (first, min (size - first) (last - first + 1))
  | Sfx 0 -> None
  | Sfx n -> Some (max 0 (size - n), min n size)

(* [handler o ~mode ~clen ~probe] answers from [o]. [mode] is what the
   origin does with a [Range] header: honour it, ignore it and send the
   whole object with 200, or reject an unsatisfiable one with 416.
   [clen] sets [Content-Length], and [probe] makes each request yield
   once so that concurrent ones overlap observably. *)
let handler o ~mode ~clen ~probe (req : Fetch.Middleware.request) =
  let url = Url.to_string req.url in
  let key = key_of_url req.url in
  let range = Http.Header.get req.headers "range" in
  o.log <- { meth = Http.Method.to_string req.meth; url; range } :: o.log;
  if probe then begin
    o.in_flight <- o.in_flight + 1;
    o.peak <- max o.peak o.in_flight;
    Eio.Fiber.yield ();
    o.in_flight <- o.in_flight - 1
  end;
  (* [len] overrides the declared length, which a HEAD needs: it names
     the length of the object, not of the empty body it sends. *)
  let reply ?(status = 200) ?len ?(headers = []) body =
    let headers =
      if clen then
        let n = match len with Some n -> n | None -> String.length body in
        ("content-length", string_of_int n) :: headers
      else headers
    in
    Fetch_mock.respond ~status ~headers:(Http.Header.of_list headers) body req
  in
  match Hashtbl.find_opt o.forced key with
  | Some status -> reply ~status ""
  | None -> (
      match Hashtbl.find_opt o.objs key with
      | None -> reply ~status:404 ""
      | Some body -> (
          let size = String.length body in
          if req.meth = `HEAD then reply ~len:size ""
          else
            match (range, mode) with
            | None, _ | Some _, `Ignore -> reply body
            | Some r, (`Honour | `Oob416) -> (
                o.ranged_gets <- o.ranged_gets + 1;
                if o.ranged_gets > o.vanish_after then reply ~status:404 ""
                else
                  match parse_range r with
                  | None -> reply ~status:400 ""
                  | Some s -> (
                      match span ~size s with
                      | None when mode = `Oob416 ->
                          reply ~status:416
                            ~headers:
                              [
                                ( "content-range",
                                  Printf.sprintf "bytes */%d" size );
                              ]
                            ""
                      | None -> reply ~status:206 ""
                      | Some (off, len) ->
                          reply ~status:206
                            ~headers:
                              [
                                ( "content-range",
                                  Printf.sprintf "bytes %d-%d/%d" off
                                    (off + len - 1) size );
                              ]
                            (String.sub body off len)))))

let client ?(mode = `Honour) ?(clen = true) ?(probe = false) o =
  Fetch_mock.client (handler o ~mode ~clen ~probe)

let store ?mode ?clen ?probe ?ranged o =
  Zarrz_fetch.store ?ranged ~base_url:base (client ?mode ?clen ?probe o)

(* {1 Assertions} *)

let check_str = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_strs = Alcotest.(check (list string))
let check_bool = Alcotest.(check bool)

let check_bytes name want got =
  check_str name want (match got with None -> "<none>" | Some b -> str b)

let raises_store name f =
  match f () with
  | _ -> Alcotest.failf "%s: expected a store error" name
  | exception Error.E (Error.Store m) -> m
  | exception e -> Alcotest.failf "%s: expected a store error, got %s" name
                     (Printexc.to_string e)

let contains ~needle s =
  let n = String.length needle and l = String.length s in
  let rec go i = i + n <= l && (String.sub s i n = needle || go (i + 1)) in
  go 0

let check_contains name needle s =
  check_bool (Printf.sprintf "%s mentions %s" name needle) true
    (contains ~needle s)

(* {1 URLs} *)

let test_urls () =
  run @@ fun () ->
  let o = origin () in
  put o "zarr.json" "{}";
  put o "a/b/zarr.json" "{}";
  put o "a/b/c/0/0" "chunk";
  let s = store o in
  ignore (s.Store.get ~key:"zarr.json");
  ignore (s.Store.get ~key:"a/b/zarr.json");
  ignore (s.Store.get ~key:"a/b/c/0/0");
  ignore (s.Store.size ~key:"a/b/c/0/0");
  check_strs "exact URLs"
    [
      "http://zarr.example/data/zarr.json";
      "http://zarr.example/data/a/b/zarr.json";
      "http://zarr.example/data/a/b/c/0/0";
      "http://zarr.example/data/a/b/c/0/0";
    ]
    (urls o);
  check_strs "methods" [ "GET"; "GET"; "GET"; "HEAD" ]
    (List.map (fun e -> e.meth) (log o))

let test_base_url () =
  let bad name b =
    match Zarrz_fetch.store ~base_url:b (client (origin ())) with
    | _ -> Alcotest.failf "%s: expected Invalid_argument" name
    | exception Invalid_argument _ -> ()
  in
  bad "empty" "";
  bad "trailing slash" "http://zarr.example/data/";
  bad "bare slash" "/"

(* {1 get} *)

let test_get_statuses () =
  run @@ fun () ->
  let o = origin () in
  put o "hit" "payload";
  Hashtbl.replace o.forced "gone" 410;
  Hashtbl.replace o.forced "broken" 500;
  Hashtbl.replace o.forced "teapot" 418;
  let s = store o in
  check_bytes "200" "payload" (s.Store.get ~key:"hit");
  check_bool "404" true (s.Store.get ~key:"absent" = None);
  check_bool "410" true (s.Store.get ~key:"gone" = None);
  let m = raises_store "500" (fun () -> s.Store.get ~key:"broken") in
  check_contains "500 message" "500" m;
  check_contains "500 message" "http://zarr.example/data/broken" m;
  let m = raises_store "418" (fun () -> s.Store.get ~key:"teapot") in
  check_contains "418 message" "418" m

let test_body_buffering () =
  run @@ fun () ->
  (* Bigger than the growing buffer's first allocation, so the unknown
     length path doubles at least twice. *)
  let big = String.init 200_000 (fun i -> Char.chr (i land 0xff)) in
  let o = origin () in
  put o "big" big;
  put o "empty" "";
  let sized = store o in
  let unsized = store ~clen:false o in
  check_bytes "sized" big (sized.Store.get ~key:"big");
  check_bytes "unsized" big (unsized.Store.get ~key:"big");
  check_bytes "empty sized" "" (sized.Store.get ~key:"empty");
  check_bytes "empty unsized" "" (unsized.Store.get ~key:"empty")

let test_short_body () =
  run @@ fun () ->
  (* A Content-Length that overstates the body is an HTTP-level
     surprise, not a silent truncation. *)
  let c =
    Fetch_mock.client (fun req ->
        Fetch_mock.respond ~status:200
          ~headers:(Http.Header.of_list [ ("content-length", "99") ])
          "short" req)
  in
  let s = Zarrz_fetch.store ~base_url:base c in
  let m = raises_store "short body" (fun () -> s.Store.get ~key:"k") in
  check_contains "short body" "5 of the 99" m

(* {1 get_range} *)

let alphabet = String.init 256 Char.chr

let test_range_headers () =
  run @@ fun () ->
  let o = origin () in
  put o "obj" alphabet;
  let s = store o in
  let got r = s.Store.get_range ~key:"obj" r in
  check_bytes "first hundred" (String.sub alphabet 0 100)
    (got (Byte_range.From_start { off = 0; len = Some 100 }));
  check_bytes "last sixteen" (String.sub alphabet 240 16)
    (got (Byte_range.Suffix 16));
  check_bytes "open ended" (String.sub alphabet 100 156)
    (got (Byte_range.From_start { off = 100; len = None }));
  check_strs "exact Range headers"
    [ "bytes=0-99"; "bytes=-16"; "bytes=100-" ]
    (ranges o);
  check_int "one request each" 3 (List.length (log o))

let test_range_empty () =
  run @@ fun () ->
  (* No [Range] header spells zero bytes, so the store answers without
     asking the origin anything. *)
  let o = origin () in
  put o "obj" alphabet;
  let s = store o in
  let got r = s.Store.get_range ~key:"obj" r in
  check_bytes "zero length" ""
    (got (Byte_range.From_start { off = 8; len = Some 0 }));
  check_bytes "zero suffix" "" (got (Byte_range.Suffix 0));
  check_int "no requests" 0 (List.length (log o))

let test_range_ignored () =
  run @@ fun () ->
  (* The origin sends 200 and the whole object. The store must slice it
     itself, with the same truncation a resolved range has. *)
  let o = origin () in
  put o "obj" alphabet;
  let s = store ~mode:`Ignore o in
  let got r = s.Store.get_range ~key:"obj" r in
  check_bytes "from start" (String.sub alphabet 10 20)
    (got (Byte_range.From_start { off = 10; len = Some 20 }));
  check_bytes "suffix" (String.sub alphabet 226 30)
    (got (Byte_range.Suffix 30));
  check_bytes "open ended" (String.sub alphabet 250 6)
    (got (Byte_range.From_start { off = 250; len = None }));
  check_bytes "truncated length" (String.sub alphabet 250 6)
    (got (Byte_range.From_start { off = 250; len = Some 99 }));
  check_bytes "past the end" ""
    (got (Byte_range.From_start { off = 300; len = Some 4 }));
  check_bytes "suffix past the start" alphabet (got (Byte_range.Suffix 999));
  check_strs "the header was still sent"
    [
      "bytes=10-29"; "bytes=-30"; "bytes=250-"; "bytes=250-348";
      "bytes=300-303"; "bytes=-999";
    ]
    (ranges o)

let test_range_unsatisfiable () =
  run @@ fun () ->
  let o = origin () in
  put o "obj" alphabet;
  let s = store ~mode:`Oob416 o in
  let m =
    raises_store "416" (fun () ->
        s.Store.get_range ~key:"obj"
          (Byte_range.From_start { off = 1000; len = Some 4 }))
  in
  check_contains "416 message" "416" m;
  check_contains "416 message" "http://zarr.example/data/obj" m

let test_range_missing () =
  run @@ fun () ->
  let o = origin () in
  Hashtbl.replace o.forced "gone" 410;
  let s = store o in
  check_bool "404" true
    (s.Store.get_range ~key:"absent" (Byte_range.Suffix 4) = None);
  check_bool "410" true
    (s.Store.get_range ~key:"gone" (Byte_range.Suffix 4) = None)

(* {1 get_ranges} *)

let test_ranges_empty () =
  run @@ fun () ->
  let o = origin () in
  put o "obj" alphabet;
  let s = store o in
  check_bool "empty list" true (s.Store.get_ranges ~key:"obj" [] = Some []);
  check_int "no requests" 0 (List.length (log o))

let test_ranges_concurrent () =
  run @@ fun () ->
  let o = origin () in
  put o "obj" alphabet;
  let s = store ~probe:true o in
  let rs =
    List.init 8 (fun i -> Byte_range.From_start { off = i * 8; len = Some 8 })
  in
  let got = Option.get (s.Store.get_ranges ~key:"obj" rs) in
  check_strs "in order"
    (List.init 8 (fun i -> String.sub alphabet (i * 8) 8))
    (List.map str got);
  check_int "one request per range" 8 (List.length (log o));
  check_int "six in flight at the peak" 6 o.peak

let test_ranges_vanished () =
  run @@ fun () ->
  (* One range answering 404 means the object went away mid read, so
     the whole batch is [None] rather than a short list. *)
  let o = origin () in
  put o "obj" alphabet;
  o.vanish_after <- 2;
  let s = store o in
  let rs =
    List.init 4 (fun i -> Byte_range.From_start { off = i * 8; len = Some 8 })
  in
  check_bool "vanished" true (s.Store.get_ranges ~key:"obj" rs = None)

(* {1 size} *)

let test_size () =
  run @@ fun () ->
  let o = origin () in
  put o "obj" alphabet;
  Hashtbl.replace o.forced "gone" 410;
  Hashtbl.replace o.forced "broken" 503;
  let s = store o in
  check_bool "length" true (s.Store.size ~key:"obj" = Some 256);
  check_bool "absent" true (s.Store.size ~key:"absent" = None);
  check_bool "410" true (s.Store.size ~key:"gone" = None);
  check_strs "HEAD throughout" [ "HEAD"; "HEAD"; "HEAD" ]
    (List.map (fun e -> e.meth) (log o));
  let m = raises_store "503" (fun () -> s.Store.size ~key:"broken") in
  check_contains "503 message" "HEAD" m;
  (* An origin that answers HEAD without a length is indistinguishable
     from one with no such object, which is what [Store.size] says. *)
  let unsized = store ~clen:false o in
  check_bool "no length header" true (unsized.Store.size ~key:"obj" = None)

(* {1 An end to end hierarchy} *)

let json_t = Alcotest.testable Jsont.Json.pp Jsont.Json.equal

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.failf "test JSON is invalid: %s" m

let exts_of_string s =
  match Jsont_bytesrw.decode_string (Jsont.list Ext.jsont) s with
  | Ok l -> l
  | Error m -> Alcotest.failf "test codec metadata is invalid: %s" m

let bytes_crc32c =
  {|[{"name":"bytes","configuration":{"endian":"little"}},
     {"name":"crc32c"}]|}

let sharded ~inner_shape =
  Printf.sprintf
    {|[{"name":"sharding_indexed","configuration":{
         "chunk_shape":[%s],
         "codecs":%s,
         "index_codecs":%s,
         "index_location":"end"}}]|}
    (String.concat "," (List.map string_of_int inner_shape))
    bytes_crc32c bytes_crc32c

let set32 s i v = Slab.I32.set s i (I32u.of_int32 (Int32.of_int v))
let get32 s i = Int32.to_int (I32u.to_int32 (Slab.I32.get s i))
let ints_of_slab s = List.init (Slab.num_elements s) (get32 s)
let sub_of ~start ~shape =
  { Subset.start = Ia.of_array start; shape = Ia.of_array shape }

let fill_of v =
  let s = Slab.create Dtype.Int32 [: 1 :] in
  set32 s 0 v;
  Fill_value.of_bytes (str (Slab.bigstring s))

(* [seq shape] holds its own C-order index at every element. *)
let seq shape =
  let s = Slab.create Dtype.Int32 (Ia.of_array shape) in
  for i = 0 to Slab.num_elements s - 1 do
    set32 s i i
  done;
  s

(* Builds the hierarchy in a memory store and copies every key of it
   into [o], so the mock serves bytes a real writer produced. *)
let publish o ~codecs ~shape ~chunk_shape =
  let m = Store.memory () in
  ignore
    (Group.create m ~path:"/" ~attributes:(json_of_string {|{"kind":"root"}|}));
  let a =
    Arr.create ~codecs:(exts_of_string codecs) ~shape ~chunk_shape
      ~dtype:Dtype.Int32 ~fill_value:(fill_of (-1)) m ~path:"/a"
  in
  let start = Array.map (fun _ -> 0) shape in
  Arr.write a (sub_of ~start ~shape) (seq shape);
  List.iter
    (fun key ->
      put o key (str (Option.get (m.Store.get ~key))))
    ((Option.get m.Store.list) ~prefix:"");
  m

let test_e2e_plain () =
  run @@ fun () ->
  let o = origin () in
  let _ =
    publish o ~codecs:bytes_crc32c ~shape:[| 4; 4 |] ~chunk_shape:[| 2; 2 |]
  in
  let s = store o in
  let a = Arr.open_ s ~path:"/a" in
  check_bool "shape" true (Arr.shape a = [| 4; 4 |]);
  check_bool "dtype" true (Dtype.equal (Arr.dtype a) Dtype.Int32);
  Alcotest.(check (list int))
    "whole array"
    (List.init 16 Fun.id)
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 4; 4 |])));
  (* A subset straddling all four chunks. *)
  Alcotest.(check (list int))
    "straddling"
    [ 5; 6; 9; 10 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 1; 1 |] ~shape:[| 2; 2 |])));
  check_bool "metadata was fetched" true
    (List.exists
       (fun e -> e.url = "http://zarr.example/data/a/zarr.json")
       (log o))

let test_e2e_node () =
  run @@ fun () ->
  let o = origin () in
  let _ =
    publish o ~codecs:bytes_crc32c ~shape:[| 2; 2 |] ~chunk_shape:[| 2; 2 |]
  in
  let s = store o in
  (match Node.open_ s ~path:"/" with
  | `Group g ->
      Alcotest.check json_t "root attributes"
        (json_of_string {|{"kind":"root"}|})
        (Option.get (Group.attributes g));
      (* No [list], so children are unknowable over HTTP. *)
      check_bool "no children" true (Group.children g = None)
  | `Array _ -> Alcotest.fail "the root is a group");
  (match Node.open_ s ~path:"/a" with
  | `Array a -> check_bool "array shape" true (Arr.shape a = [| 2; 2 |])
  | `Group _ -> Alcotest.fail "/a is an array");
  let g = Group.open_ s ~path:"/" in
  check_str "group path" "/" (Group.path g)

(* The encoded size of a shard index: two uint64 per inner chunk plus
   the four bytes crc32c appends. *)
let index_size ~inner_chunks = (16 * inner_chunks) + 4

let test_e2e_sharded () =
  run @@ fun () ->
  let o = origin () in
  let _ =
    publish o
      ~codecs:(sharded ~inner_shape:[ 2; 2 ])
      ~shape:[| 4; 4 |] ~chunk_shape:[| 4; 4 |]
  in
  let s = store o in
  let a = Arr.open_ s ~path:"/a" in
  Alcotest.(check (list int))
    "whole array"
    (List.init 16 Fun.id)
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 4; 4 |])));
  (* Now read one inner chunk and account for every byte asked for. *)
  o.log <- [];
  Alcotest.(check (list int))
    "one inner chunk"
    [ 0; 1; 4; 5 ]
    (ints_of_slab (Arr.read a (sub_of ~start:[| 0; 0 |] ~shape:[| 2; 2 |])));
  let shard = "http://zarr.example/data/a/c/0/0" in
  let entries = log o in
  check_bool "every request is for the shard" true
    (List.for_all (fun e -> e.url = shard) entries);
  check_strs "HEAD then two ranged GETs" [ "HEAD"; "GET"; "GET" ]
    (List.map (fun e -> e.meth) entries);
  (* The index is at the end, so a suffix range, and the inner chunk is
     the first in the shard: four int32 plus crc32c, at offset 0. *)
  check_strs "index suffix then inner chunk"
    [ Printf.sprintf "bytes=-%d" (index_size ~inner_chunks:4); "bytes=0-19" ]
    (ranges o);
  check_bool "no whole shard GET" true
    (not
       (List.exists (fun e -> e.meth = "GET" && e.range = None) entries));
  (* Told the origin ignores ranges, the core stops asking for them. *)
  let plain = store ~ranged:false o in
  let b = Arr.open_ plain ~path:"/a" in
  o.log <- [];
  Alcotest.(check (list int))
    "same values"
    [ 0; 1; 4; 5 ]
    (ints_of_slab (Arr.read b (sub_of ~start:[| 0; 0 |] ~shape:[| 2; 2 |])));
  let entries = log o in
  check_strs "one whole GET" [ "GET" ] (List.map (fun e -> e.meth) entries);
  check_strs "with no Range header" [] (ranges o)

(* {1 Suites} *)

let () =
  Alcotest.run "zarrz-fetch"
    [
      ( "urls",
        [
          ("key to URL", `Quick, test_urls);
          ("base_url", `Quick, test_base_url);
        ] );
      ( "get",
        [
          ("statuses", `Quick, test_get_statuses);
          ("body buffering", `Quick, test_body_buffering);
          ("short body", `Quick, test_short_body);
        ] );
      ( "get_range",
        [
          ("headers", `Quick, test_range_headers);
          ("empty", `Quick, test_range_empty);
          ("ignored", `Quick, test_range_ignored);
          ("unsatisfiable", `Quick, test_range_unsatisfiable);
          ("missing", `Quick, test_range_missing);
        ] );
      ( "get_ranges",
        [
          ("empty", `Quick, test_ranges_empty);
          ("concurrent", `Quick, test_ranges_concurrent);
          ("vanished", `Quick, test_ranges_vanished);
        ] );
      ("size", [ ("head", `Quick, test_size) ]);
      ( "hierarchy",
        [
          ("plain array", `Quick, test_e2e_plain);
          ("nodes", `Quick, test_e2e_node);
          ("sharded array", `Quick, test_e2e_sharded);
        ] );
    ]
