(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module P = Perma_proxy
module C = P.Cache
module R = P.Coverage
module M = Proffer_mock
let check name ok = if not ok then failwith name
let get = function Some x -> x | None -> failwith "Expected a cached object"
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
let string_json j = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json j)
let request proxy ?(meth = Httpz.Method.Get) ?range path =
  M.request P.site proxy meth path
    ~on_error:(fun exn -> raise exn)
    ~headers:(Option.fold ~none:[] ~some:(fun r -> ["Range", r]) range)
let status r = Httpz.Res.status_code (M.status r)
let field r name =
  let result = ref None in
  Proffer.Headers.iter (fun _ key value ->
      if Proffer.Req.globalize key = name then
        result := Some (Proffer.Req.globalize value)) (M.headers r);
  !result

let with_dir env f =
  let tmp = Filename.temp_dir "perma-proxy-test-" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect ~finally:(fun () ->
      List.iter (fun name -> Eio.Path.unlink Eio.Path.(dir / name))
        (Eio.Path.read_dir dir);
      Eio.Path.rmdir dir) (fun () -> f dir)

let test_coverage () =
  for size = 0 to 60 do
    let ranges = ref [] and present = Array.make size false in
    for i = 0 to 100 do
      let a = if size = 0 then 0 else (i * 7) mod (size + 1) in
      let b = if size = 0 then 0 else (i * 11) mod (size + 1) in
      let a, b = min a b, max a b in
      ranges := R.merge !ranges (R.v a b);
      for p = a to b - 1 do present.(p) <- true done;
      let holes = R.missing !ranges (R.v 0 size) in
      for p = 0 to size - 1 do
        check "coverage preserves exactly the written bytes"
          (R.covers !ranges (R.v p (p+1)) = present.(p));
        check "missing is the complement"
          (List.exists (fun r -> r.R.start <= p && p < r.stop) holes = not present.(p))
      done
    done
  done

let test_manifest () =
  let module Manifest = P.Manifest in
  let m : Manifest.t = {
    url = "https://test.invalid/data"; absent = false;
    generation = String.make 32 'a'; size = 20;
    content_type = "application/octet-stream";
    etag = Some "\"a\""; modified = None;
    ranges = [R.v 0 5; R.v 10 20];
  } in
  let encoded = Result.get_ok (Jsont_bytesrw.encode_string Manifest.jsont m) in
  check "manifest round trip"
    (Jsont_bytesrw.decode_string Manifest.jsont encoded = Ok m);
  let fields = match json encoded with
    | Jsont.Object (fields, _) -> fields
    | _ -> failwith "Expected manifest object" in
  let named name ((key, _), _) = key = name in
  let change name value =
    let fields = List.filter (fun f -> not (named name f)) fields in
    string_json (Jsont.Json.object' ((Jsont.Json.name name, json value) :: fields)) in
  let reject name value =
    check ("manifest rejects " ^ name ^ "=" ^ value)
      (Result.is_error (Jsont_bytesrw.decode_string Manifest.jsont (change name value))) in
  let legacy = string_json (Jsont.Json.object'
      (List.filter (fun f -> not (named "version" f)) fields)) in
  check "legacy manifest accepted"
    (Jsont_bytesrw.decode_string Manifest.jsont legacy = Ok m);
  reject "version" "2";
  reject "generation" "\"../unsafe\"";
  List.iter (reject "size") ["-1"; "1.5"; "\"20\""; "9007199254740992"];
  List.iter (reject "ranges") [
    "[{\"start\":5,\"stop\":4}]";
    "[{\"start\":0,\"stop\":0}]";
    "[{\"start\":0,\"stop\":21}]";
    "[{\"start\":0.5,\"stop\":1}]";
    "[{\"start\":0,\"stop\":5},{\"start\":5,\"stop\":10}]";
    "[{\"start\":10,\"stop\":20},{\"start\":0,\"stop\":5}]";
    "[{\"start\":0,\"stop\":10},{\"start\":5,\"stop\":20}]";
  ];
  reject "absent" "true";
  check "invalid manifest encoding rejected"
    (Result.is_error (Jsont_bytesrw.encode_string Manifest.jsont
       {m with ranges = [R.v 0 21]}));
  check "unknown manifest field rejected"
    (Result.is_error (Jsont_bytesrw.decode_string Manifest.jsont
       (change "unexpected" "true")))

let test_cache env = with_dir env (fun dir ->
    let calls = ref [] and revision = ref "a" and truncate = ref false in
    let client = Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
        let range = Http.Header.get req.headers "range" in
        calls := (req.meth, url, range) :: !calls;
        let range = if String.ends_with ~suffix:"ignored" url then None else range in
        let body = if !revision = "a" then "0123456789abcdefghijklmnopqrstuvwxyz"
          else "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
        let size = String.length body in
        let headers = ["etag", "\"" ^ !revision ^ "\""; "content-type", "application/octet-stream"] in
        if String.ends_with ~suffix:"missing" url then Fetch_mock.respond ~status:404 "" req
        else if req.meth = `HEAD then Fetch_mock.respond "" req
            ~headers:(Http.Header.of_list (headers @ ["content-length", string_of_int size]))
        else if Option.fold ~none:false ~some:(fun tag -> tag <> "\"" ^ !revision ^ "\"")
                  (Http.Header.get req.headers "if-match") then
          Fetch_mock.respond ~status:412 "" req
        else
          let r = match range with None -> R.v 0 size
            | Some s -> C.resolve size (match Fetch.Header.decode Fetch.Header.range s with
                | Some { ranges = [`Range (off, last)]; _ } ->
                    C.From (Int64.to_int off, Option.map (fun n -> Int64.to_int n - Int64.to_int off + 1) last)
                | Some { ranges = [`Suffix n]; _ } -> C.Suffix (Int64.to_int n)
                | _ -> failwith "Bad test range") in
          let text = String.sub body r.start (r.stop - r.start) in
          let text = if !truncate && text <> "" then String.sub text 0 (String.length text - 1) else text in
          let headers = headers @ ["content-length", string_of_int (r.stop-r.start)] @
            (if range = None then [] else ["content-range",
              Printf.sprintf "bytes %d-%d/%d" r.start (r.stop-1) size]) in
          Fetch_mock.respond ~status:(if range = None then 200 else 206)
            ~headers:(Http.Header.of_list headers) text req) in
    let cache () = C.create ~dir ~client ~random:(Eio.Stdenv.secure_random env) in
    let proxy () = P.create ~cache:(cache ()) [P.mapping ~prefix:"/data" ~upstream:"https://test.invalid/store"] in
    let p = proxy () in
    let r = request p ~range:"bytes=4-7" "/data/x" in
    check "range body" (M.body r = "4567" && status r = 206);
    check "partial is not complete" (field r "X-Cache-Complete" = Some "false");
    let count = List.length !calls in
    check "repeated range" (M.body (request p ~range:"bytes=4-7" "/data/x") = "4567");
    ignore (request p ~meth:Httpz.Method.Head "/data/x");
    check "hits and HEAD make no upstream request" (List.length !calls = count);
    ignore (request p ~range:"bytes=6-11" "/data/x");
    check "overlap fetches just the gap"
      (let _,_,r = List.hd !calls in r = Some "bytes=8-11");
    let count = List.length !calls in
    let restarted = proxy () in
    let p = restarted in
    check "restart serves bytes" (M.body (request restarted ~range:"bytes=4-11" "/data/x") = "456789ab");
    check "restart causes no upstream requests" (List.length !calls = count);
    let full = request restarted "/data/x" in
    check "full assembly" (M.body full = "0123456789abcdefghijklmnopqrstuvwxyz");
    check "full coverage" (field full "X-Cache-Complete" = Some "true");
    let count = List.length !calls in
    check "suffix from full" (M.body (request p ~range:"bytes=-3" "/data/x") = "xyz");
    check "open range from full" (M.body (request p ~range:"bytes=33-" "/data/x") = "xyz");
    check "assembled shard reused" (List.length !calls = count);
    check "unsatisfiable" (status (request p ~range:"bytes=100-101" "/data/x") = 416);
    check "HEAD ignores Range"
      (status (request p ~meth:Httpz.Method.Head ~range:"bytes=0-1,3-4" "/data/x") = 200);
    check "ignored upstream range sliced correctly"
      (M.body (request p ~range:"bytes=1-2" "/data/ignored") = "12");
    let count = List.length !calls in
    ignore (request p "/data/ignored");
    check "ignored range cached whole object" (List.length !calls = count);
    check "multi-range rejected" (status (request p ~range:"bytes=0-1,3-4" "/data/x") = 400);
    check "traversal refused" (status (request p "/data/../x") = 400);
    check "prefix boundary" (status (request p "/database/x") = 404);
    check "cached missing" (status (request p "/data/missing") = 404);
    let count = List.length !calls in
    ignore (request (proxy ()) "/data/missing");
    check "negative cache survives restart" (List.length !calls = count);
    ignore (request p ~range:"bytes=0-2" "/data/change");
    revision := "b";
    check "revision change restarts without mixing bytes"
      (M.body (request p ~range:"bytes=1-4" "/data/change") = "BCDE");
    check "old bytes invalidated" (M.body (request p ~range:"bytes=0-4" "/data/change") = "ABCDE");
    truncate := true;
    check "truncated response fails" (status (request p ~range:"bytes=0-3" "/data/broken") = 502);
    truncate := false;
    let repaired = request p ~range:"bytes=0-3" "/data/broken" in
    if M.body repaired <> "ABCD" then
      failwith (Printf.sprintf "Failed retry: status %d body %S" (status repaired) (M.body repaired));
    let count = List.length !calls in
    Eio.Fiber.both
      (fun () -> ignore (request p ~range:"bytes=0-3" "/data/concurrent"))
      (fun () -> ignore (request p ~range:"bytes=0-3" "/data/concurrent"));
    check "concurrent misses are single flight" (List.length !calls = count + 1);
    let count = List.length !calls in
    ignore (request p "/data/x?version=2");
    check "queries have separate cache keys" (List.length !calls = count + 1))

let test_shard ~at_start env = with_dir env (fun dir ->
    let store = Zarrz.Store.memory () in
    let codec = Result.get_ok (Jsont.Json.decode Zarrz.Ext.jsont (json
        (Printf.sprintf {|{"name":"sharding_indexed","configuration":{
          "chunk_shape":[4],"codecs":[{"name":"bytes"}],
          "index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},
                          {"name":"crc32c"}],"index_location":"%s"}}|}
          (if at_start then "start" else "end")))) in
    let a = Zarrz.Arr.create ~shape:[|12|] ~chunk_shape:[|12|]
        ~dtype:Zarrz.Dtype.Uint8 ~fill_value:(Result.get_ok
          (Zarrz.Fill_value.of_json Zarrz.Dtype.Uint8 (json "0")))
        ~codecs:[codec] store ~path:"/a" in
    let slab = Zarrz.Slab.create Zarrz.Dtype.Uint8 [:12:] in
    let view = Bigarray.reshape_1 (Zarrz.Slab.to_genarray slab Bigarray.int8_unsigned) 12 in
    for i = 0 to 11 do Bigarray.Array1.set view i (i+1) done;
    Zarrz.Arr.write a {Zarrz.Subset.start=[:0:]; shape=[:12:]} slab;
    let calls = ref 0 in
    let client = Fetch_mock.client (fun req ->
        incr calls;
        let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
        let key = String.sub url (String.length "https://test.invalid/")
            (String.length url - String.length "https://test.invalid/") in
        match store.get ~key with
        | None -> Fetch_mock.respond ~status:404 "" req
        | Some b ->
            let body = Base_bigstring.to_string b in
            let size = String.length body in
            let selection = match Http.Header.get req.headers "range" with
              | None -> C.Whole
              | Some s -> (match Fetch.Header.decode Fetch.Header.range s with
                | Some {ranges=[`Range (a,Some b)];_} -> C.From (Int64.to_int a,Some (Int64.to_int (Int64.sub b a)+1))
                | Some {ranges=[`Suffix n];_} -> C.Suffix (Int64.to_int n)
                | _ -> failwith "Bad test range") in
            let r = C.resolve size selection in
            let headers = ["etag","\"shard\""; "content-length",string_of_int (if req.meth = `HEAD then size else r.stop-r.start)] @
              (if selection = C.Whole then [] else ["content-range",Printf.sprintf "bytes %d-%d/%d" r.start (r.stop-1) size]) in
            Fetch_mock.respond ~headers:(Http.Header.of_list headers)
              ~status:(if selection = C.Whole then 200 else 206)
              (if req.meth = `HEAD then "" else String.sub body r.start (r.stop-r.start)) req) in
    let cache = C.create ~dir ~client ~random:(Eio.Stdenv.secure_random env) in
    let p = P.create ~cache [P.mapping ~prefix:"/z" ~upstream:"https://test.invalid"] in
    ignore (request p "/z/a/zarr.json");
    let body = Base_bigstring.to_string (get (store.get ~key:"a/c/0")) in
    let payload = if at_start then 52 else 0 in
    let range a b = Printf.sprintf "bytes=%d-%d" (payload+a) (payload+b) in
    let first = request p ~range:(range 1 1) "/z/a/c/0" in
    check "shard slice" (M.body first = String.sub body (payload+1) 1);
    let count = !calls in
    check "inner chunk expanded" (M.body (request p ~range:(range 0 3) "/z/a/c/0") = String.sub body payload 4);
    check "rest of inner chunk is local" (!calls = count);
    let index_header = if at_start then "bytes=0-51" else "bytes=-52" in
    let index_offset = if at_start then 0 else 12 in
    check "index cached" (M.body (request p ~range:index_header "/z/a/c/0")
      = String.sub body index_offset 52);
    check "index read is local" (!calls = count);
    ignore (request p ~range:(range 4 7) "/z/a/c/0");
    ignore (request p ~range:(range 8 11) "/z/a/c/0");
    let count = !calls in
    check "whole shard assembled locally" (M.body (request p "/z/a/c/0") = body && !calls = count);
    let layout = get (P.Shard.of_json (Zarrz.Store.get_json store ~key:"a/zarr.json")) in
    let corrupt = Bytes.of_string (String.sub body index_offset 52) in
    Bytes.set corrupt 0 '\255';
    check "index checksum checked"
      (try ignore (P.Shard.ranges layout ~size:(String.length body) (Bytes.to_string corrupt)); false
       with Zarrz.Error.E _ -> true);
    Eio.Switch.run (fun sw ->
        let ready, resolver = Eio.Promise.create () in
        Eio.Fiber.fork_daemon ~sw (fun () ->
            Proffer_httpz.run ~sw ~port:0 env ~env:p P.site
              ~on_listening:(fun addr -> Eio.Promise.resolve resolver addr)
              ~on_error:(fun exn -> raise exn);
            `Stop_daemon);
        let port = match Eio.Promise.await ready with `Tcp (_, port) -> port | _ -> assert false in
        let wire = Fetch_httpz.v (Eio.Stdenv.net env) () in
        let url = Printf.sprintf "http://127.0.0.1:%d/z" port in
        let remote = Zarrz_fetch.store ~base_url:url wire in
        let array = Zarrz.Arr.open_ remote ~path:"/a" in
        let read = Zarrz.Arr.read array {Zarrz.Subset.start=[:2:]; shape=[:3:]} in
        let view = Bigarray.reshape_1 (Zarrz.Slab.to_genarray read Bigarray.int8_unsigned) 3 in
        check "real HTTP Zarr read" (Bigarray.Array1.get view 0 = 3 && Bigarray.Array1.get view 2 = 5);
        check "real HTTP Zarr read has zero upstream traffic" (!calls = count)))

let () =
  test_coverage ();
  test_manifest ();
  Eio_main.run (fun env -> test_cache env; test_shard ~at_start:false env; test_shard ~at_start:true env);
  print_endline "Range coverage, restart, revisions, shard assembly and HTTP Zarr reads passed."
