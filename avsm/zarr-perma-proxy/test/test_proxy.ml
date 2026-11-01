module P = Perma_proxy
module C = P.Cache
module M = Proffer_mock

let check name ok = if not ok then failwith name
let get = function Some x -> x | None -> failwith "Expected value"
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let request proxy ?(meth = Httpz.Method.Get) ?range ?(headers = []) path =
  M.request P.site proxy meth path ~on_error:raise
    ~headers:
      (headers @ Option.fold ~none:[] ~some:(fun r -> [ ("Range", r) ]) range)

let status r = Httpz.Res.status_code (M.status r)

let field r name =
  let result = ref None in
  Proffer.Headers.iter
    (fun _ key value ->
      if
        String.lowercase_ascii (Proffer.Req.globalize key)
        = String.lowercase_ascii name
      then result := Some (Proffer.Req.globalize value))
    (M.headers r);
  !result

let rec remove path =
  match Eio.Path.kind ~follow:false path with
  | `Directory ->
      List.iter
        (fun name -> remove Eio.Path.(path / name))
        (Eio.Path.read_dir path);
      Eio.Path.rmdir path
  | `Not_found -> ()
  | _ -> Eio.Path.unlink path

let with_dir env f =
  let tmp = Filename.temp_dir "perma-proxy-test-" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let limits =
  { C.default_limits with reserve_bytes = 0L; max_object_bytes = 1024 * 1024 }

let owned env dir ?client ?(limits = limits)
    ?(upstreams = [ "https://test.invalid" ]) f =
  C.with_open ~dir
    ~clock:(Eio.Stdenv.mono_clock env)
    ~wall_clock:(Eio.Stdenv.clock env)
    ~random:(Eio.Stdenv.secure_random env)
    ?client ~limits ~upstreams f

let proxy cache =
  P.create ~cache [ P.mapping ~prefix:"/z" ~upstream:"https://test.invalid" ]

let url s = "https://test.invalid/" ^ s

let expect_limit f =
  try
    f ();
    false
  with C.Limit _ -> true

let expect_upstream f =
  try
    f ();
    false
  with C.Upstream _ -> true

let scalar dir sql =
  Eio.Switch.run (fun sw ->
      let db = Sqlite3_eio.open_path ~sw Eio.Path.(dir / "index.sqlite3") in
      Sqlite3_eio.run db (fun db ->
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              check "sql row" (Sqlite3.step stmt = Sqlite3.Rc.ROW);
              Sqlite3.column_int stmt 0)))

let body_client calls body =
  Fetch_mock.client (fun req ->
      incr calls;
      check "only whole GET upstream"
        (req.Fetch.Middleware.meth = `GET
        && Http.Header.get req.headers "range" = None);
      Fetch_mock.respond
        ~headers:
          (Http.Header.of_list
             [
               ("content-length", string_of_int (String.length body));
               ("etag", "\"v1\"");
             ])
        body req)

let test_cache env =
  with_dir env (fun dir ->
      let calls = ref 0 in
      let client = body_client calls "abcdefghij" in
      owned env dir ~client (fun cache ->
          let p = proxy cache in
          let r = request p ~range:"bytes=2-4" "/z/a" in
          check "slice"
            (status r = 206
            && M.body r = "cde"
            && field r "X-Cache" = Some "MISS");
          check "full hit"
            (M.body (request p "/z/a") = "abcdefghij" && !calls = 1);
          check "suffix" (M.body (request p ~range:"bytes=-2" "/z/a") = "ij");
          check "open range" (M.body (request p ~range:"bytes=8-" "/z/a") = "ij");
          check "clipped range"
            (M.body (request p ~range:"bytes=8-99" "/z/a") = "ij");
          check "unsatisfiable"
            (status (request p ~range:"bytes=10-" "/z/a") = 416);
          check "multiple rejected"
            (status (request p ~range:"bytes=0-1,3-4" "/z/a") = 400);
          check "if range mismatch whole"
            (status
               (request p ~range:"bytes=0-1"
                  ~headers:[ ("If-Range", "\"other\"") ]
                  "/z/a")
            = 200);
          check "cold if range matches"
            (M.body
               (request p ~range:"bytes=0-1"
                  ~headers:[ ("If-Range", "\"v1\"") ]
                  "/z/b")
            = "ab");
          let head = request p ~meth:Httpz.Method.Head "/z/c" in
          check "cold HEAD caches complete"
            (M.body head = "" && M.content_length head = Some 10L);
          ignore (request p "/z/c");
          check "one fetch per URL" (!calls = 3);
          check "dedup" (scalar dir "SELECT count(*) FROM blobs" = 1);
          check "query rejected" (status (request p "/z/a?secret=x") = 400);
          check "traversal rejected"
            (status (request p "/z/%2e%2e/escape") = 400);
          check "maintenance not while serving"
            (try
               ignore (C.gc cache);
               false
             with Invalid_argument _ -> true);
          check "owner excludes other owner"
            (try
               owned env dir (fun _ -> ());
               false
             with Eioext.Path.Busy _ -> true);
          Eio.Switch.run (fun sw ->
              ignore (json (C.summary ~sw dir));
              ignore (json (P.Logger.summary ~sw dir))));
      owned env dir (fun cache ->
          check "restart hit"
            (M.body (request (proxy cache) "/z/a") = "abcdefghij"));
      owned env dir (fun cache ->
          C.forget cache ~url:(url "a");
          check "shared blob retained" ((C.gc cache).blobs = 0);
          C.forget cache ~url:(url "b");
          C.forget cache ~url:(url "c");
          let collected = C.gc cache in
          check "orphan collected" (collected.blobs = 1 && collected.bytes = 10L));
      check "rows removed" (scalar dir "SELECT count(*) FROM blobs" = 0))

let test_failures env =
  List.iter
    (fun (headers, body, code) ->
      with_dir env (fun dir ->
          let client =
            Fetch_mock.client (fun req ->
                Fetch_mock.respond ~status:code
                  ~headers:(Http.Header.of_list headers)
                  body req)
          in
          owned env dir ~client (fun cache ->
              check "invalid response rejected"
                (expect_upstream (fun () ->
                     ignore (C.get cache ~url:(url "bad"))));
              check "no committed partial"
                (scalar dir "SELECT count(*) FROM objects" = 0);
              check "temp cleaned"
                (Eio.Path.read_dir Eio.Path.(dir / "tmp") = []))))
    [
      ([ ("content-length", "10") ], "short", 200);
      ([ ("content-length", "2") ], "long", 200);
      ([ ("content-encoding", "gzip") ], "encoded", 200);
      ([ ("content-range", "bytes 0-1/10") ], "ab", 200);
      ([], "ab", 206);
    ];
  with_dir env (fun dir ->
      let calls = ref 0 in
      let client = body_client calls "12345" in
      owned env dir ~client ~limits:{ limits with max_object_bytes = 4 }
        (fun cache ->
          check "size limit"
            (expect_limit (fun () -> ignore (C.get cache ~url:(url "a")))));
      owned env dir ~client
        ~limits:{ limits with reserve_bytes = Int64.max_int } (fun cache ->
          check "reserve limit"
            (expect_limit (fun () -> ignore (C.get cache ~url:(url "a"))))));
  with_dir env (fun dir ->
      let calls = ref 0 in
      owned env dir ~client:(body_client calls "x")
        ~limits:{ limits with max_entries = 1 } (fun cache ->
          ignore (C.get cache ~url:(url "a"));
          check "URL cap"
            (expect_limit (fun () -> ignore (C.get cache ~url:(url "b"))))));
  with_dir env (fun dir ->
      let calls = ref 0 in
      let client =
        Fetch_mock.client (fun req ->
            incr calls;
            Fetch_mock.respond ~status:410 "" req)
      in
      owned env dir ~client (fun cache ->
          check "negative"
            (C.get cache ~url:(url "gone") = (C.Missing 410, C.Miss));
          check "negative hit"
            (C.get cache ~url:(url "gone") = (C.Missing 410, C.Hit)));
      owned env dir (fun cache ->
          check "negative restart"
            (C.get cache ~url:(url "gone") = (C.Missing 410, C.Hit)));
      check "negative fetched once" (!calls = 1));
  with_dir env (fun dir ->
      let calls = ref 0 in
      owned env dir ~client:(body_client calls "") (fun cache ->
          let p = proxy cache in
          check "empty whole" (status (request p "/z/empty") = 200);
          check "empty range"
            (status (request p ~range:"bytes=-1" "/z/empty") = 416)))

let test_concurrent env =
  with_dir env (fun dir ->
      let calls = ref 0 in
      let entered, signal = Eio.Promise.create ()
      and release, unblock = Eio.Promise.create () in
      let client =
        Fetch_mock.client (fun req ->
            incr calls;
            Eio.Promise.resolve signal ();
            Eio.Promise.await release;
            Fetch_mock.respond "abcdefgh" req)
      in
      owned env dir ~client (fun cache ->
          Eio.Fiber.both
            (fun () ->
              check "owner miss" (snd (C.get cache ~url:(url "same")) = C.Miss))
            (fun () ->
              Eio.Promise.await entered;
              let queued, active = C.activity cache in
              check "active transfer visible"
                (queued = 0 && List.length active = 1);
              check "active transfer has path" ((List.hd active).path = "/same");
              Eio.Fiber.both
                (fun () ->
                  check "follower shared"
                    (snd (C.get cache ~url:(url "same")) = C.Shared))
                (fun () ->
                  Eio.Fiber.yield ();
                  Eio.Promise.resolve unblock ()));
          check "single flight" (!calls = 1);
          check "transfer cleared" (C.activity cache = (0, []))))

let test_logger env =
  with_dir env (fun dir ->
      Eio.Switch.run (fun sw ->
          let log = P.Logger.open_ ~sw ~clock:(Eio.Stdenv.clock env) dir in
          P.Logger.request log ~meth:"GET" ~path:"/z/a?token=hidden"
            ~object_key:(Some "hash") ~range:true ~status:206
            ~cache:(Some "HIT") ~bytes:42 ~duration_us:100;
          P.Logger.request log ~meth:"HEAD" ~path:"/z/a" ~object_key:None
            ~range:false ~status:200 ~cache:(Some "HIT") ~bytes:0
            ~duration_us:20;
          P.Logger.download log ~object_key:"hash" ~status:(Some 200) ~bytes:100
            ~stored_bytes:100 ~duration_us:200 ~outcome:"success";
          P.Logger.heartbeat log ~session:"test" ~running:true ~connections:3
            ~queued:1
            ~transfers:
              [
                {
                  P.Logger.key = "hash";
                  path = "/z/a?secret=x";
                  bytes = 12;
                  total = Some 100;
                  age = 2.;
                };
              ];
          let live =
            P.Logger.snapshot ~sw ~clock:(Eio.Stdenv.clock env)
              ~mono_clock:(Eio.Stdenv.mono_clock env)
              dir
          in
          ignore (json live);
          check "runtime omits queries" (not (String.contains live '?'));
          let raw =
            P.Logger.export ~sw dir ~table:`Requests ~after:0L ~limit:1
          in
          check "no query in telemetry" (not (String.contains raw '?'));
          check "export first page"
            (match json raw with Jsont.Array ([ _ ], _) -> true | _ -> false);
          let next =
            P.Logger.export ~sw dir ~table:`Requests ~after:1L ~limit:10
          in
          check "export next page"
            (match json next with Jsont.Array ([ _ ], _) -> true | _ -> false);
          ignore (json (P.Logger.summary ~sw dir))))

let test_shard ~at_start env =
  with_dir env (fun dir ->
      let store = Zarrz.Store.memory () in
      let codec =
        Result.get_ok
          (Jsont.Json.decode Zarrz.Ext.jsont
             (json
                (Printf.sprintf
                   {|{"name":"sharding_indexed","configuration":{
          "chunk_shape":[4],"codecs":[{"name":"bytes"}],
          "index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},
                          {"name":"crc32c"}],"index_location":"%s"}}|}
                   (if at_start then "start" else "end"))))
      in
      let a =
        Zarrz.Arr.create ~shape:[| 12 |] ~chunk_shape:[| 12 |]
          ~dtype:Zarrz.Dtype.Uint8
          ~fill_value:
            (Result.get_ok
               (Zarrz.Fill_value.of_json Zarrz.Dtype.Uint8 (json "0")))
          ~codecs:[ codec ] store ~path:"/a"
      in
      let slab = Zarrz.Slab.create Zarrz.Dtype.Uint8 [: 12 :] in
      let view =
        Bigarray.reshape_1
          (Zarrz.Slab.to_genarray slab Bigarray.int8_unsigned)
          12
      in
      for i = 0 to 11 do
        Bigarray.Array1.set view i (i + 1)
      done;
      Zarrz.Arr.write a { Zarrz.Subset.start = [: 0 :]; shape = [: 12 :] } slab;
      let calls = ref 0 in
      let client =
        Fetch_mock.client (fun req ->
            incr calls;
            check "whole GET only"
              (req.Fetch.Middleware.meth = `GET
              && Http.Header.get req.headers "range" = None);
            let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
            let key =
              String.sub url
                (String.length "https://test.invalid/")
                (String.length url - String.length "https://test.invalid/")
            in
            match store.get ~key with
            | None -> Fetch_mock.respond ~status:404 "" req
            | Some b ->
                let body = Base_bigstring.to_string b in
                let size = String.length body in
                let selection =
                  match Http.Header.get req.headers "range" with
                  | None -> C.Whole
                  | Some s -> (
                      match Fetch.Header.decode Fetch.Header.range s with
                      | Some { ranges = [ `Range (a, Some b) ]; _ } ->
                          C.From
                            ( Int64.to_int a,
                              Some (Int64.to_int (Int64.sub b a) + 1) )
                      | Some { ranges = [ `Suffix n ]; _ } ->
                          C.Suffix (Int64.to_int n)
                      | _ -> failwith "Bad test range")
                in
                let r = C.resolve size selection in
                let headers =
                  [
                    ("etag", "\"shard\"");
                    ( "content-length",
                      string_of_int
                        (if req.meth = `HEAD then size else r.stop - r.start) );
                  ]
                  @
                  if selection = C.Whole then []
                  else
                    [
                      ( "content-range",
                        Printf.sprintf "bytes %d-%d/%d" r.start (r.stop - 1)
                          size );
                    ]
                in
                Fetch_mock.respond
                  ~headers:(Http.Header.of_list headers)
                  ~status:(if selection = C.Whole then 200 else 206)
                  (if req.meth = `HEAD then ""
                   else String.sub body r.start (r.stop - r.start))
                  req)
      in
      owned env dir ~client (fun cache ->
          let p =
            P.create ~cache
              [ P.mapping ~prefix:"/z" ~upstream:"https://test.invalid" ]
          in
          ignore (request p "/z/a/zarr.json");
          let body = Base_bigstring.to_string (get (store.get ~key:"a/c/0")) in
          let payload = if at_start then 52 else 0 in
          let range a b =
            Printf.sprintf "bytes=%d-%d" (payload + a) (payload + b)
          in
          let first = request p ~range:(range 1 1) "/z/a/c/0" in
          check "shard slice" (M.body first = String.sub body (payload + 1) 1);
          let count = !calls in
          check "inner chunk expanded"
            (M.body (request p ~range:(range 0 3) "/z/a/c/0")
            = String.sub body payload 4);
          check "rest of inner chunk is local" (!calls = count);
          let index_header = if at_start then "bytes=0-51" else "bytes=-52" in
          let index_offset = if at_start then 0 else 12 in
          check "index cached"
            (M.body (request p ~range:index_header "/z/a/c/0")
            = String.sub body index_offset 52);
          check "index read is local" (!calls = count);
          ignore (request p ~range:(range 4 7) "/z/a/c/0");
          ignore (request p ~range:(range 8 11) "/z/a/c/0");
          let count = !calls in
          check "whole shard assembled locally"
            (M.body (request p "/z/a/c/0") = body && !calls = count);
          Eio.Switch.run (fun sw ->
              let ready, resolver = Eio.Promise.create () in
              Eio.Fiber.fork_daemon ~sw (fun () ->
                  Proffer_httpz.run ~sw ~port:0 env ~env:p P.site
                    ~on_listening:(fun addr ->
                      Eio.Promise.resolve resolver addr)
                    ~on_error:(fun exn -> raise exn);
                  `Stop_daemon);
              let port =
                match Eio.Promise.await ready with
                | `Tcp (_, port) -> port
                | _ -> assert false
              in
              let wire = Fetch_httpz.v (Eio.Stdenv.net env) () in
              let url = Printf.sprintf "http://127.0.0.1:%d/z" port in
              let remote = Zarrz_fetch.store ~base_url:url wire in
              let array = Zarrz.Arr.open_ remote ~path:"/a" in
              let read =
                Zarrz.Arr.read array
                  { Zarrz.Subset.start = [: 2 :]; shape = [: 3 :] }
              in
              let view =
                Bigarray.reshape_1
                  (Zarrz.Slab.to_genarray read Bigarray.int8_unsigned)
                  3
              in
              check "real HTTP Zarr read"
                (Bigarray.Array1.get view 0 = 3
                && Bigarray.Array1.get view 2 = 5);
              check "real HTTP Zarr read has zero upstream traffic"
                (!calls = count))))

let execute dir sql =
  Eio.Switch.run (fun sw ->
      let db = Sqlite3_eio.open_path ~sw Eio.Path.(dir / "index.sqlite3") in
      Sqlite3_eio.run db (fun db -> Sqlite3.Rc.check (Sqlite3.exec db sql)))

let test_recovery env =
  with_dir env (fun dir ->
      let calls = ref 0 in
      owned env dir ~client:(body_client calls "data") (fun cache ->
          execute dir
            "CREATE TRIGGER fail_insert BEFORE INSERT ON objects BEGIN SELECT \
             RAISE(ABORT,'injected'); END";
          check "commit failure surfaced"
            (try
               ignore (C.get cache ~url:(url "a"));
               false
             with Sqlite3.SqliteError _ | Sqlite3.Error _ -> true);
          check "failed transaction invisible"
            (scalar dir "SELECT count(*) FROM objects" = 0);
          execute dir "DROP TRIGGER fail_insert");
      owned env dir (fun cache ->
          Eio.Path.save ~create:(`Exclusive 0o600)
            Eio.Path.(dir / "tmp" / (String.make 32 'a' ^ ".part"))
            "unfinished";
          let gc = C.gc cache in
          check "uncommitted blob and temp recovered"
            (gc.blobs = 1 && gc.bytes = 4L && gc.temporary = 1));
      owned env dir ~client:(body_client calls "data") (fun cache ->
          let entry =
            match fst (C.get cache ~url:(url "a")) with
            | C.Found e -> e
            | _ -> assert false
          in
          let hash = C.digest entry in
          Eio.Path.save ~create:(`Or_truncate 0o600)
            Eio.Path.(dir / "blobs" / String.sub hash 0 2 / hash)
            "bad";
          check "truncation detected"
            (expect_upstream (fun () -> ignore (C.get cache ~url:(url "a"))))))

let test_redirects env =
  List.iter
    (fun allowed ->
      with_dir env (fun dir ->
          let calls = ref 0 in
          let client =
            Fetch_mock.client (fun req ->
                incr calls;
                if !calls = 1 then
                  Fetch_mock.respond ~status:302
                    ~headers:
                      (Http.Header.of_list
                         [ ("location", "https://cdn.invalid/blob") ])
                    "" req
                else Fetch_mock.respond "blob" req)
          in
          let upstreams =
            if allowed then [ "https://test.invalid"; "https://cdn.invalid" ]
            else [ "https://test.invalid" ]
          in
          owned env dir ~client ~upstreams (fun cache ->
              let r = request (proxy cache) "/z/a" in
              check "redirect boundary"
                (if allowed then status r = 200 && !calls = 2
                 else status r = 502 && !calls = 1))))
    [ false; true ]

let test_parallel_limit env =
  with_dir env (fun dir ->
      let active = ref 0 and peak = ref 0 in
      let client =
        Fetch_mock.client (fun req ->
            incr active;
            peak := max !active !peak;
            Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.01;
            decr active;
            Fetch_mock.respond "bytes" req)
      in
      owned env dir ~client ~limits:{ limits with downloads = 2 } (fun cache ->
          Eio.Fiber.all
            (List.init 8 (fun n () ->
                 ignore (C.get cache ~url:(url (string_of_int n)))));
          check "download semaphore" (!peak = 2)))

let test_cancelled_owner env =
  with_dir env (fun dir ->
      let calls = ref 0 in
      let entered, signal = Eio.Promise.create () in
      let context, publish = Eio.Promise.create () in
      let client =
        Fetch_mock.client (fun req ->
            incr calls;
            if !calls = 1 then (
              Eio.Promise.resolve signal ();
              Eio.Fiber.await_cancel ());
            Fetch_mock.respond "retry" req)
      in
      owned env dir ~client (fun cache ->
          Eio.Fiber.both
            (fun () ->
              try
                Eio.Cancel.sub (fun cc ->
                    Eio.Promise.resolve publish cc;
                    ignore (C.get cache ~url:(url "a")))
              with Eio.Cancel.Cancelled _ -> ())
            (fun () ->
              Eio.Promise.await entered;
              Eio.Fiber.both
                (fun () -> ignore (C.get cache ~url:(url "a")))
                (fun () ->
                  Eio.Fiber.yield ();
                  Eio.Cancel.cancel
                    (Eio.Promise.await context)
                    (Failure "test cancellation")));
          check "cancelled owner retried" (!calls = 2);
          check "retry cached" (snd (C.get cache ~url:(url "a")) = C.Hit)))

let test_schema_and_legacy env =
  with_dir env (fun dir ->
      Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(dir / "old.json") "{}";
      check "legacy refused"
        (try
           owned env dir (fun _ -> ());
           false
         with Invalid_argument _ -> true));
  with_dir env (fun dir ->
      owned env dir (fun _ -> ());
      execute dir "PRAGMA user_version=999";
      check "unknown schema refused"
        (try
           owned env dir (fun _ -> ());
           false
         with Failure _ -> true))

let () =
  Eio_main.run (fun env ->
      test_recovery env;
      test_redirects env;
      test_parallel_limit env;
      test_cancelled_owner env;
      test_schema_and_legacy env;
      test_cache env;
      test_failures env;
      test_concurrent env;
      test_logger env;
      test_shard ~at_start:false env;
      test_shard ~at_start:true env);
  print_endline
    "Whole-object cache, ranges, restart, deduplication, limits, telemetry and \
     HTTP Zarr reads passed."
