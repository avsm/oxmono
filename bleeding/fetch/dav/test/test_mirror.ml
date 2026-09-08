(* The collection mirror against a scripted server, over a real directory. *)
module D = Fetch_dav
let count = ref 0
let check name b = incr count; if not b then failwith name
let xml_headers = Http.Header.of_list ["Content-Type", "application/xml; charset=utf-8"]
let reports = "<d:multistatus xmlns:d='DAV:'><d:response><d:href>/dav/col/</d:href><d:propstat><d:prop><d:supported-report-set><d:supported-report><d:report><d:sync-collection/></d:report></d:supported-report></d:supported-report-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>"
let changed href etag = Printf.sprintf "<d:response><d:href>%s</d:href><d:propstat><d:prop><d:getetag>%s</d:getetag></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>" href etag
let removed href = Printf.sprintf "<d:response><d:href>%s</d:href><d:status>HTTP/1.1 404 Not Found</d:status></d:response>" href
let page ?(truncated = false) token responses =
  "<d:multistatus xmlns:d='DAV:'>" ^ String.concat "" responses ^
  (if truncated then "<d:response><d:href>/dav/col/</d:href><d:status>HTTP/1.1 507 Insufficient Storage</d:status></d:response>" else "") ^
  "<d:sync-token>" ^ token ^ "</d:sync-token></d:multistatus>"
let () = Eio_main.run @@ fun env ->
  let dir = Eio.Path.(Eio.Stdenv.cwd env / "mirror-test") in
  let files = Hashtbl.create 4 in
  Hashtbl.replace files "/dav/col/a.vcf" ("\"1\"", "card a");
  Hashtbl.replace files "/dav/col/b.vcf" ("\"1\"", "card b");
  let script = ref [] in
  let interrupt = ref false in
  let seen = ref [] in
  let sync_token = ref "" in
  let backend = Fetch_mock.client (fun req ->
    let path = Fetch.Middleware.Url.path_and_query req.url in
    seen := (Http.Method.to_string req.meth, path) :: !seen;
    let respond ?(status = 207) ?(headers = xml_headers) body =
      Fetch.Middleware.Pi.response ~close:(fun () -> ()) ~status ~headers ~version:`HTTP_1_1
        ~body:(if !interrupt && status = 200 then (
          let flow = Eio_mock.Flow.make "interrupted mirror download" in
          Eio_mock.Flow.on_read flow [`Return "partial"; `Raise (Failure "interrupted")];
          (flow :> Eio.Flow.source_ty Eio.Resource.t))
          else Eio.Flow.string_source body) ~url:req.url () in
    match Http.Method.to_string req.meth, path with
    | "PROPFIND", "/dav/col/" -> respond reports
    | "REPORT", "/dav/col/" ->
        (match req.body with Fetch.String b -> sync_token := b | _ -> ());
        (match !script with
        | (`Page body) :: rest -> script := rest; respond body
        | (`Refuse) :: rest -> script := rest;
            respond ~status:403 "<d:error xmlns:d='DAV:'><d:valid-sync-token/></d:error>"
        | [] -> respond (page "urn:sync:same" []))
    | "GET", p -> (match Hashtbl.find_opt files p with
        | Some (etag, body) -> respond ~status:200 ~headers:(Http.Header.of_list ["ETag", etag]) body
        | None -> respond ~status:404 "")
    | _ -> respond ~status:500 "unexpected") in
  let client = D.v ~root:"https://example.test/dav/" backend in
  let log = ref [] in
  let run () = log := []; D.Mirror.run ~log:(fun a -> log := a :: !log) client ~collection:"col/" ~dir in
  let read name = Eio.Path.load Eio.Path.(dir / name) in
  let logged p = List.exists p !log in
  script := [`Page (page "urn:sync:1" [changed "/dav/col/a.vcf" "\"1\""; changed "/dav/col/b.vcf" "\"1\""])];
  let s = run () in
  check "initial fetches both" (s.fetched = 2 && s.removed = 0 && s.token = Some "urn:sync:1");
  check "initial logged" (logged (fun a -> a = D.Mirror.Initial));
  check "files written" (read "a.vcf" = "card a" && read "b.vcf" = "card b");
  check "empty token sent" (String.length !sync_token > 0 && (Result.get_ok (Httpz_dav.parse_xml !sync_token) |> fun e -> Httpz_dav.content (Option.get (Httpz_dav.find (Httpz_dav.dav "sync-token") e)) = ""));
  script := [];
  let s = run () in
  check "routine with token" (String.length !sync_token > 0 && (Result.get_ok (Httpz_dav.parse_xml !sync_token) |> fun e -> Httpz_dav.content (Option.get (Httpz_dav.find (Httpz_dav.dav "sync-token") e)) = "urn:sync:1"));
  check "unchanged" (s.fetched = 0 && logged (fun a -> a = D.Mirror.Unchanged));
  Hashtbl.replace files "/dav/col/b.vcf" ("\"2\"", "card b2");
  Hashtbl.remove files "/dav/col/a.vcf";
  Hashtbl.replace files "/dav/col/c.vcf" ("\"1\"", "card c");
  script := [`Page (page ~truncated:true "urn:sync:2" [removed "/dav/col/a.vcf"; changed "/dav/col/b.vcf" "\"2\""]);
             `Page (page "urn:sync:3" [changed "/dav/col/c.vcf" "\"1\""])];
  let s = run () in
  check "two pages" (s.fetched = 2 && s.removed = 1 && s.token = Some "urn:sync:3");
  check "truncation logged" (logged (fun a -> a = D.Mirror.Truncated));
  check "removed file" (not (Eio.Path.is_file Eio.Path.(dir / "a.vcf")) && read "b.vcf" = "card b2" && read "c.vcf" = "card c");
  script := [`Page (page "urn:sync:4" [changed "/dav/col/c.vcf" "\"1\""])];
  let gets = List.length (List.filter (fun (m, _) -> m = "GET") !seen) in
  let s = run () in
  check "same etag skipped" (s.fetched = 0 && logged (function D.Mirror.Skipped _ -> true | _ -> false) &&
    List.length (List.filter (fun (m, _) -> m = "GET") !seen) = gets);
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dir / "stale.vcf") "stale";
  let index = Eio.Path.load Eio.Path.(dir / D.Mirror.index_file) in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / D.Mirror.index_file) (index ^ "/dav/col/stale.vcf\t\"9\"\tstale.vcf\n");
  script := [`Refuse; `Page (page "urn:sync:5" [changed "/dav/col/b.vcf" "\"2\""; changed "/dav/col/c.vcf" "\"1\""])];
  let s = run () in
  check "restart" (logged (function D.Mirror.Restart "valid-sync-token" -> true | _ -> false));
  check "rebuild keeps held members" (s.fetched = 0 && s.token = Some "urn:sync:5");
  check "pruned" (logged (fun a -> a = D.Mirror.Pruned "stale.vcf") && not (Eio.Path.is_file Eio.Path.(dir / "stale.vcf")));
  let rejects label f =
    check label (try ignore (f ()); false with
      D.Protocol_error _ | Invalid_argument _ -> true) in
  List.iter (fun href -> rejects ("unsafe filename " ^ href)
    (fun () -> D.Mirror.file_of_href href))
    ["/x/."; "/x/.."; "/x/.davsync"; "/x/.davsync.tmp-0"; "/x/a%5Cb"];
  let saved_index = read D.Mirror.index_file in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / D.Mirror.index_file)
    (saved_index ^ "/dav/col/escape\t\"1\"\t../sentinel\n");
  rejects "index traversal refused" run;
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / D.Mirror.index_file) saved_index;
  script := [`Page (page "urn:sync:6" [removed "https://foreign.test/dav/col/b.vcf"])];
  rejects "foreign deletion refused" run;
  check "foreign response leaves file" (read "b.vcf" = "card b2");
  script := [`Page (page "urn:sync:6" [changed "/dav/col/nested/b.vcf" "new"])];
  rejects "filename collision refused" run;
  check "collision leaves file" (read "b.vcf" = "card b2");
  script := [`Page (page ~truncated:true "urn:sync:5" [])];
  rejects "non-advancing token refused" run;
  Hashtbl.replace files "/dav/col/b.vcf" ("\"3\"", "card b3");
  script := [`Page (page "urn:sync:6" [changed "/dav/col/b.vcf" "new"])];
  interrupt := true;
  check "interrupted download raises" (try ignore (run ()); false with Failure _ -> true);
  interrupt := false;
  check "interrupted download preserves old content"
    (read "b.vcf" = "card b2" && read D.Mirror.index_file = saved_index);
  check "interrupted download cleans staging" (List.for_all (fun name ->
    not (String.starts_with ~prefix:".davsync.tmp-" name)) (Eio.Path.read_dir dir));
  script := [`Refuse;
    `Page (page ~truncated:true "urn:sync:6" [changed "/dav/col/b.vcf" "new"]);
    `Page (page "urn:sync:7" [changed "/dav/col/missing.vcf" "new"])];
  check "interrupted rebuild raises" (try ignore (run ()); false with D.Http_error _ -> true);
  check "rebuild does not commit partial token"
    (String.starts_with ~prefix:"token\t\n" (read D.Mirror.index_file));
  script := [`Page (page "urn:sync:8" [changed "/dav/col/b.vcf" "\"3\""])];
  let s = run () in
  check "rebuild after interruption prunes stale members"
    (s.token = Some "urn:sync:8" && not (Eio.Path.is_file Eio.Path.(dir / "c.vcf")));
  Eio.Path.rmtree dir;
  Printf.printf "fetch.dav: %d mirror checks passed\n" !count
