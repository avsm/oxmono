module D = Fetch_dav
let count = ref 0
let check name b = incr count; if not b then failwith name
let invalid name f = check name (try ignore (f ()); false with Invalid_argument _ -> true)
let protocol name f = check name (try ignore (f ()); false with D.Protocol_error _ -> true)
let expect_http status f =
  match f () with
  | _ -> failwith "expected HTTP rejection"
  | exception D.Http_error e -> check "HTTP rejection status" (e.status = status); e
let xml_headers = Http.Header.of_list ["Content-Type", "application/xml; charset=utf-8"]
let multi = "<d:multistatus xmlns:d='DAV:'><d:response><d:href>/dav/a</d:href><d:status>HTTP/1.1 423 Locked</d:status></d:response></d:multistatus>"
let () = Eio_mock.Backend.run @@ fun () ->
  let seen = ref [] and closed = ref 0 in
  let status = ref 207 and body = ref multi and headers = ref xml_headers in
  let body_failure = ref None in
  let backend = Fetch_mock.client (fun req ->
    seen := req :: !seen;
    Fetch.Middleware.Pi.response ~close:(fun () -> incr closed)
      ~status:!status ~headers:!headers ~version:`HTTP_1_1
      ~body:(match !body_failure with
        | None -> Eio.Flow.string_source !body
        | Some exn ->
            let flow = Eio_mock.Flow.make "DAV response body failure" in
            Eio_mock.Flow.on_read flow [`Raise exn];
            (flow :> Eio.Flow.source_ty Eio.Resource.t)) ~url:req.url ()) in
  let client = D.v ~root:"https://example.test/dav/" backend in
  let response = D.propfind client "a" (Httpz_dav.Prop [Httpz_dav.dav "getetag"]) in
  check "207 failure preserved" ((List.hd response.responses).outcome = Httpz_dav.Status 423);
  check "response closed" (!closed = 1);
  let req = List.hd !seen in
  check "PROPFIND extension method" (Http.Method.to_string req.meth = "PROPFIND");
  check "explicit depth zero" (Http.Header.get req.headers "depth" = Some "0");
  check "XML body" (match req.body with Fetch.String s -> Result.is_ok (Httpz_dav.parse_xml s) | _ -> false);
  let before = List.length !seen in
  List.iter (fun target -> invalid "scope check before network" (fun () -> D.mkcol client target))
    ["../outside"; "https://evil.test/dav/a"; "/davx/a"; "/dav/%2e%2e/outside"; "/dav/a%2fb"; "a#f"];
  invalid "COPY destination check" (fun () -> D.copy client ~src:"a" ~dst:"https://evil.test/dav/a" ());
  invalid "weak If-Match" (fun () -> D.put ~condition:(D.If_match {weak=true; tag="x"}) client "a" Fetch.Empty);
  check "no forbidden requests sent" (List.length !seen = before);
  check "child encoding" (D.child client ~collection:"" "space % café.txt" =
    "https://example.test/dav/space%20%25%20caf%C3%A9.txt");
  List.iter (fun n -> invalid "bad child" (fun () -> D.child client ~collection:"" n)) [""; "."; ".."; "a/b"; "a\\b"];
  invalid "root collection slash" (fun () -> D.v ~root:"https://example.test/dav" backend);
  invalid "root query" (fun () -> D.v ~root:"https://example.test/dav/?q" backend);
  status := 302; body := "redirect";
  headers := Http.Header.of_list ["Location", "/dav/elsewhere"];
  ignore (expect_http 302 (fun () -> D.propfind client "a" Httpz_dav.Propname));
  check "redirect stopped" (List.length !seen = before + 1);
  status := 201; body := ""; headers := Http.Header.init ();
  ignore (D.copy client ~src:"a" ~dst:"b" ());
  let req = List.hd !seen in
  check "COPY no overwrite" (Http.Header.get req.headers "overwrite" = Some "F");
  check "absolute destination" (Http.Header.get req.headers "destination" = Some "https://example.test/dav/b");
  check "empty DAV framed body" (match req.body with Fetch.String "" -> true | _ -> false);
  ignore (D.put ~condition:D.If_absent client "new" (Fetch.String "data"));
  check "create precondition" (Http.Header.get (List.hd !seen).headers "if-none-match" = Some "*");
  status := 207; body := multi; headers := xml_headers;
  check "recursive failure preserved" (match D.delete client "a" with D.Multi m ->
    (List.hd m.responses).outcome = Httpz_dav.Status 423 | _ -> false);
  body := "<multistatus xmlns='DAV:' xmlns:p='urn:p'><response><href>/dav/a</href><propstat><prop><p:x/><p:x/></prop><status>HTTP/1.1 200 OK</status></propstat></response></multistatus>";
  let result = D.proppatch client "a" [Httpz_dav.Set [Httpz_dav.element ("urn:p", "x") [Httpz_dav.Text "first"]];
    Httpz_dav.Remove ["urn:p", "x"]] in
  check "repeated PROPPATCH reports succeed" (match Httpz_dav.property_results ("urn:p", "x") (List.hd result.responses) with
    | [Ok _; Ok _] -> true | _ -> false);
  body := "<d:multistatus xmlns:d='DAV:'>";
  let n = !closed in
  protocol "truncated XML" (fun () -> D.propfind client "" Httpz_dav.Propname);
  check "decode failure closes response" (!closed = n + 1);
  body := multi;
  let small = D.v ~limits:{Httpz_dav.default_limits with max_bytes=16} ~root:(D.root client) backend in
  protocol "XML size bound" (fun () -> D.propfind small "a" Httpz_dav.Propname);
  status := 403; headers := Http.Header.of_list ["Content-Type", "text/html"]; body := String.make 50 'x';
  let e = expect_http 403 (fun () -> D.mkcol small "a") in
  check "bounded non-XML error" (e.truncated && String.length e.body = 16 && e.dav_errors = []);
  headers := xml_headers; body := "<error xmlns='DAV:'><lock-token-submitted/></error>";
  let e = expect_http 403 (fun () -> D.mkcol client "a") in
  check "DAV error retained" ((List.hd e.dav_errors).name = Httpz_dav.dav "lock-token-submitted");
  status := 200; body := "file bytes"; headers := Http.Header.init ();
  let n = !closed in
  (try D.with_download client "a" (fun _ -> raise Exit) with Exit -> ());
  check "download callback failure closes" (!closed = n+1);
  body := "";
  headers := Http.Header.of_list ["DAV", "1, 2"; "DAV", "<urn:extension:with,comma>";
    "Allow", "GET, PROPFIND"; "Allow", "LOCK"];
  let caps = D.options client "" in
  check "repeated DAV capability fields" (caps.dav = ["1"; "2"; "<urn:extension:with,comma>"]);
  check "repeated Allow fields" (caps.allow = ["GET"; "PROPFIND"; "LOCK"]);
  check "repeated singleton refused" (Fetch.Header.get D.Header.lock_token
    (Http.Header.of_list ["Lock-Token", "<urn:a>"; "Lock-Token", "<urn:b>"]) = None);
  let token = match Httpz_dav.Token.of_string "urn:lease" with Ok t -> t | Error e -> failwith e in
  let lock_body = "<prop xmlns='DAV:'><lockdiscovery><activelock><lockscope><exclusive/></lockscope><locktype><write/></locktype><depth>0</depth><timeout>Second-5</timeout><locktoken><href>urn:lease</href></locktoken><lockroot><href>/dav/a</href></lockroot></activelock></lockdiscovery></prop>" in
  body := lock_body;
  headers := Http.Header.add xml_headers "Lock-Token" "<urn:lease>";
  let lease = D.lock client "a" in
  check "granted timeout retained" (lease.granted.timeout = Some (Httpz_dav.Seconds 5L));
  check "opaque token retained" (lease.token = token);
  status := 204; body := "";
  List.iter (fun meth ->
    ignore ((if meth = "COPY" then D.copy ~depth:`Infinity ~overwrite:true else D.move ~overwrite:true)
      ~if_:(D.lock_condition lease) client ~src:"source" ~dst:"a" ());
    let req = List.hd !seen in
    check "destination condition names lease URL"
      (Http.Header.get req.headers "if" = Some "<https://example.test/dav/a> (<urn:lease>)")
  ) ["COPY"; "MOVE"];
  status := 200; body := lock_body;
  headers := xml_headers;
  ignore (D.refresh_lock client lease);
  let req = List.hd !seen in
  check "refresh empty body" (match req.body with Fetch.String "" -> true | _ -> false);
  check "refresh uses If" (Http.Header.get req.headers "if" = Some "(<urn:lease>)" &&
    Http.Header.get req.headers "lock-token" = None);
  status := 204; body := "";
  D.unlock client lease;
  check "unlock uses Lock-Token" (Http.Header.get (List.hd !seen).headers "lock-token" = Some "<urn:lease>");
  status := 207;
  let ascii = "<multistatus xmlns='DAV:'/>" in
  body := "\255\254" ^ String.init (String.length ascii * 2) (fun i -> if i mod 2 = 0 then ascii.[i/2] else '\000');
  headers := xml_headers;
  check "BOM precedence over charset" ((D.propfind client "" Httpz_dav.Propname).responses = []);
  headers := Http.Header.of_list ["Content-Type", "application/xml; charset=unsupported"];
  check "BOM overrides unsupported charset" ((D.propfind client "" Httpz_dav.Propname).responses = []);
  body := ascii;
  protocol "unsupported charset" (fun () -> D.propfind client "" Httpz_dav.Propname);
  status := 207; headers := xml_headers;
  body := "<d:multistatus xmlns:d='DAV:'><d:response><d:href>/dav/a</d:href><d:propstat><d:prop><d:getetag>\"1\"</d:getetag></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response><d:sync-token>urn:sync:2</d:sync-token></d:multistatus>";
  let page = D.sync ~token:"urn:sync:1" client "" in
  let req = List.hd !seen in
  check "sync is a REPORT at depth zero" (Http.Method.to_string req.meth = "REPORT" && Http.Header.get req.headers "depth" = Some "0");
  check "sync token and change" (page.token = Some "urn:sync:2" &&
    (match page.changes with [Httpz_dav.Sync.Changed r] -> Httpz_dav.etag r = Some "\"1\"" | _ -> false));
  status := 201; body := ""; headers := Http.Header.init ();
  D.mkcol ~props:[Httpz_dav.leaf Httpz_dav.Prop.displayname "Book"] client "book/";
  let req = List.hd !seen in
  check "extended MKCOL body" (match req.body with
    | Fetch.String s -> (Result.get_ok (Httpz_dav.parse_xml s)).name = Httpz_dav.dav "mkcol" | _ -> false);
  D.mkcalendar ~props:[Httpz_dav.leaf Httpz_dav.Prop.displayname "Cal"] client "cal/";
  let req = List.hd !seen in
  check "MKCALENDAR body" (Http.Method.to_string req.meth = "MKCALENDAR" && match req.body with
    | Fetch.String s -> (Result.get_ok (Httpz_dav.parse_xml s)).name = ("urn:ietf:params:xml:ns:caldav", "mkcalendar") | _ -> false);
  headers := Http.Header.of_list ["ETag", "\"put\""];
  check "PUT returns the entity tag" ((D.put client "a" (Fetch.String "x")).etag = Some "\"put\"");
  status := 200; body := "BEGIN:VCARD"; headers := Http.Header.of_list ["ETag", "\"get\""];
  check "GET body and tag" (D.get client "a" = ("BEGIN:VCARD", Some "\"get\""));
  body := "FREEBUSY";
  check "report body" (D.report_body client "" "<x/>" = "FREEBUSY");
  let origin = D.v ~root:"https://example.test/" backend in
  invalid "well-known outside the root" (fun () -> D.context_path client `Carddav);
  status := 301; body := ""; headers := Http.Header.of_list ["Location", "/dav/ctx/"];
  check "context path follows the well-known redirect" (D.context_path origin `Carddav = "https://example.test/dav/ctx/");
  status := 207; headers := xml_headers;
  body := "<d:multistatus xmlns:d='DAV:'><d:response><d:href>/dav/ctx/</d:href><d:propstat><d:prop><d:current-user-principal><d:href>/dav/p/</d:href></d:current-user-principal></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>";
  check "principal" (D.principal client "ctx/" = "https://example.test/dav/p/");
  body := "<d:multistatus xmlns:d='DAV:' xmlns:c='urn:ietf:params:xml:ns:carddav'><d:response><d:href>/dav/p/</d:href><d:propstat><d:prop><c:addressbook-home-set><d:href>/dav/books/</d:href></c:addressbook-home-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>";
  check "home set" (D.home_set client ("urn:ietf:params:xml:ns:carddav", "addressbook-home-set") "p/" = ["https://example.test/dav/books/"]);
  body := "<d:multistatus xmlns:d='DAV:'><d:response><d:href>/dav/ctx/</d:href><d:propstat><d:prop><d:current-user-principal><d:unauthenticated/></d:current-user-principal></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>";
  protocol "unauthenticated principal" (fun () -> D.principal client "ctx/");
  status := 204; body := ""; headers := Http.Header.init ();
  let narrow = Fetch.restrict ~under:["https://example.test/dav/inner/"] backend in
  let wide = D.v ~root:"https://example.test/dav/" narrow in
  let denied name f = check name (try ignore (f ()); false with
    | Eio.Io (Fetch.E (Fetch.Denied _), _) -> true) in
  let before = List.length !seen in
  List.iter (fun operation -> denied "inner scope constrains Destination"
    (fun () -> operation wide ~src:"inner/a" ~dst:"outside" ()))
    [(fun t ~src ~dst () -> D.copy t ~src ~dst ());
     (fun t ~src ~dst () -> D.move t ~src ~dst ())];
  List.iter (fun meth ->
    List.iter (fun hs -> denied "raw Destination policy before transport"
      (fun () -> Fetch.with_response narrow (Http.Method.of_string meth)
        "https://example.test/dav/inner/a" ~headers:hs ignore))
      [Fetch.Header.[];
       Fetch.Header.[raw "Destination" "/dav/outside"];
       Fetch.Header.[raw "Destination" "/dav/inner/a";
                     raw "Destination" "/dav/inner/b"]]) ["COPY"; "MOVE"];
  let readonly = D.read_only client in
  denied "DAV-specific read-only rejects PUT"
    (fun () -> D.put readonly "a" (Fetch.String "forbidden"));
  check "narrowing failures have no transport effects" (List.length !seen = before);
  ignore (D.copy wide ~src:"inner/a" ~dst:"inner/b" ());
  status := 207; headers := xml_headers; body := "<multistatus xmlns='DAV:'/>";
  ignore (D.propfind readonly "" Httpz_dav.Propname);
  body := "<multistatus xmlns='DAV:'><response><href>/dav/unrelated</href><propstat><prop><current-user-principal><href>/dav/p/</href></current-user-principal></prop><status>HTTP/1.1 200 OK</status></propstat></response></multistatus>";
  protocol "discovery never selects unrelated response"
    (fun () -> D.principal client "ctx/");
  status := 301; body := "";
  headers := Http.Header.of_list ["Location", "relative/"];
  check "discovery resolves against request URL"
    (D.context_path origin `Carddav = "https://example.test/.well-known/relative/");
  status := 401; headers := Http.Header.init ();
  ignore (expect_http 401 (fun () -> D.context_path origin `Carddav));
  status := 404;
  check "only explicit absence falls back to root"
    (D.context_path origin `Carddav = D.root origin);
  Eio.Switch.run (fun sw ->
    let home = Httpz_dav.carddav "addressbook-home-set" in
    let connect url = D.Session.connect ~sw ~service:`Carddav ~home_set:home backend url in
    status := 401; headers := Http.Header.init (); body := "";
    check "session reports a refused principal"
      (match connect "https://example.test/dav/p/" with Error (D.Session.Http (401, _)) -> true | _ -> false);
    status := 207; headers := xml_headers;
    body := "<d:multistatus xmlns:d='DAV:' xmlns:c='urn:ietf:params:xml:ns:carddav'><d:response><d:href>/dav/p/</d:href><d:propstat><d:prop><d:current-user-principal><d:href>/dav/p/</d:href></d:current-user-principal><c:addressbook-home-set><d:href>/dav/books/</d:href></c:addressbook-home-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>";
    let session = match connect "https://example.test/dav/p/" with
      | Ok s -> s | Error e -> failwith (D.Session.error_to_string e) in
    check "session principal" (D.Session.principal session = "https://example.test/dav/p/");
    check "session home set" (D.Session.home_sets session = ["https://example.test/dav/books/"]);
    check "session resolves against the principal"
      (D.Session.resolve session "/dav/p/x.vcf" = "https://example.test/dav/p/x.vcf");
    status := 404; headers := Http.Header.init (); body := "";
    check "session names the missing resource"
      (D.Session.get session "/dav/books/x.vcf" = Error (D.Session.Not_found "https://example.test/dav/books/x.vcf"));
    check "session refuses a malformed entity tag"
      (match D.Session.put session ~etag:"no quotes" ~content_type:"text/vcard" "/dav/books/x.vcf" "" with
       | Error (D.Session.Data _) -> true | _ -> false);
    let closures = !closed in
    check "rejected session download reports the resource"
      (D.Session.download session "/dav/books/x.vcf" =
        Error (D.Session.Not_found "https://example.test/dav/books/x.vcf"));
    check "rejected session download closes before switch exit"
      (!closed = closures + 1);
    let closures = !closed in
    body_failure := Some Exit;
    check "session error-body exception propagates"
      (try ignore (D.Session.download session "/dav/books/x.vcf"); false
       with Exit -> true);
    body_failure := None;
    check "failed session error body closes before switch exit"
      (!closed = closures + 1);
    let closures = !closed in
    body_failure := Some (Eio.Cancel.Cancelled Exit);
    check "session cancellation propagates"
      (try ignore (D.Session.download session "/dav/books/x.vcf"); false
       with Eio.Cancel.Cancelled Exit -> true);
    body_failure := None;
    check "cancelled session error body closes before switch exit"
      (!closed = closures + 1);
    let headers = Fetch.Header.[
      raw "If" "(<urn:session-secret>)";
      raw "Lock-Token" "<urn:session-secret>"] in
    List.iter (fun code ->
      status := code; body := "streamed";
      let n = List.length !seen and closures = !closed in
      match D.Session.download session ~headers "/dav/books/x.vcf" with
      | Ok r ->
          check "session download stays open"
            (Fetch.status r = code && List.length !seen = n + 1 &&
             !closed = closures);
          let printed = Format.asprintf "%a" Fetch.Middleware.pp_request
            (List.hd !seen) in
          let contains needle =
            let rec loop i =
              i + String.length needle <= String.length printed &&
              (String.sub printed i (String.length needle) = needle ||
               loop (i + 1)) in
            loop 0 in
          check "session download redacts DAV tokens"
            (not (contains "urn:session-secret") && contains "<redacted>");
          Fetch.close r;
          check "caller closes session download" (!closed = closures + 1)
      | Error e -> failwith (D.Session.error_to_string e)) [200; 206; 304];
    check "member names keep a safe uid" (D.Session.member_name (Some "a-b@c") ".vcf" = "a-b@c.vcf");
    check "member names replace an unsafe uid"
      (String.length (D.Session.member_name (Some "a/b") ".vcf") = 20);
    let codec = {
      D.Objects.content_type = "text/plain; charset=utf-8";
      decode = (fun s -> if s = "bad" then Error "undecodable" else Ok (String.uppercase_ascii s));
      encode = (fun v -> if v = "BAD" then Error "unencodable" else Ok (String.lowercase_ascii v)) } in
    let data (r : Httpz_dav.response) =
      Option.map Httpz_dav.content (Httpz_dav.find_property (Httpz_dav.dav "x") r) in
    let page body =
      match Httpz_dav.parse_xml body with
      | Error m -> failwith m
      | Ok x -> (match Httpz_dav.multistatus x with
        | Error m -> failwith m
        | Ok m -> D.Objects.page_of_multistatus codec ~data ~base:"https://example.test/dav/books/" m) in
    let entry href value =
      Printf.sprintf "<d:response><d:href>%s</d:href><d:propstat><d:prop><d:getetag>\"e\"</d:getetag><d:x>%s</d:x></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>" href value in
    let wrap inner = "<d:multistatus xmlns:d='DAV:'>" ^ inner ^ "</d:multistatus>" in
    (match page (wrap (entry "/dav/books/a.txt" "hello")) with
     | Ok p ->
       check "objects decode a member" (List.map (fun (e : _ D.Objects.entry) -> e.value) p.entries = ["HELLO"]);
       check "objects resolve the href"
         (List.map (fun (e : _ D.Objects.entry) -> e.href) p.entries = ["https://example.test/dav/books/a.txt"]);
       check "objects keep the entity tag"
         (List.map (fun (e : _ D.Objects.entry) -> e.etag) p.entries = [Some "\"e\""]);
       check "objects report an untruncated page" (not p.truncated)
     | Error e -> failwith (D.Session.error_to_string e));
    check "objects skip the collection itself and a member with no body"
      (match page (wrap (entry "/dav/books/" "self" ^ "<d:response><d:href>/dav/books/b.txt</d:href><d:propstat><d:prop><d:getetag>\"f\"</d:getetag></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>")) with
       | Ok p -> p.entries = [] | Error _ -> false);
    check "objects skip a failed member"
      (match page (wrap "<d:response><d:href>/dav/books/c.txt</d:href><d:status>HTTP/1.1 404 Not Found</d:status></d:response>") with
       | Ok p -> p.entries = [] | Error _ -> false);
    check "objects report the collection's 507 as truncation"
      (match page (wrap ("<d:response><d:href>/dav/books/</d:href><d:status>HTTP/1.1 507 Insufficient Storage</d:status></d:response>" ^ entry "/dav/books/a.txt" "hi")) with
       | Ok p -> p.truncated && List.length p.entries = 1 | Error _ -> false);
    check "objects report an undecodable body as data"
      (match page (wrap (entry "/dav/books/a.txt" "bad")) with
       | Error (D.Session.Data m) -> m = "undecodable" | _ -> false);
    check "objects refuse a name that is not one segment"
      (match D.Objects.add session codec ~name:"a/b" ~uid:(fun _ -> None) ~ext:".txt" "/dav/books/" "x" with
       | Error (D.Session.Data _) -> true | _ -> false);
    check "objects report an unencodable value as data"
      (match D.Objects.put session codec "/dav/books/a.txt" "BAD" with
       | Error (D.Session.Data m) -> m = "unencodable" | _ -> false));
  Printf.printf "fetch.dav: %d client checks passed\n" !count
