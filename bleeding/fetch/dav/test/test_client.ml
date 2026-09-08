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
  let backend = Fetch_mock.client (fun req ->
    seen := req :: !seen;
    Fetch.Middleware.Pi.response ~close:(fun () -> incr closed)
      ~status:!status ~headers:!headers ~version:`HTTP_1_1
      ~body:(Eio.Flow.string_source !body) ~url:req.url ()) in
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
  Printf.printf "fetch.dav: %d client checks passed\n" !count
