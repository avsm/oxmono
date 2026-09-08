module D = Proffer_dav
module X = Httpz_dav
module P = Proffer
module Mock = Proffer_mock
let checks = ref 0
let check name ok = incr checks; if not ok then failwith name
let code r = P.Status.code (Mock.status r)
let invalid name f =
  check name (try ignore (f ()); false with Invalid_argument _ -> true)
let meth name =
  let open Httpz.Method in
  List.find (fun m -> to_string m = name)
    [Get; Head; Options; Put; Delete; Mkcol; Copy; Move; Propfind; Proppatch;
     Lock; Unlock; Report]
let file = {D.kind=D.File; length=5L; etag=Some "\"hello\"";
  modified=1_700_000_000.; properties=[]}
let collection = {file with D.kind=D.Collection; length=0L; etag=None}
let reads = ref 0
let read path (fn @ local) =
  if D.Path.segments path <> ["hello"] then raise (D.Error (404, []));
  incr reads;
  let () = fn file (fun sink -> P.Body.Sink.write sink "hello") in ()
let reader = D.Reader.v
  ~stat:(fun path -> match D.Path.segments path with
    | [] -> Some collection | ["hello"] -> Some file | _ -> None)
  ~list:(fun _ -> ["hello", file]) ~read
let security = D.Security.authenticated ~realm:"tests"
  ~authenticate:(function "Basic good" -> Some "alice" | _ -> None)
  ~authorize:(function "alice" -> Some D.Read_write | _ -> None)
let export = D.read_only ~origin:"https://example.test" ~at:["dav"]
  ~security reader
let site = P.Site.of_routes [] |> D.mount ~at:["dav"] (fun export -> export)
let request ?(headers=[]) ?(body="") meth target =
  Mock.request ~transport:P.Req.Secure site export (List.find (fun m -> P.Method.to_string m = meth)
    [Httpz.Method.Get; Head; Options; Put; Delete; Mkcol; Copy; Move;
     Propfind; Proppatch; Lock; Unlock; Report]) target ~body
    ~headers:(["Host", "example.test"; "Authorization", "Basic good"] @ headers)
let xml = ["Content-Type", "application/xml"]
let () =
  let plain = Mock.request (P.Site.of_routes []) ()
    (meth "PROPFIND") "/dav/" in
  check "ordinary Proffer has no DAV endpoint" (code plain = 404);
  let rejected = Mock.request ~transport:P.Req.Secure site export (meth "PUT")
    "/dav/hello" ~headers:["Host", "example.test"] in
  check "authentication precedes method denial" (code rejected = 401);
  check "auth prevents file open" (!reads = 0);
  let insecure = Mock.request site export (meth "GET") "/dav/hello"
    ~headers:["Host", "example.test"; "Authorization", "Basic good";
      "X-Forwarded-Proto", "https"] in
  check "HTTPS origin needs backend TLS provenance" (code insecure = 403);
  let get = request "GET" "/dav/hello" in
  check "read bytes" (code get = 200 && Mock.body get = "hello");
  let head = request "HEAD" "/dav/hello" in
  check "HEAD framing" (Mock.body head = "" && Mock.content_length head = Some 5L);
  List.iter (fun meth -> check ("read-only denies " ^ meth)
    (code (request ~body:"hostile" meth "/dav/hello") = 403))
    ["PUT"; "MKCOL"; "DELETE"; "COPY"; "MOVE"; "PROPPATCH"; "LOCK"; "UNLOCK"];
  check "unknown method" (code (request "REPORT" "/dav/hello") = 405);
  let options = request "OPTIONS" "/dav/" in
  check "read-only Allow excludes mutations"
    (Mock.header options Httpz.Header_name.Dav = Some "1" &&
     Mock.header options Httpz.Header_name.Allow = Some "OPTIONS, GET, HEAD, PROPFIND");
  List.iter (fun target -> check ("unsafe path " ^ target)
    (code (request "GET" target) = 400))
    ["/dav/%2e%2e/escape"; "/dav/a%2fb"; "/dav/a%5cb";
     "/dav//hello"; "/dav/%00"; "/dav/%ff"; "/dav/hello?x"];
  check "duplicate auth rejected" (code (request
    ~headers:["Authorization", "Basic good"] "GET" "/dav/hello") = 401);
  check "duplicate host rejected" (code (request
    ~headers:["Host", "example.test"] "GET" "/dav/hello") = 400);
  check "weak If-None-Match on GET" (code (request
    ~headers:["If-None-Match", "W/\"hello\""] "GET" "/dav/hello") = 304);
  check "strong If-Match required" (code (request
    ~headers:["If-Match", "W/\"hello\""] "GET" "/dav/hello") = 412);
  check "unsupported infinity is finite-depth error"
    (code (request "PROPFIND" "/dav/") = 403);
  let propfind = request ~headers:["Depth", "1"] "PROPFIND" "/dav/" in
  check "depth one multistatus" (code propfind = 207);
  let parsed = Result.bind (X.parse_xml (Mock.body propfind)) X.multistatus in
  check "listing includes self and children" (match parsed with
    | Ok m -> List.length m.responses = 2 | _ -> false);
  check "malformed XML" (code (request ~headers:(("Depth", "0") :: xml)
    ~body:"<propfind>" "PROPFIND" "/dav/") = 400);
  let wrapped = site
    |> P.Site.with_auth ~scope:[["dav"]] ~realm:"outer"
      ~check:(fun _ -> false) in
  let r = Mock.request ~transport:P.Req.Secure wrapped export (meth "GET") "/dav/hello"
    ~headers:["Host", "example.test"; "Authorization", "Basic good"] in
  check "outer auth wraps admission" (code r = 401);
  let wrapped = P.Site.with_headers ["X-Outer", "yes"] site in
  let r = Mock.request ~transport:P.Req.Secure wrapped export (meth "GET") "/dav/hello"
    ~headers:["Host", "example.test"; "Authorization", "Basic good"] in
  check "outer headers wrap accepted response"
    (Mock.header_other r "X-Outer" = Some "yes");
  invalid "overlapping mounts" (fun () -> D.mount ~at:["dav"; "child"]
    (fun export -> export) site);
  invalid "HTTP is explicit" (fun () -> D.read_only
    ~origin:"http://example.test" ~at:[] ~security reader);
  invalid "insecure exception only loopback" (fun () -> D.read_only
    ~allow_insecure_loopback:true ~origin:"http://example.test" ~at:[]
    ~security reader);
  List.iter (fun segments -> invalid "invalid structured path" (fun () ->
    D.Path.of_segments segments)) [[".."]; ["a/b"]; ["a\\b"]; [""]];
  Printf.printf "%d Proffer DAV policy checks passed\n" !checks
