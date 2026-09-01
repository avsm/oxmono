open Proffer
open Proffer.Route
module H = Httpz.Header_name
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  check "absent accept is empty" (Negotiate.of_accept None = []);
  check "html preferred"
    (Negotiate.of_accept (Some "text/html,text/markdown;q=0.9")
    = [ `Html; `Markdown ]);
  check "q-value orders"
    (Negotiate.of_accept (Some "text/markdown;q=0.4, text/html;q=0.9")
    = [ `Html; `Markdown ]);
  check "zero quality is dropped"
    (Negotiate.of_accept (Some "application/json;q=0") = []);
  check "zero quality is dropped from a list"
    (Negotiate.of_accept (Some "application/json;q=0, text/html;q=0.5")
    = [ `Html ]);
  check "q=0.000 is still zero"
    (Negotiate.of_accept (Some "application/json;q=0.000") = []);
  check "q=1 is kept"
    (Negotiate.of_accept (Some "application/json;q=1") = [ `Json ]);
  check "q=1.000 is kept"
    (Negotiate.of_accept (Some "application/json;q=1.000") = [ `Json ]);
  check "the q parameter is case-insensitive"
    (Negotiate.of_accept (Some "application/json;Q=0") = []);
  check "a semicolon in a quoted extension is not a q parameter"
    (Negotiate.of_accept (Some "text/html;foo=\";q=0\"") = [ `Html ]);
  check "a comma in a quoted extension does not split the range"
    (Negotiate.of_accept (Some "text/html;foo=\",application/json\"") = [ `Html ]);
  List.iter
    (fun value ->
      check ("a malformed media range is dropped: " ^ value)
        (Negotiate.of_accept (Some value) = []))
    [ "*/html"; "text/foo*bar"; "text/html;broken"; "text html";
      "text/html;foo=\"unterminated"; "text/html;foo=";
      "text/html;foo=\"\\\r\""; "text/html;foo=\"\\\127\"" ];
  check "quoted-pair permits an escaped tab"
    (Negotiate.of_accept (Some "text/html;foo=\"\\\t\"") = [ `Html ])

(* RFC 9110 section 12.4.2 fixes the qvalue spelling. Anything else would let
   a member outrank, or sort below, everything the client actually asked for,
   so the member is unusable and dropped. *)
let () =
  let bad q =
    Negotiate.of_accept (Some ("application/json;q=" ^ q ^ ", text/html"))
  in
  List.iter
    (fun q -> check ("q=" ^ q ^ " is not a qvalue") (bad q = [ `Html ]))
    [
      "2"; "1.1"; "1.001"; "0.1234"; "nan"; "inf"; "infinity"; "-0.5"; "0x1";
      "1e0"; ".5"; ""; "abc"; "01"; "1.0e1";
    ];
  check "three decimals are a qvalue"
    (Negotiate.of_accept (Some "application/json;q=0.123") = [ `Json ]);
  check "a bare zero is a qvalue"
    (Negotiate.of_accept (Some "application/json;q=0.") = [])

let handler =
  Negotiate.v
    [
      (`Html, fun _env _req respond -> Resp.html respond "<h1>hi</h1>");
      ( `Markdown,
        fun _env _req respond ->
          Resp.media respond "text/markdown" "# hi" );
    ]

let site = Site.of_routes [ get (s "p") handler ]
let compiled = site

let () =
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/markdown") ]
  in
  check "markdown chosen" (Proffer_mock.body r = "# hi");
  check "vary added" (Proffer_mock.header r H.Vary = Some "Accept");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/html") ]
  in
  check "html chosen" (Proffer_mock.body r = "<h1>hi</h1>");
  (* The client accepts both, and ranks Markdown first. Its order decides,
     not the order the variants were offered in. *)
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/markdown, text/html") ]
  in
  check "client order decides" (Proffer_mock.body r = "# hi");
  let r = Proffer_mock.request compiled () M.Get "/p" in
  check "no accept falls back to first" (Proffer_mock.body r = "<h1>hi</h1>");
  (* A client that stated what it accepts is told when none of it is on
     offer, rather than handed a representation it did not ask for. *)
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "application/json") ]
  in
  check "an unsatisfiable accept is 406"
    (Status.code (Proffer_mock.status r) = 406);
  check "406 lists the variants"
    (Proffer_mock.body r = "Not Acceptable\ntext/html\ntext/markdown\n");
  check "406 varies on accept" (Proffer_mock.header r H.Vary = Some "Accept");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/html;q=0") ]
  in
  check "a zero-quality variant is 406"
    (Status.code (Proffer_mock.status r) = 406);
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/html;q=0, text/markdown") ]
  in
  check "a zero-quality member is skipped, not served"
    (Proffer_mock.body r = "# hi");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/html;q=0, */*") ]
  in
  check "a specific refusal overrides a wildcard"
    (Proffer_mock.body r = "# hi");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/*;q=0, */*;q=0.5") ]
  in
  check "a type refusal overrides a wildcard"
    (Status.code (Proffer_mock.status r) = 406);
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/html;Q=0") ]
  in
  check "an uppercase q refusal is honoured"
    (Status.code (Proffer_mock.status r) = 406);
  let r =
    Proffer_mock.request compiled () M.Get "/p" ~headers:[ ("Accept", "*/*") ]
  in
  check "*/* gets the server's first choice"
    (Proffer_mock.body r = "<h1>hi</h1>");
  let r =
    Proffer_mock.request compiled () M.Get "/p" ~headers:[ ("Accept", "text/*") ]
  in
  check "a subtype wildcard matches" (Proffer_mock.body r = "<h1>hi</h1>");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "application/*") ]
  in
  check "a wildcard over another type is 406"
    (Status.code (Proffer_mock.status r) = 406);
  let local_ repeated =
    Headers.vary
      (Headers.of_list [ ("Vary", "Origin"); ("Vary", "Accept-Encoding") ])
      "Accept"
  in
  check "repeated vary fields are combined"
    (Headers.find repeated H.Vary = Some "Origin, Accept-Encoding, Accept");
  let local_ wildcard =
    Headers.vary
      (Headers.of_list [ ("Vary", "*"); ("Vary", "Origin") ])
      "Accept"
  in
  check "a vary wildcard remains a wildcard"
    (Headers.find wildcard H.Vary = Some "*");
  let local_ requested_headers =
    Headers.of_list
      [ ("Vary", "Origin"); ("Vary", "Accept-Encoding"); ("X-Keep", "yes") ]
  in
  let local_ requested_wildcard = Headers.vary requested_headers "*"
  in
  check "requesting a vary wildcard replaces every existing vary field"
    (Headers.find requested_wildcard H.Vary = Some "*"
     && Headers.find_other requested_wildcard "X-Keep" = Some "yes");
  let upper_other =
    Negotiate.v
      [
        ( `Other "IMAGE/PNG",
          fun _env _req respond -> Resp.media respond "image/png" "png" );
      ]
  in
  let upper_site =
    Site.of_routes [ get (s "image") upper_other ]
  in
  let r =
    Proffer_mock.request upper_site () M.Get "/image"
      ~headers:[ ("Accept", "image/png") ]
  in
  check "other media types match without regard to case"
    (Proffer_mock.body r = "png");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "application/json"); ("Accept", "text/markdown") ]
  in
  check "repeated accept fields are combined" (Proffer_mock.body r = "# hi")

(* [select] keeps a first-codec fallback for callers that must produce
   something; [select_opt] and [encode] report the refusal instead. *)
let () =
  let codecs = [ Httpz.Media.html; Httpz.Media.octets ] in
  let req accept = exclave_
    Req.v ~meth:M.Get ~target:"/"
      ~headers:(Headers.of_list [ ("Accept", accept) ])
      ()
  in
  check "select falls back to the first codec"
    (Httpz.Media.media_type (Negotiate.select codecs (req "image/png"))
    = Httpz.Media.media_type Httpz.Media.html);
  check "select_opt reports no match"
    (Negotiate.select_opt codecs (req "image/png") = None);
  check "select_opt skips a zero-quality range"
    (Negotiate.select_opt codecs (req "text/html;q=0") = None);
  check "select_opt applies a specific refusal before a wildcard"
    (match Negotiate.select_opt codecs (req "text/html;q=0, */*") with
    | Some c ->
        String.equal (Httpz.Media.media_type c)
          (Httpz.Media.media_type Httpz.Media.octets)
    | None -> false);
  check "select_opt honours a wildcard"
    (match Negotiate.select_opt codecs (req "*/*") with
    | Some c ->
        String.equal (Httpz.Media.media_type c)
          (Httpz.Media.media_type Httpz.Media.html)
    | None -> false);
  let no_accept = Req.v ~meth:M.Get ~target:"/" () in
  check "select_opt without accept is the first codec"
    (match Negotiate.select_opt codecs no_accept with
    | Some c ->
        String.equal (Httpz.Media.media_type c)
          (Httpz.Media.media_type Httpz.Media.html)
    | None -> false)

let () =
  let route =
    get (s "e") (fun () req respond ->
        Negotiate.encode respond req [ Httpz.Media.html ] "<p>x</p>")
  in
  let site = Site.of_routes [ route ] in
  let r =
    Proffer_mock.request site () M.Get "/e"
      ~headers:[ ("Accept", "application/json") ]
  in
  check "encode answers 406" (Status.code (Proffer_mock.status r) = 406);
  check "encode lists the codecs"
    (Proffer_mock.body r = "Not Acceptable\ntext/html\n");
  check "encode varies on accept"
    (Proffer_mock.header r H.Vary = Some "Accept");
  let r =
    Proffer_mock.request site () M.Get "/e" ~headers:[ ("Accept", "text/html") ]
  in
  check "encode still encodes" (Proffer_mock.body r = "<p>x</p>")

let () = Printf.printf "test_negotiate: %d checks ok\n" !checks
