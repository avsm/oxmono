(* SPDX-License-Identifier: ISC *)
module S = Sitekit
module W = Proffer_mock

let has s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

(* A field may be stored under its httpz name or its spelling. *)
let field r name known =
  match W.header r known with Some v -> Some v | None -> W.header_other r name

let render h = Htmlit.El.to_string ~doctype:false h
let url s = Option.get (S.Url.of_path s)

let urls () =
  (* Segments and queries are encoded, so a value cannot add a parameter or a
     path. *)
  assert (
    S.Url.to_string (S.Url.path ~query:[ ("q", "a&b=c d") ] [ "jobs"; "x/y" ])
    = "/jobs/x%2Fy?q=a%26b%3Dc%20d");
  assert (S.Url.to_string (S.Url.path []) = "/");
  List.iter
    (fun bad -> assert (S.Url.of_path bad = None))
    [
      "";
      "jobs";
      "//evil.example/";
      "/\\evil.example";
      "/a\\b";
      "/a b";
      "/a\r\nSet-Cookie: x";
      "/a\tb";
      "javascript:alert(1)";
      "/caf\xc3\xa9";
    ];
  assert (S.Url.of_path "/jobs?state=failed&x=%2F" <> None);
  List.iter
    (fun bad -> assert (S.Url.external_ bad = None))
    [
      "javascript:alert(1)";
      "JAVASCRIPT:alert(1)";
      "data:text/html,<script>alert(1)</script>";
      "vbscript:x";
      "https://";
      "https:///path";
      "//evil.example";
      "https://a.example/\"onmouseover=x";
      "https://a.example/<script>";
      "https://a.example/ x";
      "https://a.example\\@evil.example";
    ];
  assert (S.Url.external_ "https://tangled.org/x?y=1" <> None);
  assert (S.Url.external_ "HTTP://example.org" <> None);
  assert (S.Url.path_part (url "/jobs?a=1#f") = "/jobs")

let escaping () =
  let nasty = {|<script>alert("x")</script>' onload=x|} in
  let out =
    render
      (Htmlit.El.splice
         [
           S.badge nasty;
           S.metric ~label:nasty (S.text nasty);
           S.link (S.Url.path ~query:[ ("q", nasty) ] [ "s" ]) nasty;
           S.table
             [ (nasty, `Text) ]
             [ [ S.cell ~sort:nasty [ S.text nasty ] ] ];
           S.facts [ (nasty, Some (S.text nasty)); ("empty", None) ];
           S.input ~name:nasty ~label:nasty ~value:nasty ();
           S.select ~name:"s" ~label:nasty ~value:nasty [ (nasty, nasty) ];
           S.post_button ~action:(S.Url.path [ "logout" ])
             ~hidden:[ ("csrf", nasty) ]
             ~label:nasty;
           S.log nasty;
           S.state nasty;
         ])
  in
  assert (not (has out "<script>"));
  assert (not (has out {|"x")|}));
  assert (has out "&lt;script&gt;");
  (* An unknown state gets the unknown icon, not an id from the input. *)
  assert (has out {|href="#sk-state-unknown"|});
  assert (not (has out "empty"));
  match S.input ~name:"x" ~label:"x" ~kind:"hidden" ~value:"" () with
  | _ -> assert false
  | exception Invalid_argument _ -> ()

let no_inline_style () =
  let out =
    render
      (Htmlit.El.splice
         [
           S.coverage ~present:3 ~total:7;
           S.coverage ~present:0 ~total:0;
           S.coverage ~present:9 ~total:2;
           S.state_sprite;
           S.metrics [ S.metric ~label:"a" (S.text "1") ];
           S.table [ ("n", `Number) ] [ [ S.cell ~ratio:0.5 [ S.text "1" ] ] ];
           S.checkbox ~id:"c" ~label:"c" ~checked:true;
           S.get_form ~action:(S.Url.path [ "q" ]) [];
           S.next_page (S.Url.path [ "q" ]) "Next →";
           S.sr_only "x";
         ])
  in
  assert (not (has out "style="));
  assert (not (has out " on"));
  (* A coverage bar never overflows its frame. *)
  assert (has out {|width="100.000"|});
  assert (has out {|width="42.857"|})

let headers () =
  let csp = S.Headers.csp () in
  assert (not (has csp "unsafe"));
  assert (has csp "default-src 'none'" && has csp "frame-ancestors 'none'");
  assert (has csp "script-src 'self'" && has csp "style-src 'self'");
  assert (
    has
      (S.Headers.csp ~img_src:[ "https:" ] ~frame_ancestors:[ "'self'" ] ())
      "img-src 'self' https:");
  List.iter
    (fun bad ->
      match S.Headers.csp ~img_src:[ bad ] () with
      | _ -> assert false
      | exception Invalid_argument _ -> ())
    [ "'unsafe-inline'"; "'UNSAFE-EVAL'"; "a; script-src *"; "a b"; "" ];
  let h = S.Headers.html () in
  assert (List.assoc "Cache-Control" h = "no-store");
  assert (List.assoc "X-Content-Type-Options" h = "nosniff");
  assert (List.assoc "Referrer-Policy" h = "same-origin");
  assert (List.assoc "Cross-Origin-Opener-Policy" h = "same-origin")

let site_css = S.Asset.css ~name:"site" ".x{color:red}"

let site =
  S.site ~name:"Example <Site>" ~brand:"ex"
    ~nav:
      [
        S.nav "Pipelines" (S.Url.path [ "pipelines" ]);
        S.nav "Repositories" (S.Url.path ~query:[ ("all", "1") ] [ "repos" ]);
      ]
    ~stylesheets:[ site_css ]
    ~footer:[ S.text "Footer" ]
    ()

let routes =
  Proffer.Site.of_routes
    (Proffer.Route.get
       Proffer.Route.(s "pipelines")
       (fun page _ respond -> S.respond_html respond page)
    :: S.Asset.routes [ site_css ])

let pages () =
  (* Asset paths name their content. *)
  let path = S.Url.to_string (S.Asset.url site_css) in
  let hex =
    String.sub Digestif.SHA256.(to_hex (digest_string ".x{color:red}")) 0 16
  in
  assert (path = "/_sitekit/site." ^ hex ^ ".css");
  assert (S.Asset.url site_css <> S.Asset.url (S.Asset.css ~name:"site" ".x{}"));
  (match S.Asset.css ~name:"../x" "" with
  | _ -> assert false
  | exception Invalid_argument _ -> ());
  let page =
    S.page site ~current:"/repos" ~title:"All <repos>"
      ~account:
        (S.account ~name:"@alice.example" ~logout:(S.Url.path [ "logout" ])
           ~csrf:("csrf", "t0k\"en") ())
      [ S.text "body" ]
  in
  assert (has page "<title>All &lt;repos&gt; · Example &lt;Site&gt;</title>");
  assert (
    has page {|<a href="/repos?all=1" aria-current="page">Repositories</a>|});
  assert (not (has page {|aria-current="page">Pipelines|}));
  assert (
    has page {|<main id="main" class="sk-main"><h1>All &lt;repos&gt;</h1>|});
  assert (has page {|<input type="hidden" name="csrf" value="t0k&quot;en">|});
  assert (has page {|method="post" action="/logout"|});
  (* The account is a menu that opens without the script, showing the
     account's initial when it has no picture. *)
  assert (has page {|<details class="sk-menu"><summary>|});
  assert (has page {|<span aria-hidden="true" class="sk-initial">A</span>|});
  assert (has page "Account menu for @alice.example");
  let menu =
    String.concat ""
      (List.map
         (Htmlit.El.to_string ~doctype:false)
         (S.account ~avatar:(S.Url.path [ "me.png" ])
            ~links:[ ("Settings", S.Url.path [ "settings" ]) ]
            ~name:"bob" ~logout:(S.Url.path [ "logout" ]) ~csrf:("c", "t") ()))
  in
  assert (has menu {|src="/me.png"|} && has menu {|width="28"|});
  assert (has menu {|<a href="/settings">Settings</a>|});
  assert (not (has menu "sk-initial"));
  assert (has page (Printf.sprintf {|<link rel="stylesheet" href="%s">|} path));
  assert (has page {|defer|});
  assert ((not (has page "<style")) && not (has page "style="));
  assert (not (has page "<script>"));
  let get target =
    W.request ~on_error:raise routes page Httpz.Method.Get target
  in
  let r = get "/pipelines" in
  assert (Proffer.Status.code (W.status r) = 200);
  assert (W.body r = page);
  assert (
    not (has (Option.get (W.header_other r "Content-Security-Policy")) "unsafe"));
  assert (
    field r "Cache-Control" Httpz.Header_name.Cache_control = Some "no-store");
  let a = get path in
  assert (Proffer.Status.code (W.status a) = 200);
  assert (W.body a = ".x{color:red}");
  assert (
    field a "Content-Type" Httpz.Header_name.Content_type
    = Some "text/css; charset=utf-8");
  assert (
    field a "Cache-Control" Httpz.Header_name.Cache_control
    = Some "public, max-age=31536000, immutable");
  assert (W.header_other a "X-Content-Type-Options" = Some "nosniff");
  let js = get (S.Url.to_string (S.Asset.url S.Asset.script)) in
  assert (has (W.body js) "sitekit:content");
  assert (Proffer.Status.code (W.status (get "/_sitekit/site.0000.css")) = 404);
  assert (
    Proffer.Status.code
      (W.status (get (S.Url.to_string (S.Asset.url S.Asset.stylesheet))))
    = 200);
  (* A light-only site says so, for its own colours. *)
  assert (
    has
      (S.page
         (S.site ~name:"L" ~brand:"l" ~theme:`Light ())
         ~current:"/" ~title:"t" [])
      {|data-theme="light"|})

let post_forms () =
  let html =
    Htmlit.El.to_string ~doctype:false
      (S.post_form ~action:(S.Url.path [ "login" ])
         ~hidden:[ ("_csrf", "a\"b<c") ]
         ~submit:"Log in"
         [ S.input ~name:"handle" ~label:"Handle" ~value:"<x>" () ])
  in
  assert (has html {|method="post"|} && has html {|action="/login"|});
  assert (has html "a&quot;b&lt;c" && not (has html "a\"b"));
  assert (has html "&lt;x&gt;" && has html "Log in");
  let f =
    Htmlit.El.to_string ~doctype:false
      (S.follow
         (Option.get (S.Url.external_ "https://as.example/a?b=1&c=2"))
         "Go")
  in
  assert (has f {|data-sk-follow|} && has f "b=1&amp;c=2")

let terminal () =
  let render s = Htmlit.El.to_string ~doctype:false (S.terminal s) in
  let t = render "\027[1;31mred\027[0m plain" in
  assert (has t {|<span class="sk-fg1 sk-b">red</span> plain|});
  assert (has (render "\027[38;5;208mx") {|class="sk-fg208"|});
  (* Truecolour is drawn as the nearest palette colour. *)
  assert (has (render "\027[38;2;255;0;0mx") {|class="sk-fg196"|});
  (* Colours in the colon form are dropped, leaving the text. *)
  assert (
    has (render "\027[38:2::0:0:255mx") {|<pre class="sk-log sk-term">x</pre>|});
  assert (has (render "\027[48;5;232mx") {|class="sk-bg232"|});
  assert (
    has
      (render "\027[92;4mx\027[24;39my")
      {|<span class="sk-fg10 sk-u">x</span>y|});
  (* Text is escaped, and escape codes that do anything but style go. *)
  let t = render "\027[31m<script>alert(1)</script>\027]0;title\007\027[2Jok" in
  assert (has t "&lt;script&gt;" && not (has t "<script"));
  assert ((not (has t "title")) && has t "ok");
  assert ((not (has t "\027")) && not (has t "style="));
  (* The stylesheet holds the palette. *)
  assert (has (S.Asset.content S.Asset.stylesheet) ".sk-fg208{color:#ff8700}")

let avatars () =
  let html =
    Htmlit.El.to_string ~doctype:false
      (S.person
         ~avatar:(S.Url.path [ "a" ] ~query:[ ("did", "x\"y") ])
         "@alice")
  in
  assert (has html {|<img src="/a?did=x%22y"|} && has html {|class="sk-avatar"|});
  assert (
    has html {|width="20"|} && has html "@alice" && not (has html "style="))

let formatting () =
  assert (S.count 1234567 = "1,234,567");
  assert (S.count (-1234) = "-1,234");
  assert (S.size 2048L = "2.0 KiB");
  assert (S.duration 3725. = "1h02m" && S.duration 65. = "1m05s");
  assert (S.time (Option.get (Ptime.of_float_s 0.)) = "1970-01-01 00:00 UTC")

let () =
  urls ();
  escaping ();
  no_inline_style ();
  headers ();
  pages ();
  post_forms ();
  terminal ();
  avatars ();
  formatting ();
  print_endline
    "sitekit: URLs, escaping, no inline styles, security headers, assets and \
     pages passed"
