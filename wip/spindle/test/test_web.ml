(* SPDX-License-Identifier: ISC *)
(* The web pages: who may see what, the security fields every page carries,
   and the checks on logging in and out. The OAuth exchange itself is
   tested in xrpc-auth.web. *)
module J = Spindle__Json
module Store = Spindle__Store
module Catalog = Spindle__Catalog
module Engine = Spindle__Engine
module Runner = Spindle__Runner
module Web = Spindle__Web
module Job = Spindle.Job
module M = Proffer_mock
open J

let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa"
let bob = "did:plc:bbbbbbbbbbbbbbbbbbbbbbbb"
let repo = "did:web:repo.test"
let bobs = "did:web:bobs.test"

let has s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 60. @@ fun () ->
  let name = Filename.temp_file "spindle-web-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  let directory = Eio.Path.(env#fs / name) in
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  (* The engine's workers run until the switch ends, so the test ends it. *)
  try
    Eio.Switch.run @@ fun sw ->
    let store = Store.open_ ~sw directory in
    let settings =
      Spindle__Settings.v ~store
        ~now:(fun () -> 0.)
        ~defaults:{ repo = []; user = [] }
    in
    let network =
      Spindle__Network.v ~allow_http:true ~plc:"http://127.0.0.1:1" env
    in
    let catalog =
      Catalog.v ~store ~settings ~network ~owner ~hostname:"spindle.test"
        ~admission:Catalog.Closed ~ttl:60.
        ~now:(fun () -> Eio.Time.now env#clock)
        ~static:
          (Some
             {
               Catalog.did = repo;
               owner;
               name = "demo";
               knot = "";
               source = name ^ "/missing";
               created = "";
             })
        ()
    in
    let done_, set_done = Eio.Promise.create () in
    let executor (ctx : Spindle.Executor.context) _ =
      ctx.step "build" (fun () ->
          ctx.log `Stdout "<script>alert(1)</script>\n";
          ctx.log `Stdout "\027[1;32mok\027[0m \027]0;title\007done\n");
      Eio.Promise.resolve set_done ()
    in
    let jobs =
      Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
        Eio.Path.(directory / "jobs.sqlite")
    in
    let engine =
      Engine.v ~store ~catalog ~settings ~hostname:"spindle.test"
        ~jobs:[ Job.inspect ] ~durable:jobs ~executor ~system:env ~directory ~sw
        ()
    in
    Engine.serve engine;
    Spindle__Settings.note_owner settings ~repo ~owner;
    Spindle__Settings.note_owner settings ~repo:bobs ~owner:bob;
    let id =
      Engine.create engine ~automatic:false ~actor:owner
        (obj
           [
             ("repo", str repo);
             ( "trigger",
               obj
                 [
                   ("$type", str "org.tangled.ci.trigger.manual");
                   ("commit", str (String.make 40 'a'));
                 ] );
           ])
      |> Option.get
    in
    Eio.Promise.await done_;
    let web =
      Web.v ~sw ~jobs ~engine ~store ~catalog ~directory
        ~hostname:"spindle.test" ~origin:"https://spindle.test" env
    in
    let site = Proffer.Site.of_routes (Web.routes ()) in
    let env' name req respond =
      Web.handle web (String.sub name 4 (String.length name - 4)) req respond
    in
    let get ?(headers = []) target =
      M.request ~headers site env' Httpz.Method.Get target
    in
    let post ?(headers = []) ~body target =
      M.request
        ~headers:
          (("Content-Type", "application/x-www-form-urlencoded") :: headers)
        ~body site env' Httpz.Method.Post target
    in
    let code r = Proffer.Status.code (M.status r) in
    let other r k = M.header_other r k in
    (* Every page carries a strict policy and is never cached. *)
    let login = get "/ui" in
    assert (code login = 200);
    assert (has (M.body login) {|action="/ui/login"|});
    (match other login "Content-Security-Policy" with
    | Some csp ->
        assert (has csp "default-src 'none'" && not (has csp "unsafe"))
    | None -> assert false);
    assert (other login "X-Content-Type-Options" = Some "nosniff");

    assert (
      M.header login Httpz.Header_name.Cache_control = Some "no-store"
      || other login "Cache-Control" = Some "no-store");
    (* The client is confidential, and its key set holds no private key. *)
    let metadata = M.body (get "/oauth-client-metadata.json") in
    assert (
      has metadata
        {|"client_id":"https://spindle.test/oauth-client-metadata.json"|});
    assert (has metadata "private_key_jwt");
    (* The metadata names what moving a repository here asks for. *)
    assert (has metadata "rpc:com.atproto.repo.putRecord?aud=*");
    assert (has metadata "repo:org.tangled.graph.attestation?action=create");
    let jwks = M.body (get "/oauth/jwks.json") in
    assert (has jwks {|"kty":"EC"|} && not (has jwks {|"d":|}));
    assert (
      (Unix.stat (Filename.concat name "web-client.key")).st_perm land 0o077 = 0);
    (* A login posted from another site is refused before anything is asked. *)
    assert (
      code
        (post
           ~headers:[ ("Sec-Fetch-Site", "cross-site") ]
           ~body:"handle=a.b" "/ui/login")
      = 403);
    assert (
      has (M.body (post ~body:"handle=%3Cscript%3E" "/ui/login")) "not a handle");
    (* A callback for a login nobody started is refused. *)
    let stray = get "/oauth/callback?state=abc&iss=https://x&code=c" in
    assert (code stray = 400 && has (M.body stray) "expired");
    (* The owner, an admin, sees every repository. *)
    let cookie, csrf = Web.login_as web owner in
    let as_owner t = get ~headers:[ ("Cookie", cookie) ] t in
    let home = M.body (as_owner "/ui") in
    assert (has home "demo" && has home bobs);
    (* An account shows by its handle once one is known, with its picture
       from here, and by its DID until then. *)
    assert (has home owner);
    let cookie, csrf = Web.login_as ~handle:"alice.test" web owner in
    let as_owner t = get ~headers:[ ("Cookie", cookie) ] t in
    let named = M.body (as_owner "/ui") in
    assert (has named "@alice.test");
    assert (
      has named
        ({|<img src="/ui/avatar?did=|}
        ^ String.concat "%3A" (String.split_on_char ':' owner)));
    (* An account whose picture cannot be read has none, and asking for one
       of something that is not a DID is no page. *)
    assert (code (as_owner ("/ui/avatar?did=" ^ owner)) = 404);
    assert (code (as_owner "/ui/avatar?did=../../etc") = 404);
    assert (code (get ("/ui/avatar?did=" ^ owner)) = 200);
    assert (
      has (M.body (get ("/ui/avatar?did=" ^ owner))) {|action="/ui/login"|});
    (* A browser's request for the root is sent to the pages, and others get
       the health report. *)
    let root =
      Proffer.Site.of_routes
        [
          Proffer.Route.get Proffer.Route.root (fun env req respond ->
              env "root" req respond);
        ]
    in
    let answer _ req respond =
      Proffer.Resp.text respond
        (if Web.wants_html req then "pages" else "health")
    in
    let at accept =
      M.body (M.request ~headers:accept root answer Httpz.Method.Get "/")
    in
    assert (at [ ("Accept", "Text/HTML,application/xhtml+xml;q=0.9") ] = "pages");
    assert (at [ ("Accept", "application/json") ] = "health");
    assert (at [] = "health");
    (* Bob sees only his own. *)
    let bob_cookie, _ = Web.login_as web bob in
    let as_bob t = get ~headers:[ ("Cookie", bob_cookie) ] t in
    let bob_home = M.body (as_bob "/ui") in
    assert (has bob_home bobs && not (has bob_home "demo"));
    assert (code (as_bob ("/ui/repo?did=" ^ repo)) = 403);
    assert (
      code
        (as_bob
           (Printf.sprintf "/ui/log?repo=%s&id=%s&workflow=inspect" repo id))
      = 403);
    (* A pipeline is shown only under its own repository. *)
    assert (
      code (as_owner (Printf.sprintf "/ui/pipeline?repo=%s&id=%s" bobs id))
      = 404);
    assert (
      code (as_owner (Printf.sprintf "/ui/pipeline?repo=%s&id=%s" repo id))
      = 200);
    (* Build output is text. *)
    let log =
      M.body
        (as_owner
           (Printf.sprintf "/ui/log?repo=%s&id=%s&workflow=inspect" repo id))
    in
    assert (has log "&lt;script&gt;alert(1)&lt;/script&gt;");
    assert (not (has log "<script>alert"));
    (* Terminal colours are classes, and other escapes are gone. *)
    assert (has log {|<span class="sk-fg2 sk-b">ok</span> done|});
    assert (not (has log "0;title"));
    (* Admins see the jobs tab, the queue's activity and each job. *)
    assert (has (M.body (as_owner "/ui")) {|href="/ui/jobs"|});
    assert (not (has (M.body (as_bob "/ui")) {|href="/ui/jobs"|}));
    let jobs_page = M.body (as_owner "/ui/jobs") in
    assert (has jobs_page "Recent activity" && has jobs_page "claimed");
    assert (has jobs_page "succeeded");
    let jobs_done = M.body (as_owner "/ui/jobs?state=done") in
    assert (has jobs_done ("inspect of " ^ id));
    let job = M.body (as_owner "/ui/job?id=1") in
    assert (has job "Timeline" && has job "enqueued");
    assert (code (as_bob "/ui/jobs") = 403 && code (as_bob "/ui/job?id=1") = 403);
    assert (code (as_owner "/ui/jobs?state=bogus") = 404);
    (* Admins add and remove members, with the session's token from this
     site. *)
    let carol = "did:plc:cccccccccccccccccccccccc" in
    let form ?(origin = "https://spindle.test") cookie body target =
      post ~headers:[ ("Cookie", cookie); ("Origin", origin) ] ~body target
    in
    let token = Uriz.pct_encode ~component:`Query_value csrf in
    assert (has (M.body (as_owner "/ui/members")) {|action="/ui/members/add"|});
    assert (has (M.body (as_owner "/ui")) {|href="/ui/members"|});
    assert (code (form cookie ("handle=" ^ carol) "/ui/members/add") = 403);
    assert (
      code
        (form ~origin:"https://evil.test" cookie
           ("_csrf=" ^ token ^ "&handle=" ^ carol)
           "/ui/members/add")
      = 403);
    assert (
      code
        (form cookie ("_csrf=" ^ token ^ "&handle=" ^ carol) "/ui/members/add")
      = 303);
    assert (List.mem carol (Catalog.allowed catalog));
    assert (has (M.body (as_owner "/ui/members")) carol);
    assert (
      has
        (M.body
           (form cookie
              ("_csrf=" ^ token ^ "&handle=not%20a%20handle")
              "/ui/members/add"))
        "Could not add");
    (* Moving a repository here needs a form from this site with the
       session's token, for a repository the page lists as the user's. *)
    let elsewhere = "did:plc:eeeeeeeeeeeeeeeeeeeeeeee" in
    assert (code (form cookie ("did=" ^ elsewhere) "/ui/move") = 403);
    assert (
      code
        (form ~origin:"https://evil.test" cookie
           ("_csrf=" ^ token ^ "&did=" ^ elsewhere)
           "/ui/move")
      = 403);
    assert (
      code (form cookie ("_csrf=" ^ token ^ "&did=" ^ elsewhere) "/ui/move")
      = 404);
    (* Others may neither see nor change them, even with their own token. *)
    let bob_session, bob_csrf = Web.login_as web bob in
    assert (code (as_bob "/ui/members") = 403);
    assert (
      code
        (form bob_session
           ("_csrf="
           ^ Uriz.pct_encode ~component:`Query_value bob_csrf
           ^ "&handle=" ^ bob)
           "/ui/members/add")
      = 403);
    assert (not (List.mem bob (Catalog.allowed catalog)));
    assert (
      code
        (get
           ~headers:[ ("Cookie", cookie) ]
           ("/ui/members/add?handle=" ^ carol))
      <> 303);
    assert (
      code
        (form cookie ("_csrf=" ^ token ^ "&did=" ^ carol) "/ui/members/remove")
      = 303);
    assert (not (List.mem carol (Catalog.allowed catalog)));
    (* Logging out by any method but POST does nothing, even with the token, as
     when a GET is routed to it by mistake. *)
    let stray_site =
      Proffer.Site.of_routes
        [
          Proffer.Route.get
            Proffer.Route.(s "x")
            (fun env req respond -> env "web:logout" req respond);
        ]
    in
    ignore
      (M.request
         ~headers:[ ("Cookie", cookie) ]
         stray_site env' Httpz.Method.Get
         ("/x?_csrf=" ^ Uriz.pct_encode ~component:`Query_value csrf));
    assert (has (M.body (as_owner "/ui")) "demo");
    (* Logging out needs the session's CSRF token from this site. *)
    assert (
      code
        (post ~headers:[ ("Cookie", cookie) ] ~body:"_csrf=wrong" "/ui/logout")
      = 403);
    let out =
      post
        ~headers:[ ("Cookie", cookie); ("Origin", "https://spindle.test") ]
        ~body:("_csrf=" ^ Uriz.pct_encode ~component:`Query_value csrf)
        "/ui/logout"
    in
    assert (code out = 303);
    assert (has (M.body (as_owner "/ui")) {|action="/ui/login"|});
    (* A forged or repeated session cookie is no session. *)
    assert (
      has
        (M.body
           (get
              ~headers:[ ("Cookie", "__Host-spindle=" ^ String.make 43 'A') ]
              "/ui"))
        {|action="/ui/login"|});
    assert (
      has
        (M.body
           (get ~headers:[ ("Cookie", bob_cookie ^ "; " ^ bob_cookie) ] "/ui"))
        {|action="/ui/login"|});
    print_endline
      "web: security fields, confidential client metadata, login and logout \
       checks, who sees which repository, handles, avatars, the root redirect \
       and the jobs tab passed";
    Eio.Switch.fail sw Exit
  with Exit -> ()
