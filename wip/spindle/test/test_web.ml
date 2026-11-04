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
  Eio.Switch.run @@ fun sw ->
  let store = Store.open_ ~sw directory in
  let settings =
    Spindle.Settings.v ~store
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
    ctx.step "build" (fun () -> ctx.log `Stdout "<script>alert(1)</script>\n");
    Eio.Promise.resolve set_done ()
  in
  let engine =
    Engine.v ~store ~catalog ~settings ~hostname:"spindle.test"
      ~jobs:[ Job.inspect ] ~executor ~system:env ~directory ~sw ()
  in
  Spindle.Settings.note_owner settings ~repo ~owner;
  Spindle.Settings.note_owner settings ~repo:bobs ~owner:bob;
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
    Web.v ~sw ~engine ~store ~catalog ~directory ~hostname:"spindle.test"
      ~origin:"https://spindle.test" env
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
      ~headers:(("Content-Type", "application/x-www-form-urlencoded") :: headers)
      ~body site env' Httpz.Method.Post target
  in
  let code r = Proffer.Status.code (M.status r) in
  let other r k = M.header_other r k in
  (* Every page carries a strict policy and is never cached. *)
  let login = get "/ui" in
  assert (code login = 200);
  assert (has (M.body login) {|action="/ui/login"|});
  (match other login "Content-Security-Policy" with
  | Some csp -> assert (has csp "default-src 'none'" && not (has csp "unsafe"))
  | None -> assert false);
  assert (other login "X-Content-Type-Options" = Some "nosniff");

  assert (
    M.header login Httpz.Header_name.Cache_control = Some "no-store"
    || other login "Cache-Control" = Some "no-store");
  (* The client is confidential, and its key set holds no private key. *)
  let metadata = M.body (get "/oauth/client-metadata.json") in
  assert (
    has metadata
      {|"client_id":"https://spindle.test/oauth/client-metadata.json"|});
  assert (has metadata "private_key_jwt");
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
  (* Bob sees only his own. *)
  let bob_cookie, _ = Web.login_as web bob in
  let as_bob t = get ~headers:[ ("Cookie", bob_cookie) ] t in
  let bob_home = M.body (as_bob "/ui") in
  assert (has bob_home bobs && not (has bob_home "demo"));
  assert (code (as_bob ("/ui/repo?did=" ^ repo)) = 403);
  assert (
    code
      (as_bob (Printf.sprintf "/ui/log?repo=%s&id=%s&workflow=inspect" repo id))
    = 403);
  (* A pipeline is shown only under its own repository. *)
  assert (
    code (as_owner (Printf.sprintf "/ui/pipeline?repo=%s&id=%s" bobs id)) = 404);
  assert (
    code (as_owner (Printf.sprintf "/ui/pipeline?repo=%s&id=%s" repo id)) = 200);
  (* Build output is text. *)
  let log =
    M.body
      (as_owner
         (Printf.sprintf "/ui/log?repo=%s&id=%s&workflow=inspect" repo id))
  in
  assert (has log "&lt;script&gt;alert(1)&lt;/script&gt;");
  assert (not (has log "<script>alert"));
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
    code (post ~headers:[ ("Cookie", cookie) ] ~body:"_csrf=wrong" "/ui/logout")
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
     checks, and who sees which repository passed"
