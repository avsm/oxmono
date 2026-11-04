(* SPDX-License-Identifier: ISC *)
open Proffer
module Status = Httpz.Res
module S = Sitekit
module Session = Proffer_session
module Oauth = Xrpc_auth_web

type t = {
  engine : Engine.t;
  store : Store.t;
  catalog : Catalog.t;
  sessions : Session.t;
  client : Oauth.Client.t;
  http : Fetch.plain;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  plc : string option;
  site : S.site;
  admin_site : S.site;  (** With the jobs tab, which only admins may see. *)
  jobs : Duraml.plain option;
  collaborates : (string * string, float * bool) Hashtbl.t;
      (* Whether a user collaborates on a repository, and when the knot said
         so, so that a page does not ask the knot about every repository. *)
  lock : Eio.Mutex.t;
  mutable logins : int;  (** Logins being started or finished now. *)
  identity : Identity.t;
  elsewhere : (string, float * (string * string * string option) list) Hashtbl.t;
      (* Each user's repositories, with their names and spindles, briefly. *)
  hostname : string;
}

(* Bounds on what visitors who have not logged in can make the spindle do. *)
let max_logins = 8
let max_pending = 500
let login_deadline = 30.

exception Forbidden
exception Missing

(* Stored records are JSON. *)
let encode codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error e -> failwith e

let decode codec s =
  match Jsont_bytesrw.decode_string codec s with
  | Ok v -> Some v
  | Error _ -> None


let ptime =
  Jsont.map ~kind:"time"
    ~dec:(fun s ->
      match Ptime.of_rfc3339 s with
      | Ok (t, _, _) -> t
      | Error _ -> Jsont.Error.msgf Jsont.Meta.none "not a time: %s" s)
    ~enc:(fun t -> Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 t)
    Jsont.string

(* The CSRF secret is raw bytes, so it is stored as base64url. *)
let bytes =
  Jsont.map ~kind:"bytes"
    ~dec:(fun s ->
      match Jsonwt.base64url_decode s with
      | Ok v -> v
      | Error _ -> Jsont.Error.msgf Jsont.Meta.none "not base64url")
    ~enc:Jsonwt.base64url_encode Jsont.string

let record_jsont =
  Jsont.Object.map ~kind:"session"
    (fun subject created seen expires csrf data ->
      { Session.Store.subject; created; seen; expires; csrf; data })
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.Session.Store.subject)
  |> Jsont.Object.mem "created" ptime ~enc:(fun r -> r.Session.Store.created)
  |> Jsont.Object.mem "seen" ptime ~enc:(fun r -> r.Session.Store.seen)
  |> Jsont.Object.mem "expires" ptime ~enc:(fun r -> r.Session.Store.expires)
  |> Jsont.Object.mem "csrf" bytes ~enc:(fun r -> r.Session.Store.csrf)
  |> Jsont.Object.mem "data" Jsont.string ~enc:(fun r -> r.Session.Store.data)
  |> Jsont.Object.finish

(* Sessions live in the store, keyed by the hash of their ID, which
   [Proffer_session] gives. *)
let session_store store : Session.Store.t =
  {
    get = (fun key -> Option.bind (Store.get store "web-session" key) (decode record_jsont));
    put = (fun key r -> Store.put store "web-session" key (encode record_jsont r));
    delete = (fun key -> Store.delete store "web-session" key);
  }

(* A login in progress, keyed by the hash of its OAuth state, bound to the
   browser that started it by the hash of a cookie's nonce. *)
type login = { pending : Oauth.pending; nonce : string }

let login_jsont =
  Jsont.Object.map ~kind:"login" (fun pending nonce -> { pending; nonce })
  |> Jsont.Object.mem "pending" Oauth.pending_jsont ~enc:(fun l -> l.pending)
  |> Jsont.Object.mem "nonce" Jsont.string ~enc:(fun l -> l.nonce)
  |> Jsont.Object.finish

let state_key state = Digestif.SHA256.(to_hex (digest_string state))

(* How long the client's signing key is used before a new one replaces it. *)
let key_lifetime = 90. *. 86400.

(* The client's signing key is kept with the spindle's state and replaced
   when the spindle starts after [key_lifetime]. The spindle keeps no
   sessions with authorization servers, which only see the key during a
   login, so the old key need not be published any longer. A login in
   progress across a rotation must start again. *)
let client_key ~now directory =
  let path = Eio.Path.(directory / "web-client.key") in
  let fresh () =
    let key = Oauth.Key.generate () in
    let next = Eio.Path.(directory / "web-client.key.new") in
    (try Eio.Path.unlink next with Eio.Io _ -> ());
    Eio.Path.save ~create:(`Exclusive 0o600) next (Oauth.Key.to_string key);
    Eio.Path.rename next path;
    key
  in
  match (Eio.Path.load path, Eio.Path.stat ~follow:false path) with
  | s, st when now -. st.mtime < key_lifetime -> Oauth.Key.of_string (String.trim s)
  | _ -> fresh ()
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> fresh ()

let idle = 2. *. 3600.

(* Expired sessions and logins are swept away, since a session is otherwise
   removed only when its own cookie comes back. *)
let sweep t =
  let now = Eio.Time.now t.clock in
  let ended r =
    Ptime.to_float_s r.Session.Store.expires <= now
    || Ptime.to_float_s r.Session.Store.seen +. idle <= now
  in
  List.iter
    (fun (key, raw) ->
      match decode record_jsont raw with
      | Some r when not (ended r) -> ()
      | _ -> Store.delete t.store "web-session" key)
    (Store.list t.store "web-session");
  List.iter
    (fun (key, raw) ->
      match decode login_jsont raw with
      | Some l when Oauth.pending_expires_at l.pending > now -> ()
      | _ -> Store.delete t.store "web-login" key)
    (Store.list t.store "web-login")

let v ~sw ~engine ~store ~catalog ~directory ~hostname ?jobs ?plc ?origin ?http
    env =
  let origin = Option.value ~default:("https://" ^ hostname) origin in
  let key = client_key ~now:(Eio.Time.now env#clock) directory in
  let kid =
    String.sub
      Digestif.SHA256.(
        to_hex
          (digest_string
             (encode Jsont.json (Oauth.Key.public_jwk key))))
      0 16
  in
  let client =
    Oauth.Client.v
      (* Bluesky's login page names a client by its host alone when its
         metadata is at this conventional path, and by its whole URL
         otherwise. *)
      ~client_id:(origin ^ "/oauth-client-metadata.json")
      ~redirect_uri:(origin ^ "/oauth/callback")
      ~jwks_uri:(origin ^ "/oauth/jwks.json")
      (* Bluesky's authorization server requires the client's address to be a
         parent of its client ID, which the spec does not. *)
      ~key ~kid ~client_name:hostname ~client_uri:origin ()
  in
  let http =
    match http with
    | Some h -> h
    | None ->
        (* Each visitor's login is its own: no cookie jar, no retries that an
           unresponsive server could stretch, and no answer over 1 MiB. *)
        (Fetch_httpz.v env#net ~clock:env#mono_clock ~https:Httpz_tls.system
           ~connect:(Oauth.Public_net.connect env#net)
           ~max_response:(1024 * 1024)
           ()
          :> Fetch.plain)
  in
  {
    engine;
    store;
    catalog;
    sessions =
      Session.v ~name:"spindle" ~origin ~random:env#secure_random
        ~clock:env#clock (session_store store);
    client;
    http;
    clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
    plc;
    site =
      S.site ~name:hostname ~brand:"sp"
        ~home:(S.Url.path [ "ui" ])
        ~nav:[ S.nav "Repositories" (S.Url.path [ "ui" ]) ]
        ();
    admin_site =
      S.site ~name:hostname ~brand:"sp"
        ~home:(S.Url.path [ "ui" ])
        ~nav:
          [
            S.nav "Repositories" (S.Url.path [ "ui" ]);
            S.nav "Jobs" (S.Url.path [ "ui"; "jobs" ]);
            S.nav "Members" (S.Url.path [ "ui"; "members" ]);
          ]
        ();
    jobs;
    collaborates = Hashtbl.create 16;
    lock = Eio.Mutex.create ();
    logins = 0;
    identity =
      Identity.v ~store ~network:(Catalog.network catalog) ~http
        ~clock:env#clock;
    elsewhere = Hashtbl.create 16;
    hostname;
  }
  |> fun t ->
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        (try sweep t with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e -> Printf.eprintf "spindle web sweep: %s\n%!" (Printexc.to_string e));
        Eio.Time.sleep t.clock 600.
      done;
      `Stop_daemon);
  t

(* {1 Who sees what} *)

let admin t did = Catalog.admin t.catalog did

(* Every repository the spindle has accepted, with its owner. *)
let served t = Store.list t.store "repo-owner"

let collaborator t ~user repo =
  let now = Eio.Time.now t.clock in
  match Hashtbl.find_opt t.collaborates (user, repo) with
  | Some (at, yes) when now -. at < 300. -> yes
  | _ ->
      let yes =
        try
          match Catalog.managed t.catalog repo with
          | Some r -> Catalog.authorized t.catalog r user
          | None -> false
        with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | _ -> false
      in
      if Hashtbl.length t.collaborates > 4096 then Hashtbl.reset t.collaborates;
      Hashtbl.replace t.collaborates (user, repo) (now, yes);
      yes

let can_see t ~user repo =
  admin t user
  ||
  match List.assoc_opt repo (served t) with
  | Some owner when owner = user -> true
  | Some _ -> collaborator t ~user repo
  | None -> false

let visible t ~user =
  List.filter (fun (repo, _) -> can_see t ~user repo) (served t)

(* The spindle's users, who may log in at all. *)
let user t did =
  admin t did || Catalog.admitted t.catalog did || visible t ~user:did <> []

(* {1 Responses} *)

let set_cookie (f : Headers.field) = ("Set-Cookie", f.value)

let html ?(status = Status.Success) ?(cookies = []) (respond @ local) body =
  S.respond_html respond ~status
    ~headers:(S.Headers.html () @ List.map set_cookie cookies)
    body

let see_other ?(cookies = []) (respond @ local) location =
  Resp.html respond ~status:Status.See_other
    ~headers:
      (Headers.of_list
         ((("Location", location) :: List.map set_cookie cookies)
         @ S.Headers.html ()))
    ""

(* The client's metadata and keys are public, for authorization servers. *)
let json (respond @ local) j =
  Resp.media respond
    ~headers:
      (Headers.of_list
         [
           ("Cache-Control", "public, max-age=300");
           ("X-Content-Type-Options", "nosniff");
           ("Access-Control-Allow-Origin", "*");
         ])
    "application/json" (encode Jsont.json j)

let avatar_url did = S.Url.path [ "ui"; "avatar" ] ~query:[ ("did", did) ]

(* An account by its verified handle, or by its DID while it has none. *)
let name_of t did =
  match Identity.handle t.identity did with Some h -> "@" ^ h | None -> did

let who t did = S.person ~avatar:(avatar_url did) (name_of t did)

let account s =
  S.account
    ~avatar:(avatar_url (Session.subject s))
    ~name:(Session.data s)
    ~logout:(S.Url.path [ "ui"; "logout" ])
    ~csrf:(Session.csrf_field, Session.csrf_token s)
    ()

(* A page for someone logged in shows their account, and to admins the jobs
   tab. *)
let page t ?session ?(current = "/ui") ~title body =
  let site, account =
    match session with
    | Some s ->
        ( (if Catalog.admin t.catalog (Session.subject s) then t.admin_site
           else t.site),
          Some (account s) )
    | None -> (t.site, None)
  in
  S.page site ~current ?account ~title (S.state_sprite :: body)

let paragraph s = Htmlit.El.p [ S.text s ]

let message t (respond @ local) ~status ~title text =
  html ~status respond (page t ~title [ paragraph text ])

(* {1 Logging in} *)

let login_page ?error t (respond @ local) =
  html respond
    (page t ~title:"Log in"
       ((match error with
        | Some e -> [ Htmlit.El.p [ S.badge ~tone:`Danger "Error"; S.text (" " ^ e) ] ]
        | None -> [])
       @ [
           paragraph
             "Log in with your atproto account, such as your Bluesky handle, \
              to see the pipelines of the repositories you use this spindle \
              for. You can also give your account's server, such as \
              https://bsky.social, to choose the account there.";
           S.post_form
             ~action:(S.Url.path [ "ui"; "login" ])
             ~hidden:[] ~submit:"Log in"
             [
               S.input ~name:"handle" ~label:"Handle, DID or server address"
                 ~value:"" ();
             ];
         ]))

(* A login holds one of [max_logins] places while it talks to servers, and
   gives up after [login_deadline] seconds. *)
let limited t f =
  if t.logins >= max_logins then None
  else (
    t.logins <- t.logins + 1;
    Fun.protect ~finally:(fun () -> t.logins <- t.logins - 1) @@ fun () ->
    match Eio.Time.with_timeout t.clock login_deadline (fun () -> Ok (f ())) with
    | Ok r -> Some r
    | Error `Timeout -> Some (Error `Timeout))

let busy = "The spindle is busy with other logins. Try again in a minute."

let login t (req @ local) (respond @ local) =
  (* A login form posted from another site could log the browser in to an
     account the visitor did not choose. *)
  if Session.request_origin t.sessions req = `Cross then raise Forbidden;
  let identifier =
    match Req.form_param req "handle" with
    | Some h ->
        let h = String.trim h in
        if String.length h > 0 && h.[0] = '@' then
          String.sub h 1 (String.length h - 1)
        else h
    | None -> ""
  in
  (* An account's server may be named instead, as https://HOST, since a bare
     host name is read as a handle. *)
  let server =
    String.starts_with ~prefix:"https://" identifier
    && Option.is_some (S.Url.external_ identifier)
  in
  if
    not
      (Atp.Handle.is_valid identifier || Atp.Did.is_valid identifier || server)
  then
    login_page t respond
      ~error:"That is not a handle, a DID or an https:// server address."
  else if List.length (Store.list t.store "web-login") >= max_pending then (
    sweep t;
    login_page t respond ~error:busy)
  else
    match
      limited t (fun () ->
          (Oauth.start ~clock:t.clock ~http:t.http ?plc:t.plc t.client
             ~identifier
            :> (_, [ Oauth.error | `Timeout ]) result))
    with
    | None -> login_page t respond ~error:busy
    | Some (Error e) ->
        (* Details of other servers stay in the log, not on the page. *)
        Printf.eprintf "spindle web login %s: %s\n%!" identifier
          (match e with
          | `Timeout -> "timed out"
          | #Oauth.error as e -> Oauth.error_to_string e);
        login_page t respond
          ~error:
            "The login could not start. Check the handle, or try again \
             later."
    | Some (Ok (pending, url)) -> (
        match S.Url.external_ url with
        | None -> login_page t respond ~error:"The account's server is unusable."
        | Some link ->
            let nonce, cookie = Session.Login.start t.sessions in
            Store.put t.store "web-login"
              (state_key (Oauth.pending_state pending))
              (encode login_jsont { pending; nonce = Session.Login.hash nonce });
            (* A redirect after a form may only go to this site, under the
               pages' form-action policy, so the browser follows a link. *)
            html ~cookies:[ cookie ] respond
              (page t ~title:"Log in"
                 [
                   paragraph "Continuing to your account's login page.";
                   S.follow link "Continue";
                 ]))

let failed t (respond @ local) text =
  html ~status:Status.Bad_request
    ~cookies:[ Session.Login.clear t.sessions ]
    respond
    (page t ~title:"Log in"
       [ paragraph text; S.link (S.Url.path [ "ui" ]) "Try again" ])

let callback t (req @ local) (respond @ local) =
  match Req.query_param req "state" with
  | None -> failed t respond "This is not a login."
  | Some state -> (
      let key = state_key state in
      (* A login is finished at most once, even by two callbacks at once. *)
      let stored =
        Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
            let stored = Store.get t.store "web-login" key in
            Store.delete t.store "web-login" key;
            stored)
      in
      match Option.bind stored (decode login_jsont) with
      | None -> failed t respond "This login has expired or was used already."
      | Some l -> (
          if not (Session.Login.matches t.sessions req ~hash:l.nonce) then
            failed t respond "This login was started in another browser."
          else
            let query = Xrpc_auth_protocol.encode_query (Req.query req) in
            match
              limited t (fun () ->
                  (Oauth.finish ~clock:t.clock ~http:t.http ?plc:t.plc
                     t.client l.pending ~query
                    :> (_, [ Oauth.error | `Timeout ]) result))
            with
            | None -> failed t respond busy
            | Some (Error e) ->
                Printf.eprintf "spindle web callback: %s\n%!"
                  (match e with
                  | `Timeout -> "timed out"
                  | #Oauth.error as e -> Oauth.error_to_string e);
                failed t respond "The login failed. Try again."
            | Some (Ok identity) ->
                if not (user t identity.did) then
                  failed t respond
                    "This spindle has no repositories of yours or that you \
                     collaborate on, so there is nothing for you to see."
                else
                  (* A new login ends whatever session the browser had. *)
                  let ended =
                    match Session.find t.sessions req with
                    | Some old -> [ Session.destroy t.sessions old ]
                    | None -> []
                  in
                  Option.iter
                    (Identity.remember t.identity identity.did)
                    identity.handle;
                  let _, cookie =
                    Session.create t.sessions ~subject:identity.did
                      ~data:
                        (match identity.handle with
                        | Some h -> "@" ^ h
                        | None -> identity.did)
                      ()
                  in
                  (* The new cookie comes last, so that it wins over the
                     cleared one of the same name. *)
                  see_other
                    ~cookies:(ended @ [ Session.Login.clear t.sessions; cookie ])
                    respond "/ui"))

let logout t s (req @ local) (respond @ local) =
  (* Logging out changes state, so only a POST from this site with the
     session's token does it. *)
  if
    Req.meth req <> Httpz.Method.Post
    || Session.request_origin t.sessions req = `Cross
    || not (Session.check_csrf s req)
  then raise Forbidden;
  see_other ~cookies:[ Session.destroy t.sessions s ] respond "/ui"

(* {1 Pages} *)

let field k j =
  match j with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem k members with
      | Some (_, v) -> Some v
      | None -> None)
  | _ -> None

let text_field k j =
  match field k j with Some (Jsont.String (s, _)) -> Some s | _ -> None

let list_field k j =
  match field k j with Some (Jsont.Array (l, _)) -> l | _ -> []

(* A pipeline's status, as Tangled's pages show it. *)
let aggregate statuses =
  if statuses = [] then "pending"
  else
    match
      List.find_opt
        (fun s -> List.mem s statuses)
        [ "running"; "pending"; "blocked"; "failed"; "timeout"; "cancelled" ]
    with
    | Some s -> s
    | None -> "success"

(* Sitekit's icons name states as its job pages do. *)
let icon = function
  | "success" -> "done"
  | "pending" -> "queued"
  | "timeout" -> "failed"
  | s -> s

let status s = S.state ~label:s (icon s)
let short sha = if String.length sha > 7 then String.sub sha 0 7 else sha

let trigger j =
  match field "trigger" j with
  | None -> ""
  | Some t -> (
      let kind =
        match text_field "$type" t with
        | Some "org.tangled.event.push" -> "push"
        | Some "org.tangled.event.pullRequest" -> "pull request"
        | Some "org.tangled.ci.trigger.manual" -> "manual"
        | Some k -> k
        | None -> ""
      in
      match (text_field "ref" t, text_field "sourceBranch" t) with
      | Some r, _ | None, Some r -> kind ^ " " ^ r
      | None, None -> kind)

let repo_name t did =
  match Catalog.managed t.catalog did with
  | Some r when r.name <> "" -> r.name
  | _ -> did
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception _ -> did

(* The user's own repositories that this spindle does not serve, from the
   declarations in their PDS and the manifests on their knots, kept for five
   minutes. *)
let elsewhere t user =
  let now = Eio.Time.now t.clock in
  match Hashtbl.find_opt t.elsewhere user with
  | Some (at, l) when now -. at < 300. -> l
  | _ ->
      let served = List.map fst (served t) in
      let repos =
        match
          Eio.Time.with_timeout t.clock 10. (fun () ->
              Ok
                (Network.records (Catalog.network t.catalog) user
                   "org.tangled.repo.declaration"))
        with
        | Ok l ->
            List.filter_map
              (fun r -> Option.bind (field "value" r) (text_field "repo"))
              l
        | Error `Timeout -> []
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception _ -> []
      in
      let l =
        Eio.Fiber.List.filter_map ~max_fibers:8
          (fun repo ->
            if List.mem repo served || not (Atp.Did.is_valid repo) then None
            else
              match
                Eio.Time.with_timeout t.clock 5. (fun () ->
                    Ok (Catalog.manifest t.catalog repo))
              with
              | Ok (Some (name, spindle)) -> Some (repo, name, spindle)
              | Ok None | Error `Timeout -> None
              | exception (Eio.Cancel.Cancelled _ as e) -> raise e
              | exception _ -> None)
          (List.sort_uniq String.compare repos)
      in
      if Hashtbl.length t.elsewhere > 1000 then Hashtbl.reset t.elsewhere;
      Hashtbl.replace t.elsewhere user (now, l);
      l

(* Whether [v] holds [sub], which is lower case, ignoring case. *)
let rec contains (v @ local) sub i =
  let n = String.length v and m = String.length sub in
  let rec matches k =
    k >= m
    || (Char.lowercase_ascii (String.get v (i + k)) = String.get sub k
       && matches (k + 1))
  in
  i + m <= n && (matches 0 || contains v sub (i + 1))

(* A browser asking for the root page wants the pages. *)
let wants_html (req @ local) =
  match Req.header_single req "Accept" with
  | Ok (Some v) -> contains v "text/html" 0 [@nontail]
  | Ok None | Error `Repeated -> false

let home t s (respond @ local) =
  let user = Session.subject s in
  let seen = visible t ~user in
  Identity.handles t.identity (List.map snd seen);
  let rows =
    List.map
      (fun (repo, owner) ->
        let latest =
          match
            list_field "pipelines"
              (Engine.query t.engine ~repo ~limit:1 ~cursor:None ~triggers:[]
                 ~commits:[])
          with
          | p :: _ ->
              [
                status
                  (aggregate
                     (List.filter_map (text_field "status")
                        (list_field "workflows" p)));
              ]
          | [] -> [ S.text "none yet" ]
        in
        [
          S.cell
            [
              S.link
                (S.Url.path [ "ui"; "repo" ] ~query:[ ("did", repo) ])
                (repo_name t repo);
            ];
          S.cell ~sort:(name_of t owner) [ who t owner ];
          S.cell latest;
        ])
      seen
  in
  let others = elsewhere t user in
  html respond
    (page t ~session:s ~title:"Repositories"
       ((if rows = [] then
           [ paragraph "None of your repositories run CI on this spindle yet." ]
         else
           [
             S.table
               [ ("Repository", `Text); ("Owner", `Text); ("Latest", `Text) ]
               rows;
           ])
       @
       if others = [] then []
       else
         [
           Htmlit.El.h2 [ S.text "Your other repositories" ];
           paragraph
             "A repository runs CI on the one spindle its settings name. To \
              move one here, run the command beside it with the Tangled CLI.";
           S.table
             [ ("Repository", `Text); ("Spindle", `Text); ("Move it here", `Text) ]
             (List.map
                (fun (_, name, spindle) ->
                  [
                    S.cell [ S.text name ];
                    S.cell
                      [
                        S.text
                          (match spindle with
                          | Some s when String.starts_with ~prefix:"did:web:" s
                            ->
                              String.sub s 8 (String.length s - 8)
                          | Some s -> s
                          | None -> "none");
                      ];
                    S.cell
                      [
                        Htmlit.El.code
                          [
                            S.text
                              (Printf.sprintf
                                 "tangled repo set-spindle -r %s/%s %s"
                                 (match Identity.handle t.identity user with
                                 | Some h -> h
                                 | None -> user)
                                 name t.hostname);
                          ];
                      ];
                  ])
                others);
         ]))

let required (req @ local) name =
  match Req.query_param req name with Some v -> v | None -> raise Missing

(* A profile picture from the account's own PDS, served from here, so that
   pages load nothing from elsewhere. Its type is one of a few image types,
   and it is never sniffed as anything else. *)
let avatar t (req @ local) (respond @ local) =
  let did = required req "did" in
  match Identity.avatar t.identity did with
  | None -> raise Missing
  | Some (mime, bytes) ->
      Resp.media respond
        ~headers:
          (Headers.of_list
             [
               ("Cache-Control", "private, max-age=3600");
               ("X-Content-Type-Options", "nosniff");
               ("Content-Security-Policy", "default-src 'none'; sandbox");
               ("Cross-Origin-Resource-Policy", "same-origin");
             ])
        mime bytes

let repo t s (req @ local) (respond @ local) =
  let repo = required req "did" in
  if not (can_see t ~user:(Session.subject s) repo) then raise Forbidden;
  let cursor = Req.query_param req "cursor" in
  let result =
    Engine.query t.engine ~repo ~limit:30 ~cursor ~triggers:[] ~commits:[]
  in
  let pipelines = list_field "pipelines" result in
  let rows =
    List.map
      (fun p ->
        let id = Option.value ~default:"" (text_field "id" p) in
        let statuses =
          List.filter_map (text_field "status") (list_field "workflows" p)
        in
        [
          S.cell
            [
              S.link
                (S.Url.path [ "ui"; "pipeline" ]
                   ~query:[ ("repo", repo); ("id", id) ])
                id;
            ];
          S.cell [ status (aggregate statuses) ];
          S.cell [ S.text (trigger p) ];
          S.cell
            [ S.text (short (Option.value ~default:"" (text_field "commit" p))) ];
          S.cell [ S.text (Option.value ~default:"" (text_field "createdAt" p)) ];
          S.cell ~sort:(string_of_int (List.length statuses))
            [ S.text (string_of_int (List.length statuses)) ];
        ])
      pipelines
  in
  let next =
    match text_field "cursor" result with
    | Some c ->
        [
          S.next_page
            (S.Url.path [ "ui"; "repo" ]
               ~query:[ ("did", repo); ("cursor", c) ])
            "Older pipelines →";
        ]
    | None -> []
  in
  html respond
    (page t ~session:s ~title:(repo_name t repo)
       ((if rows = [] then [ paragraph "No pipelines yet." ]
         else
           [
             S.table
               [
                 ("Pipeline", `Text);
                 ("Status", `Text);
                 ("Trigger", `Text);
                 ("Commit", `Text);
                 ("Created", `Text);
                 ("Workflows", `Number);
               ]
               rows;
           ])
       @ next))

(* The pipeline [id] of [repo], if the session may see it. *)
let pipeline_of t s (req @ local) =
  let repo = required req "repo" and id = required req "id" in
  if not (can_see t ~user:(Session.subject s) repo) then raise Forbidden;
  match Engine.find t.engine id with
  | Some p when p.repo = repo -> p
  | _ -> raise Missing

let pipeline t s (req @ local) (respond @ local) =
  let p = pipeline_of t s req in
  let rows =
    List.map
      (fun (w : Runner.t) ->
        [
          S.cell
            [
              S.link
                (S.Url.path [ "ui"; "log" ]
                   ~query:
                     [
                       ("repo", p.repo); ("id", p.id); ("workflow", w.job.name);
                     ])
                w.job.name;
            ];
          S.cell [ status w.status ];
          S.cell [ S.text (Option.value ~default:"" w.started) ];
          S.cell [ S.text (Option.value ~default:"" w.finished) ];
          S.cell [ S.text (Option.value ~default:"" w.error) ];
        ])
      p.workflows
  in
  html respond
    (page t ~session:s ~title:("Pipeline " ^ p.id)
       [
         S.facts
           [
             ( "Repository",
               Some
                 (S.link
                    (S.Url.path [ "ui"; "repo" ] ~query:[ ("did", p.repo) ])
                    (repo_name t p.repo)) );
             ("Commit", Some (S.text p.commit));
             ("Created", Some (S.text p.created));
           ];
         S.table
           [
             ("Workflow", `Text);
             ("Status", `Text);
             ("Started", `Text);
             ("Finished", `Text);
             ("Error", `Text);
           ]
           rows;
       ])

let log t s (req @ local) (respond @ local) =
  let p = pipeline_of t s req in
  let name = required req "workflow" in
  match List.find_opt (fun (w : Runner.t) -> w.job.name = name) p.workflows with
  | None -> raise Missing
  | Some w ->
      html respond
        (page t ~session:s ~title:name
           [
             S.facts
               [
                 ( "Pipeline",
                   Some
                     (S.link
                        (S.Url.path [ "ui"; "pipeline" ]
                           ~query:[ ("repo", p.repo); ("id", p.id) ])
                        p.id) );
                 ("Status", Some (status w.status));
                 ("Error", Option.map S.text w.error);
               ];
             S.terminal (Runner.outcome w).log;
           ])

(* {1 Jobs} *)

(* The Duraml queue holds every repository's work, so only admins see it. *)
let jobs_of t s =
  if not (admin t (Session.subject s)) then raise Forbidden;
  match t.jobs with Some j -> j | None -> raise Missing

let state_name = function
  | Duraml.Queued -> "queued"
  | Running -> "running"
  | Done -> "done"
  | Failed_job -> "failed"
  | Cancelled -> "cancelled"

let states =
  Duraml.[ Queued; Running; Done; Failed_job; Cancelled ]

let state_of_string v =
  match List.find_opt (fun st -> state_name st = v) states with
  | Some st -> st
  | None -> raise Missing

(* What an event did to its job, as a state icon. *)
let event_icon = function
  | "enqueued" | "retry_scheduled" -> "queued"
  | "claimed" -> "running"
  | "succeeded" -> "done"
  | "failed" | "lease_expired" -> "failed"
  | "cancelled" -> "cancelled"
  | _ -> "notice"

let job_link id =
  S.link
    (S.Url.path [ "ui"; "job" ] ~query:[ ("id", Duraml.Id.to_string id) ])
    (Duraml.Id.to_string id)

(* A workflow job's key names its pipeline and workflow, and its lane is the
   repository, which together name the workflow's log. *)
let workflow_link spec =
  match Option.map (String.split_on_char '/') (Duraml.Spec.key spec) with
  | Some [ "workflow"; id; name ] ->
      S.link
        (S.Url.path [ "ui"; "log" ]
           ~query:
             [ ("repo", Duraml.Spec.lane spec); ("id", id); ("workflow", name) ])
        (name ^ " of " ^ id)
  | _ -> S.text (Option.value ~default:"" (Duraml.Spec.key spec))

let outcome_text = function
  | None -> ""
  | Some (Duraml.Succeeded _) -> "succeeded"
  | Some (Retry { at; reason }) -> "retry at " ^ S.time at ^ ": " ^ reason
  | Some (Failed { code; detail }) -> code ^ ": " ^ detail

let job_row (j : Duraml.job) =
  let spec = Duraml.Job.spec j in
  [
    S.cell ~sort:(Duraml.Id.to_string (Duraml.Job.id j)) [ job_link (Duraml.Job.id j) ];
    S.cell [ S.state (state_name (Duraml.Job.state j)) ];
    S.cell [ workflow_link spec ];
    S.cell [ S.text (string_of_int (Duraml.Job.attempt j)) ];
    S.cell [ S.text (S.time (Duraml.Job.updated_at j)) ];
    S.cell [ S.text (outcome_text (Duraml.Job.outcome j)) ];
  ]

let job_columns =
  [
    ("Job", `Number);
    ("State", `Text);
    ("Workflow", `Text);
    ("Attempt", `Number);
    ("Updated", `Text);
    ("Outcome", `Text);
  ]

let event_rows events =
  List.map
    (fun e ->
      [
        S.cell [ S.text (S.time (Duraml.Event.time e)) ];
        S.cell
          [
            S.state ~label:(Duraml.Event.kind e)
              (event_icon (Duraml.Event.kind e));
          ];
        S.cell [ job_link (Duraml.Event.job e) ];
        S.cell [ S.text (string_of_int (Duraml.Event.attempt e)) ];
        S.cell [ S.text (Duraml.Event.detail e) ];
      ])
    events

let event_columns =
  [
    ("Time", `Text);
    ("Event", `Text);
    ("Job", `Number);
    ("Attempt", `Number);
    ("Detail", `Text);
  ]

let jobs_page t s (req @ local) (respond @ local) =
  let store = jobs_of t s in
  let state = Option.map state_of_string (Req.query_param req "state") in
  let after =
    Option.map
      (fun a ->
        match Duraml.Id.of_string a with Ok id -> id | Error _ -> raise Missing)
      (Req.query_param req "after")
  in
  let count st = Duraml.count store (Duraml.query ~state:st ()) in
  let metrics =
    S.metrics
      (List.map
         (fun st ->
           S.metric ~label:(state_name st)
             (S.link
                (S.Url.path [ "ui"; "jobs" ] ~query:[ ("state", state_name st) ])
                (S.count (count st))))
         states)
  in
  let listing =
    match state with
    | None ->
        (* Without a filter, what is happening now. *)
        let running =
          Duraml.list store (Duraml.query ~state:Running ~limit:200 ())
        in
        [
          Htmlit.El.h2 [ S.text "Running" ];
          (if running = [] then paragraph "Nothing is running."
           else S.table job_columns (List.map job_row running));
        ]
    | Some st ->
        let jobs = Duraml.list store (Duraml.query ~state:st ?after ~limit:100 ()) in
        [
          Htmlit.El.h2 [ S.text (String.capitalize_ascii (state_name st)) ];
          (if jobs = [] then paragraph "No jobs."
           else S.table job_columns (List.map job_row jobs));
        ]
        @ (match List.rev jobs with
          | last :: _ when List.length jobs = 100 ->
              [
                S.next_page
                  (S.Url.path [ "ui"; "jobs" ]
                     ~query:
                       [
                         ("state", state_name st);
                         ("after", Duraml.Id.to_string (Duraml.Job.id last));
                       ])
                  "Later jobs →";
              ]
          | _ -> [])
  in
  let recent = Duraml.tail ~limit:100 store in
  html respond
    (page t ~session:s ~current:"/ui/jobs" ~title:"Jobs"
       ([ metrics ] @ listing
       @ [
           Htmlit.El.h2 [ S.text "Recent activity" ];
           S.table event_columns (event_rows recent);
         ]))

let job_page t s (req @ local) (respond @ local) =
  let store = jobs_of t s in
  let id =
    match Duraml.Id.of_string (required req "id") with
    | Ok id -> id
    | Error _ -> raise Missing
  in
  match Duraml.find store id with
  | None -> raise Missing
  | Some j ->
      let spec = Duraml.Job.spec j in
      let events =
        List.rev
          (List.filter
             (fun e -> Duraml.Id.compare (Duraml.Event.job e) id = 0)
             (Duraml.tail ~limit:1000 store))
      in
      html respond
        (page t ~session:s ~current:"/ui/jobs"
           ~title:("Job " ^ Duraml.Id.to_string id)
           [
             S.facts
               [
                 ("State", Some (S.state (state_name (Duraml.Job.state j))));
                 ("Workflow", Some (workflow_link spec));
                 ("Queue", Some (S.text (Duraml.Spec.queue spec)));
                 ("Repository", Some (S.text (Duraml.Spec.lane spec)));
                 ("Attempt", Some (S.text (string_of_int (Duraml.Job.attempt j))));
                 ("Created", Some (S.text (S.time (Duraml.Job.created_at j))));
                 ("Updated", Some (S.text (S.time (Duraml.Job.updated_at j))));
                 ( (if Duraml.Job.state j = Running then "Lease until"
                    else "Available"),
                   Some (S.text (S.time (Duraml.Job.available_at j))) );
                 ( "Outcome",
                   match outcome_text (Duraml.Job.outcome j) with
                   | "" -> None
                   | o -> Some (S.text o) );
               ];
             Htmlit.El.h2 [ S.text "Timeline" ];
             (if events = [] then
                paragraph "No events among the newest thousand."
              else S.table event_columns (event_rows events));
           ])

(* {1 Members} *)

(* Members are the accounts whose repositories may run CI here, the
   allow-list that the spindle's XRPC methods also change. The handle a member
   was added by is kept to show beside the DID, and is not checked again. *)

let require_admin t s =
  if not (admin t (Session.subject s)) then raise Forbidden

(* A change from a form needs a POST from this site with the session's
   token. *)
let check_form t s (req @ local) =
  if
    Req.meth req <> Httpz.Method.Post
    || Session.request_origin t.sessions req = `Cross
    || not (Session.check_csrf s req)
  then raise Forbidden

let members_page ?error t s (respond @ local) =
  require_admin t s;
  let csrf = (Session.csrf_field, Session.csrf_token s) in
  let rows =
    List.map
      (fun did ->
        [
          S.cell ~sort:(name_of t did) [ who t did ];
          S.cell [ S.text did ];
          S.cell
            [
              S.post_button
                ~action:(S.Url.path [ "ui"; "members"; "remove" ])
                ~hidden:[ csrf; ("did", did) ]
                ~label:"Remove";
            ];
        ])
      (Catalog.allowed t.catalog)
  in
  html respond
    (page t ~session:s ~current:"/ui/members" ~title:"Members"
       ((match error with
        | Some e ->
            [ Htmlit.El.p [ S.badge ~tone:`Danger "Error"; S.text (" " ^ e) ] ]
        | None -> [])
       @ [
           paragraph
             "Members' repositories may run CI on this spindle, and members \
              may log in here to see them.";
           S.post_form
             ~action:(S.Url.path [ "ui"; "members"; "add" ])
             ~hidden:[ csrf ] ~submit:"Add member"
             [ S.input ~name:"handle" ~label:"Handle or DID" ~value:"" () ];
           (if rows = [] then paragraph "There are no members yet."
            else
              S.table
                [ ("Member", `Text); ("DID", `Text); ("", `Text) ]
                rows);
         ]))

let add_member t s (req @ local) (respond @ local) =
  require_admin t s;
  check_form t s req;
  let identifier =
    match Req.form_param req "handle" with
    | Some h ->
        let h = String.trim h in
        if String.length h > 0 && h.[0] = '@' then
          String.sub h 1 (String.length h - 1)
        else h
    | None -> ""
  in
  let resolved =
    if Atp.Did.is_valid identifier then Ok (identifier, None)
    else if Atp.Handle.is_valid identifier then
      match
        Eio.Time.with_timeout t.clock 15. (fun () ->
            Ok (Xrpc.Identity.did_of_handle t.http identifier))
      with
      | Ok (Ok did) -> Ok (did, Some (String.lowercase_ascii identifier))
      | Ok (Error e) -> Error e
      | Error `Timeout -> Error "resolving it took too long"
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e -> Error (Printexc.to_string e)
    else Error "that is not a handle or a DID"
  in
  match resolved with
  | Error e ->
      Printf.eprintf "spindle web member %s: %s\n%!" identifier e;
      members_page t s respond
        ~error:("Could not add " ^ identifier ^ ": the handle does not resolve.")
  | Ok (did, handle) ->
      Catalog.allow t.catalog did;
      (match handle with
      | Some h -> Store.put t.store "web-member" did ("@" ^ h)
      | None -> ());
      Printf.eprintf "spindle web: %s added member %s\n%!" (Session.subject s)
        did;
      see_other respond "/ui/members"

let remove_member t s (req @ local) (respond @ local) =
  require_admin t s;
  check_form t s req;
  match Req.form_param req "did" with
  | Some did when Atp.Did.is_valid did ->
      Catalog.disallow t.catalog did;
      Store.delete t.store "web-member" did;
      Printf.eprintf "spindle web: %s removed member %s\n%!"
        (Session.subject s) did;
      see_other respond "/ui/members"
  | _ -> raise Missing

let handle t name (req @ local) (respond @ local) =
  try
    match name with
    | "metadata" -> json respond (Oauth.Client.metadata t.client)
    | "jwks" -> json respond (Oauth.Client.jwks t.client)
    | "login" -> login t req respond
    | "callback" -> callback t req respond
    | _ -> (
        match Session.find t.sessions req with
        | None ->
            if name = "logout" then see_other respond "/ui"
            else login_page t respond
        | Some s -> (
            match name with
            | "home" -> home t s respond
            | "repo" -> repo t s req respond
            | "pipeline" -> pipeline t s req respond
            | "log" -> log t s req respond
            | "logout" -> logout t s req respond
            | "jobs" -> jobs_page t s req respond
            | "job" -> job_page t s req respond
            | "members" -> members_page t s respond
            | "member-add" -> add_member t s req respond
            | "member-remove" -> remove_member t s req respond
            | "avatar" -> avatar t req respond
            | _ -> raise Missing))
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Forbidden ->
      message t respond ~status:Status.Forbidden ~title:"Not allowed"
        "You may not see this."
  | Missing ->
      message t respond ~status:Status.Not_found ~title:"Not found"
        "There is nothing here."
  | e ->
      Printf.eprintf "spindle web %s: %s\n%!" name (Printexc.to_string e);
      message t respond ~status:Status.Internal_server_error
        ~title:"Something went wrong" "The spindle could not show this page."

let routes () =
  let open Route in
  Sitekit.Asset.routes [ Sitekit.Asset.stylesheet; Sitekit.Asset.script ]
  @ [
      get (s "ui") (fun env req respond -> env "web:home" req respond);
      get (s "ui" / s "repo") (fun env req respond -> env "web:repo" req respond);
      get
        (s "ui" / s "pipeline")
        (fun env req respond -> env "web:pipeline" req respond);
      get (s "ui" / s "log") (fun env req respond -> env "web:log" req respond);
      get (s "ui" / s "jobs") (fun env req respond -> env "web:jobs" req respond);
      get
        (s "ui" / s "avatar")
        (fun env req respond -> env "web:avatar" req respond);
      get (s "ui" / s "job") (fun env req respond -> env "web:job" req respond);
      get
        (s "ui" / s "members")
        (fun env req respond -> env "web:members" req respond);
      post
        (s "ui" / s "members" / s "add")
        (fun env req respond -> env "web:member-add" req respond);
      post
        (s "ui" / s "members" / s "remove")
        (fun env req respond -> env "web:member-remove" req respond);
      post
        (s "ui" / s "login")
        (fun env req respond -> env "web:login" req respond);
      post
        (s "ui" / s "logout")
        (fun env req respond -> env "web:logout" req respond);
      get
        (s "oauth" / s "callback")
        (fun env req respond -> env "web:callback" req respond);
      get
        (s "oauth-client-metadata.json")
        (fun env req respond -> env "web:metadata" req respond);
      get
        (s "oauth" / s "jwks.json")
        (fun env req respond -> env "web:jwks" req respond);
    ]

let login_as ?handle t did =
  Option.iter (Identity.remember t.identity did) handle;
  let data = match handle with Some h -> "@" ^ h | None -> did in
  let s, cookie = Session.create t.sessions ~subject:did ~data () in
  let value =
    match String.index_opt cookie.value ';' with
    | Some i -> String.sub cookie.value 0 i
    | None -> cookie.value
  in
  (value, Session.csrf_token s)
