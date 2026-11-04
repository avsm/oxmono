(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let app_name = "tangled"

(* Common options *)

let pds =
  let doc =
    "The address of your account's server. Give it to log in with a password \
     to an account hosted outside Bluesky."
  in
  let env = Cmd.Env.info "TANGLED_PDS" in
  Arg.(value & opt (some string) None & info [ "pds" ] ~env ~docv:"URL" ~doc)

(* Auth login command *)

let handle_arg =
  let doc = "Log in as HANDLE, such as $(i,alice.bsky.social)." in
  Arg.(
    value & opt (some string) None & info [ "handle"; "u" ] ~docv:"HANDLE" ~doc)

let password_arg =
  let doc =
    "The password. Prefer an app password, which you can revoke on its own. \
     Without this option you are asked for it."
  in
  let env = Cmd.Env.info "TANGLED_PASSWORD" in
  Arg.(
    value
    & opt (some string) None
    & info [ "password"; "p" ] ~env ~docv:"PASSWORD" ~doc)

let save_password_arg =
  let doc =
    "Save the password, readable only by you, so that later logins need not \
     ask for it."
  in
  Arg.(value & flag & info [ "save-password" ] ~doc)

let oauth_arg =
  let doc =
    "Log in through your browser instead of with a password. The browser ends \
     on a page showing a line to paste back here. Commands that are new since \
     your last OAuth login may need you to log in again."
  in
  Arg.(value & flag & info [ "oauth" ] ~doc)

let loopback_arg =
  let doc =
    "With $(b,--oauth), have the browser hand the login straight back to this \
     command instead of showing a line to paste. The browser must run on this \
     computer."
  in
  Arg.(value & flag & info [ "loopback" ] ~doc)

let client_id_arg =
  let doc =
    "With $(b,--oauth), identify this program by the client metadata published \
     at URL instead of the Tangled CLI's own."
  in
  Arg.(value & opt (some string) None & info [ "client-id" ] ~docv:"URL" ~doc)

let redirect_uri_arg =
  let doc =
    "The page, named in the $(b,--client-id) metadata, that shows the line to \
     paste."
  in
  Arg.(
    value & opt (some string) None & info [ "redirect-uri" ] ~docv:"URL" ~doc)

let scope_arg =
  let doc =
    "The access to ask for. By default, only what Tangled commands need."
  in
  Arg.(value & opt (some string) None & info [ "scope" ] ~docv:"SCOPE" ~doc)

let no_browser_arg =
  let doc = "Print the login address instead of opening a browser." in
  Arg.(value & flag & info [ "no-browser" ] ~doc)

(* Record collections the CLI may write, and the methods it needs service
   tokens for. Legacy sh.tangled collections are read-only here. Matrix
   bindings are this CLI's own collection, outside Tangled's lexicons. *)
let records () =
  List.filter_map
    (fun (nsid, _) ->
      if
        String.starts_with ~prefix:"org.tangled." nsid
        && Tangled.Schema.kind nsid = "record"
      then Some nsid
      else None)
    Tangled.Schema.documents
  @ [ Tangled.Matrix.nsid ]

(* A loopback client ID carries its scope, so keep it compact. *)
let loopback_scope () =
  String.concat " "
    [
      "atproto";
      "repo?"
      ^ String.concat "&" (List.map (( ^ ) "collection=") (records ()));
      "rpc?"
      ^ String.concat "&"
          (List.map (fun m -> "lxm=" ^ m) Tangled.Api.service_methods)
      ^ "&aud=*";
    ]

(* One scope per grant, as Tangled's own published client lists them. A
   server grants only scopes the metadata declares, token by token, so the
   paste flow must request exactly this string. *)
let hosted_scope () =
  String.concat " "
    (("atproto" :: List.map (( ^ ) "repo:") (records ()))
    @ List.map (fun m -> "rpc:" ^ m ^ "?aud=*") Tangled.Api.service_methods)

let tangled_client_id =
  "https://www.cl.cam.ac.uk/~avsm2/tangled-oauth-client.json"

let client_metadata ~client_id ~redirect_uri =
  let s = Common.string in
  let strings l = Jsont.Json.list (List.map s l) in
  Common.object_
    [
      ("client_id", s client_id);
      ("client_name", s "Tangled CLI (paste mode)");
      ("client_uri", s "https://www.cl.cam.ac.uk/~avsm2/");
      ("application_type", s "web");
      ("grant_types", strings [ "authorization_code"; "refresh_token" ]);
      ("response_types", strings [ "code" ]);
      ("redirect_uris", strings [ redirect_uri ]);
      ("scope", s (hosted_scope ()));
      ("token_endpoint_auth_method", s "none");
      ("dpop_bound_access_tokens", Jsont.Json.bool true);
    ]

let open_url ~no_browser url =
  if Ui.tty () then
    Ui.print
      (Ui.V.vcat
         [
           Ui.V.concat
             [
               Ui.V.role `Accent (Ui.text "→ ");
               Ui.bold "Open this link to sign in";
               Ui.muted
                 (if no_browser then "  (works once, for five minutes)"
                  else "  (opening your browser; open it only once)");
             ];
           Ui.V.concat [ Ui.text "  "; Ui.link url ];
         ])
  else Fmt.pr "Open this URL to sign in:@.  %s@." url;
  if not no_browser then
    let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
    let spawn prog =
      ignore (Unix.create_process prog [| prog; url |] null null null)
    in
    Fun.protect
      ~finally:(fun () -> Unix.close null)
      (fun () ->
        try spawn "xdg-open"
        with Unix.Unix_error _ -> (
          try spawn "open" with Unix.Unix_error _ -> ()))

let prompt_or label = function
  | Some v -> v
  | None ->
      if Ui.tty () then
        Fmt.pr "%a %a @?" Ui.V.pp
          (Ui.V.role `Accent (Ui.text "?"))
          Ui.V.pp
          (Ui.bold (label ^ ":"))
      else Fmt.pr "%s: @?" label;
      if label = "Password" && Unix.isatty Unix.stdin then
        let original = Unix.tcgetattr Unix.stdin in
        Fun.protect
          ~finally:(fun () ->
            Unix.tcsetattr Unix.stdin Unix.TCSANOW original;
            Fmt.pr "@.")
          (fun () ->
            Unix.tcsetattr Unix.stdin Unix.TCSANOW
              { original with c_echo = false };
            read_line ())
      else read_line ()

let read_pasted () =
  if Ui.tty () then
    Fmt.pr "%a %a @?" Ui.V.pp
      (Ui.V.role `Accent (Ui.text "?"))
      Ui.V.pp
      (Ui.bold "Paste the line shown in the browser:")
  else Fmt.pr "Paste the line shown in the browser: @?";
  match In_channel.input_line stdin with
  | Some line -> line
  | None -> failwith "No authorization code was pasted"

let logged_in (session : Xrpc_auth.Session.t) =
  if Ui.tty () then Ui.ok "Logged in as %s (%s)" session.handle session.did
  else Fmt.pr "Logged in as %s (%s)@." session.handle session.did

let oauth_login ~pds ~handle ~scope ~no_browser ~loopback ~client_id
    ~redirect_uri env =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  let config = Tangled.Config.load fs xdg in
  let handle =
    match (handle, config, pds) with
    | Some h, _, _ -> Some h
    | None, _, Some _ -> None
    | None, Some c, None -> Some c.handle
    | None, None, None -> Some (prompt_or "Handle" None)
  in
  let http = Xrpc.Trace.wrap (Fetch_httpz.std env) in
  let plc =
    Option.value ~default:Tangled.Api.default_plc (Sys.getenv_opt "TANGLED_PLC")
  in
  let pds =
    match (handle, pds) with
    | Some handle, _ -> (
        let resolve =
          if String.starts_with ~prefix:"did:" handle then
            Xrpc.Identity.pds_of_did
          else Xrpc.Identity.pds_of_handle
        in
        match resolve ~plc http handle with
        | Ok pds -> pds
        | Error message -> failwith message)
    | None, Some pds -> pds
    | None, None -> assert false
  in
  let api = Common.create_api ~sw ~env ~app_name ~pds () in
  let redirect : Xrpc_auth.Oauth.redirect =
    if loopback then Loopback
    else
      Paste
        {
          client_id = Option.value ~default:tangled_client_id client_id;
          redirect_uri =
            Option.value ~default:Xrpc_auth.Oauth.hosted_redirect_uri
              redirect_uri;
          read = read_pasted;
        }
  in
  let scope =
    match (scope, redirect) with
    | Some s, _ -> s
    | None, Loopback -> loopback_scope ()
    | None, Paste _ when client_id = None -> hosted_scope ()
    | None, Paste _ -> Xrpc_auth.Oauth.default_scope
  in
  Tangled.Api.login_oauth api ~net:env#net ?identifier:handle ~redirect ~scope
    ~open_url:(open_url ~no_browser) ();
  match Tangled.Api.get_session api with
  | Some session ->
      Tangled.Config.save fs xdg { handle = session.handle; pds };
      Tangled.Config.clear_password fs xdg;
      logged_in session
  | None -> failwith "Login failed"

let login_action ~pds ~handle ~password ~save_password env =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  (* Try to get credentials from config if not provided *)
  let config = Tangled.Config.load fs xdg in
  let saved_password = Tangled.Config.load_password fs xdg in
  let pds =
    match (pds, config) with
    | Some pds, _ -> pds
    | None, Some config -> config.pds
    | None, None -> "https://bsky.social"
  in
  let api = Common.create_api ~sw ~env ~app_name ~pds () in

  let handle =
    match handle with
    | Some h -> h
    | None -> (
        match config with Some c -> c.handle | None -> prompt_or "Handle" None)
  in
  let password =
    match password with
    | Some p -> p
    | None -> (
        match saved_password with
        | Some p -> p
        | None -> prompt_or "Password" None)
  in

  Ui.busy ("Signing in as " ^ handle) (fun () ->
      Tangled.Api.login api ~identifier:handle ~password);

  match Tangled.Api.get_session api with
  | Some session ->
      (* Save config *)
      let config : Tangled.Config.t = { handle = session.handle; pds } in
      Tangled.Config.save fs xdg config;
      if save_password then Tangled.Config.save_password fs xdg password;
      logged_in session;
      if save_password then
        if Ui.tty () then Ui.ok "Password saved to config"
        else Fmt.pr "Password saved to config.@."
  | None -> failwith "Login failed"

let login_cmd =
  let doc = "Log in to your account." in
  let man =
    Common.man
      [
        "Logs in and saves the login for the commands that follow. With \
         $(b,--oauth), you approve the login in a browser and this command \
         never sees your password. Otherwise it logs in with a password, \
         preferably an app password made for it.";
        "Each login is saved under its handle. $(b,tangled auth account) \
         switches between them.";
      ]
  in
  let info = Cmd.info "login" ~doc ~man in
  let login' pds handle password save_password oauth scope no_browser loopback
      client_id redirect_uri =
    Common.run (fun env ->
        if oauth then begin
          if password <> None || save_password then
            invalid_arg "--oauth does not take a password";
          if loopback && (client_id <> None || redirect_uri <> None) then
            invalid_arg
              "--loopback does not take --client-id or --redirect-uri";
          oauth_login ~pds ~handle ~scope ~no_browser ~loopback ~client_id
            ~redirect_uri env
        end
        else login_action ~pds ~handle ~password ~save_password env)
  in
  Common.cmd info
    Term.(
      term_result
        (const login' $ pds $ handle_arg $ password_arg $ save_password_arg
       $ oauth_arg $ scope_arg $ no_browser_arg $ loopback_arg $ client_id_arg
       $ redirect_uri_arg))

(* Auth logout command *)

let logout_action env =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  Fun.protect
    ~finally:(fun () ->
      Xrpc_auth.Session.clear fs ~app_name ();
      Tangled.Config.clear_password fs xdg)
    (fun () ->
      match Xrpc_auth.Session.load fs ~app_name () with
      | None ->
          if Ui.tty () then Ui.print (Ui.muted "Not logged in.")
          else Fmt.pr "Not logged in.@."
      | Some session ->
          let api = Common.create_api ~sw ~env ~app_name ~pds:session.pds () in
          Tangled.Api.resume api ~session;
          Tangled.Api.logout api;
          if Ui.tty () then Ui.ok "Logged out of %s" session.handle
          else Fmt.pr "Logged out.@.")

let logout_cmd =
  let doc = "Log out and forget the saved login." in
  let info = Cmd.info "logout" ~doc in
  let logout' () = Common.run logout_action in
  Common.cmd info Term.(term_result (const logout' $ const ()))

(* Auth status command *)

(* [access scope] says in a few words what an OAuth scope grants. *)
let access scope =
  let tokens = String.split_on_char ' ' scope in
  let count prefix =
    List.length (List.filter (String.starts_with ~prefix) tokens)
  in
  if List.mem "transition:generic" tokens then
    Ui.V.role `Warning (Ui.text "whole account (transition:generic)")
  else
    Ui.V.role `Succeeded
      (Ui.text
         (Printf.sprintf "%d record types, %d service methods" (count "repo")
            (count "rpc")))

let show_status fs xdg =
  let session = Xrpc_auth.Session.load fs ~app_name () in
  let password = Tangled.Config.load_password fs xdg <> None in
  let subtitle =
    Ui.muted
      (Option.value ~default:""
         (Eio.Path.native (Xdge.config_dir xdg)))
  in
  match session with
  | None ->
      Ui.panel ~subtitle (Ui.title "Not logged in")
        (Ui.V.concat
           [
             Ui.muted "Run ";
             Ui.bold "tangled auth login --oauth";
             Ui.muted " to sign in.";
           ])
  | Some s ->
      let expired = Xrpc_auth.Session.is_expired s in
      Ui.panel ~subtitle
        (Ui.V.concat [ Ui.muted "Logged in as "; Ui.title s.handle ])
        (Ui.fields
           [
             ("DID", Some (Ui.accent s.did));
             ("PDS", Some (Ui.link s.pds));
             ( "Login",
               Some
                 (match s.oauth with
                 | Some o -> Ui.text ("OAuth via " ^ o.issuer)
                 | None ->
                     Ui.text
                       (if password then "password (saved)" else "password")) );
             ( "Access",
               Option.map (fun o -> access o.Xrpc_auth.Session.scope) s.oauth );
             ( "Token",
               Some
                 (if expired then
                    Ui.V.role `Warning
                      (Ui.text "expired, renewed on the next request")
                  else
                    match s.oauth with
                    | Some o -> Ui.muted ("valid until " ^ o.expires_at)
                    | None -> Ui.muted "valid") );
           ])

let print_status fs xdg =
  Fmt.pr "@[<v>Config directory: %a@,@]" Eio.Path.pp (Xdge.config_dir xdg);
  (match Tangled.Config.load fs xdg with
  | Some config -> Fmt.pr "@[<v>Config:@,  %a@]@." Tangled.Config.pp config
  | None -> Fmt.pr "No config file found.@.");
  (match Tangled.Config.load_password fs xdg with
  | Some _ -> Fmt.pr "Password: (saved)@."
  | None -> Fmt.pr "Password: (not saved)@.");
  Fmt.pr "@.";
  match Xrpc_auth.Session.load fs ~app_name () with
  | None ->
      Fmt.pr "Session: Not logged in.@.";
      Fmt.pr "Use 'tangled auth login' to authenticate.@."
  | Some session ->
      Fmt.pr "Session:@.";
      Fmt.pr "  @[<v>%a@]@." Xrpc_auth.Session.pp session;
      if Xrpc_auth.Session.is_expired session then
        Fmt.pr
          "@[<v>@,\
           Note: Access token is expired, will refresh on next request.@]@."

let status_action env =
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  if Ui.tty () then show_status fs xdg else print_status fs xdg

let status_cmd =
  let doc = "Show who you are logged in as, and what you may do." in
  let info = Cmd.info "status" ~doc in
  let status' () = Eio_main.run @@ fun env -> status_action env in
  Common.cmd info Term.(const status' $ const ())

(* Auth command group *)

let client_metadata_cmd =
  let doc =
    "Print the client metadata to publish for $(b,auth login --oauth), for \
     those hosting their own copy."
  in
  let action client_id redirect_uri =
    Common.json
      (client_metadata
         ~client_id:(Option.value ~default:tangled_client_id client_id)
         ~redirect_uri:
           (Option.value ~default:Xrpc_auth.Oauth.hosted_redirect_uri
              redirect_uri))
  in
  Common.cmd
    (Cmd.info "client-metadata" ~doc)
    Term.(const action $ client_id_arg $ redirect_uri_arg)

let cmd =
  let doc = "Log in and out, and switch between accounts." in
  let man =
    Common.man
      [
        "Most commands that change something need you to be logged in. Reading \
         public repositories, issues and CI does not.";
      ]
  in
  let info = Cmd.info "auth" ~doc ~man in
  Cmd.group info
    [
      login_cmd;
      logout_cmd;
      status_cmd;
      client_metadata_cmd;
      Xrpc_auth.Cmd.profile_cmd ~name:"account" ~app_name ();
    ]
