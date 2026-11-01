(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let app_name = "tangled"

(* Common options *)

let pds =
  let doc = "PDS base URL." in
  let env = Cmd.Env.info "TANGLED_PDS" in
  Arg.(value & opt (some string) None & info [ "pds" ] ~env ~docv:"URL" ~doc)

(* Auth login command *)

let handle_arg =
  let doc = "Handle or DID to login as." in
  Arg.(
    value & opt (some string) None & info [ "handle"; "u" ] ~docv:"HANDLE" ~doc)

let password_arg =
  let doc = "Password or app password." in
  let env = Cmd.Env.info "TANGLED_PASSWORD" in
  Arg.(
    value
    & opt (some string) None
    & info [ "password"; "p" ] ~env ~docv:"PASSWORD" ~doc)

let save_password_arg =
  let doc =
    "Save password to config (stored in separate file with 600 permissions)."
  in
  Arg.(value & flag & info [ "save-password" ] ~doc)

let oauth_arg =
  let doc =
    "Sign in through the PDS's OAuth server in a browser instead of with a \
     password. The browser ends on a page showing a line to paste back here."
  in
  Arg.(value & flag & info [ "oauth" ] ~doc)

let loopback_arg =
  let doc =
    "With $(b,--oauth), have the browser return to a listener on 127.0.0.1 \
     instead of a paste page. The browser must run on this host. This mode \
     needs no published client metadata, so it can request only the Tangled \
     access the CLI uses."
  in
  Arg.(value & flag & info [ "loopback" ] ~doc)

let client_id_arg =
  let doc =
    "HTTPS URL of the OAuth client metadata for paste mode. The default is \
     the Tangled CLI's published metadata, whose scope covers only Tangled \
     records and the methods the CLI calls."
  in
  Arg.(value & opt (some string) None & info [ "client-id" ] ~docv:"URL" ~doc)

let redirect_uri_arg =
  let doc =
    "Paste page declared in the $(b,--client-id) metadata. The default is \
     the ocaml-atp CLI's page."
  in
  Arg.(
    value & opt (some string) None & info [ "redirect-uri" ] ~docv:"URL" ~doc)

let scope_arg =
  let doc =
    "OAuth scope to request. The default is the Tangled records and methods \
     the CLI uses, or $(b,atproto transition:generic) with another \
     $(b,--client-id). A scope must be within the one the client metadata \
     declares."
  in
  Arg.(value & opt (some string) None & info [ "scope" ] ~docv:"SCOPE" ~doc)

let no_browser_arg =
  let doc = "Print the OAuth URL without opening a browser." in
  Arg.(value & flag & info [ "no-browser" ] ~doc)

(* Record collections the CLI may write, and the methods it needs service
   tokens for. Legacy sh.tangled collections are read-only here. *)
let records () =
  List.filter_map
    (fun (nsid, _) ->
      if
        String.starts_with ~prefix:"org.tangled." nsid
        && Tangled.Schema.kind nsid = "record"
      then Some nsid
      else None)
    Tangled.Schema.documents

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
  let doc = "Login to Tangled." in
  let info = Cmd.info "login" ~doc in
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
  let doc = "Logout from Tangled." in
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
  let doc = "Show current authentication status." in
  let info = Cmd.info "status" ~doc in
  let status' () = Eio_main.run @@ fun env -> status_action env in
  Common.cmd info Term.(const status' $ const ())

(* Auth command group *)

let client_metadata_cmd =
  let doc =
    "Print the OAuth client metadata that the paste flow's default client ID \
     must serve. Publish it at the $(b,--client-id) URL with the paste page \
     at $(b,--redirect-uri)."
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
  let doc = "Authentication commands." in
  let info = Cmd.info "auth" ~doc in
  Cmd.group info
    [
      login_cmd;
      logout_cmd;
      status_cmd;
      client_metadata_cmd;
      Xrpc_auth.Cmd.profile_cmd ~app_name ();
    ]
