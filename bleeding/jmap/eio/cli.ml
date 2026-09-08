(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

type source = Default | Env of string | Profile of string | Cmdline
type auth_scheme = Auth.scheme = Bearer | Basic

type config = {
  profile : string option;
  profile_source : source;
  session_url : string;
  session_url_source : source;
  api_key : string;
  api_key_source : source;
  api_key_file : string option;
  auth : auth_scheme;
  auth_source : source;
  account_id : string option;
  account_id_source : source;
  allow_insecure : bool;
  debug : bool;
}

let terminal_text text = Format.asprintf "%a" Jmap.Proto.Error.pp_escaped text

let pp_source ppf = function
  | Default -> Fmt.(styled `Faint string) ppf "default"
  | Env var -> Fmt.pf ppf "%a" Fmt.(styled `Yellow string) ("env(" ^ var ^ ")")
  | Profile name ->
      Fmt.pf ppf "%a" Fmt.(styled `Magenta string) ("profile(" ^ name ^ ")")
  | Cmdline -> Fmt.(styled `Blue string) ppf "cmdline"

let scheme_to_string = function Bearer -> "bearer" | Basic -> "basic"

let pp_config ppf cfg =
  let pp_field name value source =
    Fmt.pf ppf "@,%a %a %a"
      Fmt.(styled `Cyan string)
      (name ^ ":")
      Fmt.(styled `Green string)
      value
      Fmt.(styled `Faint (brackets pp_source))
      source
  in
  Fmt.pf ppf "@[<v>%a" Fmt.(styled `Bold string) "JMAP config:";
  (match cfg.profile with
  | None -> ()
  | Some profile -> pp_field "profile" profile cfg.profile_source);
  if cfg.session_url <> "" then
    pp_field "session_url" cfg.session_url cfg.session_url_source;
  (match cfg.api_key_file with
  | Some path -> pp_field "api_key_file" path cfg.api_key_source
  | None when cfg.api_key <> "" ->
      pp_field "api_key"
        (String.make (min 8 (String.length cfg.api_key)) '*' ^ "...")
        cfg.api_key_source
  | None -> ());
  if cfg.api_key <> "" || cfg.api_key_file <> None || cfg.auth_source <> Default
  then pp_field "auth" (scheme_to_string cfg.auth) cfg.auth_source;
  (match cfg.account_id with
  | None -> ()
  | Some id -> pp_field "account_id" id cfg.account_id_source);
  pp_field "debug" (string_of_bool cfg.debug)
    (if cfg.debug then Cmdline else Default);
  pp_field "allow_insecure"
    (string_of_bool cfg.allow_insecure)
    (if cfg.allow_insecure then Cmdline else Default);
  Fmt.pf ppf "@]"

let env_var_name suffix = "JMAP_" ^ suffix

let resolve_opt_with_env ~cmdline ~env_var =
  match cmdline with
  | Some v -> (Some v, Cmdline)
  | None -> (
      match Sys.getenv_opt env_var with
      | Some v when v <> "" -> (Some v, Env env_var)
      | _ -> (None, Default))

let session_url_term =
  let doc =
    Printf.sprintf
      "JMAP session URL. Can also be set with %s environment variable."
      (env_var_name "SESSION_URL")
  in
  Arg.(value & opt (some string) None & info [ "url"; "u" ] ~docv:"URL" ~doc)

let api_key_term =
  let doc =
    Printf.sprintf
      "JMAP API key or Bearer token. Can also be set with %s environment \
       variable. A command-line value is visible in the process list; prefer \
       --api-key-file or the environment."
      (env_var_name "API_KEY")
  in
  Arg.(
    value & opt (some string) None & info [ "api-key"; "k" ] ~docv:"KEY" ~doc)

let api_key_file_term =
  let doc =
    Printf.sprintf
      "File containing JMAP API key. Can also be set with %s environment \
       variable."
      (env_var_name "API_KEY_FILE")
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "api-key-file"; "K" ] ~docv:"FILE" ~doc)

let auth_term =
  let doc =
    Printf.sprintf
      "Authentication scheme: $(b,bearer) sends the API key as a Bearer token, \
       $(b,basic) sends it as HTTP Basic credentials in the form \
       USER:PASSWORD. Can also be set with %s environment variable."
      (env_var_name "AUTH")
  in
  let schemes = [ ("bearer", Bearer); ("basic", Basic) ] in
  Arg.(
    value & opt (some (enum schemes)) None & info [ "auth" ] ~docv:"SCHEME" ~doc)

let account_id_term =
  let doc =
    Printf.sprintf
      "Account ID to use (defaults to primary mail account). Can also be set \
       with %s."
      (env_var_name "ACCOUNT_ID")
  in
  Arg.(value & opt (some string) None & info [ "account"; "a" ] ~docv:"ID" ~doc)

let profile_setting_term =
  let doc =
    "Use the shared JMAP connection profile $(docv). Profiles are stored under \
     \\$XDG_CONFIG_HOME/jmap/profiles. Can also be set with JMAP_PROFILE."
  in
  let arg =
    Arg.(value & opt (some string) None & info [ "profile" ] ~docv:"NAME" ~doc)
  in
  Term.(
    const (fun cmdline ->
        resolve_opt_with_env ~cmdline ~env_var:(env_var_name "PROFILE"))
    $ arg)

let profile_term = Term.(const fst $ profile_setting_term)

let debug_term =
  let doc = "Enable debug output" in
  Arg.(value & flag & info [ "debug"; "d" ] ~doc)

let allow_insecure_term =
  let doc =
    "Allow credentials to be sent over a cleartext HTTP connection. Use only \
     for a trusted local test server."
  in
  Arg.(value & flag & info [ "allow-insecure" ] ~doc)

let auth_value ?fs cfg =
  let key_name =
    match cfg.api_key_source with
    | Env var -> var
    | Profile name -> Fmt.str "profile %S" name
    | Cmdline | Default -> "--api-key"
  in
  let scheme = scheme_to_string cfg.auth in
  let auth_name =
    match cfg.auth_source with
    | Env var -> Fmt.str "%s=%s" var scheme
    | Profile name -> Fmt.str "profile %S with %s auth" name scheme
    | Cmdline | Default -> Fmt.str "--auth %s" scheme
  in
  let secret =
    match cfg.api_key_file with
    | Some path -> `File path
    | None -> `Key cfg.api_key
  in
  Auth.of_scheme ?fs ~key_name ~auth_name cfg.auth secret

let config_term =
  let make (profile, profile_source) session_url_opt api_key_opt
      api_key_file_opt auth_opt account_id_opt allow_insecure debug =
    let ( let* ) = Result.bind in
    let* () =
      match profile with
      | Some name when not (Profile.valid_name name) ->
          Error
            (Fmt.str
               "profile name %S must contain only letters, digits, '.', '-' \
                and '_', and may not be '.' or '..'"
               name)
      | Some _ | None -> Ok ()
    in
    let session_url, session_url_source =
      let url, source =
        resolve_opt_with_env ~cmdline:session_url_opt
          ~env_var:(env_var_name "SESSION_URL")
      in
      (Option.value url ~default:"", source)
    in
    let* () =
      if session_url = "" && profile = None then
        Error "no session URL. Set --url, JMAP_SESSION_URL or --profile"
      else Ok ()
    in
    let* api_key, api_key_file, api_key_source =
      match (api_key_opt, api_key_file_opt) with
      | Some _, Some _ ->
          Error "--api-key and --api-key-file cannot be used together"
      | Some key, None -> Ok (key, None, Cmdline)
      | None, Some path -> Ok ("", Some path, Cmdline)
      | None, None -> (
          let file_var = env_var_name "API_KEY_FILE" in
          match Sys.getenv_opt file_var with
          | Some path when path <> "" -> Ok ("", Some path, Env file_var)
          | Some _ | None ->
              let key, source =
                resolve_opt_with_env ~cmdline:None
                  ~env_var:(env_var_name "API_KEY")
              in
              Ok (Option.value key ~default:"", None, source))
    in
    let* () =
      if api_key = "" && api_key_file = None && profile = None then
        Error
          "no API key. Set --api-key, --api-key-file, JMAP_API_KEY or \
           JMAP_API_KEY_FILE, or select --profile"
      else Ok ()
    in
    let* auth, auth_source =
      match auth_opt with
      | Some a -> Ok (a, Cmdline)
      | None -> (
          let var = env_var_name "AUTH" in
          match Sys.getenv_opt var with
          | None | Some "" -> Ok (Bearer, Default)
          | Some v ->
              let* scheme = Auth.scheme_of_string ~name:var v in
              Ok (scheme, Env var))
    in
    let account_id, account_id_source =
      resolve_opt_with_env ~cmdline:account_id_opt
        ~env_var:(env_var_name "ACCOUNT_ID")
    in
    let cfg =
      {
        profile;
        profile_source;
        session_url;
        session_url_source;
        api_key;
        api_key_source;
        api_key_file;
        auth;
        auth_source;
        account_id;
        account_id_source;
        allow_insecure;
        debug;
      }
    in
    let* _ =
      if api_key = "" && api_key_file = None then Ok Auth.none
      else auth_value cfg
    in
    Ok cfg
  in
  let term =
    Term.(
      const make $ profile_setting_term $ session_url_term $ api_key_term
      $ api_key_file_term $ auth_term $ account_id_term $ allow_insecure_term
      $ debug_term)
  in
  Term.term_result' ~usage:true term

let env_docs =
  {|
Environment Variables:
  JMAP_SESSION_URL    JMAP session URL (e.g., https://api.fastmail.com/jmap/session)
  JMAP_API_KEY        API key or Bearer token for authentication
  JMAP_API_KEY_FILE   Path to a private file containing the API key
  JMAP_AUTH           bearer (default) or basic (API key is USER:PASSWORD)
  JMAP_ACCOUNT_ID     Account ID to use (optional, defaults to primary mail account)
  JMAP_PROFILE        Shared profile selected by clients that accept --profile

Configuration Precedence:
  1. Command-line flags (e.g., --url, --api-key)
  2. Environment variables (e.g., JMAP_SESSION_URL)
  3. Fields from the selected JMAP_PROFILE

Security:
  --api-key is visible in the process list. Prefer JMAP_API_KEY_FILE, a
  group/world-inaccessible file, or JMAP_API_KEY.
  Credentials are refused over HTTP unless --allow-insecure is given. Use it
  only for a trusted local test server.

Example:
  jmap session --profile personal

Or configure the connection directly:
  export JMAP_SESSION_URL="https://api.fastmail.com/jmap/session"
  export JMAP_API_KEY_FILE="\$HOME/.jmap-api-key"
|}

let default_timeout = 60.

let resolve env cfg =
  let use_profile_url =
    cfg.session_url = "" && cfg.session_url_source = Default
  in
  let use_profile_credential =
    cfg.api_key = "" && cfg.api_key_file = None && cfg.api_key_source = Default
  in
  match (cfg.profile, use_profile_url || use_profile_credential) with
  | None, _ | Some _, false -> Ok cfg
  | Some name, true ->
      let ( let* ) = Result.bind in
      let* store =
        Profile.xdg_store env |> Result.map_error Profile.error_to_string
      in
      let* profile =
        Profile.load store name |> Result.map_error Profile.error_to_string
      in
      let profile_auth, profile_key =
        match Profile.credential profile with
        | Profile.Bearer token -> (Bearer, token)
        | Profile.Basic { user; password } -> (Basic, user ^ ":" ^ password)
      in
      Ok
        {
          cfg with
          session_url =
            (if use_profile_url then Profile.session_url profile
             else cfg.session_url);
          session_url_source =
            (if use_profile_url then Profile name else cfg.session_url_source);
          api_key =
            (if use_profile_credential then profile_key else cfg.api_key);
          api_key_source =
            (if use_profile_credential then Profile name else cfg.api_key_source);
          auth =
            (if use_profile_credential && cfg.auth_source = Default then
               profile_auth
             else cfg.auth);
          auth_source =
            (if use_profile_credential && cfg.auth_source = Default then
               Profile name
             else cfg.auth_source);
        }

let invalid_configuration message =
  Client.Transport (Fetch.Invalid_request message, message)

let connect_resolved ~sw ?(timeout = default_timeout) env cfg =
  match auth_value ~fs:(Eio.Stdenv.fs env) cfg with
  | Error message -> Error (invalid_configuration message)
  | Ok auth ->
      Client.connect_env ~sw ~auth ~timeout ~allow_insecure:cfg.allow_insecure
        env cfg.session_url

let connect ~sw ?(timeout = default_timeout) env cfg =
  match resolve env cfg with
  | Error message -> Error (invalid_configuration message)
  | Ok cfg -> connect_resolved ~sw ~timeout env cfg

let create_client ~sw ?timeout env cfg =
  match connect ~sw ?timeout env cfg with
  | Ok client -> client
  | Error e ->
      Fmt.epr "@[<v>%a Failed to connect: %a@]@."
        Fmt.(styled `Red string)
        "Error:" Client.pp_error e;
      exit 1

let account_id ?(capability = Jmap.Proto.Capability.mail) cfg client =
  match cfg.account_id with
  | Some id -> (
      match Jmap.Proto.Id.of_string id with
      | Ok id -> Ok id
      | Error msg -> Error (Fmt.str "invalid account id %S: %s" id msg))
  | None -> (
      match
        Jmap.Proto.Session.primary_account_for capability
          (Client.session client)
      with
      | Some id -> Ok id
      | None ->
          Error
            (Fmt.str "no primary account for capability %S. Specify --account"
               capability))

type context = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  config : config;
  client : Client.t;
  account_id : Jmap.Proto.Id.t;
}

let run_context ?capability ~name ~sw env cfg f =
  let die fmt =
    Fmt.kstr
      (fun s ->
        Fmt.epr "%s: %s@." name s;
        1)
      fmt
  in
  match resolve env cfg with
  | Error message -> die "%s" message
  | Ok cfg -> (
      match connect_resolved ~sw env cfg with
      | Error e -> die "%a" Client.pp_error e
      | Ok client -> (
          match account_id ?capability cfg client with
          | Error msg -> die "%s" msg
          | Ok account_id -> (
              let ctx = { env; sw; config = cfg; client; account_id } in
              match f ctx with
              | () -> Cmd.Exit.ok
              | exception Client.Jmap_client_error e ->
                  die "%a" Client.pp_error e
              | exception Jmap.Chain.Parse_error e ->
                  die "%a" Jmap.Chain.pp_parse_error e
              | exception Sync.Sync_error e -> die "%a" Sync.pp_error e
              | exception Failure msg -> die "%s" (terminal_text msg))))

let command ?doc ?man ?capability ~args name f =
  let man =
    Option.value man ~default:[] @ [ `S Manpage.s_environment; `Pre env_docs ]
  in
  let run cfg extra =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    run_context ?capability ~name ~sw env cfg (fun ctx -> f ctx extra)
  in
  Cmd.v (Cmd.info name ?doc ~man) Term.(const run $ config_term $ args)

let main' ?doc ?man ?capability ~args name f =
  exit (Cmd.eval' (command ?doc ?man ?capability ~args name f))

let main ?doc ?man ?capability name f =
  main' ?doc ?man ?capability ~args:(Term.const ()) name (fun ctx () -> f ctx)

let debug cfg fmt =
  if cfg.debug then
    Fmt.kpf
      (fun ppf -> Fmt.pf ppf "@.")
      Fmt.stderr
      ("@[<h>[DEBUG] " ^^ fmt ^^ "@]")
  else Format.ikfprintf ignore Format.err_formatter fmt
