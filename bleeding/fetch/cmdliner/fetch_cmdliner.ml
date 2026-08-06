(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner integration for {!Fetch} clients. *)

open Cmdliner

let src = Logs.Src.create "fetch.cmdliner" ~doc:"fetch command-line configuration"

module Log = (val Logs.src_log src : Logs.LOG)

(* ========================================================================= *)
(* Source tracking                                                           *)
(* ========================================================================= *)

type source = Default | Env of string | Cmdline

type 'a with_source = { value : 'a; source : source }

type proxy_config = {
  proxy_url : string with_source option;
  no_proxy : string with_source option;
}

type config = {
  xdg : Xdge.t * Xdge.Cmd.t;
  persist_cookies : bool with_source;
  verify_tls : bool with_source;
  timeout : float option with_source;
  max_retries : int with_source;
  retry_backoff : float with_source;
  follow_redirects : bool with_source;
  max_redirects : int with_source;
  user_agent : string option with_source;
  verbose_http : bool with_source;
  proxy : proxy_config;
}

(** Helper to check environment variable and track source *)
let check_env_bool ~app_name ~suffix ~default =
  let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
  match Sys.getenv_opt env_var with
  | Some v
    when String.lowercase_ascii v = "1" || String.lowercase_ascii v = "true" ->
      { value = true; source = Env env_var }
  | Some v
    when String.lowercase_ascii v = "0" || String.lowercase_ascii v = "false" ->
      { value = false; source = Env env_var }
  | Some _ | None -> { value = default; source = Default }

let check_env_string ~app_name ~suffix =
  let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
  match Sys.getenv_opt env_var with
  | Some v when v <> "" -> Some { value = v; source = Env env_var }
  | Some _ | None -> None

let check_env_float ~app_name ~suffix ~default =
  let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
  match Sys.getenv_opt env_var with
  | Some v -> (
      try { value = float_of_string v; source = Env env_var }
      with _ -> { value = default; source = Default })
  | None -> { value = default; source = Default }

let check_env_int ~app_name ~suffix ~default =
  let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
  match Sys.getenv_opt env_var with
  | Some v -> (
      try { value = int_of_string v; source = Env env_var }
      with _ -> { value = default; source = Default })
  | None -> { value = default; source = Default }

(** Parse proxy configuration from environment. Follows standard
    HTTP_PROXY/HTTPS_PROXY/ALL_PROXY/NO_PROXY conventions. *)
let proxy_from_env () =
  let first vars =
    List.fold_left
      (fun acc var ->
        match acc with
        | Some _ -> acc
        | None -> (
            match Sys.getenv_opt var with
            | Some v when v <> "" -> Some { value = v; source = Env var }
            | _ -> None))
      None vars
  in
  let proxy_url =
    first
      [ "HTTP_PROXY"; "http_proxy"; "HTTPS_PROXY"; "https_proxy"; "ALL_PROXY";
        "all_proxy" ]
  in
  let no_proxy = first [ "NO_PROXY"; "no_proxy" ] in
  { proxy_url; no_proxy }

(* ========================================================================= *)
(* Deriving fetch arguments                                                  *)
(* ========================================================================= *)

let redirects config =
  if config.follow_redirects.value then config.max_redirects.value else 0

let retry_config config =
  if config.max_retries.value > 0 then
    Some
      (Fetch.Retry.v ~max_retries:config.max_retries.value
         ~backoff_factor:config.retry_backoff.value ())
  else None

let create config env sw =
  let xdg, _xdg_cmd = config.xdg in
  let clock = env#clock in
  let mono_clock = env#mono_clock in
  let proxy = Option.map (fun ws -> ws.value) config.proxy.proxy_url in
  (match config.proxy.no_proxy with
  | Some { value; _ } ->
      Log.info (fun m ->
          m
            "--no-proxy %S has no fetch equivalent and is ignored (fetch-curl \
             exposes a single proxy URL, not libcurl's NOPROXY list)"
            value)
  | None -> ());
  if not config.follow_redirects.value || config.max_redirects.value <> 10 then
    Log.info (fun m ->
        m
          "redirect policy is per-request in fetch: pass ?redirects:%d to \
           Fetch.fetch (see Fetch_cmdliner.redirects)"
          (redirects config));
  let base =
    Fetch_curl.v ~sw ~tls_verify:config.verify_tls.value ?proxy
      ?timeout:config.timeout.value ?user_agent:config.user_agent.value
      ~verbose:config.verbose_http.value ()
  in
  let jar =
    if config.persist_cookies.value then
      let path = Eio.Path.(Xdge.data_dir xdg / "cookies.txt") in
      Fetch_cookies.Jar.of_file ~clock path
    else Fetch_cookies.Jar.in_memory ~clock ()
  in
  let t = Fetch_cookies.with_jar jar base in
  let t = Fetch.with_limits ~clock:mono_clock ~max_concurrent:6 t in
  match retry_config config with
  | None -> t
  | Some retry ->
      Fetch.with_retry ~clock:mono_clock ~random:env#secure_random ~config:retry
        t

(* ========================================================================= *)
(* Individual terms                                                          *)
(* ========================================================================= *)

let persist_cookies_term app_name =
  let doc = "Persist cookies to disk between sessions" in
  let env_name = String.uppercase_ascii app_name ^ "_PERSIST_COOKIES" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(value & flag & info [ "persist-cookies" ] ~env:env_info ~doc)
  in
  Term.(
    const (fun cmdline ->
        if cmdline then { value = true; source = Cmdline }
        else check_env_bool ~app_name ~suffix:"PERSIST_COOKIES" ~default:false)
    $ cmdline_arg)

let verify_tls_term app_name =
  let doc = "Skip TLS certificate verification (insecure)" in
  let env_name = String.uppercase_ascii app_name ^ "_NO_VERIFY_TLS" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(value & flag & info [ "no-verify-tls" ] ~env:env_info ~doc)
  in
  Term.(
    const (fun no_verify ->
        if no_verify then { value = false; source = Cmdline }
        else
          let env_val =
            check_env_bool ~app_name ~suffix:"NO_VERIFY_TLS" ~default:false
          in
          { value = not env_val.value; source = env_val.source })
    $ cmdline_arg)

let timeout_term app_name =
  let doc = "Request timeout in seconds" in
  let env_name = String.uppercase_ascii app_name ^ "_TIMEOUT" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(
      value
      & opt (some float) None
      & info [ "timeout" ] ~env:env_info ~docv:"SECONDS" ~doc)
  in
  Term.(
    const (fun cmdline ->
        match cmdline with
        | Some t -> { value = Some t; source = Cmdline }
        | None -> (
            match check_env_string ~app_name ~suffix:"TIMEOUT" with
            | Some { value = v; source } -> (
                try { value = Some (float_of_string v); source }
                with _ -> { value = None; source = Default })
            | None -> { value = None; source = Default }))
    $ cmdline_arg)

let retries_term app_name =
  let doc = "Maximum number of request retries" in
  let env_name = String.uppercase_ascii app_name ^ "_MAX_RETRIES" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(
      value
      & opt (some int) None
      & info [ "max-retries" ] ~env:env_info ~docv:"N" ~doc)
  in
  Term.(
    const (fun cmdline ->
        match cmdline with
        | Some n -> { value = n; source = Cmdline }
        | None -> check_env_int ~app_name ~suffix:"MAX_RETRIES" ~default:3)
    $ cmdline_arg)

let retry_backoff_term app_name =
  let doc = "Retry backoff factor for exponential delay" in
  let env_name = String.uppercase_ascii app_name ^ "_RETRY_BACKOFF" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(
      value
      & opt (some float) None
      & info [ "retry-backoff" ] ~env:env_info ~docv:"FACTOR" ~doc)
  in
  Term.(
    const (fun cmdline ->
        match cmdline with
        | Some f -> { value = f; source = Cmdline }
        | None -> check_env_float ~app_name ~suffix:"RETRY_BACKOFF" ~default:0.3)
    $ cmdline_arg)

let follow_redirects_term app_name =
  let doc = "Don't follow HTTP redirects" in
  let env_name = String.uppercase_ascii app_name ^ "_NO_FOLLOW_REDIRECTS" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(value & flag & info [ "no-follow-redirects" ] ~env:env_info ~doc)
  in
  Term.(
    const (fun no_follow ->
        if no_follow then { value = false; source = Cmdline }
        else
          let env_val =
            check_env_bool ~app_name ~suffix:"NO_FOLLOW_REDIRECTS" ~default:false
          in
          { value = not env_val.value; source = env_val.source })
    $ cmdline_arg)

let max_redirects_term app_name =
  let doc = "Maximum number of redirects to follow" in
  let env_name = String.uppercase_ascii app_name ^ "_MAX_REDIRECTS" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(
      value
      & opt (some int) None
      & info [ "max-redirects" ] ~env:env_info ~docv:"N" ~doc)
  in
  Term.(
    const (fun cmdline ->
        match cmdline with
        | Some n -> { value = n; source = Cmdline }
        | None -> check_env_int ~app_name ~suffix:"MAX_REDIRECTS" ~default:10)
    $ cmdline_arg)

let user_agent_term app_name =
  let doc = "User-Agent header to send with requests" in
  let env_name = String.uppercase_ascii app_name ^ "_USER_AGENT" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(
      value
      & opt (some string) None
      & info [ "user-agent" ] ~env:env_info ~docv:"STRING" ~doc)
  in
  Term.(
    const (fun cmdline ->
        match cmdline with
        | Some ua -> { value = Some ua; source = Cmdline }
        | None -> (
            match check_env_string ~app_name ~suffix:"USER_AGENT" with
            | Some { value; source } -> { value = Some value; source }
            | None -> { value = None; source = Default }))
    $ cmdline_arg)

let verbose_http_term app_name =
  let doc = "Enable verbose HTTP-level logging (hexdumps, TLS details)" in
  let env_name = String.uppercase_ascii app_name ^ "_VERBOSE_HTTP" in
  let env_info = Cmdliner.Cmd.Env.info env_name in
  let cmdline_arg =
    Arg.(value & flag & info [ "verbose-http" ] ~env:env_info ~doc)
  in
  Term.(
    const (fun cmdline ->
        if cmdline then { value = true; source = Cmdline }
        else check_env_bool ~app_name ~suffix:"VERBOSE_HTTP" ~default:false)
    $ cmdline_arg)

let proxy_term _app_name =
  let doc = "HTTP/HTTPS proxy URL (e.g., http://proxy:8080)" in
  let cmdline_arg =
    Arg.(value & opt (some string) None & info [ "proxy" ] ~docv:"URL" ~doc)
  in
  let no_proxy_doc = "Comma-separated list of hosts to bypass proxy" in
  let no_proxy_arg =
    Arg.(
      value
      & opt (some string) None
      & info [ "no-proxy" ] ~docv:"HOSTS" ~doc:no_proxy_doc)
  in
  Term.(
    const (fun cmdline_proxy cmdline_no_proxy ->
        let proxy_url =
          match cmdline_proxy with
          | Some url -> Some { value = url; source = Cmdline }
          | None -> (proxy_from_env ()).proxy_url
        in
        let no_proxy =
          match cmdline_no_proxy with
          | Some np -> Some { value = np; source = Cmdline }
          | None -> (proxy_from_env ()).no_proxy
        in
        { proxy_url; no_proxy })
    $ cmdline_arg $ no_proxy_arg)

(* ========================================================================= *)
(* Combined terms                                                            *)
(* ========================================================================= *)

let config_term app_name fs =
  let xdg_term = Xdge.Cmd.term app_name fs ~dirs:[ `Config; `Data; `Cache ] () in
  Term.(
    const
      (fun xdg persist verify timeout retries backoff follow max_redir ua verbose
           proxy ->
        {
          xdg;
          persist_cookies = persist;
          verify_tls = verify;
          timeout;
          max_retries = retries;
          retry_backoff = backoff;
          follow_redirects = follow;
          max_redirects = max_redir;
          user_agent = ua;
          verbose_http = verbose;
          proxy;
        })
    $ xdg_term
    $ persist_cookies_term app_name
    $ verify_tls_term app_name
    $ timeout_term app_name
    $ retries_term app_name
    $ retry_backoff_term app_name
    $ follow_redirects_term app_name
    $ max_redirects_term app_name
    $ user_agent_term app_name
    $ verbose_http_term app_name
    $ proxy_term app_name)

let client_term app_name eio_env sw fs =
  let config_t = config_term app_name fs in
  Term.(const (fun config -> create config eio_env sw) $ config_t)

let minimal_term app_name fs =
  let xdg_term = Xdge.Cmd.term app_name fs ~dirs:[ `Data; `Cache ] () in
  Term.(
    const (fun (xdg, _xdg_cmd) persist -> (xdg, persist.value))
    $ xdg_term
    $ persist_cookies_term app_name)

let env_docs app_name =
  let app_upper = String.uppercase_ascii app_name in
  Printf.sprintf
    "## ENVIRONMENT\n\n\
     The following environment variables affect %s:\n\n\
     ### XDG Directories\n\n\
     **%s_CONFIG_DIR**\n\
     :   Override configuration directory location\n\n\
     **%s_DATA_DIR**\n\
     :   Override data directory location (for cookies)\n\n\
     **%s_CACHE_DIR**\n\
     :   Override cache directory location\n\n\
     **XDG_CONFIG_HOME**\n\
     :   Base directory for user configuration files (default: ~/.config)\n\n\
     **XDG_DATA_HOME**\n\
     :   Base directory for user data files (default: ~/.local/share)\n\n\
     **XDG_CACHE_HOME**\n\
     :   Base directory for user cache files (default: ~/.cache)\n\n\
     ### HTTP Settings\n\n\
     **%s_PERSIST_COOKIES**\n\
     :   Set to '1' to persist cookies by default\n\n\
     **%s_NO_VERIFY_TLS**\n\
     :   Set to '1' to disable TLS verification (insecure)\n\n\
     **%s_TIMEOUT**\n\
     :   Default request timeout in seconds\n\n\
     **%s_MAX_RETRIES**\n\
     :   Maximum number of retries (default: 3)\n\n\
     **%s_RETRY_BACKOFF**\n\
     :   Retry backoff factor (default: 0.3)\n\n\
     **%s_NO_FOLLOW_REDIRECTS**\n\
     :   Set to '1' to disable redirect following\n\n\
     **%s_MAX_REDIRECTS**\n\
     :   Maximum redirects to follow (default: 10)\n\n\
     **%s_USER_AGENT**\n\
     :   User-Agent header to send with requests\n\n\
     **%s_VERBOSE_HTTP**\n\
     :   Set to '1' to enable verbose HTTP-level logging\n\n\
     ### Proxy Configuration\n\n\
     **HTTP_PROXY** / **http_proxy**\n\
     :   HTTP proxy URL (e.g., http://proxy:8080 or \
     http://user:pass@proxy:8080)\n\n\
     **HTTPS_PROXY** / **https_proxy**\n\
     :   HTTPS proxy URL (used for HTTPS requests)\n\n\
     **ALL_PROXY** / **all_proxy**\n\
     :   Fallback proxy URL for all protocols\n\n\
     **NO_PROXY** / **no_proxy**\n\
     :   Comma-separated list of hosts to bypass proxy (e.g., \
     localhost,*.example.com)"
    app_name app_upper app_upper app_upper app_upper app_upper app_upper
    app_upper app_upper app_upper app_upper app_upper app_upper

(* ========================================================================= *)
(* Pretty-printing                                                           *)
(* ========================================================================= *)

let pp_source ppf = function
  | Default -> Format.fprintf ppf "default"
  | Env var -> Format.fprintf ppf "env(%s)" var
  | Cmdline -> Format.fprintf ppf "cmdline"

let pp_with_source pp_val ppf ws =
  Format.fprintf ppf "%a [%a]" pp_val ws.value pp_source ws.source

let pp_config ?(show_sources = true) ppf config =
  let _xdg, xdg_cmd = config.xdg in
  let pp_bool = Format.pp_print_bool in
  let pp_float = Format.pp_print_float in
  let pp_int = Format.pp_print_int in
  let pp_string_opt = Format.pp_print_option Format.pp_print_string in
  let pp_float_opt = Format.pp_print_option Format.pp_print_float in
  let pp_val pp =
    if show_sources then pp_with_source pp else fun ppf ws -> pp ppf ws.value
  in
  Format.fprintf ppf
    "@[<v>Configuration:@,\
     @[<v 2>XDG:@,\
     %a@]@,\
     persist_cookies: %a@,\
     verify_tls: %a@,\
     timeout: %a@,\
     max_retries: %a@,\
     retry_backoff: %a@,\
     follow_redirects: %a@,\
     max_redirects: %a@,\
     user_agent: %a@,\
     verbose_http: %a@,\
     @[<v 2>Proxy:@,\
     url: %a@,\
     no_proxy: %a@]@]"
    Xdge.Cmd.pp xdg_cmd
    (pp_val pp_bool) config.persist_cookies
    (pp_val pp_bool) config.verify_tls
    (pp_val pp_float_opt) config.timeout
    (pp_val pp_int) config.max_retries
    (pp_val pp_float) config.retry_backoff
    (pp_val pp_bool) config.follow_redirects
    (pp_val pp_int) config.max_redirects
    (pp_val pp_string_opt) config.user_agent
    (pp_val pp_bool) config.verbose_http
    (Format.pp_print_option (pp_with_source Format.pp_print_string))
    config.proxy.proxy_url
    (Format.pp_print_option (pp_with_source Format.pp_print_string))
    config.proxy.no_proxy

(* ========================================================================= *)
(* Logging configuration                                                     *)
(* ========================================================================= *)

let setup_log_sources ?(verbose_http = false) level =
  let set_tls_tracing_level lvl =
    match
      List.find_opt (fun s -> Logs.Src.name s = "tls.tracing") (Logs.Src.list ())
    with
    | Some tls_src -> Logs.Src.set_level tls_src (Some lvl)
    | None -> () (* TLS not loaded, ignore *)
  in
  match level with
  | Some Logs.Debug ->
      Logs.Src.set_level src (Some Logs.Debug);
      if verbose_http then set_tls_tracing_level Logs.Debug
      else set_tls_tracing_level Logs.Warning
  | Some Logs.Info ->
      Logs.Src.set_level src (Some Logs.Info);
      set_tls_tracing_level Logs.Warning
  | _ -> set_tls_tracing_level Logs.Warning
