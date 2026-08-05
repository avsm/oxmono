(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner integration for {!Fetch} clients.

    This module provides command-line argument handling for configuring an HTTP
    client, including XDG directory paths, timeouts, retries, proxy settings,
    and other parameters, and builds a {!Fetch_curl} stack from the result.

    {2 Source Tracking}

    Configuration values include source tracking to indicate where each value
    came from (command line, environment variable, or default). This enables
    transparent debugging and helps users understand how their configuration
    was resolved.

    {[
      let config = ... in
      if show_sources then
        Format.printf "%a@." (Fetch_cmdliner.pp_config ~show_sources:true) config
    ]}

    {2:mapping What each flag maps onto}

    The flag names, environment variables, defaults and precedence are those of
    the [Requests.Cmd] term this replaces, so a command's interface does not
    change. What differs is where each value lands:

    {ul
     {- [--timeout SECONDS] is [Fetch_curl.v ?timeout], a bound on the whole
        transfer. There is no flag for [?connect_timeout], which keeps
        libcurl's 30s default. A composable per-request bound is
        [Eio.Time.with_timeout].}
     {- [--max-retries N] and [--retry-backoff FACTOR] become a
        {!Fetch.Retry.config} applied with {!Fetch.with_retry}. [N = 0] leaves
        the retry wrapper off altogether. See {!retry_config}.}
     {- [--proxy URL] is [Fetch_curl.v ?proxy]. Userinfo in the URL
        ([http://user:pass@proxy:8080]) is passed through to libcurl, which
        authenticates with it.}
     {- [--no-proxy HOSTS] is parsed and reported by {!pp_config} but has
        {b no effect}: [fetch-curl] exposes a single proxy URL rather than
        libcurl's [NOPROXY] list. {!create} logs it at info level.}
     {- [--no-verify-tls] is [Fetch_curl.v ~tls_verify:false].}
     {- [--follow-redirects]/[--max-redirects N] have {b no effect on the
        client}: redirects are a per-request argument in fetch
        ([Fetch.fetch ?redirects]), not a client property. {!redirects} turns
        the configuration into the integer to pass, [0] meaning "do not
        follow".}
     {- [--user-agent STRING] is [Fetch_curl.v ?user_agent], which applies only
        when a request does not set one of its own.}
     {- [--persist-cookies] selects a {!Fetch_cookies.Jar.of_file} jar at
        [<xdg-data-dir>/cookies.txt] instead of an in-memory one.}
     {- [--verbose-http] is [Fetch_curl.v ~verbose:true], which writes
        libcurl's transfer chatter to stderr, and via {!setup_log_sources}
        raises this library's own [Logs] source, ["fetch.cmdliner"], to debug.
        [fetch] and [fetch-curl] have no log sources of their own.}
     {- [--config-dir], [--data-dir] and [--cache-dir] come from
        {!Xdge.Cmd.term}, unchanged.}}

    There is no flag for [Fetch_curl.v ?max_response], so the cap stays at
    libcurl's 256 MiB default; build from {!Fetch_curl.v} directly to change
    it. *)

(** {1 Source Tracking Types} *)

type source =
  | Default  (** Value from hardcoded default *)
  | Env of string  (** Value from environment variable (stores var name) *)
  | Cmdline  (** Value from command-line argument *)
      (** Source of a configuration value. Tracks where each configuration
          value originated from for debugging and transparency. *)

type 'a with_source = { value : 'a; source : source }
(** Wrapper for values with source tracking *)

type proxy_config = {
  proxy_url : string with_source option;
      (** Proxy URL (from HTTP_PROXY/HTTPS_PROXY/etc) *)
  no_proxy : string with_source option;  (** NO_PROXY patterns *)
}
(** Proxy configuration from command line and environment *)

(** {1 Configuration Type} *)

type config = {
  xdg : Xdge.t * Xdge.Cmd.t;  (** XDG paths and their sources *)
  persist_cookies : bool with_source;  (** Whether to persist cookies *)
  verify_tls : bool with_source;  (** Whether to verify TLS certificates *)
  timeout : float option with_source;  (** Request timeout in seconds *)
  max_retries : int with_source;  (** Maximum number of retries *)
  retry_backoff : float with_source;  (** Retry backoff factor *)
  follow_redirects : bool with_source;  (** Whether to follow redirects *)
  max_redirects : int with_source;  (** Maximum number of redirects *)
  user_agent : string option with_source;  (** User-Agent header *)
  verbose_http : bool with_source;  (** Enable verbose HTTP-level logging *)
  proxy : proxy_config;  (** Proxy configuration *)
}
(** Configuration from command line and environment. All values include source
    tracking for debugging. *)

(** {1 Building a client} *)

val create :
  config ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Eio.Switch.t ->
  Fetch.plain
(** [create config env sw] builds the client [config] describes: a
    {!Fetch_curl.v} backend carrying the TLS, proxy, timeout, user-agent and
    verbosity settings, wrapped in a cookie jar, per-origin flow control (6
    concurrent requests, as {!Fetch_curl.std} uses) and, when [max_retries] is
    above zero, {!Fetch.with_retry}.

    The redirect flags do not appear here — see {!redirects} and the
    {{!mapping}mapping} above. *)

val redirects : config -> int
(** [redirects config] is the value to pass as [?redirects] to {!Fetch.fetch}
    and friends: [config.max_redirects] when [config.follow_redirects] is set
    and [0] otherwise. Redirects are a per-request argument in fetch, so this
    cannot be baked into the client {!create} returns. *)

val retry_config : config -> Fetch.Retry.config option
(** [retry_config config] is the retry policy {!create} applies, or [None] when
    [--max-retries] is zero. *)

(** {1 Individual Terms}

    Each term returns a value with source tracking to indicate whether the
    value came from the command line, environment, or default. Source
    precedence: Cmdline > Env > Default *)

val persist_cookies_term : string -> bool with_source Cmdliner.Term.t
(** Term for [--persist-cookies] flag with app-specific env var. Env var:
    [{APP_NAME}_PERSIST_COOKIES] *)

val verify_tls_term : string -> bool with_source Cmdliner.Term.t
(** Term for [--no-verify-tls] flag with app-specific env var. Env var:
    [{APP_NAME}_NO_VERIFY_TLS] *)

val timeout_term : string -> float option with_source Cmdliner.Term.t
(** Term for [--timeout SECONDS] option with app-specific env var. Env var:
    [{APP_NAME}_TIMEOUT] *)

val retries_term : string -> int with_source Cmdliner.Term.t
(** Term for [--max-retries N] option with app-specific env var. Env var:
    [{APP_NAME}_MAX_RETRIES] *)

val retry_backoff_term : string -> float with_source Cmdliner.Term.t
(** Term for [--retry-backoff FACTOR] option with app-specific env var. Env
    var: [{APP_NAME}_RETRY_BACKOFF] *)

val follow_redirects_term : string -> bool with_source Cmdliner.Term.t
(** Term for [--no-follow-redirects] flag with app-specific env var. Env var:
    [{APP_NAME}_NO_FOLLOW_REDIRECTS] *)

val max_redirects_term : string -> int with_source Cmdliner.Term.t
(** Term for [--max-redirects N] option with app-specific env var. Env var:
    [{APP_NAME}_MAX_REDIRECTS] *)

val user_agent_term : string -> string option with_source Cmdliner.Term.t
(** Term for [--user-agent STRING] option with app-specific env var. Env var:
    [{APP_NAME}_USER_AGENT] *)

val verbose_http_term : string -> bool with_source Cmdliner.Term.t
(** Term for [--verbose-http] flag with app-specific env var.

    Enables verbose HTTP-level logging: libcurl's own transfer chatter on
    stderr, and debug logging on this library's [Logs] source. Typically used
    in conjunction with debug-level logging. Env var:
    [{APP_NAME}_VERBOSE_HTTP] *)

val proxy_term : string -> proxy_config Cmdliner.Term.t
(** Term for [--proxy URL] and [--no-proxy HOSTS] options.

    Provides cmdliner integration for proxy configuration with proper source
    tracking. Environment variables are checked in order: HTTP_PROXY,
    http_proxy, HTTPS_PROXY, https_proxy, ALL_PROXY, all_proxy.

    {b Generated Flags:}
    - [--proxy URL]: HTTP/HTTPS proxy URL (e.g., http://proxy:8080)
    - [--no-proxy HOSTS]: Comma-separated list of hosts to bypass proxy
      (parsed for compatibility; see the {{!mapping}mapping})

    {b Environment Variables:}
    - [HTTP_PROXY] / [http_proxy]: HTTP proxy URL
    - [HTTPS_PROXY] / [https_proxy]: HTTPS proxy URL
    - [ALL_PROXY] / [all_proxy]: Fallback proxy URL for all protocols
    - [NO_PROXY] / [no_proxy]: Hosts to bypass proxy *)

(** {1 Combined Terms} *)

val config_term :
  string -> Eio.Fs.dir_ty Eio.Path.t -> config Cmdliner.Term.t
(** [config_term app_name fs] creates a complete configuration term.

    This combines all individual terms plus XDG configuration into a single
    term. All values include source tracking.

    {b Generated Flags:}
    - [--config-dir DIR]: Configuration directory
    - [--data-dir DIR]: Data directory
    - [--cache-dir DIR]: Cache directory
    - [--persist-cookies]: Enable cookie persistence
    - [--no-verify-tls]: Disable TLS verification
    - [--timeout SECONDS]: Request timeout
    - [--max-retries N]: Maximum retries
    - [--retry-backoff FACTOR]: Retry backoff multiplier
    - [--no-follow-redirects]: Disable redirect following
    - [--max-redirects N]: Maximum redirects to follow
    - [--user-agent STRING]: User-Agent header
    - [--verbose-http]: Enable verbose HTTP-level logging
    - [--proxy URL]: HTTP/HTTPS proxy URL
    - [--no-proxy HOSTS]: Hosts to bypass proxy

    {b Example:}
    {[
      let open Cmdliner in
      let config_t = Fetch_cmdliner.config_term "myapp" env#fs in
      let main config =
        Eio.Switch.run @@ fun sw ->
        let t = Fetch_cmdliner.create config env sw in
        ...
      in
      let cmd = Cmd.v info Term.(const main $ config_t) in
      Cmd.eval cmd
    ]} *)

val client_term :
  string ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Eio.Switch.t ->
  Eio.Fs.dir_ty Eio.Path.t ->
  Fetch.plain Cmdliner.Term.t
(** [client_term app_name env sw fs] creates a term that directly produces a
    client, combining {!config_term} with {!create}. The filesystem argument is
    explicit rather than taken from [env], so that a narrowed directory can be
    handed to the XDG layer. *)

val minimal_term :
  string -> Eio.Fs.dir_ty Eio.Path.t -> (Xdge.t * bool) Cmdliner.Term.t
(** [minimal_term app_name fs] creates a minimal configuration term.

    This only provides:
    - [--cache-dir DIR]: Cache directory for responses
    - [--data-dir DIR]: Data directory
    - [--persist-cookies]: Cookie persistence flag

    Returns the XDG context and persist_cookies boolean (without source
    tracking for simplified usage). *)

(** {1 Documentation and Pretty-Printing} *)

val env_docs : string -> string
(** [env_docs app_name] generates environment variable documentation.

    Returns formatted documentation for all environment variables that affect
    the configuration, including XDG variables and proxy settings. *)

val pp_source : Format.formatter -> source -> unit
(** Pretty print a source type. Output format: "default", "env(VAR_NAME)", or
    "cmdline" *)

val pp_with_source :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a with_source ->
  unit
(** [pp_with_source pp_val ppf ws] pretty prints a value with its source.
    Output format: "value [source]" *)

val pp_config : ?show_sources:bool -> Format.formatter -> config -> unit
(** [pp_config ?show_sources ppf config] pretty prints configuration for
    debugging.

    @param show_sources
      If true (default), shows the source of each value (e.g., "default",
      "env(VAR_NAME)", "cmdline"). If false, only shows the values without
      source annotations. *)

(** {1 Logging Configuration} *)

val src : Logs.Src.t
(** [src] is this library's log source, named ["fetch.cmdliner"]. Neither
    [fetch] nor [fetch-curl] defines one, so this is what
    {!setup_log_sources} raises when [--verbose-http] is given; libcurl's own
    chatter is enabled separately, through [Fetch_curl.v ~verbose]. *)

val setup_log_sources : ?verbose_http:bool -> Logs.level option -> unit
(** [setup_log_sources ~verbose_http level] configures the log sources this
    library governs. It is designed to work with [Logs_cli].

    {b Log Level Behavior:}
    - [Some Debug]: sets {!src} to debug. With [verbose_http], the
      ["tls.tracing"] source, if the process links one, also goes to debug;
      without it, that source is held at warning to suppress hexdumps.
    - [Some Info]: sets {!src} to info and ["tls.tracing"] to warning.
    - [None] or other levels: holds ["tls.tracing"] at warning.

    The [requests] version of this function drove a dozen per-module sources.
    [fetch] and [fetch-curl] have none — transport chatter comes from libcurl,
    which {!create} enables from [verbose_http] — so only {!src} and the TLS
    source remain.

    {b Example with Logs_cli:}
    {[
      let setup_logging =
        let open Cmdliner.Term in
        const (fun style level verbose_http ->
            Fmt_tty.setup_std_outputs ?style_renderer:style ();
            Logs.set_level level;
            Logs.set_reporter (Logs_fmt.reporter ());
            Fetch_cmdliner.setup_log_sources
              ~verbose_http:verbose_http.Fetch_cmdliner.value level)
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ()
        $ Fetch_cmdliner.verbose_http_term "myapp"
    ]} *)
