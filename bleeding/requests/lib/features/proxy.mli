(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Proxy Configuration

    Per RFC 9110 Section 3.7 and Section 7.3.2:
    A proxy is a message-forwarding agent chosen by the client,
    usually configured via local rules.

    {2 Usage}

    Create a proxy configuration:
    {[
      let proxy = Proxy.http ~port:8080 "proxy.example.com"

      (* With authentication *)
      let proxy = Proxy.http
        ~port:8080
        ~auth:(Auth.basic ~username:"user" ~password:"pass")
        "proxy.example.com"

      (* With bypass list *)
      let proxy = Proxy.http
        ~no_proxy:["localhost"; "*.internal.example.com"]
        "proxy.example.com"
    ]}

    Read from environment variables:
    {[
      match Proxy.from_env () with
      | Some proxy -> (* use proxy *)
      | None -> (* no proxy configured *)
    ]} *)

(** Log source for proxy operations *)
val src : Logs.Src.t

(** {1 Proxy Types} *)

(** Proxy protocol type *)
type proxy_type =
  | HTTP   (** HTTP proxy (CONNECT for HTTPS, absolute-URI for HTTP) *)
  | SOCKS5 (** SOCKS5 proxy (RFC 1928) - future extension *)

(** Proxy configuration *)
type config = {
  host : string;          (** Proxy server hostname *)
  port : int;             (** Proxy server port (default: 8080) *)
  proxy_type : proxy_type;
  auth : Auth.t option;   (** Proxy authentication (Proxy-Authorization) *)
  no_proxy : string list; (** Hosts/patterns to bypass proxy *)
}

(** {1 Configuration Constructors} *)

val http : ?port:int -> ?auth:Auth.t -> ?no_proxy:string list -> string -> config
(** [http ?port ?auth ?no_proxy host] creates an HTTP proxy configuration.

    @param port Proxy port (default: 8080)
    @param auth Proxy authentication credentials
    @param no_proxy List of hosts/patterns to bypass the proxy.
           Supports wildcards like [*.example.com] to match [foo.example.com].
    @param host Proxy server hostname *)

val socks5 : ?port:int -> ?auth:Auth.t -> ?no_proxy:string list -> string -> config
(** [socks5 ?port ?auth ?no_proxy host] creates a SOCKS5 proxy configuration.

    {b Note:} SOCKS5 support is not yet implemented. This function creates
    the configuration type for future use.

    @param port Proxy port (default: 1080)
    @param auth Proxy authentication credentials
    @param no_proxy List of hosts/patterns to bypass the proxy
    @param host Proxy server hostname *)

(** {1 Configuration Utilities} *)

val should_bypass : config -> string -> bool
(** [should_bypass config url] returns [true] if [url] should bypass
    the proxy based on the [no_proxy] list.

    Matching rules:
    - Exact hostname match (case-insensitive)
    - Wildcard prefix match: [*.example.com] matches [foo.example.com]
    - [localhost] and [127.0.0.1] match by default if in no_proxy list *)

val host_port : config -> string * int
(** [host_port config] returns the proxy host and port as a tuple. *)

val validate_supported : config -> unit
(** [validate_supported config] checks that the proxy type is currently supported.
    @raise Error.Proxy_error if SOCKS5 is requested (not yet implemented) *)

(** {1 Environment Variable Support} *)

val from_env : unit -> config option
(** [from_env ()] reads proxy configuration from environment variables.

    Checks the following variables (in order of preference):
    - [HTTP_PROXY] / [http_proxy]
    - [HTTPS_PROXY] / [https_proxy]
    - [ALL_PROXY] / [all_proxy]
    - [NO_PROXY] / [no_proxy] (comma-separated list of bypass patterns)

    Returns [None] if no proxy is configured.

    URL format: [http://[user:pass@]host[:port]]

    Example environment:
    {[
      HTTP_PROXY=http://user:pass@proxy.example.com:8080
      NO_PROXY=localhost,*.internal.example.com,.example.org
    ]} *)

val from_env_for_url : string -> config option
(** [from_env_for_url url] reads proxy configuration appropriate for [url].

    - Uses [HTTPS_PROXY] for HTTPS URLs
    - Uses [HTTP_PROXY] for HTTP URLs
    - Falls back to [ALL_PROXY]
    - Returns [None] if the URL matches [NO_PROXY] patterns *)

(** {1 Pretty Printing} *)

val pp_proxy_type : Format.formatter -> proxy_type -> unit
(** Pretty printer for proxy type *)

val pp_config : Format.formatter -> config -> unit
(** Pretty printer for proxy configuration.
    Note: Authentication credentials are redacted. *)
