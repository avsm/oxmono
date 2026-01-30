(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Proxy Configuration

    Per RFC 9110 Section 3.7 and Section 7.3.2:
    A proxy is a message-forwarding agent chosen by the client,
    usually configured via local rules. *)

let src = Logs.Src.create "requests.proxy" ~doc:"HTTP Proxy Support"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Proxy Types} *)

type proxy_type =
  | HTTP
  | SOCKS5

type config = {
  host : string;
  port : int;
  proxy_type : proxy_type;
  auth : Auth.t option;
  no_proxy : string list;
}

(** {1 Configuration Constructors} *)

let http ?(port = 8080) ?auth ?(no_proxy = []) host =
  Log.debug (fun m -> m "Creating HTTP proxy config: %s:%d" host port);
  { host; port; proxy_type = HTTP; auth; no_proxy }

let socks5 ?(port = 1080) ?auth ?(no_proxy = []) host =
  Log.debug (fun m -> m "Creating SOCKS5 proxy config: %s:%d" host port);
  { host; port; proxy_type = SOCKS5; auth; no_proxy }

(** {1 Pattern Matching for NO_PROXY} *)

(** Check if a hostname matches a no_proxy pattern.
    Supports:
    - Exact match: "example.com"
    - Wildcard prefix: "*.example.com" matches foo.example.com
    - Dot prefix: ".example.com" matches example.com and foo.example.com *)
let host_matches_pattern ~host pattern =
  let host_lower = String.lowercase_ascii host in
  let pattern_lower = String.lowercase_ascii (String.trim pattern) in
  match String.length pattern_lower with
  | 0 -> false
  | _ when pattern_lower.[0] = '*' ->
      (* Wildcard pattern: *.example.com matches foo.example.com *)
      let suffix = String.sub pattern_lower 1 (String.length pattern_lower - 1) in
      String.length host_lower >= String.length suffix &&
      String.sub host_lower
        (String.length host_lower - String.length suffix)
        (String.length suffix) = suffix
  | _ when pattern_lower.[0] = '.' ->
      (* .example.com matches example.com and foo.example.com *)
      host_lower = String.sub pattern_lower 1 (String.length pattern_lower - 1) ||
      (String.length host_lower > String.length pattern_lower &&
       String.sub host_lower
         (String.length host_lower - String.length pattern_lower)
         (String.length pattern_lower) = pattern_lower)
  | _ ->
      (* Exact match *)
      host_lower = pattern_lower

(** {1 Configuration Utilities} *)

let should_bypass config url =
  let uri = Uri.of_string url in
  let target_host = Uri.host uri |> Option.value ~default:"" in
  let bypassed = List.exists (host_matches_pattern ~host:target_host) config.no_proxy in
  if bypassed then
    Log.debug (fun m -> m "URL %s bypasses proxy (matches no_proxy pattern)"
      (Error.sanitize_url url));
  bypassed

let host_port config = (config.host, config.port)

(** Validate that the proxy type is supported.
    Currently only HTTP proxies are implemented.
    @raise Error.Proxy_error if SOCKS5 is requested *)
let validate_supported config =
  match config.proxy_type with
  | HTTP -> ()
  | SOCKS5 ->
      Log.err (fun m -> m "SOCKS5 proxy requested but not implemented");
      raise (Error.err (Error.Proxy_error {
        host = config.host;
        reason = "SOCKS5 proxy is not yet implemented"
      }))

(** {1 Environment Variable Support} *)

let get_env key =
  try Some (Sys.getenv key) with Not_found -> None

let get_env_insensitive key =
  match get_env key with
  | Some v -> Some v
  | None -> get_env (String.lowercase_ascii key)

let parse_no_proxy () =
  let no_proxy_str =
    match get_env "NO_PROXY" with
    | Some v -> v
    | None ->
        match get_env "no_proxy" with
        | Some v -> v
        | None -> ""
  in
  no_proxy_str
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)

let parse_proxy_url url =
  let uri = Uri.of_string url in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port = Uri.port uri |> Option.value ~default:8080 in
  let auth = match Uri.userinfo uri with
    | Some info ->
        (match String.index_opt info ':' with
         | Some idx ->
             let username = String.sub info 0 idx in
             let password = String.sub info (idx + 1) (String.length info - idx - 1) in
             Some (Auth.basic ~username ~password)
         | None ->
             (* Username only, no password *)
             Some (Auth.basic ~username:info ~password:""))
    | None -> None
  in
  (host, port, auth)

let from_env () =
  let no_proxy = parse_no_proxy () in
  let proxy_url =
    match get_env_insensitive "HTTP_PROXY" with
    | Some url -> Some url
    | None ->
        match get_env_insensitive "HTTPS_PROXY" with
        | Some url -> Some url
        | None -> get_env_insensitive "ALL_PROXY"
  in
  match proxy_url with
  | Some url ->
      let (host, port, auth) = parse_proxy_url url in
      Log.info (fun m -> m "Proxy configured from environment: %s:%d" host port);
      Some { host; port; proxy_type = HTTP; auth; no_proxy }
  | None ->
      Log.debug (fun m -> m "No proxy configured in environment");
      None

let from_env_for_url url =
  let uri = Uri.of_string url in
  let is_https = Uri.scheme uri = Some "https" in
  let no_proxy = parse_no_proxy () in

  (* Check if URL should bypass proxy *)
  let target_host = Uri.host uri |> Option.value ~default:"" in
  let should_bypass_url =
    List.exists (host_matches_pattern ~host:target_host) no_proxy
  in

  if should_bypass_url then begin
    Log.debug (fun m -> m "URL %s bypasses proxy (matches NO_PROXY)"
      (Error.sanitize_url url));
    None
  end
  else
    let proxy_url =
      if is_https then
        match get_env_insensitive "HTTPS_PROXY" with
        | Some url -> Some url
        | None -> get_env_insensitive "ALL_PROXY"
      else
        match get_env_insensitive "HTTP_PROXY" with
        | Some url -> Some url
        | None -> get_env_insensitive "ALL_PROXY"
    in
    match proxy_url with
    | Some purl ->
        let (host, port, auth) = parse_proxy_url purl in
        Log.debug (fun m -> m "Using proxy %s:%d for URL %s"
          host port (Error.sanitize_url url));
        Some { host; port; proxy_type = HTTP; auth; no_proxy }
    | None -> None

(** {1 Pretty Printing} *)

let pp_proxy_type ppf = function
  | HTTP -> Format.fprintf ppf "HTTP"
  | SOCKS5 -> Format.fprintf ppf "SOCKS5"

let pp_config ppf config =
  Format.fprintf ppf "@[<v>Proxy Configuration:@,";
  Format.fprintf ppf "  Type: %a@," pp_proxy_type config.proxy_type;
  Format.fprintf ppf "  Host: %s@," config.host;
  Format.fprintf ppf "  Port: %d@," config.port;
  Format.fprintf ppf "  Auth: %s@,"
    (if Option.is_some config.auth then "[CONFIGURED]" else "None");
  Format.fprintf ppf "  No-proxy: [%s]@]"
    (String.concat ", " config.no_proxy)
