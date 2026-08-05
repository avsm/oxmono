(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Header Value Parsing

    This module provides parsing and generation functions for complex HTTP header
    values that go beyond simple strings.

    @see <https://www.rfc-editor.org/rfc/rfc9110> RFC 9110: HTTP Semantics *)

let src = Logs.Src.create "requests.header_parsing" ~doc:"HTTP Header Parsing"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Content-Range (RFC 9110 Section 14.4)}

    The Content-Range header indicates which part of a representation is
    enclosed when a 206 (Partial Content) response is returned.

    Format: [bytes start-end/complete-length] or [bytes */complete-length]

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.4> RFC 9110 Section 14.4 *)

type content_range = {
  unit : string;
  (** The range unit, typically "bytes" *)
  range : (int64 * int64) option;
  (** The byte range (start, end) inclusive, or None for unsatisfied range *)
  complete_length : int64 option;
  (** The complete representation length, or None if unknown *)
}

let content_range_to_string cr =
  let range_part = match cr.range with
    | Some (start, end_) -> Printf.sprintf "%Ld-%Ld" start end_
    | None -> "*"
  in
  let length_part = match cr.complete_length with
    | Some len -> Int64.to_string len
    | None -> "*"
  in
  Printf.sprintf "%s %s/%s" cr.unit range_part length_part

let parse_content_range s =
  let s = String.trim s in
  (* Parse unit (e.g., "bytes") *)
  match String.index_opt s ' ' with
  | None ->
      Log.debug (fun m -> m "Content-Range missing unit separator: %s" s);
      None
  | Some space_idx ->
      let unit = String.sub s 0 space_idx in
      let rest = String.sub s (space_idx + 1) (String.length s - space_idx - 1) in
      (* Parse range/length *)
      match String.index_opt rest '/' with
      | None ->
          Log.debug (fun m -> m "Content-Range missing range/length separator: %s" s);
          None
      | Some slash_idx ->
          let range_part = String.sub rest 0 slash_idx in
          let length_part = String.sub rest (slash_idx + 1) (String.length rest - slash_idx - 1) in
          (* Parse range *)
          let range =
            if range_part = "*" then None
            else match String.index_opt range_part '-' with
              | None -> None
              | Some dash_idx ->
                  let start_s = String.sub range_part 0 dash_idx in
                  let end_s = String.sub range_part (dash_idx + 1) (String.length range_part - dash_idx - 1) in
                  match Int64.of_string_opt start_s, Int64.of_string_opt end_s with
                  | Some start, Some end_ -> Some (start, end_)
                  | _ ->
                      Log.debug (fun m -> m "Content-Range invalid range numbers: %s" range_part);
                      None
          in
          (* Parse complete length *)
          let complete_length =
            if length_part = "*" then None
            else Int64.of_string_opt length_part
          in
          Some { unit; range; complete_length }

(** Create a Content-Range value for a byte range response.

    @param start The first byte position (0-indexed)
    @param end_ The last byte position (inclusive)
    @param complete_length The total size of the representation *)
let make_content_range ~start ~end_ ~complete_length =
  { unit = "bytes"; range = Some (start, end_); complete_length = Some complete_length }

(** Create a Content-Range value for an unsatisfied range (416 response).

    @param complete_length The total size of the representation *)
let make_unsatisfied_range ~complete_length =
  { unit = "bytes"; range = None; complete_length = Some complete_length }

(** {1 If-Range (RFC 9110 Section 13.1.5)}

    The If-Range header makes a Range request conditional. It can contain
    either an ETag or a Last-Modified date.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5> RFC 9110 Section 13.1.5 *)

type if_range =
  | If_range_etag of string
  (** An entity tag (strong or weak) *)
  | If_range_date of string
  (** A Last-Modified date in HTTP-date format *)

let if_range_to_string = function
  | If_range_etag etag -> etag
  | If_range_date date -> date

(** Parse an If-Range header value.

    Distinguishes between ETags (contain quotes or start with W/) and
    HTTP-date values (start with a weekday abbreviation).

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3> RFC 9110 Section 8.8.3 (ETag)
    @see <https://www.rfc-editor.org/rfc/rfc9110#section-5.6.7> RFC 9110 Section 5.6.7 (HTTP-date) *)
let parse_if_range s =
  let s = String.trim s in
  if String.length s = 0 then None
  else
    (* ETags are quoted strings or start with W/ for weak ETags *)
    let is_etag =
      (* Strong ETag: starts with quote *)
      (String.length s >= 2 && s.[0] = '"') ||
      (* Weak ETag: starts with W/ followed by quote *)
      (String.length s >= 3 && s.[0] = 'W' && s.[1] = '/' && s.[2] = '"')
    in
    if is_etag then
      Some (If_range_etag s)
    else
      (* HTTP-date starts with a weekday: Mon, Tue, Wed, Thu, Fri, Sat, Sun
         or in obsolete formats: Monday, Tuesday, etc. *)
      let weekdays = ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun";
                      "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"; "Sunday"] in
      let starts_with_weekday =
        List.exists (fun day ->
          String.length s >= String.length day &&
          String.sub s 0 (String.length day) = day
        ) weekdays
      in
      if starts_with_weekday then
        Some (If_range_date s)
      else
        (* Ambiguous - treat as date if it contains digits typical of dates *)
        if String.exists (fun c -> c >= '0' && c <= '9') s then
          Some (If_range_date s)
        else
          (* Default to ETag for other values *)
          Some (If_range_etag s)

(** Create an If-Range value from an ETag. *)
let if_range_of_etag etag = If_range_etag etag

(** Create an If-Range value from a Last-Modified date string. *)
let if_range_of_date date = If_range_date date

(** Check if an If-Range value is an ETag. *)
let if_range_is_etag = function
  | If_range_etag _ -> true
  | If_range_date _ -> false

(** Check if an If-Range value is a date. *)
let if_range_is_date = function
  | If_range_date _ -> true
  | If_range_etag _ -> false

(** {1 Allow (RFC 9110 Section 10.2.1)}

    The Allow header lists the set of methods supported by the target resource.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.1> RFC 9110 Section 10.2.1 *)

(** Parse an Allow header value into a list of methods.

    The Allow header is a comma-separated list of HTTP method names.
    Example: "GET, HEAD, PUT" *)
let parse_allow s =
  String.split_on_char ',' s
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> List.map Method.of_string

(** Format a list of methods as an Allow header value. *)
let allow_to_string methods =
  methods
  |> List.map Method.to_string
  |> String.concat ", "

(** Check if a method is in an Allow header value. *)
let allow_contains method_ allow_value =
  let methods = parse_allow allow_value in
  List.exists (Method.equal method_) methods

(** {1 Authentication-Info (RFC 9110 Section 11.6.3)}

    The Authentication-Info header is sent by the server after successful
    authentication. For Digest authentication, it provides a new nonce for
    subsequent requests (avoiding 401 round-trips) and response authentication.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.3> RFC 9110 Section 11.6.3 *)

type authentication_info = {
  nextnonce : string option;
  (** Next nonce to use for subsequent requests *)
  qop : string option;
  (** Quality of protection that was used *)
  rspauth : string option;
  (** Response authentication (server proves it knows the password) *)
  cnonce : string option;
  (** Client nonce echoed back *)
  nc : string option;
  (** Nonce count echoed back *)
}

(** Parse an Authentication-Info header value.

    Format is comma-separated key=value or key="value" pairs.
    Example: [nextnonce="abc123", qop=auth, rspauth="xyz789"] *)
let parse_authentication_info s =
  let pairs =
    let rec parse_pairs acc str =
      let str = String.trim str in
      if str = "" then List.rev acc
      else
        match String.index_opt str '=' with
        | None -> List.rev acc
        | Some eq_idx ->
            let key = String.trim (String.sub str 0 eq_idx) in
            let rest = String.sub str (eq_idx + 1) (String.length str - eq_idx - 1) in
            let rest = String.trim rest in
            let value, remaining =
              if String.length rest > 0 && rest.[0] = '"' then
                (* Quoted value *)
                match String.index_from_opt rest 1 '"' with
                | Some end_quote ->
                    let v = String.sub rest 1 (end_quote - 1) in
                    let rem = String.sub rest (end_quote + 1) (String.length rest - end_quote - 1) in
                    let rem = String.trim rem in
                    let rem = if String.length rem > 0 && rem.[0] = ',' then
                      String.sub rem 1 (String.length rem - 1)
                    else rem in
                    (v, rem)
                | None -> (rest, "")
              else
                (* Unquoted value *)
                match String.index_opt rest ',' with
                | Some comma ->
                    let v = String.trim (String.sub rest 0 comma) in
                    let rem = String.sub rest (comma + 1) (String.length rest - comma - 1) in
                    (v, rem)
                | None -> (String.trim rest, "")
            in
            parse_pairs ((String.lowercase_ascii key, value) :: acc) remaining
    in
    parse_pairs [] s
  in
  {
    nextnonce = List.assoc_opt "nextnonce" pairs;
    qop = List.assoc_opt "qop" pairs;
    rspauth = List.assoc_opt "rspauth" pairs;
    cnonce = List.assoc_opt "cnonce" pairs;
    nc = List.assoc_opt "nc" pairs;
  }

(** Check if the Authentication-Info contains a new nonce.

    If present, the client should use this nonce for subsequent requests
    instead of waiting for a 401 response with a new challenge. *)
let has_nextnonce info = Option.is_some info.nextnonce

(** Get the next nonce from Authentication-Info, if present. *)
let get_nextnonce info = info.nextnonce

(** {1 Retry-After (RFC 9110 Section 10.2.3)}

    The Retry-After header indicates how long to wait before retrying.
    It can be either a date or a number of seconds.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.3> RFC 9110 Section 10.2.3 *)

type retry_after =
  | Retry_after_date of string
  (** An HTTP-date when the resource will be available *)
  | Retry_after_seconds of int
  (** Number of seconds to wait before retrying *)

(** Parse a Retry-After header value. *)
let parse_retry_after s =
  let s = String.trim s in
  match int_of_string_opt s with
  | Some seconds -> Some (Retry_after_seconds seconds)
  | None ->
      (* Not a number, must be a date *)
      if String.length s > 0 then
        Some (Retry_after_date s)
      else
        None

(** Convert a Retry-After value to a delay in seconds.

    For date values, this requires the current time to compute the difference.
    Returns None if the date cannot be parsed. Returns 0 if the date is in the past.

    Per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.3}RFC 9110 Section 10.2.3}:
    "A delay-seconds value is a non-negative decimal integer, representing
    time in seconds."

    @param now The current time as a Unix timestamp *)
let retry_after_to_seconds ?now retry_after =
  match retry_after with
  | Retry_after_seconds s -> Some s
  | Retry_after_date date_str ->
      match now with
      | None ->
          Log.debug (fun m -> m "Retry-After date requires 'now' parameter: %s" date_str);
          None
      | Some now_ts ->
          match Http_date.parse date_str with
          | None ->
              Log.debug (fun m -> m "Failed to parse Retry-After HTTP-date: %s" date_str);
              None
          | Some ptime ->
              let date_ts = Ptime.to_float_s ptime in
              let diff = date_ts -. now_ts in
              (* Clamp to 0 if date is in the past *)
              Some (max 0 (int_of_float diff))

(** {1 Accept-Ranges (RFC 9110 Section 14.3)}

    The Accept-Ranges header indicates whether the server supports range requests.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.3> RFC 9110 Section 14.3 *)

type accept_ranges =
  | Accept_ranges_bytes
  (** Server supports byte range requests *)
  | Accept_ranges_none
  (** Server does not support range requests *)
  | Accept_ranges_other of string
  (** Server supports some other range unit *)

(** Parse an Accept-Ranges header value. *)
let parse_accept_ranges s =
  match String.lowercase_ascii (String.trim s) with
  | "bytes" -> Accept_ranges_bytes
  | "none" -> Accept_ranges_none
  | other -> Accept_ranges_other other

(** Check if the server supports byte range requests. *)
let supports_byte_ranges = function
  | Accept_ranges_bytes -> true
  | Accept_ranges_none | Accept_ranges_other _ -> false

(** {1 Cache-Status (RFC 9211)}

    The Cache-Status header field indicates how caches have handled a request.
    It is a List structured field (RFC 8941) where each member is a cache
    identifier with optional parameters.

    @see <https://www.rfc-editor.org/rfc/rfc9211> RFC 9211: The Cache-Status HTTP Response Header Field *)

(** Forward/stored response indicator for Cache-Status *)
type cache_status_fwd =
  | Fwd_uri_miss
  (** The cache did not contain any matching response *)
  | Fwd_vary_miss
  (** The cache contained a response, but Vary header prevented match *)
  | Fwd_miss
  (** The cache did not find a usable response (generic) *)
  | Fwd_request
  (** The request semantics required forwarding (e.g., no-cache) *)
  | Fwd_stale
  (** The cache had a stale response that needed revalidation *)
  | Fwd_partial
  (** The cache had a partial response that needed completion *)
  | Fwd_bypass
  (** The cache was configured to bypass for this request *)
  | Fwd_other of string
  (** Other forward reason *)

(** A single cache status entry from the Cache-Status header *)
type cache_status_entry = {
  cache_id : string;
  (** Identifier for the cache (e.g., "CDN", "proxy", "Cloudflare") *)
  hit : bool option;
  (** True if served from cache without forwarding *)
  fwd : cache_status_fwd option;
  (** Why the request was forwarded *)
  fwd_status : int option;
  (** Status code from the forwarded response *)
  stored : bool option;
  (** Whether the response was stored in cache *)
  collapsed : bool option;
  (** Whether request was collapsed with others *)
  ttl : int option;
  (** Time-to-live remaining in seconds *)
  key : string option;
  (** Cache key used *)
  detail : string option;
  (** Implementation-specific detail *)
}

let cache_status_fwd_of_string = function
  | "uri-miss" -> Fwd_uri_miss
  | "vary-miss" -> Fwd_vary_miss
  | "miss" -> Fwd_miss
  | "request" -> Fwd_request
  | "stale" -> Fwd_stale
  | "partial" -> Fwd_partial
  | "bypass" -> Fwd_bypass
  | other -> Fwd_other other

let cache_status_fwd_to_string = function
  | Fwd_uri_miss -> "uri-miss"
  | Fwd_vary_miss -> "vary-miss"
  | Fwd_miss -> "miss"
  | Fwd_request -> "request"
  | Fwd_stale -> "stale"
  | Fwd_partial -> "partial"
  | Fwd_bypass -> "bypass"
  | Fwd_other s -> s

(** Parse a single Cache-Status entry.
    Format: cache-id; param1; param2=value; param3="quoted" *)
let parse_cache_status_entry s =
  let s = String.trim s in
  let parts = String.split_on_char ';' s in
  match parts with
  | [] -> None
  | cache_id_part :: params ->
      let cache_id = String.trim cache_id_part in
      if cache_id = "" then None
      else
        let parse_param acc p =
          let p = String.trim p in
          match String.index_opt p '=' with
          | None ->
              (* Boolean parameter (presence = true) *)
              (String.lowercase_ascii p, "?1") :: acc
          | Some eq_idx ->
              let key = String.trim (String.sub p 0 eq_idx) in
              let value = String.trim (String.sub p (eq_idx + 1) (String.length p - eq_idx - 1)) in
              (* Remove quotes if present *)
              let value =
                if String.length value >= 2 && value.[0] = '"' && value.[String.length value - 1] = '"' then
                  String.sub value 1 (String.length value - 2)
                else value
              in
              (String.lowercase_ascii key, value) :: acc
        in
        let param_list = List.fold_left parse_param [] params in
        let get_bool key =
          match List.assoc_opt key param_list with
          | Some "?1" | Some "true" | Some "1" -> Some true
          | Some "?0" | Some "false" | Some "0" -> Some false
          | _ -> None
        in
        let get_int key =
          match List.assoc_opt key param_list with
          | Some v -> int_of_string_opt v
          | None -> None
        in
        let get_string key = List.assoc_opt key param_list in
        Some {
          cache_id;
          hit = get_bool "hit";
          fwd = Option.map cache_status_fwd_of_string (get_string "fwd");
          fwd_status = get_int "fwd-status";
          stored = get_bool "stored";
          collapsed = get_bool "collapsed";
          ttl = get_int "ttl";
          key = get_string "key";
          detail = get_string "detail";
        }

(** Parse a Cache-Status header value into a list of entries.
    Multiple caches are separated by commas, with the response generator first. *)
let parse_cache_status s =
  String.split_on_char ',' s
  |> List.filter_map parse_cache_status_entry

(** Format a Cache-Status entry as a string. *)
let cache_status_entry_to_string entry =
  let params = [] in
  let params = match entry.detail with Some v -> ("detail", Printf.sprintf "\"%s\"" v) :: params | None -> params in
  let params = match entry.key with Some v -> ("key", Printf.sprintf "\"%s\"" v) :: params | None -> params in
  let params = match entry.ttl with Some v -> ("ttl", string_of_int v) :: params | None -> params in
  let params = match entry.collapsed with Some true -> ("collapsed", "") :: params | _ -> params in
  let params = match entry.stored with Some true -> ("stored", "") :: params | _ -> params in
  let params = match entry.fwd_status with Some v -> ("fwd-status", string_of_int v) :: params | None -> params in
  let params = match entry.fwd with Some v -> ("fwd", cache_status_fwd_to_string v) :: params | None -> params in
  let params = match entry.hit with Some true -> ("hit", "") :: params | _ -> params in
  let param_strs = List.map (fun (k, v) ->
    if v = "" then k else k ^ "=" ^ v
  ) params in
  match param_strs with
  | [] -> entry.cache_id
  | _ -> entry.cache_id ^ "; " ^ String.concat "; " param_strs

(** Format a list of Cache-Status entries as a header value. *)
let cache_status_to_string entries =
  String.concat ", " (List.map cache_status_entry_to_string entries)

(** Check if any cache reported a hit. *)
let cache_status_is_hit entries =
  List.exists (fun e -> e.hit = Some true) entries

(** Check if the response was stored by any cache. *)
let cache_status_is_stored entries =
  List.exists (fun e -> e.stored = Some true) entries

(** Get the forward reason from the first cache that forwarded. *)
let cache_status_get_fwd entries =
  List.find_map (fun e -> e.fwd) entries

(** {1 Content-Digest / Repr-Digest (RFC 9530)}

    Content-Digest contains a digest of the content (after content coding).
    Repr-Digest contains a digest of the representation (before content coding).

    Format: algorithm=:base64-digest:, algorithm=:base64-digest:

    @see <https://www.rfc-editor.org/rfc/rfc9530> RFC 9530: Digest Fields *)

(** Supported digest algorithms *)
type digest_algorithm =
  | Sha256
  (** SHA-256 (recommended) *)
  | Sha512
  (** SHA-512 *)
  | Other of string
  (** Other algorithm (for forward compatibility) *)

let digest_algorithm_of_string s =
  match String.lowercase_ascii s with
  | "sha-256" -> Sha256
  | "sha-512" -> Sha512
  | other -> Other other

let digest_algorithm_to_string = function
  | Sha256 -> "sha-256"
  | Sha512 -> "sha-512"
  | Other s -> s

(** A single digest value with its algorithm *)
type digest_value = {
  algorithm : digest_algorithm;
  (** The hash algorithm used *)
  digest : string;
  (** The base64-encoded digest value *)
}

(** Parse a Content-Digest or Repr-Digest header value.
    Format: sha-256=:base64data:, sha-512=:base64data: *)
let parse_digest_header s =
  String.split_on_char ',' s
  |> List.filter_map (fun part ->
      let part = String.trim part in
      match String.index_opt part '=' with
      | None -> None
      | Some eq_idx ->
          let algo = String.trim (String.sub part 0 eq_idx) in
          let value = String.trim (String.sub part (eq_idx + 1) (String.length part - eq_idx - 1)) in
          (* RFC 9530 uses :base64: format for byte sequences *)
          let digest =
            if String.length value >= 2 && value.[0] = ':' && value.[String.length value - 1] = ':' then
              String.sub value 1 (String.length value - 2)
            else value
          in
          Some { algorithm = digest_algorithm_of_string algo; digest }
    )

(** Format a digest value as a string. *)
let digest_value_to_string dv =
  Printf.sprintf "%s=:%s:" (digest_algorithm_to_string dv.algorithm) dv.digest

(** Format a list of digest values as a header value. *)
let digest_header_to_string digests =
  String.concat ", " (List.map digest_value_to_string digests)

(** Compute the SHA-256 digest of content and return base64-encoded result. *)
let compute_sha256 content =
  let hash = Digestif.SHA256.digest_string content in
  Base64.encode_string (Digestif.SHA256.to_raw_string hash)

(** Compute the SHA-512 digest of content and return base64-encoded result. *)
let compute_sha512 content =
  let hash = Digestif.SHA512.digest_string content in
  Base64.encode_string (Digestif.SHA512.to_raw_string hash)

(** Compute a digest for content using the specified algorithm. *)
let compute_digest ~algorithm content =
  let digest = match algorithm with
    | Sha256 -> compute_sha256 content
    | Sha512 -> compute_sha512 content
    | Other _ ->
        Log.warn (fun m -> m "Unsupported digest algorithm, using SHA-256");
        compute_sha256 content
  in
  { algorithm; digest }

(** Create a Content-Digest header value for content.
    Defaults to SHA-256 which is recommended by RFC 9530. *)
let make_content_digest ?(algorithm = Sha256) content =
  compute_digest ~algorithm content

(** Validate that a digest matches the content.
    Returns true if any of the provided digests matches. *)
let validate_digest ~digests content =
  List.exists (fun dv ->
    let computed = compute_digest ~algorithm:dv.algorithm content in
    computed.digest = dv.digest
  ) digests

(** Get the strongest available digest (prefer SHA-512 over SHA-256). *)
let get_strongest_digest digests =
  let sha512 = List.find_opt (fun d -> d.algorithm = Sha512) digests in
  let sha256 = List.find_opt (fun d -> d.algorithm = Sha256) digests in
  match sha512, sha256 with
  | Some d, _ -> Some d
  | None, Some d -> Some d
  | None, None -> List.nth_opt digests 0

(** {1 Strict-Transport-Security (RFC 6797)}

    The Strict-Transport-Security (HSTS) header tells browsers to only
    access the site over HTTPS.

    @see <https://www.rfc-editor.org/rfc/rfc6797> RFC 6797: HTTP Strict Transport Security *)

(** HSTS directive values *)
type hsts = {
  max_age : int64;
  (** Required: Time in seconds the browser should remember HTTPS-only *)
  include_subdomains : bool;
  (** If true, policy applies to all subdomains *)
  preload : bool;
  (** If true, site requests inclusion in browser preload lists *)
}

(** Parse a Strict-Transport-Security header value.
    Format: max-age=31536000; includeSubDomains; preload *)
let parse_hsts s =
  let directives =
    String.split_on_char ';' s
    |> List.map String.trim
    |> List.filter (fun s -> String.length s > 0)
  in
  let max_age = ref None in
  let include_subdomains = ref false in
  let preload = ref false in
  List.iter (fun directive ->
    let directive_lower = String.lowercase_ascii directive in
    if String.length directive_lower >= 8 &&
       String.sub directive_lower 0 8 = "max-age=" then begin
      let value_str = String.sub directive 8 (String.length directive - 8) in
      max_age := Int64.of_string_opt (String.trim value_str)
    end
    else if directive_lower = "includesubdomains" then
      include_subdomains := true
    else if directive_lower = "preload" then
      preload := true
    else
      Log.debug (fun m -> m "Unknown HSTS directive: %s" directive)
  ) directives;
  match !max_age with
  | Some age -> Some {
      max_age = age;
      include_subdomains = !include_subdomains;
      preload = !preload;
    }
  | None ->
      Log.debug (fun m -> m "HSTS header missing required max-age directive");
      None

(** Format an HSTS value as a header string. *)
let hsts_to_string hsts =
  let parts = [Printf.sprintf "max-age=%Ld" hsts.max_age] in
  let parts = if hsts.include_subdomains then parts @ ["includeSubDomains"] else parts in
  let parts = if hsts.preload then parts @ ["preload"] else parts in
  String.concat "; " parts

(** Create an HSTS header value.
    @param max_age Time in seconds (default: 1 year = 31536000)
    @param include_subdomains Apply to subdomains (default: false)
    @param preload Request preload list inclusion (default: false) *)
let make_hsts ?(max_age = 31536000L) ?(include_subdomains = false) ?(preload = false) () =
  { max_age; include_subdomains; preload }

(** Check if HSTS is effectively enabled (max-age > 0). *)
let hsts_is_enabled hsts = hsts.max_age > 0L

(** Common HSTS configurations *)

(** One year with subdomains - recommended for production *)
let hsts_one_year_subdomains = { max_age = 31536000L; include_subdomains = true; preload = false }

(** Two years with subdomains and preload - for HSTS preload submission *)
let hsts_preload = { max_age = 63072000L; include_subdomains = true; preload = true }

(** Disable HSTS by setting max-age to 0 *)
let hsts_disable = { max_age = 0L; include_subdomains = false; preload = false }
