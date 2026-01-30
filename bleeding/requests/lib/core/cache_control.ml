(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Cache-Control header parsing per RFC 9111 (HTTP Caching)

    This module provides parsing and representation of Cache-Control directives
    for both requests and responses. It supports all standard directives from
    RFC 9111 Section 5.2.

    Per Recommendation #17: Response Caching with RFC 7234/9111 Compliance *)

let src = Logs.Src.create "requests.cache_control" ~doc:"HTTP Cache-Control"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Response Cache-Control Directives}

    RFC 9111 Section 5.2.2: Cache-Control Response Directives *)

type response_directive =
  | Max_age of int              (** max-age=N - response is fresh for N seconds *)
  | S_maxage of int             (** s-maxage=N - shared cache max-age *)
  | No_cache of string list     (** no-cache[=headers] - must revalidate *)
  | No_store                    (** no-store - must not be stored *)
  | No_transform                (** no-transform - must not be transformed *)
  | Must_revalidate             (** must-revalidate - stale must be revalidated *)
  | Proxy_revalidate            (** proxy-revalidate - shared caches must revalidate *)
  | Must_understand             (** must-understand - RFC 9111 *)
  | Private of string list      (** private[=headers] - only private cache *)
  | Public                      (** public - can be stored by any cache *)
  | Immutable                   (** immutable - will not change during freshness *)
  | Stale_while_revalidate of int  (** stale-while-revalidate=N *)
  | Stale_if_error of int       (** stale-if-error=N *)
  | Response_extension of string * string option  (** Unknown directive *)

(** {1 Request Cache-Control Directives}

    RFC 9111 Section 5.2.1: Cache-Control Request Directives *)

type request_directive =
  | Req_max_age of int          (** max-age=N *)
  | Req_max_stale of int option (** max-stale[=N] *)
  | Req_min_fresh of int        (** min-fresh=N *)
  | Req_no_cache                (** no-cache *)
  | Req_no_store                (** no-store *)
  | Req_no_transform            (** no-transform *)
  | Req_only_if_cached          (** only-if-cached *)
  | Request_extension of string * string option  (** Unknown directive *)

(** Parsed response Cache-Control header *)
type response = {
  max_age : int option;
  s_maxage : int option;
  no_cache : string list option;      (** None = not present, Some [] = present without headers *)
  no_store : bool;
  no_transform : bool;
  must_revalidate : bool;
  proxy_revalidate : bool;
  must_understand : bool;
  private_ : string list option;      (** None = not present, Some [] = present without headers *)
  public : bool;
  immutable : bool;
  stale_while_revalidate : int option;
  stale_if_error : int option;
  extensions : (string * string option) list;
}

(** Parsed request Cache-Control header *)
type request = {
  req_max_age : int option;
  req_max_stale : int option option;  (** None = not present, Some None = present without value *)
  req_min_fresh : int option;
  req_no_cache : bool;
  req_no_store : bool;
  req_no_transform : bool;
  req_only_if_cached : bool;
  req_extensions : (string * string option) list;
}

(** {1 Parsing Functions} *)

let empty_response = {
  max_age = None;
  s_maxage = None;
  no_cache = None;
  no_store = false;
  no_transform = false;
  must_revalidate = false;
  proxy_revalidate = false;
  must_understand = false;
  private_ = None;
  public = false;
  immutable = false;
  stale_while_revalidate = None;
  stale_if_error = None;
  extensions = [];
}

let empty_request = {
  req_max_age = None;
  req_max_stale = None;
  req_min_fresh = None;
  req_no_cache = false;
  req_no_store = false;
  req_no_transform = false;
  req_only_if_cached = false;
  req_extensions = [];
}

(** Parse a single token (alphanumeric + some punctuation) *)
let parse_token s start =
  let len = String.length s in
  let rec find_end i =
    if i >= len then i
    else match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '!' | '#' | '$'
      | '%' | '&' | '\'' | '*' | '+' | '^' | '`' | '|' | '~' -> find_end (i + 1)
      | _ -> i
  in
  let end_pos = find_end start in
  if end_pos = start then None
  else Some (String.sub s start (end_pos - start), end_pos)

(** Parse a quoted string starting at position (after opening quote) *)
let parse_quoted_string s start =
  let buf = Buffer.create 32 in
  let len = String.length s in
  let rec loop i =
    if i >= len then None  (* Unterminated quote *)
    else match s.[i] with
      | '"' -> Some (Buffer.contents buf, i + 1)
      | '\\' when i + 1 < len ->
          Buffer.add_char buf s.[i + 1];
          loop (i + 2)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop start

(** Parse a directive value (token or quoted-string) *)
let parse_value s start =
  let len = String.length s in
  if start >= len then None
  else if s.[start] = '"' then
    parse_quoted_string s (start + 1)
  else
    parse_token s start

(** Parse comma-separated header list (for no-cache=, private=) *)
let parse_header_list s =
  (* Handle quoted list like "Accept, Accept-Encoding" *)
  let s = String.trim s in
  let s = if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'
    then String.sub s 1 (String.length s - 2)
    else s
  in
  String.split_on_char ',' s
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(** Skip whitespace and optional comma *)
let skip_ws_comma s start =
  let len = String.length s in
  let rec loop i =
    if i >= len then i
    else match s.[i] with
      | ' ' | '\t' | ',' -> loop (i + 1)
      | _ -> i
  in
  loop start

(** Parse all directives from a Cache-Control header value *)
let parse_directives s =
  let s = String.trim s in
  let len = String.length s in
  let rec loop i acc =
    if i >= len then List.rev acc
    else
      let i = skip_ws_comma s i in
      if i >= len then List.rev acc
      else match parse_token s i with
        | None -> List.rev acc  (* Invalid, stop parsing *)
        | Some (name, next_pos) ->
            let name_lower = String.lowercase_ascii name in
            (* Check for =value *)
            let next_pos = skip_ws_comma s next_pos in
            if next_pos < len && s.[next_pos] = '=' then
              let value_start = skip_ws_comma s (next_pos + 1) in
              match parse_value s value_start with
              | Some (value, end_pos) ->
                  loop (skip_ws_comma s end_pos) ((name_lower, Some value) :: acc)
              | None ->
                  loop (skip_ws_comma s (next_pos + 1)) ((name_lower, None) :: acc)
            else
              loop next_pos ((name_lower, None) :: acc)
  in
  loop 0 []

(** Parse response Cache-Control header *)
let parse_response header_value =
  let directives = parse_directives header_value in
  Log.debug (fun m -> m "Parsing response Cache-Control: %s" header_value);
  List.fold_left (fun acc (name, value) ->
    match name, value with
    | "max-age", Some v ->
        (try { acc with max_age = Some (int_of_string v) }
         with _ -> acc)
    | "s-maxage", Some v ->
        (try { acc with s_maxage = Some (int_of_string v) }
         with _ -> acc)
    | "no-cache", None ->
        { acc with no_cache = Some [] }
    | "no-cache", Some v ->
        { acc with no_cache = Some (parse_header_list v) }
    | "no-store", _ ->
        { acc with no_store = true }
    | "no-transform", _ ->
        { acc with no_transform = true }
    | "must-revalidate", _ ->
        { acc with must_revalidate = true }
    | "proxy-revalidate", _ ->
        { acc with proxy_revalidate = true }
    | "must-understand", _ ->
        { acc with must_understand = true }
    | "private", None ->
        { acc with private_ = Some [] }
    | "private", Some v ->
        { acc with private_ = Some (parse_header_list v) }
    | "public", _ ->
        { acc with public = true }
    | "immutable", _ ->
        { acc with immutable = true }
    | "stale-while-revalidate", Some v ->
        (try { acc with stale_while_revalidate = Some (int_of_string v) }
         with _ -> acc)
    | "stale-if-error", Some v ->
        (try { acc with stale_if_error = Some (int_of_string v) }
         with _ -> acc)
    | other, v ->
        { acc with extensions = (other, v) :: acc.extensions }
  ) empty_response directives

(** Parse request Cache-Control header *)
let parse_request header_value =
  let directives = parse_directives header_value in
  Log.debug (fun m -> m "Parsing request Cache-Control: %s" header_value);
  List.fold_left (fun acc (name, value) ->
    match name, value with
    | "max-age", Some v ->
        (try { acc with req_max_age = Some (int_of_string v) }
         with _ -> acc)
    | "max-stale", None ->
        { acc with req_max_stale = Some None }
    | "max-stale", Some v ->
        (try { acc with req_max_stale = Some (Some (int_of_string v)) }
         with _ -> { acc with req_max_stale = Some None })
    | "min-fresh", Some v ->
        (try { acc with req_min_fresh = Some (int_of_string v) }
         with _ -> acc)
    | "no-cache", _ ->
        { acc with req_no_cache = true }
    | "no-store", _ ->
        { acc with req_no_store = true }
    | "no-transform", _ ->
        { acc with req_no_transform = true }
    | "only-if-cached", _ ->
        { acc with req_only_if_cached = true }
    | other, v ->
        { acc with req_extensions = (other, v) :: acc.req_extensions }
  ) empty_request directives

(** {1 Freshness Calculation}

    RFC 9111 Section 4.2: Freshness *)

(** Calculate freshness lifetime from response directives and headers.
    Returns freshness lifetime in seconds, or None if not cacheable. *)
let freshness_lifetime ~response_cc ?expires ?date () =
  (* RFC 9111 Section 4.2.1: Priority:
     1. s-maxage (shared caches only, we skip this)
     2. max-age
     3. Expires - Date
     4. Heuristic (we return None, let caller decide) *)
  let ( let* ) = Option.bind in
  match response_cc.max_age with
  | Some age -> Some age
  | None ->
      match expires, date with
      | Some exp_str, Some date_str ->
          (* Use Http_date.parse to parse HTTP dates *)
          let* exp_time = Http_date.parse exp_str in
          let* date_time = Http_date.parse date_str in
          let diff = Ptime.diff exp_time date_time in
          Ptime.Span.to_int_s diff
      | _ -> None

(** {1 Age Calculation}

    RFC 9111 Section 4.2.3: Calculating Age *)

(** Age calculation inputs *)
type age_inputs = {
  date_value : Ptime.t option;
  (** Value of Date header (when response was generated) *)

  age_value : int;
  (** Value of Age header in seconds (0 if not present) *)

  request_time : Ptime.t;
  (** Time when the request was initiated *)

  response_time : Ptime.t;
  (** Time when the response was received *)
}

(** Calculate the current age of a cached response.
    Per RFC 9111 Section 4.2.3:

    {v
    apparent_age = max(0, response_time - date_value)
    response_delay = response_time - request_time
    corrected_age_value = age_value + response_delay
    corrected_initial_age = max(apparent_age, corrected_age_value)
    resident_time = now - response_time
    current_age = corrected_initial_age + resident_time
    v}

    @param inputs Age calculation inputs
    @param now Current time
    @return Current age in seconds *)
let calculate_age ~inputs ~now =
  (* apparent_age = max(0, response_time - date_value) *)
  let apparent_age =
    match inputs.date_value with
    | Some date ->
      let diff = Ptime.diff inputs.response_time date in
      max 0 (Option.value ~default:0 (Ptime.Span.to_int_s diff))
    | None -> 0
  in
  (* response_delay = response_time - request_time *)
  let response_delay =
    let diff = Ptime.diff inputs.response_time inputs.request_time in
    max 0 (Option.value ~default:0 (Ptime.Span.to_int_s diff))
  in
  (* corrected_age_value = age_value + response_delay *)
  let corrected_age_value = inputs.age_value + response_delay in
  (* corrected_initial_age = max(apparent_age, corrected_age_value) *)
  let corrected_initial_age = max apparent_age corrected_age_value in
  (* resident_time = now - response_time *)
  let resident_time =
    let diff = Ptime.diff now inputs.response_time in
    max 0 (Option.value ~default:0 (Ptime.Span.to_int_s diff))
  in
  (* current_age = corrected_initial_age + resident_time *)
  corrected_initial_age + resident_time

(** {1 Heuristic Freshness}

    RFC 9111 Section 4.2.2: Calculating Heuristic Freshness *)

(** Default heuristic fraction: 10% of time since Last-Modified.
    RFC 9111 recommends this as a typical value. *)
let default_heuristic_fraction = 0.10

(** Maximum heuristic freshness lifetime: 1 day (86400 seconds).
    This prevents excessively long heuristic caching. *)
let default_max_heuristic_age = 86400

(** Calculate heuristic freshness lifetime when no explicit caching info provided.
    Per RFC 9111 Section 4.2.2, caches MAY use heuristics when explicit freshness
    is not available.

    @param last_modified Value of Last-Modified header
    @param response_time When the response was received
    @param fraction Fraction of (now - last_modified) to use (default 10%)
    @param max_age Maximum heuristic age in seconds (default 1 day)
    @return Heuristic freshness lifetime in seconds, or None *)
let heuristic_freshness
    ?last_modified
    ~response_time
    ?(fraction = default_heuristic_fraction)
    ?(max_age = default_max_heuristic_age)
    () =
  match last_modified with
  | Some lm_str ->
    (match Http_date.parse lm_str with
     | Some lm_time ->
       let age_since_modified =
         let diff = Ptime.diff response_time lm_time in
         max 0 (Option.value ~default:0 (Ptime.Span.to_int_s diff))
       in
       let heuristic = int_of_float (float_of_int age_since_modified *. fraction) in
       Some (min heuristic max_age)
     | None ->
       Log.debug (fun m -> m "Failed to parse Last-Modified: %s" lm_str);
       None)
  | None -> None

(** Check if a cached response is fresh.

    @param current_age Current age from calculate_age
    @param freshness_lifetime From freshness_lifetime or heuristic_freshness
    @return true if the response is still fresh *)
let is_fresh ~current_age ~freshness_lifetime =
  current_age < freshness_lifetime

(** Check if a stale response can still be served based on request directives.

    @param request_cc Parsed request Cache-Control
    @param current_age Current age of the cached response
    @param freshness_lifetime Freshness lifetime of the cached response
    @return true if the stale response can be served *)
let can_serve_stale ~request_cc ~current_age ~freshness_lifetime =
  let staleness = current_age - freshness_lifetime in
  if staleness <= 0 then true  (* Not stale *)
  else
    match request_cc.req_max_stale with
    | Some None -> true  (* max-stale without value: accept any staleness *)
    | Some (Some allowed_stale) -> staleness <= allowed_stale
    | None -> false  (* No max-stale: don't serve stale *)

(** Check if a response is cacheable based on Cache-Control directives *)
let is_cacheable ~response_cc ~status =
  (* RFC 9111 Section 3: A response is cacheable if:
     - no-store is not present
     - status is cacheable by default OR explicit caching directive present *)
  if response_cc.no_store then false
  else
    (* Default cacheable statuses per RFC 9110 Section 15.1 *)
    let default_cacheable = List.mem status [200; 203; 204; 206; 300; 301; 308; 404; 405; 410; 414; 501] in
    default_cacheable || Option.is_some response_cc.max_age || Option.is_some response_cc.s_maxage

(** Check if response requires revalidation before use *)
let must_revalidate ~response_cc =
  response_cc.must_revalidate || response_cc.proxy_revalidate ||
  Option.is_some response_cc.no_cache

(** Check if response can be stored in shared caches *)
let is_public ~response_cc =
  response_cc.public && not (Option.is_some response_cc.private_)

(** Check if response can only be stored in private caches *)
let is_private ~response_cc =
  Option.is_some response_cc.private_

(** {1 Pretty Printers} *)

let pp_response ppf r =
  let items = [] in
  let items = match r.max_age with Some a -> Printf.sprintf "max-age=%d" a :: items | None -> items in
  let items = match r.s_maxage with Some a -> Printf.sprintf "s-maxage=%d" a :: items | None -> items in
  let items = match r.no_cache with
    | Some [] -> "no-cache" :: items
    | Some hs -> Printf.sprintf "no-cache=\"%s\"" (String.concat ", " hs) :: items
    | None -> items
  in
  let items = if r.no_store then "no-store" :: items else items in
  let items = if r.no_transform then "no-transform" :: items else items in
  let items = if r.must_revalidate then "must-revalidate" :: items else items in
  let items = if r.proxy_revalidate then "proxy-revalidate" :: items else items in
  let items = if r.must_understand then "must-understand" :: items else items in
  let items = match r.private_ with
    | Some [] -> "private" :: items
    | Some hs -> Printf.sprintf "private=\"%s\"" (String.concat ", " hs) :: items
    | None -> items
  in
  let items = if r.public then "public" :: items else items in
  let items = if r.immutable then "immutable" :: items else items in
  let items = match r.stale_while_revalidate with
    | Some s -> Printf.sprintf "stale-while-revalidate=%d" s :: items | None -> items
  in
  let items = match r.stale_if_error with
    | Some s -> Printf.sprintf "stale-if-error=%d" s :: items | None -> items
  in
  Format.fprintf ppf "%s" (String.concat ", " (List.rev items))

let pp_request ppf r =
  let items = [] in
  let items = match r.req_max_age with Some a -> Printf.sprintf "max-age=%d" a :: items | None -> items in
  let items = match r.req_max_stale with
    | Some None -> "max-stale" :: items
    | Some (Some s) -> Printf.sprintf "max-stale=%d" s :: items
    | None -> items
  in
  let items = match r.req_min_fresh with Some s -> Printf.sprintf "min-fresh=%d" s :: items | None -> items in
  let items = if r.req_no_cache then "no-cache" :: items else items in
  let items = if r.req_no_store then "no-store" :: items else items in
  let items = if r.req_no_transform then "no-transform" :: items else items in
  let items = if r.req_only_if_cached then "only-if-cached" :: items else items in
  Format.fprintf ppf "%s" (String.concat ", " (List.rev items))
