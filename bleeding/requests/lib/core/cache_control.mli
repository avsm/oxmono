(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Cache-Control header parsing per RFC 9111 (HTTP Caching)

    This module provides parsing and representation of Cache-Control directives
    for both requests and responses. It supports all standard directives from
    RFC 9111 Section 5.2.

    Per Recommendation #17: Response Caching with RFC 7234/9111 Compliance

    {2 Examples}

    {[
      (* Parse response Cache-Control *)
      let cc = Cache_control.parse_response "max-age=3600, public" in
      Printf.printf "Max age: %d\n" (Option.get cc.max_age);

      (* Check if cacheable *)
      if Cache_control.is_cacheable ~response_cc:cc ~status:200 then
        Printf.printf "Response is cacheable\n"
    ]}
*)

(** Log source for cache control operations *)
val src : Logs.Src.t

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

(** {1 Parsed Cache-Control Types} *)

(** Parsed response Cache-Control header *)
type response = {
  max_age : int option;
  (** max-age directive value in seconds *)

  s_maxage : int option;
  (** s-maxage directive value for shared caches *)

  no_cache : string list option;
  (** [None] = not present, [Some []] = present without headers,
      [Some headers] = must revalidate for these headers *)

  no_store : bool;
  (** If true, the response must not be stored *)

  no_transform : bool;
  (** If true, intermediaries must not transform the response *)

  must_revalidate : bool;
  (** If true, stale responses must be revalidated *)

  proxy_revalidate : bool;
  (** Like must_revalidate but only for shared caches *)

  must_understand : bool;
  (** If true, cache must understand the caching rules *)

  private_ : string list option;
  (** [None] = not present, [Some []] = entirely private,
      [Some headers] = these headers are private *)

  public : bool;
  (** If true, response may be stored by any cache *)

  immutable : bool;
  (** If true, response will not change during freshness lifetime *)

  stale_while_revalidate : int option;
  (** Seconds stale responses may be served while revalidating *)

  stale_if_error : int option;
  (** Seconds stale responses may be served on error *)

  extensions : (string * string option) list;
  (** Unknown directives for forward compatibility *)
}

(** Parsed request Cache-Control header *)
type request = {
  req_max_age : int option;
  (** max-age directive - maximum age client will accept *)

  req_max_stale : int option option;
  (** [None] = not present, [Some None] = accept any stale,
      [Some (Some n)] = accept stale up to n seconds *)

  req_min_fresh : int option;
  (** min-fresh directive - response must be fresh for at least n more seconds *)

  req_no_cache : bool;
  (** If true, force revalidation with origin server *)

  req_no_store : bool;
  (** If true, response must not be stored *)

  req_no_transform : bool;
  (** If true, intermediaries must not transform *)

  req_only_if_cached : bool;
  (** If true, return cached response or 504 Gateway Timeout *)

  req_extensions : (string * string option) list;
  (** Unknown directives for forward compatibility *)
}

(** {1 Empty Values} *)

val empty_response : response
(** An empty response Cache-Control (no directives set) *)

val empty_request : request
(** An empty request Cache-Control (no directives set) *)

(** {1 Parsing Functions} *)

val parse_response : string -> response
(** [parse_response header_value] parses a response Cache-Control header value.
    Unknown directives are preserved in [extensions] for forward compatibility. *)

val parse_request : string -> request
(** [parse_request header_value] parses a request Cache-Control header value.
    Unknown directives are preserved in [req_extensions] for forward compatibility. *)

(** {1 Freshness Calculation}

    RFC 9111 Section 4.2: Freshness *)

val freshness_lifetime :
  response_cc:response ->
  ?expires:string ->
  ?date:string ->
  unit ->
  int option
(** [freshness_lifetime ~response_cc ?expires ?date ()] calculates the freshness
    lifetime of a response in seconds, based on Cache-Control directives and
    optional Expires/Date headers.

    Priority (per RFC 9111 Section 4.2.1):
    1. max-age directive
    2. Expires header minus Date header
    3. Returns [None] if no explicit freshness (caller should use heuristics)

    @param response_cc Parsed Cache-Control from response
    @param expires Optional Expires header value (HTTP-date format)
    @param date Optional Date header value (HTTP-date format) *)

(** {1 Age Calculation}

    Per RFC 9111 Section 4.2.3: Calculating Age. *)

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
(** Inputs required for age calculation per RFC 9111 Section 4.2.3. *)

val calculate_age : inputs:age_inputs -> now:Ptime.t -> int
(** [calculate_age ~inputs ~now] calculates the current age of a cached response.

    Per RFC 9111 Section 4.2.3:
    {v
    apparent_age = max(0, response_time - date_value)
    response_delay = response_time - request_time
    corrected_age_value = age_value + response_delay
    corrected_initial_age = max(apparent_age, corrected_age_value)
    resident_time = now - response_time
    current_age = corrected_initial_age + resident_time
    v}

    @return Current age in seconds *)

(** {1 Heuristic Freshness}

    Per RFC 9111 Section 4.2.2: Calculating Heuristic Freshness. *)

val default_heuristic_fraction : float
(** Default heuristic fraction: 10% of time since Last-Modified.
    RFC 9111 recommends this as a typical value. *)

val default_max_heuristic_age : int
(** Maximum heuristic freshness lifetime: 1 day (86400 seconds). *)

val heuristic_freshness :
  ?last_modified:string ->
  response_time:Ptime.t ->
  ?fraction:float ->
  ?max_age:int ->
  unit ->
  int option
(** [heuristic_freshness ?last_modified ~response_time ?fraction ?max_age ()]
    calculates heuristic freshness lifetime when no explicit caching info provided.

    Per RFC 9111 Section 4.2.2, caches MAY use heuristics when explicit freshness
    is not available. The typical heuristic is 10% of time since Last-Modified.

    @param last_modified Value of Last-Modified header
    @param response_time When the response was received
    @param fraction Fraction of (now - last_modified) to use (default 10%)
    @param max_age Maximum heuristic age in seconds (default 1 day)
    @return Heuristic freshness lifetime in seconds, or None *)

val is_fresh : current_age:int -> freshness_lifetime:int -> bool
(** [is_fresh ~current_age ~freshness_lifetime] returns true if a cached
    response is still fresh (current_age < freshness_lifetime). *)

val can_serve_stale :
  request_cc:request ->
  current_age:int ->
  freshness_lifetime:int ->
  bool
(** [can_serve_stale ~request_cc ~current_age ~freshness_lifetime] returns true
    if a stale response can still be served based on request Cache-Control
    directives (specifically max-stale). *)

(** {1 Cacheability Checks} *)

val is_cacheable : response_cc:response -> status:int -> bool
(** [is_cacheable ~response_cc ~status] returns true if the response may be
    cached based on its Cache-Control directives and HTTP status code.

    A response is cacheable if:
    - no-store is NOT present
    - Status is cacheable by default (200, 203, 204, 206, 300, 301, 308, 404,
      405, 410, 414, 501) OR explicit caching directive is present *)

val must_revalidate : response_cc:response -> bool
(** [must_revalidate ~response_cc] returns true if cached response must be
    revalidated with the origin server before use.

    True if any of: must-revalidate, proxy-revalidate, or no-cache is set. *)

val is_public : response_cc:response -> bool
(** [is_public ~response_cc] returns true if the response may be stored
    in shared caches (CDNs, proxies). *)

val is_private : response_cc:response -> bool
(** [is_private ~response_cc] returns true if the response may only be
    stored in private caches (browser cache). *)

(** {1 Pretty Printers} *)

val pp_response : Format.formatter -> response -> unit
(** Pretty print a parsed response Cache-Control *)

val pp_request : Format.formatter -> request -> unit
(** Pretty print a parsed request Cache-Control *)
