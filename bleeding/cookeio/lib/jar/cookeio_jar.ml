(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "cookie_jar" ~doc:"Cookie jar management"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  mutable original_cookies : Cookeio.t list; (* from client *)
  mutable delta_cookies : Cookeio.t list; (* to send back *)
  mutex : Eio.Mutex.t;
}
(** Cookie jar for storing and managing cookies *)

(** {1 Cookie Jar Creation} *)

let create () =
  Log.debug (fun m -> m "Creating new empty cookie jar");
  { original_cookies = []; delta_cookies = []; mutex = Eio.Mutex.create () }

(** {1 Cookie Matching Helpers} *)

(** Two cookies are considered identical if they have the same name, domain,
    and path. This is used when replacing or removing cookies.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)
let cookie_identity_matches c1 c2 =
  Cookeio.name c1 = Cookeio.name c2
  && Cookeio.domain c1 = Cookeio.domain c2
  && Cookeio.path c1 = Cookeio.path c2

(** Normalize a domain by stripping the leading dot.

    Per RFC 6265, the Domain attribute value is canonicalized by removing any
    leading dot before storage.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3> RFC 6265 Section 5.2.3 - The Domain Attribute *)
let normalize_domain domain =
  match String.starts_with ~prefix:"." domain with
  | true when String.length domain > 1 ->
      String.sub domain 1 (String.length domain - 1)
  | _ -> domain

(** Remove duplicate cookies, keeping the last occurrence.

    Used to deduplicate combined cookie lists where delta cookies should
    take precedence over original cookies. *)
let dedup_by_identity cookies =
  let rec aux acc = function
    | [] -> List.rev acc
    | c :: rest ->
        let has_duplicate =
          List.exists (fun c2 -> cookie_identity_matches c c2) rest
        in
        if has_duplicate then aux acc rest else aux (c :: acc) rest
  in
  aux [] cookies

(** Check if a string is an IP address (IPv4 or IPv6).

    Per RFC 6265 Section 5.1.3, domain matching should only apply to hostnames,
    not IP addresses. IP addresses require exact match only.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.3> RFC 6265 Section 5.1.3 - Domain Matching *)
let is_ip_address domain = Result.is_ok (Ipaddr.of_string domain)

(** Check if a cookie domain matches a request domain.

    Per RFC 6265 Section 5.1.3, a string domain-matches a given domain string if:
    - The domain string and the string are identical, OR
    - All of the following are true:
      - The domain string is a suffix of the string
      - The last character of the string not in the domain string is "."
      - The string is a host name (i.e., not an IP address)

    Additionally, per Section 5.3 Step 6, if the cookie has the host-only-flag
    set, only exact matches are allowed.

    @param host_only If true, only exact domain match is allowed
    @param cookie_domain The domain stored in the cookie (without leading dot)
    @param request_domain The domain from the HTTP request
    @return true if the cookie should be sent for this domain

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.3> RFC 6265 Section 5.1.3 - Domain Matching
    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model (host-only-flag) *)
let domain_matches ~host_only cookie_domain request_domain =
  request_domain = cookie_domain
  || (not (is_ip_address request_domain || host_only)
      && String.ends_with ~suffix:("." ^ cookie_domain) request_domain)

(** Check if a cookie path matches a request path.

    Per RFC 6265 Section 5.1.4, a request-path path-matches a given cookie-path if:
    - The cookie-path and the request-path are identical, OR
    - The cookie-path is a prefix of the request-path, AND either:
      - The last character of the cookie-path is "/", OR
      - The first character of the request-path that is not included in the
        cookie-path is "/"

    @param cookie_path The path stored in the cookie
    @param request_path The path from the HTTP request
    @return true if the cookie should be sent for this path

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.4> RFC 6265 Section 5.1.4 - Paths and Path-Match *)
let path_matches cookie_path request_path =
  if cookie_path = request_path then true
  else if String.starts_with ~prefix:cookie_path request_path then
    let cookie_len = String.length cookie_path in
    String.ends_with ~suffix:"/" cookie_path
    || (String.length request_path > cookie_len && request_path.[cookie_len] = '/')
  else false

(** {1 Cookie Expiration} *)

(** Check if a cookie has expired based on its expiry-time.

    Per RFC 6265 Section 5.3, a cookie is expired if the current date and time
    is past the expiry-time. Session cookies (with no Expires or Max-Age) never
    expire via this check - they expire when the "session" ends.

    @param cookie The cookie to check
    @param clock The Eio clock for current time
    @return true if the cookie has expired

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)
let is_expired cookie clock =
  match Cookeio.expires cookie with
  | None -> false (* No expiration *)
  | Some `Session ->
      false (* Session cookie - not expired until browser closes *)
  | Some (`DateTime exp_time) ->
      let now =
        Ptime.of_float_s (Eio.Time.now clock)
        |> Option.value ~default:Ptime.epoch
      in
      Ptime.compare now exp_time > 0

let pp ppf jar =
  Eio.Mutex.lock jar.mutex;
  let original = jar.original_cookies in
  let delta = jar.delta_cookies in
  Eio.Mutex.unlock jar.mutex;

  let all_cookies = original @ delta in
  Format.fprintf ppf "@[<v>CookieJar with %d cookies (%d original, %d delta):@,"
    (List.length all_cookies) (List.length original) (List.length delta);
  List.iter
    (fun cookie -> Format.fprintf ppf "  %a@," Cookeio.pp cookie)
    all_cookies;
  Format.fprintf ppf "@]"

(** {1 Cookie Management} *)

(** Preserve creation time from an existing cookie when replacing.

    Per RFC 6265 Section 5.3, Step 11.3: "If the newly created cookie was
    received from a 'non-HTTP' API and the old-cookie's http-only-flag is
    true, abort these steps and ignore the newly created cookie entirely."
    Step 11.3 also states: "Update the creation-time of the old-cookie to
    match the creation-time of the newly created cookie."

    However, the common interpretation (and browser behavior) is to preserve
    the original creation-time when updating a cookie. This matches what
    Step 3 of Section 5.4 uses for ordering (creation-time stability).

    @param old_cookie The existing cookie being replaced (if any)
    @param new_cookie The new cookie to add
    @return The new cookie with creation_time preserved from old_cookie if present

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)
let preserve_creation_time old_cookie_opt new_cookie =
  match old_cookie_opt with
  | None -> new_cookie
  | Some old_cookie ->
      Cookeio.make ~domain:(Cookeio.domain new_cookie)
        ~path:(Cookeio.path new_cookie) ~name:(Cookeio.name new_cookie)
        ~value:(Cookeio.value new_cookie) ~secure:(Cookeio.secure new_cookie)
        ~http_only:(Cookeio.http_only new_cookie)
        ?expires:(Cookeio.expires new_cookie)
        ?max_age:(Cookeio.max_age new_cookie)
        ?same_site:(Cookeio.same_site new_cookie)
        ~partitioned:(Cookeio.partitioned new_cookie)
        ~host_only:(Cookeio.host_only new_cookie)
        ~creation_time:(Cookeio.creation_time old_cookie)
        ~last_access:(Cookeio.last_access new_cookie)
        ()

let add_cookie jar cookie =
  Log.debug (fun m ->
      m "Adding cookie to delta: %s=%s for domain %s" (Cookeio.name cookie)
        (Cookeio.value cookie) (Cookeio.domain cookie));

  Eio.Mutex.lock jar.mutex;

  (* Find existing cookie with same identity to preserve creation_time
     per RFC 6265 Section 5.3, Step 11.3 *)
  let existing =
    List.find_opt (fun c -> cookie_identity_matches c cookie) jar.delta_cookies
  in
  let existing =
    match existing with
    | Some _ -> existing
    | None ->
        (* Also check original cookies for creation time preservation *)
        List.find_opt
          (fun c -> cookie_identity_matches c cookie)
          jar.original_cookies
  in

  let cookie = preserve_creation_time existing cookie in

  (* Remove existing cookie with same identity from delta *)
  jar.delta_cookies <-
    List.filter
      (fun c -> not (cookie_identity_matches c cookie))
      jar.delta_cookies;
  jar.delta_cookies <- cookie :: jar.delta_cookies;
  Eio.Mutex.unlock jar.mutex

let add_original jar cookie =
  Log.debug (fun m ->
      m "Adding original cookie: %s=%s for domain %s" (Cookeio.name cookie)
        (Cookeio.value cookie) (Cookeio.domain cookie));

  Eio.Mutex.lock jar.mutex;

  (* Find existing cookie with same identity to preserve creation_time
     per RFC 6265 Section 5.3, Step 11.3 *)
  let existing =
    List.find_opt
      (fun c -> cookie_identity_matches c cookie)
      jar.original_cookies
  in

  let cookie = preserve_creation_time existing cookie in

  (* Remove existing cookie with same identity from original *)
  jar.original_cookies <-
    List.filter
      (fun c -> not (cookie_identity_matches c cookie))
      jar.original_cookies;
  jar.original_cookies <- cookie :: jar.original_cookies;
  Eio.Mutex.unlock jar.mutex

let delta jar =
  Eio.Mutex.lock jar.mutex;
  let result = jar.delta_cookies in
  Eio.Mutex.unlock jar.mutex;
  Log.debug (fun m -> m "Returning %d delta cookies" (List.length result));
  result

(** Create a removal cookie for deleting a cookie from the client.

    Per RFC 6265 Section 5.3, to remove a cookie, the server sends a Set-Cookie
    header with an expiry date in the past. We also set Max-Age=0 and an empty
    value for maximum compatibility.

    @param cookie The cookie to create a removal for
    @param clock The Eio clock for timestamps
    @return A new cookie configured to cause deletion

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)
let make_removal_cookie cookie ~clock =
  let now =
    Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
  in
  (* Create a cookie with Max-Age=0 and past expiration (1 year ago) *)
  let past_expiry =
    Ptime.sub_span now (Ptime.Span.of_int_s (365 * 24 * 60 * 60))
    |> Option.value ~default:Ptime.epoch
  in
  Cookeio.make ~domain:(Cookeio.domain cookie) ~path:(Cookeio.path cookie)
    ~name:(Cookeio.name cookie) ~value:"" ~secure:(Cookeio.secure cookie)
    ~http_only:(Cookeio.http_only cookie) ~expires:(`DateTime past_expiry)
    ~max_age:(Ptime.Span.of_int_s 0) ?same_site:(Cookeio.same_site cookie)
    ~partitioned:(Cookeio.partitioned cookie)
    ~host_only:(Cookeio.host_only cookie)
    ~creation_time:now ~last_access:now ()

let remove jar ~clock cookie =
  Log.debug (fun m ->
      m "Removing cookie: %s=%s for domain %s" (Cookeio.name cookie)
        (Cookeio.value cookie) (Cookeio.domain cookie));

  Eio.Mutex.lock jar.mutex;
  (* Check if this cookie exists in original_cookies *)
  let in_original =
    List.exists (fun c -> cookie_identity_matches c cookie) jar.original_cookies
  in

  if in_original then (
    (* Create a removal cookie and add it to delta *)
    let removal = make_removal_cookie cookie ~clock in
    jar.delta_cookies <-
      List.filter
        (fun c -> not (cookie_identity_matches c removal))
        jar.delta_cookies;
    jar.delta_cookies <- removal :: jar.delta_cookies;
    Log.debug (fun m -> m "Created removal cookie in delta for original cookie"))
  else (
    (* Just remove from delta if it exists there *)
    jar.delta_cookies <-
      List.filter
        (fun c -> not (cookie_identity_matches c cookie))
        jar.delta_cookies;
    Log.debug (fun m -> m "Removed cookie from delta"));

  Eio.Mutex.unlock jar.mutex

(** Compare cookies for ordering per RFC 6265 Section 5.4, Step 2.

    Cookies SHOULD be sorted:
    1. Cookies with longer paths listed first
    2. Among equal-length paths, cookies with earlier creation-times first

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.4> RFC 6265 Section 5.4 - The Cookie Header *)
let compare_cookie_order c1 c2 =
  let path1_len = String.length (Cookeio.path c1) in
  let path2_len = String.length (Cookeio.path c2) in
  (* Longer paths first (descending order) *)
  match Int.compare path2_len path1_len with
  | 0 ->
      (* Equal path lengths: earlier creation time first (ascending order) *)
      Ptime.compare (Cookeio.creation_time c1) (Cookeio.creation_time c2)
  | n -> n

(** Retrieve cookies that should be sent for a given request.

    Per RFC 6265 Section 5.4, the user agent should include a Cookie header
    containing cookies that match the request-uri's domain, path, and security
    context. This function also updates the last-access-time for matched cookies.

    Cookies are sorted per Section 5.4, Step 2:
    1. Cookies with longer paths listed first
    2. Among equal-length paths, earlier creation-times listed first

    @param jar The cookie jar to search
    @param clock The Eio clock for timestamp updates
    @param domain The request domain (hostname or IP address)
    @param path The request path
    @param is_secure Whether the request is over a secure channel (HTTPS)
    @return List of cookies that should be included in the Cookie header, sorted

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.4> RFC 6265 Section 5.4 - The Cookie Header *)
let get_cookies jar ~clock ~domain:request_domain ~path:request_path ~is_secure
    =
  Log.debug (fun m ->
      m "Getting cookies for domain=%s path=%s secure=%b" request_domain
        request_path is_secure);

  Eio.Mutex.lock jar.mutex;

  (* Combine original and delta cookies, with delta taking precedence *)
  let all_cookies = jar.original_cookies @ jar.delta_cookies in
  let unique_cookies = dedup_by_identity all_cookies in

  (* Filter for applicable cookies, excluding removal cookies and expired cookies *)
  let applicable =
    List.filter
      (fun cookie ->
        Cookeio.value cookie <> ""
        (* Exclude removal cookies *)
        && (not (is_expired cookie clock))
        (* Exclude expired cookies *)
        && domain_matches ~host_only:(Cookeio.host_only cookie)
             (Cookeio.domain cookie) request_domain
        && path_matches (Cookeio.path cookie) request_path
        && ((not (Cookeio.secure cookie)) || is_secure))
      unique_cookies
  in

  (* Sort cookies per RFC 6265 Section 5.4, Step 2:
     - Longer paths first
     - Equal paths: earlier creation time first *)
  let sorted = List.sort compare_cookie_order applicable in

  (* Update last access time in both lists *)
  let now =
    Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
  in
  let update_last_access cookies =
    List.map
      (fun c ->
        if List.exists (fun a -> cookie_identity_matches a c) applicable then
          Cookeio.make ~domain:(Cookeio.domain c) ~path:(Cookeio.path c)
            ~name:(Cookeio.name c) ~value:(Cookeio.value c)
            ~secure:(Cookeio.secure c) ~http_only:(Cookeio.http_only c)
            ?expires:(Cookeio.expires c) ?max_age:(Cookeio.max_age c)
            ?same_site:(Cookeio.same_site c)
            ~partitioned:(Cookeio.partitioned c)
            ~host_only:(Cookeio.host_only c)
            ~creation_time:(Cookeio.creation_time c) ~last_access:now ()
        else c)
      cookies
  in
  jar.original_cookies <- update_last_access jar.original_cookies;
  jar.delta_cookies <- update_last_access jar.delta_cookies;

  Eio.Mutex.unlock jar.mutex;

  Log.debug (fun m -> m "Found %d applicable cookies" (List.length sorted));
  sorted

let clear jar =
  Log.info (fun m -> m "Clearing all cookies");
  Eio.Mutex.lock jar.mutex;
  jar.original_cookies <- [];
  jar.delta_cookies <- [];
  Eio.Mutex.unlock jar.mutex

let clear_expired jar ~clock =
  Eio.Mutex.lock jar.mutex;
  let before_count =
    List.length jar.original_cookies + List.length jar.delta_cookies
  in
  jar.original_cookies <-
    List.filter (fun c -> not (is_expired c clock)) jar.original_cookies;
  jar.delta_cookies <-
    List.filter (fun c -> not (is_expired c clock)) jar.delta_cookies;
  let removed =
    before_count
    - (List.length jar.original_cookies + List.length jar.delta_cookies)
  in
  Eio.Mutex.unlock jar.mutex;
  Log.info (fun m -> m "Cleared %d expired cookies" removed)

let clear_session_cookies jar =
  Eio.Mutex.lock jar.mutex;
  let before_count =
    List.length jar.original_cookies + List.length jar.delta_cookies
  in
  (* Keep only cookies that are NOT session cookies *)
  let is_not_session c =
    match Cookeio.expires c with
    | Some `Session -> false (* This is a session cookie, remove it *)
    | None | Some (`DateTime _) -> true (* Keep these *)
  in
  jar.original_cookies <- List.filter is_not_session jar.original_cookies;
  jar.delta_cookies <- List.filter is_not_session jar.delta_cookies;
  let removed =
    before_count
    - (List.length jar.original_cookies + List.length jar.delta_cookies)
  in
  Eio.Mutex.unlock jar.mutex;
  Log.info (fun m -> m "Cleared %d session cookies" removed)

let count jar =
  Eio.Mutex.lock jar.mutex;
  let all_cookies = jar.original_cookies @ jar.delta_cookies in
  let unique = dedup_by_identity all_cookies in
  let n = List.length unique in
  Eio.Mutex.unlock jar.mutex;
  n

let get_all_cookies jar =
  Eio.Mutex.lock jar.mutex;
  let all_cookies = jar.original_cookies @ jar.delta_cookies in
  let unique = dedup_by_identity all_cookies in
  Eio.Mutex.unlock jar.mutex;
  unique

let is_empty jar =
  Eio.Mutex.lock jar.mutex;
  let empty = jar.original_cookies = [] && jar.delta_cookies = [] in
  Eio.Mutex.unlock jar.mutex;
  empty

(** {1 Mozilla Format} *)

let to_mozilla_format_internal jar =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "# Netscape HTTP Cookie File\n";
  Buffer.add_string buffer "# This is a generated file!  Do not edit.\n\n";

  (* Combine and deduplicate cookies *)
  let all_cookies = jar.original_cookies @ jar.delta_cookies in
  let unique = dedup_by_identity all_cookies in

  List.iter
    (fun cookie ->
      (* Mozilla format: include_subdomains=TRUE means host_only=false *)
      let include_subdomains = if Cookeio.host_only cookie then "FALSE" else "TRUE" in
      let secure_flag = if Cookeio.secure cookie then "TRUE" else "FALSE" in
      let expires_str =
        match Cookeio.expires cookie with
        | None -> "0" (* No expiration *)
        | Some `Session -> "0" (* Session cookie *)
        | Some (`DateTime t) ->
            let epoch = Ptime.to_float_s t |> int_of_float |> string_of_int in
            epoch
      in

      Buffer.add_string buffer
        (Printf.sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" (Cookeio.domain cookie)
           include_subdomains (Cookeio.path cookie) secure_flag expires_str
           (Cookeio.name cookie) (Cookeio.value cookie)))
    unique;

  Buffer.contents buffer

let to_mozilla_format jar =
  Eio.Mutex.lock jar.mutex;
  let result = to_mozilla_format_internal jar in
  Eio.Mutex.unlock jar.mutex;
  result

let from_mozilla_format ~clock content =
  Log.debug (fun m -> m "Parsing Mozilla format cookies");
  let jar = create () in

  let lines = String.split_on_char '\n' content in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line <> "" && not (String.starts_with ~prefix:"#" line) then
        match String.split_on_char '\t' line with
        | [ domain; include_subdomains; path; secure; expires; name; value ] ->
            let now =
              Ptime.of_float_s (Eio.Time.now clock)
              |> Option.value ~default:Ptime.epoch
            in
            let expires =
              match int_of_string_opt expires with
              | Some exp_int when exp_int <> 0 ->
                  Option.map (fun t -> `DateTime t)
                    (Ptime.of_float_s (float_of_int exp_int))
              | _ -> None
            in
            (* Mozilla format: include_subdomains=TRUE means host_only=false *)
            let host_only = include_subdomains <> "TRUE" in

            let cookie =
              Cookeio.make ~domain:(normalize_domain domain) ~path ~name ~value
                ~secure:(secure = "TRUE") ~http_only:false ?expires
                ?max_age:None ?same_site:None ~partitioned:false ~host_only
                ~creation_time:now ~last_access:now ()
            in
            add_original jar cookie;
            Log.debug (fun m -> m "Loaded cookie: %s=%s" name value)
        | _ -> Log.warn (fun m -> m "Invalid cookie line: %s" line))
    lines;

  Log.info (fun m -> m "Loaded %d cookies" (List.length jar.original_cookies));
  jar

(** {1 File Operations} *)

let load ~clock path =
  Log.info (fun m -> m "Loading cookies from %a" Eio.Path.pp path);

  try
    let content = Eio.Path.load path in
    from_mozilla_format ~clock content
  with
  | Eio.Io _ ->
      Log.info (fun m -> m "Cookie file not found, creating empty jar");
      create ()
  | exn ->
      Log.err (fun m -> m "Failed to load cookies: %s" (Printexc.to_string exn));
      create ()

let save path jar =
  Eio.Mutex.lock jar.mutex;
  let total_cookies =
    List.length jar.original_cookies + List.length jar.delta_cookies
  in
  Eio.Mutex.unlock jar.mutex;
  Log.info (fun m -> m "Saving %d cookies to %a" total_cookies Eio.Path.pp path);

  let content = to_mozilla_format jar in

  try
    Eio.Path.save ~create:(`Or_truncate 0o600) path content;
    Log.debug (fun m -> m "Cookies saved successfully")
  with exn ->
    Log.err (fun m -> m "Failed to save cookies: %s" (Printexc.to_string exn))
