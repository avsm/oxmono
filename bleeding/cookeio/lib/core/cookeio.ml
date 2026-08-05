(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "cookeio" ~doc:"Cookie management"

module Log = (val Logs.src_log src : Logs.LOG)

(** SameSite attribute for cross-site request control.

    The SameSite attribute is defined in the RFC 6265bis draft and controls
    whether cookies are sent with cross-site requests.

    @see <https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.4.7> RFC 6265bis Section 5.4.7 - The SameSite Attribute *)
module SameSite = struct
  type t = [ `Strict | `Lax | `None ]

  let equal = ( = )

  let pp ppf = function
    | `Strict -> Format.pp_print_string ppf "Strict"
    | `Lax -> Format.pp_print_string ppf "Lax"
    | `None -> Format.pp_print_string ppf "None"
end

(** Cookie expiration type.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
    cookies have either a persistent expiry time or are session cookies that
    expire when the user agent session ends.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)
module Expiration = struct
  type t = [ `Session | `DateTime of Ptime.t ]

  let equal e1 e2 =
    match (e1, e2) with
    | `Session, `Session -> true
    | `DateTime t1, `DateTime t2 -> Ptime.equal t1 t2
    | _ -> false

  let pp ppf = function
    | `Session -> Format.pp_print_string ppf "Session"
    | `DateTime t -> Format.fprintf ppf "DateTime(%a)" Ptime.pp t
end

type t = {
  domain : string;
  path : string;
  name : string;
  value : string;
  secure : bool;
  http_only : bool;
  partitioned : bool;
  host_only : bool;
  expires : Expiration.t option;
  max_age : Ptime.Span.t option;
  same_site : SameSite.t option;
  creation_time : Ptime.t;
  last_access : Ptime.t;
}
(** HTTP Cookie *)

(** {1 Cookie Accessors} *)

let domain cookie = cookie.domain
let path cookie = cookie.path
let name cookie = cookie.name
let value cookie = cookie.value

let value_trimmed cookie =
  let v = cookie.value in
  let len = String.length v in
  if len < 2 then v
  else
    match (v.[0], v.[len - 1]) with
    | '"', '"' -> String.sub v 1 (len - 2)
    | _ -> v

let secure cookie = cookie.secure
let http_only cookie = cookie.http_only
let partitioned cookie = cookie.partitioned
let host_only cookie = cookie.host_only
let expires cookie = cookie.expires
let max_age cookie = cookie.max_age
let same_site cookie = cookie.same_site
let creation_time cookie = cookie.creation_time
let last_access cookie = cookie.last_access

let make ~domain ~path ~name ~value ?(secure = false) ?(http_only = false)
    ?expires ?max_age ?same_site ?(partitioned = false) ?(host_only = false)
    ~creation_time ~last_access () =
  {
    domain;
    path;
    name;
    value;
    secure;
    http_only;
    partitioned;
    host_only;
    expires;
    max_age;
    same_site;
    creation_time;
    last_access;
  }

(** {1 RFC 6265 Validation}

    Validation functions for cookie names, values, and attributes per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} RFC 6265 Section 4.1.1}.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 - Syntax *)
module Validate = struct
  (** Check if a character is a valid RFC 2616 token character.

      Per RFC 6265, cookie-name must be a token as defined in RFC 2616 Section 2.2:
      token = 1*<any CHAR except CTLs or separators>
      separators = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" | "\" |
                   <"> | "/" | "[" | "]" | "?" | "=" | "{" | "}" | SP | HT

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)
  let is_token_char = function
    | '\x00' .. '\x1F' | '\x7F' -> false (* CTL characters *)
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
    | ']' | '?' | '=' | '{' | '}' | ' ' ->
        false (* separators - note: HT (0x09) is already covered by CTL range *)
    | _ -> true

  (** Validate a cookie name per RFC 6265.

      Cookie names must be valid RFC 2616 tokens: one or more characters
      excluding control characters and separators.

      @param name The cookie name to validate
      @return [Ok name] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)
  let cookie_name name =
    let len = String.length name in
    if len = 0 then
      Error "Cookie name is empty; RFC 6265 requires at least one character"
    else
      let rec find_invalid i acc =
        if i >= len then acc
        else
          let c = String.unsafe_get name i in
          if is_token_char c then find_invalid (i + 1) acc
          else find_invalid (i + 1) (c :: acc)
      in
      match find_invalid 0 [] with
      | [] -> Ok name
      | invalid_chars ->
          let chars_str =
            invalid_chars
            |> List.rev
            |> List.map (fun c -> Printf.sprintf "%C" c)
            |> String.concat ", "
          in
          Error
            (Printf.sprintf
               "Cookie name %S contains invalid characters: %s. RFC 6265 requires \
                cookie names to be valid tokens (no control characters, spaces, \
                or separators like ()[]{}=,;:@\\\"/?<>)"
               name chars_str)

  (** Check if a character is a valid cookie-octet.

      Per RFC 6265 Section 4.1.1:
      cookie-octet = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
      (US-ASCII excluding CTLs, whitespace, DQUOTE, comma, semicolon, backslash)

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)
  let is_cookie_octet = function
    | '\x21' -> true (* ! *)
    | '\x23' .. '\x2B' -> true (* # $ % & ' ( ) * + *)
    | '\x2D' .. '\x3A' -> true (* - . / 0-9 : *)
    | '\x3C' .. '\x5B' -> true (* < = > ? @ A-Z [ *)
    | '\x5D' .. '\x7E' -> true (* ] ^ _ ` a-z { | } ~ *)
    | _ -> false

  (** Validate a cookie value per RFC 6265.

      Cookie values must contain only cookie-octets, optionally wrapped in
      double quotes. Invalid characters include: control characters, space,
      double quote (except as wrapper), comma, semicolon, and backslash.

      @param value The cookie value to validate
      @return [Ok value] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)
  let cookie_value value =
    (* Handle optional DQUOTE wrapper *)
    let len = String.length value in
    let inner_value, inner_len =
      if len >= 2 && value.[0] = '"' && value.[len - 1] = '"' then
        (String.sub value 1 (len - 2), len - 2)
      else (value, len)
    in
    let rec find_invalid i acc =
      if i >= inner_len then acc
      else
        let c = String.unsafe_get inner_value i in
        if is_cookie_octet c then find_invalid (i + 1) acc
        else find_invalid (i + 1) (c :: acc)
    in
    match find_invalid 0 [] with
    | [] -> Ok value
    | invalid_chars ->
        let chars_str =
          invalid_chars
          |> List.rev
          |> List.map (fun c ->
                 match c with
                 | ' ' -> "space (0x20)"
                 | '"' -> "double-quote (0x22)"
                 | ',' -> "comma (0x2C)"
                 | ';' -> "semicolon (0x3B)"
                 | '\\' -> "backslash (0x5C)"
                 | c when Char.code c < 0x20 ->
                     Printf.sprintf "control char (0x%02X)" (Char.code c)
                 | c -> Printf.sprintf "%C (0x%02X)" c (Char.code c))
          |> String.concat ", "
        in
        Error
          (Printf.sprintf
             "Cookie value %S contains invalid characters: %s. RFC 6265 cookie \
              values may only contain printable ASCII excluding space, \
              double-quote, comma, semicolon, and backslash"
             value chars_str)

  (** Validate a domain attribute value.

      Domain values must be either:
      - A valid domain name per RFC 1034 Section 3.5
      - A valid IPv4 address
      - A valid IPv6 address

      @param domain The domain value to validate (leading dot is stripped first)
      @return [Ok domain] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.2.3> RFC 6265 Section 4.1.2.3
      @see <https://datatracker.ietf.org/doc/html/rfc1034#section-3.5> RFC 1034 Section 3.5 *)
  let domain_value domain =
    (* Strip leading dot per RFC 6265 Section 5.2.3 *)
    let domain =
      if String.starts_with ~prefix:"." domain && String.length domain > 1 then
        String.sub domain 1 (String.length domain - 1)
      else domain
    in
    if String.length domain = 0 then
      Error "Domain attribute is empty"
    else
      (* First check if it's an IP address *)
      match Ipaddr.of_string domain with
      | Ok _ -> Ok domain (* Valid IP address *)
      | Error _ -> (
          (* Not an IP, validate as domain name using domain-name library *)
          match Domain_name.of_string domain with
          | Ok _ -> Ok domain
          | Error (`Msg msg) ->
              Error
                (Printf.sprintf
                   "Domain %S is not a valid domain name: %s. Domain names \
                    must follow RFC 1034: labels must start with a letter, \
                    contain only letters/digits/hyphens, not end with a \
                    hyphen, and be at most 63 characters each"
                   domain msg))

  (** Validate a path attribute value.

      Per RFC 6265 Section 4.1.1, path-value may contain any CHAR except
      control characters and semicolon.

      @param path The path value to validate
      @return [Ok path] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)
  let path_value path =
    let len = String.length path in
    let rec find_invalid i acc =
      if i >= len then acc
      else
        let c = String.unsafe_get path i in
        match c with
        | '\x00' .. '\x1F' | '\x7F' | ';' -> find_invalid (i + 1) (c :: acc)
        | _ -> find_invalid (i + 1) acc
    in
    match find_invalid 0 [] with
    | [] -> Ok path
    | invalid_chars ->
        let chars_str =
          invalid_chars
          |> List.rev
          |> List.map (fun c -> Printf.sprintf "0x%02X" (Char.code c))
          |> String.concat ", "
        in
        Error
          (Printf.sprintf
             "Path %S contains invalid characters: %s. Paths may not contain \
              control characters or semicolons"
             path chars_str)

  (** Validate a Max-Age attribute value.

      Per RFC 6265 Section 4.1.1, max-age-av uses non-zero-digit *DIGIT.
      However, per Section 5.2.2, user agents should treat values <= 0 as
      "delete immediately". This function returns [Ok] for any integer since
      the parsing code handles negative values by converting to 0.

      @param seconds The Max-Age value in seconds
      @return [Ok seconds] always (negative values are handled in parsing)

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1
      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.2> RFC 6265 Section 5.2.2 *)
  let max_age seconds = Ok seconds
end

(** {1 Public Suffix Validation}

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3 Step 5},
    cookies with Domain attributes that are public suffixes must be rejected
    to prevent domain-wide cookie attacks.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model
    @see <https://publicsuffix.org/list/> Public Suffix List *)

(** Module-level Public Suffix List instance.

    Lazily initialized on first use. The PSL data is compiled into the
    publicsuffix library at build time from the Mozilla Public Suffix List. *)
let psl = lazy (Publicsuffix.create ())

(** Validate that a cookie domain is not a public suffix.

    Per RFC 6265 Section 5.3 Step 5, user agents MUST reject cookies where
    the Domain attribute is a public suffix (e.g., ".com", ".co.uk") unless
    the request host exactly matches that domain.

    This prevents attackers from setting domain-wide cookies that would affect
    all sites under a TLD.

    @param request_domain The host from the HTTP request
    @param cookie_domain The Domain attribute value (already normalized, without leading dot)
    @return [Ok ()] if the domain is allowed, [Error msg] if it's a public suffix

    Examples:
    - Request from "www.example.com", Domain=".com" → Error (public suffix)
    - Request from "www.example.com", Domain=".example.com" → Ok (not public suffix)
    - Request from "com", Domain=".com" → Ok (request host matches domain exactly)

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 *)
let validate_not_public_suffix ~request_domain ~cookie_domain =
  (* IP addresses bypass PSL check per RFC 6265 Section 5.1.3 *)
  match Ipaddr.of_string cookie_domain with
  | Ok _ -> Ok () (* IP addresses are not subject to PSL rules *)
  | Error _ -> (
      let psl = Lazy.force psl in
      match Publicsuffix.is_public_suffix psl cookie_domain with
      | Error _ | Ok false ->
          (* If PSL lookup fails (e.g., invalid domain) or not a public suffix,
             allow the cookie. Domain name validation is handled separately. *)
          Ok ()
      | Ok true ->
          (* It's a public suffix - only allow if request host matches exactly.
             This allows a server that IS a public suffix (rare but possible with
             private domains like blogspot.com) to set cookies for itself. *)
          let request_lower = String.lowercase_ascii request_domain in
          let cookie_lower = String.lowercase_ascii cookie_domain in
          if request_lower = cookie_lower then Ok ()
          else
            Error
              (Printf.sprintf
                 "Domain %S is a public suffix; RFC 6265 Section 5.3 prohibits \
                  setting cookies for public suffixes to prevent domain-wide \
                  cookie attacks. The request host %S does not exactly match \
                  the domain."
                 cookie_domain request_domain))

(** {1 Cookie Parsing Helpers} *)

(** Normalize a domain by stripping the leading dot.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3} RFC 6265 Section 5.2.3},
    if the first character of the Domain attribute value is ".", that character
    is ignored (the domain remains case-insensitive).

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3> RFC 6265 Section 5.2.3 - The Domain Attribute *)
let normalize_domain domain =
  match String.starts_with ~prefix:"." domain with
  | true when String.length domain > 1 ->
      String.sub domain 1 (String.length domain - 1)
  | _ -> domain

(** {1 HTTP Date Parsing}

    Date parsing follows {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1} RFC 6265 Section 5.1.1}
    which requires parsing dates in various HTTP formats. *)

module DateParser = struct
  (** Month name to number mapping (case-insensitive).

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1} RFC 6265 Section 5.1.1},
      month tokens are matched case-insensitively.

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1> RFC 6265 Section 5.1.1 - Dates *)
  let month_of_string s =
    match String.lowercase_ascii s with
    | "jan" -> Some 1
    | "feb" -> Some 2
    | "mar" -> Some 3
    | "apr" -> Some 4
    | "may" -> Some 5
    | "jun" -> Some 6
    | "jul" -> Some 7
    | "aug" -> Some 8
    | "sep" -> Some 9
    | "oct" -> Some 10
    | "nov" -> Some 11
    | "dec" -> Some 12
    | _ -> None

  (** Normalize abbreviated years per RFC 6265.

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1} RFC 6265 Section 5.1.1}:
      - Years 70-99 get 1900 added (e.g., 95 → 1995)
      - Years 0-69 get 2000 added (e.g., 25 → 2025)
      - Years >= 100 are returned as-is

      Note: This implementation treats year 69 as 1969 (adding 1900), which
      technically differs from the RFC's "70 and less than or equal to 99" rule.

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1> RFC 6265 Section 5.1.1 - Dates *)
  let normalize_year year =
    if year >= 0 && year <= 68 then year + 2000
    else if year >= 69 && year <= 99 then year + 1900
    else year

  (** Parse FMT1: "Wed, 21 Oct 2015 07:28:00 GMT" (RFC 1123) *)
  let parse_fmt1 s =
    try
      Scanf.sscanf s "%s %d %s %d %d:%d:%d %s"
        (fun _wday day mon year hour min sec tz ->
          (* Check timezone is GMT (case-insensitive) *)
          if String.lowercase_ascii tz <> "gmt" then None
          else
            match month_of_string mon with
            | None -> None
            | Some month ->
                let year = normalize_year year in
                Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)))
    with _ -> None

  (** Parse FMT2: "Wednesday, 21-Oct-15 07:28:00 GMT" (RFC 850) *)
  let parse_fmt2 s =
    try
      Scanf.sscanf s "%[^,], %d-%3s-%d %d:%d:%d %s"
        (fun _wday day mon year hour min sec tz ->
          (* Check timezone is GMT (case-insensitive) *)
          if String.lowercase_ascii tz <> "gmt" then None
          else
            match month_of_string mon with
            | None -> None
            | Some month ->
                let year = normalize_year year in
                Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)))
    with _ -> None

  (** Parse FMT3: "Wed Oct 21 07:28:00 2015" (asctime) *)
  let parse_fmt3 s =
    try
      Scanf.sscanf s "%s %s %d %d:%d:%d %d"
        (fun _wday mon day hour min sec year ->
          match month_of_string mon with
          | None -> None
          | Some month ->
              let year = normalize_year year in
              Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)))
    with _ -> None

  (** Parse FMT4: "Wed, 21-Oct-2015 07:28:00 GMT" (variant) *)
  let parse_fmt4 s =
    try
      Scanf.sscanf s "%s %d-%3s-%d %d:%d:%d %s"
        (fun _wday day mon year hour min sec tz ->
          (* Check timezone is GMT (case-insensitive) *)
          if String.lowercase_ascii tz <> "gmt" then None
          else
            match month_of_string mon with
            | None -> None
            | Some month ->
                let year = normalize_year year in
                Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)))
    with _ -> None

  (** Parse HTTP date by trying all supported formats in sequence *)
  let parse_http_date s =
    let ( <|> ) a b = match a with Some _ -> a | None -> b () in
    parse_fmt1 s <|> fun () ->
    parse_fmt2 s <|> fun () ->
    parse_fmt3 s <|> fun () ->
    parse_fmt4 s

  (** Format a Ptime.t as an HTTP-date (rfc1123-date format).

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} RFC 6265 Section 4.1.1},
      the Expires attribute uses sane-cookie-date which references
      {{:https://datatracker.ietf.org/doc/html/rfc1123#section-5.2.14} RFC 1123 Section 5.2.14}.

      Format: "Sun, 06 Nov 1994 08:49:37 GMT"

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)
  let format_http_date time =
    let (year, month, day), ((hour, min, sec), _tz_offset) = Ptime.to_date_time time in
    let weekday = match Ptime.weekday time with
      | `Sun -> "Sun" | `Mon -> "Mon" | `Tue -> "Tue" | `Wed -> "Wed"
      | `Thu -> "Thu" | `Fri -> "Fri" | `Sat -> "Sat"
    in
    let month_name = [| ""; "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                        "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |].(month) in
    Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
      weekday day month_name year hour min sec
end

(** {1 Cookie Parsing} *)

type cookie_attributes = {
  mutable domain : string option;
  mutable path : string option;
  mutable secure : bool;
  mutable http_only : bool;
  mutable partitioned : bool;
  mutable expires : Expiration.t option;
  mutable max_age : Ptime.Span.t option;
  mutable same_site : SameSite.t option;
}
(** Accumulated attributes from parsing Set-Cookie header *)

(** Create empty attribute accumulator *)
let empty_attributes () =
  {
    domain = None;
    path = None;
    secure = false;
    http_only = false;
    partitioned = false;
    expires = None;
    max_age = None;
    same_site = None;
  }

(** Parse a single cookie attribute and update the accumulator in-place.

    Attribute parsing follows {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2} RFC 6265 Section 5.2}
    which defines the grammar and semantics for each cookie attribute.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2> RFC 6265 Section 5.2 - The Set-Cookie Header *)
let parse_attribute now attrs attr_name attr_value =
  let attr_lower = String.lowercase_ascii attr_name in
  match attr_lower with
  | "domain" -> attrs.domain <- Some (normalize_domain attr_value)
  | "path" -> attrs.path <- Some attr_value
  | "expires" -> (
      if
        (* Special case: Expires=0 means session cookie *)
        attr_value = "0"
      then attrs.expires <- Some `Session
      else
        match Ptime.of_rfc3339 attr_value with
        | Ok (time, _, _) -> attrs.expires <- Some (`DateTime time)
        | Error (`RFC3339 (_, err)) -> (
            (* Try HTTP date format as fallback *)
            match DateParser.parse_http_date attr_value with
            | Some time -> attrs.expires <- Some (`DateTime time)
            | None ->
                Log.warn (fun m ->
                    m "Failed to parse expires attribute '%s': %a" attr_value
                      Ptime.pp_rfc3339_error err)))
  | "max-age" -> (
      match int_of_string_opt attr_value with
      | Some seconds ->
          (* Handle negative values as 0 per RFC 6265 *)
          let seconds = max 0 seconds in
          let current_time = now () in
          (* Store the max-age as a Ptime.Span *)
          attrs.max_age <- Some (Ptime.Span.of_int_s seconds);
          (* Also compute and store expires as DateTime *)
          let expires =
            Ptime.add_span current_time (Ptime.Span.of_int_s seconds)
          in
          (match expires with
          | Some time -> attrs.expires <- Some (`DateTime time)
          | None -> ());
          Log.debug (fun m -> m "Parsed Max-Age: %d seconds" seconds)
      | None ->
          Log.warn (fun m ->
              m "Failed to parse max-age attribute '%s'" attr_value))
  | "secure" -> attrs.secure <- true
  | "httponly" -> attrs.http_only <- true
  | "partitioned" -> attrs.partitioned <- true
  | "samesite" -> (
      match String.lowercase_ascii attr_value with
      | "strict" -> attrs.same_site <- Some `Strict
      | "lax" -> attrs.same_site <- Some `Lax
      | "none" -> attrs.same_site <- Some `None
      | _ ->
          Log.warn (fun m ->
              m "Invalid samesite value '%s', ignoring" attr_value))
  | _ ->
      Log.debug (fun m -> m "Unknown cookie attribute '%s', ignoring" attr_name)

(** Validate cookie attributes and log warnings for invalid combinations.

    Validates:
    - SameSite=None requires the Secure flag (per RFC 6265bis)
    - Partitioned requires the Secure flag (per CHIPS specification)

    @see <https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.4.7> RFC 6265bis Section 5.4.7 - SameSite
    @see <https://developer.chrome.com/docs/privacy-sandbox/chips/> CHIPS - Cookies Having Independent Partitioned State *)
let validate_attributes attrs =
  match (attrs.same_site, attrs.secure, attrs.partitioned) with
  | Some `None, false, _ ->
      Log.warn (fun m ->
          m
            "Cookie has SameSite=None but Secure flag is not set; this \
             violates RFC requirements");
      false
  | _, false, true ->
      Log.warn (fun m ->
          m
            "Cookie has Partitioned attribute but Secure flag is not set; this \
             violates CHIPS requirements");
      false
  | _ -> true

(** Build final cookie from name/value and accumulated attributes.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3}:
    - If Domain attribute is present, host-only-flag = false, domain = attribute value
    - If Domain attribute is absent, host-only-flag = true, domain = request host

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)
let build_cookie ~request_domain ~request_path ~name ~value attrs ~now =
  let host_only, domain =
    match attrs.domain with
    | Some d -> (false, normalize_domain d)
    | None -> (true, request_domain)
  in
  let path = Option.value attrs.path ~default:request_path in
  make ~domain ~path ~name ~value ~secure:attrs.secure
    ~http_only:attrs.http_only ?expires:attrs.expires ?max_age:attrs.max_age
    ?same_site:attrs.same_site ~partitioned:attrs.partitioned ~host_only
    ~creation_time:now ~last_access:now ()

(** {1 Pretty Printing} *)

let pp ppf cookie =
  Format.fprintf ppf
    "@[<hov 2>{ name=%S;@ value=%S;@ domain=%S;@ path=%S;@ secure=%b;@ \
     http_only=%b;@ partitioned=%b;@ host_only=%b;@ expires=%a;@ max_age=%a;@ \
     same_site=%a }@]"
    (name cookie) (value cookie) (domain cookie) (path cookie) (secure cookie)
    (http_only cookie) (partitioned cookie) (host_only cookie)
    (Format.pp_print_option Expiration.pp)
    (expires cookie)
    (Format.pp_print_option Ptime.Span.pp)
    (max_age cookie)
    (Format.pp_print_option SameSite.pp)
    (same_site cookie)

(** {1 Cookie Parsing} *)

(** Parse a Set-Cookie HTTP response header.

    Parses the header according to {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2} RFC 6265 Section 5.2},
    extracting the cookie name, value, and all attributes. Returns [Error msg] if
    the cookie is invalid or fails validation, with a descriptive error message.

    @param now Function returning current time for Max-Age computation
    @param domain The request host (used as default domain)
    @param path The request path (used as default path)
    @param header_value The Set-Cookie header value string
    @return [Ok cookie] if parsing succeeds, [Error msg] with explanation if invalid

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2> RFC 6265 Section 5.2 - The Set-Cookie Header *)
let of_set_cookie_header ~now ~domain:request_domain ~path:request_path
    header_value =
  Log.debug (fun m -> m "Parsing Set-Cookie: %s" header_value);

  (* Split into attributes *)
  let parts = String.split_on_char ';' header_value |> List.map String.trim in

  match parts with
  | [] -> Error "Empty Set-Cookie header"
  | name_value :: attrs -> (
      (* Parse name=value *)
      match String.index_opt name_value '=' with
      | None ->
          Error
            (Printf.sprintf
               "Set-Cookie header missing '=' separator in name-value pair: %S"
               name_value)
      | Some eq_pos -> (
          let name = String.sub name_value 0 eq_pos |> String.trim in
          let cookie_value =
            String.sub name_value (eq_pos + 1)
              (String.length name_value - eq_pos - 1)
            |> String.trim
          in

          (* Validate cookie name per RFC 6265 *)
          match Validate.cookie_name name with
          | Error msg -> Error msg
          | Ok name -> (
              (* Validate cookie value per RFC 6265 *)
              match Validate.cookie_value cookie_value with
              | Error msg -> Error msg
              | Ok cookie_value ->
                  let current_time = now () in

                  (* Parse all attributes into mutable accumulator *)
                  let accumulated_attrs = empty_attributes () in
                  let attr_errors = ref [] in
                  List.iter
                    (fun attr ->
                      match String.index_opt attr '=' with
                      | None ->
                          (* Attribute without value (e.g., Secure, HttpOnly) *)
                          parse_attribute now accumulated_attrs attr ""
                      | Some eq ->
                          let attr_name = String.sub attr 0 eq |> String.trim in
                          let attr_value =
                            String.sub attr (eq + 1)
                              (String.length attr - eq - 1)
                            |> String.trim
                          in
                          (* Validate domain and path attributes *)
                          (match String.lowercase_ascii attr_name with
                          | "domain" -> (
                              match Validate.domain_value attr_value with
                              | Error msg -> attr_errors := msg :: !attr_errors
                              | Ok _ -> ())
                          | "path" -> (
                              match Validate.path_value attr_value with
                              | Error msg -> attr_errors := msg :: !attr_errors
                              | Ok _ -> ())
                          | "max-age" -> (
                              match int_of_string_opt attr_value with
                              | Some seconds -> (
                                  match Validate.max_age seconds with
                                  | Error msg ->
                                      attr_errors := msg :: !attr_errors
                                  | Ok _ -> ())
                              | None -> ())
                          | _ -> ());
                          parse_attribute now accumulated_attrs attr_name
                            attr_value)
                    attrs;

                  (* Check for attribute validation errors *)
                  if List.length !attr_errors > 0 then
                    Error (String.concat "; " (List.rev !attr_errors))
                  else if not (validate_attributes accumulated_attrs) then
                    Error
                      "Cookie validation failed: SameSite=None requires \
                       Secure flag, and Partitioned requires Secure flag"
                  else
                    (* Public suffix validation per RFC 6265 Section 5.3 Step 5.
                       Only applies when Domain attribute is present. *)
                    let psl_result =
                      match accumulated_attrs.domain with
                      | None ->
                          (* No Domain attribute - cookie is host-only, no PSL check needed *)
                          Ok ()
                      | Some cookie_domain ->
                          let normalized = normalize_domain cookie_domain in
                          validate_not_public_suffix ~request_domain ~cookie_domain:normalized
                    in
                    (match psl_result with
                    | Error msg -> Error msg
                    | Ok () ->
                        let cookie =
                          build_cookie ~request_domain ~request_path ~name
                            ~value:cookie_value accumulated_attrs ~now:current_time
                        in
                        Log.debug (fun m -> m "Parsed cookie: %a" pp cookie);
                        Ok cookie))))

(** Parse a Cookie HTTP request header.

    Parses the header according to {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.2} RFC 6265 Section 4.2}.
    The Cookie header contains semicolon-separated name=value pairs.

    Validates cookie names and values per RFC 6265 and detects duplicate
    cookie names (which is an error per Section 4.2.1).

    Cookies parsed from the Cookie header have [host_only = true] since we
    cannot determine from the header alone whether they originally had a
    Domain attribute.

    @param now Function returning current time for timestamps
    @param domain The request host (assigned to all parsed cookies)
    @param path The request path (assigned to all parsed cookies)
    @param header_value The Cookie header value string
    @return [Ok cookies] if all cookies parse successfully with no duplicates,
            [Error msg] with explanation if validation fails

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.2> RFC 6265 Section 4.2 - The Cookie Header *)
let of_cookie_header ~now ~domain ~path header_value =
  Log.debug (fun m -> m "Parsing Cookie header: %s" header_value);

  (* Split on semicolons *)
  let parts = String.split_on_char ';' header_value |> List.map String.trim in

  (* Filter out empty parts *)
  let parts = List.filter (fun s -> String.length s > 0) parts in

  (* Parse each name=value pair, collecting results *)
  let results =
    List.fold_left
      (fun acc name_value ->
        match acc with
        | Error _ -> acc (* Propagate earlier errors *)
        | Ok (cookies, seen_names) -> (
            match String.index_opt name_value '=' with
            | None ->
                Error
                  (Printf.sprintf "Cookie missing '=' separator: %S" name_value)
            | Some eq_pos -> (
                let cookie_name =
                  String.sub name_value 0 eq_pos |> String.trim
                in
                (* Validate cookie name per RFC 6265 *)
                match Validate.cookie_name cookie_name with
                | Error msg -> Error msg
                | Ok cookie_name -> (
                    (* Check for duplicate names per RFC 6265 Section 4.2.1 *)
                    if List.mem cookie_name seen_names then
                      Error
                        (Printf.sprintf
                           "Duplicate cookie name %S in Cookie header; RFC \
                            6265 Section 4.2.1 forbids duplicate names"
                           cookie_name)
                    else
                      let cookie_value =
                        String.sub name_value (eq_pos + 1)
                          (String.length name_value - eq_pos - 1)
                        |> String.trim
                      in
                      (* Validate cookie value per RFC 6265 *)
                      match Validate.cookie_value cookie_value with
                      | Error msg -> Error msg
                      | Ok cookie_value ->
                          let current_time = now () in
                          (* Create cookie with defaults from Cookie header context.
                             Cookies from Cookie headers have host_only=true since we don't
                             know if they originally had a Domain attribute. *)
                          let cookie =
                            make ~domain ~path ~name:cookie_name
                              ~value:cookie_value ~secure:false ~http_only:false
                              ~partitioned:false ~host_only:true
                              ~creation_time:current_time
                              ~last_access:current_time ()
                          in
                          Ok (cookie :: cookies, cookie_name :: seen_names)))))
      (Ok ([], []))
      parts
  in
  match results with
  | Error msg -> Error msg
  | Ok (cookies, _) -> Ok (List.rev cookies)

(** Generate a Cookie HTTP request header from a list of cookies.

    Formats cookies according to {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.2} RFC 6265 Section 4.2}
    as semicolon-separated name=value pairs.

    @param cookies List of cookies to include
    @return The Cookie header value string

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.2> RFC 6265 Section 4.2 - The Cookie Header *)
let make_cookie_header cookies =
  cookies
  |> List.map (fun c -> Printf.sprintf "%s=%s" (name c) (value c))
  |> String.concat "; "

(** Generate a Set-Cookie HTTP response header from a cookie.

    Formats the cookie according to {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1} RFC 6265 Section 4.1}
    including all attributes.

    Note: The Expires attribute is currently formatted using RFC 3339, which
    differs from the RFC-recommended rfc1123-date format.

    @param cookie The cookie to serialize
    @return The Set-Cookie header value string

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1> RFC 6265 Section 4.1 - The Set-Cookie Header *)
let make_set_cookie_header cookie =
  let buffer = Buffer.create 128 in
  Buffer.add_string buffer (Printf.sprintf "%s=%s" (name cookie) (value cookie));

  (* Add Max-Age if present *)
  Option.iter
    (fun span ->
      Option.iter
        (fun seconds ->
          Buffer.add_string buffer (Printf.sprintf "; Max-Age=%d" seconds))
        (Ptime.Span.to_int_s span))
    (max_age cookie);

  (* Add Expires if present - using RFC 1123 date format per RFC 6265 Section 4.1.1 *)
  Option.iter
    (function
      | `Session -> Buffer.add_string buffer "; Expires=0"
      | `DateTime exp_time ->
          let exp_str = DateParser.format_http_date exp_time in
          Buffer.add_string buffer (Printf.sprintf "; Expires=%s" exp_str))
    (expires cookie);

  (* Add Domain *)
  Buffer.add_string buffer (Printf.sprintf "; Domain=%s" (domain cookie));

  (* Add Path *)
  Buffer.add_string buffer (Printf.sprintf "; Path=%s" (path cookie));

  (* Add Secure flag *)
  if secure cookie then Buffer.add_string buffer "; Secure";

  (* Add HttpOnly flag *)
  if http_only cookie then Buffer.add_string buffer "; HttpOnly";

  (* Add Partitioned flag *)
  if partitioned cookie then Buffer.add_string buffer "; Partitioned";

  (* Add SameSite *)
  Option.iter
    (function
      | `Strict -> Buffer.add_string buffer "; SameSite=Strict"
      | `Lax -> Buffer.add_string buffer "; SameSite=Lax"
      | `None -> Buffer.add_string buffer "; SameSite=None")
    (same_site cookie);

  Buffer.contents buffer
