(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The RFC 6265 cookie model, both roles: a client parses Set-Cookie
   and emits Cookie, a server parses Cookie and emits Set-Cookie. Pure —
   time arrives as [Ptime.t] arguments; client storage lives in
   [Cookeio_jar]. The client half was reworked in fetch (as its vendored
   Cookie module) and folded back here, replacing the original, laxer
   parser. *)

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-fable-5"]
[@@@ai_provider "Anthropic"]

module Same_site = struct
  type t = [ `Strict | `Lax | `None ]

  let equal (a : t) (b : t) = a = b

  let to_string = function
    | `Strict -> "Strict"
    | `Lax -> "Lax"
    | `None -> "None"

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

(* One expiry, computed at parse time: RFC 6265 s5.3 step 3 gives
   Max-Age precedence over Expires whatever their order in the header,
   so keeping both around would only invite disagreement. *)
type expiry = [ `Session | `At of Ptime.t ]

type t = {
  domain : string;  (* canonical: lowercase, no leading dot *)
  path : string;
  name : string;
  value : string;
  secure : bool;
  http_only : bool;
  host_only : bool;
  partitioned : bool;
  same_site : Same_site.t option;
  expiry : expiry;
  creation_time : Ptime.t;
  last_access : Ptime.t;
}

let domain c = c.domain
let path c = c.path
let name c = c.name
let value c = c.value

(* The DQUOTE wrapper is grammar, not value (s4.1.1). *)
let value_trimmed c =
  let v = c.value in
  let n = String.length v in
  if n >= 2 && v.[0] = '"' && v.[n - 1] = '"' then String.sub v 1 (n - 2)
  else v

let secure c = c.secure
let http_only c = c.http_only
let host_only c = c.host_only
let partitioned c = c.partitioned
let same_site c = c.same_site
let expiry c = c.expiry
let creation_time c = c.creation_time
let last_access c = c.last_access

let v ~domain ~path ~name ~value ?(secure = false) ?(http_only = false)
    ?(host_only = true) ?(partitioned = false) ?same_site ~expiry ~now () =
  { domain = String.lowercase_ascii domain; path; name; value; secure;
    http_only; host_only; partitioned; same_site; expiry;
    creation_time = now; last_access = now }

let touch ~now c = { c with last_access = now }
let with_creation_time t c = { c with creation_time = t }

let is_expired ~now c =
  match c.expiry with
  | `Session -> false
  | `At t -> Ptime.compare now t > 0

(* Cookies replace each other on (name, domain, path) — RFC 6265
   s5.3 step 12. *)
let same_identity a b =
  String.equal a.name b.name
  && String.equal a.domain b.domain
  && String.equal a.path b.path

(* {1 Matching} *)

(* A request host that is an IP literal never suffix-matches (RFC 6265
   s5.1.3). IPv6 literals contain ':'; an all-digits-and-dots name can
   only be an IPv4 literal, since a registered name's last label cannot
   be all digits. *)
let is_ip host =
  String.contains host ':'
  || (host <> ""
      && String.for_all (function '0' .. '9' | '.' -> true | _ -> false) host)

(* [domain_suffix_matches ~sub d]: [sub] domain-matches [d] (identical,
   or a dot-aligned suffix of a non-IP [sub]) per RFC 6265 s5.1.3. Both
   must already be canonical (lowercase). *)
let domain_suffix_matches ~sub d =
  String.equal sub d
  || (String.ends_with ~suffix:("." ^ d) sub && not (is_ip sub))

(* Should [c] be sent to [host]? Host-only cookies need the exact
   host. *)
let domain_matches ~host c =
  String.equal host c.domain
  || ((not c.host_only) && domain_suffix_matches ~sub:host c.domain)

(* RFC 6265 s5.1.4 path matching. *)
let path_matches ~request_path c =
  let cookie_path = c.path in
  request_path = cookie_path
  || (String.starts_with ~prefix:cookie_path request_path
      && (String.ends_with ~suffix:"/" cookie_path
          || (String.length request_path > String.length cookie_path
              && request_path.[String.length cookie_path] = '/')))

(* RFC 6265 s5.4 step 2: longer paths first; equal lengths, earlier
   creation first. Cookies created in the same clock tick fall back to
   the name, so the order is total and the Cookie header is reproducible
   without relying on the sort being stable. *)
let compare_order a b =
  match Int.compare (String.length b.path) (String.length a.path) with
  | 0 ->
    (match Ptime.compare a.creation_time b.creation_time with
     | 0 -> String.compare a.name b.name
     | n -> n)
  | n -> n

(* {1 Syntax (RFC 6265 s4.1.1)} *)

(* RFC 2616 token characters, for cookie names. *)
let is_token_char = function
  | '\x00' .. '\x1F' | '\x7F' -> false (* CTLs, including HT *)
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
  | ']' | '?' | '=' | '{' | '}' | ' ' -> false (* separators *)
  | _ -> true

let valid_name n = n <> "" && String.for_all is_token_char n

(* cookie-octet, plus space: real servers send values with spaces
   ("delete me") and browsers accept them. *)
let is_cookie_octet = function
  | '\x20' | '\x21' -> true
  | '\x23' .. '\x2B' -> true
  | '\x2D' .. '\x3A' -> true
  | '\x3C' .. '\x5B' -> true
  | '\x5D' .. '\x7E' -> true
  | _ -> false

let valid_value v =
  (* An optional DQUOTE wrapper is part of the grammar. *)
  let inner =
    let n = String.length v in
    if n >= 2 && v.[0] = '"' && v.[n - 1] = '"' then String.sub v 1 (n - 2)
    else v
  in
  String.for_all is_cookie_octet inner

(* {1 Name prefixes (RFC 6265bis s4.1.3)} *)

let has_prefix ~prefix name =
  String.length name >= String.length prefix
  && String.lowercase_ascii (String.sub name 0 (String.length prefix))
     = String.lowercase_ascii prefix

(* A __Secure-/__Host- name asserts secure provenance; the attribute
   half of the rule is checked at parse below, the channel half (never
   stored from plaintext) by the jar, which knows the request scheme. *)
let has_secure_prefix name =
  has_prefix ~prefix:"__Secure-" name || has_prefix ~prefix:"__Host-" name

(* {1 HTTP dates (RFC 6265 s5.1.1)} *)

let month_of_string s =
  match String.lowercase_ascii s with
  | "jan" -> Some 1 | "feb" -> Some 2 | "mar" -> Some 3 | "apr" -> Some 4
  | "may" -> Some 5 | "jun" -> Some 6 | "jul" -> Some 7 | "aug" -> Some 8
  | "sep" -> Some 9 | "oct" -> Some 10 | "nov" -> Some 11 | "dec" -> Some 12
  | _ -> None

(* Two-digit years: 70-99 are 19xx, 0-69 are 20xx (s5.1.1 step 3-4). *)
let normalize_year year =
  if year >= 0 && year <= 69 then year + 2000
  else if year >= 70 && year <= 99 then year + 1900
  else year

let date_of ~day ~mon ~year ~hour ~min ~sec =
  match month_of_string mon with
  | None -> None
  | Some month ->
    Ptime.of_date_time ((normalize_year year, month, day), ((hour, min, sec), 0))

let gmt tz = String.lowercase_ascii tz = "gmt"

(* "Wed, 21 Oct 2015 07:28:00 GMT" (RFC 1123) *)
let parse_fmt1 s =
  try
    Scanf.sscanf s "%s %d %s %d %d:%d:%d %s"
      (fun _wday day mon year hour min sec tz ->
         if gmt tz then date_of ~day ~mon ~year ~hour ~min ~sec else None)
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(* "Wednesday, 21-Oct-15 07:28:00 GMT" (RFC 850) *)
let parse_fmt2 s =
  try
    Scanf.sscanf s "%[^,], %d-%3s-%d %d:%d:%d %s"
      (fun _wday day mon year hour min sec tz ->
         if gmt tz then date_of ~day ~mon ~year ~hour ~min ~sec else None)
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(* "Wed Oct 21 07:28:00 2015" (asctime) *)
let parse_fmt3 s =
  try
    Scanf.sscanf s "%s %s %d %d:%d:%d %d"
      (fun _wday mon day hour min sec year ->
         date_of ~day ~mon ~year ~hour ~min ~sec)
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(* "Wed, 21-Oct-2015 07:28:00 GMT" *)
let parse_fmt4 s =
  try
    Scanf.sscanf s "%s %d-%3s-%d %d:%d:%d %s"
      (fun _wday day mon year hour min sec tz ->
         if gmt tz then date_of ~day ~mon ~year ~hour ~min ~sec else None)
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

let parse_http_date s =
  let ( <|> ) a b = match a with Some _ -> a | None -> b () in
  parse_fmt1 s <|> fun () ->
  parse_fmt2 s <|> fun () ->
  parse_fmt3 s <|> fun () -> parse_fmt4 s

(* IMF-fixdate, for the Expires attribute a server emits (s4.1.1's
   sane-cookie-date via RFC 1123). *)
let format_http_date t =
  let (year, month, day), ((hour, min, sec), _) = Ptime.to_date_time t in
  let weekday =
    match Ptime.weekday t with
    | `Sun -> "Sun" | `Mon -> "Mon" | `Tue -> "Tue" | `Wed -> "Wed"
    | `Thu -> "Thu" | `Fri -> "Fri" | `Sat -> "Sat"
  in
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug";
                  "Sep"; "Oct"; "Nov"; "Dec" |] in
  Fmt.str "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekday day months.(month - 1) year hour min sec

(* {1 Set-Cookie parsing (RFC 6265 s5.2/s5.3)} *)

type attrs = {
  mutable a_domain : string option;  (* canonical *)
  mutable a_path : string option;
  mutable a_secure : bool;
  mutable a_http_only : bool;
  mutable a_partitioned : bool;
  mutable a_expires : Ptime.t option;
  mutable a_max_age : int option;
  mutable a_same_site : Same_site.t option;
}

let strip_leading_dot d =
  if String.length d > 1 && d.[0] = '.' then String.sub d 1 (String.length d - 1)
  else d

let int_attr s =
  (* max-age: optional '-', then digits (s5.2.2). *)
  let body =
    if String.length s > 1 && s.[0] = '-' then String.sub s 1 (String.length s - 1)
    else s
  in
  if body <> ""
  && String.for_all (function '0' .. '9' -> true | _ -> false) body
  then int_of_string_opt s
  else None

(* Later occurrences of an attribute override earlier ones (s5.2), but an
   unparseable value is not an occurrence — s5.2.1/s5.2.2 say to ignore
   that cookie-av — so [Max-Age=5; Max-Age=x] keeps the 5 instead of
   losing both. *)
let set_if_parsed field = function None -> () | Some _ as v -> field v

let parse_attribute attrs attr_name attr_value =
  match String.lowercase_ascii attr_name with
  | "domain" ->
    (* s5.2.3: an empty Domain is ignored, leaving the cookie host-only,
       rather than rejecting it. *)
    let d = String.lowercase_ascii (strip_leading_dot attr_value) in
    if d <> "" then attrs.a_domain <- Some d
  | "path" -> attrs.a_path <- Some attr_value
  | "expires" ->
    set_if_parsed (fun v -> attrs.a_expires <- v) (parse_http_date attr_value)
  | "max-age" ->
    set_if_parsed (fun v -> attrs.a_max_age <- v) (int_attr attr_value)
  | "secure" -> attrs.a_secure <- true
  | "httponly" -> attrs.a_http_only <- true
  | "partitioned" -> attrs.a_partitioned <- true
  | "samesite" ->
    set_if_parsed
      (fun v -> attrs.a_same_site <- v)
      (match String.lowercase_ascii attr_value with
       | "strict" -> Some `Strict
       | "lax" -> Some `Lax
       | "none" -> Some `None
       | _ -> None)
  | _ -> () (* unknown attributes are ignored, s5.2 *)

(* RFC 6265bis s4.1.3: __Secure- needs Secure; __Host- needs Secure, no
   Domain, and Path=/. *)
let check_prefix ~name attrs =
  if has_prefix ~prefix:"__Secure-" name && not attrs.a_secure then
    Error "__Secure- prefix without the Secure attribute"
  else if has_prefix ~prefix:"__Host-" name then
    if not attrs.a_secure then Error "__Host- prefix without the Secure attribute"
    else if attrs.a_domain <> None then Error "__Host- prefix with a Domain attribute"
    else if attrs.a_path <> Some "/" then Error "__Host- prefix without Path=/"
    else Ok ()
  else Ok ()

(* SameSite=None requires Secure (RFC 6265bis s5.4.7), as does
   Partitioned (CHIPS). *)
let check_same_site attrs =
  match attrs.a_same_site, attrs.a_secure with
  | Some `None, false -> Error "SameSite=None without the Secure attribute"
  | _ -> Ok ()

let check_partitioned attrs =
  if attrs.a_partitioned && not attrs.a_secure then
    Error "Partitioned without the Secure attribute"
  else Ok ()

(* The public-suffix check of RFC 6265 s5.3 step 5: a [Domain=] that is
   a public suffix is refused unless the request host is exactly that
   suffix. The PSL table is initialized on first use. *)
let psl = lazy (Publicsuffix.create ())

let check_public_suffix ~host domain =
  if is_ip domain then Ok () (* s5.1.3: IPs are outside PSL rules *)
  else
    match Publicsuffix.is_public_suffix (Lazy.force psl) domain with
    | Error _ | Ok false -> Ok ()
    | Ok true ->
      if String.equal host domain then Ok ()
      else Error (Fmt.str "Domain=%s is a public suffix" domain)

(* RFC 6265 s5.1.4: the default path is the request path up to but not
   including its last '/'. *)
let default_path request_path =
  if request_path = "" || request_path.[0] <> '/' then "/"
  else
    match String.rindex request_path '/' with
    | 0 -> "/"
    | i -> String.sub request_path 0 i
    | exception Not_found -> "/"

(* [host] is the canonical (lowercase) request host, [path] the request
   path; [now] resolves Max-Age and stamps creation. Returns a short
   reason on rejection, for tracing — per s5.2, a rejected Set-Cookie is
   otherwise ignored. *)
let parse_set_cookie ~now ~host ~path:request_path line =
  let parts = String.split_on_char ';' line |> List.map String.trim in
  match parts with
  | [] | [ "" ] -> Error "empty Set-Cookie"
  | name_value :: attr_parts ->
    match String.index_opt name_value '=' with
    | None -> Error "missing '=' in the name-value pair"
    | Some eq ->
      let name = String.trim (String.sub name_value 0 eq) in
      let value =
        String.trim
          (String.sub name_value (eq + 1) (String.length name_value - eq - 1))
      in
      if not (valid_name name) then Error "cookie name is not a token"
      else if not (valid_value value) then
        Error "cookie value contains an invalid character"
      else begin
        let attrs = { a_domain = None; a_path = None; a_secure = false;
                      a_http_only = false; a_partitioned = false;
                      a_expires = None; a_max_age = None; a_same_site = None }
        in
        List.iter
          (fun part ->
             match String.index_opt part '=' with
             | None -> parse_attribute attrs part ""
             | Some eq ->
               parse_attribute attrs
                 (String.trim (String.sub part 0 eq))
                 (String.trim
                    (String.sub part (eq + 1) (String.length part - eq - 1))))
          attr_parts;
        let ( let* ) = Result.bind in
        let* () = check_same_site attrs in
        let* () = check_partitioned attrs in
        let* () = check_prefix ~name attrs in
        let* host_only, domain =
          match attrs.a_domain with
          | None -> Ok (true, host)
          | Some d ->
            (* s5.3 step 6: the setting host must domain-match the
               Domain attribute — widening to a parent is allowed,
               capturing an unrelated domain is not. *)
            if not (domain_suffix_matches ~sub:host d) then
              Error (Fmt.str "Domain=%s does not cover the request host" d)
            else
              let* () = check_public_suffix ~host d in
              Ok (false, d)
        in
        (* s5.3 step 3: Max-Age wins over Expires regardless of
           attribute order; s5.2.2: a non-positive Max-Age expires the
           cookie at once. *)
        let expiry =
          match attrs.a_max_age with
          | Some seconds ->
            (* s5.2.2: non-positive means the earliest representable
               time — [Ptime.min], so it is expired even for a clock
               sitting exactly on the epoch. *)
            if seconds <= 0 then `At Ptime.min
            else
              `At
                (Option.value ~default:Ptime.max
                   (Ptime.add_span now (Ptime.Span.of_int_s seconds)))
          | None ->
            (match attrs.a_expires with
             | Some t -> `At t
             | None -> `Session)
        in
        let path =
          match attrs.a_path with
          | Some p when p <> "" && p.[0] = '/' -> p
          | _ -> default_path request_path (* s5.2.4 *)
        in
        Ok
          (v ~domain ~path ~name ~value ~secure:attrs.a_secure
             ~http_only:attrs.a_http_only ~host_only
             ~partitioned:attrs.a_partitioned ?same_site:attrs.a_same_site
             ~expiry ~now ())
      end

(* {1 The Cookie request header (RFC 6265 s4.2)} *)

let cookie_header cookies =
  String.concat "; " (List.map (fun c -> c.name ^ "=" ^ c.value) cookies)

(* A server reads the same header leniently (s4.2.2): keep the
   well-formed pairs, in order, and drop the rest. Repeated names are
   legitimate — same-named cookies from different domains or paths
   arrive together, most specific first. *)
let parse_cookie_header line =
  String.split_on_char ';' line
  |> List.filter_map (fun part ->
      match String.index_opt part '=' with
      | None -> None
      | Some eq ->
        let name = String.trim (String.sub part 0 eq) in
        let value =
          String.trim (String.sub part (eq + 1) (String.length part - eq - 1))
        in
        if valid_name name && valid_value value then Some (name, value)
        else None)

(* {1 The Set-Cookie response header (RFC 6265 s4.1)} *)

let set_cookie_header c =
  let buf = Buffer.create 128 in
  Buffer.add_string buf c.name;
  Buffer.add_char buf '=';
  Buffer.add_string buf c.value;
  (match c.expiry with
   | `Session -> ()
   | `At t ->
     Buffer.add_string buf "; Expires=";
     Buffer.add_string buf (format_http_date t));
  (* Naming a Domain is what widens the cookie to subdomains (s4.1.2.3),
     so a host-only cookie must omit it. *)
  if not c.host_only then begin
    Buffer.add_string buf "; Domain=";
    Buffer.add_string buf c.domain
  end;
  Buffer.add_string buf "; Path=";
  Buffer.add_string buf c.path;
  if c.secure then Buffer.add_string buf "; Secure";
  if c.http_only then Buffer.add_string buf "; HttpOnly";
  if c.partitioned then Buffer.add_string buf "; Partitioned";
  (match c.same_site with
   | None -> ()
   | Some s ->
     Buffer.add_string buf "; SameSite=";
     Buffer.add_string buf (Same_site.to_string s));
  Buffer.contents buf

(* {1 Pretty printing} *)

let pp_expiry ppf = function
  | `Session -> Format.pp_print_string ppf "session"
  | `At t -> Ptime.pp ppf t

let pp ppf c =
  Format.fprintf ppf
    "@[<hov 2>{ name=%S;@ value=%S;@ domain=%S;@ path=%S;@ secure=%b;@ \
     http_only=%b;@ host_only=%b;@ partitioned=%b;@ expiry=%a;@ \
     same_site=%a }@]"
    c.name c.value c.domain c.path c.secure c.http_only c.host_only
    c.partitioned pp_expiry c.expiry
    (Format.pp_print_option Same_site.pp) c.same_site
