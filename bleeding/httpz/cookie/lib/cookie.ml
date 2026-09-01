module Same_site = struct
  type t =
    [ `Strict
    | `Lax
    | `None
    ]

  let to_string = function
    | `Strict -> "Strict"
    | `Lax -> "Lax"
    | `None -> "None"
  ;;

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type expiry =
  [ `Session
  | `At of Ptime.t
  ]

type t =
  { domain : string
  ; path : string
  ; name : string
  ; value : string
  ; secure : bool
  ; http_only : bool
  ; host_only : bool
  ; partitioned : bool
  ; same_site : Same_site.t option
  ; expiry : expiry
  ; creation_time : Ptime.t
  ; last_access : Ptime.t
  }

let domain c = c.domain
let path c = c.path
let name c = c.name
let value c = c.value

let strip_quotes v =
  let n = String.length v in
  if n >= 2 && v.[0] = '"' && v.[n - 1] = '"' then String.sub v 1 (n - 2) else v
;;

let value_trimmed c = strip_quotes c.value

let valid_name n = Httpz.Header.Syntax.is_token n

(* Spaces extend cookie-octet for compatibility with browsers and deployed
   servers. *)
let is_cookie_octet = function
  | '\x20' | '\x21' -> true
  | '\x23' .. '\x2B' -> true
  | '\x2D' .. '\x3A' -> true
  | '\x3C' .. '\x5B' -> true
  | '\x5D' .. '\x7E' -> true
  | _ -> false
;;

let valid_value v = String.for_all is_cookie_octet (strip_quotes v)

let valid_domain d =
  let len = String.length d in
  len > 0
  && d.[0] <> '.'
  && d.[len - 1] <> '.'
  && String.equal d (String.lowercase_ascii d)
  && String.for_all (fun c -> Char.code c < 0x80) d
  &&
  (Httpz.Ip.is_literal d
   ||
   match Punycode_idna.to_ascii ~use_std3_rules:true d with
   | ascii -> String.equal ascii d
   | exception Punycode_idna.Error _ -> false)
;;

let valid_path p =
  p <> ""
  && p.[0] = '/'
  && String.for_all
       (fun c ->
          let code = Char.code c in
          code >= 0x20 && code <> 0x3B && code <> 0x7F)
       p
;;

let secure c = c.secure
let http_only c = c.http_only
let host_only c = c.host_only
let partitioned c = c.partitioned
let same_site c = c.same_site
let expiry c = c.expiry
let creation_time c = c.creation_time
let last_access c = c.last_access

let v
      ~domain
      ~path
      ~name
      ~value
      ?(secure = false)
      ?(http_only = false)
      ?(host_only = true)
      ?(partitioned = false)
      ?same_site
      ~expiry
      ~now
      ()
  =
  let domain = String.lowercase_ascii domain in
  (* A name or value outside the grammar would emit a second name-value pair
     into a Cookie header, so it is refused at construction rather than at
     the point where it is serialized. *)
  if not (valid_name name)
  then invalid_arg (Fmt.str "Cookie.v: name %S is not a token" name);
  if not (valid_value value)
  then invalid_arg (Fmt.str "Cookie.v: value %S has an invalid character" value);
  if not (valid_domain domain)
  then invalid_arg (Fmt.str "Cookie.v: domain %S is not canonical" domain);
  if not (valid_path path)
  then invalid_arg (Fmt.str "Cookie.v: path %S is not a cookie path" path);
  { domain
  ; path
  ; name
  ; value
  ; secure
  ; http_only
  ; host_only
  ; partitioned
  ; same_site
  ; expiry
  ; creation_time = now
  ; last_access = now
  }
;;

let touch ~now c = { c with last_access = now }
let with_creation_time t c = { c with creation_time = t }

let is_expired ~now c =
  match c.expiry with
  | `Session -> false
  | `At t -> Ptime.compare now t >= 0
;;

let same_identity a b =
  String.equal a.name b.name
  && String.equal a.domain b.domain
  && String.equal a.path b.path
;;

(* IP literals cannot suffix-match. Recognition is the resolver's, not a
   character class: a host reached as [0x7f.1] would otherwise pass for a DNS
   name and let [Domain=1] suffix-match every other [.1] spelling. *)
let is_ip host = Httpz.Ip.is_literal host

let domain_suffix_matches ~sub d =
  String.equal sub d || (String.ends_with ~suffix:("." ^ d) sub && not (is_ip sub))
;;

let domain_matches ~host c =
  String.equal host c.domain
  || ((not c.host_only) && domain_suffix_matches ~sub:host c.domain)
;;

let path_matches ~request_path c =
  let request_path = if request_path = "" then "/" else request_path in
  let cookie_path = c.path in
  request_path = cookie_path
  || (String.starts_with ~prefix:cookie_path request_path
      && (String.ends_with ~suffix:"/" cookie_path
          || (String.length request_path > String.length cookie_path
              && request_path.[String.length cookie_path] = '/')))
;;

(* The name makes the RFC ordering total when two cookies have equal paths and
   creation times, avoiding dependence on a stable sort. *)
let compare_order a b =
  match Int.compare (String.length b.path) (String.length a.path) with
  | 0 ->
    (match Ptime.compare a.creation_time b.creation_time with
     | 0 -> String.compare a.name b.name
     | n -> n)
  | n -> n
;;

let has_prefix ~prefix name =
  String.length name >= String.length prefix
  && String.lowercase_ascii (String.sub name 0 (String.length prefix))
     = String.lowercase_ascii prefix
;;

let has_secure_prefix name =
  has_prefix ~prefix:"__Secure-" name || has_prefix ~prefix:"__Host-" name
;;

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
;;

(* Two-digit years: 70-99 are 19xx, 0-69 are 20xx (s5.1.1 step 3-4). *)
let normalize_year year =
  if year >= 0 && year <= 69
  then year + 2000
  else if year >= 70 && year <= 99
  then year + 1900
  else year
;;

let date_of ~day ~mon ~year ~hour ~min ~sec =
  match month_of_string mon with
  | None -> None
  | Some month ->
    let year = normalize_year year in
    if year < 1601 || sec > 59 then None
    else Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0))
;;

let is_date_delimiter = function
  | '\x09' | '\x20' .. '\x2f' | '\x3b' .. '\x40'
  | '\x5b' .. '\x60' | '\x7b' .. '\x7e' -> true
  | _ -> false

let date_tokens s =
  let n = String.length s in
  let rec skip i =
    if i < n && is_date_delimiter s.[i] then skip (i + 1) else token i i
  and token first i =
    if i = n then if first = i then [] else [ String.sub s first (i - first) ]
    else if is_date_delimiter s.[i] then
      String.sub s first (i - first) :: skip (i + 1)
    else token first (i + 1)
  in
  skip 0

let leading_digits ~max s =
  let rec loop i =
    if i < String.length s && i < max then
      match s.[i] with '0' .. '9' -> loop (i + 1) | _ -> i
    else i
  in
  let length = loop 0 in
  if length = 0 || (length = max && length < String.length s
                    && match s.[length] with '0' .. '9' -> true | _ -> false)
  then None
  else int_of_string_opt (String.sub s 0 length) |> Option.map (fun n -> n, length)

let parse_time_token s =
  let n = String.length s in
  let field pos =
    if pos >= n then None
    else
      match s.[pos] with
      | '0' .. '9' ->
          if pos + 1 < n then
            match s.[pos + 1] with
            | '0' .. '9' ->
                Some (((Char.code s.[pos] - 48) * 10) + Char.code s.[pos + 1] - 48,
                      pos + 2)
            | _ -> Some (Char.code s.[pos] - 48, pos + 1)
          else Some (Char.code s.[pos] - 48, pos + 1)
      | _ -> None
  in
  match field 0 with
  | Some (hour, colon1) when colon1 < n && s.[colon1] = ':' ->
      (match field (colon1 + 1) with
       | Some (min, colon2) when colon2 < n && s.[colon2] = ':' ->
           (match field (colon2 + 1) with
            | Some (sec, stop)
              when stop = n
                   || (match s.[stop] with '0' .. '9' -> false | _ -> true) ->
                Some (hour, min, sec)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* RFC 6265 section 5.1.1 deliberately recognizes date components as tokens,
   in any order, to accommodate the historical spellings found in practice. *)
let parse_http_date s =
  let day = ref None and month = ref None and year = ref None and time = ref None in
  List.iter
    (fun token ->
       let token_month =
         if String.length token >= 3
            && (String.length token = 3
                || match token.[3] with '0' .. '9' -> false | _ -> true)
         then month_of_string (String.sub token 0 3)
         else None
       in
       match !time, parse_time_token token with
       | None, (Some _ as parsed) -> time := parsed
       | _ when !day = None && Option.is_some (leading_digits ~max:2 token) ->
           day := Option.map fst (leading_digits ~max:2 token)
       | _ when !month = None && token_month <> None -> month := token_month
       | _ when !year = None ->
           (match leading_digits ~max:4 token with
            | Some (value, length) when length >= 2 -> year := Some value
            | _ -> ())
       | _ -> ())
    (date_tokens s);
  match !day, !month, !year, !time with
  | Some day, Some month, Some year, Some (hour, min, sec) ->
      let months =
        [| "jan"; "feb"; "mar"; "apr"; "may"; "jun";
           "jul"; "aug"; "sep"; "oct"; "nov"; "dec" |]
      in
      date_of ~day ~mon:months.(month - 1) ~year ~hour ~min ~sec
  | _ -> None
;;

let format_http_date t =
  let (year, month, day), ((hour, min, sec), _) = Ptime.to_date_time t in
  let weekday =
    match Ptime.weekday t with
    | `Sun -> "Sun"
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
  in
  let months =
    [| "Jan"
     ; "Feb"
     ; "Mar"
     ; "Apr"
     ; "May"
     ; "Jun"
     ; "Jul"
     ; "Aug"
     ; "Sep"
     ; "Oct"
     ; "Nov"
     ; "Dec"
    |]
  in
  Fmt.str
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekday
    day
    months.(month - 1)
    year
    hour
    min
    sec
;;

type attrs =
  { mutable a_domain : string option
  ; mutable a_path : string option
  ; mutable a_secure : bool
  ; mutable a_http_only : bool
  ; mutable a_partitioned : bool
  ; mutable a_expires : Ptime.t option
  ; mutable a_max_age : int option
  ; mutable a_same_site : Same_site.t option
  }

let strip_leading_dot d =
  if String.length d > 1 && d.[0] = '.' then String.sub d 1 (String.length d - 1) else d
;;

let int_attr s =
  let body =
    if String.length s > 1 && s.[0] = '-' then String.sub s 1 (String.length s - 1) else s
  in
  if
    body <> ""
    && String.for_all
         (function
           | '0' .. '9' -> true
           | _ -> false)
         body
  then int_of_string_opt s
  else None
;;

(* A malformed duplicate attribute is ignored rather than erasing the last
   valid value. *)
let set_if_parsed field = function
  | None -> ()
  | Some _ as v -> field v
;;

let parse_attribute attrs attr_name attr_value =
  match String.lowercase_ascii attr_name with
  | "domain" ->
    let d = String.lowercase_ascii (strip_leading_dot attr_value) in
    if d <> "" then attrs.a_domain <- Some d
  | "path" -> attrs.a_path <- Some attr_value
  | "expires" ->
    set_if_parsed (fun v -> attrs.a_expires <- v) (parse_http_date attr_value)
  | "max-age" -> set_if_parsed (fun v -> attrs.a_max_age <- v) (int_attr attr_value)
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
  | _ -> ()
;;

let check_prefix ~name attrs =
  if has_prefix ~prefix:"__Secure-" name && not attrs.a_secure
  then Error "__Secure- prefix without the Secure attribute"
  else if has_prefix ~prefix:"__Host-" name
  then
    if not attrs.a_secure
    then Error "__Host- prefix without the Secure attribute"
    else if attrs.a_domain <> None
    then Error "__Host- prefix with a Domain attribute"
    else if attrs.a_path <> Some "/"
    then Error "__Host- prefix without Path=/"
    else Ok ()
  else Ok ()
;;

let check_same_site attrs =
  match attrs.a_same_site, attrs.a_secure with
  | Some `None, false -> Error "SameSite=None without the Secure attribute"
  | _ -> Ok ()
;;

let check_partitioned attrs =
  if attrs.a_partitioned && not attrs.a_secure
  then Error "Partitioned without the Secure attribute"
  else Ok ()
;;

(* RFC 6265 s5.3 step 5: a Domain attribute that is a public suffix is ignored
   when it is the request host itself, and rejects the cookie otherwise. A
   lookup that fails says nothing about the name, so it fails closed: the
   alternative hands a shared-hosting suffix a domain cookie over every site
   beneath it. *)
let check_public_suffix ~host domain =
  match Pubsuffix.is_public_suffix domain with
  | Ok false -> `Store_domain
  | Ok true | Error Pubsuffix.Domain_is_public_suffix ->
    if String.equal host domain
    then `Store_host_only
    else `Reject (Fmt.str "Domain=%s is a public suffix" domain)
  | Error e ->
    `Reject
      (Fmt.str
         "Domain=%s failed the public-suffix lookup: %s"
         domain
         (Pubsuffix.error_to_string e))
;;

let default_path request_path =
  if request_path = "" || request_path.[0] <> '/'
  then "/"
  else (
    match String.rindex request_path '/' with
    | 0 -> "/"
    | i -> String.sub request_path 0 i
    | exception Not_found -> "/")
;;

let parse_set_cookie ~now ~host ~path:request_path line =
  if not (valid_domain host)
  then Error "request host is not a canonical domain or IP literal"
  else
  let parts = String.split_on_char ';' line |> List.map String.trim in
  match parts with
  | [] | [ "" ] -> Error "empty Set-Cookie"
  | name_value :: attr_parts ->
    (match String.index_opt name_value '=' with
     | None -> Error "missing '=' in the name-value pair"
     | Some eq ->
       let name = String.trim (String.sub name_value 0 eq) in
       let value =
         String.trim (String.sub name_value (eq + 1) (String.length name_value - eq - 1))
       in
       if not (valid_name name)
       then Error "cookie name is not a token"
       else if not (valid_value value)
       then Error "cookie value contains an invalid character"
       else (
         let attrs =
           { a_domain = None
           ; a_path = None
           ; a_secure = false
           ; a_http_only = false
           ; a_partitioned = false
           ; a_expires = None
           ; a_max_age = None
           ; a_same_site = None
           }
         in
         List.iter
           (fun part ->
              match String.index_opt part '=' with
              | None -> parse_attribute attrs part ""
              | Some eq ->
                parse_attribute
                  attrs
                  (String.trim (String.sub part 0 eq))
                  (String.trim (String.sub part (eq + 1) (String.length part - eq - 1))))
           attr_parts;
         let ( let* ) = Result.bind in
         let* () = check_same_site attrs in
         let* () = check_partitioned attrs in
         let* () = check_prefix ~name attrs in
         let* () =
           match attrs.a_domain with
           | Some d when not (valid_domain d) -> Error "Domain is not a canonical host"
           | None | Some _ -> Ok ()
         in
         let* host_only, domain =
           match attrs.a_domain with
           | None -> Ok (true, host)
           | Some d ->
             if not (domain_suffix_matches ~sub:host d)
             then Error (Fmt.str "Domain=%s does not cover the request host" d)
             else if is_ip d
             then
               (* An address has no subdomains, so the attribute can only
                  restate the host; anything else is a bid for another
                  address. *)
               if String.equal host d
               then Ok (true, host)
               else Error (Fmt.str "Domain=%s is an IP literal, not the request host" d)
             else (
               match check_public_suffix ~host d with
               | `Store_domain -> Ok (false, d)
               | `Store_host_only -> Ok (true, host)
               | `Reject reason -> Error reason)
         in
         let expiry =
           match attrs.a_max_age with
           | Some seconds ->
             (* [Ptime.min] is expired even when the clock is at the epoch. *)
             if seconds <= 0
             then `At Ptime.min
             else
               `At
                 (Option.value
                    ~default:Ptime.max
                    (Ptime.add_span now (Ptime.Span.of_int_s seconds)))
           | None ->
             (match attrs.a_expires with
              | Some t -> `At t
              | None -> `Session)
         in
         let* path =
           match attrs.a_path with
           | Some p when p <> "" && p.[0] = '/' ->
             if valid_path p
             then Ok p
             else Error "Path contains a control byte or semicolon"
           | _ ->
             let path = default_path request_path in
             Ok (if valid_path path then path else "/")
         in
         Ok
           (v
              ~domain
              ~path
              ~name
              ~value
              ~secure:attrs.a_secure
              ~http_only:attrs.a_http_only
              ~host_only
              ~partitioned:attrs.a_partitioned
              ?same_site:attrs.a_same_site
              ~expiry
              ~now
              ())))
;;

let cookie_header cookies =
  String.concat "; " (List.map (fun c -> c.name ^ "=" ^ c.value) cookies)
;;

let parse_cookie_header line =
  String.split_on_char ';' line
  |> List.filter_map (fun part ->
    match String.index_opt part '=' with
    | None -> None
    | Some eq ->
      let name = String.trim (String.sub part 0 eq) in
      let value = String.trim (String.sub part (eq + 1) (String.length part - eq - 1)) in
      if valid_name name && valid_value value then Some (name, value) else None)
;;

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
  if not c.host_only
  then (
    Buffer.add_string buf "; Domain=";
    Buffer.add_string buf c.domain);
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
;;

let pp_expiry ppf = function
  | `Session -> Format.pp_print_string ppf "session"
  | `At t -> Ptime.pp ppf t
;;

let pp ppf c =
  Format.fprintf
    ppf
    "@[<hov 2>{ name=%S;@ value=%S;@ domain=%S;@ path=%S;@ secure=%b;@ http_only=%b;@ \
     host_only=%b;@ partitioned=%b;@ expiry=%a;@ same_site=%a }@]"
    c.name
    c.value
    c.domain
    c.path
    c.secure
    c.http_only
    c.host_only
    c.partitioned
    pp_expiry
    c.expiry
    (Format.pp_print_option Same_site.pp)
    c.same_site
;;
