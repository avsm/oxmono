(* Typed HTTP header values. *)

type 'a t = {
  name : string;
  list_valued : bool;
  enc : 'a -> string;
  dec : string -> 'a option;
}

let v ?(list_valued = false) name ~encode ~decode =
  { name; list_valued; enc = encode; dec = decode }

let name t = t.name
let encode t x = t.enc x
let decode t s = t.dec s

(* A header defined as a comma-separated list may be split across
   several field lines. *)
let get t hs =
  if t.list_valued then
    match Http.Header.get_multi hs t.name with
    | [] -> None
    | vs -> t.dec (String.concat ", " vs)
  else Option.bind (Http.Header.get hs t.name) t.dec

let pair t x = (t.name, t.enc x)

let text n = v n ~encode:Fun.id ~decode:Option.some

(* {2 Parsing helpers} *)

let ( let* ) = Option.bind

(* [cut c s] splits [s] at the first [c]. *)
let cut c s =
  Option.map
    (fun i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)))
    (String.index_opt s c)

(* Strip a surrounding quote pair and undo quoted-pair escapes: inside
   a quoted string, [\c] stands for [c] (RFC 9110 s5.6.4). *)
let unquote s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then begin
    let buf = Buffer.create (n - 2) in
    let i = ref 1 in
    while !i < n - 1 do
      (match s.[!i] with
       | '\\' when !i + 1 < n - 1 -> incr i; Buffer.add_char buf s.[!i]
       | c -> Buffer.add_char buf c);
      incr i
    done;
    Buffer.contents buf
  end
  else s

(* Split a field value on [sep], respecting quoted strings. *)
let split_on sep s =
  let buf = Buffer.create (String.length s) in
  let acc = ref [] in
  let in_quotes = ref false in
  let escaped = ref false in
  let flush () =
    let part = String.trim (Buffer.contents buf) in
    Buffer.clear buf;
    if part <> "" then acc := part :: !acc
  in
  String.iter
    (fun c ->
       if !escaped then (Buffer.add_char buf c; escaped := false)
       else if !in_quotes && c = '\\' then (Buffer.add_char buf c; escaped := true)
       else if c = '"' then (in_quotes := not !in_quotes; Buffer.add_char buf c)
       else if c = sep && not !in_quotes then flush ()
       else Buffer.add_char buf c)
    s;
  flush ();
  List.rev !acc

let split_commas = split_on ','
let split_semis = split_on ';'

(* The RFC integer grammars are unsigned decimal digits. *)
let is_digits s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

let dec_int s =
  let s = String.trim s in
  if is_digits s then int_of_string_opt s else None

let dec_int64 s =
  let s = String.trim s in
  if is_digits s then Int64.of_string_opt s else None

(* Signed variant, for fields defined as structured-field integers
   (Cache-Status [ttl] may be negative). *)
let dec_int_signed s =
  let s = String.trim s in
  let body =
    if String.length s > 1 && s.[0] = '-' then String.sub s 1 (String.length s - 1)
    else s
  in
  if is_digits body then int_of_string_opt s else None

(* An RFC 9110 s5.6.4 quoted-string: only '"' and '\' are escaped. *)
let quote_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
       if c = '"' || c = '\\' then (Buffer.add_char buf '\\'; Buffer.add_char buf c)
       else Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* Can [s] be sent unquoted? RFC 9110 s5.6.2 tchars. *)
let is_token_value s =
  s <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
         | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.'
         | '^' | '_' | '`' | '|' | '~' -> true
         | _ -> false)
       s

(* [k=v] or [k="v"] as a lowercased key and unquoted value. *)
let param_of p =
  Option.map
    (fun (k, v) ->
       (String.lowercase_ascii (String.trim k), unquote (String.trim v)))
    (cut '=' p)

(* {2 Representation metadata (RFC 9110 s8)} *)

type media_type = { media : string; params : (string * string) list }

let media ?(params = []) media = { media; params }

let content_type =
  v "Content-Type"
    ~encode:(fun mt ->
        String.concat "; "
          (mt.media :: List.map (fun (k, v) -> k ^ "=" ^ v) mt.params))
    ~decode:(fun s ->
        match split_semis s with
        | [] -> None
        | m :: ps ->
          match String.lowercase_ascii m with
          | "" -> None
          | media -> Some { media; params = List.filter_map param_of ps })

let content_length = v "Content-Length" ~encode:Int64.to_string ~decode:dec_int64

let content_encoding =
  v ~list_valued:true "Content-Encoding"
    ~encode:(String.concat ", ")
    ~decode:(fun s ->
        match split_commas s with
        | [] -> None
        | cs -> Some (List.map String.lowercase_ascii cs))

let content_language =
  v ~list_valued:true "Content-Language"
    ~encode:(String.concat ", ")
    ~decode:(fun s -> match split_commas s with [] -> None | ls -> Some ls)

(* {2 Content negotiation (RFC 9110 s12)} *)

type pref = { value : string; q : float option }

let pref ?q value = { value; q }

(* The RFC 9110 s12.4.2 qvalue grammar: 0 to 1, at most three decimals.
   [float_of_string_opt] alone would also accept exponents, hex floats
   and out-of-range values. *)
let parse_q s =
  if s <> ""
  && String.length s <= 5
  && String.for_all (function '0' .. '9' | '.' -> true | _ -> false) s
  then
    match float_of_string_opt s with
    | Some q when q >= 0. && q <= 1. -> Some q
    | _ -> None
  else None

let qvalue_to_string q =
  (* NaN would propagate through the clamp and print as "nan". *)
  let q = if Float.is_nan q then 0. else Float.max 0. (Float.min 1. q) in
  let s = Fmt.str "%.3f" q in
  (* Trim trailing zeros, then a bare trailing dot: 0.700 -> 0.7, 1.000 -> 1 *)
  let i = ref (String.length s - 1) in
  while !i > 0 && s.[!i] = '0' do decr i done;
  if s.[!i] = '.' then decr i;
  String.sub s 0 (!i + 1)

(* The [q] parameter separates preferences from their weights; any
   other parameters stay attached to [value] verbatim, so
   [text/html;level=1;q=0.7] round-trips. *)
let parse_pref s =
  let parts = split_semis s in
  let is_q p = match param_of p with Some ("q", _) -> true | _ -> false in
  let q =
    List.find_map
      (fun p ->
         match param_of p with
         | Some ("q", w) -> parse_q w
         | _ -> None)
      parts
  in
  match String.concat ";" (List.filter (Fun.negate is_q) parts) with
  | "" -> None
  | value -> Some { value; q }

let pref_codec name =
  v ~list_valued:true name
    ~encode:(fun ps ->
        String.concat ", "
          (List.map
             (fun p ->
                match p.q with
                | None -> p.value
                | Some q -> Fmt.str "%s;q=%s" p.value (qvalue_to_string q))
             ps))
    ~decode:(fun s ->
        match List.filter_map parse_pref (split_commas s) with
        | [] -> None
        | ps -> Some ps)

let accept = pref_codec "Accept"
let accept_encoding = pref_codec "Accept-Encoding"
let accept_language = pref_codec "Accept-Language"

(* {2 Conditional requests (RFC 9110 s8.8, s13)}

   HTTP-dates (Last-Modified, If-Modified-Since, Date, Expires) are
   opaque strings: RFC 9110 s13.1.3 tells a client to echo
   the exact Last-Modified value it received, so the common flows never
   need to interpret one. *)

type etag = { weak : bool; tag : string }

let etag_to_string e = Fmt.str "%s\"%s\"" (if e.weak then "W/" else "") e.tag

let parse_etag s =
  let s = String.trim s in
  let weak, rest =
    if String.length s >= 2 && (s.[0] = 'W' || s.[0] = 'w') && s.[1] = '/'
    then (true, String.sub s 2 (String.length s - 2))
    else (false, s)
  in
  let n = String.length rest in
  if n >= 2 && rest.[0] = '"' && rest.[n - 1] = '"' then
    Some { weak; tag = String.sub rest 1 (n - 2) }
  else if rest = "" then None
  else Some { weak; tag = rest } (* tolerate the common unquoted mistake *)

let etag = v "ETag" ~encode:etag_to_string ~decode:parse_etag

type etags = [ `Any | `Etags of etag list ]

let etags_codec name =
  v ~list_valued:true name
    ~encode:(function
        | `Any -> "*"
        | `Etags es -> String.concat ", " (List.map etag_to_string es))
    ~decode:(fun s ->
        if String.trim s = "*" then Some `Any
        else
          match List.filter_map parse_etag (split_commas s) with
          | [] -> None
          | es -> Some (`Etags es))

let if_match = etags_codec "If-Match"
let if_none_match = etags_codec "If-None-Match"

let last_modified = text "Last-Modified"
let if_modified_since = text "If-Modified-Since"
let if_unmodified_since = text "If-Unmodified-Since"
let date = text "Date"

(* {2 Range requests (RFC 9110 s13.1.5, s14)} *)

type range_spec = [ `Range of int64 * int64 option | `Suffix of int64 ]
type range = { unit : string; ranges : range_spec list }

let bytes ranges = { unit = "bytes"; ranges }

let range_spec_to_string = function
  | `Range (first, Some last) -> Fmt.str "%Ld-%Ld" first last
  | `Range (first, None) -> Fmt.str "%Ld-" first
  | `Suffix n -> Fmt.str "-%Ld" n

let parse_range_spec s =
  let s = String.trim s in
  if s = "" then None
  else if s.[0] = '-' then
    Option.map (fun n -> `Suffix n)
      (dec_int64 (String.sub s 1 (String.length s - 1)))
  else
    let* first, rest = cut '-' s in
    let* first = dec_int64 first in
    match rest with
    | "" -> Some (`Range (first, None))
    | last -> Option.map (fun l -> `Range (first, Some l)) (dec_int64 last)

let range =
  v "Range"
    ~encode:(fun r ->
        Fmt.str "%s=%s" r.unit
          (String.concat "," (List.map range_spec_to_string r.ranges)))
    ~decode:(fun s ->
        let* unit, rest = cut '=' s in
        let unit = String.trim unit in
        let specs = split_commas rest in
        let parsed = List.filter_map parse_range_spec specs in
        if unit <> "" && parsed <> [] && List.length parsed = List.length specs
        then Some { unit; ranges = parsed }
        else None)

type if_range = [ `Etag of etag | `Date of string ]

let if_range =
  v "If-Range"
    ~encode:(function `Etag e -> etag_to_string e | `Date d -> d)
    ~decode:(fun s ->
        let s = String.trim s in
        let looks_like_etag =
          (String.length s >= 1 && s.[0] = '"')
          || (String.length s >= 2 && (s.[0] = 'W' || s.[0] = 'w') && s.[1] = '/')
        in
        if s = "" then None
        else if looks_like_etag then
          Option.map (fun e -> `Etag e) (parse_etag s)
        else Some (`Date s))

type content_range = {
  unit : string;
  range : (int64 * int64) option;
  complete_length : int64 option;
}

let complete_range ~first ~last ~complete_length =
  { unit = "bytes"; range = Some (first, last);
    complete_length = Some complete_length }

let content_range =
  v "Content-Range"
    ~encode:(fun cr ->
        let range =
          match cr.range with
          | Some (first, last) -> Fmt.str "%Ld-%Ld" first last
          | None -> "*"
        in
        let length =
          match cr.complete_length with
          | Some len -> Int64.to_string len
          | None -> "*"
        in
        Fmt.str "%s %s/%s" cr.unit range length)
    ~decode:(fun s ->
        let* unit, rest = cut ' ' (String.trim s) in
        let* range_part, length_part = cut '/' rest in
        let* range =
          (* "*" is legitimate (416); a dashed range must parse. *)
          if range_part = "*" then Some None
          else
            let* first, last = cut '-' range_part in
            let* first = dec_int64 first in
            let* last = dec_int64 last in
            Some (Some (first, last))
        in
        let* complete_length =
          (* "*" (unknown) is [None]; anything else must be a number —
             a malformed length fails the decode rather than reading as
             "unknown". *)
          if length_part = "*" then Some None
          else Option.map Option.some (dec_int64 length_part)
        in
        Some { unit; range; complete_length })

type accept_ranges = [ `Bytes | `None | `Other of string ]

let accept_ranges =
  v "Accept-Ranges"
    ~encode:(function `Bytes -> "bytes" | `None -> "none" | `Other s -> s)
    ~decode:(fun s ->
        match String.lowercase_ascii (String.trim s) with
        | "" -> None
        | "bytes" -> Some `Bytes
        | "none" -> Some `None
        | other -> Some (`Other other))

(* {2 Caching (RFC 9111)} *)

type cache_control = {
  max_age : int option;
  s_maxage : int option;
  no_cache : bool;
  no_store : bool;
  no_transform : bool;
  only_if_cached : bool;
  must_revalidate : bool;
  proxy_revalidate : bool;
  public : bool;
  private_ : bool;
  immutable : bool;
  min_fresh : int option;
  max_stale : int option;
  stale_while_revalidate : int option;
  extension : (string * string option) list;
}

let cache_directives ?max_age ?s_maxage ?(no_cache = false) ?(no_store = false)
    ?(no_transform = false) ?(only_if_cached = false) ?(must_revalidate = false)
    ?(proxy_revalidate = false) ?(public = false) ?(private_ = false)
    ?(immutable = false) ?min_fresh ?max_stale ?stale_while_revalidate
    ?(extension = []) () =
  { max_age; s_maxage; no_cache; no_store; no_transform; only_if_cached;
    must_revalidate; proxy_revalidate; public; private_; immutable;
    min_fresh; max_stale; stale_while_revalidate; extension }

let cache_control =
  v ~list_valued:true "Cache-Control"
    ~encode:(fun cc ->
        (* A directive the extension list already carries in its
           qualified form ([no-cache="Set-Cookie"]) must not also be
           emitted bare. *)
        let qualified name = List.mem_assoc name cc.extension in
        let flag name b = if b && not (qualified name) then Some name else None in
        let num name = Option.map (Fmt.str "%s=%d" name) in
        let dirs =
          List.filter_map Fun.id
            [ num "max-age" cc.max_age;
              num "s-maxage" cc.s_maxage;
              flag "no-cache" cc.no_cache;
              flag "no-store" cc.no_store;
              flag "no-transform" cc.no_transform;
              flag "only-if-cached" cc.only_if_cached;
              flag "must-revalidate" cc.must_revalidate;
              flag "proxy-revalidate" cc.proxy_revalidate;
              flag "public" cc.public;
              flag "private" cc.private_;
              flag "immutable" cc.immutable;
              num "min-fresh" cc.min_fresh;
              num "max-stale" cc.max_stale;
              num "stale-while-revalidate" cc.stale_while_revalidate ]
          @ List.map
              (fun (k, v) ->
                 match v with
                 | None -> k
                 | Some v ->
                   (* A directive argument that is not a bare token — a
                      field list like [no-cache="Set-Cookie, Age"] — must
                      go back out quoted or the commas would read as
                      directive separators. *)
                   if is_token_value v then k ^ "=" ^ v
                   else k ^ "=" ^ quote_string v)
              cc.extension
        in
        String.concat ", " dirs)
    ~decode:(fun s ->
        match split_commas s with
        | [] -> None
        | dirs ->
          let cc = ref (cache_directives ()) in
          let ext = ref [] in
          List.iter
            (fun d ->
               let key, value =
                 match cut '=' d with
                 | None -> (String.lowercase_ascii (String.trim d), None)
                 | Some (k, v) ->
                   (String.lowercase_ascii (String.trim k),
                    Some (unquote (String.trim v)))
               in
               let num = Option.bind value dec_int in
               (* A numeric directive whose value fails the digit
                  grammar falls through to the extension list rather
                  than silently reading as absent. *)
               match key, value, num with
               | "max-age", Some _, Some _ -> cc := { !cc with max_age = num }
               | "s-maxage", Some _, Some _ -> cc := { !cc with s_maxage = num }
               | "no-cache", None, _ -> cc := { !cc with no_cache = true }
               | "no-cache", Some _, _ ->
                 (* The qualified forms — [no-cache="Set-Cookie"],
                    [private="field"] — name specific fields, which is
                    weaker than the bare directive. The flag is set (the
                    safe reading for a consumer that only looks there)
                    and the field list kept verbatim in [extension], so
                    the value still round-trips; the encoder drops the
                    bare directive when [extension] carries it. *)
                 cc := { !cc with no_cache = true };
                 ext := (key, value) :: !ext
               | "no-store", _, _ -> cc := { !cc with no_store = true }
               | "no-transform", _, _ -> cc := { !cc with no_transform = true }
               | "only-if-cached", _, _ -> cc := { !cc with only_if_cached = true }
               | "must-revalidate", _, _ -> cc := { !cc with must_revalidate = true }
               | "proxy-revalidate", _, _ -> cc := { !cc with proxy_revalidate = true }
               | "public", _, _ -> cc := { !cc with public = true }
               | "private", None, _ -> cc := { !cc with private_ = true }
               | "private", Some _, _ ->
                 cc := { !cc with private_ = true };
                 ext := (key, value) :: !ext
               | "immutable", _, _ -> cc := { !cc with immutable = true }
               | "min-fresh", Some _, Some _ -> cc := { !cc with min_fresh = num }
               | "max-stale", Some _, Some _ -> cc := { !cc with max_stale = num }
               | "stale-while-revalidate", Some _, Some _ ->
                 cc := { !cc with stale_while_revalidate = num }
               | key, value, _ ->
                 (* Bare [max-stale], [private=fields], unknown and
                    malformed directives land here, preserved
                    verbatim. *)
                 ext := (key, value) :: !ext)
            dirs;
          Some { !cc with extension = List.rev !ext })

let age = v "Age" ~encode:string_of_int ~decode:dec_int

let expires = text "Expires"

type vary = [ `Any | `Fields of string list ]

let vary =
  v ~list_valued:true "Vary"
    ~encode:(function `Any -> "*" | `Fields fs -> String.concat ", " fs)
    ~decode:(fun s ->
        match split_commas s with
        | [] -> None
        | fs when List.mem "*" fs -> Some `Any
        | fs -> Some (`Fields (List.map String.lowercase_ascii fs)))

(* {2 Cache-Status (RFC 9211)} *)

type forward =
  [ `Uri_miss | `Vary_miss | `Miss | `Request | `Stale | `Partial | `Bypass
  | `Other of string ]

type cache_status = {
  cache : string;
  hit : bool;
  fwd : forward option;
  fwd_status : int option;
  stored : bool;
  collapsed : bool;
  ttl : int option;
  key : string option;
  detail : string option;
}

let forward_of_string = function
  | "uri-miss" -> `Uri_miss
  | "vary-miss" -> `Vary_miss
  | "miss" -> `Miss
  | "request" -> `Request
  | "stale" -> `Stale
  | "partial" -> `Partial
  | "bypass" -> `Bypass
  | other -> `Other other

let forward_to_string = function
  | `Uri_miss -> "uri-miss"
  | `Vary_miss -> "vary-miss"
  | `Miss -> "miss"
  | `Request -> "request"
  | `Stale -> "stale"
  | `Partial -> "partial"
  | `Bypass -> "bypass"
  | `Other s -> s

let parse_cache_status_entry s =
  match split_semis s with
  | [] -> None
  | cache :: params ->
    if cache = "" then None
    else
      let params =
        List.filter_map
          (fun p ->
             (* A bare parameter is a structured-field true. *)
             match param_of p with
             | None -> Some (String.lowercase_ascii p, "?1")
             | some -> some)
          params
      in
      let flag key =
        match List.assoc_opt key params with
        | Some ("?1" | "true" | "1") -> true
        | _ -> false
      in
      (* Structured-field integers may be negative (a stale entry's
         [ttl]). *)
      let int key = Option.bind (List.assoc_opt key params) dec_int_signed in
      Some
        { cache;
          hit = flag "hit";
          fwd = Option.map forward_of_string (List.assoc_opt "fwd" params);
          fwd_status = int "fwd-status";
          stored = flag "stored";
          collapsed = flag "collapsed";
          ttl = int "ttl";
          key = List.assoc_opt "key" params;
          detail = List.assoc_opt "detail" params }

let cache_status_entry_to_string e =
  let params =
    List.filter_map Fun.id
      [ (if e.hit then Some "hit" else None);
        Option.map (fun f -> "fwd=" ^ forward_to_string f) e.fwd;
        Option.map (Fmt.str "fwd-status=%d") e.fwd_status;
        (if e.stored then Some "stored" else None);
        (if e.collapsed then Some "collapsed" else None);
        Option.map (Fmt.str "ttl=%d") e.ttl;
        Option.map (fun v -> "key=" ^ quote_string v) e.key;
        Option.map (fun v -> "detail=" ^ quote_string v) e.detail ]
  in
  String.concat "; " (e.cache :: params)

let cache_status =
  v ~list_valued:true "Cache-Status"
    ~encode:(fun entries ->
        String.concat ", " (List.map cache_status_entry_to_string entries))
    ~decode:(fun s ->
        match List.filter_map parse_cache_status_entry (split_commas s) with
        | [] -> None
        | entries -> Some entries)

let cache_hit entries = List.exists (fun e -> e.hit) entries

(* {2 Authentication (RFC 9110 s11)} *)

(* Minimal RFC 4648 base64, for Basic credentials — not worth a
   dependency. *)
let b64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let b64_encode s =
  let n = String.length s in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let get i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then begin
      let b0 = get i and b1 = get (i + 1) and b2 = get (i + 2) in
      Buffer.add_char buf b64_alphabet.[b0 lsr 2];
      Buffer.add_char buf b64_alphabet.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
      Buffer.add_char buf
        (if i + 1 < n then b64_alphabet.[((b1 land 15) lsl 2) lor (b2 lsr 6)]
         else '=');
      Buffer.add_char buf (if i + 2 < n then b64_alphabet.[b2 land 63] else '=');
      go (i + 3)
    end
  in
  go 0;
  Buffer.contents buf

let b64_decode s =
  let idx = function
    | 'A' .. 'Z' as c -> Some (Char.code c - 65)
    | 'a' .. 'z' as c -> Some (Char.code c - 97 + 26)
    | '0' .. '9' as c -> Some (Char.code c - 48 + 52)
    | '+' -> Some 62
    | '/' -> Some 63
    | _ -> None
  in
  let buf = Buffer.create (String.length s * 3 / 4) in
  let acc = ref 0 and bits = ref 0 and ok = ref true in
  String.iter
    (fun c ->
       if c <> '=' then
         match idx c with
         | None -> ok := false
         | Some v ->
           acc := (!acc lsl 6) lor v;
           bits := !bits + 6;
           if !bits >= 8 then begin
             bits := !bits - 8;
             Buffer.add_char buf (Char.chr ((!acc lsr !bits) land 0xff))
           end)
    s;
  if !ok then Some (Buffer.contents buf) else None

type credentials =
  [ `Basic of string * string | `Bearer of string | `Other of string * string ]

let authorization_codec name =
  let basic_of blob =
    let* up = b64_decode blob in
    let* user, pass = cut ':' up in
    Some (`Basic (user, pass))
  in
  v name
    ~encode:(function
        | `Basic (user, pass) -> "Basic " ^ b64_encode (user ^ ":" ^ pass)
        | `Bearer token -> "Bearer " ^ token
        | `Other (scheme, rest) -> scheme ^ " " ^ rest)
    ~decode:(fun s ->
        let s = String.trim s in
        match cut ' ' s with
        | None -> if s = "" then None else Some (`Other (s, ""))
        | Some (scheme, rest) ->
          let rest = String.trim rest in
          match String.lowercase_ascii scheme with
          | "bearer" -> Some (`Bearer rest)
          | "basic" ->
            (match basic_of rest with
             | Some _ as basic -> basic
             | None -> Some (`Other (scheme, rest)))
          | _ -> Some (`Other (scheme, rest)))

let authorization = authorization_codec "Authorization"
let proxy_authorization = authorization_codec "Proxy-Authorization"

type challenge = { scheme : string; params : (string * string) list }

let is_token_word s =
  s <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
         | _ -> false)
       s

let challenge_to_string c =
  match c.params with
  | [] -> c.scheme
  | ps ->
    (* A token68 blob is carried as the single unnamed parameter that
       {!parse_challenges} produced it as. *)
    match ps with
    | [ ("", blob) ] -> c.scheme ^ " " ^ blob
    | ps ->
      c.scheme ^ " "
      ^ String.concat ", "
          (List.map (fun (k, v) -> Fmt.str "%s=%s" k (quote_string v)) ps)

(* Challenges and their parameters share the comma separator, so a part
   opens a new challenge when it looks like [Scheme] or [Scheme key=v]
   rather than [key=v]. *)
let parse_challenges s =
  let challenges = ref [] in
  let current = ref None in
  let start scheme params = current := Some { scheme; params } in
  let close () =
    Option.iter (fun c -> challenges := c :: !challenges) !current;
    current := None
  in
  let attach kv =
    match !current, kv with
    | Some c, Some kv -> current := Some { c with params = c.params @ [ kv ] }
    | _ -> ()
  in
  (* [Scheme blob] where the blob is a token68 (base64-ish, no '=' other
     than trailing padding) is not a parameter list: splitting it at the
     padding would invent a garbage key. Keep it whole, under the empty
     key, which is the one name the [key=value] grammar cannot produce. *)
  let is_token68 s =
    s <> ""
    && (match String.index_opt s '=' with
        | None -> true
        | Some i ->
          (* Only trailing '=' padding is allowed. *)
          i > 0
          && String.for_all (fun c -> c = '=') (String.sub s i (String.length s - i)))
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
           | '-' | '.' | '_' | '~' | '+' | '/' | '=' -> true
           | _ -> false)
         s
  in
  let param_or_blob part =
    match param_of part with
    | Some _ as kv -> kv
    | None -> None
  in
  List.iter
    (fun part ->
       match cut ' ' part with
       | None when is_token_word part -> close (); start part []
       | None -> attach (param_or_blob part)
       | Some (word, rest) ->
         if is_token_word word then begin
           close ();
           let rest = String.trim rest in
           if param_of rest = None || is_token68 rest then
             start word (if rest = "" then [] else [ ("", rest) ])
           else start word (Option.to_list (param_of rest))
         end
         else attach (param_or_blob part))
    (split_commas s);
  close ();
  List.rev !challenges

let www_authenticate =
  v ~list_valued:true "WWW-Authenticate"
    ~encode:(fun cs -> String.concat ", " (List.map challenge_to_string cs))
    ~decode:(fun s ->
        match parse_challenges s with [] -> None | cs -> Some cs)

type authentication_info = {
  nextnonce : string option;
  qop : string option;
  rspauth : string option;
  cnonce : string option;
  nc : string option;
}

let authentication_info =
  v "Authentication-Info"
    ~encode:(fun i ->
        let quoted k v = k ^ "=" ^ quote_string v in
        let plain k v = Fmt.str "%s=%s" k v in
        List.filter_map Fun.id
          [ Option.map (quoted "nextnonce") i.nextnonce;
            Option.map (plain "qop") i.qop;
            Option.map (quoted "rspauth") i.rspauth;
            Option.map (quoted "cnonce") i.cnonce;
            Option.map (plain "nc") i.nc ]
        |> String.concat ", ")
    ~decode:(fun s ->
        match List.filter_map param_of (split_commas s) with
        | [] -> None
        | pairs ->
          Some
            { nextnonce = List.assoc_opt "nextnonce" pairs;
              qop = List.assoc_opt "qop" pairs;
              rspauth = List.assoc_opt "rspauth" pairs;
              cnonce = List.assoc_opt "cnonce" pairs;
              nc = List.assoc_opt "nc" pairs })

(* {2 Integrity digests (RFC 9530)} *)

type digest = {
  algorithm : [ `Sha256 | `Sha512 | `Other of string ];
  digest : string;  (* base64, as received *)
}

let digest_algorithm_of_string s =
  match String.lowercase_ascii s with
  | "sha-256" -> `Sha256
  | "sha-512" -> `Sha512
  | other -> `Other other

let digest_algorithm_to_string = function
  | `Sha256 -> "sha-256"
  | `Sha512 -> "sha-512"
  | `Other s -> s

let digest_codec name =
  v ~list_valued:true name
    ~encode:(fun digests ->
        String.concat ", "
          (List.map
             (fun d ->
                Fmt.str "%s=:%s:" (digest_algorithm_to_string d.algorithm)
                  d.digest)
             digests))
    ~decode:(fun s ->
        let parse part =
          let* algo, value = cut '=' part in
          let value = String.trim value in
          (* RFC 9530 wraps byte sequences as :base64:. *)
          let digest =
            if String.length value >= 2
               && value.[0] = ':'
               && value.[String.length value - 1] = ':'
            then String.sub value 1 (String.length value - 2)
            else value
          in
          Some { algorithm = digest_algorithm_of_string (String.trim algo); digest }
        in
        match List.filter_map parse (split_commas s) with
        | [] -> None
        | ds -> Some ds)

let content_digest = digest_codec "Content-Digest"
let repr_digest = digest_codec "Repr-Digest"

let strongest_digest digests =
  let by a = List.find_opt (fun d -> d.algorithm = a) digests in
  match by `Sha512 with
  | Some _ as d -> d
  | None ->
    (match by `Sha256 with
     | Some _ as d -> d
     | None -> List.nth_opt digests 0)

(* {2 Strict-Transport-Security (RFC 6797)} *)

type hsts = {
  max_age : int64;
  include_subdomains : bool;
  preload : bool;
}

let strict_transport_security =
  v "Strict-Transport-Security"
    ~encode:(fun h ->
        String.concat "; "
          (Fmt.str "max-age=%Ld" h.max_age
           :: ((if h.include_subdomains then [ "includeSubDomains" ] else [])
               @ (if h.preload then [ "preload" ] else []))))
    ~decode:(fun s ->
        let max_age = ref None in
        let include_subdomains = ref false in
        let preload = ref false in
        split_semis s
        |> List.iter (fun d ->
            match param_of d with
            | Some ("max-age", v) -> max_age := dec_int64 v
            | Some _ -> ()
            | None ->
              match String.lowercase_ascii d with
              | "includesubdomains" -> include_subdomains := true
              | "preload" -> preload := true
              | _ -> ());
        Option.map
          (fun age ->
             { max_age = age;
               include_subdomains = !include_subdomains;
               preload = !preload })
          !max_age)

(* {2 Link (RFC 8288)} *)

type link = {
  target : string;
  rel : string option;
  media_type : string option;
  title : string option;
  hreflang : string option;
  params : (string * string) list;
}

let link ?rel ?media_type ?title ?hreflang ?(params = []) target =
  { target; rel; media_type; title; hreflang; params }

let link_rel r links = List.find_opt (fun l -> l.rel = Some r) links

let link_to_string l =
  let param k = Option.map (fun v -> k ^ "=" ^ quote_string v) in
  String.concat "; "
    (Fmt.str "<%s>" l.target
     :: List.filter_map Fun.id
          [ param "rel" l.rel; param "title" l.title;
            param "type" l.media_type; param "hreflang" l.hreflang ]
     @ List.map (fun (k, v) -> k ^ "=" ^ quote_string v) l.params)

let parse_link_value str =
  let str = String.trim str in
  if String.length str = 0 || str.[0] <> '<' then None
  else
    let* target, rest = cut '>' (String.sub str 1 (String.length str - 1)) in
    let params = split_semis rest |> List.filter_map param_of in
    let take k = List.assoc_opt k params in
    let rest =
      List.filter
        (fun (k, _) -> not (List.mem k [ "rel"; "title"; "type"; "hreflang" ]))
        params
    in
    Some
      { target; rel = take "rel"; title = take "title";
        media_type = take "type"; hreflang = take "hreflang"; params = rest }

let links =
  v ~list_valued:true "Link"
    ~encode:(fun ls -> String.concat ", " (List.map link_to_string ls))
    ~decode:(fun s ->
        match List.filter_map parse_link_value (split_commas s) with
        | [] -> None
        | ls -> Some ls)

(* {2 Other headers} *)

let allow =
  v ~list_valued:true "Allow"
    ~encode:(fun methods ->
        String.concat ", " (List.map Http.Method.to_string methods))
    ~decode:(fun s ->
        match split_commas s with
        | [] -> None
        | ms -> Some (List.map Http.Method.of_string ms))

type retry_after = [ `Seconds of int | `Date of string ]

let retry_after =
  v "Retry-After"
    ~encode:(function `Seconds s -> string_of_int s | `Date d -> d)
    ~decode:(fun s ->
        let s = String.trim s in
        match dec_int s with
        | Some seconds -> Some (`Seconds seconds)
        | None -> if s = "" then None else Some (`Date s))

let location = text "Location"
let user_agent = text "User-Agent"

(* {2 The typed request-header list} *)

type headers =
  | [] : headers
  | ( :: ) : ('a t * 'a) * headers -> headers

let raw name value = (text name, value)

(* In the bodies below, [[]] and [(::)] appear at both types; the
   expected type picks the right constructor each time. *)
let rec to_list : headers -> (string * string) list = function
  | [] -> []
  | (h, x) :: rest -> (h.name, h.enc x) :: to_list rest

let to_http hs = Http.Header.of_list (to_list hs)

let of_http hs =
  Stdlib.List.fold_right
    (fun (n, x) acc -> (text n, x) :: acc)
    (Http.Header.to_list hs) []

let rec append (a : headers) (b : headers) : headers =
  match a with [] -> b | x :: rest -> x :: append rest b
