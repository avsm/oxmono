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

(* Singleton fields reject duplicates. HSTS explicitly specifies first-field
   processing (RFC 6797 section 8.1). Lists combine occurrences in wire order. *)
let get t hs =
  match Http.Header.get_multi hs t.name with
  | [] -> None
  | [value] -> t.dec value
  | values ->
      if t.list_valued then t.dec (String.concat ", " values)
      else if String.lowercase_ascii t.name = "strict-transport-security" then
        t.dec (List.hd values)
      else None

let pair t x = (t.name, t.enc x)

let text n = v n ~encode:Fun.id ~decode:Option.some

let ( let* ) = Option.bind

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

(* Split a field value on [sep], respecting quoted strings. With [angles],
   a [<...>] group is opaque too: a Link target is a URI reference, which
   may carry the separator ([</a?x=1,2>]) without quoting. *)
let split_on ?(angles = false) sep s =
  let buf = Buffer.create (String.length s) in
  let acc = ref [] in
  let in_quotes = ref false in
  let in_angles = ref false in
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
       else if c = '"' && not !in_angles then
         (in_quotes := not !in_quotes; Buffer.add_char buf c)
       else if angles && not !in_quotes && c = '<' && not !in_angles then
         (in_angles := true; Buffer.add_char buf c)
       else if !in_angles && c = '>' then
         (in_angles := false; Buffer.add_char buf c)
       else if c = sep && not !in_quotes && not !in_angles then flush ()
       else Buffer.add_char buf c)
    s;
  flush ();
  List.rev !acc

let split_commas = split_on ','
let split_semis = split_on ';'

(* Check the list syntax before [split_on] discards empty members. This also
   rejects unterminated quoted strings instead of treating their remainder as
   an ordinary value. *)
let split_checked ?(empty = false) ?(angles = false) sep s =
  let in_quotes = ref false in
  let in_angles = ref false in
  let escaped = ref false in
  let member = ref false in
  let separated = ref false in
  let valid = ref true in
  String.iter
    (fun c ->
       if !escaped then (escaped := false; member := true)
       else if !in_quotes && c = '\\' then (escaped := true; member := true)
       else if c = '"' && not !in_angles then
         (in_quotes := not !in_quotes; member := true)
       else if angles && c = '<' && not !in_quotes && not !in_angles then
         (in_angles := true; member := true)
       else if angles && c = '>' && !in_angles then
         (in_angles := false; member := true)
       else if c = sep && not !in_quotes && not !in_angles then begin
         if not !member then valid := false;
         member := false;
         separated := true
       end
       else if c <> ' ' && c <> '\t' then member := true)
    s;
  if !in_quotes || !in_angles || !escaped || not !valid then None
  else if !member then Some (split_on ~angles sep s)
  else if empty && not !separated then Some []
  else None

let rec map_all f = function
  | [] -> Some []
  | x :: xs ->
    let* y = f x in
    let* ys = map_all f xs in
    Some (y :: ys)

let unique_keys pairs =
  let rec loop seen = function
    | [] -> true
    | (key, _) :: rest ->
      if List.mem key seen then false else loop (key :: seen) rest
  in
  loop [] pairs

(* The RFC integer grammars are unsigned decimal digits. *)
let is_digits s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

let dec_int s =
  let s = String.trim s in
  if is_digits s then int_of_string_opt s else None

let dec_int64 s =
  let s = String.trim s in
  if is_digits s then Int64.of_string_opt s else None

(* RFC 9651 s3.3.3 restricts an sf-string to visible ASCII and space; an
   already-unquoted value carries none of the grammar's escapes, so every
   byte must fall in this range. *)
let is_sf_string s =
  String.for_all (fun c -> Char.code c >= 0x20 && Char.code c <= 0x7e) s

(* The permissive [unquote] above is retained for legacy directive grammars;
   strict RFC 9110 quoted strings use Httpz's shared field syntax. *)
let quote_string = Httpz.Header.Syntax.quote_string
let quoted_string = Httpz.Header.Syntax.unquote_string

(* [k=v] or [k="v"] as a lowercased key and unquoted value. *)
let param_of p =
  Option.map
    (fun (k, v) ->
       (String.lowercase_ascii (String.trim k), unquote (String.trim v)))
    (cut '=' p)

let strict_param_of p =
  let* key, value = cut '=' p in
  let key = String.lowercase_ascii (String.trim key) in
  let value = String.trim value in
  if not (Middleware.is_token key) then None
  else
    let* value =
      if Middleware.is_token value then Some value else quoted_string value
    in
    Some (key, value)

type media_type = { media : string; params : (string * string) list }

let media ?(params = []) media = { media; params }

let media_name ?(wildcards = false) value =
  let value = String.trim value in
  let valid =
    if wildcards then Httpz.Media.Syntax.valid_range
    else Httpz.Media.Syntax.valid_type
  in
  if valid value ~pos:0 ~len:(String.length value)
  then Some (String.lowercase_ascii value)
  else None

let parse_media ?(wildcards = false) s =
  match split_checked ';' s with
  | Some (name :: raw_params) ->
    let* media = media_name ~wildcards name in
    let* params = map_all strict_param_of raw_params in
    if unique_keys params then Some { media; params } else None
  | Some [] | None -> None

let media_to_string mt =
  let param (key, value) =
    let value =
      if Middleware.is_token value then value else quote_string value
    in
    key ^ "=" ^ value
  in
  String.concat "; " (mt.media :: List.map param mt.params)

let content_type =
  v "Content-Type"
    ~encode:media_to_string
    ~decode:parse_media

let content_length = v "Content-Length" ~encode:Int64.to_string ~decode:dec_int64

let content_encoding =
  v ~list_valued:true "Content-Encoding"
    ~encode:(String.concat ", ")
    ~decode:(fun s ->
        let* cs = split_checked ',' s in
        if List.for_all Middleware.is_token cs
        then Some (List.map String.lowercase_ascii cs)
        else None)

(* The RFC 5646 s2.1 language-tag shape, [1*8ALPHA *("-" 1*8alphanum)],
   which is all of the grammar a consumer can check without the registry. *)
let valid_language_tag s =
  match String.split_on_char '-' s with
  | [] -> false
  | first :: rest ->
    let alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
    let alnum = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
      | _ -> false
    in
    let component chars part =
      let n = String.length part in
      n >= 1 && n <= 8 && String.for_all chars part
    in
    component alpha first && List.for_all (component alnum) rest

let content_language =
  v ~list_valued:true "Content-Language"
    ~encode:(String.concat ", ")
    ~decode:(fun s ->
        let* ls = split_checked ',' s in
        (* Tags keep the case they were sent in; the grammar is
           case-insensitive. *)
        if ls <> [] && List.for_all valid_language_tag ls then Some ls else None)

type pref = { value : string; q : float option }

let pref ?q value = { value; q }

(* The RFC 9110 s12.4.2 qvalue grammar: 0 to 1, at most three decimals.
   [float_of_string_opt] alone would also accept exponents, hex floats
   and out-of-range values. *)
let parse_q s =
  let thousandths = Httpz.Header.Syntax.qvalue_sub s ~pos:0 ~len:(String.length s) in
  if thousandths < 0 then None else Some (float_of_int thousandths /. 1000.)

let qvalue_to_string q =
  (* NaN would propagate through the clamp and print as "nan". *)
  let q = if Float.is_nan q then 0. else Float.max 0. (Float.min 1. q) in
  let s = Fmt.str "%.3f" q in
  let i = ref (String.length s - 1) in
  while !i > 0 && s.[!i] = '0' do decr i done;
  if s.[!i] = '.' then decr i;
  String.sub s 0 (!i + 1)

(* The [q] parameter separates preferences from their weights; any
   other parameters stay attached to [value] verbatim, so
   [text/html;level=1;q=0.7] round-trips. *)
let parse_pref ~allow_params ~valid_value s =
  match split_checked ';' s with
  | None | Some [] -> None
  | Some (value :: params) ->
    let q = ref None in
    let kept = ref [] in
    let valid = ref true in
    List.iter
      (fun raw ->
         match strict_param_of raw with
         | Some ("q", weight) ->
           if Option.is_some !q then valid := false
           else
             (match parse_q weight with
              | Some weight -> q := Some weight
              | None -> valid := false)
         | Some _ when allow_params -> kept := raw :: !kept
         | Some _ | None -> valid := false)
      params;
    let value = String.concat ";" (value :: List.rev !kept) in
    if !valid && valid_value value then Some { value; q = !q } else None

let pref_codec ?(allow_empty = false) ~allow_params ~valid_value name =
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
        let* parts = split_checked ~empty:allow_empty ',' s in
        map_all (parse_pref ~allow_params ~valid_value) parts)

let valid_media_range s = Option.is_some (parse_media ~wildcards:true s)
let valid_coding s = s = "*" || Middleware.is_token s

let valid_language_range s = s = "*" || valid_language_tag s

let accept =
  pref_codec ~allow_params:true ~valid_value:valid_media_range "Accept"

let accept_encoding =
  pref_codec ~allow_empty:true ~allow_params:false ~valid_value:valid_coding
    "Accept-Encoding"

let accept_language =
  pref_codec ~allow_params:false ~valid_value:valid_language_range
    "Accept-Language"

type etag = { weak : bool; tag : string }

let etag_to_string e =
  if not (String.for_all (fun c -> Httpz.Etag.valid_tag_char (Stdlib_stable.Char_u.of_char c)) e.tag) then
    invalid_arg "Header.etag: invalid opaque entity-tag byte";
  Fmt.str "%s\"%s\"" (if e.weak then "W/" else "") e.tag

let parse_etag s =
  let s = String.trim s in
  let buf = Bytes.of_string s in
  let span =
    Httpz.Span.make ~off:(Httpz.Buf_read.i16 0)
      ~len:(Httpz.Buf_read.i16 (Bytes.length buf))
  in
  let #(status, parsed) = Httpz.Etag.parse buf span in
  match status with
  | Httpz.Etag.Invalid -> None
  | Httpz.Etag.Valid ->
    Some { weak = parsed.#weak; tag = Httpz.Etag.to_string buf parsed }

let etag = v "ETag" ~encode:etag_to_string ~decode:parse_etag

type etags = [ `Any | `Etags of etag list ]

let etags_codec name =
  v ~list_valued:true name
    ~encode:(function
        | `Any -> "*"
        | `Etags [] -> invalid_arg "Header: entity-tag list is empty"
        | `Etags es -> String.concat ", " (List.map etag_to_string es))
    ~decode:(fun s ->
        if String.trim s = "*" then Some `Any
        else
          let* parts = split_checked ',' s in
          let* es = map_all parse_etag parts in
          match es with [] -> None | _ -> Some (`Etags es))

let if_match = etags_codec "If-Match"
let if_none_match = etags_codec "If-None-Match"

let canonical_http_date s =
  let s = String.trim s in
  let buf = Bytes.of_string s in
  let span =
    Httpz.Span.make ~off:(Httpz.Buf_read.i16 0)
      ~len:(Httpz.Buf_read.i16 (Bytes.length buf))
  in
  let #(status, timestamp) = Httpz.Date.parse (buf) span in
  match status with
  | Httpz.Date.Valid -> Some (Httpz.Date.format timestamp)
  | Httpz.Date.Invalid -> None

let http_date name = v name ~encode:Fun.id ~decode:canonical_http_date

let last_modified = http_date "Last-Modified"
let if_modified_since = http_date "If-Modified-Since"
let if_unmodified_since = http_date "If-Unmodified-Since"
let date = http_date "Date"

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
    | last ->
      let* last = dec_int64 last in
      if Int64.compare first last <= 0
      then Some (`Range (first, Some last))
      else None

let range =
  v "Range"
    ~encode:(fun r ->
        Fmt.str "%s=%s" r.unit
          (String.concat "," (List.map range_spec_to_string r.ranges)))
    ~decode:(fun s ->
        let* unit, rest = cut '=' s in
        let unit = String.trim unit in
        let* specs = split_checked ',' rest in
        let* parsed = map_all parse_range_spec specs in
        if Middleware.is_token unit && parsed <> []
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
          let* e = parse_etag s in
          if e.weak then None else Some (`Etag e)
        else Option.map (fun date -> `Date date) (canonical_http_date s))

type content_range = {
  unit : string;
  range : (int64 * int64) option;
  complete_length : int64 option;
}

let valid_content_range cr =
  Middleware.is_token cr.unit
  && Httpz.Range.Content.valid_bounds ~range:cr.range ~complete_length:cr.complete_length

let complete_range ~first ~last ~complete_length =
  let cr = { unit = "bytes"; range = Some (first, last);
             complete_length = Some complete_length } in
  if not (valid_content_range cr) then invalid_arg "Header.complete_range: invalid range";
  cr

let content_range =
  v "Content-Range"
    ~encode:(fun cr ->
        if not (valid_content_range cr) then invalid_arg "Header.content_range: invalid range";
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
        let* () = match Httpz.Range.Content.kind ~unit s with
          | Invalid -> None | Satisfied | Unsatisfied -> Some () in
        let* range_part, length_part = cut '/' rest in
        let* range =
          (* "*" is legitimate (416); a dashed range must parse. *)
          if range_part = "*" then Some None
          else
            let* first, last = cut '-' range_part in
            let* first = dec_int64 first in
            let* last = dec_int64 last in
            if Int64.compare first last <= 0
            then Some (Some (first, last))
            else None
        in
        let* complete_length =
          (* "*" (unknown) is [None]; anything else must be a number —
             a malformed length fails the decode rather than reading as
             "unknown". *)
          if length_part = "*" then Some None
          else Option.map Option.some (dec_int64 length_part)
        in
        let cr = { unit; range; complete_length } in
        if valid_content_range cr then Some cr else None)

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
                   if Middleware.is_token v then k ^ "=" ^ v
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

let max_age = 2_147_483_648L

let age =
  v "Age" ~encode:Int64.to_string
    ~decode:(fun s ->
        let s = String.trim s in
        if not (is_digits s) then None
        else
          match Int64.of_string_opt s with
          | Some value -> Some (Int64.min value max_age)
          | None -> Some max_age)

let expires = http_date "Expires"

type vary = [ `Any | `Fields of string list ]

let vary =
  v ~list_valued:true "Vary"
    ~encode:(function `Any -> "*" | `Fields fs -> String.concat ", " fs)
    ~decode:(fun s ->
        let* fs = split_checked ',' s in
        (* RFC 9111 s4.1: a [*] member means the response is unreusable, so
           it outranks whatever else the list names. *)
        if List.mem "*" fs then Some `Any
        else if fs <> [] && List.for_all Middleware.is_token fs then
          Some (`Fields (List.map String.lowercase_ascii fs))
        else None)

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

let valid_base64 s =
  let n = String.length s in
  let rec data_end i =
    if i = n then i
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' -> data_end (i + 1)
      | _ -> i
  in
  let rec only_padding i = i = n || (s.[i] = '=' && only_padding (i + 1)) in
  let first_padding = data_end 0 in
  let padding = n - first_padding in
  padding <= 2
  && only_padding first_padding
  && first_padding mod 4 <> 1
  && (padding = 0
      || n mod 4 = 0
         && (padding = 1 && first_padding mod 4 = 3
             || padding = 2 && first_padding mod 4 = 2))


(* RFC 8941 item lists. This syntax layer keeps bare-item types until a field
   applies its own constraints; commas and semicolons inside strings are data.
   Inner lists are deliberately excluded because Cache-Status forbids them. *)
module Structured = struct
  type bare = Token of string | String of string | Integer of int
            | Decimal | Bytes | Boolean of bool

  let alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let digit = function '0' .. '9' -> true | _ -> false
  let token_first c = alpha c || c = '*'
  let token_char c = Httpz.Buf_read.is_token_char (Stdlib_stable.Char_u.of_char c) || c = ':' || c = '/'
  let token s = s <> "" && token_first s.[0] && String.for_all token_char s
  let key_first = function 'a' .. 'z' | '*' -> true | _ -> false
  let key_char c = key_first c || digit c || c = '_' || c = '-' || c = '.'

  let list s =
    let pos = ref 0 and len = String.length s in
    let peek () = if !pos < len then s.[!pos] else '\000' in
    let take () = let c = peek () in if !pos >= len then raise Exit; incr pos; c in
    let skip p = while !pos < len && p s.[!pos] do incr pos done in
    let ows () = skip (function ' ' | '\t' -> true | _ -> false) in
    let scan p = let start = !pos in skip p; String.sub s start (!pos - start) in
    let quoted () =
      ignore (take ());
      let out = Buffer.create 32 in
      let rec loop () =
        match take () with
        | '"' -> String (Buffer.contents out)
        | '\\' ->
            let c = take () in
            if c <> '"' && c <> '\\' then raise Exit;
            Buffer.add_char out c; loop ()
        | c when c >= '\x20' && c <= '\x7e' -> Buffer.add_char out c; loop ()
        | _ -> raise Exit
      in
      loop ()
    in
    let bare () =
      match peek () with
      | '"' -> quoted ()
      | '?' ->
          incr pos;
          (match take () with '0' -> Boolean false | '1' -> Boolean true | _ -> raise Exit)
      | ':' ->
          incr pos;
          let data = scan (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' | '=' -> true | _ -> false) in
          if take () <> ':' || not (valid_base64 data) then raise Exit;
          (match Base64.decode ~pad:false data with Ok _ -> Bytes | Error _ -> raise Exit)
      | '-' | '0' .. '9' ->
          let start = !pos in
          if peek () = '-' then incr pos;
          let digits = scan digit in
          if digits = "" || String.length digits > 15 then raise Exit;
          if peek () = '.' then begin
            incr pos;
            let fraction = scan digit in
            if String.length digits > 12 || fraction = "" || String.length fraction > 3 then raise Exit;
            Decimal
          end else
            (match int_of_string_opt (String.sub s start (!pos - start)) with
             | Some n -> Integer n | None -> raise Exit)
      | c when token_first c -> Token (scan token_char)
      | _ -> raise Exit
    in
    let rec parameters acc =
      if peek () <> ';' then List.rev acc
      else begin
        incr pos;
        skip ((=) ' ');
        if not (key_first (peek ())) then raise Exit;
        let key = scan key_char in
        let value = if peek () = '=' then (incr pos; bare ()) else Boolean true in
        (* Structured Fields uses the last value for a repeated parameter. *)
        parameters ((key, value) :: List.remove_assoc key acc)
      end
    in
    let rec items acc =
      let value = bare () in
      let params = parameters [] in
      ows ();
      let acc = (value, params) :: acc in
      if !pos = len then List.rev acc
      else if take () = ',' then (ows (); items acc)
      else raise Exit
    in
    try ows (); if !pos = len then Some [] else Some (items []) with Exit -> None
end

let cache_status_of_item (cache, params) =
  let open Structured in
  let* cache = match cache with Token s | String s -> Some s | _ -> None in
  let optional key parse = match List.assoc_opt key params with
    | None -> Some None
    | Some value -> Option.map Option.some (parse value)
  in
  let flag key = match List.assoc_opt key params with
    | None -> Some false | Some (Boolean b) -> Some b | _ -> None
  in
  let integer = function Integer n -> Some n | _ -> None in
  let* hit = flag "hit" in
  let* stored = flag "stored" in
  let* collapsed = flag "collapsed" in
  let* fwd = optional "fwd" (function Token s -> Some (forward_of_string s) | _ -> None) in
  let* fwd_status = optional "fwd-status" (function Integer n when n >= 100 && n <= 599 -> Some n | _ -> None) in
  let* ttl = optional "ttl" integer in
  let* key = optional "key" (function String s -> Some s | _ -> None) in
  let* detail = optional "detail" (function String s | Token s -> Some s | _ -> None) in
  Some { cache; hit; stored; collapsed; fwd; fwd_status; ttl; key; detail }

let cache_status_entry_to_string e =
  let sf_string s =
    if not (is_sf_string s) then invalid_arg "Header.cache_status: invalid structured string";
    quote_string s
  in
  let sf_integer n =
    if n < -999_999_999_999_999 || n > 999_999_999_999_999 then
      invalid_arg "Header.cache_status: structured integer out of range";
    string_of_int n
  in
  let sf_token s =
    if not (Structured.token s) then invalid_arg "Header.cache_status: invalid structured token";
    s
  in
  let cache = if Structured.token e.cache then e.cache else sf_string e.cache in
  let params =
    List.filter_map Fun.id
      [ (if e.hit then Some "hit" else None);
        Option.map (fun f -> "fwd=" ^ sf_token (forward_to_string f)) e.fwd;
        Option.map (fun n ->
          if n < 100 || n > 599 then invalid_arg "Header.cache_status: invalid status";
          "fwd-status=" ^ sf_integer n) e.fwd_status;
        (if e.stored then Some "stored" else None);
        (if e.collapsed then Some "collapsed" else None);
        Option.map (fun n -> "ttl=" ^ sf_integer n) e.ttl;
        Option.map (fun v -> "key=" ^ sf_string v) e.key;
        Option.map (fun v -> "detail=" ^ sf_string v) e.detail ]
  in
  String.concat "; " (cache :: params)

let cache_status =
  v ~list_valued:true "Cache-Status"
    ~encode:(fun entries ->
        if entries = [] then invalid_arg "Header.cache_status: empty list";
        String.concat ", " (List.map cache_status_entry_to_string entries))
    ~decode:(fun s ->
        let* items = Structured.list s in
        if items = [] then None else map_all cache_status_of_item items)

let cache_hit entries = List.exists (fun e -> e.hit) entries

(* Let the maintained RFC 4648 package own the codec. The local predicate is
   narrower HTTP policy: it rejects misplaced padding before asking the
   decoder to interpret the bytes. *)
let b64_encode = Base64.encode_string


let b64_decode s =
  match Base64.decode ~pad:false s with
  | Ok decoded -> Some decoded
  | Error _ -> None

type credentials =
  [ `Basic of string * string | `Bearer of string | `Other of string * string ]

(* RFC 6750 s2.1: [b64token = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+"
   / "/" ) *"="]. *)
let is_b64token s =
  let n = String.length s in
  let is_body = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '+' | '/'
      -> true
    | _ -> false
  in
  let rec body_end i = if i < n && is_body s.[i] then body_end (i + 1) else i in
  let rec padding i = i = n || Char.equal s.[i] '=' && padding (i + 1) in
  let b = body_end 0 in
  (* Scan the padding in place: unlike the stock implementation this does not
     allocate a suffix merely to ask whether it contains only equals signs. *)
  b > 0 && padding b

let is_basic_text = String.for_all (fun c -> c >= ' ' && c <= '\126')

let authorization_codec name =
  (* [b64_decode] ignores stray padding, so a non-canonical blob such as
     [dXNlcg==pw==] would otherwise decode to a different credential than
     the one on the wire. *)
  let basic_of blob =
    if not (valid_base64 blob) then None
    else
      let* up = b64_decode blob in
      let* user, pass = cut ':' up in
      Some (`Basic (user, pass))
  in
  v name
    ~encode:(function
        | `Basic (user, pass) ->
          (* RFC 7617 §2: the colon separates the pair, so a user-id
             carrying one would decode as a different pair than it names. *)
          if String.contains user ':' then
            invalid_arg
              (Printf.sprintf
                 "Header.%s: a Basic user-id cannot contain a colon"
                 (String.lowercase_ascii name));
          if not (is_basic_text user && is_basic_text pass) then
            invalid_arg
              (Printf.sprintf
                 "Header.%s: Basic credentials must contain only printable ASCII"
                 (String.lowercase_ascii name));
          "Basic " ^ b64_encode (user ^ ":" ^ pass)
        | `Bearer token ->
          (* RFC 6750 s2.1: the token is a b64token, which cannot carry a
             space; encoding one anyway would authenticate as a different,
             truncated credential than it names. *)
          if not (is_b64token token) then
            invalid_arg
              (Printf.sprintf "Header.%s: Bearer value is not a valid b64token"
                 (String.lowercase_ascii name));
          "Bearer " ^ token
        | `Other (scheme, rest) ->
            if not (Middleware.is_token scheme && Middleware.is_field_value rest) then
              invalid_arg "Header.authorization: invalid scheme or credentials";
            if rest = "" then scheme else scheme ^ " " ^ rest)
    ~decode:(fun s ->
        let s = String.trim s in
        match cut ' ' s with
        | None ->
          if Middleware.is_token s then Some (`Other (s, "")) else None
        | Some (scheme, rest) ->
          let rest = String.trim rest in
          if not (Middleware.is_token scheme) || rest = "" then None
          else
            match String.lowercase_ascii scheme with
            | "bearer" -> if is_b64token rest then Some (`Bearer rest) else None
            | "basic" -> basic_of rest
            | _ -> Some (`Other (scheme, rest)))

let authorization = authorization_codec "Authorization"
let proxy_authorization = authorization_codec "Proxy-Authorization"

type challenge = { scheme : string; params : (string * string) list }

let is_token_word = Middleware.is_token

let challenge_to_string c =
  if not (Middleware.is_token c.scheme) || not (unique_keys c.params) then
    invalid_arg "Header.www_authenticate: invalid scheme or duplicate parameter";
  List.iter (fun (key, _) -> if key <> "" && not (Middleware.is_token key) then
    invalid_arg "Header.www_authenticate: invalid parameter name") c.params;
  match c.params with
  | [] -> c.scheme
  | ps ->
    (* A token68 blob is carried as the single unnamed parameter that
       {!parse_challenges} produced it as. *)
    match ps with
    | [ ("", blob) ] ->
        if not (is_b64token blob) then
          invalid_arg "Header.www_authenticate: invalid token68 credentials";
        c.scheme ^ " " ^ blob
    | ps ->
      if List.exists (fun (key, _) -> key = "") ps then
        invalid_arg "Header.www_authenticate: unnamed parameter must be a single token68";
      c.scheme ^ " "
      ^ String.concat ", "
          (List.map (fun (k, v) -> Fmt.str "%s=%s" k (quote_string v)) ps)

(* Challenges and their parameters share the comma separator, so a part
   opens a new challenge when it looks like [Scheme] or [Scheme key=v]
   rather than [key=v]. *)
let parse_challenges s =
  let* parts = split_checked ',' s in
  let challenges = ref [] in
  let current = ref None in
  let valid = ref true in
  let start scheme params = current := Some { scheme; params } in
  let close () =
    Option.iter (fun c -> challenges := c :: !challenges) !current;
    current := None
  in
  (* A parameter that names no challenge — one before the first scheme, or
     one that is not [key=value] at all — cannot be dropped: the caller
     would read a challenge weaker than the one the server sent. *)
  let attach part =
    match !current, strict_param_of part with
    | Some c, Some kv -> current := Some { c with params = c.params @ [ kv ] }
    | _ -> valid := false
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
  List.iter
    (fun part ->
       match cut ' ' part with
       | None when is_token_word part -> close (); start part []
       | None -> attach part
       | Some (word, rest) ->
         if is_token_word word then begin
           close ();
           let rest = String.trim rest in
           if rest = "" then start word []
           else if is_token68 rest then start word [ ("", rest) ]
           else
             match strict_param_of rest with
             | Some kv -> start word [ kv ]
             | None -> start word []; valid := false
         end
         else attach part)
    parts;
  close ();
  if !valid then Some (List.rev !challenges) else None

let www_authenticate =
  v ~list_valued:true "WWW-Authenticate"
    ~encode:(fun cs -> String.concat ", " (List.map challenge_to_string cs))
    ~decode:(fun s ->
        let* cs = parse_challenges s in
        let unique c = unique_keys c.params in
        match cs with
        | [] -> None
        | _ when List.for_all unique cs -> Some cs
        | _ -> None)

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
        let plain k v =
          if not (Middleware.is_token v) then invalid_arg "Header.authentication_info: invalid token";
          Fmt.str "%s=%s" k v in
        List.filter_map Fun.id
          [ Option.map (quoted "nextnonce") i.nextnonce;
            Option.map (plain "qop") i.qop;
            Option.map (quoted "rspauth") i.rspauth;
            Option.map (quoted "cnonce") i.cnonce;
            Option.map (plain "nc") i.nc ]
        |> String.concat ", ")
    ~decode:(fun s ->
        let* parts = split_checked ',' s in
        let* pairs = map_all strict_param_of parts in
        if pairs = [] || not (unique_keys pairs) then None
        else Some
            { nextnonce = List.assoc_opt "nextnonce" pairs;
              qop = List.assoc_opt "qop" pairs;
              rspauth = List.assoc_opt "rspauth" pairs;
              cnonce = List.assoc_opt "cnonce" pairs;
              nc = List.assoc_opt "nc" pairs })

type digest = {
  algorithm : [ `Sha256 | `Sha512 | `Other of string ];
  digest : string;
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
          let algo = String.lowercase_ascii (String.trim algo) in
          let value = String.trim value in
          (* RFC 9530 wraps byte sequences as :base64:. *)
          if not (Middleware.is_token algo)
             || String.length value < 2
             || value.[0] <> ':'
             || value.[String.length value - 1] <> ':'
          then None
          else
            let digest = String.sub value 1 (String.length value - 2) in
            if valid_base64 digest
            then Some { algorithm = digest_algorithm_of_string algo; digest }
            else None
        in
        let* parts = split_checked ',' s in
        let* parsed = map_all parse parts in
        let replace acc digest =
          List.filter (fun d -> d.algorithm <> digest.algorithm) acc
          @ [ digest ]
        in
        match List.fold_left replace [] parsed with
        | [] -> None
        | ds -> Some ds)

let content_digest = digest_codec "Content-Digest"
let repr_digest = digest_codec "Repr-Digest"

let strongest_digest digests =
  let by a = List.find_opt (fun d -> d.algorithm = a) digests in
  match List.find_map by [ `Sha512; `Sha256 ] with
  | Some _ as d -> d
  | None -> List.nth_opt digests 0

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
        let valid = ref true in
        let seen = ref [] in
        let* directives = split_checked ';' s in
        (* RFC 6797 s6.1 rule 2: a directive name, valued or not, may occur
           at most once. Rule 4: [max-age] must carry a value. *)
        List.iter
          (fun d ->
             let parsed = param_of d in
             let name =
               match parsed with
               | Some (k, _) -> k
               | None -> String.lowercase_ascii d
             in
             if List.mem name !seen then valid := false
             else seen := name :: !seen;
             match parsed with
             | Some ("max-age", value) ->
               (match dec_int64 value with
                | Some value -> max_age := Some value
                | None -> valid := false)
             | Some _ -> ()
             | None ->
               (match name with
                | "includesubdomains" -> include_subdomains := true
                | "preload" -> preload := true
                | "max-age" -> valid := false
                | _ -> ()))
          directives;
        if not !valid then None
        else
          Option.map
            (fun age ->
               { max_age = age;
                 include_subdomains = !include_subdomains;
                 preload = !preload })
            !max_age)

type link = {
  target : string;
  rel : string option;
  media_type : string option;
  title : string option;
  hreflang : string option;
  params : (string * string) list;
}

let check_link_target target =
  if
    String.contains target '<'
    || String.contains target '>'
    || not
         (Httpz.Uriz.Scanner.is_valid
            (Httpz.Uriz.Scanner.parse target))
  then
    invalid_arg (Printf.sprintf "Header.link: target %S is not a URI reference" target)

let link ?rel ?media_type ?title ?hreflang ?(params = []) target =
  check_link_target target;
  { target; rel; media_type; title; hreflang; params }

let link_rel r links = List.find_opt (fun l -> l.rel = Some r) links

(* RFC 8288's link-param grammar is [token BWS "=" BWS ( token / quoted-string
   )]: every param, including an unrecognized one such as [anchor], may be
   written as a bare token whenever its value happens to be one. A real Link
   header favors the bare form; this is what a written-back header uses too.
*)
let token_or_quoted v = if Middleware.is_token v then v else quote_string v

let link_to_string l =
  check_link_target l.target;
  List.iter
    (fun (name, _) ->
      if not (Middleware.is_token name) then
        invalid_arg
          (Printf.sprintf "Header.link: parameter name %S is not a token" name))
    l.params;
  let param k = Option.map (fun v -> k ^ "=" ^ quote_string v) in
  let hreflang =
    Option.map (fun v -> "hreflang=" ^ token_or_quoted v) l.hreflang
  in
  String.concat "; "
    (Fmt.str "<%s>" l.target
     :: List.filter_map Fun.id
          [ param "rel" l.rel; param "title" l.title;
            param "type" l.media_type; hreflang ]
     @ List.map (fun (k, v) -> k ^ "=" ^ token_or_quoted v) l.params)

(* RFC 8187 ext-value characters. A percent sign is accepted here only as the
   beginning of a triplet. [percent_decode_into] validates the complete encoding. *)
let is_ext_value_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '!' | '#' | '$' | '&' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~'
  | '%' -> true
  | _ -> false

let decode_ext_value s =
  let* charset, rest = cut '\'' s in
  let* _language, encoded = cut '\'' rest in
  if not (String.equal (String.lowercase_ascii charset) "utf-8")
     || not (String.for_all is_ext_value_char encoded)
  then None
  else
    let dst = Bytes.create (String.length encoded) in
    let written =
      Httpz.Uriz.Scanner.percent_decode_into encoded ~pos:0 ~len:(String.length encoded)
        ~dst ~dst_pos:0 ~plus_as_space:false
    in
    if written < 0 then None
    else
      let decoded = Bytes.sub_string dst 0 written in
      if String.is_valid_utf_8 decoded then Some decoded else None

let parse_link_value str =
  let str = String.trim str in
  if String.length str = 0 || str.[0] <> '<' then None
  else
    let* target, rest = cut '>' (String.sub str 1 (String.length str - 1)) in
    let target_ok =
      not (String.contains target '<')
      && Httpz.Uriz.Scanner.is_valid (Httpz.Uriz.Scanner.parse target)
    in
    if not target_ok then None else
    let rest = String.trim rest in
    let* params =
      if rest = "" then Some []
      else if rest.[0] <> ';' then None
      else
        let values = String.sub rest 1 (String.length rest - 1) in
        let* values = split_checked ';' values in
        map_all strict_param_of values
    in
    let take k = List.assoc_opt k params in
    let title =
      match take "title*" with
      | None -> take "title"
      | Some value ->
        (match decode_ext_value value with
         | Some _ as title -> title
         | None -> take "title")
    in
    let rest =
      List.filter
        (fun (k, _) ->
           not (List.mem k [ "rel"; "title"; "title*"; "type"; "hreflang" ]))
        params
    in
    Some
      { target; rel = take "rel"; title;
        media_type = take "type"; hreflang = take "hreflang"; params = rest }

let links =
  v ~list_valued:true "Link"
    ~encode:(fun ls -> String.concat ", " (List.map link_to_string ls))
    ~decode:(fun s ->
        let* values = split_checked ~angles:true ',' s in
        let* values = map_all parse_link_value values in
        match values with [] -> None | values -> Some values)

let allow =
  v ~list_valued:true "Allow"
    ~encode:(fun methods ->
        String.concat ", " (List.map Http.Method.to_string methods))
    ~decode:(fun s ->
        let* ms = split_checked ~empty:true ',' s in
        if List.for_all Middleware.is_token ms
        then Some (List.map Http.Method.of_string ms)
        else None)

type retry_after = [ `Seconds of int | `Date of string ]

let retry_after =
  v "Retry-After"
    ~encode:(function `Seconds s -> string_of_int s | `Date d -> d)
    ~decode:(fun s ->
        let s = String.trim s in
        match dec_int s with
        | Some seconds -> Some (`Seconds seconds)
        | None -> Option.map (fun date -> `Date date) (canonical_http_date s))

let location =
  v "Location" ~encode:Fun.id
    ~decode:(fun value ->
        if Httpz.Uriz.Scanner.is_valid (Httpz.Uriz.Scanner.parse value)
        then Some value
        else None)
let user_agent = text "User-Agent"

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
