type scheme = [ `Http | `Https ]

module Uriz = Httpz.Uriz

type t = {
  scheme : scheme;
  host : string;
  port : int;
  uri : Uriz.t;
}

let scheme t = t.scheme
let host t = t.host
let port t = t.port

let default_port = function
  | `Http -> 80
  | `Https -> 443

let scheme_string = function
  | `Http -> "http"
  | `Https -> "https"

(* After percent-decoding, an ordinary registered name may contain only
   unreserved characters. Anything else, such as '/', '@', whitespace, or NUL,
   must not reach the origin compared by policy. *)
let valid_host_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

(* An IPv6 literal reaches us with its brackets already stripped, so it is the
   one host form allowed to contain ':'. Any other host containing a colon is
   rejected rather than becoming a name whose interpretation policy has not
   checked. *)
let has_colon host = String.contains host ':'

let ascii_host host =
  if String.exists (fun c -> Char.code c > 127) host then
    try Punycode_idna.to_ascii ~use_std3_rules:true host with
    | Punycode_idna.Error reason ->
      invalid_arg
        (Printf.sprintf
           "invalid internationalized host: %s"
           (Punycode_idna.error_reason_to_string reason))
  else host

let has_dot_dot host =
  let rec go index =
    index + 1 < String.length host
    && ((host.[index] = '.' && host.[index + 1] = '.') || go (index + 1))
  in
  go 0

(* A host is stored in the one spelling every policy layer compares: the
   A-label, lowercase, without the root dot, and with an IPv4 address in
   any of inet_aton(3)'s spellings folded to its dotted quad. Leaving
   "127.1" or "2130706433" as reg-names would let them stand for a host a
   blocklist named and did not recognize. *)
let check_host host =
  if host = "" then invalid_arg "empty host";
  let host = ascii_host host in
  if has_colon host
  then (
    if not (Httpz.Ip.is_ipv6_literal host)
    then invalid_arg (Printf.sprintf "host %S is not a valid IPv6 literal" host);
    if String.contains host '.'
    then invalid_arg (Printf.sprintf "host %S embeds an IPv4 address in IPv6" host);
    match Ipaddr.V6.of_string host with
    | Error _ -> invalid_arg "invalid IPv6 host"
    | Ok ip ->
        if Option.is_some (Ipaddr.to_v4 (Ipaddr.V6 ip)) then
          invalid_arg "IPv4-mapped IPv6 hosts are not supported";
        Ipaddr.V6.to_string ip)
  else (
    String.iter
      (fun c ->
        if not (valid_host_char c) then
          invalid_arg (Printf.sprintf "invalid character %C in host" c))
      host;
    if has_dot_dot host then
      invalid_arg (Printf.sprintf "host %S has an empty label" host);
    let length = String.length host in
    let rooted = length > 1 && host.[length - 1] = '.' in
    let host = if rooted then String.sub host 0 (length - 1) else host in
    if host = "" then invalid_arg "empty host";
    match Httpz.Ip.ipv4_canonical host with
    | Some _ when rooted ->
      (* An address has no root label, so a trailing dot on one is a
         spelling no resolver accepts, not a name to canonicalize. *)
      invalid_arg
        (Printf.sprintf "host %S is an IP address with a trailing dot" host)
    | Some dotted_quad -> dotted_quad
    | None -> String.lowercase_ascii host)

let check_port = function
  | Some p when p < 1 || p > 65535 ->
    invalid_arg (Printf.sprintf "port %d out of range" p)
  | p -> p

let check_authority scheme host port =
  let host = check_host host in
  let port = Option.value (check_port port) ~default:(default_port scheme) in
  (host, port)

let of_uri uri =
  match Uriz.scheme uri with
  | Null -> Error "not an absolute URL (missing scheme)"
  | This scheme ->
    match String.lowercase_ascii scheme with
    | "http" | "https" as s ->
      let scheme = if s = "http" then `Http else `Https in
      if Uriz.encoded_userinfo uri <> Null then
        Error "userinfo (user:password@) is not allowed in http URLs"
      else if Uriz.host_kind uri = This `Ipvfuture then
        Error "IPvFuture literals are not supported as HTTP connection hosts"
      else (
        match Uriz.decoded_host uri with
        | Null | This "" -> Error "URL has no host"
        | This host ->
          let explicit_port =
            match Uriz.port uri with Null -> None | This port -> Some port
          in
          let empty_port = Uriz.has_port uri && Option.is_none explicit_port in
          match check_authority scheme host explicit_port with
          | exception Invalid_argument msg -> Error msg
          | host, port ->
            let uri = Uriz.with_encoded_host uri (This host) in
            let uri =
              if empty_port || explicit_port = Some (default_port scheme)
              then Uriz.with_port uri Null
              else uri
            in
            let uri =
              if Uriz.encoded_path uri = ""
              then Uriz.with_encoded_path uri "/"
              else uri
            in
            let uri = Uriz.normalize uri in
            Ok { scheme; host; port; uri }
      )
    | s -> Error (Printf.sprintf "unsupported scheme %S (must be http or https)" s)

(* RFC 3986 requires raw non-ASCII authority bytes to be percent encoded.
   Encode only those UTF-8 bytes before parsing; [Uriz.decoded_host] decodes them again,
   after which [check_host] applies IDNA and stores the A-label. *)
let encode_non_ascii_authority s =
  let length = String.length s in
  let authority_start =
    if length >= 2 && String.starts_with ~prefix:"//" s then Some 2
    else
      match String.index_opt s ':' with
      | Some colon when colon + 2 < length && String.sub s colon 3 = "://" ->
        Some (colon + 3)
      | _ -> None
  in
  match authority_start with
  | None -> s
  | Some authority_start ->
    let rec authority_end index =
      if index >= length then length
      else
        match s.[index] with
        | '/' | '?' | '#' -> index
        | _ -> authority_end (index + 1)
    in
    let authority_end = authority_end authority_start in
    let rec has_non_ascii index =
      index < authority_end
      && (Char.code s.[index] > 127 || has_non_ascii (index + 1))
    in
    if not (has_non_ascii authority_start) then s
    else
      let buffer = Buffer.create (length + 16) in
      Buffer.add_substring buffer s 0 authority_start;
      for index = authority_start to authority_end - 1 do
        let code = Char.code s.[index] in
        if code <= 127 then Buffer.add_char buffer s.[index]
        else Buffer.add_string buffer (Printf.sprintf "%%%02X" code)
      done;
      Buffer.add_substring buffer s authority_end (length - authority_end);
      Buffer.contents buffer

let of_string s =
  match Uriz.of_string (encode_non_ascii_authority s) with
  | Null -> Error "not a valid URI reference"
  | This uri -> of_uri uri

let to_uri t =
  match Uriz.encoded_fragment t.uri with
  | Null -> t.uri
  | This _ -> Uriz.with_encoded_fragment t.uri Null

let same_origin a b =
  a.scheme = b.scheme && String.equal a.host b.host && a.port = b.port

(* An IPv6 literal is stored bracketless; re-add the brackets when
   printing so the port (or a following path) cannot be confused with
   the address. Only such a literal can contain ':' — [check_host]
   rejects it anywhere else. *)
let pp_host f host =
  if String.contains host ':' then Fmt.pf f "[%s]" host
  else Fmt.string f host

let origin t =
  if t.port = default_port t.scheme then
    Fmt.str "%s://%a" (scheme_string t.scheme) pp_host t.host
  else
    Fmt.str "%s://%a:%d" (scheme_string t.scheme) pp_host t.host t.port

let path_and_query t =
  let pq = Uriz.encoded_path_and_query t.uri in
  if pq = "" || pq.[0] <> '/' then "/" ^ pq else pq

let path_segments t =
  (* Normalize only once, in [of_uri], where the serialized path changes too.
     Remove the absolute-path marker, but preserve every empty wire segment. *)
  match Uriz.encoded_path t.uri with
  | "" | "/" -> []
  | raw ->
      let parts = String.split_on_char '/' raw in
      let parts = match parts with "" :: rest -> rest | parts -> parts in
      List.map (fun part -> match Uriz.percent_decode part with
        | This part -> part
        | Null -> assert false) parts
;;

let has_query t = match Uriz.encoded_query t.uri with Null -> false | This _ -> true

let has_fragment t =
  match Uriz.encoded_fragment t.uri with Null -> false | This _ -> true

(* Segment-wise, so a prefix covers whole path components only: without
   that, a scope for "/v3" would also admit "/v3x", a different
   endpoint that merely shares an opening. A decoded separator is unsafe for
   policy matching because an origin might split it after decoding. *)
let under ~prefix t =
  let path = Uriz.encoded_path t.uri in
  let scope = Uriz.encoded_path prefix.uri in
  let safe_segment s = not (String.contains s '/' || String.contains s '\\') in
  same_origin prefix t
  && (scope = "/"
      || (List.for_all safe_segment (path_segments prefix)
          && List.for_all safe_segment (path_segments t)
          && (String.equal scope path
              || String.starts_with
                   ~prefix:(if String.ends_with ~suffix:"/" scope then scope else scope ^ "/")
                   path)))
;;

let resolve ~base reference =
  match Uriz.of_string (encode_non_ascii_authority reference) with
  | Null -> Error "not a valid URI reference"
  | This rel ->
    let resolved = Uriz.resolve ~base:base.uri rel in
    let resolved =
      match Uriz.encoded_fragment rel with
      | This _ -> resolved
      | Null ->
        Uriz.with_encoded_fragment resolved (Uriz.encoded_fragment base.uri)
    in
    of_uri resolved

let set_query_params t params =
  let uri =
    List.fold_left
      (fun u (key, _) ->
        Uriz.remove_query_param ~plus_as_space:true u key)
      t.uri params
  in
  (* [add_query_param] appends, so folding in caller order preserves it. *)
  let uri =
    List.fold_left
      (fun u (key, value) -> Uriz.add_query_param u ~key ~value)
      uri params
  in
  (* Scheme, host and userinfo are untouched, so re-validation cannot
     fail; going through [of_uri] keeps the stored form canonical. *)
  match of_uri uri with Ok t -> t | Error _ -> assert false

let to_string t = Uriz.to_string (to_uri t)

let effective_string t = Uriz.to_string t.uri

let redacted_string t names =
  let uri = to_uri t in
  match Uriz.encoded_query uri with
  | Null -> Uriz.to_string uri
  | This query ->
    let base = Uriz.to_string (Uriz.with_encoded_query uri Null) in
    let out = Buffer.create (String.length base + String.length query + 1) in
    Buffer.add_string out base;
    Buffer.add_char out '?';
    let limit = String.length query in
    let mutable pos = 0 in
    let mutable more = true in
    while more do
      let amp =
        match String.index_from_opt query pos '&' with
        | None -> limit
        | Some amp -> amp
      in
      let eq =
        match String.index_from_opt query pos '=' with
        | Some eq when eq < amp -> eq
        | _ -> amp
      in
      if pos > 0 then Buffer.add_char out '&';
      Buffer.add_substring out query pos (eq - pos);
      if eq < amp then begin
        Buffer.add_char out '=';
        let encoded_key = String.sub query pos (eq - pos) in
        let sensitive =
          match Uriz.percent_decode ~plus_as_space:true encoded_key with
          | Null -> false
          | This key -> List.mem key names
        in
        if sensitive
        then Buffer.add_string out "<redacted>"
        else Buffer.add_substring out query (eq + 1) (amp - eq - 1)
      end;
      if amp < limit then pos <- amp + 1 else more <- false
    done;
    Buffer.contents out


let pp f t = Fmt.string f (to_string t)
let pp_redacted ~names f t = Fmt.string f (redacted_string t names)
