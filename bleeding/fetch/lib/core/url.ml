type scheme = [ `Http | `Https ]

type t = {
  scheme : scheme;
  host : string;  (* lowercase ASCII, an IP literal left bracketless *)
  port : int;  (* effective: the scheme's default is already applied *)
  uri : Uri.t;  (* canonical: http(s), host present, no userinfo, no fragment *)
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

(* Unreserved reg-name characters. Anything else, such as '/', '@', '%',
   whitespace or NUL, means uri recovered from something malformed and
   left it in the host, where it would go on to be compared against the
   origin of a policy scope. *)
let valid_host_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

(* An IPv6 literal reaches us with its brackets already stripped, so it
   is the one host form allowed to contain ':' — hex groups and the '::'
   elision, plus the '.' of an IPv4-mapped tail. Distinguishing it means
   any other host holding a colon, which uri yields only when it could
   not split the authority, is rejected rather than silently becoming a
   host no request can ever have. *)
let looks_like_ipv6 host =
  String.contains host ':'
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | ':' | '.' -> true
         | _ -> false)
       host

let check_host host =
  if host = "" then invalid_arg "empty host";
  if not (looks_like_ipv6 host) then
    String.iter (fun c ->
        if Char.code c > 127 then
          invalid_arg "non-ASCII host (IDN hosts must be given in punycode form)"
        else if c = ':' then
          invalid_arg
            (Printf.sprintf
               "host %S contains ':', which only an IPv6 literal may" host)
        else if not (valid_host_char c) then
          invalid_arg (Printf.sprintf "invalid character %C in host" c)
      ) host;
  String.lowercase_ascii host

let check_port = function
  | Some p when p < 1 || p > 65535 ->
    invalid_arg (Printf.sprintf "port %d out of range" p)
  | p -> p

(* Host before port, so a URL wrong in both ways names the host first. *)
let check_authority scheme host port =
  let host = check_host host in
  let port = Option.value (check_port port) ~default:(default_port scheme) in
  (host, port)

let of_uri uri =
  match Uri.scheme uri with
  | None -> Error "not an absolute URL (missing scheme)"
  | Some scheme ->
    match String.lowercase_ascii scheme with
    | "http" | "https" as s ->
      let scheme = if s = "http" then `Http else `Https in
      if Uri.userinfo uri <> None then
        Error "userinfo (user:password@) is not allowed in http URLs"
      else (
        match Uri.host uri with
        | None | Some "" -> Error "URL has no host"
        | Some host ->
          match check_authority scheme host (Uri.port uri) with
          | exception Invalid_argument msg -> Error msg
          | host, port ->
            let uri = Uri.canonicalize (Uri.with_fragment uri None) in
            Ok { scheme; host; port; uri }
      )
    | s -> Error (Printf.sprintf "unsupported scheme %S (must be http or https)" s)

(* [Uri.of_string] is total: parsing itself never fails, so validation
   happens entirely in [of_uri]. *)
let of_string s = of_uri (Uri.of_string s)

let to_uri t = t.uri

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
  let pq = Uri.path_and_query t.uri in
  if pq = "" || pq.[0] <> '/' then "/" ^ pq else pq

let path_segments t =
  (* [Uri.path] keeps percent-encoding, so we can decode per segment: an
     encoded "/" (%2F) stays inside its segment rather than splitting it.
     "." and ".." are resolved after decoding — a backstop: canonical
     URLs have already had dot segments removed by [Uri.canonicalize],
     so this only fires if a non-canonical [t] is ever constructed. *)
  let raw = Uri.path t.uri in
  let parts = String.split_on_char '/' raw in
  List.fold_left (fun acc part ->
      match Uri.pct_decode part with
      | "" | "." -> acc
      | ".." -> (match acc with _ :: tl -> tl | [] -> [])
      | seg -> seg :: acc
    ) [] parts
  |> List.rev

let has_query t = Uri.query t.uri <> []

(* Segment-wise, so a prefix covers whole path components only: without
   that, a scope for "/v3" would also admit "/v3x", a different
   endpoint that merely shares an opening. *)
let under ~prefix t =
  let rec is_prefix a b =
    match a, b with
    | [], _ -> true
    | x :: a', y :: b' -> String.equal x y && is_prefix a' b'
    | _ :: _, [] -> false
  in
  same_origin prefix t && is_prefix (path_segments prefix) (path_segments t)

let resolve ~base reference =
  let rel = Uri.of_string reference in
  of_uri (Uri.resolve (scheme_string base.scheme) base.uri rel)

let set_query_params t params =
  let uri = List.fold_left (fun u (k, _) -> Uri.remove_query_param u k) t.uri params in
  (* [add_query_param'] prepends, so folding over the reversed list keeps
     the callers' order. *)
  let uri = List.fold_left Uri.add_query_param' uri (List.rev params) in
  (* Scheme, host and userinfo are untouched, so re-validation cannot
     fail; going through [of_uri] keeps the stored form canonical. *)
  match of_uri uri with Ok t -> t | Error _ -> assert false

let to_string t = Uri.to_string t.uri


let pp f t = Fmt.string f (to_string t)
