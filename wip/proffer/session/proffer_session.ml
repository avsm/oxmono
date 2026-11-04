(* SPDX-License-Identifier: ISC *)
module Req = Proffer.Req

module Store = struct
  type record = {
    subject : string;
    created : Ptime.t;
    seen : Ptime.t;
    expires : Ptime.t;
    csrf : string;
    data : string;
  }

  type t = {
    get : string -> record option;
    put : string -> record -> unit;
    delete : string -> unit;
  }

  let memory () =
    let table = Hashtbl.create 16 in
    {
      get = Hashtbl.find_opt table;
      put = Hashtbl.replace table;
      delete = Hashtbl.remove table;
    }
end

type t = {
  name : string;
  origin : string;
  host : string;
  idle : Ptime.Span.t;
  lifetime : Ptime.Span.t;
  random : int -> string;
  now : unit -> Ptime.t;
  store : Store.t;
}

type session = { key : string; record : Store.record }

(* Unpadded base64url, the alphabet a cookie value may hold unquoted. *)
let base64url s =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  in
  let b = Buffer.create ((String.length s * 4 / 3) + 4) in
  let byte i = Char.code s.[i] in
  let n = String.length s in
  let rec go i =
    if i + 2 < n then (
      let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      List.iter
        (fun shift -> Buffer.add_char b alphabet.[(v lsr shift) land 63])
        [ 18; 12; 6; 0 ];
      go (i + 3))
    else if i + 1 = n - 1 then (
      let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) in
      List.iter
        (fun shift -> Buffer.add_char b alphabet.[(v lsr shift) land 63])
        [ 18; 12; 6 ])
    else if i = n - 1 then (
      let v = byte i lsl 16 in
      List.iter
        (fun shift -> Buffer.add_char b alphabet.[(v lsr shift) land 63])
        [ 18; 12 ])
  in
  go 0;
  Buffer.contents b

(* An ID is 32 random bytes, so 43 base64url characters. *)
let id_bytes = 32

let well_formed s =
  String.length s = 43
  && String.for_all
       (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       s

let sha256_hex s = Digestif.SHA256.(to_hex (digest_string s))
let id_hash = sha256_hex

let valid_name name =
  name <> ""
  && String.length name <= 64
  && String.for_all
       (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       name

(* The origin is configuration: an https origin, or http on a loopback host
   for development, with no path. *)
let parse_origin origin =
  let origin = String.lowercase_ascii origin in
  let rest scheme =
    let n = String.length scheme in
    if String.length origin > n && String.sub origin 0 n = scheme then
      Some (String.sub origin n (String.length origin - n))
    else None
  in
  let authority, secure =
    match (rest "https://", rest "http://") with
    | Some a, _ -> (a, true)
    | None, Some a -> (a, false)
    | None, None -> invalid_arg ("Not an http(s) origin: " ^ origin)
  in
  if
    authority = ""
    || String.exists (function '/' | '?' | '#' | '@' -> true | _ -> false) authority
  then invalid_arg ("An origin has no path, query or credentials: " ^ origin);
  let host =
    match String.index_opt authority ':' with
    | Some i -> String.sub authority 0 i
    | None -> authority
  in
  if host = "" || not (Cookie.valid_domain host) then
    invalid_arg ("Not a host name: " ^ host);
  if (not secure) && host <> "localhost" && host <> "127.0.0.1" then
    invalid_arg ("Only a loopback origin may be plain http: " ^ origin);
  (origin, host)

let reader random n =
  let buf = Cstruct.create n in
  Eio.Flow.read_exact random buf;
  Cstruct.to_string buf

let v ?(idle = Ptime.Span.of_int_s (2 * 3600))
    ?(lifetime = Ptime.Span.of_int_s (12 * 3600)) ~name ~origin ~random ~clock
    store =
  if not (valid_name name) then invalid_arg ("Not a cookie name: " ^ name);
  let origin, host = parse_origin origin in
  let random = (random :> Eio.Flow.source_ty Eio.Resource.t) in
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  {
    name;
    origin;
    host;
    idle;
    lifetime;
    random = reader random;
    now =
      (fun () ->
        match Ptime.of_float_s (Eio.Time.now clock) with
        | Some t -> t
        | None -> failwith "The clock is out of range");
    store;
  }

let cookie_name t = "__Host-" ^ t.name
let subject s = s.record.subject
let data s = s.record.data
let created s = s.record.created
let expires s = s.record.expires

let set_cookie t ~name ~value ~expiry =
  let c =
    Cookie.v ~domain:t.host ~path:"/" ~name ~value ~secure:true
      ~http_only:true ~host_only:true ~same_site:`Lax ~expiry ~now:(t.now ())
      ()
  in
  assert (Cookie.prefix_is_satisfied c);
  Proffer.Headers.h Httpz.Header_name.Set_cookie (Cookie.set_cookie_header c)

let add t span =
  match Ptime.add_span (t.now ()) span with
  | Some x -> x
  | None -> failwith "The session expiry is out of range"

let create t ~subject ?(data = "") () =
  let id = base64url (t.random id_bytes) in
  let now = t.now () in
  let record =
    {
      Store.subject;
      created = now;
      seen = now;
      expires = add t t.lifetime;
      csrf = t.random 32;
      data;
    }
  in
  let key = id_hash id in
  t.store.put key record;
  ( { key; record },
    set_cookie t ~name:(cookie_name t) ~value:id ~expiry:(`At record.expires) )

(* A cookie counts only when the request carries exactly one of that name. *)
let only_cookie req name =
  match
    List.filter (fun (n, _) -> n = name) (Cookie.parse_cookie_header (Req.cookies req))
  with
  | [ (_, value) ] when well_formed value -> Some value
  | _ -> None

let minute = Ptime.Span.of_int_s 60

let find t req =
  match only_cookie req (cookie_name t) with
  | None -> None
  | Some id -> (
      let key = id_hash id in
      match t.store.get key with
      | None -> None
      | Some record ->
          let now = t.now () in
          let idle_end =
            Option.value ~default:now (Ptime.add_span record.seen t.idle)
          in
          if (not (Ptime.is_later record.expires ~than:now))
             || not (Ptime.is_later idle_end ~than:now)
          then (
            t.store.delete key;
            None)
          else
            let record =
              if Ptime.Span.compare (Ptime.diff now record.seen) minute >= 0
              then (
                let r = { record with seen = now } in
                t.store.put key r;
                r)
              else record
            in
            Some { key; record })

let set_data t s data =
  let record = { s.record with data } in
  t.store.put s.key record;
  { s with record }

let clear_cookie t = set_cookie t ~name:(cookie_name t) ~value:"" ~expiry:(`At Ptime.epoch)

let destroy t s =
  t.store.delete s.key;
  clear_cookie t

let csrf_field = "_csrf"

let csrf_token ?(purpose = "form") s =
  base64url
    Digestif.SHA256.(to_raw_string (hmac_string ~key:s.record.csrf ("csrf:" ^ purpose)))

let header req name =
  match Req.header_single req name with
  | Ok (Some v) -> `One (Req.globalize v)
  | Ok None -> `None
  | Error `Repeated -> `Repeated

let check_csrf ?purpose s req =
  let submitted =
    match if Req.is_form req then Req.form_param req csrf_field else None with
    | Some v -> Some v
    | None -> ( match header req "X-CSRF-Token" with `One v -> Some v | _ -> None)
  in
  match submitted with
  | Some v -> Eqaf.equal v (csrf_token ?purpose s)
  | None -> false

type origin = [ `Same | `Cross | `Unknown ]

let request_origin t req =
  let site =
    match header req "Sec-Fetch-Site" with
    | `None -> `Unknown
    | `Repeated -> `Cross
    | `One v -> (
        match String.lowercase_ascii (String.trim v) with
        | "same-origin" | "none" -> `Same
        | _ -> `Cross)
  in
  let origin =
    match header req "Origin" with
    | `None -> `Unknown
    | `Repeated -> `Cross
    | `One v ->
        if String.lowercase_ascii (String.trim v) = t.origin then `Same
        else `Cross
  in
  match (site, origin) with
  | `Cross, _ | _, `Cross -> `Cross
  | `Same, _ | _, `Same -> `Same
  | `Unknown, `Unknown -> `Unknown

let unsafe (m : Proffer.Method.t) =
  match Proffer.Method.to_string m with
  | "POST" | "PUT" | "PATCH" | "DELETE" -> true
  | _ -> false

let check_unsafe ?purpose t s req =
  (not (unsafe (Req.meth req)))
  || (request_origin t req <> `Cross && check_csrf ?purpose s req)

module Login = struct
  let cookie_name t = "__Host-" ^ t.name ^ "-login"
  let lifetime = Ptime.Span.of_int_s 600
  let hash = sha256_hex

  let start t =
    let nonce = base64url (t.random id_bytes) in
    (nonce, set_cookie t ~name:(cookie_name t) ~value:nonce ~expiry:(`At (add t lifetime)))

  let matches t req ~hash:expected =
    match only_cookie req (cookie_name t) with
    | Some nonce -> Eqaf.equal (hash nonce) (String.lowercase_ascii expected)
    | None -> false

  let clear t =
    set_cookie t ~name:(cookie_name t) ~value:"" ~expiry:(`At Ptime.epoch)
end
