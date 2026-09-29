type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
type secret = Password of (unit -> string) | Bearer of (unit -> string)
type t = {
  username : string;
  secret : secret;
  mechanism : mechanism;
  allow_insecure_transport : bool;
}

exception Invalid_credentials

let validate_username username =
  if username = "" || not (String.is_valid_utf_8 username) ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) username then
    invalid_arg "invalid IMAP username"

let valid_password password = not (String.contains password '\000')

(* RFC 6750 b64token: token characters, then only trailing padding. *)
let valid_token token =
  let token_char = function
    | 'A'..'Z' | 'a'..'z' | '0'..'9'
    | '-' | '.' | '_' | '~' | '+' | '/' -> true
    | _ -> false in
  let n = String.length token in
  let rec body i = if i < n && token_char token.[i] then body (i + 1) else i in
  let k = body 0 in
  k > 0 && n <= 32768 &&
  String.for_all (fun c -> c = '=') (String.sub token k (n - k))

let refreshing ~username ?(mechanism=`Auto)
    ?(allow_insecure_transport=false) get_password =
  validate_username username;
  if mechanism = `Oauthbearer then
    invalid_arg "OAUTHBEARER requires a bearer token provider";
  if mechanism = `Cram_md5 &&
     (String.contains username ' ' || String.contains username '\t') then
    invalid_arg "CRAM-MD5 username contains whitespace";
  { username; secret = Password get_password;
    mechanism; allow_insecure_transport }

let password ~username ~password ?mechanism ?allow_insecure_transport () =
  if not (valid_password password) then
    invalid_arg "IMAP password contains NUL";
  refreshing ~username ?mechanism ?allow_insecure_transport (fun () -> password)

let refreshing_bearer ~username ?(allow_insecure_transport=false) get_token =
  validate_username username;
  { username; secret = Bearer get_token;
    mechanism = `Oauthbearer; allow_insecure_transport }

let bearer ~username ~token ?allow_insecure_transport () =
  if not (valid_token token) then invalid_arg "invalid OAuth bearer token";
  refreshing_bearer ~username ?allow_insecure_transport (fun () -> token)

let username t = t.username
let mechanism t = t.mechanism
let allow_insecure_transport t = t.allow_insecure_transport

(* A provider's exception may carry the secret it failed to fetch, so it is
   replaced rather than propagated. *)
let resolve get valid =
  match get () with
  | value when valid value -> value
  | _ -> raise Invalid_credentials
  | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
  | exception _ -> raise Invalid_credentials

let resolve_password t = match t.secret with
  | Password get -> resolve get valid_password
  | Bearer _ -> invalid_arg "password requested from bearer credentials"

let resolve_token t = match t.secret with
  | Bearer get -> resolve get valid_token
  | Password _ -> invalid_arg "bearer token requested from password credentials"

let plain_response t =
  let password = resolve_password t in
  if password = "" || not (String.is_valid_utf_8 password) then
    raise Invalid_credentials;
  Base64.encode_string ("\000" ^ t.username ^ "\000" ^ password)

let escape_gs2 s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | ',' -> Buffer.add_string b "=2C"
    | '=' -> Buffer.add_string b "=3D"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let oauthbearer_response t =
  let token = resolve_token t in
  Base64.encode_string
    ("n,a=" ^ escape_gs2 t.username ^ ",\001auth=Bearer " ^ token ^ "\001\001")

(* RFC 2195, section 2. Digest.string returns the raw 16 octets. *)
let hmac_md5 ~key data =
  let key = if String.length key > 64 then Digest.string key else key in
  let pad byte =
    String.init 64 (fun i ->
      Char.chr ((if i < String.length key then Char.code key.[i] else 0) lxor byte))
  in
  Digest.to_hex (Digest.string (pad 0x5c ^
    Digest.string (pad 0x36 ^ data)))

let cram_md5_response t =
  if String.contains t.username ' ' || String.contains t.username '\t' then
    raise Invalid_credentials;
  let password = resolve_password t in
  fun challenge ->
    Base64.encode_string (t.username ^ " " ^ hmac_md5 ~key:password challenge)
