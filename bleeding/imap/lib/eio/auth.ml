type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
type secret = Password of (unit -> string) | Bearer of (unit -> string)
type t = {
  username : string;
  secret : secret;
  mechanism : mechanism;
  allow_insecure_transport : bool;
}

let validate_username username =
  if username = "" || not (String.is_valid_utf_8 username) ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) username then
    invalid_arg "invalid IMAP username"

let password ~username ~password ?(mechanism=`Auto)
    ?(allow_insecure_transport=false) () =
  validate_username username;
  if mechanism = `Oauthbearer then
    invalid_arg "OAUTHBEARER requires a bearer token provider";
  { username; secret = Password (fun () -> password);
    mechanism; allow_insecure_transport }

let refreshing ~username ?(mechanism=`Auto)
    ?(allow_insecure_transport=false) get_password =
  validate_username username;
  if mechanism = `Oauthbearer then
    invalid_arg "OAUTHBEARER requires a bearer token provider";
  { username; secret = Password get_password;
    mechanism; allow_insecure_transport }

let bearer ~username ~token ?(allow_insecure_transport=false) () =
  validate_username username;
  { username; secret = Bearer (fun () -> token);
    mechanism = `Oauthbearer; allow_insecure_transport }

let refreshing_bearer ~username ?(allow_insecure_transport=false) get_token =
  validate_username username;
  { username; secret = Bearer get_token;
    mechanism = `Oauthbearer; allow_insecure_transport }

let username t = t.username
let mechanism t = t.mechanism
let allow_insecure_transport t = t.allow_insecure_transport
let resolve_password t =
  let secret = match t.secret with
  | Password get -> get ()
  | Bearer _ -> invalid_arg "password requested from bearer credentials" in
  if String.contains secret '\000' then invalid_arg "IMAP password contains NUL";
  secret

let resolve_token t =
  let token = match t.secret with
  | Bearer get -> get ()
  | Password _ -> invalid_arg "bearer token requested from password credentials" in
  if token = "" || String.length token > 32768 ||
     not (String.for_all (function
       | 'A'..'Z' | 'a'..'z' | '0'..'9'
       | '-' | '.' | '_' | '~' | '+' | '/' | '=' -> true
       | _ -> false) token) then
    invalid_arg "invalid OAuth bearer token";
  token

let plain_response t =
  let password = resolve_password t in
  if password = "" || not (String.is_valid_utf_8 password) then
    invalid_arg "SASL PLAIN password must be nonempty UTF-8";
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

let cram_md5_response t challenge =
  if String.contains t.username ' ' || String.contains t.username '\t' then
    invalid_arg "CRAM-MD5 username contains whitespace";
  let password = resolve_password t in
  Base64.encode_string (t.username ^ " " ^ hmac_md5 ~key:password challenge)
