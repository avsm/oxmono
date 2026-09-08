module Rid = Matrix_proto.Id.Room_id
module Sid = Matrix_proto.Id.Session_id

type room_key = {
  algorithm : string;
  room_id : Rid.t;
  sender_key : string;
  session_id : Sid.t;
  session_key : string;
  sender_claimed_keys : (string * string) list;
  forwarding_curve25519_key_chain : string list;
  shared_history : bool;
}

type history_not_shared = {
  algorithm : string;
  room_id : Rid.t;
  session_id : Sid.t;
  sender_key : string;
  from_device : string option;
  code : string;
  reason : string option;
}

type historic_room_key = {
  algorithm : string;
  room_id : Rid.t;
  sender_key : string;
  session_id : Sid.t;
  session_key : string;
  sender_claimed_keys : (string * string) list;
}

type room_key_bundle = {
  room_keys : historic_room_key list;
  withheld : history_not_shared list;
}

let room_key_jsont : room_key Jsont.t =
  Jsont.Object.(
    map
      (fun
        algorithm
        room_id
        sender_key
        session_id
        session_key
        sender_claimed_keys
        forwarding_curve25519_key_chain
        stable_shared_history
        unstable_shared_history
      ->
        let shared_history =
          Option.value stable_shared_history
            ~default:(Option.value unstable_shared_history ~default:false)
        in
        ({
           algorithm;
           room_id;
           sender_key;
           session_id;
           session_key;
           sender_claimed_keys;
           forwarding_curve25519_key_chain;
           shared_history;
         }
          : room_key))
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : room_key) -> t.algorithm)
    |> mem "room_id" Rid.jsont ~enc:(fun (t : room_key) -> t.room_id)
    |> mem "sender_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : room_key) -> t.sender_key)
    |> mem "session_id" Sid.jsont ~enc:(fun (t : room_key) -> t.session_id)
    |> mem "session_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : room_key) -> t.session_key)
    |> mem "sender_claimed_keys"
         (Json_codec.string_map Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_key) -> t.sender_claimed_keys)
    |> mem "forwarding_curve25519_key_chain"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_key) -> t.forwarding_curve25519_key_chain)
    |> opt_mem "m.shared_history" Jsont.bool ~enc:(fun (t : room_key) ->
        Some t.shared_history)
    |> opt_mem "org.matrix.msc3061.shared_history" Jsont.bool ~enc:(fun _ ->
        None)
    |> finish)

let room_keys_jsont = Jsont.list room_key_jsont

let historic_room_key_jsont : historic_room_key Jsont.t =
  Jsont.Object.(
    map
      (fun
        algorithm
        room_id
        sender_key
        session_id
        session_key
        sender_claimed_keys
      ->
        ({
           algorithm;
           room_id;
           sender_key;
           session_id;
           session_key;
           sender_claimed_keys;
         }
          : historic_room_key))
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : historic_room_key) -> t.algorithm)
    |> mem "room_id" Rid.jsont ~enc:(fun (t : historic_room_key) -> t.room_id)
    |> mem "sender_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : historic_room_key) -> t.sender_key)
    |> mem "session_id" Sid.jsont ~enc:(fun (t : historic_room_key) ->
        t.session_id)
    |> mem "session_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : historic_room_key) -> t.session_key)
    |> mem "sender_claimed_keys"
         (Json_codec.string_map Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : historic_room_key) -> t.sender_claimed_keys)
    |> finish)

let history_not_shared_jsont : history_not_shared Jsont.t =
  Jsont.Object.(
    map (fun algorithm room_id session_id sender_key from_device code reason ->
        {
          algorithm;
          room_id;
          session_id;
          sender_key;
          from_device;
          code;
          reason;
        })
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : history_not_shared) -> t.algorithm)
    |> mem "room_id" Rid.jsont ~enc:(fun (t : history_not_shared) -> t.room_id)
    |> mem "session_id" Sid.jsont ~enc:(fun (t : history_not_shared) ->
        t.session_id)
    |> mem "sender_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : history_not_shared) -> t.sender_key)
    |> opt_mem "from_device" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : history_not_shared) -> t.from_device)
    |> mem "code" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "m.history_not_shared")
         ~enc:(fun (t : history_not_shared) -> t.code)
    |> opt_mem "reason" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : history_not_shared) -> t.reason)
    |> finish)

let room_key_bundle_jsont : room_key_bundle Jsont.t =
  Jsont.Object.(
    map (fun room_keys withheld -> { room_keys; withheld })
    |> mem "room_keys"
         (Jsont.list historic_room_key_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.room_keys)
    |> mem "withheld"
         (Jsont.list history_not_shared_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.withheld)
    |> finish)

type error =
  | Invalid_headers
  | Invalid_base64
  | Truncated
  | Unsupported_version of int
  | Invalid_rounds of int64
  | Invalid_mac
  | Invalid_utf8
  | Invalid_json of string

let pp_error ppf = function
  | Invalid_headers -> Format.pp_print_string ppf "invalid or missing headers"
  | Invalid_base64 -> Format.pp_print_string ppf "invalid Base64 payload"
  | Truncated -> Format.pp_print_string ppf "truncated key export"
  | Unsupported_version version ->
      Format.fprintf ppf "unsupported key export version %d" version
  | Invalid_rounds rounds ->
      Format.fprintf ppf "invalid PBKDF2 iteration count %Ld" rounds
  | Invalid_mac -> Format.pp_print_string ppf "MAC verification failed"
  | Invalid_utf8 -> Format.pp_print_string ppf "plaintext is not valid UTF-8"
  | Invalid_json message -> Format.fprintf ppf "invalid key JSON: %s" message

let header = "-----BEGIN MEGOLM SESSION DATA-----"
let footer = "-----END MEGOLM SESSION DATA-----"
let version = 1
let salt_size = 16
let iv_size = 16
let mac_size = 32
let fixed_prefix_size = 1 + salt_size + iv_size + 4
let minimum_payload_size = fixed_prefix_size + mac_size
let default_rounds = 500_000

(* The wire field is an unsigned 32-bit PBKDF2 count.  Encryption is an
   explicit caller choice, so it accepts every positive value in that field
   (subject to the range representable by the host [int]). Decryption has a
   deliberately lower default because the count is supplied by an untrusted
   file; trusted callers can opt into a higher policy with [~max_rounds]. *)
let wire_max_rounds = 0xffff_ffffL
let default_max_rounds = 2_000_000L
let ( let* ) = Result.bind

let derive_keys ~passphrase ~salt ~rounds =
  let expanded =
    Pbkdf.pbkdf2 ~prf:`SHA512 ~password:passphrase ~salt ~count:rounds
      ~dk_len:64l
  in
  (String.sub expanded 0 32, String.sub expanded 32 32)

let ctr_crypt ~key ~iv data =
  let key = Mirage_crypto.AES.CTR.of_secret key in
  let ctr = Mirage_crypto.AES.CTR.ctr_of_octets iv in
  Mirage_crypto.AES.CTR.encrypt ~key ~ctr data

let hmac ~key data =
  Digestif.SHA256.to_raw_string (Digestif.SHA256.hmac_string ~key data)

let clamp_iv iv =
  let iv = Bytes.of_string iv in
  Bytes.set iv 8 (Char.chr (Char.code (Bytes.get iv 8) land 0x7f));
  Bytes.to_string iv

let uint32_be rounds =
  let bytes = Bytes.create 4 in
  Bytes.set_int32_be bytes 0
    (Int64.to_int32 (Int64.logand (Int64.of_int rounds) 0xffff_ffffL));
  Bytes.to_string bytes

let read_uint32_be data offset =
  Int64.logand (Int64.of_int32 (String.get_int32_be data offset)) 0xffff_ffffL

let encode_plaintext keys =
  match Jsont_bytesrw.encode_string room_keys_jsont keys with
  | Ok plaintext -> Ok plaintext
  | Error message -> Error (Invalid_json message)

let encrypt ~random ~passphrase ?(rounds = default_rounds) keys =
  let rounds64 = Int64.of_int rounds in
  if rounds <= 0 || rounds64 > wire_max_rounds then
    Error (Invalid_rounds rounds64)
  else
    let* plaintext = encode_plaintext keys in
    let salt = Random.generate random salt_size in
    let iv = clamp_iv (Random.generate random iv_size) in
    let aes_key, mac_key = derive_keys ~passphrase ~salt ~rounds in
    let ciphertext = ctr_crypt ~key:aes_key ~iv plaintext in
    let authenticated =
      String.make 1 (Char.chr version)
      ^ salt ^ iv ^ uint32_be rounds ^ ciphertext
    in
    let payload = authenticated ^ hmac ~key:mac_key authenticated in
    Ok
      (String.concat "\n"
         [ header; Matrix_proto.Base64.encode payload; footer ])

let strip_cr line =
  let length = String.length line in
  if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1)
  else line

let nonempty_lines input =
  String.split_on_char '\n' input
  |> List.map (fun line -> String.trim (strip_cr line))
  |> List.filter (fun line -> line <> "")

let payload_from_armour input =
  match nonempty_lines input with
  | first :: rest when String.equal first header -> (
      match List.rev rest with
      | last :: reversed_payload when String.equal last footer ->
          if
            List.exists
              (fun line -> String.starts_with ~prefix:"-----" line)
              reversed_payload
          then Error Invalid_headers
          else Ok (String.concat "" (List.rev reversed_payload))
      | _ -> Error Invalid_headers)
  | _ -> Error Invalid_headers

let valid_utf_8 = String.is_valid_utf_8

let decrypt ?(max_rounds = default_max_rounds) ~passphrase input =
  if max_rounds <= 0L || max_rounds > wire_max_rounds then
    invalid_arg
      "Matrix_client.Room_key_export.decrypt: max_rounds must be between 1 and \
       0xffffffff";
  let* encoded = payload_from_armour input in
  let* payload =
    match Matrix_proto.Base64.decode encoded with
    | Ok payload -> Ok payload
    | Error _ -> Error Invalid_base64
  in
  let length = String.length payload in
  if length < minimum_payload_size then Error Truncated
  else
    let encoded_version = Char.code payload.[0] in
    if encoded_version <> version then
      Error (Unsupported_version encoded_version)
    else
      let salt = String.sub payload 1 salt_size in
      let iv = String.sub payload (1 + salt_size) iv_size in
      let rounds64 = read_uint32_be payload (1 + salt_size + iv_size) in
      let host_max_rounds = Int64.of_int max_int in
      if rounds64 = 0L || rounds64 > max_rounds || rounds64 > host_max_rounds
      then Error (Invalid_rounds rounds64)
      else
        let rounds = Int64.to_int rounds64 in
        let ciphertext_length = length - fixed_prefix_size - mac_size in
        let authenticated = String.sub payload 0 (length - mac_size) in
        let expected_mac = String.sub payload (length - mac_size) mac_size in
        let aes_key, mac_key = derive_keys ~passphrase ~salt ~rounds in
        if
          not
            (Olm_primitives.ct_equal expected_mac
               (hmac ~key:mac_key authenticated))
        then Error Invalid_mac
        else
          let ciphertext =
            String.sub payload fixed_prefix_size ciphertext_length
          in
          let plaintext = ctr_crypt ~key:aes_key ~iv ciphertext in
          if not (valid_utf_8 plaintext) then Error Invalid_utf8
          else
            match Jsont_bytesrw.decode_string room_keys_jsont plaintext with
            | Ok keys -> Ok keys
            | Error message -> Error (Invalid_json message)
