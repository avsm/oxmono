module Uid = Matrix_proto.Id.User_id
module Rid = Matrix_proto.Id.Room_id
module Sid = Matrix_proto.Id.Session_id
module Curve25519 = Crypto_key.Curve25519
module Key_id = Crypto_key.Key_id

type error = [ `Msg of string ]

let err fmt = Format.kasprintf (fun s -> Error (`Msg s)) fmt
let backup_algorithm = "m.megolm_backup.v1.curve25519-aes-sha2"
let megolm_algorithm = "m.megolm.v1.aes-sha2"

type encryption_key = Curve25519.Public.t

module Decryption_key = struct
  type t = Curve25519.Secret.t

  let generate ~random = fst (Curve25519.generate ~random ())
  let of_bytes b = Curve25519.Secret.of_bytes b

  let of_base64 s =
    match Matrix_proto.Base64.decode s with
    | Error _ -> err "not base64"
    | Ok b -> of_bytes b

  let to_base64 t = Matrix_proto.Base64.encode (Curve25519.Secret.to_bytes t)
  let public = Curve25519.Secret.public
end

module Recovery_key = struct
  let encode key = Base58.encode_key (Curve25519.Secret.to_bytes key)

  let decode s =
    match Base58.decode_key s with
    | Error (`Msg _) as e -> e
    | Ok bytes -> Decryption_key.of_bytes bytes
end

type encrypted_session_data = {
  ephemeral : string;
  ciphertext : string;
  mac : string;
}

let encrypted_session_data_jsont =
  Jsont.Object.(
    map (fun ephemeral ciphertext mac -> { ephemeral; ciphertext; mac })
    |> mem "ephemeral" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.ephemeral)
    |> mem "ciphertext" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.ciphertext)
    |> mem "mac" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.mac)
    |> finish)

type key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : encrypted_session_data;
}

let key_backup_data_jsont =
  Jsont.Object.(
    map (fun first_message_index forwarded_count is_verified session_data ->
        { first_message_index; forwarded_count; is_verified; session_data })
    |> mem "first_message_index" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.first_message_index)
    |> mem "forwarded_count" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.forwarded_count)
    |> mem "is_verified" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.is_verified)
    |> mem "session_data" encrypted_session_data_jsont ~enc:(fun t ->
        t.session_data)
    |> finish)

type sessions = (string * key_backup_data) list
type rooms = (string * sessions) list

type backed_up_session_data = {
  algorithm : string;
  forwarding_curve25519_key_chain : string list;
  sender_key : string;
  sender_claimed_keys : (string * string) list;
  session_key : string;
  shared_history : bool;
}

let string_map_jsont = Json_codec.string_map Matrix_proto.Json.Codec.string

let backed_up_session_data_jsont =
  Jsont.Object.(
    map
      (fun
        algorithm
        forwarding_curve25519_key_chain
        sender_key
        sender_claimed_keys
        session_key
        shared_history
      ->
        {
          algorithm;
          forwarding_curve25519_key_chain;
          sender_key;
          sender_claimed_keys;
          session_key;
          shared_history;
        })
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> megolm_algorithm)
         ~enc:(fun t -> t.algorithm)
    |> mem "forwarding_curve25519_key_chain"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.forwarding_curve25519_key_chain)
    |> mem "sender_key" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.sender_key)
    |> mem "sender_claimed_keys" string_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.sender_claimed_keys)
    |> mem "session_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.session_key)
    |> mem "shared_history" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.shared_history)
    |> finish)

(* The spec asks for a salt of 32 zero bytes; HMAC pads a short key with
   zeros to the block size, so an empty salt is the same key schedule. *)
let derive_cipher_keys shared_secret =
  let prk = Hkdf.extract ~hash:`SHA256 ~salt:"" shared_secret in
  let okm = Hkdf.expand ~hash:`SHA256 ~prk ~info:"" 80 in
  (String.sub okm 0 32, String.sub okm 32 32, String.sub okm 64 16)

(* The MAC covers the empty string rather than the ciphertext, which is what
   the algorithm specifies; MACing the ciphertext would make this unable to
   read any other client's backup. *)
let session_mac ~mac_key =
  let mac = Digestif.SHA256.hmac_string ~key:mac_key "" in
  String.sub (Digestif.SHA256.to_raw_string mac) 0 8

let pkcs7_pad s =
  let pad_len = 16 - (String.length s mod 16) in
  s ^ String.make pad_len (Char.chr pad_len)

let pkcs7_unpad s =
  let n = String.length s in
  if n = 0 || n mod 16 <> 0 then err "the ciphertext is not block-aligned"
  else
    let pad_len = Char.code (String.get s (n - 1)) in
    if pad_len < 1 || pad_len > 16 || pad_len > n then err "invalid padding"
    else
      (* PKCS#7 requires every padding byte to carry the length. *)
      let ok = ref true in
      for i = n - pad_len to n - 1 do
        if Char.code s.[i] <> pad_len then ok := false
      done;
      if !ok then Ok (String.sub s 0 (n - pad_len)) else err "invalid padding"

let encrypt_session_data ~random recipient plaintext =
  let ephemeral_secret, ephemeral_public = Curve25519.generate ~random () in
  match Curve25519.key_exchange ~secret:ephemeral_secret ~public:recipient with
  | Error (`Msg m) -> err "key exchange failed: %s" m
  | Ok shared_secret ->
      let aes_key, mac_key, iv = derive_cipher_keys shared_secret in
      let key = Mirage_crypto.AES.CBC.of_secret aes_key in
      let ciphertext =
        Mirage_crypto.AES.CBC.encrypt ~key ~iv (pkcs7_pad plaintext)
      in
      Ok
        {
          ephemeral =
            Matrix_proto.Base64.encode
              (Curve25519.Public.to_bytes ephemeral_public);
          ciphertext = Matrix_proto.Base64.encode ciphertext;
          mac = Matrix_proto.Base64.encode (session_mac ~mac_key);
        }

let encrypt_room_key ~random encryption_key ~session_key ~sender_key =
  match
    ( Curve25519.Public.of_base64 sender_key,
      Megolm.Inbound.validate_exported_session_key session_key )
  with
  | Error (`Msg message), _ -> err "invalid sender key: %s" message
  | _, Error error -> err "%a" Olm_error.pp error
  | Ok sender_key, Ok () -> (
      let payload =
        {
          algorithm = megolm_algorithm;
          forwarding_curve25519_key_chain = [];
          sender_key = Curve25519.Public.to_base64 sender_key;
          sender_claimed_keys = [];
          session_key;
          shared_history = false;
        }
      in
      match
        Jsont_bytesrw.encode_string backed_up_session_data_jsont payload
      with
      | Error m -> err "%s" m
      | Ok plaintext -> encrypt_session_data ~random encryption_key plaintext)

let decrypt_room_key decryption_key session_data =
  let decode what s =
    match Matrix_proto.Base64.decode s with
    | Ok v -> Ok v
    | Error _ -> err "invalid %s encoding" what
  in
  let ( let* ) = Result.bind in
  let* ephemeral = decode "ephemeral key" session_data.ephemeral in
  let* ciphertext = decode "ciphertext" session_data.ciphertext in
  let* mac_bytes = decode "MAC" session_data.mac in
  let* ephemeral = Curve25519.Public.of_bytes ephemeral in
  let* shared_secret =
    Curve25519.key_exchange ~secret:decryption_key ~public:ephemeral
  in
  let aes_key, mac_key, iv = derive_cipher_keys shared_secret in
  if not (Olm_primitives.ct_equal mac_bytes (session_mac ~mac_key)) then
    err "MAC verification failed"
  else if String.length ciphertext mod 16 <> 0 then
    err "the ciphertext is not block-aligned"
  else
    let key = Mirage_crypto.AES.CBC.of_secret aes_key in
    pkcs7_unpad (Mirage_crypto.AES.CBC.decrypt ~key ~iv ciphertext)

type recovered_room_key = {
  room_id : Rid.t;
  session_id : Sid.t;
  session_key : string;
  sender_key : string;
  algorithm : string;
  forwarded : bool;
  sender_claimed_keys : (string * string) list;
  forwarding_curve25519_key_chain : string list;
  shared_history : bool;
}

let parse_recovered_key ~room_id ~session_id json_str =
  match Jsont_bytesrw.decode_string backed_up_session_data_jsont json_str with
  | Error m -> err "%s" m
  | Ok d ->
      Ok
        {
          room_id;
          session_id;
          session_key = d.session_key;
          sender_key = d.sender_key;
          algorithm = d.algorithm;
          forwarded = d.forwarding_curve25519_key_chain <> [];
          sender_claimed_keys = d.sender_claimed_keys;
          forwarding_curve25519_key_chain = d.forwarding_curve25519_key_chain;
          shared_history = d.shared_history;
        }

type megolm_v1_auth_data = {
  public_key : encryption_key;
  signatures : Keys.signatures;
}

let megolm_v1_auth_data_jsont : megolm_v1_auth_data Jsont.t =
  Jsont.Object.(
    map (fun public_key signatures -> { public_key; signatures })
    |> mem "public_key" Curve25519.Public.jsont
         ~enc:(fun (t : megolm_v1_auth_data) -> t.public_key)
    |> mem "signatures" Keys.signatures_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : megolm_v1_auth_data) -> t.signatures)
    |> finish)

(* [Jsont.json]'s encoder is total for a value built from a record, so the
   error branch is unreachable. *)
let auth_data_to_json auth_data =
  match Jsont.Json.encode megolm_v1_auth_data_jsont auth_data with
  | Ok j -> j
  | Error _ -> Jsont.Json.null ()

(* What a signature over the auth data covers: its canonical JSON with
   [signatures] removed. Only [public_key] is left. *)
let auth_data_signing_bytes (auth_data : megolm_v1_auth_data) =
  Matrix_proto.Signed_json.canonical_json
    (Jsont.Json.object'
       [
         Jsont.Json.mem
           (Jsont.Json.name "public_key")
           (Jsont.Json.string
              (Curve25519.Public.to_base64 auth_data.public_key));
       ])

let sign_auth_data ~signing_key ~user_id ~key_id
    (auth_data : megolm_v1_auth_data) =
  let signature =
    Crypto_key.Ed25519.Private.sign signing_key
      (auth_data_signing_bytes auth_data)
  in
  let mine =
    match
      List.find_opt (fun (u, _) -> Uid.equal u user_id) auth_data.signatures
    with
    | None -> []
    | Some (_, sigs) ->
        List.filter (fun (k, _) -> not (Key_id.equal k key_id)) sigs
  in
  let others =
    List.filter (fun (u, _) -> not (Uid.equal u user_id)) auth_data.signatures
  in
  {
    auth_data with
    signatures = (user_id, (key_id, signature) :: mine) :: others;
  }

type signature_state =
  | Missing
  | Invalid
  | Valid_but_not_trusted
  | Valid_and_trusted

let verify_auth_data_signature ~verify_key (auth_data : megolm_v1_auth_data)
    ~user_id ~key_id =
  match
    Option.bind
      (List.find_opt (fun (u, _) -> Uid.equal u user_id) auth_data.signatures)
      (fun (_, sigs) ->
        List.find_opt (fun (k, _) -> Key_id.equal k key_id) sigs)
  with
  | None -> Missing
  | Some (_, signature) ->
      if
        Crypto_key.Ed25519.Public.verify verify_key ~signature
          ~data:(auth_data_signing_bytes auth_data)
      then Valid_but_not_trusted
      else Invalid

type version_state =
  | Absent
  | Current of string
  | Server_only of string
  | Local_only of string
  | Diverged of { server : string; local : string }

let version_state ~server ~local =
  match (server, local) with
  | None, None -> Absent
  | Some server, Some local when String.equal server local -> Current server
  | Some server, None -> Server_only server
  | None, Some local -> Local_only local
  | Some server, Some local -> Diverged { server; local }

type current_version_state =
  | Compatible
  | Missing_local_key
  | Unsupported_algorithm of string
  | Malformed_auth_data of string
  | Different_public_key

let current_version_state ~algorithm ~auth_data ~local_key =
  if not (String.equal algorithm backup_algorithm) then
    Unsupported_algorithm algorithm
  else
    match local_key with
    | None -> Missing_local_key
    | Some local_key -> (
        match Jsont.Json.decode megolm_v1_auth_data_jsont auth_data with
        | Error msg -> Malformed_auth_data msg
        | Ok server_auth ->
            if Curve25519.Public.equal server_auth.public_key local_key then
              Compatible
            else Different_public_key)
