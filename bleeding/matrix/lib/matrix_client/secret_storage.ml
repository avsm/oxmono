type error = [ `Msg of string ]

let err fmt = Format.kasprintf (fun s -> Error (`Msg s)) fmt
let algorithm = "m.secret_storage.v1.aes-hmac-sha2"
let pbkdf2_algorithm = "m.pbkdf2"
let secret_cross_signing_master = "m.cross_signing.master"
let secret_cross_signing_self_signing = "m.cross_signing.self_signing"
let secret_cross_signing_user_signing = "m.cross_signing.user_signing"
let secret_megolm_backup_v1 = "m.megolm_backup.v1"
let key_size = 32
let iv_size = 16

type key = string

let generate_key ~random = Random.generate random key_size

let key_of_bytes b =
  if String.length b <> key_size then
    err "a secret storage key must be %d bytes, got %d" key_size
      (String.length b)
  else Ok b

let to_bytes k = k

module Recovery_key = struct
  let encode k = Base58.encode_key k

  let decode s =
    match Base58.decode_key s with
    | Error (`Msg _) as e -> e
    | Ok b -> key_of_bytes b
end

let zero_salt = String.make 32 '\x00'
let zero_message = String.make 32 '\x00'

(* HKDF-SHA256 with a 32-zero-byte salt and the secret's name as info; the
   check value in a key description derives with an empty name. *)
let derive_keys key ~info =
  let prk = Hkdf.extract ~hash:`SHA256 ~salt:zero_salt key in
  let okm = Hkdf.expand ~hash:`SHA256 ~prk ~info 64 in
  (String.sub okm 0 32, String.sub okm 32 32)

let ctr_crypt ~aes_key ~iv data =
  let key = Mirage_crypto.AES.CTR.of_secret aes_key in
  let ctr = Mirage_crypto.AES.CTR.ctr_of_octets iv in
  Mirage_crypto.AES.CTR.encrypt ~key ~ctr data

let hmac ~mac_key data =
  Digestif.SHA256.to_raw_string (Digestif.SHA256.hmac_string ~key:mac_key data)

(* Bit 63 is cleared, as the spec requires: implementations disagree about
   AES-CTR's low 64 bits rolling over. *)
let fresh_iv ~random =
  let iv = Bytes.of_string (Random.generate random iv_size) in
  Bytes.set iv 8 (Char.chr (Char.code (Bytes.get iv 8) land 0x7f));
  Bytes.to_string iv

let check_value key ~iv =
  let aes_key, mac_key = derive_keys key ~info:"" in
  hmac ~mac_key (ctr_crypt ~aes_key ~iv zero_message)

module Passphrase_info = struct
  type t = { algorithm : string; salt : string; iterations : int; bits : int }

  let default_bits = 256
  let default_iterations = 500_000

  let v ~random ?(iterations = default_iterations) ?(bits = default_bits) () =
    {
      algorithm = pbkdf2_algorithm;
      salt = Matrix_proto.Base64.encode (Random.generate random 32);
      iterations;
      bits;
    }

  let jsont =
    Jsont.Object.(
      map (fun algorithm salt iterations bits ->
          { algorithm; salt; iterations; bits })
      |> mem "algorithm" Matrix_proto.Json.Codec.string
           ~dec_absent:(fun () -> pbkdf2_algorithm)
           ~enc:(fun (t : t) -> t.algorithm)
      |> mem "salt" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) -> t.salt)
      |> mem "iterations" Matrix_proto.Json.Codec.int ~enc:(fun (t : t) ->
          t.iterations)
      |> mem "bits" Matrix_proto.Json.Codec.int
           ~dec_absent:(fun () -> default_bits)
           ~enc:(fun (t : t) -> t.bits)
      |> finish)
end

module Key_description = struct
  type t = {
    name : string option;
    algorithm : string;
    passphrase : Passphrase_info.t option;
    iv : string option;
    mac : string option;
  }

  let v ~random ?name ?passphrase key =
    let iv = fresh_iv ~random in
    {
      name;
      algorithm;
      passphrase;
      iv = Some (Matrix_proto.Base64.encode iv);
      mac = Some (Matrix_proto.Base64.encode (check_value key ~iv));
    }

  let jsont =
    Jsont.Object.(
      map (fun name algorithm passphrase iv mac ->
          { name; algorithm; passphrase; iv; mac })
      |> opt_mem "name" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) ->
          t.name)
      |> mem "algorithm" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) ->
          t.algorithm)
      |> opt_mem "passphrase" Passphrase_info.jsont ~enc:(fun (t : t) ->
          t.passphrase)
      |> opt_mem "iv" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) -> t.iv)
      |> opt_mem "mac" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) ->
          t.mac)
      |> finish)
end

let key_of_passphrase ~passphrase (info : Passphrase_info.t) =
  if info.algorithm <> pbkdf2_algorithm then
    err "unsupported passphrase algorithm %S" info.algorithm
  else if info.bits <> key_size * 8 then
    err "unsupported passphrase key length %d" info.bits
  else if info.iterations <= 0 then err "invalid PBKDF2 iteration count"
  else
    (* The salt is used as the raw bytes of the string in account data, not
       base64-decoded, which is what every other client does. *)
    Ok
      (Pbkdf.pbkdf2 ~prf:`SHA512 ~password:passphrase ~salt:info.salt
         ~count:info.iterations
         ~dk_len:(Int32.of_int (info.bits / 8)))

type key_check = Correct | Incorrect | Unchecked

let check_key key (description : Key_description.t) =
  match (description.iv, description.mac) with
  | None, None -> Unchecked
  | None, Some _ | Some _, None -> Incorrect
  | Some iv, Some mac -> (
      match (Matrix_proto.Base64.decode iv, Matrix_proto.Base64.decode mac) with
      | Ok iv, Ok mac when String.length iv = iv_size ->
          if Olm_primitives.ct_equal mac (check_value key ~iv) then Correct
          else Incorrect
      | _ -> Incorrect)

module Encrypted = struct
  type t = { iv : string; ciphertext : string; mac : string }

  let jsont =
    Jsont.Object.(
      map (fun iv ciphertext mac -> { iv; ciphertext; mac })
      |> mem "iv" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) -> t.iv)
      |> mem "ciphertext" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) ->
          t.ciphertext)
      |> mem "mac" Matrix_proto.Json.Codec.string ~enc:(fun (t : t) -> t.mac)
      |> finish)
end

let encrypt ~random key ~name secret =
  let aes_key, mac_key = derive_keys key ~info:name in
  let iv = fresh_iv ~random in
  let ciphertext = ctr_crypt ~aes_key ~iv secret in
  {
    Encrypted.iv = Matrix_proto.Base64.encode iv;
    ciphertext = Matrix_proto.Base64.encode ciphertext;
    mac = Matrix_proto.Base64.encode (hmac ~mac_key ciphertext);
  }

let decrypt key ~name (data : Encrypted.t) =
  let decode what s =
    match Matrix_proto.Base64.decode s with
    | Ok v -> Ok v
    | Error _ -> err "invalid %s encoding" what
  in
  let ( let* ) = Result.bind in
  let* iv = decode "iv" data.iv in
  let* ciphertext = decode "ciphertext" data.ciphertext in
  let* mac = decode "mac" data.mac in
  if String.length iv <> iv_size then err "invalid iv length"
  else
    let aes_key, mac_key = derive_keys key ~info:name in
    if not (Olm_primitives.ct_equal mac (hmac ~mac_key ciphertext)) then
      err "MAC verification failed"
    else Ok (ctr_crypt ~aes_key ~iv ciphertext)
