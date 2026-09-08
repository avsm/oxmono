module Curve25519 = Crypto_key.Curve25519

type error =
  | Malformed_initial_message
  | Malformed_message
  | Invalid_public_key
  | Non_contributory_key
  | Authentication_failed
  | Counter_exhausted
  | Pending_consumed

let pp_error ppf = function
  | Malformed_initial_message ->
      Format.pp_print_string ppf "malformed initial message"
  | Malformed_message -> Format.pp_print_string ppf "malformed message"
  | Invalid_public_key -> Format.pp_print_string ppf "invalid public key"
  | Non_contributory_key -> Format.pp_print_string ppf "non-contributory key"
  | Authentication_failed ->
      Format.pp_print_string ppf "message authentication failed"
  | Counter_exhausted ->
      Format.pp_print_string ppf "message nonce counter exhausted"
  | Pending_consumed ->
      Format.pp_print_string ppf "pending channel already consumed"

type role = Initiator | Recipient

type pending = {
  mutable secret_key : Curve25519.Secret.t option;
  public_key : Curve25519.Public.t;
}

type t = {
  encryption_key : Mirage_crypto.Chacha20.key;
  decryption_key : Mirage_crypto.Chacha20.key;
  check_code : string;
  mutable encryption_counter : bytes;
  mutable decryption_counter : bytes;
  mutable encryption_exhausted : bool;
  mutable decryption_exhausted : bool;
}

let info_prefix = "MATRIX_QR_CODE_LOGIN"

let create ~random () =
  let secret_key, public_key = Curve25519.generate ~random () in
  { secret_key = Some secret_key; public_key }

let public_key (t : pending) = t.public_key

let role_info role suffix ~our_public_key ~their_public_key =
  let gp, sp =
    match role with
    | Initiator -> (their_public_key, our_public_key)
    | Recipient -> (our_public_key, their_public_key)
  in
  Printf.sprintf "%s_%s|%s|%s" info_prefix suffix
    (Curve25519.Public.to_base64 gp)
    (Curve25519.Public.to_base64 sp)

let derive ~role ~our_public_key ~their_public_key shared_secret =
  let hkdf info length =
    let prk = Hkdf.extract ~hash:`SHA512 shared_secret in
    Hkdf.expand ~hash:`SHA512 ~prk ~info length
  in
  let encryption_suffix, decryption_suffix =
    match role with
    | Initiator -> ("ENCKEY_S", "ENCKEY_G")
    | Recipient -> ("ENCKEY_G", "ENCKEY_S")
  in
  let encryption_key =
    Mirage_crypto.Chacha20.of_secret
      (hkdf
         (role_info role encryption_suffix ~our_public_key ~their_public_key)
         32)
  in
  let decryption_key =
    Mirage_crypto.Chacha20.of_secret
      (hkdf
         (role_info role decryption_suffix ~our_public_key ~their_public_key)
         32)
  in
  let check_code =
    hkdf (role_info role "CHECKCODE" ~our_public_key ~their_public_key) 2
  in
  (encryption_key, decryption_key, check_code)

let established ~role ~our_public_key ~their_public_key shared_secret =
  let encryption_key, decryption_key, check_code =
    derive ~role ~our_public_key ~their_public_key shared_secret
  in
  {
    encryption_key;
    decryption_key;
    check_code;
    encryption_counter = Bytes.make 12 '\000';
    decryption_counter = Bytes.make 12 '\000';
    encryption_exhausted = false;
    decryption_exhausted = false;
  }

let take_secret t =
  match t.secret_key with
  | None -> Error Pending_consumed
  | Some secret_key ->
      (* Spend the ephemeral key before any operation that can fail. *)
      t.secret_key <- None;
      Ok secret_key

let shared_secret secret_key public_key =
  match Curve25519.key_exchange ~secret:secret_key ~public:public_key with
  | Ok shared -> Ok shared
  | Error _ -> Error Non_contributory_key

let next_nonce counter exhausted =
  if exhausted then Error Counter_exhausted
  else
    let nonce = Bytes.to_string counter in
    let rec first_non_ff index =
      if index = Bytes.length counter then None
      else if Char.code (Bytes.get counter index) <> 0xff then Some index
      else first_non_ff (index + 1)
    in
    match first_non_ff 0 with
    | None ->
        (* Use the all-FF nonce once, then fail closed. Reusing a nonce after
           this point would be worse than refusing to send. *)
        Ok (nonce, true)
    | Some index ->
        for i = 0 to index - 1 do
          Bytes.set counter i '\000'
        done;
        Bytes.set counter index
          (Char.chr (Char.code (Bytes.get counter index) + 1));
        Ok (nonce, false)

let encrypt_raw key ~nonce plaintext =
  Mirage_crypto.Chacha20.authenticate_encrypt ~key ~nonce plaintext

let decrypt_raw key ~nonce ciphertext =
  Mirage_crypto.Chacha20.authenticate_decrypt ~key ~nonce ciphertext

let encode_initial ~public_key ciphertext =
  Matrix_proto.Base64.encode ciphertext
  ^ "|"
  ^ Curve25519.Public.to_base64 public_key

let decode_initial value =
  match String.index_opt value '|' with
  | None -> Error Malformed_initial_message
  | Some separator -> (
      if String.index_from_opt value (separator + 1) '|' <> None then
        Error Malformed_initial_message
      else
        let ciphertext = String.sub value 0 separator in
        let public_key =
          String.sub value (separator + 1) (String.length value - separator - 1)
        in
        match Matrix_proto.Base64.decode ciphertext with
        | Error _ -> Error Malformed_initial_message
        | Ok ciphertext -> (
            match Curve25519.Public.of_base64 public_key with
            | Error _ -> Error Invalid_public_key
            | Ok public_key -> Ok (public_key, ciphertext)))

let establish_outbound t ~recipient ~initial_plaintext =
  match take_secret t with
  | Error _ as error -> error
  | Ok secret_key -> (
      match shared_secret secret_key recipient with
      | Error _ as error -> error
      | Ok shared_secret -> (
          let established =
            established ~role:Initiator ~our_public_key:t.public_key
              ~their_public_key:recipient shared_secret
          in
          match
            next_nonce established.encryption_counter
              established.encryption_exhausted
          with
          | Error _ as error -> error
          | Ok (nonce, exhausted) ->
              established.encryption_exhausted <- exhausted;
              let ciphertext =
                encrypt_raw established.encryption_key ~nonce initial_plaintext
              in
              Ok
                (established, encode_initial ~public_key:t.public_key ciphertext)
          ))

let establish_inbound t value =
  match decode_initial value with
  | Error _ as error -> error
  | Ok (their_public_key, ciphertext) -> (
      match take_secret t with
      | Error _ as error -> error
      | Ok secret_key -> (
          match shared_secret secret_key their_public_key with
          | Error _ as error -> error
          | Ok shared_secret -> (
              let established =
                established ~role:Recipient ~our_public_key:t.public_key
                  ~their_public_key shared_secret
              in
              match
                next_nonce established.decryption_counter
                  established.decryption_exhausted
              with
              | Error _ as error -> error
              | Ok (nonce, exhausted) -> (
                  established.decryption_exhausted <- exhausted;
                  match
                    decrypt_raw established.decryption_key ~nonce ciphertext
                  with
                  | None -> Error Authentication_failed
                  | Some plaintext -> Ok (established, plaintext)))))

let encode_message ciphertext = Matrix_proto.Base64.encode ciphertext

let decode_message value =
  match Matrix_proto.Base64.decode value with
  | Error _ -> Error Malformed_message
  | Ok value -> Ok value

let encrypt t plaintext =
  match next_nonce t.encryption_counter t.encryption_exhausted with
  | Error _ as error -> error
  | Ok (nonce, exhausted) ->
      t.encryption_exhausted <- exhausted;
      Ok (encode_message (encrypt_raw t.encryption_key ~nonce plaintext))

let decrypt t value =
  match decode_message value with
  | Error _ as error -> error
  | Ok ciphertext -> (
      match next_nonce t.decryption_counter t.decryption_exhausted with
      | Error _ as error -> error
      | Ok (nonce, exhausted) -> (
          t.decryption_exhausted <- exhausted;
          match decrypt_raw t.decryption_key ~nonce ciphertext with
          | None -> Error Authentication_failed
          | Some plaintext -> Ok plaintext))

let check_code_bytes t = t.check_code

let check_code t =
  let first = Char.code t.check_code.[0] mod 10 in
  let second = Char.code t.check_code.[1] mod 10 in
  (first * 10) + second
