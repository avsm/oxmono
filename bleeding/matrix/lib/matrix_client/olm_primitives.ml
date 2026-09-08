let version = '\x03'
let default_salt = "\x00"
let ct_equal = Eqaf.equal
let sha256 s = Digestif.SHA256.(digest_string s |> to_raw_string)
let hmac_sha256 ~key s = Digestif.SHA256.(hmac_string ~key s |> to_raw_string)

let hkdf ~salt ~info ~ikm len =
  let prk = Hkdf.extract ~hash:`SHA256 ~salt ikm in
  Hkdf.expand ~hash:`SHA256 ~prk ~info len

let now () = Ptime_clock.now ()
let base64_encode = Matrix_proto.Base64.encode

let base64_decode s ~what =
  match Matrix_proto.Base64.decode s with
  | Ok b -> Ok b
  | Error (`Msg _) -> Error (Olm_error.Bad_base64 what)

module Varint = struct
  let encode n =
    if n < 0 then invalid_arg "Matrix_client.Olm_primitives.Varint.encode";
    let b = Buffer.create 4 in
    let rec go n =
      if n < 0x80 then Buffer.add_char b (Char.chr n)
      else begin
        Buffer.add_char b (Char.chr (0x80 lor (n land 0x7f)));
        go (n lsr 7)
      end
    in
    go n;
    Buffer.contents b

  let decode s off =
    let len = String.length s in
    (* Capped well short of the 63-bit OCaml [int]'s sign bit, so a
       malformed varint cannot make the accumulator wrap negative. Real
       Matrix field values (indices, lengths) never come close to this. *)
    let rec go off shift acc =
      if off >= len then Error (Olm_error.Bad_message_format "truncated varint")
      else if shift > 55 then
        Error (Olm_error.Bad_message_format "varint too long")
      else
        let c = Char.code s.[off] in
        let acc = acc lor ((c land 0x7f) lsl shift) in
        if c land 0x80 = 0 then Ok (acc, off + 1)
        else go (off + 1) (shift + 7) acc
    in
    go off 0 0
end

module Pb = struct
  type value = Varint of int | Bytes of string

  let ( let* ) = Result.bind

  let parse s =
    let len = String.length s in
    let rec go off acc =
      if off >= len then Ok (List.rev acc)
      else
        let* key, off = Varint.decode s off in
        let field = key lsr 3 and wire = key land 7 in
        match wire with
        | 0 ->
            let* v, off = Varint.decode s off in
            go off ((field, Varint v) :: acc)
        | 2 ->
            let* n, off = Varint.decode s off in
            if n < 0 || off + n > len then
              Error
                (Olm_error.Bad_message_format "truncated length-delimited field")
            else go (off + n) ((field, Bytes (String.sub s off n)) :: acc)
        | w ->
            Error
              (Olm_error.Bad_message_format
                 (Printf.sprintf "unsupported wire type %d" w))
    in
    go 0 []

  let bytes fields n =
    match List.assoc_opt n fields with
    | Some (Bytes s) -> Ok s
    | Some (Varint _) ->
        Error
          (Olm_error.Bad_message_format
             (Printf.sprintf "field %d is not a byte string" n))
    | None ->
        Error
          (Olm_error.Bad_message_format (Printf.sprintf "missing field %d" n))

  let varint fields n ~default =
    match List.assoc_opt n fields with
    | Some (Varint v) -> Ok v
    | Some (Bytes _) ->
        Error
          (Olm_error.Bad_message_format
             (Printf.sprintf "field %d is not an integer" n))
    | None -> Ok default

  let tag_bytes ~tag s = tag ^ Varint.encode (String.length s) ^ s
  let tag_varint tag n = tag ^ Varint.encode n
end

module Cipher = struct
  type t = { aes_key : string; mac_key : string; iv : string }

  let of_expanded e =
    if String.length e <> 80 then
      invalid_arg "Matrix_client.Olm_primitives.Cipher.of_expanded";
    {
      aes_key = String.sub e 0 32;
      mac_key = String.sub e 32 32;
      iv = String.sub e 64 16;
    }

  let olm key =
    of_expanded (hkdf ~salt:default_salt ~info:"OLM_KEYS" ~ikm:key 80)

  let megolm ratchet =
    of_expanded (hkdf ~salt:default_salt ~info:"MEGOLM_KEYS" ~ikm:ratchet 80)

  let encrypt t plaintext =
    let block = 16 in
    let pad = block - (String.length plaintext mod block) in
    let padded = plaintext ^ String.make pad (Char.chr pad) in
    let key = Mirage_crypto.AES.CBC.of_secret t.aes_key in
    Mirage_crypto.AES.CBC.encrypt ~key ~iv:t.iv padded

  let decrypt t ciphertext =
    let n = String.length ciphertext in
    if n = 0 || n mod 16 <> 0 then
      Error
        (Olm_error.Bad_message_format
           "the ciphertext is not a whole number of AES blocks")
    else
      let key = Mirage_crypto.AES.CBC.of_secret t.aes_key in
      let plain = Mirage_crypto.AES.CBC.decrypt ~key ~iv:t.iv ciphertext in
      let m = String.length plain in
      let pad = Char.code plain.[m - 1] in
      if pad = 0 || pad > 16 || pad > m then Error Olm_error.Bad_padding
      else
        (* PKCS#7 requires every padding byte to carry the length. *)
        let ok = ref true in
        for i = m - pad to m - 1 do
          if Char.code plain.[i] <> pad then ok := false
        done;
        if !ok then Ok (String.sub plain 0 (m - pad))
        else Error Olm_error.Bad_padding

  let mac t msg = hmac_sha256 ~key:t.mac_key msg
  let mac8 t msg = String.sub (mac t msg) 0 8
  let verify_mac8 t ~msg ~tag = ct_equal (mac8 t msg) tag
end
