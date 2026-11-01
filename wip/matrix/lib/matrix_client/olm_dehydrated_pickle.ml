module Ck = Crypto_key
module Account = Olm_account

type error = [ `Msg of string ]

type decoded = {
  account : Account.t;
  published_one_time_keys : Ck.Key_id.t list;
  published_fallback_keys : Ck.Key_id.t list;
}

let fail fmt = Format.kasprintf (fun s -> Error (`Msg s)) fmt
let ( let* ) = Result.bind

let key_id_of_string key_id =
  Ck.Key_id.v ~algorithm:Account.one_time_key_algorithm ~id:key_id

let version = 4l
let max_array_length = 0xffff
let pickle_info = "dehydrated-device-pickle-key"

let key_id_of_u32 n =
  let b = Bytes.create 8 in
  (* [n] is already a non-negative OCaml int; converting through int32 would
     sign-extend IDs whose top u32 bit is set. *)
  Bytes.set_int64_be b 0 (Int64.of_int n);
  Ck.Key_id.v ~algorithm:Account.one_time_key_algorithm
    ~id:(Olm_primitives.base64_encode (Bytes.unsafe_to_string b))

let u32_of_key_id id =
  if Ck.Key_id.algorithm id <> Account.one_time_key_algorithm then
    fail "one-time key has unexpected algorithm"
  else
    match Matrix_proto.Base64.decode (Ck.Key_id.id id) with
    | Error _ -> fail "one-time key id is not base64"
    | Ok b when String.length b <> 8 -> fail "one-time key id is not 8 bytes"
    | Ok b when String.sub b 0 4 <> "\000\000\000\000" ->
        fail "one-time key id exceeds the legacy u32 range"
    | Ok b ->
        Ok
          (Int64.to_int
             (Int64.logand
                (Int64.of_int32 (Bytes.get_int32_be (Bytes.of_string b) 4))
                0xffffffffL))

let add_u32 b n =
  if n < 0 || Int64.compare (Int64.of_int n) 0xffffffffL > 0 then
    invalid_arg "Olm_dehydrated_pickle.add_u32";
  let x = Bytes.create 4 in
  Bytes.set_int32_be x 0 (Int32.of_int n);
  Buffer.add_bytes b x

let add_byte b n = Buffer.add_char b (Char.chr n)
let add_bytes b s = Buffer.add_string b s

let read s off n =
  if n < 0 || n > String.length s - !off then fail "truncated account pickle"
  else
    let r = String.sub s !off n in
    off := !off + n;
    Ok r

let read_u8 s off =
  let* b = read s off 1 in
  Ok (Char.code b.[0])

let read_u32 s off =
  let* b = read s off 4 in
  Ok
    (Int64.to_int
       (Int64.logand
          (Int64.of_int32 (Bytes.get_int32_be (Bytes.of_string b) 0))
          0xffffffffL))

let read_key_id s off =
  let* n = read_u32 s off in
  Ok (n, key_id_of_u32 n)

type raw_key = {
  key_id : int;
  id : Ck.Key_id.t;
  published : bool;
  public_key : string;
  private_key : string;
}

let read_record s off =
  let* key_id, id = read_key_id s off in
  let* published = read_u8 s off in
  if published > 1 then fail "invalid one-time-key publication flag"
  else
    let* public_key = read s off 32 in
    let* private_key = read s off 32 in
    Ok { key_id; id; published = published = 1; public_key; private_key }

let encode_record b ~is_published (k : Account.stored_key) =
  let id = Ck.Key_id.v ~algorithm:Account.one_time_key_algorithm ~id:k.key_id in
  let* n = u32_of_key_id id in
  add_u32 b n;
  add_byte b (if is_published id then 1 else 0);
  add_bytes b
    (Ck.Curve25519.Public.to_bytes (Ck.Curve25519.Secret.public k.secret));
  add_bytes b (Ck.Curve25519.Secret.to_bytes k.secret);
  Ok ()

let encode_plain ~is_published (a : Account.t) =
  let p = Account.to_pickle a in
  let b = Buffer.create 256 in
  add_u32 b (Int32.to_int version);
  add_bytes b (Ck.Ed25519.Public.to_bytes (Ck.Ed25519.Private.public p.ed25519));
  add_bytes b (Ck.Ed25519.Private.to_expanded_bytes p.ed25519);
  add_bytes b
    (Ck.Curve25519.Public.to_bytes (Ck.Curve25519.Secret.public p.curve25519));
  add_bytes b (Ck.Curve25519.Secret.to_bytes p.curve25519);
  if List.length p.stored_one_time_keys > max_array_length then
    fail "too many one-time keys"
  else begin
    add_u32 b (List.length p.stored_one_time_keys);
    let* () =
      List.fold_left
        (fun acc k ->
          let* () = acc in
          encode_record b ~is_published k)
        (Ok ()) p.stored_one_time_keys
    in
    let fallback =
      match (p.stored_fallback_key, p.stored_previous_fallback_key) with
      | None, None -> []
      | Some k, None -> [ k ]
      | Some k, Some previous -> [ k; previous ]
      | None, Some k -> [ k ]
    in
    add_byte b (List.length fallback);
    let* () =
      List.fold_left
        (fun acc k ->
          let* () = acc in
          encode_record b ~is_published k)
        (Ok ()) fallback
    in
    if p.next_key_id < 0 || p.next_key_id > 0xffffffff then
      fail "next key ID exceeds the legacy u32 range"
    else begin
      add_u32 b p.next_key_id;
      Ok (Buffer.contents b)
    end
  end

let derive_key ~device_id ~pickle_key =
  if String.length pickle_key <> 32 then fail "pickle key must be 32 bytes"
  else
    Ok
      (Olm_primitives.hkdf
         ~salt:(Matrix_proto.Id.Device_id.to_string device_id)
         ~info:pickle_info ~ikm:pickle_key 32)

let encrypt ~device_id ~pickle_key plain =
  let* key = derive_key ~device_id ~pickle_key in
  let cipher =
    Olm_primitives.Cipher.of_expanded
      (Olm_primitives.hkdf ~salt:Olm_primitives.default_salt ~info:"Pickle"
         ~ikm:key 80)
  in
  let ciphertext = Olm_primitives.Cipher.encrypt cipher plain in
  Ok
    (Olm_primitives.base64_encode
       (ciphertext ^ Olm_primitives.Cipher.mac8 cipher ciphertext))

let decrypt ~device_id ~pickle_key value =
  let* key = derive_key ~device_id ~pickle_key in
  let* encoded =
    match Olm_primitives.base64_decode value ~what:"account pickle" with
    | Ok bytes -> Ok bytes
    | Error (Olm_error.Bad_base64 what) -> fail "invalid base64: %s" what
    | Error _ -> fail "invalid base64"
  in
  if String.length encoded < 24 then fail "account pickle is too short"
  else
    let n = String.length encoded - 8 in
    let ciphertext = String.sub encoded 0 n in
    let tag = String.sub encoded n 8 in
    let cipher =
      Olm_primitives.Cipher.of_expanded
        (Olm_primitives.hkdf ~salt:Olm_primitives.default_salt ~info:"Pickle"
           ~ikm:key 80)
    in
    if not (Olm_primitives.Cipher.verify_mac8 cipher ~msg:ciphertext ~tag) then
      Error (`Msg "account pickle MAC mismatch")
    else
      match Olm_primitives.Cipher.decrypt cipher ciphertext with
      | Ok plain -> Ok plain
      | Error _ -> fail "account pickle has invalid padding"

let decode_plain s =
  let off = ref 0 in
  let* v = read_u32 s off in
  if v <> Int32.to_int version then
    fail "unsupported account pickle version %d" v
  else
    let* ed_public = read s off 32 in
    let* ed_expanded = read s off 64 in
    let* curve_public = read s off 32 in
    let* curve_private = read s off 32 in
    let* one_time_count = read_u32 s off in
    if one_time_count > max_array_length then fail "too many one-time keys"
    else
      let rec records n acc seen =
        if n = 0 then Ok (List.rev acc, seen)
        else
          let* k = read_record s off in
          if List.mem k.key_id seen then fail "duplicate one-time key ID"
          else records (n - 1) (k :: acc) (k.key_id :: seen)
      in
      let* one_time, seen = records one_time_count [] [] in
      let* fallback_count = read_u8 s off in
      if fallback_count > 2 then fail "invalid fallback-key count"
      else
        let rec fallback n acc seen =
          if n = 0 then Ok (List.rev acc, seen)
          else
            let* k = read_record s off in
            if List.mem k.key_id seen then fail "duplicate fallback key ID"
            else fallback (n - 1) (k :: acc) (k.key_id :: seen)
        in
        (* One-time and fallback key counters are independent in vodozemac,
           so the same numeric ID may legitimately occur once in each set. *)
        let* fallback, _ = fallback fallback_count [] [] in
        let* next_key_id = read_u32 s off in
        if !off <> String.length s then fail "trailing bytes in account pickle"
        else
          let* ed = Ck.Ed25519.Private.of_expanded_bytes ed_expanded in
          let ed_actual =
            Ck.Ed25519.Public.to_bytes (Ck.Ed25519.Private.public ed)
          in
          if not (String.equal ed_actual ed_public) then
            fail "Ed25519 public/private mismatch"
          else
            let* curve = Ck.Curve25519.Secret.of_bytes curve_private in
            let curve_actual =
              Ck.Curve25519.Public.to_bytes (Ck.Curve25519.Secret.public curve)
            in
            if not (String.equal curve_actual curve_public) then
              fail "Curve25519 public/private mismatch"
            else
              let convert (k : raw_key) =
                let* secret = Ck.Curve25519.Secret.of_bytes k.private_key in
                let actual =
                  Ck.Curve25519.Public.to_bytes
                    (Ck.Curve25519.Secret.public secret)
                in
                if not (String.equal actual k.public_key) then
                  fail "one-time public/private mismatch"
                else
                  Ok
                    ({ Account.key_id = Ck.Key_id.id k.id; secret }, k.published)
              in
              let* otks =
                List.fold_left
                  (fun acc k ->
                    let* xs = acc in
                    let* x = convert k in
                    Ok (x :: xs))
                  (Ok []) one_time
              in
              let* fb =
                List.fold_left
                  (fun acc k ->
                    let* xs = acc in
                    let* x = convert k in
                    Ok (x :: xs))
                  (Ok []) fallback
              in
              let one_time_keys, published_otks = List.split (List.rev otks) in
              let fallback_keys, published_fallback =
                List.split (List.rev fb)
              in
              let current, previous =
                match fallback_keys with
                | [] -> (None, None)
                | [ x ] -> (Some x, None)
                | x :: y :: _ -> (Some x, Some y)
              in
              Ok
                ( {
                    Account.ed25519 = ed;
                    curve25519 = curve;
                    stored_one_time_keys = one_time_keys;
                    stored_fallback_key = current;
                    stored_previous_fallback_key = previous;
                    next_key_id;
                    max_one_time_keys = 50;
                  },
                  List.filter_map
                    (fun ((k : Account.stored_key), p) ->
                      if p then Some (key_id_of_string k.key_id) else None)
                    (List.combine one_time_keys published_otks),
                  List.filter_map
                    (fun ((k : Account.stored_key), p) ->
                      if p then Some (key_id_of_string k.key_id) else None)
                    (List.combine fallback_keys published_fallback) )

let pickle ?(is_published = fun _ -> false) ~device_id ~pickle_key account =
  let* plain = encode_plain ~is_published account in
  encrypt ~device_id ~pickle_key plain

let unpickle ~device_id ~pickle_key value =
  let* plain = decrypt ~device_id ~pickle_key value in
  let* p, published_one_time_keys, published_fallback_keys =
    decode_plain plain
  in
  Ok
    {
      account = Account.of_pickle p;
      published_one_time_keys;
      published_fallback_keys;
    }
