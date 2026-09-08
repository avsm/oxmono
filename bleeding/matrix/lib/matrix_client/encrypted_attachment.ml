type error =
  | Malformed_metadata of string
  | Unsupported_version of string
  | Unsupported_algorithm of string
  | Hash_mismatch
  | Invalid_state of string

let pp_error ppf = function
  | Malformed_metadata s ->
      Format.fprintf ppf "malformed encrypted attachment metadata: %s" s
  | Unsupported_version s ->
      Format.fprintf ppf "unsupported encrypted attachment version: %s" s
  | Unsupported_algorithm s ->
      Format.fprintf ppf "unsupported encrypted attachment algorithm: %s" s
  | Hash_mismatch ->
      Format.pp_print_string ppf "encrypted attachment ciphertext hash mismatch"
  | Invalid_state s ->
      Format.fprintf ppf "invalid encrypted attachment state: %s" s

type metadata = {
  version : string;
  algorithm : string;
  kty : string;
  key_ops : string list;
  ext : bool;
  key : string;
  iv : string;
  hash : string;
}

type jwk = {
  kty : string;
  alg : string;
  ext : bool;
  k : string;
  key_ops : string list;
}

let jwk_jsont =
  Jsont.Object.(
    map (fun kty alg ext k key_ops -> { kty; alg; ext; k; key_ops })
    |> mem "kty" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.kty)
    |> mem "alg" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.alg)
    |> mem "ext" Jsont.bool ~enc:(fun t -> t.ext)
    |> mem "k" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.k)
    |> mem "key_ops" (Jsont.list Matrix_proto.Json.Codec.string) ~enc:(fun t ->
        t.key_ops)
    |> finish)

(* The codec above needs the complete JWK while decoding.  Decode it with a
   small explicit shape so malformed JWKs don't get silently reduced to an
   empty field by the convenience codec. *)
type decoded = {
  version : string;
  jwk : jwk;
  iv : string;
  hashes : (string * string) list;
}

let decoded_jsont =
  Jsont.Object.(
    map (fun version jwk iv hashes -> { version; jwk; iv; hashes })
    |> mem "v" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.version)
    |> mem "key" jwk_jsont ~enc:(fun t -> t.jwk)
    |> mem "iv" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.iv)
    |> mem "hashes" (Json_codec.string_map Matrix_proto.Json.Codec.string)
         ~enc:(fun t -> t.hashes)
    |> finish)

let b64std s = Matrix_proto.Base64.decode s

let b64url s =
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s with
  | Ok x -> Ok x
  | Error _ -> Error (`Msg "invalid base64url")

let malformed s = Error (Malformed_metadata s)

let validate ({ version; algorithm; kty; key_ops; ext; key; iv; hash } as t) =
  if version <> "v2" then Error (Unsupported_version version)
  else if algorithm <> "A256CTR" then Error (Unsupported_algorithm algorithm)
  else if kty <> "oct" then malformed "JWK kty must be oct"
  else if not ext then malformed "JWK ext must be true"
  else if
    key_ops <> [ "decrypt"; "encrypt" ] && key_ops <> [ "encrypt"; "decrypt" ]
  then malformed "JWK key_ops must contain encrypt and decrypt"
  else
    match b64url key with
    | Error _ -> malformed "JWK k is not valid base64url"
    | Ok key_bytes when String.length key_bytes <> 32 ->
        malformed "JWK k must decode to 32 bytes"
    | Ok _ -> (
        match b64std iv with
        | Error _ -> malformed "iv is not valid base64"
        | Ok iv_bytes when String.length iv_bytes <> 16 ->
            malformed "iv must decode to 16 bytes"
        | Ok _ -> (
            match b64std hash with
            | Error _ -> malformed "sha256 is not valid base64"
            | Ok hash_bytes when String.length hash_bytes <> 32 ->
                malformed "sha256 must decode to 32 bytes"
            | Ok _ -> Ok t))

let metadata_jsont =
  Jsont.map
    ~dec:(fun { version; jwk; iv; hashes } ->
      match List.assoc_opt "sha256" hashes with
      | None -> Jsont.Error.msg Jsont.Meta.none "hashes has no sha256 member"
      | Some hash -> (
          let metadata =
            {
              version;
              algorithm = jwk.alg;
              kty = jwk.kty;
              key_ops = jwk.key_ops;
              ext = jwk.ext;
              key = jwk.k;
              iv;
              hash;
            }
          in
          match validate metadata with
          | Ok metadata -> metadata
          | Error error ->
              Jsont.Error.msg Jsont.Meta.none
                (Format.asprintf "%a" pp_error error)))
    ~enc:(fun t ->
      {
        version = t.version;
        jwk =
          {
            kty = t.kty;
            alg = t.algorithm;
            ext = t.ext;
            k = t.key;
            key_ops = t.key_ops;
          };
        iv = t.iv;
        hashes = [ ("sha256", t.hash) ];
      })
    decoded_jsont

module Metadata = struct
  type t = metadata

  let make ?(version = "v2") ?(algorithm = "A256CTR")
      ?(key_ops = [ "decrypt"; "encrypt" ]) ?(ext = true) ~key ~iv ~hash () =
    validate { version; algorithm; kty = "oct"; key_ops; ext; key; iv; hash }

  let key (t : metadata) = t.key
  let iv (t : metadata) = t.iv
  let hash (t : metadata) = t.hash
  let version (t : metadata) = t.version
  let algorithm (t : metadata) = t.algorithm
  let key_ops (t : metadata) = t.key_ops
  let ext (t : metadata) = t.ext
  let jsont = metadata_jsont

  let of_json json =
    match Jsont.Json.decode decoded_jsont json with
    | Error e -> malformed e
    | Ok { version; jwk; iv; hashes } -> (
        match List.assoc_opt "sha256" hashes with
        | None -> malformed "hashes has no sha256 member"
        | Some hash ->
            validate
              {
                version;
                algorithm = jwk.alg;
                kty = jwk.kty;
                key_ops = jwk.key_ops;
                ext = jwk.ext;
                key = jwk.k;
                iv;
                hash;
              })

  let to_json (t : metadata) =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "v") (Jsont.Json.string t.version);
        Jsont.Json.mem (Jsont.Json.name "key")
          (Jsont.Json.object'
             [
               Jsont.Json.mem (Jsont.Json.name "kty") (Jsont.Json.string t.kty);
               Jsont.Json.mem (Jsont.Json.name "alg")
                 (Jsont.Json.string t.algorithm);
               Jsont.Json.mem (Jsont.Json.name "ext") (Jsont.Json.bool t.ext);
               Jsont.Json.mem (Jsont.Json.name "k") (Jsont.Json.string t.key);
               Jsont.Json.mem
                 (Jsont.Json.name "key_ops")
                 (Jsont.Json.list (List.map Jsont.Json.string t.key_ops));
             ]);
        Jsont.Json.mem (Jsont.Json.name "iv") (Jsont.Json.string t.iv);
        Jsont.Json.mem (Jsont.Json.name "hashes")
          (Jsont.Json.object'
             [
               Jsont.Json.mem (Jsont.Json.name "sha256")
                 (Jsont.Json.string t.hash);
             ]);
      ]

  let of_json_string s =
    match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s with
    | Error e -> malformed e
    | Ok json -> of_json json

  let to_json_string t =
    match
      Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json (to_json t)
    with
    | Ok s -> s
    | Error _ -> invalid_arg "Encrypted_attachment.Metadata.to_json_string"

  let of_event_file
      (f : Matrix_proto.Event.Media_message_content.encrypted_file) =
    match
      of_json
        (Jsont.Json.object'
           [
             Jsont.Json.mem (Jsont.Json.name "v") (Jsont.Json.string f.v);
             Jsont.Json.mem (Jsont.Json.name "key") f.key;
             Jsont.Json.mem (Jsont.Json.name "iv") (Jsont.Json.string f.iv);
             Jsont.Json.mem (Jsont.Json.name "hashes")
               (Jsont.Json.object'
                  (List.map
                     (fun (k, v) ->
                       Jsont.Json.mem (Jsont.Json.name k) (Jsont.Json.string v))
                     f.hashes));
           ])
    with
    | Ok t -> Ok t
    | Error e -> Error e

  let to_event_file ~url (t : metadata) =
    let key =
      Jsont.Json.object'
        [
          Jsont.Json.mem (Jsont.Json.name "kty") (Jsont.Json.string t.kty);
          Jsont.Json.mem (Jsont.Json.name "alg") (Jsont.Json.string t.algorithm);
          Jsont.Json.mem (Jsont.Json.name "ext") (Jsont.Json.bool t.ext);
          Jsont.Json.mem (Jsont.Json.name "k") (Jsont.Json.string t.key);
          Jsont.Json.mem
            (Jsont.Json.name "key_ops")
            (Jsont.Json.list (List.map Jsont.Json.string t.key_ops));
        ]
    in
    {
      Matrix_proto.Event.Media_message_content.url;
      key;
      iv = t.iv;
      hashes = [ ("sha256", t.hash) ];
      v = t.version;
    }
end

let xor_with_keystream key ctr position input =
  let n = String.length input in
  if n = 0 then ""
  else
    let block = position / 16 and rem = position mod 16 in
    let ctr = Mirage_crypto.AES.CTR.add_ctr ctr (Int64.of_int block) in
    let stream = Mirage_crypto.AES.CTR.stream ~key ~ctr (n + rem) in
    let out = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set out i
        (Char.chr (Char.code input.[i] lxor Char.code stream.[i + rem]))
    done;
    Bytes.unsafe_to_string out

module Encryptor = struct
  type t = {
    key : string;
    iv : string;
    ctr : Mirage_crypto.AES.CTR.ctr;
    mutable position : int;
    mutable sha : Digestif.SHA256.ctx;
    mutable done_ : bool;
  }

  let create ~random () =
    let key = Random.generate random 32 in
    let iv = Random.generate random 8 ^ String.make 8 '\000' in
    let ctr = Mirage_crypto.AES.CTR.ctr_of_octets iv in
    { key; iv; ctr; position = 0; sha = Digestif.SHA256.init (); done_ = false }

  let feed t input =
    if t.done_ then
      invalid_arg "Encrypted_attachment.Encryptor.feed after finish";
    let encrypted =
      xor_with_keystream
        (Mirage_crypto.AES.CTR.of_secret t.key)
        t.ctr t.position input
    in
    t.position <- t.position + String.length input;
    t.sha <- Digestif.SHA256.feed_string t.sha encrypted;
    encrypted

  let finish t =
    if t.done_ then invalid_arg "Encrypted_attachment.Encryptor.finish twice";
    t.done_ <- true;
    let hash = Digestif.SHA256.to_raw_string (Digestif.SHA256.get t.sha) in
    let b64url s =
      Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s
    in
    let key = b64url t.key
    and iv = Matrix_proto.Base64.encode t.iv
    and hash = Matrix_proto.Base64.encode hash in
    (* All three values are encoded directly from fixed-size byte strings, so
       constructing the private representation here preserves [Metadata]'s
       validation invariant without introducing a partial result path. *)
    {
      version = "v2";
      algorithm = "A256CTR";
      kty = "oct";
      key_ops = [ "decrypt"; "encrypt" ];
      ext = true;
      key;
      iv;
      hash;
    }
end

module Decryptor = struct
  type t = {
    key : string;
    iv : string;
    expected : string;
    ctr : Mirage_crypto.AES.CTR.ctr;
    mutable position : int;
    mutable sha : Digestif.SHA256.ctx;
    mutable done_ : bool;
  }

  let create metadata =
    match validate metadata with
    | Error e -> Error e
    | Ok _ -> (
        match
          (b64url metadata.key, b64std metadata.iv, b64std metadata.hash)
        with
        | Error _, _, _ -> malformed "JWK k is not valid base64url"
        | _, Error _, _ -> malformed "iv is not valid base64"
        | _, _, Error _ -> malformed "sha256 is not valid base64"
        | Ok key, Ok iv, Ok expected ->
            Ok
              {
                key;
                iv;
                expected;
                ctr = Mirage_crypto.AES.CTR.ctr_of_octets iv;
                position = 0;
                sha = Digestif.SHA256.init ();
                done_ = false;
              })

  let feed t input =
    if t.done_ then
      invalid_arg "Encrypted_attachment.Decryptor.feed after finish";
    t.sha <- Digestif.SHA256.feed_string t.sha input;
    let plain =
      xor_with_keystream
        (Mirage_crypto.AES.CTR.of_secret t.key)
        t.ctr t.position input
    in
    t.position <- t.position + String.length input;
    plain

  let equal_bytes a b = Eqaf.equal a b

  let finish t =
    if t.done_ then Error (Invalid_state "Decryptor.finish twice")
    else begin
      t.done_ <- true;
      let got = Digestif.SHA256.to_raw_string (Digestif.SHA256.get t.sha) in
      if equal_bytes got t.expected then Ok () else Error Hash_mismatch
    end
end

type encrypted = { ciphertext : string; metadata : metadata }

let encrypt ~random plaintext =
  let e = Encryptor.create ~random () in
  let ciphertext = Encryptor.feed e plaintext in
  { ciphertext; metadata = Encryptor.finish e }

let decrypt metadata ciphertext =
  match Decryptor.create metadata with
  | Error e -> Error e
  | Ok d -> (
      let plaintext = Decryptor.feed d ciphertext in
      match Decryptor.finish d with Ok () -> Ok plaintext | Error e -> Error e)

let decrypt_chunks metadata ~chunks ~on_chunk =
  match Decryptor.create metadata with
  | Error e -> Error e
  | Ok d ->
      let rec loop plaintext_chunks =
        match chunks () with
        | None -> (
            match Decryptor.finish d with
            | Error e -> Error e
            | Ok () ->
                List.iter on_chunk (List.rev plaintext_chunks);
                Ok ())
        | Some chunk ->
            let plaintext = Decryptor.feed d chunk in
            loop (plaintext :: plaintext_chunks)
      in
      loop []

let decrypt_verified = decrypt

let decrypt_spooled metadata ~spool ~output =
  let rewind () =
    Io_context.with_context "rewinding encrypted attachment spool" (fun () ->
        ignore (Eio.File.seek spool Optint.Int63.zero `Set))
  in
  let pass ~emit =
    match Decryptor.create metadata with
    | Error e -> Error e
    | Ok decryptor ->
        let buf = Cstruct.create (64 * 1024) in
        let rec loop () =
          match
            try
              Some
                (Io_context.with_context "reading encrypted attachment spool"
                   (fun () -> Eio.Flow.single_read spool buf))
            with End_of_file -> None
          with
          | None -> Decryptor.finish decryptor
          | Some n ->
              let plain =
                Decryptor.feed decryptor (Cstruct.to_string ~off:0 ~len:n buf)
              in
              emit plain;
              loop ()
        in
        loop ()
  in
  rewind ();
  match pass ~emit:(fun _ -> ()) with
  | Error e ->
      rewind ();
      Error e
  | Ok () -> (
      rewind ();
      match
        pass ~emit:(fun plain ->
            if String.length plain > 0 then
              Io_context.with_context "writing decrypted attachment" (fun () ->
                  Eio.Flow.write output [ Cstruct.of_string plain ]))
      with
      | Ok () ->
          rewind ();
          Ok ()
      | Error e ->
          rewind ();
          Error e)
