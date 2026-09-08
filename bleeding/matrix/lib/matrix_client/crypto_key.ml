module Ec_ed25519 = Mirage_crypto_ec.Ed25519
module Ec_x25519 = Mirage_crypto_ec.X25519
module B64 = Matrix_proto.Base64

type error = [ `Msg of string ]

let err fmt = Format.kasprintf (fun m -> Error (`Msg m)) fmt

let of_base64 what parse s =
  match B64.decode s with
  | Error (`Msg m) -> err "%s: invalid base64: %s" what m
  | Ok b -> parse b

let string_jsont ~dec ~enc =
  Jsont.map
    ~dec:(fun s ->
      match dec s with
      | Ok v -> v
      | Error (`Msg m) -> Jsont.Error.msg Jsont.Meta.none m)
    ~enc Matrix_proto.Json.Codec.string

module Signature = struct
  type t = string

  let length = 64

  let of_bytes b =
    if String.length b = length then Ok b
    else err "signature is %d bytes, not %d" (String.length b) length

  let to_bytes t = t
  let of_base64 s = of_base64 "signature" of_bytes s
  let to_base64 t = B64.encode t
  let equal = String.equal
  let compare = String.compare
  let pp ppf t = Format.pp_print_string ppf (to_base64 t)
  let jsont = string_jsont ~dec:of_base64 ~enc:to_base64
end

module Ed25519 = struct
  module Public = struct
    type t = Ec_ed25519.pub

    let of_bytes b =
      match Ec_ed25519.pub_of_octets b with
      | Ok k -> Ok k
      | Error e ->
          err "invalid Ed25519 public key: %a" Mirage_crypto_ec.pp_error e

    let to_bytes = Ec_ed25519.pub_to_octets
    let of_base64 s = of_base64 "Ed25519 public key" of_bytes s
    let to_base64 t = B64.encode (to_bytes t)

    let verify t ~signature ~data =
      Ec_ed25519.verify ~key:t (Signature.to_bytes signature) ~msg:data

    let equal a b = String.equal (to_bytes a) (to_bytes b)
    let compare a b = String.compare (to_bytes a) (to_bytes b)
    let pp ppf t = Format.pp_print_string ppf (to_base64 t)
    let jsont = string_jsont ~dec:of_base64 ~enc:to_base64
  end

  module Private = struct
    (* Mirage exposes Ed25519 private keys as seeds, while libolm's account
       pickle carries the expanded (scalar || prefix) representation.  Keep
       both forms here so an account restored from a legacy pickle can still
       sign without losing either half of the expanded key. *)
    type t = { seed : string option; expanded : string; public : Public.t }

    external scalar_mult_base_to_bytes : bytes -> string -> unit @@ portable
      = "mc_25519_scalar_mult_base"
    [@@noalloc]

    external reduce_l : bytes -> unit @@ portable = "mc_25519_reduce_l"
    [@@noalloc]

    external muladd : bytes -> string -> string -> string -> unit @@ portable
      = "mc_25519_muladd"
    [@@noalloc]

    let expanded_of_seed seed =
      Digestif.SHA512.(digest_string seed |> to_raw_string)

    let clamped_scalar expanded =
      let scalar = Bytes.of_string (String.sub expanded 0 32) in
      Bytes.set_uint8 scalar 0 (Bytes.get_uint8 scalar 0 land 248);
      Bytes.set_uint8 scalar 31 (Bytes.get_uint8 scalar 31 land 127 lor 64);
      Bytes.unsafe_to_string scalar

    let public_of_scalar scalar =
      let out = Bytes.create 32 in
      scalar_mult_base_to_bytes out scalar;
      Bytes.unsafe_to_string out

    let of_expanded_bytes b =
      if String.length b <> 64 then
        err "Ed25519 expanded private key is %d bytes, not 64" (String.length b)
      else
        match Public.of_bytes (public_of_scalar (clamped_scalar b)) with
        | Ok public -> Ok { seed = None; expanded = b; public }
        | Error (`Msg m) -> Error (`Msg m)

    let of_bytes b =
      if String.length b <> 32 then
        err "invalid Ed25519 private key length %d" (String.length b)
      else
        match
          Public.of_bytes
            (public_of_scalar (clamped_scalar (expanded_of_seed b)))
        with
        | Ok public ->
            Ok { seed = Some b; expanded = expanded_of_seed b; public }
        | Error (`Msg m) -> Error (`Msg m)

    let of_stored_bytes b =
      if String.length b = 64 then of_expanded_bytes b else of_bytes b

    let to_bytes t =
      match t.seed with
      | Some seed -> seed
      | None ->
          (* There is no seed corresponding to an expanded Ed25519 key; retain
             the expanded representation so existing stores can round-trip it. *)
          t.expanded

    let to_expanded_bytes t = t.expanded
    let public t = t.public

    let sign t data =
      let scalar = clamped_scalar t.expanded in
      let prefix = String.sub t.expanded 32 32 in
      let r =
        Digestif.SHA512.(digest_string (prefix ^ data) |> to_raw_string)
      in
      let r = Bytes.of_string r in
      reduce_l r;
      let r = Bytes.unsafe_to_string r in
      let r_big = Bytes.create 32 in
      scalar_mult_base_to_bytes r_big r;
      let r_big = Bytes.unsafe_to_string r_big in
      let k =
        Digestif.SHA512.(
          digest_string (r_big ^ Public.to_bytes t.public ^ data)
          |> to_raw_string)
      in
      let k = Bytes.of_string k in
      reduce_l k;
      let k = Bytes.unsafe_to_string k in
      let s = Bytes.create 32 in
      muladd s k scalar r;
      let signature = r_big ^ Bytes.unsafe_to_string s in
      match Signature.of_bytes signature with
      | Ok s -> s
      | Error (`Msg m) ->
          (* Ed25519 signatures are 64 bytes by construction. *)
          invalid_arg ("Matrix_client.Crypto_key.Ed25519.Private.sign: " ^ m)
  end

  (* [Random.generate] returns exactly the length asked for and the curve
     library rejects only a wrong length, so a seed it refuses means the
     randomness source is broken, which no caller can act on. *)
  let seed_rejected what e =
    invalid_arg
      (Format.asprintf "Matrix_client.Crypto_key.%s: %a" what
         Mirage_crypto_ec.pp_error e)

  let generate ~random () =
    let seed = Random.generate random 32 in
    match Private.of_bytes seed with
    | Ok priv -> (priv, Private.public priv)
    | Error (`Msg m) ->
        invalid_arg ("Matrix_client.Crypto_key.Ed25519.generate: " ^ m)
end

module Curve25519 = struct
  module Public = struct
    type t = string

    let length = 32

    let of_bytes b =
      if String.length b = length then Ok b
      else
        err "Curve25519 public key is %d bytes, not %d" (String.length b) length

    let to_bytes t = t
    let of_base64 s = of_base64 "Curve25519 public key" of_bytes s
    let to_base64 t = B64.encode t
    let equal = String.equal
    let compare = String.compare
    let pp ppf t = Format.pp_print_string ppf (to_base64 t)
    let jsont = string_jsont ~dec:of_base64 ~enc:to_base64
  end

  module Secret = struct
    type t = { secret : Ec_x25519.secret; public : Public.t }

    let of_bytes b =
      match Ec_x25519.secret_of_octets b with
      | Ok (secret, public) -> Ok { secret; public }
      | Error e ->
          err "invalid Curve25519 secret key: %a" Mirage_crypto_ec.pp_error e

    let to_bytes t = Ec_x25519.secret_to_octets t.secret
    let public t = t.public
  end

  let generate ~random () =
    match Secret.of_bytes (Random.generate random 32) with
    | Ok s -> (s, Secret.public s)
    | Error (`Msg m) ->
        invalid_arg ("Matrix_client.Crypto_key.Curve25519.generate: " ^ m)

  (* Non-contributory keys are refused, as vodozemac does. [Ec_x25519] already
     makes this exact check (it rejects a low-order result as [`Low_order]
     before ever returning [Ok] with one), so nothing further is needed
     here. *)
  let key_exchange ~secret ~public =
    match Ec_x25519.key_exchange secret.Secret.secret public with
    | Error e ->
        err "X25519 key exchange failed: %a" Mirage_crypto_ec.pp_error e
    | Ok s -> Ok s
end

module Key_id = struct
  type t = { algorithm : string; id : string }

  let valid_algorithm a = a <> "" && not (String.contains a ':')

  let v ~algorithm ~id =
    if not (valid_algorithm algorithm) then
      invalid_arg "Matrix_client.Crypto_key.Key_id.v: bad algorithm"
    else if id = "" then
      invalid_arg "Matrix_client.Crypto_key.Key_id.v: empty identifier"
    else { algorithm; id }

  let of_device ~algorithm device =
    v ~algorithm ~id:(Matrix_proto.Id.Device_id.to_string device)

  let of_string s =
    match String.index_opt s ':' with
    | None -> err "key identifier %S has no algorithm" s
    | Some i ->
        let algorithm = String.sub s 0 i in
        let id = String.sub s (i + 1) (String.length s - i - 1) in
        if valid_algorithm algorithm && id <> "" then Ok { algorithm; id }
        else err "key identifier %S is malformed" s

  let to_string t = t.algorithm ^ ":" ^ t.id
  let algorithm t = t.algorithm
  let id t = t.id
  let equal a b = String.equal a.algorithm b.algorithm && String.equal a.id b.id

  let compare a b =
    match String.compare a.algorithm b.algorithm with
    | 0 -> String.compare a.id b.id
    | c -> c

  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let jsont = string_jsont ~dec:of_string ~enc:to_string
end
