open Olm_primitives
module Ed25519 = Crypto_key.Ed25519
module Curve25519 = Crypto_key.Curve25519
module Session_id = Matrix_proto.Id.Session_id
module Room_id = Matrix_proto.Id.Room_id
module User_id = Matrix_proto.Id.User_id
module Device_id = Matrix_proto.Id.Device_id

let ( let* ) = Result.bind
let ratchet_length = 128
let part_count = 4

module Ratchet = struct
  type t = { data : Bytes.t; mutable counter : int }

  let of_bytes s counter =
    { data = Bytes.of_string s; counter = counter land 0xFFFFFFFF }

  let copy t = { data = Bytes.copy t.data; counter = t.counter }
  let to_string t = Bytes.to_string t.data
  let index t = t.counter

  (* H_j(R_from) = HMAC-SHA256(R_from, "\x0j"), written into part [to_]. *)
  let update t ~from_ ~to_ =
    let key = Bytes.sub_string t.data (from_ * 32) 32 in
    let out = hmac_sha256 ~key (String.make 1 (Char.chr to_)) in
    Bytes.blit_string out 0 t.data (to_ * 32) 32

  let advance t =
    t.counter <- (t.counter + 1) land 0xFFFFFFFF;
    let h = ref (part_count - 1) in
    (try
       let mask = ref 0x00FFFFFF in
       for i = 0 to part_count - 1 do
         if t.counter land !mask = 0 then begin
           h := i;
           raise Exit
         end;
         mask := !mask lsr 8
       done
     with Exit -> ());
    for i = part_count - 1 downto !h do
      update t ~from_:!h ~to_:i
    done

  let advance_to t target =
    let target = target land 0xFFFFFFFF in
    for j = 0 to part_count - 1 do
      let shift = (part_count - 1 - j) * 8 in
      let mask = (0xFFFFFFFF lsl shift) land 0xFFFFFFFF in
      let steps =
        ref (((target lsr shift) - (t.counter lsr shift)) land 0xff)
      in
      let skip = ref false in
      if !steps = 0 then
        if target < t.counter then steps := 0x100 else skip := true;
      if not !skip then begin
        while !steps > 1 do
          update t ~from_:j ~to_:j;
          decr steps
        done;
        for k = part_count - 1 downto j do
          update t ~from_:j ~to_:k
        done;
        t.counter <- target land mask
      end
    done
end

type exported_key = {
  ek_index : int;
  ek_ratchet : string;
  ek_signing_key : Ed25519.Public.t;
}

let be32 n =
  let b = Bytes.create 4 in
  Bytes.set_int32_be b 0 (Int32.of_int n);
  Bytes.unsafe_to_string b

let read_be32 s off = Int32.to_int (String.get_int32_be s off) land 0xFFFFFFFF

let encode_exported_key ~version k =
  String.concat ""
    [
      String.make 1 (Char.chr version);
      be32 k.ek_index;
      k.ek_ratchet;
      Ed25519.Public.to_bytes k.ek_signing_key;
    ]

let exported_key_length = 1 + 4 + ratchet_length + 32
let session_key_length = exported_key_length + 64

let decode_exported_key ~version bytes =
  if String.length bytes < exported_key_length then
    Error (Olm_error.Bad_message_format "Megolm session key is too short")
  else if Char.code bytes.[0] <> version then
    Error (Olm_error.Bad_message_version (Char.code bytes.[0]))
  else
    match
      Ed25519.Public.of_bytes (String.sub bytes (5 + ratchet_length) 32)
    with
    | Error (`Msg _) -> Error (Olm_error.Bad_key "the session signing key")
    | Ok ek_signing_key ->
        Ok
          {
            ek_index = read_be32 bytes 1;
            ek_ratchet = String.sub bytes 5 ratchet_length;
            ek_signing_key;
          }

let encode_message ~message_index ~ciphertext =
  String.concat ""
    [
      String.make 1 version;
      Pb.tag_varint "\x08" message_index;
      Pb.tag_bytes ~tag:"\x12" ciphertext;
    ]

type parsed_message = {
  mm_mac_body : string;
  mm_sig_body : string;
  mm_signature : Crypto_key.Signature.t;
  mm_index : int;
  mm_mac : string;
  mm_ciphertext : string;
}

let parse_message bytes =
  let n = String.length bytes in
  if n < 8 + 64 + 2 then
    Error (Olm_error.Bad_message_format "Megolm message too short")
  else if bytes.[0] <> version then
    Error (Olm_error.Bad_message_version (Char.code bytes.[0]))
  else begin
    let mac_body = String.sub bytes 0 (n - 72) in
    let mac = String.sub bytes (n - 72) 8 in
    let* signature =
      match Crypto_key.Signature.of_bytes (String.sub bytes (n - 64) 64) with
      | Ok s -> Ok s
      | Error (`Msg m) -> Error (Olm_error.Bad_message_format m)
    in
    let* fields =
      Pb.parse (String.sub mac_body 1 (String.length mac_body - 1))
    in
    let* index = Pb.varint fields 1 ~default:0 in
    (* The ratchet's counter is 32 bits (see [Ratchet]); mask a wire value
       that claims to be wider so it cannot desynchronise the index reported
       in [decrypted] from the key actually used to decrypt. *)
    let index = index land 0xFFFFFFFF in
    let* ciphertext = Pb.bytes fields 2 in
    Ok
      {
        mm_mac_body = mac_body;
        mm_sig_body = String.sub bytes 0 (n - 64);
        mm_signature = signature;
        mm_index = index;
        mm_mac = mac;
        mm_ciphertext = ciphertext;
      }
  end

type decrypted = { plaintext : string; message_index : int }
type encrypted = { message_index : int; ciphertext : string }

module Inbound = struct
  type t = {
    initial_ratchet : Ratchet.t;
    mutable latest_ratchet : Ratchet.t;
    signing_key : Ed25519.Public.t;
    signing_key_verified : bool;
    sender_key : Curve25519.Public.t;
    (* The sender's *device* Ed25519 key, as claimed in the Olm plaintext that
       carried this session. It is unrelated to the session's own signing key,
       which is what verifies the messages. *)
    claimed_ed25519 : Ed25519.Public.t option;
    room_id : Room_id.t;
    creation_time : Ptime.t;
  }

  let of_exported ~sender_key ~room_id ~claimed_ed25519 ~verified k =
    let initial = Ratchet.of_bytes k.ek_ratchet k.ek_index in
    {
      initial_ratchet = initial;
      latest_ratchet = Ratchet.copy initial;
      signing_key = k.ek_signing_key;
      signing_key_verified = verified;
      sender_key;
      claimed_ed25519;
      room_id;
      creation_time = now ();
    }

  let of_session_key ?claimed_ed25519 ~sender_key ~room_id ~session_key () =
    let* bytes = base64_decode session_key ~what:"the Megolm session key" in
    if String.length bytes <> session_key_length then
      Error
        (Olm_error.Bad_message_format "Megolm session key has the wrong length")
    else
      let* k = decode_exported_key ~version:2 bytes in
      let signed = String.sub bytes 0 exported_key_length in
      let* signature =
        match
          Crypto_key.Signature.of_bytes
            (String.sub bytes exported_key_length 64)
        with
        | Ok s -> Ok s
        | Error (`Msg m) -> Error (Olm_error.Bad_message_format m)
      in
      if not (Ed25519.Public.verify k.ek_signing_key ~signature ~data:signed)
      then Error Olm_error.Bad_signature
      else
        Ok (of_exported ~sender_key ~room_id ~claimed_ed25519 ~verified:true k)

  let decode_exported_session_key session_key =
    let* bytes = base64_decode session_key ~what:"the Megolm session key" in
    if String.length bytes <> exported_key_length then
      Error
        (Olm_error.Bad_message_format
           "Megolm exported session key has the wrong length")
    else decode_exported_key ~version:1 bytes

  let validate_exported_session_key session_key =
    Result.map ignore (decode_exported_session_key session_key)

  let of_exported_session_key ?claimed_ed25519 ~sender_key ~room_id ~session_key
      () =
    let* k = decode_exported_session_key session_key in
    Ok (of_exported ~sender_key ~room_id ~claimed_ed25519 ~verified:false k)

  let session_id t =
    Session_id.of_string_exn (Ed25519.Public.to_base64 t.signing_key)

  let sender_key t = t.sender_key
  let room_id t = t.room_id
  let signing_key t = t.signing_key
  let sender_claimed_ed25519_key t = t.claimed_ed25519
  let signing_key_verified t = t.signing_key_verified
  let first_known_index t = Ratchet.index t.initial_ratchet
  let latest_index t = Ratchet.index t.latest_ratchet
  let creation_time t = t.creation_time

  let of_any_session_key ?claimed_ed25519 ~sender_key ~room_id ~session_key () =
    let* bytes = base64_decode session_key ~what:"the Megolm session key" in
    if String.length bytes = 0 then
      Error (Olm_error.Bad_message_format "empty Megolm session key")
    else
      match Char.code bytes.[0] with
      | 2 ->
          of_session_key ?claimed_ed25519 ~sender_key ~room_id ~session_key ()
      | 1 ->
          of_exported_session_key ?claimed_ed25519 ~sender_key ~room_id
            ~session_key ()
      | v -> Error (Olm_error.Bad_message_version v)

  let from_room_key ?signing_key ~sender_key ~room_id ~session_id:expected
      ~session_key () =
    let* t =
      of_any_session_key ?claimed_ed25519:signing_key ~sender_key ~room_id
        ~session_key ()
    in
    if Session_id.equal expected (session_id t) then Ok t
    else Error Olm_error.Session_id_mismatch

  (* Winding [latest_ratchet] forward is destructive, so it is only used for an
     index at or after the one it already holds. *)
  let find_ratchet t index =
    if Ratchet.index t.initial_ratchet = index then Some t.initial_ratchet
    else if Ratchet.index t.latest_ratchet = index then Some t.latest_ratchet
    else if Ratchet.index t.latest_ratchet < index then begin
      Ratchet.advance_to t.latest_ratchet index;
      Some t.latest_ratchet
    end
    else if Ratchet.index t.initial_ratchet < index then begin
      t.latest_ratchet <- Ratchet.copy t.initial_ratchet;
      Ratchet.advance_to t.latest_ratchet index;
      Some t.latest_ratchet
    end
    else None

  let decrypt t ~ciphertext =
    let* bytes = base64_decode ciphertext ~what:"the Megolm message" in
    let* m = parse_message bytes in
    if
      not
        (Ed25519.Public.verify t.signing_key ~signature:m.mm_signature
           ~data:m.mm_sig_body)
    then Error Olm_error.Bad_signature
    else
      (* Ratcheting is destructive.  Work on a private candidate until all
         authentication and decryption checks have succeeded, so a signed but
         corrupt future message cannot consume the cached receiving state. *)
      let candidate =
        { t with latest_ratchet = Ratchet.copy t.latest_ratchet }
      in
      match find_ratchet candidate m.mm_index with
      | None ->
          Error
            (Olm_error.Unknown_message_index
               { index = m.mm_index; first_known = first_known_index t })
      | Some r ->
          let cipher = Cipher.megolm (Ratchet.to_string r) in
          if not (Cipher.verify_mac8 cipher ~msg:m.mm_mac_body ~tag:m.mm_mac)
          then Error Olm_error.Bad_mac
          else
            let* plaintext = Cipher.decrypt cipher m.mm_ciphertext in
            t.latest_ratchet <- candidate.latest_ratchet;
            Ok { plaintext; message_index = m.mm_index }

  let export_of t r =
    base64_encode
      (encode_exported_key ~version:1
         {
           ek_index = Ratchet.index r;
           ek_ratchet = Ratchet.to_string r;
           ek_signing_key = t.signing_key;
         })

  let export_at t ~index =
    match find_ratchet t index with
    | None ->
        Error
          (Olm_error.Unknown_message_index
             { index; first_known = first_known_index t })
    | Some r -> Ok (export_of t r)

  let export_at_first_known_index t = export_of t t.initial_ratchet

  type pickle = {
    ratchet : string;
    index : int;
    signing_key : Ed25519.Public.t;
    signing_key_verified : bool;
    sender_key : Curve25519.Public.t;
    claimed_ed25519 : Ed25519.Public.t option;
    room_id : Room_id.t;
    creation_time : Ptime.t;
  }

  let to_pickle (t : t) =
    {
      ratchet = Ratchet.to_string t.initial_ratchet;
      index = Ratchet.index t.initial_ratchet;
      signing_key = t.signing_key;
      signing_key_verified = t.signing_key_verified;
      sender_key = t.sender_key;
      claimed_ed25519 = t.claimed_ed25519;
      room_id = t.room_id;
      creation_time = t.creation_time;
    }

  let of_pickle p =
    if String.length p.ratchet <> ratchet_length then
      Error
        (Olm_error.Bad_message_format "pickled Megolm ratchet is not 128 bytes")
    else
      let initial = Ratchet.of_bytes p.ratchet p.index in
      Ok
        {
          initial_ratchet = initial;
          latest_ratchet = Ratchet.copy initial;
          signing_key = p.signing_key;
          signing_key_verified = p.signing_key_verified;
          sender_key = p.sender_key;
          claimed_ed25519 = p.claimed_ed25519;
          room_id = p.room_id;
          creation_time = p.creation_time;
        }
end

module Outbound = struct
  type t = {
    room_id : Room_id.t;
    ratchet : Ratchet.t;
    signing_key : Ed25519.Private.t;
    signing_pub : Ed25519.Public.t;
    creation_time : Ptime.t;
    mutable message_count : int;
    rotation_messages : int;
    rotation_period : Ptime.Span.t;
    mutable shared_with : (User_id.t * Device_id.t) list;
  }

  (* The defaults [m.room.encryption] gives. *)
  let default_rotation_messages = 100
  let default_rotation_period = Ptime.Span.of_int_s (7 * 24 * 60 * 60)

  let of_parts ~room_id ~ratchet ~signing_key ~creation_time ~message_count
      ~rotation_messages ~rotation_period ~shared_with =
    {
      room_id;
      ratchet;
      signing_key;
      signing_pub = Ed25519.Private.public signing_key;
      creation_time;
      message_count;
      rotation_messages;
      rotation_period;
      shared_with;
    }

  let create ?(rotation_period = default_rotation_period)
      ?(rotation_messages = default_rotation_messages) ~random ~room_id () =
    let signing_key, _ = Ed25519.generate ~random () in
    of_parts ~room_id
      ~ratchet:(Ratchet.of_bytes (Random.generate random ratchet_length) 0)
      ~signing_key ~creation_time:(now ()) ~message_count:0 ~rotation_messages
      ~rotation_period ~shared_with:[]

  let signing_key t = t.signing_pub

  let session_id t =
    Session_id.of_string_exn (Ed25519.Public.to_base64 t.signing_pub)

  let room_id t = t.room_id
  let message_index t = Ratchet.index t.ratchet
  let message_count t = t.message_count
  let creation_time t = t.creation_time
  let rotation_period t = t.rotation_period
  let rotation_messages t = t.rotation_messages

  let needs_rotation t =
    t.message_count >= t.rotation_messages
    || Ptime.Span.compare
         (Ptime.diff (now ()) t.creation_time)
         t.rotation_period
       > 0

  let exported_key_at t index =
    {
      ek_index = index;
      ek_ratchet = Ratchet.to_string t.ratchet;
      ek_signing_key = t.signing_pub;
    }

  let session_key t =
    let signed =
      encode_exported_key ~version:2
        (exported_key_at t (Ratchet.index t.ratchet))
    in
    let signature = Ed25519.Private.sign t.signing_key signed in
    base64_encode (signed ^ Crypto_key.Signature.to_bytes signature)

  let exported_session_key t =
    base64_encode
      (encode_exported_key ~version:1
         (exported_key_at t (Ratchet.index t.ratchet)))

  let encrypt t plaintext =
    let cipher = Cipher.megolm (Ratchet.to_string t.ratchet) in
    let ct = Cipher.encrypt cipher plaintext in
    let index = Ratchet.index t.ratchet in
    let body = encode_message ~message_index:index ~ciphertext:ct in
    let signed = body ^ Cipher.mac8 cipher body in
    let signature = Ed25519.Private.sign t.signing_key signed in
    Ratchet.advance t.ratchet;
    t.message_count <- t.message_count + 1;
    {
      message_index = index;
      ciphertext =
        base64_encode (signed ^ Crypto_key.Signature.to_bytes signature);
    }

  let is_shared_with t ~user_id ~device_id =
    List.exists
      (fun (u, d) -> User_id.equal u user_id && Device_id.equal d device_id)
      t.shared_with

  let mark_shared_with t ~user_id ~device_id =
    if not (is_shared_with t ~user_id ~device_id) then
      t.shared_with <- (user_id, device_id) :: t.shared_with

  let shared_with t = t.shared_with

  type pickle = {
    room_id : Room_id.t;
    ratchet : string;
    index : int;
    signing_key : Ed25519.Private.t;
    creation_time : Ptime.t;
    message_count : int;
    rotation_messages : int;
    rotation_period : Ptime.Span.t;
    shared_with : (User_id.t * Device_id.t) list;
  }

  let to_pickle (t : t) =
    {
      room_id = t.room_id;
      ratchet = Ratchet.to_string t.ratchet;
      index = Ratchet.index t.ratchet;
      signing_key = t.signing_key;
      creation_time = t.creation_time;
      message_count = t.message_count;
      rotation_messages = t.rotation_messages;
      rotation_period = t.rotation_period;
      shared_with = t.shared_with;
    }

  let of_pickle p =
    if String.length p.ratchet <> ratchet_length then
      Error
        (Olm_error.Bad_message_format "pickled Megolm ratchet is not 128 bytes")
    else
      Ok
        (of_parts ~room_id:p.room_id
           ~ratchet:(Ratchet.of_bytes p.ratchet p.index)
           ~signing_key:p.signing_key ~creation_time:p.creation_time
           ~message_count:p.message_count ~rotation_messages:p.rotation_messages
           ~rotation_period:p.rotation_period ~shared_with:p.shared_with)
end
