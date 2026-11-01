module Ed25519 = Crypto_key.Ed25519
module Curve25519 = Crypto_key.Curve25519
module Room_id = Matrix_proto.Id.Room_id
module User_id = Matrix_proto.Id.User_id
module Device_id = Matrix_proto.Id.Device_id

(* A pickle is an opaque string as far as the session store's schema is
   concerned: nothing outside this module parses one, so the shape below is
   ours to choose. Matrix uses unpadded base64, and every field that holds key
   material or raw ratchet bytes goes through it, since JSON strings carry text
   rather than octets.

   The public half of a key pair is written out even though it is derivable
   from the secret, so that a pickle written by an earlier version still
   reads. *)

let ( let* ) = Result.bind
let b64 = Matrix_proto.Base64.encode

(* Session recency is an ordering key, so unlike general profile timestamps it
   must retain sub-second precision across a store round trip. *)
let ptime_precise : Ptime.t Jsont.t =
  Jsont.of_of_string ~kind:"RFC 3339 timestamp"
    ~enc:(fun value -> (Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0) value)
    (fun s ->
      match Ptime.of_rfc3339 s with
      | Ok (t, _, _) -> Ok t
      | Error (`RFC3339 (_, e)) ->
          Error (Format.asprintf "%a" Ptime.pp_rfc3339_error e))

(* Decoders report through jsont so that [decode_string] turns a bad pickle
   into an [Error], never an escaping exception. *)
let fail what = Jsont.Error.msg Jsont.Meta.none what

let decoded what = function
  | Ok v -> v
  | Error (`Msg m) -> fail (what ^ ": " ^ m)

let bytes_jsont : string Jsont.t =
  Jsont.map
    ~dec:(fun s -> decoded "invalid base64" (Matrix_proto.Base64.decode s))
    ~enc:b64 Matrix_proto.Json.Codec.string

let of_base64_jsont ~what ~of_base64 ~to_base64 =
  Jsont.map
    ~dec:(fun s -> decoded what (of_base64 s))
    ~enc:to_base64 Matrix_proto.Json.Codec.string

let ed25519_priv_jsont =
  of_base64_jsont ~what:"invalid Ed25519 private key"
    ~of_base64:(fun s ->
      Result.bind (Matrix_proto.Base64.decode s) Ed25519.Private.of_stored_bytes)
    ~to_base64:(fun k -> b64 (Ed25519.Private.to_bytes k))

let ed25519_pub_jsont =
  of_base64_jsont ~what:"invalid Ed25519 public key"
    ~of_base64:Ed25519.Public.of_base64 ~to_base64:Ed25519.Public.to_base64

let curve25519_secret_jsont =
  of_base64_jsont ~what:"invalid Curve25519 secret"
    ~of_base64:(fun s ->
      Result.bind (Matrix_proto.Base64.decode s) Curve25519.Secret.of_bytes)
    ~to_base64:(fun k -> b64 (Curve25519.Secret.to_bytes k))

let curve25519_pub_jsont =
  of_base64_jsont ~what:"invalid Curve25519 public key"
    ~of_base64:Curve25519.Public.of_base64
    ~to_base64:Curve25519.Public.to_base64

let id_jsont ~what ~of_string ~to_string =
  Jsont.map
    ~dec:(fun s -> decoded what (of_string s))
    ~enc:to_string Matrix_proto.Json.Codec.string

let room_id_jsont =
  id_jsont ~what:"invalid room id" ~of_string:Room_id.of_string
    ~to_string:Room_id.to_string

let stored_key_jsont : Olm.Account.stored_key Jsont.t =
  Jsont.Object.(
    map (fun key_id secret _public -> { Olm.Account.key_id; secret })
    |> mem "key_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (k : Olm.Account.stored_key) -> k.key_id)
    |> mem "secret" curve25519_secret_jsont
         ~enc:(fun (k : Olm.Account.stored_key) -> k.secret)
    |> mem "public" bytes_jsont ~enc:(fun (k : Olm.Account.stored_key) ->
        Curve25519.Public.to_bytes (Curve25519.Secret.public k.secret))
    |> finish)

let account_jsont : Olm.Account.pickle Jsont.t =
  Jsont.Object.(
    map
      (fun
        ed25519
        _ed25519_pub
        curve25519
        _curve25519_public
        stored_one_time_keys
        stored_fallback_key
        stored_previous_fallback_key
        next_key_id
        max_one_time_keys
      ->
        {
          Olm.Account.ed25519;
          curve25519;
          stored_one_time_keys;
          stored_fallback_key;
          stored_previous_fallback_key;
          next_key_id;
          max_one_time_keys;
        })
    |> mem "ed25519_priv" ed25519_priv_jsont
         ~enc:(fun (a : Olm.Account.pickle) -> a.ed25519)
    |> mem "ed25519_pub" ed25519_pub_jsont ~enc:(fun (a : Olm.Account.pickle) ->
        Ed25519.Private.public a.ed25519)
    |> mem "curve25519_secret" curve25519_secret_jsont
         ~enc:(fun (a : Olm.Account.pickle) -> a.curve25519)
    |> mem "curve25519_public" bytes_jsont ~enc:(fun (a : Olm.Account.pickle) ->
        Curve25519.Public.to_bytes (Curve25519.Secret.public a.curve25519))
    |> mem "one_time_keys"
         (Jsont.list stored_key_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (a : Olm.Account.pickle) -> a.stored_one_time_keys)
    |> opt_mem "fallback_key" stored_key_jsont
         ~enc:(fun (a : Olm.Account.pickle) -> a.stored_fallback_key)
    |> opt_mem "previous_fallback_key" stored_key_jsont
         ~enc:(fun (a : Olm.Account.pickle) -> a.stored_previous_fallback_key)
    |> mem "next_key_id" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun (a : Olm.Account.pickle) -> a.next_key_id)
    |> mem "max_one_time_keys" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 50)
           (* Matches [Olm.Account.create]'s own default; see olm_account.ml. *)
         ~enc:(fun (a : Olm.Account.pickle) -> a.max_one_time_keys)
    |> finish)

let chain_key_jsont : Olm.Session.chain_key Jsont.t =
  Jsont.Object.(
    map (fun key index -> { Olm.Session.key; index })
    |> mem "key" bytes_jsont ~enc:(fun (c : Olm.Session.chain_key) -> c.key)
    |> mem "index" Matrix_proto.Json.Codec.Legacy.int
         ~enc:(fun (c : Olm.Session.chain_key) -> c.index)
    |> finish)

type sending = {
  sd_kind : string;
  sd_root_key : string;
  sd_ratchet_secret : Curve25519.Secret.t option;
  sd_ratchet_public : Curve25519.Public.t option;
  sd_chain : Olm.Session.chain_key option;
  sd_their_ratchet_key : Curve25519.Public.t option;
}

let sending_jsont : sending Jsont.t =
  Jsont.Object.(
    map
      (fun
        sd_kind
        sd_root_key
        sd_ratchet_secret
        sd_ratchet_public
        sd_chain
        sd_their_ratchet_key
      ->
        {
          sd_kind;
          sd_root_key;
          sd_ratchet_secret;
          sd_ratchet_public;
          sd_chain;
          sd_their_ratchet_key;
        })
    |> mem "kind" Matrix_proto.Json.Codec.string ~enc:(fun s -> s.sd_kind)
    |> mem "root_key" bytes_jsont ~enc:(fun s -> s.sd_root_key)
    |> opt_mem "ratchet_secret" curve25519_secret_jsont ~enc:(fun s ->
        s.sd_ratchet_secret)
    |> opt_mem "ratchet_public" curve25519_pub_jsont ~enc:(fun s ->
        s.sd_ratchet_public)
    |> opt_mem "chain" chain_key_jsont ~enc:(fun s -> s.sd_chain)
    |> opt_mem "their_ratchet_key" curve25519_pub_jsont ~enc:(fun s ->
        s.sd_their_ratchet_key)
    |> finish)

let sending_of (s : Olm.Session.sending) =
  match s with
  | Olm.Session.Active a ->
      {
        sd_kind = "active";
        sd_root_key = a.root_key;
        sd_ratchet_secret = Some a.ratchet_secret;
        sd_ratchet_public = Some a.ratchet_public;
        sd_chain = Some a.chain;
        sd_their_ratchet_key = None;
      }
  | Olm.Session.Inactive i ->
      {
        sd_kind = "inactive";
        sd_root_key = i.root_key;
        sd_ratchet_secret = None;
        sd_ratchet_public = None;
        sd_chain = None;
        sd_their_ratchet_key = Some i.their_ratchet_key;
      }

let sending_to s : (Olm.Session.sending, string) result =
  match s.sd_kind with
  | "active" -> (
      match (s.sd_ratchet_secret, s.sd_ratchet_public, s.sd_chain) with
      | Some ratchet_secret, Some ratchet_public, Some chain ->
          Ok
            (Olm.Session.Active
               {
                 root_key = s.sd_root_key;
                 ratchet_secret;
                 ratchet_public;
                 chain;
               })
      | _ -> Error "an active sending ratchet is missing its keys")
  | "inactive" -> (
      match s.sd_their_ratchet_key with
      | Some their_ratchet_key ->
          Ok
            (Olm.Session.Inactive
               { root_key = s.sd_root_key; their_ratchet_key })
      | None -> Error "an inactive sending ratchet has no peer key")
  | k -> Error (Printf.sprintf "unknown sending ratchet kind %S" k)

let receiver_chain_jsont : Olm.Session.receiver_chain Jsont.t =
  Jsont.Object.(
    map (fun ratchet_key chain skipped ->
        { Olm.Session.ratchet_key; chain; skipped })
    |> mem "ratchet_key" curve25519_pub_jsont
         ~enc:(fun (c : Olm.Session.receiver_chain) -> c.ratchet_key)
    |> mem "chain" chain_key_jsont ~enc:(fun (c : Olm.Session.receiver_chain) ->
        c.chain)
    |> mem "skipped"
         (Jsont.list
            Jsont.Object.(
              map (fun index key -> (index, key))
              |> mem "index" Matrix_proto.Json.Codec.Legacy.int ~enc:fst
              |> mem "key" bytes_jsont ~enc:snd
              |> finish))
         ~dec_absent:(fun () -> [])
         ~enc:(fun (c : Olm.Session.receiver_chain) -> c.skipped)
    |> finish)

type olm_session = {
  ps_identity_key : Curve25519.Public.t;
  ps_base_key : Curve25519.Public.t;
  ps_one_time_key : Curve25519.Public.t;
  ps_their_identity_key : Curve25519.Public.t;
  ps_sending : sending;
  ps_receiving : Olm.Session.receiver_chain list;
  ps_creation_time : Ptime.t;
  ps_last_used_at : Ptime.t;
  ps_last_received_at : Ptime.t;
}

let olm_session_jsont : olm_session Jsont.t =
  Jsont.Object.(
    map
      (fun
        ps_identity_key
        ps_base_key
        ps_one_time_key
        ps_their_identity_key
        ps_sending
        ps_receiving
        ps_creation_time
        ps_last_used_at
        ps_last_received_at
      ->
        {
          ps_identity_key;
          ps_base_key;
          ps_one_time_key;
          ps_their_identity_key;
          ps_sending;
          ps_receiving;
          ps_creation_time;
          ps_last_used_at =
            Option.value ps_last_used_at ~default:ps_creation_time;
          ps_last_received_at =
            Option.value ps_last_received_at ~default:ps_creation_time;
        })
    |> mem "identity_key" curve25519_pub_jsont ~enc:(fun p -> p.ps_identity_key)
    |> mem "base_key" curve25519_pub_jsont ~enc:(fun p -> p.ps_base_key)
    |> mem "one_time_key" curve25519_pub_jsont ~enc:(fun p -> p.ps_one_time_key)
    |> mem "their_identity_key" curve25519_pub_jsont ~enc:(fun p ->
        p.ps_their_identity_key)
    |> mem "sending" sending_jsont ~enc:(fun p -> p.ps_sending)
    |> mem "receiving_chains"
         (Jsont.list receiver_chain_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun p -> p.ps_receiving)
    |> mem "creation_time" Json_codec.ptime ~enc:(fun p -> p.ps_creation_time)
    |> opt_mem "last_used_at" ptime_precise ~enc:(fun p ->
        Some p.ps_last_used_at)
    |> opt_mem "last_received_at" ptime_precise ~enc:(fun p ->
        Some p.ps_last_received_at)
    |> finish)

let of_session s =
  let p = Olm.Session.to_pickle s in
  {
    ps_identity_key = p.identity_key;
    ps_base_key = p.base_key;
    ps_one_time_key = p.one_time_key;
    ps_their_identity_key = p.their_identity_key;
    ps_sending = sending_of p.sending;
    ps_receiving = p.receiving_chains;
    ps_creation_time = p.creation_time;
    ps_last_used_at = Olm.Session.last_used_at s;
    ps_last_received_at = Olm.Session.last_received_at s;
  }

let to_session p =
  let* sending = sending_to p.ps_sending in
  Ok
    (Olm.Session.of_pickle ~last_used_at:p.ps_last_used_at
       ~last_received_at:p.ps_last_received_at
       {
         identity_key = p.ps_identity_key;
         base_key = p.ps_base_key;
         one_time_key = p.ps_one_time_key;
         their_identity_key = p.ps_their_identity_key;
         sending;
         receiving_chains = p.ps_receiving;
         creation_time = p.ps_creation_time;
       })

(* [claimed_ed25519] is absent as [""], which is what earlier versions wrote
   when the Olm plaintext carried no claim. *)
let claimed_ed25519_jsont =
  Jsont.map
    ~dec:(fun s ->
      if s = "" then None
      else
        Some
          (decoded "invalid claimed Ed25519 key" (Ed25519.Public.of_base64 s)))
    ~enc:(function None -> "" | Some k -> Ed25519.Public.to_base64 k)
    Matrix_proto.Json.Codec.string

let megolm_inbound_jsont : Olm.Megolm.Inbound.pickle Jsont.t =
  Jsont.Object.(
    map
      (fun
        ratchet
        index
        signing_key
        signing_key_verified
        sender_key
        claimed_ed25519
        room_id
        creation_time
      ->
        {
          Olm.Megolm.Inbound.ratchet;
          index;
          signing_key;
          signing_key_verified;
          sender_key;
          claimed_ed25519;
          room_id;
          creation_time;
        })
    |> mem "ratchet" bytes_jsont ~enc:(fun (p : Olm.Megolm.Inbound.pickle) ->
        p.ratchet)
    |> mem "index" Matrix_proto.Json.Codec.Legacy.int
         ~enc:(fun (p : Olm.Megolm.Inbound.pickle) -> p.index)
    |> mem "signing_key" ed25519_pub_jsont
         ~enc:(fun (p : Olm.Megolm.Inbound.pickle) -> p.signing_key)
    |> mem "signing_key_verified" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (p : Olm.Megolm.Inbound.pickle) -> p.signing_key_verified)
    |> mem "sender_key" curve25519_pub_jsont
         ~enc:(fun (p : Olm.Megolm.Inbound.pickle) -> p.sender_key)
    |> mem "claimed_ed25519" claimed_ed25519_jsont
         ~dec_absent:(fun () -> None)
         ~enc:(fun (p : Olm.Megolm.Inbound.pickle) -> p.claimed_ed25519)
    |> mem "room_id" room_id_jsont ~enc:(fun (p : Olm.Megolm.Inbound.pickle) ->
        p.room_id)
    |> mem "creation_time" Json_codec.ptime
         ~enc:(fun (p : Olm.Megolm.Inbound.pickle) -> p.creation_time)
    |> finish)

let shared_with_jsont : (User_id.t * Device_id.t) Jsont.t =
  Jsont.Object.(
    map (fun user device -> (user, device))
    |> mem "user_id" User_id.jsont ~enc:fst
    |> mem "device_id" Device_id.jsont ~enc:snd
    |> finish)

let megolm_outbound_jsont : Olm.Megolm.Outbound.pickle Jsont.t =
  Jsont.Object.(
    map
      (fun
        room_id
        ratchet
        index
        signing_key
        _signing_pub
        creation_time
        message_count
        rotation_messages
        rotation_period_s
        shared_with
      ->
        {
          Olm.Megolm.Outbound.room_id;
          ratchet;
          index;
          signing_key;
          creation_time;
          message_count;
          rotation_messages;
          rotation_period = Ptime.Span.of_int_s rotation_period_s;
          shared_with;
        })
    |> mem "room_id" room_id_jsont ~enc:(fun (q : Olm.Megolm.Outbound.pickle) ->
        q.room_id)
    |> mem "ratchet" bytes_jsont ~enc:(fun (q : Olm.Megolm.Outbound.pickle) ->
        q.ratchet)
    |> mem "index" Matrix_proto.Json.Codec.Legacy.int
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) -> q.index)
    |> mem "signing_priv" ed25519_priv_jsont
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) -> q.signing_key)
    |> mem "signing_pub" ed25519_pub_jsont
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) ->
           Ed25519.Private.public q.signing_key)
    |> mem "creation_time" Json_codec.ptime
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) -> q.creation_time)
    |> mem "message_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) -> q.message_count)
    |> mem "max_messages" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 100)
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) -> q.rotation_messages)
    |> mem "max_age_s" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 604_800)
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) ->
           int_of_float (Ptime.Span.to_float_s q.rotation_period))
    |> mem "shared_with"
         (Jsont.list shared_with_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (q : Olm.Megolm.Outbound.pickle) -> q.shared_with)
    |> finish)

type error = [ `Msg of string ]

let msg r = Result.map_error (fun m -> `Msg m) r
let olm_msg r = Result.map_error (Format.asprintf "%a" Olm.pp_error) r

let pickle_account a =
  msg (Jsont_bytesrw.encode_string account_jsont (Olm.Account.to_pickle a))

let unpickle_account s =
  msg
    (let* a = Jsont_bytesrw.decode_string account_jsont s in
     Ok (Olm.Account.of_pickle a))

let pickle_session s =
  msg (Jsont_bytesrw.encode_string olm_session_jsont (of_session s))

let unpickle_session s =
  msg
    (let* p = Jsont_bytesrw.decode_string olm_session_jsont s in
     to_session p)

let pickle_megolm_inbound s =
  msg
    (Jsont_bytesrw.encode_string megolm_inbound_jsont
       (Olm.Megolm.Inbound.to_pickle s))

let unpickle_megolm_inbound s =
  msg
    (let* p = Jsont_bytesrw.decode_string megolm_inbound_jsont s in
     olm_msg (Olm.Megolm.Inbound.of_pickle p))

let pickle_megolm_outbound s =
  msg
    (Jsont_bytesrw.encode_string megolm_outbound_jsont
       (Olm.Megolm.Outbound.to_pickle s))

let unpickle_megolm_outbound s =
  msg
    (let* p = Jsont_bytesrw.decode_string megolm_outbound_jsont s in
     olm_msg (Olm.Megolm.Outbound.of_pickle p))
