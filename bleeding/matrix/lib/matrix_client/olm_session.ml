open Olm_primitives
module Curve25519 = Crypto_key.Curve25519
module Public = Crypto_key.Curve25519.Public

let ( let* ) = Result.bind

(* vodozemac's limits, which are tighter than libolm's. *)
let max_message_gap = 2000
let max_message_keys = 40
let max_receiving_chains = 5

type message_type = Matrix_proto.Event.Olm_message_type.t = Pre_key | Normal
type message = { message_type : message_type; ciphertext : string }
type chain_key = { key : string; index : int }

type active_chain = {
  root_key : string;
  ratchet_secret : Curve25519.Secret.t;
  ratchet_public : Public.t;
  chain : chain_key;
}

type sending =
  | Active of active_chain
  | Inactive of { root_key : string; their_ratchet_key : Public.t }

type receiver_chain = {
  ratchet_key : Public.t;
  chain : chain_key;
  skipped : (int * string) list;
}

type session_keys = {
  identity_key : Public.t;
  base_key : Public.t;
  one_time_key : Public.t;
}

type live_chain = {
  rc_ratchet_key : Public.t;
  mutable rc_chain : chain_key;
  mutable rc_skipped : (int * string) list;
}

type t = {
  session_keys : session_keys;
  their_identity_key : Public.t;
  mutable sending : sending;
  mutable receiving_chains : live_chain list;
  creation_time : Ptime.t;
  mutable last_used_at : Ptime.t;
  mutable last_received_at : Ptime.t;
}

let session_id t =
  let k = t.session_keys in
  base64_encode
    (sha256
       (Public.to_bytes k.identity_key
       ^ Public.to_bytes k.base_key
       ^ Public.to_bytes k.one_time_key))

let their_identity_key t = t.their_identity_key
let creation_time t = t.creation_time
let last_used_at t = t.last_used_at
let last_received_at t = t.last_received_at
let has_received_message t = t.receiving_chains <> []

let dh ~secret ~public =
  match Curve25519.key_exchange ~secret ~public with
  | Ok s -> Ok s
  | Error (`Msg m) -> Error (Olm_error.Key_exchange_failed m)

(* Chain key advance and message key derivation (olm.md). *)
let message_key_of_chain ck = hmac_sha256 ~key:ck.key "\x01"

let advance_chain ck =
  { key = hmac_sha256 ~key:ck.key "\x02"; index = ck.index + 1 }

(* R_i || C_i,0 = HKDF(R_{i-1}, ECDH(T_{i-1}, T_i), "OLM_RATCHET", 64) *)
let advance_root ~root_key ~secret ~public =
  let* shared = dh ~secret ~public in
  let out = hkdf ~salt:root_key ~info:"OLM_RATCHET" ~ikm:shared 64 in
  Ok (String.sub out 0 32, String.sub out 32 32)

(* R_0 || C_0,0 = HKDF(0, S, "OLM_ROOT", 64) *)
let expand_shared_secret s =
  let out = hkdf ~salt:default_salt ~info:"OLM_ROOT" ~ikm:s 64 in
  (String.sub out 0 32, String.sub out 32 32)

let encode_message ~ratchet_key ~chain_index ~ciphertext =
  String.concat ""
    [
      String.make 1 version;
      Pb.tag_bytes ~tag:"\x0a" (Public.to_bytes ratchet_key);
      Pb.tag_varint "\x10" chain_index;
      Pb.tag_bytes ~tag:"\x22" ciphertext;
    ]

type parsed_message = {
  pm_body : string; (* everything the MAC covers *)
  pm_mac : string;
  pm_ratchet_key : Public.t;
  pm_chain_index : int;
  pm_ciphertext : string;
}

let parse_message bytes =
  let n = String.length bytes in
  if n < 10 then Error (Olm_error.Bad_message_format "Olm message too short")
  else if bytes.[0] <> version then
    Error (Olm_error.Bad_message_version (Char.code bytes.[0]))
  else begin
    let body = String.sub bytes 0 (n - 8) in
    let mac = String.sub bytes (n - 8) 8 in
    let* fields = Pb.parse (String.sub body 1 (String.length body - 1)) in
    let* ratchet_key = Pb.bytes fields 1 in
    let* chain_index = Pb.varint fields 2 ~default:0 in
    let* ciphertext = Pb.bytes fields 4 in
    match Public.of_bytes ratchet_key with
    | Error (`Msg _) -> Error (Olm_error.Bad_key "the message ratchet key")
    | Ok pm_ratchet_key ->
        Ok
          {
            pm_body = body;
            pm_mac = mac;
            pm_ratchet_key;
            pm_chain_index = chain_index;
            pm_ciphertext = ciphertext;
          }
  end

let encode_pre_key ~one_time_key ~base_key ~identity_key ~message =
  String.concat ""
    [
      String.make 1 version;
      Pb.tag_bytes ~tag:"\x0a" (Public.to_bytes one_time_key);
      Pb.tag_bytes ~tag:"\x12" (Public.to_bytes base_key);
      Pb.tag_bytes ~tag:"\x1a" (Public.to_bytes identity_key);
      Pb.tag_bytes ~tag:"\x22" message;
    ]

type parsed_pre_key = { pk_keys : session_keys; pk_message : string }

let parse_pre_key bytes =
  let n = String.length bytes in
  if n < 2 then
    Error (Olm_error.Bad_message_format "Olm pre-key message too short")
  else if bytes.[0] <> version then
    Error (Olm_error.Bad_message_version (Char.code bytes.[0]))
  else begin
    let* fields = Pb.parse (String.sub bytes 1 (n - 1)) in
    let* one_time_key = Pb.bytes fields 1 in
    let* base_key = Pb.bytes fields 2 in
    let* identity_key = Pb.bytes fields 3 in
    let* message = Pb.bytes fields 4 in
    let key what b =
      match Public.of_bytes b with
      | Ok k -> Ok k
      | Error (`Msg _) -> Error (Olm_error.Bad_key what)
    in
    let* one_time_key = key "the pre-key message one-time key" one_time_key in
    let* base_key = key "the pre-key message base key" base_key in
    let* identity_key = key "the pre-key message identity key" identity_key in
    Ok
      {
        pk_keys = { identity_key; base_key; one_time_key };
        pk_message = message;
      }
  end

let create_outbound ~random account ~their_identity_key ~their_one_time_key =
  let base_secret, base_public = Curve25519.generate ~random () in
  (* S = ECDH(I_A, E_B) || ECDH(E_A, I_B) || ECDH(E_A, E_B) *)
  let* dh1 = Olm_account.identity_exchange account their_one_time_key in
  let* dh2 = dh ~secret:base_secret ~public:their_identity_key in
  let* dh3 = dh ~secret:base_secret ~public:their_one_time_key in
  let root_key, chain_key = expand_shared_secret (dh1 ^ dh2 ^ dh3) in
  let created_at = now () in
  let ratchet_secret, ratchet_public = Curve25519.generate ~random () in
  Ok
    {
      session_keys =
        {
          identity_key = Olm_account.curve25519_key account;
          base_key = base_public;
          one_time_key = their_one_time_key;
        };
      their_identity_key;
      sending =
        Active
          {
            root_key;
            ratchet_secret;
            ratchet_public;
            chain = { key = chain_key; index = 0 };
          };
      receiving_chains = [];
      creation_time = created_at;
      last_used_at = created_at;
      last_received_at = created_at;
    }

let decrypt_with_message_key mk (m : parsed_message) =
  let cipher = Cipher.olm mk in
  if Cipher.verify_mac8 cipher ~msg:m.pm_body ~tag:m.pm_mac then
    Cipher.decrypt cipher m.pm_ciphertext
  else Error Olm_error.Bad_mac

(* [keep_last max xs] is the last [max] elements of [xs], oldest first. *)
let keep_last max xs =
  let n = List.length xs in
  if n > max then List.filteri (fun i _ -> i >= n - max) xs else xs

let push_skipped rc index key =
  rc.rc_skipped <- keep_last max_message_keys (rc.rc_skipped @ [ (index, key) ])

let decrypt_in_chain rc (m : parsed_message) =
  let target = m.pm_chain_index in
  let gap = target - rc.rc_chain.index in
  if gap > max_message_gap then
    Error (Olm_error.Message_gap_too_large { gap; max = max_message_gap })
  else if target < rc.rc_chain.index then (
    match List.assoc_opt target rc.rc_skipped with
    | None -> Error (Olm_error.No_message_key target)
    | Some mk ->
        let* plaintext = decrypt_with_message_key mk m in
        rc.rc_skipped <- List.filter (fun (i, _) -> i <> target) rc.rc_skipped;
        Ok plaintext)
  else begin
    (* Advance a scratch copy of the chain; only commit on success. *)
    let chain = ref rc.rc_chain in
    let skipped = ref [] in
    while !chain.index < target do
      if target - !chain.index <= max_message_keys then
        skipped := (!chain.index, message_key_of_chain !chain) :: !skipped;
      chain := advance_chain !chain
    done;
    let mk = message_key_of_chain !chain in
    let* plaintext = decrypt_with_message_key mk m in
    List.iter (fun (i, k) -> push_skipped rc i k) (List.rev !skipped);
    rc.rc_chain <- advance_chain !chain;
    Ok plaintext
  end

let push_receiver_chain t rc =
  t.receiving_chains <-
    keep_last max_receiving_chains (t.receiving_chains @ [ rc ])

(* Bring an inactive sending ratchet back to life by deriving the next root
   key and this side's new ratchet key T_i. *)
let activate ~random root_key their_ratchet_key =
  let ratchet_secret, ratchet_public = Curve25519.generate ~random () in
  let* new_root, chain_key =
    advance_root ~root_key ~secret:ratchet_secret ~public:their_ratchet_key
  in
  Ok
    {
      root_key = new_root;
      ratchet_secret;
      ratchet_public;
      chain = { key = chain_key; index = 0 };
    }

(* The sending ratchet's parameters, activating it first if the other side
   has moved it on. Pure: every caller commits whatever it decides [t.sending]
   should become next, once it knows the message that prompted the read has
   verified. *)
let active_sending ~random t =
  match t.sending with
  | Active a -> Ok a
  | Inactive { root_key; their_ratchet_key } ->
      activate ~random root_key their_ratchet_key

let decrypt_parsed ~random t (m : parsed_message) =
  match
    List.find_opt
      (fun rc -> Public.equal rc.rc_ratchet_key m.pm_ratchet_key)
      t.receiving_chains
  with
  | Some rc -> decrypt_in_chain rc m
  | None ->
      (* A new chain from the other side: turn the crank on the DH ratchet.
         Nothing here mutates [t] until [decrypt_in_chain] has verified the
         message's MAC, so a forged message with an unrecognised ratchet key
         leaves the session untouched rather than burning the sending
         ratchet. *)
      let* a = active_sending ~random t in
      let* new_root, chain_key =
        advance_root ~root_key:a.root_key ~secret:a.ratchet_secret
          ~public:m.pm_ratchet_key
      in
      let rc =
        {
          rc_ratchet_key = m.pm_ratchet_key;
          rc_chain = { key = chain_key; index = 0 };
          rc_skipped = [];
        }
      in
      let* plaintext = decrypt_in_chain rc m in
      t.sending <-
        Inactive { root_key = new_root; their_ratchet_key = m.pm_ratchet_key };
      push_receiver_chain t rc;
      Ok plaintext

let create_inbound account ~their_identity_key ~ciphertext =
  let* bytes = base64_decode ciphertext ~what:"the pre-key message" in
  let* pre = parse_pre_key bytes in
  if not (Public.equal pre.pk_keys.identity_key their_identity_key) then
    Error Olm_error.Identity_key_mismatch
  else
    let otk = pre.pk_keys.one_time_key in
    (* S = ECDH(E_B, I_A) || ECDH(I_B, E_A) || ECDH(E_B, E_A) *)
    let* dh1 =
      Olm_account.one_time_key_exchange account ~key:otk
        ~peer:pre.pk_keys.identity_key
    in
    let* dh2 = Olm_account.identity_exchange account pre.pk_keys.base_key in
    let* dh3 =
      Olm_account.one_time_key_exchange account ~key:otk
        ~peer:pre.pk_keys.base_key
    in
    let root_key, chain_key = expand_shared_secret (dh1 ^ dh2 ^ dh3) in
    let* m = parse_message pre.pk_message in
    let rc =
      {
        rc_ratchet_key = m.pm_ratchet_key;
        rc_chain = { key = chain_key; index = 0 };
        rc_skipped = [];
      }
    in
    let created_at = now () in
    let t =
      {
        session_keys = pre.pk_keys;
        their_identity_key;
        sending = Inactive { root_key; their_ratchet_key = m.pm_ratchet_key };
        receiving_chains = [ rc ];
        creation_time = created_at;
        last_used_at = created_at;
        last_received_at = created_at;
      }
    in
    let* plaintext = decrypt_in_chain rc m in
    Olm_account.consume_one_time_key account otk;
    t.last_used_at <- now ();
    t.last_received_at <- t.last_used_at;
    Ok (t, plaintext)

let encrypt ~random t plaintext =
  let* a = active_sending ~random t in
  let chain = a.chain in
  let mk = message_key_of_chain chain in
  t.sending <- Active { a with chain = advance_chain chain };
  let cipher = Cipher.olm mk in
  let ct = Cipher.encrypt cipher plaintext in
  let body =
    encode_message ~ratchet_key:a.ratchet_public ~chain_index:chain.index
      ~ciphertext:ct
  in
  let body = body ^ Cipher.mac8 cipher body in
  let message =
    if has_received_message t then
      { message_type = Normal; ciphertext = base64_encode body }
    else
      let pre =
        encode_pre_key ~one_time_key:t.session_keys.one_time_key
          ~base_key:t.session_keys.base_key
          ~identity_key:t.session_keys.identity_key ~message:body
      in
      { message_type = Pre_key; ciphertext = base64_encode pre }
  in
  t.last_used_at <- now ();
  Ok message

let decrypt ~random t message =
  let* bytes = base64_decode message.ciphertext ~what:"the Olm message" in
  let* m =
    match message.message_type with
    | Pre_key ->
        let* pre = parse_pre_key bytes in
        parse_message pre.pk_message
    | Normal -> parse_message bytes
  in
  let* plaintext = decrypt_parsed ~random t m in
  t.last_used_at <- now ();
  t.last_received_at <- t.last_used_at;
  Ok plaintext

type pickle = {
  identity_key : Public.t;
  base_key : Public.t;
  one_time_key : Public.t;
  their_identity_key : Public.t;
  sending : sending;
  receiving_chains : receiver_chain list;
  creation_time : Ptime.t;
}

let to_pickle t =
  {
    identity_key = t.session_keys.identity_key;
    base_key = t.session_keys.base_key;
    one_time_key = t.session_keys.one_time_key;
    their_identity_key = t.their_identity_key;
    sending = t.sending;
    receiving_chains =
      List.map
        (fun rc : receiver_chain ->
          {
            ratchet_key = rc.rc_ratchet_key;
            chain = rc.rc_chain;
            skipped = rc.rc_skipped;
          })
        t.receiving_chains;
    creation_time = t.creation_time;
  }

let of_pickle ?last_used_at ?last_received_at p =
  {
    session_keys =
      {
        identity_key = p.identity_key;
        base_key = p.base_key;
        one_time_key = p.one_time_key;
      };
    their_identity_key = p.their_identity_key;
    sending = p.sending;
    receiving_chains =
      keep_last max_receiving_chains
        (List.map
           (fun (rc : receiver_chain) : live_chain ->
             {
               rc_ratchet_key = rc.ratchet_key;
               rc_chain = rc.chain;
               rc_skipped = keep_last max_message_keys rc.skipped;
             })
           p.receiving_chains);
    creation_time = p.creation_time;
    last_used_at = Option.value last_used_at ~default:p.creation_time;
    last_received_at = Option.value last_received_at ~default:p.creation_time;
  }
