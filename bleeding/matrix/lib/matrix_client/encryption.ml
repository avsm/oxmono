let src =
  Logs.Src.create "matrix.encryption" ~doc:"Matrix end-to-end encryption"

module Log = (val Logs.src_log src : Logs.LOG)
module Uid = Matrix_proto.Id.User_id
module Rid = Matrix_proto.Id.Room_id
module Did = Matrix_proto.Id.Device_id
module Server_name = Matrix_proto.Id.Server_name
module Sid = Matrix_proto.Id.Session_id
module Ed25519 = Crypto_key.Ed25519
module Curve25519 = Crypto_key.Curve25519
module Key_id = Crypto_key.Key_id
module Ev = Matrix_proto.Event
module Cross_signing = Cross_signing
module String_set = Set.Make (String)

let ( let* ) = Result.bind

type trust = Unverified | Verified | Blacklisted

type utd_cause =
  | Unknown
  | Sent_before_we_joined
  | Verification_violation
  | Unsigned_device
  | Unknown_device
  | Historical_message_and_backup_is_disabled
  | Withheld_for_unverified_or_insecure_device
  | Withheld_by_sender
  | Historical_message_and_device_is_unverified

type utd_context = {
  device_created_at : Ptime.t option;
  backup_exists : bool;
  backup_configured : bool;
  local_device_verified : bool;
  withheld : withheld option;
}

and withheld = {
  room_id : Rid.t;
  session_id : Sid.t;
  code : string;
  reason : string option;
  sender_key : string option;
  from_device : Did.t option;
  (* The user whose authenticated event carried this withholding evidence.
     This is provenance, not part of the room-key-withheld wire content. *)
  sender_user : Uid.t option;
}

type pending_key_bundle = {
  room_id : Rid.t;
  inviter : Uid.t;
  invite_accepted_at : Ptime.t;
}

type room_key_bundle_content = {
  room_id : Rid.t;
  file : Matrix_proto.Event.Media_message_content.encrypted_file;
}

type received_key_bundle = {
  room_id : Rid.t;
  sender : Uid.t;
  sender_key : Curve25519.Public.t;
  sender_ed25519 : string;
  file : Matrix_proto.Event.Media_message_content.encrypted_file;
}

let default_utd_context =
  {
    device_created_at = None;
    backup_exists = false;
    backup_configured = false;
    local_device_verified = false;
    withheld = None;
  }

type trust_requirement = Untrusted | Cross_signed_or_legacy | Cross_signed

type identity_status =
  | Identity_unverified
  | Identity_verified
  | Verification_violation

type identity = {
  identity_user_id : Uid.t;
  identity_master_key : string;
  identity_self_signing_key : string;
  identity_user_signing_key : string option;
  identity_was_previously_verified : bool;
  identity_status : identity_status;
  identity_pinned_master_key : string;
}

type identity_change = { user_id : Uid.t; status : identity_status }

type device = {
  user_id : Uid.t;
  device_id : Did.t;
  algorithms : string list;
  keys : (Key_id.t * string) list;
  signatures : Keys.signatures;
  dehydrated : bool option;
  trust : trust;
}

let device_key (d : device) ~algorithm =
  let key_id = Key_id.of_device ~algorithm d.device_id in
  Option.map snd (List.find_opt (fun (k, _) -> Key_id.equal k key_id) d.keys)

let device_ed25519 d =
  Option.bind (device_key d ~algorithm:"ed25519") (fun s ->
      Result.to_option (Ed25519.Public.of_base64 s))

let device_curve25519 d =
  Option.bind (device_key d ~algorithm:"curve25519") (fun s ->
      Result.to_option (Curve25519.Public.of_base64 s))

type room_settings = {
  algorithm : string;
  rotation_period_ms : int64;
  rotation_period_msgs : int;
}

let megolm_algorithm = Ev.Encryption_algorithm.(to_string Megolm_v1_aes_sha2)

let olm_algorithm =
  Ev.Encryption_algorithm.(to_string Olm_v1_curve25519_aes_sha2)

(* One week and 100 messages, the defaults the spec gives for
   [m.room.encryption]. *)
let default_room_settings =
  {
    algorithm = megolm_algorithm;
    rotation_period_ms = 604_800_000L;
    rotation_period_msgs = 100;
  }

(* [algorithm] is required, so that an [m.room.encryption] which names none is
   refused rather than taken for a Megolm room. *)
let encryption_content_jsont : room_settings Jsont.t =
  Jsont.Object.(
    map (fun algorithm rotation_period_ms rotation_period_msgs ->
        { algorithm; rotation_period_ms; rotation_period_msgs })
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~enc:(fun (r : room_settings) -> r.algorithm)
    |> mem "rotation_period_ms" Matrix_proto.Json.Codec.int64
         ~dec_absent:(fun () -> default_room_settings.rotation_period_ms)
         ~enc:(fun (r : room_settings) -> r.rotation_period_ms)
    |> mem "rotation_period_msgs" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> default_room_settings.rotation_period_msgs)
         ~enc:(fun (r : room_settings) -> r.rotation_period_msgs)
    |> finish)

let enable_room_encryption ?rotation_period_ms ?rotation_period_msgs () =
  {
    algorithm = megolm_algorithm;
    rotation_period_ms =
      Option.value rotation_period_ms
        ~default:default_room_settings.rotation_period_ms;
    rotation_period_msgs =
      Option.value rotation_period_msgs
        ~default:default_room_settings.rotation_period_msgs;
  }

let decode_json codec j =
  match Jsont.Json.decode codec j with
  | Ok v -> Ok v
  | Error m -> Error (Error.Json_error m)

let encode_json codec v =
  match Jsont.Json.encode codec v with
  | Ok j -> Ok j
  | Error m -> Error (Error.Json_error m)

(* Every record here encodes, so the encoder's error branch is unreachable. *)
let json_of codec v =
  match Jsont.Json.encode codec v with
  | Ok j -> j
  | Error _ -> Jsont.Json.null ()

let room_encryption_content s = json_of encryption_content_jsont s

module Session_meta = struct
  type t = {
    room_id : Rid.t;
    session_id : Sid.t;
    forwarding_chain : string list;
    sender : Uid.t option;
    sender_ed25519 : string option;
    shared_history : bool;
    legacy : bool;
  }
end

(* Forwarding chains are public provenance, but they are still typed
   Curve25519 keys on the Matrix wire. Keep only canonical encodings in the
   in-memory metadata so old stores and imported data cannot be re-emitted as
   arbitrary strings. Matrix and matrix-rust-sdk specify no small chain limit,
   but the direct import and snapshot APIs have no HTTP body boundary, so cap
   retained provenance well above any plausible device-forwarding path. *)
let max_forwarding_chain_length = 100

let canonical_forwarding_chain chain =
  if List.length chain > max_forwarding_chain_length then
    Error
      (Printf.sprintf "forwarding key chain exceeds %d entries"
         max_forwarding_chain_length)
  else
    let rec loop index acc = function
      | [] -> Ok (List.rev acc)
      | encoded :: rest -> (
          match Curve25519.Public.of_base64 encoded with
          | Error (`Msg message) ->
              Error
                (Printf.sprintf "invalid forwarding key at index %d: %s" index
                   message)
          | Ok key ->
              loop (index + 1) (Curve25519.Public.to_base64 key :: acc) rest)
    in
    loop 0 [] chain

let sanitize_session_metadata metadata =
  List.filter_map
    (fun (m : Session_meta.t) ->
      match canonical_forwarding_chain m.forwarding_chain with
      | Error _ -> None
      | Ok forwarding_chain -> Some { m with forwarding_chain })
    metadata

type backup_state = {
  version : string option;
  encryption_key : Backup.encryption_key option;
  decryption_key : Backup.Decryption_key.t option;
  backed_up : (Rid.t * Sid.t) list;
  room_key_backups_fully_downloaded : Rid.t list;
}

type outbound_withheld = {
  ow_room_id : Rid.t;
  ow_session_id : Sid.t;
  ow_user_id : Uid.t;
  ow_device_id : Did.t;
  ow_code : string;
  ow_txn_id : string;
  ow_content : Jsont.json;
  ow_sent : bool;
}

type secret_cancel = {
  sc_txn_id : string;
  sc_content : Jsont.json;
  sc_sent : bool;
  sc_messages : To_device.messages option;
}

type secret_request = {
  sr_name : string;
  sr_request_id : string;
  sr_txn_id : string;
  sr_content : Jsont.json;
  sr_sent : bool;
  sr_messages : To_device.messages option;
  sr_cancel : secret_cancel option;
}

type secret_send = {
  ss_request_id : string;
  ss_user_id : Uid.t;
  ss_device_id : Did.t;
  ss_txn_id : string;
  ss_content : Jsont.json;
  ss_sent : bool;
  ss_messages : To_device.messages option;
}

type state = {
  devices : device list;
  tracked_users : Uid.t list;
  outdated_users : Uid.t list;
  rooms : (Rid.t * room_settings) list;
  backup : backup_state;
  session_meta : Session_meta.t list;
  trust_requirement : trust_requirement;
  identities : identity list;
  withheld : withheld list;
  pending_key_bundles : pending_key_bundle list;
  received_key_bundles : received_key_bundle list;
  outbound_withheld : outbound_withheld list;
  secrets : (string * string) list;
  secret_requests : secret_request list;
  secret_sends : secret_send list;
  fallback_key_created_at : Ptime.t option;
  fallback_key_pending : bool;
  published_one_time_keys : string list;
  uploaded_one_time_key_count : int;
  device_keys_uploaded : bool;
  dehydrated_pickle_key : string option;
  last_uploaded_device_id : Did.t option;
}

let empty_backup =
  {
    version = None;
    encryption_key = None;
    decryption_key = None;
    backed_up = [];
    room_key_backups_fully_downloaded = [];
  }

let empty_state =
  {
    devices = [];
    tracked_users = [];
    outdated_users = [];
    rooms = [];
    backup = empty_backup;
    session_meta = [];
    trust_requirement = Untrusted;
    identities = [];
    withheld = [];
    pending_key_bundles = [];
    received_key_bundles = [];
    outbound_withheld = [];
    secrets = [];
    secret_requests = [];
    secret_sends = [];
    fallback_key_created_at = None;
    fallback_key_pending = false;
    published_one_time_keys = [];
    uploaded_one_time_key_count = 0;
    device_keys_uploaded = false;
    dehydrated_pickle_key = None;
    last_uploaded_device_id = None;
  }

type snapshot = {
  account : Olm.Account.t;
  olm_sessions : Olm.Session.t list;
  megolm_inbound : Olm.Megolm.Inbound.t list;
  megolm_outbound : Olm.Megolm.Outbound.t list;
  state : state;
}

(* The device-keys object signed by [/keys/upload], in canonical form.

   @see <https://spec.matrix.org/v1.11/appendices/#canonical-json> *)
let canonical_device_keys ?dehydrated ~user_id ~device_id ~algorithms ~keys () =
  let jstring = Jsont.Json.string in
  let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v in
  let fields =
    [
      jmem "algorithms" (Jsont.Json.list (List.map jstring algorithms));
      jmem "device_id" (jstring (Did.to_string device_id));
      jmem "keys"
        (Jsont.Json.object'
           (List.map (fun (k, v) -> jmem (Key_id.to_string k) (jstring v)) keys));
      jmem "user_id" (jstring (Uid.to_string user_id));
    ]
  in
  let fields =
    match dehydrated with
    | None -> fields
    | Some value -> jmem "dehydrated" (Jsont.Json.bool value) :: fields
  in
  Matrix_proto.Signed_json.canonical_json (Jsont.Json.object' fields)

(* The signature a device makes over its own keys, named [ed25519:<device>]. *)
let device_key_id device_id = Key_id.of_device ~algorithm:"ed25519" device_id

let find_signature (signatures : Keys.signatures) ~user_id ~key_id =
  Option.bind
    (List.find_opt (fun (u, _) -> Uid.equal u user_id) signatures)
    (fun (_, sigs) ->
      Option.map snd (List.find_opt (fun (k, _) -> Key_id.equal k key_id) sigs))

let validate_sender_device_keys ~sender ~sender_key ~sender_ed25519 json =
  let bad reason = Error reason in
  match Jsont.Json.decode Keys.device_keys_jsont json with
  | Error msg -> bad msg
  | Ok keys -> (
      let ed25519_id = Key_id.of_device ~algorithm:"ed25519" keys.device_id in
      let curve25519_id =
        Key_id.of_device ~algorithm:"curve25519" keys.device_id
      in
      let key id = List.assoc_opt id keys.keys in
      let device_id = Did.to_string keys.device_id in
      if not (Uid.equal keys.user_id sender) then
        bad "sender_device_keys user_id does not match sender"
      else if
        not
          (List.for_all
             (fun (key_id, _) -> String.equal (Key_id.id key_id) device_id)
             keys.keys)
      then bad "sender_device_keys has incoherent key identifiers"
      else
        match (key ed25519_id, key curve25519_id) with
        | None, _ | _, None ->
            bad "sender_device_keys is missing an identity key"
        | Some ed, Some curve -> (
            match
              ( Ed25519.Public.of_base64 ed,
                Curve25519.Public.of_base64 curve,
                find_signature keys.signatures ~user_id:keys.user_id
                  ~key_id:ed25519_id )
            with
            | Error (`Msg m), _, _ -> bad m
            | _, Error (`Msg m), _ -> bad m
            | _, _, None -> bad "sender_device_keys has no self-signature"
            | Ok ed, Ok curve, Some signature ->
                let canonical =
                  canonical_device_keys ~user_id:keys.user_id
                    ~device_id:keys.device_id ~algorithms:keys.algorithms
                    ~keys:keys.keys ?dehydrated:keys.dehydrated ()
                in
                if not (Ed25519.Public.verify ed ~signature ~data:canonical)
                then bad "sender_device_keys self-signature is invalid"
                else if not (Ed25519.Public.equal ed sender_ed25519) then
                  bad "sender_device_keys Ed25519 key does not match Olm"
                else if not (Curve25519.Public.equal curve sender_key) then
                  bad "sender_device_keys Curve25519 key does not match Olm"
                else Ok keys))

type to_device_raw = {
  td_type : string;
  td_sender : string;
  td_content : Jsont.json;
}

let to_device_raw_jsont : to_device_raw Jsont.t =
  Jsont.Object.(
    map (fun td_type td_sender td_content -> { td_type; td_sender; td_content })
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.td_type)
    |> mem "sender" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.td_sender)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~dec_absent:(fun () -> Jsont.Json.null ())
         ~enc:(fun t -> t.td_content)
    |> finish)

(* [m.room_key_request]. *)
type requested_key_info = {
  rq_algorithm : string;
  rq_room_id : string;
  rq_session_id : string;
  rq_sender_key : string option;
}

let requested_key_info_jsont : requested_key_info Jsont.t =
  Jsont.Object.(
    map (fun rq_algorithm rq_room_id rq_session_id rq_sender_key ->
        { rq_algorithm; rq_room_id; rq_session_id; rq_sender_key })
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> megolm_algorithm)
         ~enc:(fun t -> t.rq_algorithm)
    |> mem "room_id" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.rq_room_id)
    |> mem "session_id" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.rq_session_id)
    |> opt_mem "sender_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.rq_sender_key)
    |> finish)

type room_key_request_content = {
  kr_action : string;
  kr_requesting_device_id : string;
  kr_request_id : string;
  kr_body : requested_key_info option;
}

let room_key_request_content_jsont : room_key_request_content Jsont.t =
  Jsont.Object.(
    map (fun kr_action kr_requesting_device_id kr_request_id kr_body ->
        { kr_action; kr_requesting_device_id; kr_request_id; kr_body })
    |> mem "action" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "request")
         ~enc:(fun t -> t.kr_action)
    |> mem "requesting_device_id" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.kr_requesting_device_id)
    |> mem "request_id" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.kr_request_id)
    |> opt_mem "body" requested_key_info_jsont ~enc:(fun t -> t.kr_body)
    |> finish)

type encrypted_file = Matrix_proto.Event.Media_message_content.encrypted_file

let encrypted_file_jsont : encrypted_file Jsont.t =
  Jsont.Object.(
    map (fun url key iv hashes v ->
        { Matrix_proto.Event.Media_message_content.url; key; iv; hashes; v })
    |> mem "url" Matrix_proto.Json.Codec.string
         ~enc:(fun (f : encrypted_file) -> f.url)
    |> mem "key" Matrix_proto.Json.Codec.json ~enc:(fun (f : encrypted_file) ->
        f.key)
    |> mem "iv" Matrix_proto.Json.Codec.string ~enc:(fun (f : encrypted_file) ->
        f.iv)
    |> mem "hashes" (Json_codec.string_map Matrix_proto.Json.Codec.string)
         ~enc:(fun (f : encrypted_file) -> f.hashes)
    |> mem "v" Matrix_proto.Json.Codec.string ~enc:(fun (f : encrypted_file) ->
        f.v)
    |> finish)

let room_key_bundle_content_jsont : room_key_bundle_content Jsont.t =
  Jsont.Object.(
    map (fun room_id file -> { room_id; file })
    |> mem "room_id" Rid.jsont ~enc:(fun (c : room_key_bundle_content) ->
        c.room_id)
    |> mem "file" encrypted_file_jsont
         ~enc:(fun (c : room_key_bundle_content) -> c.file)
    |> finish)

(* Just the [algorithm] of an event content, so that an algorithm we do not
   implement can be reported as such. *)
let algorithm_of_content_jsont : string Jsont.t =
  Jsont.Object.(
    map Fun.id
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:Fun.id
    |> finish)

(* The [room_id] inside the plaintext is what binds an event to the room it
   arrived in; see [decrypt_room_event]. *)
type megolm_payload = {
  mp_room_id : string;
  mp_type : string;
  mp_content : Jsont.json;
}

let megolm_payload_jsont : megolm_payload Jsont.t =
  Jsont.Object.(
    map (fun mp_room_id mp_type mp_content ->
        { mp_room_id; mp_type; mp_content })
    |> mem "room_id" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.mp_room_id)
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.mp_type)
    |> mem "content" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.mp_content)
    |> finish)

type request =
  | Keys_upload of {
      device_keys : Keys.device_keys option;
      one_time_keys : (Key_id.t * Keys.one_time_key) list;
      fallback_keys : (Key_id.t * Keys.one_time_key) list;
    }
  | Keys_query of Uid.t list
  | Keys_claim of (Uid.t * (Did.t * string) list) list
  | To_device of {
      event_type : string;
      txn_id : string;
      messages : To_device.messages;
    }
  | Room_key_share of {
      room_id : Rid.t;
      session_id : Sid.t;
      txn_id : string;
      messages : To_device.messages;
    }
  | Room_key_bundle_share of {
      room_id : Rid.t;
      txn_id : string;
      messages : To_device.messages;
    }
  | Room_keys_upload of { version : string; rooms : Backup.rooms }

let count_messages messages =
  List.fold_left (fun n (_, targets) -> n + List.length targets) 0 messages

let pp_request ppf = function
  | Keys_upload { device_keys; one_time_keys; fallback_keys } ->
      Format.fprintf ppf
        "keys upload (device keys: %b, %d one-time keys, %d fallback keys)"
        (device_keys <> None)
        (List.length one_time_keys)
        (List.length fallback_keys)
  | Keys_query users ->
      Format.fprintf ppf "keys query for %d user(s)" (List.length users)
  | Keys_claim keys ->
      Format.fprintf ppf "keys claim for %d user(s)" (List.length keys)
  | To_device { event_type; messages; _ } ->
      Format.fprintf ppf "%s to %d device(s)" event_type
        (count_messages messages)
  | Room_key_share { room_id; session_id; messages; _ } ->
      Format.fprintf ppf "room key %a for %a to %d device(s)" Sid.pp session_id
        Rid.pp room_id (count_messages messages)
  | Room_key_bundle_share { room_id; messages; _ } ->
      Format.fprintf ppf "room-key bundle for %a to %d device(s)" Rid.pp room_id
        (count_messages messages)
  | Room_keys_upload { version; rooms } ->
      Format.fprintf ppf "backup upload to version %s of %d room(s)" version
        (List.length rooms)

type verification_state =
  | Verified_device
  | Unverified_device
  | Unknown_device
  | Device_info
  | Sender_unverified
  | Sender_verified
  | Verification_violation

type to_device_event =
  | Room_key of {
      room_id : Rid.t;
      session_id : Sid.t;
      sender : Uid.t;
      sender_key : Curve25519.Public.t;
    }
  | Forwarded_room_key of {
      room_id : Rid.t;
      session_id : Sid.t;
      sender : Uid.t;
      sender_key : string;
      forwarding_chain : string list;
    }
  | Room_key_bundle of received_key_bundle
  | Room_key_request of {
      sender : Uid.t;
      requesting_device_id : string;
      request_id : string;
      room_id : string;
      session_id : string;
      action : string;
      answered : bool;
    }
  | Verification of {
      event_type : string;
      sender : Uid.t;
      sender_device : Did.t option;
      content : Jsont.json;
    }
  | Secret_request of { sender : Uid.t; content : Jsont.json }
  | Secret_send of { sender : Uid.t; content : Jsont.json }
  | Undecryptable of { sender : Uid.t; reason : string }
  | Ignored of { event_type : string; reason : string }

let pp_to_device_event ppf = function
  | Room_key { room_id; session_id; sender; _ } ->
      Format.fprintf ppf "room key %a for %a from %a" Sid.pp session_id Rid.pp
        room_id Uid.pp sender
  | Forwarded_room_key { room_id; session_id; sender; _ } ->
      Format.fprintf ppf "forwarded room key %a for %a from %a" Sid.pp
        session_id Rid.pp room_id Uid.pp sender
  | Room_key_bundle { room_id; sender; _ } ->
      Format.fprintf ppf "room-key bundle for %a from %a" Rid.pp room_id Uid.pp
        sender
  | Room_key_request { sender; action; answered; _ } ->
      Format.fprintf ppf "room key %s from %a, answered: %b" action Uid.pp
        sender answered
  | Verification { event_type; sender; _ } ->
      Format.fprintf ppf "%s from %a" event_type Uid.pp sender
  | Secret_request { sender; _ } ->
      Format.fprintf ppf "m.secret.request from %a" Uid.pp sender
  | Secret_send { sender; _ } ->
      Format.fprintf ppf "m.secret.send from %a" Uid.pp sender
  | Undecryptable { sender; reason } ->
      Format.fprintf ppf "undecryptable event from %a: %s" Uid.pp sender reason
  | Ignored { event_type; reason } ->
      Format.fprintf ppf "ignored %s: %s" event_type reason

type outcome = {
  requests : request list;
  events : to_device_event list;
  changed_users : Uid.t list;
  left_users : Uid.t list;
  new_sessions : (Rid.t * Sid.t) list;
}

type outbound_session = {
  os_session : Olm.Megolm.Outbound.t;
  os_created : Ptime.t;
  os_members : Uid.t list;
}

(* A device whose existing Olm session could not decrypt a message.  This is
   deliberately kept outside [state]: it is a short-lived repair hint, not
   protocol state, and a failed decrypt must never make a snapshot fail to
   load.  A successful claim clears it before the replacement dummy is
   queued. *)
type wedged_device = { wd_user_id : Uid.t; wd_device_id : Did.t }

(* A successful [/keys/claim] response may omit a requested device when the
   homeserver has no one-time keys left for it.  Keep this repair hint
   transient, as Rust's [FailuresCache] does: retry after 15 seconds, then
   exponentially back off up to 15 minutes. *)
type claim_failure = {
  cf_user_id : Uid.t;
  cf_device_id : Did.t;
  cf_until : Ptime.t;
  cf_count : int;
}

type t = {
  random : Random.t;
  our_user : Uid.t;
  our_device : Did.t;
  machine : Olm.Machine.t;
  mutable devices : device list;
  mutable tracked : Uid.t list;
  mutable outdated : Uid.t list;
  mutable rooms : (Rid.t * room_settings) list;
  mutable backup : backup_state;
  mutable metas : Session_meta.t list;
  mutable identities : identity list;
  mutable identity_changes : identity_change list;
  mutable trust_requirement : trust_requirement;
  mutable withheld : withheld list;
  mutable pending_key_bundles : pending_key_bundle list;
  mutable received_key_bundles : received_key_bundle list;
  mutable published_otks : string list;
  mutable keys_uploaded : bool;
  mutable fallback_pending : bool;
  mutable fallback_key_created_at : Ptime.t option;
  mutable dehydrated_pickle_key : string option;
  mutable last_uploaded_device_id : Did.t option;
  mutable server_otk_count : int;
  mutable outbound_withhelds : outbound_withheld list;
  mutable secret_values : (string * string) list;
  mutable secret_request_state : secret_request list;
  mutable secret_send_state : secret_send list;
  mutable inbound : ((Rid.t * Sid.t) * Olm.Megolm.Inbound.t) list;
  mutable outbound : (Rid.t * outbound_session) list;
  mutable wedged : wedged_device list;
  (* Details of the most recently built claim, used to correlate an answer
     with exactly the user/device pairs that were requested. *)
  mutable current_claim : (Uid.t * Did.t) list option;
  mutable claim_failures : claim_failure list;
}

let user_id t = t.our_user
let device_id t = t.our_device
let dehydrated_pickle_key t = t.dehydrated_pickle_key
let set_dehydrated_pickle_key t key = t.dehydrated_pickle_key <- Some key
let last_uploaded_device_id t = t.last_uploaded_device_id

let set_last_uploaded_device_id t device_id =
  t.last_uploaded_device_id <- Some device_id

let account t = Olm.Machine.account t.machine
let identity_keys t = Olm.Account.identity_keys (account t)
let our_ed25519 t = fst (identity_keys t)
let our_curve25519 t = snd (identity_keys t)
let mem_uid u l = List.exists (Uid.equal u) l
let rem_uid u l = List.filter (fun x -> not (Uid.equal x u)) l
let eq_session (r1, s1) (r2, s2) = Rid.equal r1 r2 && Sid.equal s1 s2

let find_assoc eq k l =
  Option.map snd (List.find_opt (fun (k', _) -> eq k k') l)

let rem_assoc eq k l = List.filter (fun (k', _) -> not (eq k k')) l

(* A device can hold thousands of Megolm sessions, so what has reached the
   backup is compared as a set rather than scanned once per session. *)
module Session_set = Set.Make (struct
  type t = Rid.t * Sid.t

  let compare (r1, s1) (r2, s2) =
    match Rid.compare r1 r2 with 0 -> Sid.compare s1 s2 | n -> n
end)

let compare_session_key (r1, s1) (r2, s2) =
  match Rid.compare r1 r2 with 0 -> Sid.compare s1 s2 | n -> n

let wall_clock () = Ptime_clock.now ()

let claim_failure_delay_s count =
  (* [count] is one less than the number of failures, matching Rust's cache. *)
  let delay = 15. *. (2. ** float (min count 6)) in
  min 900. delay

let claim_failure_active t ~now ~user_id ~device_id =
  match
    List.find_opt
      (fun f ->
        Uid.equal f.cf_user_id user_id && Did.equal f.cf_device_id device_id)
      t.claim_failures
  with
  | None -> false
  | Some f -> Ptime.compare now f.cf_until < 0

let remember_claim t keys =
  let devices =
    List.concat_map
      (fun (user_id, devices) ->
        List.map (fun (device_id, _) -> (user_id, device_id)) devices)
      keys
  in
  t.current_claim <- Some devices

let mark_claim_failures t ~now devices =
  List.iter
    (fun (user_id, device_id) ->
      let previous =
        List.find_opt
          (fun f ->
            Uid.equal f.cf_user_id user_id && Did.equal f.cf_device_id device_id)
          t.claim_failures
      in
      let count =
        match previous with None -> 0 | Some f -> min 6 (f.cf_count + 1)
      in
      let until =
        Ptime.add_span now
          (Ptime.Span.of_float_s (claim_failure_delay_s count)
          |> Option.value ~default:Ptime.Span.zero)
        |> Option.value ~default:now
      in
      let failure =
        {
          cf_user_id = user_id;
          cf_device_id = device_id;
          cf_until = until;
          cf_count = count;
        }
      in
      t.claim_failures <-
        failure
        :: List.filter
             (fun f ->
               not
                 (Uid.equal f.cf_user_id user_id
                 && Did.equal f.cf_device_id device_id))
             t.claim_failures)
    devices

let clear_claim_failure t ~user_id ~device_id =
  t.claim_failures <-
    List.filter
      (fun f ->
        not
          (Uid.equal f.cf_user_id user_id && Did.equal f.cf_device_id device_id))
      t.claim_failures

let inbound_key s =
  (Olm.Megolm.Inbound.room_id s, Olm.Megolm.Inbound.session_id s)

let normalize_pending_key_bundles pending =
  let _, normalized =
    List.fold_left
      (fun (seen, kept) (record : pending_key_bundle) ->
        if List.exists (Rid.equal record.room_id) seen then (seen, kept)
        else (record.room_id :: seen, record :: kept))
      ([], []) pending
  in
  List.rev normalized

let normalize_received_key_bundles bundles =
  let _, normalized =
    List.fold_left
      (fun (seen, kept) (record : received_key_bundle) ->
        let key = (record.room_id, record.sender) in
        let valid =
          Result.is_ok (Ed25519.Public.of_base64 record.sender_ed25519)
          && Result.is_ok
               (Encrypted_attachment.Metadata.of_event_file record.file)
          && Result.is_ok (Media.Mxc.of_string record.file.url)
        in
        if
          (not valid)
          || List.exists
               (fun (room_id, sender) ->
                 Rid.equal room_id record.room_id
                 && Uid.equal sender record.sender)
               seen
        then (seen, kept)
        else (key :: seen, record :: kept))
      ([], []) bundles
  in
  List.rev normalized

let of_parts ~random ~user_id ~device_id ~machine (state : state) ~inbound
    ~outbound : t =
  {
    random;
    our_user = user_id;
    our_device = device_id;
    machine;
    devices = state.devices;
    tracked = state.tracked_users;
    outdated = state.outdated_users;
    rooms = state.rooms;
    backup = state.backup;
    (* Older stores treated forwarding chains as opaque strings. Drop only
       metadata whose chain cannot be made a typed, bounded wire value; the
       corresponding Megolm session remains available for local decryption. *)
    metas = sanitize_session_metadata state.session_meta;
    identities = state.identities;
    identity_changes = [];
    trust_requirement = state.trust_requirement;
    withheld = state.withheld;
    (* The store is keyed conceptually by room.  Keep the first (newest, for
       snapshots written by this implementation) record if a hand-edited or
       otherwise malformed snapshot contains duplicates. *)
    pending_key_bundles =
      normalize_pending_key_bundles state.pending_key_bundles;
    received_key_bundles =
      normalize_received_key_bundles state.received_key_bundles;
    outbound_withhelds = state.outbound_withheld;
    published_otks = List.sort_uniq String.compare state.published_one_time_keys;
    keys_uploaded = state.device_keys_uploaded;
    dehydrated_pickle_key = state.dehydrated_pickle_key;
    last_uploaded_device_id = state.last_uploaded_device_id;
    fallback_pending = state.fallback_key_pending;
    fallback_key_created_at = state.fallback_key_created_at;
    server_otk_count = max 0 state.uploaded_one_time_key_count;
    secret_values = state.secrets;
    secret_request_state = state.secret_requests;
    secret_send_state = state.secret_sends;
    inbound;
    outbound;
    wedged = [];
    current_claim = None;
    claim_failures = [];
  }

let create_with_account ~random ~user_id ~device_id ~account () =
  let machine = Olm.Machine.of_account account in
  of_parts ~random ~user_id ~device_id ~machine empty_state ~inbound:[]
    ~outbound:[]

let create ~random ~user_id ~device_id () =
  create_with_account ~random ~user_id ~device_id
    ~account:(Olm.Account.create ~random ())
    ()

let of_snapshot ~random ~user_id ~device_id (snap : snapshot) =
  let machine = Olm.Machine.of_account snap.account in
  List.iter (Olm.Machine.store_olm_session machine) snap.olm_sessions;
  let inbound = List.map (fun s -> (inbound_key s, s)) snap.megolm_inbound in
  let outbound =
    List.map
      (fun s ->
        ( Olm.Megolm.Outbound.room_id s,
          {
            os_session = s;
            os_created = Olm.Megolm.Outbound.creation_time s;
            os_members =
              List.sort_uniq Uid.compare
                (List.map fst (Olm.Megolm.Outbound.shared_with s));
          } ))
      snap.megolm_outbound
  in
  of_parts ~random ~user_id ~device_id ~machine snap.state ~inbound ~outbound

let snapshot t =
  {
    account = account t;
    olm_sessions = Olm.Machine.olm_sessions t.machine;
    megolm_inbound = List.map snd t.inbound;
    megolm_outbound = List.map (fun (_, o) -> o.os_session) t.outbound;
    state =
      {
        devices = t.devices;
        tracked_users = t.tracked;
        outdated_users = t.outdated;
        rooms = t.rooms;
        backup = t.backup;
        session_meta = t.metas;
        trust_requirement = t.trust_requirement;
        identities = t.identities;
        withheld = t.withheld;
        pending_key_bundles = t.pending_key_bundles;
        received_key_bundles = t.received_key_bundles;
        outbound_withheld = t.outbound_withhelds;
        secrets = t.secret_values;
        secret_requests = t.secret_request_state;
        secret_sends = t.secret_send_state;
        fallback_key_created_at = t.fallback_key_created_at;
        fallback_key_pending = t.fallback_pending;
        published_one_time_keys = t.published_otks;
        uploaded_one_time_key_count = t.server_otk_count;
        device_keys_uploaded = t.keys_uploaded;
        dehydrated_pickle_key = t.dehydrated_pickle_key;
        last_uploaded_device_id = t.last_uploaded_device_id;
      };
  }

let device_algorithms = [ olm_algorithm; megolm_algorithm ]

let device_keys_for_upload ?dehydrated t : Keys.device_keys =
  let ed, curve = identity_keys t in
  let keys =
    [
      (device_key_id t.our_device, Ed25519.Public.to_base64 ed);
      ( Key_id.of_device ~algorithm:"curve25519" t.our_device,
        Curve25519.Public.to_base64 curve );
    ]
  in
  let canonical =
    canonical_device_keys ?dehydrated ~user_id:t.our_user
      ~device_id:t.our_device ~algorithms:device_algorithms ~keys ()
  in
  let signature = Olm.Account.sign (account t) canonical in
  {
    Keys.user_id = t.our_user;
    device_id = t.our_device;
    algorithms = device_algorithms;
    keys;
    signatures = [ (t.our_user, [ (device_key_id t.our_device, signature) ]) ];
    dehydrated;
    unsigned = None;
  }

let object_mems = function Jsont.Object (mems, _) -> mems | _ -> []

let find_mem name mems =
  Option.map snd (List.find_opt (fun ((n, _), _) -> String.equal name n) mems)

let without_mem name mems =
  List.filter (fun ((n, _), _) -> not (String.equal name n)) mems

let sign t json =
  match json with
  | Jsont.Object (mems, meta) ->
      let canonical =
        Matrix_proto.Signed_json.canonical_json
          (Matrix_proto.Signed_json.json_for_signing json)
      in
      let signature =
        Crypto_key.Signature.to_base64 (Olm.Account.sign (account t) canonical)
      in
      let user = Uid.to_string t.our_user in
      let key_id = Key_id.to_string (device_key_id t.our_device) in
      let existing =
        Option.fold ~none:[] ~some:object_mems (find_mem "signatures" mems)
      in
      let mine =
        without_mem key_id
          (Option.fold ~none:[] ~some:object_mems (find_mem user existing))
      in
      let signatures =
        Jsont.Json.mem (Jsont.Json.name user)
          (Jsont.Json.object'
             (Jsont.Json.mem (Jsont.Json.name key_id)
                (Jsont.Json.string signature)
             :: mine))
        :: without_mem user existing
      in
      Jsont.Json.object' ~meta
        (Jsont.Json.mem
           (Jsont.Json.name "signatures")
           (Jsont.Json.object' signatures)
        :: without_mem "signatures" mems)
  | j -> j

let track_users t users =
  List.iter
    (fun u ->
      if not (mem_uid u t.tracked) then begin
        t.tracked <- u :: t.tracked;
        if not (mem_uid u t.outdated) then t.outdated <- u :: t.outdated
      end)
    users

let untrack_users t users =
  List.iter
    (fun u ->
      t.tracked <- rem_uid u t.tracked;
      t.outdated <- rem_uid u t.outdated;
      t.devices <-
        List.filter (fun (d : device) -> not (Uid.equal d.user_id u)) t.devices)
    users

let tracked_users t = t.tracked
let outdated_users t = t.outdated
let set_trust_requirement t requirement = t.trust_requirement <- requirement
let trust_requirement t = t.trust_requirement

let identity_status t user_id =
  Option.map
    (fun i -> i.identity_status)
    (List.find_opt (fun i -> Uid.equal i.identity_user_id user_id) t.identities)

let identity_has_pin_violation t user_id =
  if Uid.equal user_id t.our_user then false
  else
    match
      List.find_opt (fun i -> Uid.equal i.identity_user_id user_id) t.identities
    with
    | Some i -> i.identity_pinned_master_key <> i.identity_master_key
    | None -> false

(* The query state deliberately stores only the validated public halves of an
   identity. Reconstructing this small wire object lets callers publish a
   signature over exactly the master key that passed the master/self-signing
   validation boundary, without exposing the mutable identity table. *)
let identity_master_key t user_id =
  match
    List.find_opt (fun i -> Uid.equal i.identity_user_id user_id) t.identities
  with
  | None -> None
  | Some i ->
      Some
        {
          Keys.user_id;
          usage = [ Keys.Master ];
          keys =
            [
              ( Key_id.v ~algorithm:"ed25519" ~id:i.identity_master_key,
                i.identity_master_key );
            ];
          signatures = [];
        }

let identity_self_signing_key t user_id =
  match
    List.find_opt (fun i -> Uid.equal i.identity_user_id user_id) t.identities
  with
  | None -> None
  | Some i ->
      Some
        {
          Keys.user_id;
          usage = [ Keys.Self_signing ];
          keys =
            [
              ( Key_id.v ~algorithm:"ed25519" ~id:i.identity_self_signing_key,
                i.identity_self_signing_key );
            ];
          signatures = [];
        }

let identity_user_signing_key t user_id =
  Option.bind
    (List.find_opt (fun i -> Uid.equal i.identity_user_id user_id) t.identities)
    (fun i ->
      Option.map
        (fun value ->
          {
            Keys.user_id;
            usage = [ Keys.User_signing ];
            keys = [ (Key_id.v ~algorithm:"ed25519" ~id:value, value) ];
            signatures = [];
          })
        i.identity_user_signing_key)

let identity_changes t = List.rev t.identity_changes

let trust_user_identity t user_id =
  t.identities <-
    List.map
      (fun i ->
        if Uid.equal i.identity_user_id user_id then
          {
            i with
            identity_was_previously_verified = true;
            identity_status = Identity_verified;
            identity_pinned_master_key = i.identity_master_key;
          }
        else i)
      t.identities

let reset_cross_signing t =
  (* Device keys remain valid across an identity reset, but trust obtained from
     the old self-signing key does not. [trust] currently does not retain the
     provenance of a [Verified] decision, so reset all verified own devices;
     an explicit blacklist remains an explicit local refusal. *)
  t.identities <-
    List.filter
      (fun i -> not (Uid.equal i.identity_user_id t.our_user))
      t.identities;
  t.devices <-
    List.map
      (fun device ->
        if Uid.equal device.user_id t.our_user then
          match device.trust with
          | Blacklisted -> device
          | Unverified | Verified -> { device with trust = Unverified }
        else device)
      t.devices

let pin_user_identity t user_id =
  if not (Uid.equal user_id t.our_user) then
    t.identities <-
      List.map
        (fun i ->
          if Uid.equal i.identity_user_id user_id then
            { i with identity_pinned_master_key = i.identity_master_key }
          else i)
        t.identities

(* Acknowledging a TOFU pin is deliberately weaker than interactive
   cross-signing verification. *)
let acknowledge_user_identity = pin_user_identity

let devices_of t u =
  List.filter (fun (d : device) -> Uid.equal d.user_id u) t.devices

let is_device (d : device) u device_id =
  Uid.equal d.user_id u && Did.equal d.device_id device_id

let find_device t u ~device_id =
  List.find_opt (fun d -> is_device d u device_id) t.devices

let find_device_by_curve25519 t key =
  List.find_opt
    (fun (d : device) ->
      match device_curve25519 d with
      | Some k -> Curve25519.Public.equal k key
      | None -> false)
    t.devices

let set_device_trust t u ~device_id trust =
  t.devices <-
    List.map
      (fun (d : device) ->
        if is_device d u device_id then { d with trust } else d)
      t.devices

(* A queried device is accepted only when it signed its own keys. The
   identity keys of a device identifier are immutable: a device that comes
   back with different keys is an impersonation attempt or a server bug.
   Either way, and for any other way a re-query can fail to validate, a
   device this client already held is kept as it was rather than dropped: a
   hostile or malfunctioning homeserver should not be able to erase a
   recorded trust decision just by answering one [/keys/query] badly. *)
let identity_key ~user_id ~usage value : Keys.cross_signing_key =
  {
    Keys.user_id;
    usage = [ usage ];
    keys = [ (Key_id.v ~algorithm:"ed25519" ~id:value, value) ];
    signatures = [];
  }

let valid_cross_signed_device identity (qd : Keys.device_keys) =
  match Ed25519.Public.of_base64 identity.identity_self_signing_key with
  | Error _ -> false
  | Ok _ ->
      let self =
        identity_key ~user_id:identity.identity_user_id ~usage:Keys.Self_signing
          identity.identity_self_signing_key
      in
      Cross_signing.verify_device_signature
        ~self_signing_key:(Cross_signing.key ~role:Keys.Self_signing self)
        ~device:(Cross_signing.create_device qd)

let device_of_queried t user_id (qd : Keys.device_keys) =
  let existing = find_device t user_id ~device_id:qd.device_id in
  let fail reason =
    Log.warn (fun m ->
        m "Ignoring device %a of %a: %s" Did.pp qd.device_id Uid.pp qd.user_id
          reason);
    existing
  in
  let key_id algorithm = Key_id.of_device ~algorithm qd.device_id in
  let key algorithm = List.assoc_opt (key_id algorithm) qd.keys in
  if not (Uid.equal qd.user_id user_id) then
    fail "user id does not match the query"
  else
    match (key "ed25519", key "curve25519") with
    | None, _ | _, None -> fail "missing an identity key"
    | Some ed, Some curve -> (
        match
          ( Ed25519.Public.of_base64 ed,
            find_signature qd.signatures ~user_id:qd.user_id
              ~key_id:(key_id "ed25519") )
        with
        | Error (`Msg _), _ -> fail "the Ed25519 key is malformed"
        | _, None -> fail "no self-signature"
        | Ok pub, Some signature ->
            let canonical =
              canonical_device_keys ~user_id:qd.user_id ~device_id:qd.device_id
                ~algorithms:qd.algorithms ~keys:qd.keys
                ?dehydrated:qd.dehydrated ()
            in
            if not (Ed25519.Public.verify pub ~signature ~data:canonical) then
              fail "self-signature does not verify"
            else
              let same base64 = function
                | None -> false
                | Some k -> String.equal k base64
              in
              let changed =
                match existing with
                | None -> false
                | Some e ->
                    (not
                       (same ed
                          (Option.map Ed25519.Public.to_base64
                             (device_ed25519 e))))
                    || not
                         (same curve
                            (Option.map Curve25519.Public.to_base64
                               (device_curve25519 e)))
              in
              if changed then fail "identity keys changed"
              else
                Some
                  {
                    user_id;
                    device_id = qd.device_id;
                    algorithms = qd.algorithms;
                    keys = qd.keys;
                    signatures = qd.signatures;
                    dehydrated = qd.dehydrated;
                    trust =
                      (match existing with
                      | Some e -> e.trust
                      | None -> Unverified);
                  })

let process_identity_query t (resp : Keys.query_keys_response) =
  let one_key = function [ (_, k) ] -> Some k | _ -> None in
  let valid_key = function
    | Some k -> Result.is_ok (Ed25519.Public.of_base64 k)
    | None -> false
  in
  let users =
    List.sort_uniq Uid.compare
      (List.map fst resp.master_keys @ List.map fst resp.self_signing_keys)
  in
  List.iter
    (fun user_id ->
      match
        ( List.assoc_opt user_id resp.master_keys,
          List.assoc_opt user_id resp.self_signing_keys )
      with
      | Some master, Some self
        when Uid.equal master.user_id user_id
             && Uid.equal self.user_id user_id
             && List.mem Keys.Master master.usage
             && List.mem Keys.Self_signing self.usage
             && valid_key (one_key master.keys)
             && valid_key (one_key self.keys) ->
          let master_key = Option.get (one_key master.keys) in
          let self_key = Option.get (one_key self.keys) in
          let user_signing_key =
            match
              if Uid.equal user_id t.our_user then
                List.assoc_opt user_id resp.user_signing_keys
              else None
            with
            | Some user_signing
              when Uid.equal user_signing.user_id user_id
                   && List.mem Keys.User_signing user_signing.usage
                   && valid_key (one_key user_signing.keys)
                   && Cross_signing.verify_key
                        ~signer:(Cross_signing.key ~role:Keys.Master master)
                        ~signed:
                          (Cross_signing.key ~role:Keys.User_signing
                             user_signing) ->
                one_key user_signing.keys
            | _ -> None
          in
          let valid =
            Cross_signing.verify_key
              ~signer:(Cross_signing.key ~role:Keys.Master master)
              ~signed:(Cross_signing.key ~role:Keys.Self_signing self)
          in
          if valid then begin
            let old =
              List.find_opt
                (fun i -> Uid.equal i.identity_user_id user_id)
                t.identities
            in
            let status : identity_status =
              match old with
              | Some i when i.identity_status = Verification_violation ->
                  Verification_violation
              | Some i
                when i.identity_was_previously_verified
                     && i.identity_master_key <> master_key ->
                  Verification_violation
              | Some i when i.identity_status = Identity_verified ->
                  Identity_verified
              | _ -> Identity_unverified
            in
            let next =
              {
                identity_user_id = user_id;
                identity_master_key = master_key;
                identity_self_signing_key = self_key;
                identity_user_signing_key =
                  (match old with
                  | Some old
                    when old.identity_master_key = master_key
                         && old.identity_self_signing_key = self_key -> (
                      match user_signing_key with
                      | Some _ -> user_signing_key
                      | None -> old.identity_user_signing_key)
                  | _ -> user_signing_key);
                identity_was_previously_verified =
                  Option.value ~default:false
                    (Option.map
                       (fun i -> i.identity_was_previously_verified)
                       old);
                identity_status = status;
                identity_pinned_master_key =
                  Option.value
                    (Option.map (fun i -> i.identity_pinned_master_key) old)
                    ~default:master_key;
              }
            in
            t.identities <-
              next
              :: List.filter
                   (fun i -> not (Uid.equal i.identity_user_id user_id))
                   t.identities;
            if
              Option.for_all
                (fun i ->
                  i.identity_status <> status
                  || i.identity_master_key <> master_key
                  || i.identity_self_signing_key <> self_key
                  || i.identity_user_signing_key
                     <> next.identity_user_signing_key)
                old
            then t.identity_changes <- { user_id; status } :: t.identity_changes
          end
      | _ -> ())
    users

let receive_keys_query t (resp : Keys.query_keys_response) =
  t.identity_changes <- [];
  process_identity_query t resp;
  List.iter
    (fun (user_id, devices) ->
      let kept =
        List.filter_map
          (fun (_, (qd : Keys.device_keys)) -> device_of_queried t user_id qd)
          devices
      in
      (* A device the query no longer lists has been deleted, so the answer
         replaces what we held for this user rather than merging into it. *)
      t.devices <-
        List.filter
          (fun (d : device) -> not (Uid.equal d.user_id user_id))
          t.devices
        @ kept;
      t.outdated <- rem_uid user_id t.outdated;
      if not (mem_uid user_id t.tracked) then t.tracked <- user_id :: t.tracked)
    resp.device_keys

let otk_algorithm = Olm.Account.one_time_key_algorithm

(* A claimed key counts only when the device's own Ed25519 key signs it, so a
   homeserver cannot substitute a key of its own. *)
let one_time_key_of_claim device (otk : Keys.one_time_key) =
  match (device_ed25519 device, otk.signatures) with
  | None, _ | _, None -> None
  | Some pub, Some signatures -> (
      match
        find_signature signatures ~user_id:device.user_id
          ~key_id:(device_key_id device.device_id)
      with
      | None -> None
      | Some signature ->
          if
            Ed25519.Public.verify pub ~signature
              ~data:
                (Keys.one_time_key_signing_json ?fallback:otk.fallback otk.key)
          then Result.to_option (Curve25519.Public.of_base64 otk.key)
          else None)

(* One event encrypted with Olm for one device, as [m.room.encrypted]. Both
   parties and both Ed25519 keys are named in the plaintext, which is what
   defeats the unknown key-share attacks olm.md describes. *)
let olm_encrypt_for t ~session ~recipient (d : device) ~event_type ~content =
  let ed, curve = identity_keys t in
  let payload : Ev.Olm_plaintext.t =
    {
      event_type;
      content;
      sender = t.our_user;
      sender_ed25519 = Ed25519.Public.to_base64 ed;
      recipient;
      recipient_ed25519 =
        (match device_ed25519 d with
        | Some k -> Ed25519.Public.to_base64 k
        | None -> "");
      sender_device_keys =
        Some (json_of Keys.device_keys_jsont (device_keys_for_upload t));
    }
  in
  match Jsont_bytesrw.encode_string Ev.Olm_plaintext.jsont payload with
  | Error m -> Error m
  | Ok plaintext -> (
      match Olm.Session.encrypt ~random:t.random session plaintext with
      | Error e -> Error (Format.asprintf "%a" Olm.pp_error e)
      | Ok (message : Olm.Session.message) -> (
          let body : Ev.Encrypted.Olm.t =
            {
              sender_key = Curve25519.Public.to_base64 curve;
              ciphertext =
                [
                  {
                    recipient_key =
                      (match device_curve25519 d with
                      | Some k -> Curve25519.Public.to_base64 k
                      | None -> "");
                    message_type = message.message_type;
                    body = message.ciphertext;
                  };
                ];
            }
          in
          match Jsont.Json.encode Ev.Encrypted.Olm.jsont body with
          | Ok j -> Ok j
          | Error m -> Error m))

let wedge_dummy_prefix = "olm-wedge-dummy:"

let has_pending_wedge_dummy t ~user_id ~device_id =
  List.exists
    (fun (s : secret_send) ->
      String.starts_with ~prefix:wedge_dummy_prefix s.ss_request_id
      && Uid.equal s.ss_user_id user_id
      && Did.equal s.ss_device_id device_id)
    t.secret_send_state

let add_message messages (d : device) content =
  let existing =
    Option.value ~default:[]
      (Option.map snd
         (List.find_opt (fun (u, _) -> Uid.equal u d.user_id) messages))
  in
  (d.user_id, (To_device.Device d.device_id, content) :: existing)
  :: List.filter (fun (u, _) -> not (Uid.equal u d.user_id)) messages

let receive_keys_claim ?now t (resp : Keys.claim_keys_response) =
  let now = Option.value now ~default:(wall_clock ()) in
  let requested = t.current_claim in
  (* A claim response is consumed once. This is intentionally the latest
     request only, matching the SDK's one-in-flight contract. *)
  t.current_claim <- None;
  (match requested with
  | None -> ()
  | Some requested ->
      let returned user_id device_id =
        List.exists
          (fun (u, devices) ->
            Uid.equal u user_id
            && List.exists (fun (d, _) -> Did.equal d device_id) devices)
          resp.one_time_keys
      in
      let failed_server user_id =
        List.exists
          (fun (server, _) ->
            Server_name.equal server (Uid.server_name user_id))
          resp.failures
      in
      mark_claim_failures t ~now
        (List.filter
           (fun (user_id, device_id) ->
             (not (returned user_id device_id)) && not (failed_server user_id))
           requested));
  let created = ref 0 in
  List.iter
    (fun (user_id, devices) ->
      List.iter
        (fun (dev, keys) ->
          let skip reason =
            Log.warn (fun m ->
                m "Not opening a session with %a of %a: %s" Did.pp dev Uid.pp
                  user_id reason)
          in
          match find_device t user_id ~device_id:dev with
          | None -> skip "no device list covers it"
          | Some device -> (
              let was_wedged =
                List.exists
                  (fun w ->
                    Uid.equal w.wd_user_id user_id
                    && Did.equal w.wd_device_id dev)
                  t.wedged
              in
              let claimed =
                List.find_opt
                  (fun (key_id, _) ->
                    String.equal (Key_id.algorithm key_id) otk_algorithm)
                  keys
              in
              match claimed with
              | None -> skip "no signed_curve25519 key was claimed"
              | Some (_, otk) -> (
                  match
                    (one_time_key_of_claim device otk, device_curve25519 device)
                  with
                  | None, _ -> skip "the claimed one-time key is not signed"
                  | _, None -> skip "it published no identity key"
                  | Some their_one_time_key, Some their_identity_key -> (
                      match
                        Olm.Machine.create_olm_session ~random:t.random
                          t.machine ~their_identity_key ~their_one_time_key
                      with
                      | Ok session ->
                          incr created;
                          if was_wedged then
                            begin if
                              not
                                (has_pending_wedge_dummy t ~user_id
                                   ~device_id:dev)
                            then
                              let content = Jsont.Json.object' [] in
                              match
                                olm_encrypt_for t ~session ~recipient:user_id
                                  device ~event_type:"m.dummy" ~content
                              with
                              | Ok pd_content ->
                                  t.secret_send_state <-
                                    {
                                      ss_request_id =
                                        wedge_dummy_prefix
                                        ^ Uid.to_string user_id ^ ":"
                                        ^ Did.to_string dev;
                                      ss_user_id = user_id;
                                      ss_device_id = dev;
                                      ss_txn_id = Random.txn_id t.random;
                                      ss_content = content;
                                      ss_sent = false;
                                      ss_messages =
                                        Some (add_message [] device pd_content);
                                    }
                                    :: t.secret_send_state
                              | Error msg ->
                                  Log.warn (fun m ->
                                      m
                                        "Could not queue Olm wedge dummy for \
                                         %a: %s"
                                        Did.pp dev msg)
                            end;
                          (* A real session is the successful retry. The
                             transient exhaustion entry and stale-session
                             repair marker are both cleared now. *)
                          clear_claim_failure t ~user_id ~device_id:dev;
                          if was_wedged then
                            t.wedged <-
                              List.filter
                                (fun w ->
                                  not
                                    (Uid.equal w.wd_user_id user_id
                                    && Did.equal w.wd_device_id dev))
                                t.wedged
                      | Error e -> skip (Format.asprintf "%a" Olm.pp_error e)))))
        devices)
    resp.one_time_keys;
  !created

let receive_keys_upload t (resp : Keys.upload_keys_response) =
  match List.assoc_opt otk_algorithm resp.one_time_key_counts with
  | Some n -> t.server_otk_count <- n
  | None -> ()

let set_room_encryption_settings t room content =
  let* c = decode_json encryption_content_jsont content in
  if c.algorithm <> megolm_algorithm then
    Error
      (Error.Json_error
         (Printf.sprintf "unsupported room encryption algorithm %S" c.algorithm))
  else begin
    t.rooms <- (room, c) :: rem_assoc Rid.equal room t.rooms;
    Ok ()
  end

let find_room_settings t room = find_assoc Rid.equal room t.rooms
let is_room_encrypted t room = find_room_settings t room <> None
let inbound_sessions t = List.map fst t.inbound

let has_inbound_session t room ~session_id =
  List.exists (fun (k, _) -> eq_session k (room, session_id)) t.inbound

let outbound_session_id t room =
  Option.map
    (fun o -> Olm.Megolm.Outbound.session_id o.os_session)
    (find_assoc Rid.equal room t.outbound)

let outbound_message_count t room =
  match find_assoc Rid.equal room t.outbound with
  | None -> 0
  | Some o -> Olm.Megolm.Outbound.message_index o.os_session

let merge_meta ~adopt_candidate (old : Session_meta.t)
    (candidate : Session_meta.t) =
  let fill old candidate = match old with Some _ -> old | None -> candidate in
  {
    old with
    (* An empty forwarding chain is the authenticated direct-key form, not a
       value a later forwarded copy may overwrite. *)
    forwarding_chain = old.forwarding_chain;
    (* Sender facts are authenticated by the envelope that delivered them.
       A later copy can fill an absent fact, but cannot contradict one. *)
    sender = fill old.sender candidate.sender;
    sender_ed25519 = fill old.sender_ed25519 candidate.sender_ed25519;
    (* This flag grants permission to reshare history. An equal or worse copy
       must not upgrade it; when the candidate's earlier ratchet is adopted,
       its flag follows that adopted session. *)
    shared_history =
      (if adopt_candidate then candidate.shared_history else old.shared_history);
    (* Explicitly known provenance wins over old compatibility metadata. *)
    legacy = old.legacy && candidate.legacy;
  }

let record_meta t ~adopt_candidate (candidate : Session_meta.t) =
  match
    List.find_opt
      (fun (m : Session_meta.t) ->
        eq_session (m.room_id, m.session_id)
          (candidate.room_id, candidate.session_id))
      t.metas
  with
  | None -> t.metas <- candidate :: t.metas
  | Some old ->
      let merged = merge_meta ~adopt_candidate old candidate in
      t.metas <-
        merged
        :: List.filter
             (fun (m : Session_meta.t) ->
               not
                 (eq_session (m.room_id, m.session_id)
                    (candidate.room_id, candidate.session_id)))
             t.metas

(* Adopt an inbound session, keeping whichever copy reaches further back. *)
let store_inbound t ?(forwarding_chain = []) ?sender ?sender_ed25519
    ?(shared_history = false) session =
  let key = inbound_key session in
  let room_id, session_id = key in
  (* A real session supersedes any earlier explanation that its key was
     withheld. Clear that evidence even when the existing copy reaches further
     back and the candidate below is not adopted. *)
  t.withheld <-
    List.filter
      (fun (w : withheld) ->
        not (Rid.equal w.room_id room_id && Sid.equal w.session_id session_id))
      t.withheld;
  let better =
    match find_assoc eq_session key t.inbound with
    | None -> true
    | Some existing ->
        Olm.Megolm.Inbound.first_known_index session
        < Olm.Megolm.Inbound.first_known_index existing
  in
  let candidate =
    {
      Session_meta.room_id;
      session_id;
      forwarding_chain;
      sender;
      sender_ed25519;
      shared_history;
      legacy = false;
    }
  in
  if better then begin
    t.inbound <- (key, session) :: rem_assoc eq_session key t.inbound
  end;
  (* This is deliberately outside [better]: equal and worse-index copies can
     still supply sender or forwarding facts missing from the first copy. *)
  record_meta t ~adopt_candidate:better candidate;
  better

let forwarding_chain_of t key =
  match
    List.find_opt
      (fun (m : Session_meta.t) -> eq_session (m.room_id, m.session_id) key)
      t.metas
  with
  | Some m -> m.forwarding_chain
  | None -> []

(* Rust's account helper fills the complete account capacity. Keep the
   unpublished-key guard below: a failed upload must continue to offer the
   exact pending keys instead of replacing them. *)
let target_one_time_keys t = Olm.Account.max_one_time_keys (account t)

let prune_published_otks t =
  let retained =
    List.fold_left
      (fun retained key_id -> String_set.add (Key_id.id key_id) retained)
      String_set.empty
      (Olm.Account.one_time_key_ids (account t))
  in
  t.published_otks <-
    List.filter (fun id -> String_set.mem id retained) t.published_otks

let unpublished_one_time_keys t =
  let published = String_set.of_list t.published_otks in
  Olm.Account.signed_one_time_keys
    ~exclude:(fun key_id -> String_set.mem (Key_id.id key_id) published)
    (account t)

let ensure_one_time_keys t =
  prune_published_otks t;
  let unpublished = List.length (unpublished_one_time_keys t) in
  (* Like vodozemac, never extend a pending unpublished batch: an upload may be
     in flight or waiting for retry, and it must retain its exact key set. *)
  let server_count = max 0 (min (target_one_time_keys t) t.server_otk_count) in
  let want = target_one_time_keys t - server_count in
  if unpublished = 0 && want > 0 then begin
    Olm.Account.generate_one_time_keys ~random:t.random (account t) want;
    (* Generation may evict old private secrets at the vodozemac retention
       limit. Do not persist publication markers for keys no longer retained. *)
    prune_published_otks t
  end

let one_time_key_body t (key_id, public, signature) =
  ( key_id,
    {
      Keys.key = Curve25519.Public.to_base64 public;
      fallback = None;
      signatures =
        Some [ (t.our_user, [ (device_key_id t.our_device, signature) ]) ];
    } )

let fallback_key_body t =
  match Olm.Account.fallback_key (account t) with
  | None -> []
  | Some (key_id, public) ->
      let signature =
        Olm.Account.sign (account t)
          (Keys.one_time_key_signing_json ~fallback:true
             (Curve25519.Public.to_base64 public))
      in
      [
        ( key_id,
          {
            Keys.key = Curve25519.Public.to_base64 public;
            fallback = Some true;
            signatures =
              Some [ (t.our_user, [ (device_key_id t.our_device, signature) ]) ];
          } );
      ]

let fallback_key_expired t ~at =
  match t.fallback_key_created_at with
  | None -> true
  | Some created_at ->
      Ptime.compare created_at at > 0
      || Ptime.Span.to_float_s (Ptime.diff at created_at) > 7. *. 24. *. 3600.

(* The contents of [fallback_types] describe the server's unused keys, but—as
   in Rust—they are not the local publication state.  Merely receiving the
   field establishes server support.  Thereafter the persisted creation time
   keeps age-based rotation active even if a later incremental sync omits it;
   [fallback_pending] is the local unpublished-key guard. *)
let maybe_generate_fallback_key t ~at ~fallback_types =
  let support_observed =
    Option.is_some fallback_types || Option.is_some t.fallback_key_created_at
  in
  if support_observed && (not t.fallback_pending) && fallback_key_expired t ~at
  then begin
    Olm.Account.generate_fallback_key ~random:t.random (account t);
    t.fallback_key_created_at <- Some at;
    t.fallback_pending <- true
  end

let outgoing_requests_base t =
  ensure_one_time_keys t;
  let pending = unpublished_one_time_keys t in
  let fallback_keys = if t.fallback_pending then fallback_key_body t else [] in
  let upload =
    if (not t.keys_uploaded) || pending <> [] || fallback_keys <> [] then
      [
        Keys_upload
          {
            device_keys =
              (if t.keys_uploaded then None else Some (device_keys_for_upload t));
            one_time_keys = List.map (one_time_key_body t) pending;
            fallback_keys;
          };
      ]
    else []
  in
  let query = if t.outdated = [] then [] else [ Keys_query t.outdated ] in
  let wedged_claims =
    let devices =
      List.filter
        (fun (d : device) ->
          d.trust <> Blacklisted
          && List.exists
               (fun w ->
                 Uid.equal w.wd_user_id d.user_id
                 && Did.equal w.wd_device_id d.device_id)
               t.wedged)
        t.devices
    in
    if devices = [] then []
    else
      let keys =
        List.fold_left
          (fun acc (d : device) ->
            let existing =
              Option.value ~default:[]
                (Option.map snd
                   (List.find_opt (fun (u, _) -> Uid.equal u d.user_id) acc))
            in
            (d.user_id, (d.device_id, otk_algorithm) :: existing)
            :: List.filter (fun (u, _) -> not (Uid.equal u d.user_id)) acc)
          [] devices
      in
      remember_claim t keys;
      [ Keys_claim keys ]
  in
  upload @ query @ wedged_claims

let add_backed_up t sessions =
  let known = Session_set.of_list sessions in
  sessions
  @ List.filter (fun k -> not (Session_set.mem k known)) t.backup.backed_up

let mark_sent t = function
  | Keys_upload { device_keys; one_time_keys; fallback_keys } ->
      if device_keys <> None then t.keys_uploaded <- true;
      t.published_otks <-
        List.fold_left
          (fun ids (id, _) -> String_set.add (Key_id.id id) ids)
          (String_set.of_list t.published_otks)
          one_time_keys
        |> String_set.elements;
      if fallback_keys <> [] then t.fallback_pending <- false
  | Keys_query _ | Keys_claim _ -> ()
  | To_device { event_type = "m.room_key.withheld"; txn_id; _ } ->
      t.outbound_withhelds <-
        List.map
          (fun notice ->
            if String.equal notice.ow_txn_id txn_id then
              { notice with ow_sent = true }
            else notice)
          t.outbound_withhelds
  | To_device { event_type = "m.room.encrypted"; txn_id; _ } ->
      t.secret_send_state <-
        List.filter
          (fun s -> not (String.equal txn_id s.ss_txn_id))
          t.secret_send_state;
      let requests =
        List.filter_map
          (fun r ->
            if String.equal txn_id r.sr_txn_id then
              Some { r with sr_sent = true }
            else
              match r.sr_cancel with
              | Some c when String.equal txn_id c.sc_txn_id ->
                  if c.sc_sent then None
                  else
                    Some { r with sr_cancel = Some { c with sc_sent = true } }
              | _ -> Some r)
          t.secret_request_state
      in
      t.secret_request_state <-
        List.filter
          (fun r ->
            match r.sr_cancel with Some c -> not c.sc_sent | None -> true)
          requests
  | To_device { event_type = "m.secret.request"; txn_id; _ } ->
      let requests =
        List.filter_map
          (fun r ->
            if String.equal txn_id r.sr_txn_id then
              Some { r with sr_sent = true }
            else
              match r.sr_cancel with
              | Some c when String.equal txn_id c.sc_txn_id ->
                  if c.sc_sent then None
                  else
                    Some { r with sr_cancel = Some { c with sc_sent = true } }
              | _ -> Some r)
          t.secret_request_state
      in
      t.secret_request_state <-
        List.filter
          (fun r ->
            match r.sr_cancel with Some c -> not c.sc_sent | None -> true)
          requests
  | To_device _ -> ()
  | Room_key_share { room_id; session_id; messages; _ } -> (
      match find_assoc Rid.equal room_id t.outbound with
      | Some o
        when Sid.equal session_id (Olm.Megolm.Outbound.session_id o.os_session)
        ->
          List.iter
            (fun (user_id, targets) ->
              List.iter
                (fun (target, _) ->
                  match target with
                  | To_device.Device device_id ->
                      Olm.Megolm.Outbound.mark_shared_with o.os_session ~user_id
                        ~device_id
                  | To_device.All -> ())
                targets)
            messages
      (* The room rotated to a new session before the share was performed, so
         the devices this reached hold a session that is no longer current. *)
      | _ -> ())
  | Room_key_bundle_share _ -> ()
  | Room_keys_upload { version; rooms } ->
      (* A response may arrive after the caller has switched to another
         backup version. Never acknowledge an old batch into the new
         version's upload ledger; Rust couples this decision to the pending
         request id and its version. *)
      if t.backup.version = Some version then begin
        let uploaded =
          List.concat_map
            (fun (room, sessions) ->
              match Rid.of_string room with
              | Error (`Msg _) -> []
              | Ok room ->
                  List.filter_map
                    (fun (session, _) ->
                      Result.to_option
                        (Result.map
                           (fun s -> (room, s))
                           (Sid.of_string session)))
                    sessions)
            rooms
        in
        t.backup <- { t.backup with backed_up = add_backed_up t uploaded }
      end
      else
        Log.warn (fun m ->
            m "Ignoring backup upload response for stale version %s" version)

let enable_backup t ~version ?decryption_key encryption_key =
  (* Re-enabling the same server version is an idempotent configuration
     update. Keep the local upload ledger so a caller refreshing credentials
     does not schedule every historical room key again; a new version must
     start with an empty ledger. *)
  let backed_up =
    match t.backup.version with
    | Some current when String.equal current version -> t.backup.backed_up
    | _ -> []
  in
  let room_key_backups_fully_downloaded =
    match t.backup.version with
    | Some current when String.equal current version ->
        t.backup.room_key_backups_fully_downloaded
    | _ -> []
  in
  t.backup <-
    {
      version = Some version;
      encryption_key = Some encryption_key;
      decryption_key;
      backed_up;
      room_key_backups_fully_downloaded;
    }

let disable_backup t = t.backup <- empty_backup
let backup_version t = t.backup.version
let backup_decryption_enabled t = Option.is_some t.backup.decryption_key

let room_key_backup_is_fully_downloaded t room_id =
  List.exists (Rid.equal room_id) t.backup.room_key_backups_fully_downloaded

let mark_room_key_backup_fully_downloaded t room_id =
  if not (room_key_backup_is_fully_downloaded t room_id) then
    t.backup <-
      {
        t.backup with
        room_key_backups_fully_downloaded =
          room_id :: t.backup.room_key_backups_fully_downloaded;
      }

let clear_room_key_backup_fully_downloaded t room_id =
  t.backup <-
    {
      t.backup with
      room_key_backups_fully_downloaded =
        List.filter
          (fun r -> not (Rid.equal r room_id))
          t.backup.room_key_backups_fully_downloaded;
    }

let pending_sessions t =
  let known = Session_set.of_list t.backup.backed_up in
  List.filter (fun (key, _) -> not (Session_set.mem key known)) t.inbound

let backup_batch_size = 100

let take n l =
  let rec loop n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> loop (n - 1) (x :: acc) rest
  in
  loop n [] l

let backup_pending_count t =
  if t.backup.version = None || t.backup.encryption_key = None then 0
  else List.length (pending_sessions t)

let backed_up_payload t key session =
  let chain = forwarding_chain_of t key in
  let shared_history =
    Option.value
      (Option.map
         (fun (m : Session_meta.t) -> m.shared_history)
         (List.find_opt
            (fun (m : Session_meta.t) ->
              eq_session (m.room_id, m.session_id) key)
            t.metas))
      ~default:false
  in
  ( chain,
    {
      Backup.algorithm = megolm_algorithm;
      forwarding_curve25519_key_chain = chain;
      sender_key =
        Curve25519.Public.to_base64 (Olm.Megolm.Inbound.sender_key session);
      sender_claimed_keys =
        (match Olm.Megolm.Inbound.sender_claimed_ed25519_key session with
        | None -> []
        | Some k -> [ ("ed25519", Ed25519.Public.to_base64 k) ]);
      session_key = Olm.Megolm.Inbound.export_at_first_known_index session;
      shared_history;
    } )

let json_error msg = Error (Error.Json_error msg)

let claimed_ed25519_of keys =
  match List.assoc_opt "ed25519" keys with
  | None -> Ok None
  | Some value -> Result.map Option.some (Ed25519.Public.of_base64 value)

let pending_backup t =
  match (t.backup.version, t.backup.encryption_key) with
  | Some version, Some encryption_key -> (
      match pending_sessions t with
      | [] -> Ok None
      | pending ->
          let pending =
            pending
            |> List.sort (fun (key, _) (key', _) ->
                compare_session_key key key')
            |> take backup_batch_size
          in
          let add acc (key, session) =
            let* acc = acc in
            let room, sid = key in
            let chain, payload = backed_up_payload t key session in
            let* plaintext =
              match
                Jsont_bytesrw.encode_string Backup.backed_up_session_data_jsont
                  payload
              with
              | Ok s -> Ok s
              | Error m -> json_error m
            in
            let* session_data =
              match
                Backup.encrypt_session_data ~random:t.random encryption_key
                  plaintext
              with
              | Ok d -> Ok d
              | Error (`Msg m) -> json_error m
            in
            let data =
              {
                Backup.first_message_index =
                  Olm.Megolm.Inbound.first_known_index session;
                forwarded_count = List.length chain;
                is_verified = Olm.Megolm.Inbound.signing_key_verified session;
                session_data;
              }
            in
            let room = Rid.to_string room in
            let existing = Option.value ~default:[] (List.assoc_opt room acc) in
            Ok
              ((room, (Sid.to_string sid, data) :: existing)
              :: List.remove_assoc room acc)
          in
          let* rooms = List.fold_left add (Ok []) pending in
          Ok (Some (Room_keys_upload { version; rooms })))
  | _ -> Ok None

let import_backup t (rooms : Backup.rooms) =
  match t.backup.decryption_key with
  | None -> json_error "no backup decryption key is enabled"
  | Some key ->
      let imported = ref 0 in
      let restored = ref [] in
      List.iter
        (fun (room_id, sessions) ->
          List.iter
            (fun (session_id, (kbd : Backup.key_backup_data)) ->
              let skip msg =
                Log.warn (fun m ->
                    m "Skipping backed-up session %S of %S: %s" session_id
                      room_id msg)
              in
              match (Rid.of_string room_id, Sid.of_string session_id) with
              | Error (`Msg m), _ | _, Error (`Msg m) -> skip m
              | Ok room, Ok sid -> (
                  match Backup.decrypt_room_key key kbd.session_data with
                  | Error (`Msg msg) -> skip msg
                  | Ok plaintext -> (
                      match
                        Backup.parse_recovered_key ~room_id:room ~session_id:sid
                          plaintext
                      with
                      | Error (`Msg m) -> skip m
                      | Ok rk -> (
                          if not (String.equal rk.algorithm megolm_algorithm)
                          then
                            skip
                              (Printf.sprintf "unsupported backup algorithm %S"
                                 rk.algorithm)
                          else
                            match
                              ( Curve25519.Public.of_base64 rk.sender_key,
                                claimed_ed25519_of rk.sender_claimed_keys,
                                canonical_forwarding_chain
                                  rk.forwarding_curve25519_key_chain )
                            with
                            | Error (`Msg m), _, _
                            | _, Error (`Msg m), _
                            | _, _, Error m ->
                                skip m
                            | ( Ok sender_key,
                                Ok claimed_ed25519,
                                Ok forwarding_chain ) -> (
                                match
                                  Olm.Megolm.Inbound.of_exported_session_key
                                    ?claimed_ed25519 ~sender_key ~room_id:room
                                    ~session_key:rk.session_key ()
                                with
                                | Error e ->
                                    skip (Format.asprintf "%a" Olm.pp_error e)
                                | Ok session ->
                                    if
                                      not
                                        (Sid.equal sid
                                           (Olm.Megolm.Inbound.session_id
                                              session))
                                    then skip "backup session ID mismatch"
                                    else begin
                                      if
                                        store_inbound t ~forwarding_chain
                                          ~shared_history:rk.shared_history
                                          session
                                      then incr imported;
                                      restored := (room, sid) :: !restored
                                    end)))))
            sessions)
        rooms;
      (* Whatever came out of the backup is already in it. *)
      t.backup <- { t.backup with backed_up = add_backed_up t !restored };
      Ok !imported

let export_room_keys ?(predicate = Fun.const true) t =
  t.inbound
  |> List.sort (fun (key, _) (key', _) -> compare_session_key key key')
  |> List.map (fun ((room_id, session_id), session) ->
      let meta =
        List.find_opt
          (fun (m : Session_meta.t) ->
            Rid.equal m.room_id room_id && Sid.equal m.session_id session_id)
          t.metas
      in
      ({
         Room_key_export.algorithm = megolm_algorithm;
         room_id;
         sender_key =
           Curve25519.Public.to_base64 (Olm.Megolm.Inbound.sender_key session);
         session_id;
         session_key = Olm.Megolm.Inbound.export_at_first_known_index session;
         sender_claimed_keys =
           (match Olm.Megolm.Inbound.sender_claimed_ed25519_key session with
           | None -> []
           | Some key -> [ ("ed25519", Ed25519.Public.to_base64 key) ]);
         forwarding_curve25519_key_chain =
           Option.value
             (Option.map (fun (m : Session_meta.t) -> m.forwarding_chain) meta)
             ~default:[];
         shared_history =
           Option.value
             (Option.map (fun (m : Session_meta.t) -> m.shared_history) meta)
             ~default:false;
       }
        : Room_key_export.room_key))
  |> List.filter predicate

type room_key_import_result = { imported_count : int; total_count : int }

let import_room_keys t (keys : Room_key_export.room_key list) =
  let imported_count =
    List.fold_left
      (fun imported (key : Room_key_export.room_key) ->
        let skip reason =
          Log.warn (fun m ->
              m "Skipping exported Megolm session %S: %s"
                (Sid.to_string key.session_id)
                reason)
        in
        if not (String.equal key.algorithm megolm_algorithm) then begin
          skip (Printf.sprintf "unsupported algorithm %S" key.algorithm);
          imported
        end
        else
          match
            ( Curve25519.Public.of_base64 key.sender_key,
              claimed_ed25519_of key.sender_claimed_keys )
          with
          | Error (`Msg msg), _ | _, Error (`Msg msg) ->
              skip msg;
              imported
          | Ok sender_key, Ok claimed_ed25519 -> (
              match
                canonical_forwarding_chain key.forwarding_curve25519_key_chain
              with
              | Error message ->
                  skip message;
                  imported
              | Ok forwarding_chain -> (
                  match
                    Olm.Megolm.Inbound.from_room_key
                      ?signing_key:claimed_ed25519 ~sender_key
                      ~room_id:key.room_id ~session_id:key.session_id
                      ~session_key:key.session_key ()
                  with
                  | Error error ->
                      skip (Format.asprintf "%a" Olm.pp_error error);
                      imported
                  | Ok session ->
                      if
                        store_inbound t ~forwarding_chain
                          ~shared_history:key.shared_history session
                      then imported + 1
                      else imported)))
      0 keys
  in
  { imported_count; total_count = List.length keys }

let session_shared_history t key =
  Option.value
    (Option.map
       (fun (m : Session_meta.t) -> m.shared_history)
       (List.find_opt
          (fun (m : Session_meta.t) -> eq_session (m.room_id, m.session_id) key)
          t.metas))
    ~default:false

let build_room_key_bundle t ~room_id =
  let sessions =
    t.inbound
    |> List.filter (fun ((room, _), _) -> Rid.equal room room_id)
    |> List.sort (fun (key, _) (key', _) -> compare_session_key key key')
  in
  let room_keys, withheld =
    List.fold_right
      (fun ((room, session_id), session) (room_keys, withheld) ->
        let shared_history = session_shared_history t (room, session_id) in
        if shared_history then
          ( ({
               Room_key_export.algorithm = megolm_algorithm;
               room_id = room;
               sender_key =
                 Curve25519.Public.to_base64
                   (Olm.Megolm.Inbound.sender_key session);
               session_id;
               session_key =
                 Olm.Megolm.Inbound.export_at_first_known_index session;
               sender_claimed_keys =
                 (match
                    Olm.Megolm.Inbound.sender_claimed_ed25519_key session
                  with
                 | None -> []
                 | Some key -> [ ("ed25519", Ed25519.Public.to_base64 key) ]);
             }
              : Room_key_export.historic_room_key)
            :: room_keys,
            withheld )
        else
          ( room_keys,
            {
              Room_key_export.algorithm = megolm_algorithm;
              room_id = room;
              session_id;
              sender_key =
                Curve25519.Public.to_base64
                  (Olm.Megolm.Inbound.sender_key session);
              from_device = Some (Did.to_string t.our_device);
              code = "m.history_not_shared";
              reason = Some "The sender disabled sharing encrypted history.";
            }
            :: withheld ))
      sessions ([], [])
  in
  let propagated_withheld =
    List.filter_map
      (fun (w : withheld) ->
        match w.sender_key with
        | Some sender_key
          when Rid.equal w.room_id room_id
               && String.equal w.code "m.history_not_shared" ->
            Some
              {
                Room_key_export.algorithm = megolm_algorithm;
                room_id = w.room_id;
                session_id = w.session_id;
                sender_key;
                from_device = Option.map Did.to_string w.from_device;
                code = w.code;
                reason = w.reason;
              }
        | _ -> None)
      t.withheld
  in
  let withheld =
    List.sort_uniq
      (fun (a : Room_key_export.history_not_shared)
           (b : Room_key_export.history_not_shared) ->
        let c = Rid.compare a.room_id b.room_id in
        if c <> 0 then c
        else
          let c = Sid.compare a.session_id b.session_id in
          if c <> 0 then c else String.compare a.sender_key b.sender_key)
      (withheld @ propagated_withheld)
  in
  { Room_key_export.room_keys; withheld }

let import_room_key_bundle t ~room_id ~sender
    (bundle : Room_key_export.room_key_bundle) =
  let imported =
    List.fold_left
      (fun n (key : Room_key_export.historic_room_key) ->
        if not (Rid.equal key.room_id room_id) then n
        else
          (* A historic bundle is shareable by definition. The forwarding
             chain is deliberately not accepted from this wire format. The
             caller is responsible for authenticating [sender] and applying
             the trust gate before invoking this bounded pure API. *)
          let portable : Room_key_export.room_key =
            {
              algorithm = key.algorithm;
              room_id = key.room_id;
              sender_key = key.sender_key;
              session_id = key.session_id;
              session_key = key.session_key;
              sender_claimed_keys = key.sender_claimed_keys;
              forwarding_curve25519_key_chain = [];
              shared_history = true;
            }
          in
          let result = import_room_keys t [ portable ] in
          if result.imported_count = 0 then n
          else begin
            t.metas <-
              List.map
                (fun (m : Session_meta.t) ->
                  if
                    Rid.equal m.room_id room_id
                    && Sid.equal m.session_id key.session_id
                  then { m with sender = Some sender }
                  else m)
                t.metas;
            n + result.imported_count
          end)
      0 bundle.room_keys
  in
  List.iter
    (fun (w : Room_key_export.history_not_shared) ->
      if
        Rid.equal w.room_id room_id
        && String.equal w.algorithm megolm_algorithm
        && String.equal w.code "m.history_not_shared"
      then
        match Curve25519.Public.of_base64 w.sender_key with
        | Error _ -> ()
        | Ok _ ->
            let from_device =
              Option.bind w.from_device (fun value ->
                  Result.to_option (Did.of_string value))
            in
            let value =
              {
                room_id = w.room_id;
                session_id = w.session_id;
                code = w.code;
                reason = w.reason;
                sender_key = Some w.sender_key;
                from_device;
                sender_user = Some sender;
              }
            in
            t.withheld <-
              value
              :: List.filter
                   (fun (old : withheld) ->
                     not
                       (Rid.equal old.room_id value.room_id
                       && Sid.equal old.session_id value.session_id))
                   t.withheld)
    bundle.withheld;
  imported

type decrypted_event = {
  decrypted_type : string;
  decrypted_content : Jsont.json;
  decrypted_room_id : Rid.t;
  decrypted_sender : Uid.t;
  decrypted_sender_key : Curve25519.Public.t;
  decrypted_claimed_ed25519 : Ed25519.Public.t option;
  decrypted_session_id : Sid.t;
  decrypted_message_index : int;
  decrypted_verification : verification_state;
}

type decrypt_error =
  | Not_encrypted
  | Unsupported_algorithm of string
  | Malformed of string
  | Unknown_session of {
      room_id : Rid.t;
      session_id : Sid.t;
      sender_key : string option;
      sender : Uid.t;
    }
  | Unknown_message_index of {
      room_id : Rid.t;
      session_id : Sid.t;
      sender_key : string option;
      sender : Uid.t;
      message_index : int;
      first_known : int;
    }
  | Megolm_error of string
  | Room_mismatch of { expected : Rid.t; got : string }
  | Not_trusted of {
      requirement : trust_requirement;
      sender : Uid.t;
      verification : verification_state;
    }

let withheld_cause = function
  | { code = "m.unverified"; _ } -> Withheld_for_unverified_or_insecure_device
  | { code = "m.history_not_shared"; _ } -> Unknown
  | _ -> Withheld_by_sender

let event_membership (event : Ev.Raw_event.t) =
  Option.bind event.unsigned Ev.Unsigned.membership

let event_predates_device (event : Ev.Raw_event.t) context =
  match context.device_created_at with
  | None -> false
  | Some created ->
      Int64.compare
        (Ev.Timestamp.to_ms event.origin_server_ts)
        (Ev.Timestamp.to_ms (Ev.Timestamp.of_ptime created))
      < 0

let historical_utd_cause context =
  if not context.backup_exists then Historical_message_and_backup_is_disabled
  else if (not context.backup_configured) && not context.local_device_verified
  then Historical_message_and_device_is_unverified
  else Unknown

let classify_utd (event : Ev.Raw_event.t) error (context : utd_context) =
  match context.withheld with
  | Some w when w.code <> "m.history_not_shared" -> withheld_cause w
  | _ -> (
      match error with
      | Not_trusted { verification = Verification_violation; _ } ->
          Verification_violation
      | Not_trusted { verification = Device_info; _ } -> Unsigned_device
      | Not_trusted { verification = Unknown_device; _ } -> Unknown_device
      | (Unknown_session _ | Unknown_message_index _)
        when event_membership event = Some "leave" ->
          Sent_before_we_joined
      | (Unknown_session _ | Unknown_message_index _)
        when event_predates_device event context ->
          historical_utd_cause context
      | _ -> Unknown)

let withheld_for t ~room_id ~session_id =
  List.find_opt
    (fun (w : withheld) ->
      Rid.equal w.room_id room_id && Sid.equal w.session_id session_id)
    t.withheld

let pending_key_bundle t ~room_id =
  List.find_opt
    (fun (p : pending_key_bundle) -> Rid.equal p.room_id room_id)
    t.pending_key_bundles

let pending_key_bundles t = t.pending_key_bundles
let received_key_bundles t = t.received_key_bundles

let clear_received_key_bundle t ~room_id ~sender =
  let old = t.received_key_bundles in
  t.received_key_bundles <-
    List.filter
      (fun (bundle : received_key_bundle) ->
        not (Rid.equal bundle.room_id room_id && Uid.equal bundle.sender sender))
      old;
  List.length old <> List.length t.received_key_bundles

let room_key_bundle_sender_is_trusted t (bundle : received_key_bundle) =
  match find_device_by_curve25519 t bundle.sender_key with
  | None -> false
  | Some device when not (Uid.equal device.user_id bundle.sender) -> false
  | Some device -> (
      match
        ( Ed25519.Public.of_base64 bundle.sender_ed25519,
          device_ed25519 device,
          List.find_opt
            (fun identity -> Uid.equal identity.identity_user_id bundle.sender)
            t.identities )
      with
      | Ok claimed, Some published, Some identity
        when Ed25519.Public.equal claimed published
             && identity.identity_status <> Verification_violation ->
          valid_cross_signed_device identity
            {
              Keys.user_id = device.user_id;
              device_id = device.device_id;
              algorithms = device.algorithms;
              keys = device.keys;
              signatures = device.signatures;
              dehydrated = device.dehydrated;
              unsigned = None;
            }
      | _ -> false)

let record_invite_acceptance ?now t ~room_id ~inviter =
  let invite_accepted_at = Option.value now ~default:(wall_clock ()) in
  let pending = { room_id; inviter; invite_accepted_at } in
  t.pending_key_bundles <-
    pending
    :: List.filter
         (fun (p : pending_key_bundle) -> not (Rid.equal p.room_id room_id))
         t.pending_key_bundles

let clear_pending_key_bundle t ~room_id =
  let old = t.pending_key_bundles in
  t.pending_key_bundles <-
    List.filter
      (fun (p : pending_key_bundle) -> not (Rid.equal p.room_id room_id))
      old;
  List.length old <> List.length t.pending_key_bundles

let room_key_bundle_window = Ptime.Span.of_int_s (24 * 60 * 60)

let should_accept_room_key_bundle ?now t ~room_id ~joined ~sender () =
  if not joined then false
  else
    match pending_key_bundle t ~room_id with
    | None -> false
    | Some pending when not (Uid.equal pending.inviter sender) -> false
    | Some pending ->
        let at = Option.value now ~default:(wall_clock ()) in
        Ptime.compare at pending.invite_accepted_at >= 0
        && Ptime.Span.compare
             (Ptime.diff at pending.invite_accepted_at)
             room_key_bundle_window
           < 0

let accept_room_key_bundle ?now t ~room_id ~joined ~sender bundle =
  if not (should_accept_room_key_bundle ?now t ~room_id ~joined ~sender ()) then
    None
  else
    let imported = import_room_key_bundle t ~room_id ~sender bundle in
    ignore (clear_pending_key_bundle t ~room_id);
    Some imported

let clear_expired_pending_key_bundles ?now t =
  let at = Option.value now ~default:(wall_clock ()) in
  let expired, live =
    List.partition
      (fun (pending : pending_key_bundle) ->
        Ptime.compare at pending.invite_accepted_at < 0
        || Ptime.Span.compare
             (Ptime.diff at pending.invite_accepted_at)
             room_key_bundle_window
           >= 0)
      t.pending_key_bundles
  in
  t.pending_key_bundles <- live;
  List.map (fun (pending : pending_key_bundle) -> pending.room_id) expired

let utd_context ?device_created_at t error =
  let withheld =
    match error with
    | Unknown_session { room_id; session_id; _ }
    | Unknown_message_index { room_id; session_id; _ } ->
        withheld_for t ~room_id ~session_id
    | _ -> None
  in
  {
    device_created_at;
    backup_exists = Option.is_some t.backup.version;
    backup_configured = Option.is_some t.backup.decryption_key;
    local_device_verified =
      identity_status t t.our_user = Some Identity_verified;
    withheld;
  }

let pp_decrypt_error ppf = function
  | Not_encrypted -> Format.pp_print_string ppf "the event is not encrypted"
  | Unsupported_algorithm a ->
      Format.fprintf ppf "unsupported encryption algorithm %S" a
  | Malformed m -> Format.fprintf ppf "malformed m.room.encrypted content: %s" m
  | Unknown_session { session_id; _ } ->
      Format.fprintf ppf "no Megolm session %a" Sid.pp session_id
  | Unknown_message_index { message_index; first_known; _ } ->
      Format.fprintf ppf
        "Megolm session starts at message index %d, after event index %d"
        first_known message_index
  | Megolm_error m -> Format.fprintf ppf "Megolm decryption failed: %s" m
  | Room_mismatch { expected; got } ->
      Format.fprintf ppf "the plaintext claims room %s, not %a" got Rid.pp
        expected
  | Not_trusted { sender; _ } ->
      Format.fprintf ppf "sender %a does not meet the room trust requirement"
        Uid.pp sender

let session_meta t room_id session_id =
  List.find_opt
    (fun (m : Session_meta.t) ->
      eq_session (m.room_id, m.session_id) (room_id, session_id))
    t.metas

let verification_of t ~room_id ~session_id ~sender ~sender_key ~claimed_ed25519
    =
  let recorded_sender_matches =
    match session_meta t room_id session_id with
    | Some { sender = Some recorded; _ } -> Uid.equal recorded sender
    | Some { sender = None; _ } | None -> true
  in
  if not recorded_sender_matches then Unknown_device
  else
    match find_device_by_curve25519 t sender_key with
    | None -> Unknown_device
    | Some d when not (Uid.equal d.user_id sender) -> Unknown_device
    | Some d -> (
        match (claimed_ed25519, device_ed25519 d) with
        | Some claimed, Some published
          when Ed25519.Public.equal claimed published -> (
            let status = identity_status t sender in
            if status = Some Verification_violation then Verification_violation
            else if d.trust = Verified then Verified_device
            else
              let cross_signed =
                match status with
                | Some _ ->
                    Option.exists
                      (fun i ->
                        valid_cross_signed_device i
                          {
                            Keys.user_id = d.user_id;
                            device_id = d.device_id;
                            algorithms = d.algorithms;
                            keys = d.keys;
                            signatures = d.signatures;
                            dehydrated = d.dehydrated;
                            unsigned = None;
                          })
                      (List.find_opt
                         (fun i -> Uid.equal i.identity_user_id sender)
                         t.identities)
                | None -> false
              in
              match (status, cross_signed) with
              | Some Identity_verified, true -> Sender_verified
              | Some _, true -> Sender_unverified
              | Some _, false -> Device_info
              | None, _ -> Device_info)
        | _ -> Unknown_device)

let session_is_legacy t room session_id =
  match
    List.find_opt
      (fun (m : Session_meta.t) ->
        eq_session (m.room_id, m.session_id) (room, session_id))
      t.metas
  with
  | None -> true
  | Some m -> m.sender = None && m.sender_ed25519 = None

let trust_satisfies requirement ~legacy verification =
  match requirement with
  | Untrusted -> true
  | Cross_signed_or_legacy -> (
      match verification with
      | Verified_device | Sender_verified | Sender_unverified -> true
      | Device_info | Unknown_device -> legacy
      | Verification_violation -> false
      | Unverified_device -> legacy)
  | Cross_signed -> (
      match verification with
      | Verified_device | Sender_verified | Sender_unverified -> true
      | Device_info | Unknown_device | Unverified_device
      | Verification_violation ->
          false)

let decrypt_megolm t room (f : Ev.Encrypted.Megolm.t) ~sender ~requirement =
  match find_assoc eq_session (room, f.session_id) t.inbound with
  | None ->
      Error
        (Unknown_session
           {
             room_id = room;
             session_id = f.session_id;
             sender_key = f.sender_key;
             sender;
           })
  | Some session -> (
      let session_sender_key = Olm.Megolm.Inbound.sender_key session in
      let mismatched =
        match f.sender_key with
        | None -> false
        | Some k ->
            not
              (String.equal k (Curve25519.Public.to_base64 session_sender_key))
      in
      if mismatched then
        Error
          (Megolm_error "the event's sender_key is not the session's sender_key")
      else
        match Olm.Megolm.Inbound.decrypt session ~ciphertext:f.ciphertext with
        | Error (Olm.Unknown_message_index { index; first_known }) ->
            Error
              (Unknown_message_index
                 {
                   room_id = room;
                   session_id = f.session_id;
                   sender_key = f.sender_key;
                   sender;
                   message_index = index;
                   first_known;
                 })
        | Error e -> Error (Megolm_error (Format.asprintf "%a" Olm.pp_error e))
        | Ok d -> (
            match
              Jsont_bytesrw.decode_string megolm_payload_jsont d.plaintext
            with
            | Error msg -> Error (Malformed msg)
            | Ok p ->
                if p.mp_room_id <> "" && p.mp_room_id <> Rid.to_string room then
                  Error (Room_mismatch { expected = room; got = p.mp_room_id })
                else
                  let claimed =
                    Olm.Megolm.Inbound.sender_claimed_ed25519_key session
                  in
                  let verification =
                    verification_of t ~room_id:room ~session_id:f.session_id
                      ~sender ~sender_key:session_sender_key
                      ~claimed_ed25519:claimed
                  in
                  if
                    not
                      (trust_satisfies requirement
                         ~legacy:(session_is_legacy t room f.session_id)
                         verification)
                  then Error (Not_trusted { requirement; sender; verification })
                  else
                    Ok
                      {
                        decrypted_type = p.mp_type;
                        decrypted_content = p.mp_content;
                        decrypted_room_id = room;
                        decrypted_sender = sender;
                        decrypted_sender_key = session_sender_key;
                        decrypted_claimed_ed25519 = claimed;
                        decrypted_session_id = f.session_id;
                        decrypted_message_index = d.message_index;
                        decrypted_verification = verification;
                      }))

let decrypt_room_event ?trust_requirement t room (event : Ev.Raw_event.t) =
  let trust_requirement =
    Option.value trust_requirement ~default:t.trust_requirement
  in
  if Ev.Event_type.to_string event.type_ <> "m.room.encrypted" then
    Error Not_encrypted
  else
    (* An unsupported algorithm says the room is beyond us; a parse failure
       says this one event is malformed. Worth telling apart. *)
    match Jsont.Json.decode algorithm_of_content_jsont event.content with
    | Ok a when a <> megolm_algorithm -> Error (Unsupported_algorithm a)
    | Error msg -> Error (Malformed msg)
    | Ok _ -> (
        match Jsont.Json.decode Ev.Encrypted.Megolm.jsont event.content with
        | Error msg -> Error (Malformed msg)
        | Ok f ->
            decrypt_megolm t room f ~sender:event.sender
              ~requirement:trust_requirement)

let request_room_key t ~room_id ~session_id ?sender_key () =
  let content =
    {
      kr_action = "request";
      kr_requesting_device_id = Did.to_string t.our_device;
      kr_request_id = Random.txn_id t.random;
      kr_body =
        Some
          {
            rq_algorithm = megolm_algorithm;
            rq_room_id = Rid.to_string room_id;
            rq_session_id = Sid.to_string session_id;
            rq_sender_key = Option.map Curve25519.Public.to_base64 sender_key;
          };
    }
  in
  To_device
    {
      event_type = "m.room_key_request";
      txn_id = Random.txn_id t.random;
      messages =
        [
          ( t.our_user,
            [ (To_device.All, json_of room_key_request_content_jsont content) ]
          );
        ];
    }

let ptime_diff_ms a b =
  Int64.of_float (Ptime.Span.to_float_s (Ptime.diff a b) *. 1000.)

let outbound_needs_rotation t ~settings ~members o =
  let count = Olm.Megolm.Outbound.message_index o.os_session in
  let age = ptime_diff_ms (wall_clock ()) o.os_created in
  let member_left =
    List.exists (fun u -> not (List.exists (Uid.equal u) members)) o.os_members
  in
  let device_overshared (user_id, device_id) =
    mem_uid user_id members
    && not
         (List.exists
            (fun (d : device) ->
              Did.equal d.device_id device_id
              && d.trust <> Blacklisted
              && Option.is_some (device_curve25519 d))
            (devices_of t user_id))
  in
  let device_left_or_blacklisted =
    List.exists device_overshared (Olm.Megolm.Outbound.shared_with o.os_session)
  in
  count >= settings.rotation_period_msgs
  || Int64.compare age settings.rotation_period_ms >= 0
  || member_left || device_left_or_blacklisted

(* Our own outbound session is imported as an inbound one so that this device
   can read back what it sent; the server echoes our events like anybody
   else's. *)
let adopt_own_session t ~room_id session =
  let ed, curve = identity_keys t in
  match
    Olm.Megolm.Inbound.of_session_key ~claimed_ed25519:ed ~sender_key:curve
      ~room_id
      ~session_key:(Olm.Megolm.Outbound.session_key session)
      ()
  with
  | Ok s ->
      ignore
        (store_inbound t ~sender:t.our_user
           ~sender_ed25519:(Ed25519.Public.to_base64 ed)
           s)
  | Error e ->
      Log.warn (fun m ->
          m "Could not adopt our own Megolm session: %a" Olm.pp_error e)

(* Withheld notices naming a room session only describe that particular
   outbound session. Once it is rotated they must not suppress a notice for
   the new session.  [m.no_olm] is device-scoped by design and remains useful
   across room-session rotations. *)
let discard_rotated_room_withheld t room =
  t.outbound_withhelds <-
    List.filter
      (fun notice ->
        String.equal notice.ow_code "m.no_olm"
        || not (Rid.equal notice.ow_room_id room))
      t.outbound_withhelds

let get_outbound t ~room ~settings ~members =
  let members = List.sort_uniq Uid.compare members in
  match find_assoc Rid.equal room t.outbound with
  | Some o when not (outbound_needs_rotation t ~settings ~members o) -> o
  | (Some _ | None) as previous ->
      (match previous with
      | Some _ -> discard_rotated_room_withheld t room
      | None -> ());
      let session =
        Olm.Megolm.Outbound.create
          ~rotation_period:
            (Ptime.Span.of_float_s
               (Int64.to_float settings.rotation_period_ms /. 1000.)
            |> Option.value ~default:(Ptime.Span.of_int_s 604_800))
          ~rotation_messages:settings.rotation_period_msgs ~random:t.random
          ~room_id:room ()
      in
      adopt_own_session t ~room_id:room session;
      let o =
        {
          os_session = session;
          os_created = wall_clock ();
          os_members = members;
        }
      in
      t.outbound <- (room, o) :: rem_assoc Rid.equal room t.outbound;
      o

let olm_session_with t (d : device) =
  Option.bind (device_curve25519 d) (fun their_identity_key ->
      Olm.Machine.find_olm_session t.machine ~their_identity_key)

let unwedging_interval_s = 60. *. 60. *. 1.

let most_recent_session_creation t key =
  List.fold_left
    (fun latest session ->
      if Curve25519.Public.equal (Olm.Session.their_identity_key session) key
      then
        match latest with
        | None -> Some (Olm.Session.creation_time session)
        | Some at when Ptime.compare at (Olm.Session.creation_time session) < 0
          ->
            Some (Olm.Session.creation_time session)
        | Some _ -> latest
      else latest)
    None
    (Olm.Machine.olm_sessions t.machine)

let session_is_old_enough_to_unwedge t ~now key =
  match most_recent_session_creation t key with
  | None -> false
  (* Match Rust's [checked_sub(...).unwrap_or(true)]: a clock moving behind
     the session creation time is treated as old enough to attempt recovery,
     rather than suppressing recovery indefinitely. *)
  | Some created when Ptime.compare now created < 0 -> true
  | Some created ->
      Ptime.Span.to_float_s (Ptime.diff now created) > unwedging_interval_s

let mark_wedged t ~now ~sender ~identity_key =
  match find_device_by_curve25519 t identity_key with
  | None -> ()
  | Some device
    when Uid.equal device.user_id sender
         && session_is_old_enough_to_unwedge t ~now identity_key
         && not
              (List.exists
                 (fun w ->
                   Uid.equal w.wd_user_id sender
                   && Did.equal w.wd_device_id device.device_id)
                 t.wedged) ->
      t.wedged <-
        { wd_user_id = sender; wd_device_id = device.device_id } :: t.wedged
  | Some _ -> ()

let is_wedged t (d : device) =
  List.exists
    (fun w ->
      Uid.equal w.wd_user_id d.user_id && Did.equal w.wd_device_id d.device_id)
    t.wedged

(* Devices of [members] this machine may address: not our own and with an
   identity key. A blacklisted device is retained here so that the caller can
   send it an explicit [m.blacklisted] notice instead of silently dropping it.
*)
let addressable_devices t ~members =
  List.concat_map
    (fun u ->
      List.filter
        (fun (d : device) ->
          (not (is_device d t.our_user t.our_device))
          && Option.is_some (device_curve25519 d))
        (devices_of t u))
    members

let shareable_devices t ~members =
  List.filter
    (fun (d : device) -> d.trust <> Blacklisted)
    (addressable_devices t ~members)

let blacklisted_devices t ~members =
  List.filter
    (fun (d : device) -> d.trust = Blacklisted)
    (addressable_devices t ~members)

let share_room_key_bundle t ~room_id ~recipient ~file =
  let* content = encode_json room_key_bundle_content_jsont { room_id; file } in
  let messages =
    List.fold_left
      (fun messages (d : device) ->
        match olm_session_with t d with
        | None -> messages
        | Some session -> (
            match
              olm_encrypt_for t ~session ~recipient d
                ~event_type:"m.room_key_bundle" ~content
            with
            | Error msg ->
                Log.warn (fun m ->
                    m "Could not encrypt room-key bundle for %a: %s" Did.pp
                      d.device_id msg);
                messages
            | Ok encrypted -> add_message messages d encrypted))
      []
      (shareable_devices t ~members:[ recipient ])
  in
  if messages = [] then Ok None
  else
    Ok
      (Some
         (Room_key_bundle_share
            { room_id; txn_id = Random.txn_id t.random; messages }))

let recipients_lacking t ~members session =
  List.filter
    (fun (d : device) ->
      not
        (Olm.Megolm.Outbound.is_shared_with session ~user_id:d.user_id
           ~device_id:d.device_id))
    (shareable_devices t ~members)

let ensure_sessions ?now t ~members =
  let now = Option.value now ~default:(wall_clock ()) in
  let stale =
    List.filter
      (fun u -> (not (mem_uid u t.tracked)) || mem_uid u t.outdated)
      members
  in
  let query =
    if stale = [] then []
    else begin
      track_users t stale;
      [ Keys_query stale ]
    end
  in
  let needing =
    List.filter
      (fun d ->
        Option.is_none (olm_session_with t d)
        && not
             (claim_failure_active t ~now ~user_id:d.user_id
                ~device_id:d.device_id))
      (shareable_devices t ~members)
  in
  let forced = List.filter (is_wedged t) (shareable_devices t ~members) in
  let claim_devices =
    List.fold_left
      (fun seen d ->
        if
          List.exists
            (fun (old : device) ->
              Uid.equal old.user_id d.user_id
              && Did.equal old.device_id d.device_id)
            seen
        then seen
        else d :: seen)
      [] (needing @ forced)
  in
  let claim =
    if claim_devices = [] then []
    else
      let keys =
        List.fold_left
          (fun acc (d : device) ->
            let existing =
              Option.value ~default:[]
                (Option.map snd
                   (List.find_opt (fun (u, _) -> Uid.equal u d.user_id) acc))
            in
            (d.user_id, (d.device_id, otk_algorithm) :: existing)
            :: List.filter (fun (u, _) -> not (Uid.equal u d.user_id)) acc)
          [] claim_devices
      in
      remember_claim t keys;
      [ Keys_claim keys ]
  in
  query @ claim

type secret_request_content = {
  src_action : string;
  src_requesting_device_id : string;
  src_request_id : string;
  src_name : string option;
}

let secret_request_content_jsont =
  Jsont.Object.(
    map (fun src_action src_requesting_device_id src_request_id src_name ->
        { src_action; src_requesting_device_id; src_request_id; src_name })
    |> mem "action" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "request")
         ~enc:(fun t -> t.src_action)
    |> mem "requesting_device_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.src_requesting_device_id)
    |> mem "request_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.src_request_id)
    |> opt_mem "name" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.src_name)
    |> finish)

type secret_send_content = { ssc_request_id : string; ssc_secret : string }

let secret_send_content_jsont =
  Jsont.Object.(
    map (fun ssc_request_id ssc_secret -> { ssc_request_id; ssc_secret })
    |> mem "request_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.ssc_request_id)
    |> mem "secret" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.ssc_secret)
    |> finish)

let own_devices t =
  devices_of t t.our_user
  |> List.filter (fun (d : device) ->
      (not (Did.equal d.device_id t.our_device))
      && Option.is_some (device_curve25519 d))

let own_verified_devices t =
  List.filter (fun (d : device) -> d.trust = Verified) (own_devices t)

let secret_messages t ~event_type ~content devices =
  List.fold_left
    (fun messages (d : device) ->
      match olm_session_with t d with
      | None -> messages
      | Some session -> (
          match
            olm_encrypt_for t ~session ~recipient:t.our_user d ~event_type
              ~content
          with
          | Error msg ->
              Log.warn (fun m ->
                  m "Could not encrypt %s for %a: %s" event_type Did.pp
                    d.device_id msg);
              messages
          | Ok encrypted -> add_message messages d encrypted))
    [] devices

let secret_messages_for_all t ~event_type ~content =
  secret_messages t ~event_type ~content (own_verified_devices t)

let plain_secret_messages_for_all t ~content =
  [ (t.our_user, [ (To_device.All, content) ]) ]

let secret_request_content ~action ~requesting_device_id ~request_id ?name () =
  json_of secret_request_content_jsont
    {
      src_action = action;
      src_requesting_device_id = Did.to_string requesting_device_id;
      src_request_id = request_id;
      src_name = name;
    }

let secret_cancel t (r : secret_request) =
  {
    sc_txn_id = Random.txn_id t.random;
    sc_content =
      secret_request_content ~action:"request_cancellation"
        ~requesting_device_id:t.our_device ~request_id:r.sr_request_id ();
    sc_sent = false;
    sc_messages = None;
  }

let secret_send_content ~request_id ~secret =
  json_of secret_send_content_jsont
    { ssc_request_id = request_id; ssc_secret = secret }

let secret t ~name = List.assoc_opt name t.secret_values

let store_secret t ~name ~value =
  t.secret_values <- (name, value) :: List.remove_assoc name t.secret_values

let request_secret t ~name =
  if name = "" then Error (Error.Json_error "secret name is empty")
  else
    let request_id = Random.txn_id t.random in
    let request_cancel r = Some (secret_cancel t r) in
    let requests =
      List.map
        (fun r ->
          if String.equal r.sr_name name && r.sr_cancel = None then
            { r with sr_cancel = request_cancel r }
          else r)
        t.secret_request_state
    in
    let request =
      {
        sr_name = name;
        sr_request_id = request_id;
        sr_txn_id = Random.txn_id t.random;
        sr_content =
          secret_request_content ~action:"request"
            ~requesting_device_id:t.our_device ~request_id ~name ();
        sr_sent = false;
        sr_messages = None;
        sr_cancel = None;
      }
    in
    t.secret_request_state <- request :: requests;
    Ok request_id

let cancel_secret_request t ~name =
  match
    List.find_opt
      (fun r -> String.equal r.sr_name name && r.sr_cancel = None)
      t.secret_request_state
  with
  | None -> false
  | Some r ->
      t.secret_request_state <-
        List.map
          (fun candidate ->
            if String.equal candidate.sr_request_id r.sr_request_id then
              { r with sr_cancel = Some (secret_cancel t r) }
            else candidate)
          t.secret_request_state;
      true

let secret_lifecycle_requests t =
  let requests =
    List.map
      (fun r ->
        match (r.sr_sent, r.sr_messages, r.sr_cancel) with
        | _, _, Some c when (not c.sc_sent) && c.sc_messages = None ->
            let messages =
              plain_secret_messages_for_all t ~content:c.sc_content
            in
            { r with sr_cancel = Some { c with sc_messages = Some messages } }
        | false, None, None ->
            let messages =
              plain_secret_messages_for_all t ~content:r.sr_content
            in
            { r with sr_messages = Some messages }
        | _ -> r)
      t.secret_request_state
  in
  let sends_state =
    List.map
      (fun s ->
        if s.ss_sent || s.ss_messages <> None then s
        else
          let messages =
            secret_messages t ~event_type:"m.secret.send" ~content:s.ss_content
              (List.filter
                 (fun (d : device) ->
                   Uid.equal d.user_id s.ss_user_id
                   && Did.equal d.device_id s.ss_device_id)
                 (devices_of t s.ss_user_id))
          in
          if messages = [] then s else { s with ss_messages = Some messages })
      t.secret_send_state
  in
  t.secret_request_state <- requests;
  t.secret_send_state <- sends_state;
  let request_of r =
    match r.sr_messages with
    | Some messages when messages <> [] ->
        Some
          (To_device
             { event_type = "m.secret.request"; txn_id = r.sr_txn_id; messages })
    | _ -> None
  in
  let cancel_of r =
    match r.sr_cancel with
    | None -> None
    | Some c when c.sc_sent -> None
    | Some c ->
        Option.map
          (fun messages ->
            To_device
              {
                event_type = "m.secret.request";
                txn_id = c.sc_txn_id;
                messages;
              })
          c.sc_messages
  in
  let sends =
    List.filter_map
      (fun s ->
        if s.ss_sent then None
        else
          Option.map
            (fun messages ->
              To_device
                {
                  event_type = "m.room.encrypted";
                  txn_id = s.ss_txn_id;
                  messages;
                })
            (Option.bind s.ss_messages (fun messages ->
                 if messages = [] then None else Some messages)))
      sends_state
  in
  List.filter_map
    (fun r ->
      if r.sr_cancel <> None then cancel_of r
      else if r.sr_sent then None
      else request_of r)
    requests
  @ sends

let outgoing_requests t = outgoing_requests_base t @ secret_lifecycle_requests t

let secret_sender_device t ~sender ~sender_key =
  match sender_key with
  | None -> None
  | Some key -> (
      match find_device_by_curve25519 t key with
      | Some d
        when Uid.equal sender t.our_user
             && Uid.equal d.user_id sender
             && (not (Did.equal d.device_id t.our_device))
             && d.trust = Verified ->
          Some d
      | _ -> None)

let queue_secret_send t ~device ~request_id ~secret =
  if
    not
      (List.exists
         (fun s ->
           String.equal s.ss_request_id request_id
           && Did.equal s.ss_device_id device.device_id)
         t.secret_send_state)
  then
    t.secret_send_state <-
      {
        ss_request_id = request_id;
        ss_user_id = device.user_id;
        ss_device_id = device.device_id;
        ss_txn_id = Random.txn_id t.random;
        ss_content = secret_send_content ~request_id ~secret;
        ss_sent = false;
        ss_messages = None;
      }
      :: t.secret_send_state

let secret_requesting_device t ~sender ~sender_key requesting_device_id =
  match Did.of_string requesting_device_id with
  | Error _ -> None
  | Ok requested -> (
      match sender_key with
      | Some _ -> (
          match secret_sender_device t ~sender ~sender_key with
          | Some d when Did.equal d.device_id requested -> Some d
          | _ -> None)
      | None -> (
          match find_device t sender ~device_id:requested with
          | Some d
            when Uid.equal sender t.our_user
                 && (not (Did.equal d.device_id t.our_device))
                 && d.trust = Verified
                 && Option.is_some (device_curve25519 d) ->
              Some d
          | _ -> None))

let handle_secret_request t ~sender ~sender_key content =
  match Jsont.Json.decode secret_request_content_jsont content with
  | Error _ -> ()
  | Ok request -> (
      if String.equal request.src_action "request_cancellation" then
        let sender_device =
          Option.map
            (fun d -> d.device_id)
            (secret_requesting_device t ~sender ~sender_key
               request.src_requesting_device_id)
        in
        t.secret_send_state <-
          List.filter
            (fun s ->
              not
                (String.equal s.ss_request_id request.src_request_id
                && Option.exists (Did.equal s.ss_device_id) sender_device))
            t.secret_send_state
      else if String.equal request.src_action "request" then
        match
          ( secret_requesting_device t ~sender ~sender_key
              request.src_requesting_device_id,
            Option.bind request.src_name (fun name ->
                List.assoc_opt name t.secret_values) )
        with
        | Some requesting_device, Some secret ->
            queue_secret_send t ~device:requesting_device
              ~request_id:request.src_request_id ~secret
        | _ -> ())

let handle_secret_send t ~sender ~sender_key content =
  match
    ( Jsont.Json.decode secret_send_content_jsont content,
      secret_sender_device t ~sender ~sender_key )
  with
  | Ok send, Some device ->
      let matching =
        List.find_opt
          (fun r ->
            String.equal r.sr_request_id send.ssc_request_id
            && r.sr_cancel = None)
          t.secret_request_state
      in
      Option.iter
        (fun r ->
          store_secret t ~name:r.sr_name ~value:send.ssc_secret;
          if r.sr_cancel = None then
            t.secret_request_state <-
              List.map
                (fun candidate ->
                  if String.equal candidate.sr_request_id r.sr_request_id then
                    { r with sr_cancel = Some (secret_cancel t r) }
                  else candidate)
                t.secret_request_state;
          ignore device)
        matching
  | _ -> ()

let share_requests t ~room ~session devices =
  let* room_key =
    encode_json Ev.Room_key_content.jsont
      {
        room_id = room;
        session_id = Olm.Megolm.Outbound.session_id session;
        session_key = Olm.Megolm.Outbound.session_key session;
        shared_history = false;
      }
  in
  let messages =
    List.fold_left
      (fun messages (d : device) ->
        match olm_session_with t d with
        | None ->
            Log.warn (fun m ->
                m "No Olm session with %a of %a; not sharing the room key"
                  Did.pp d.device_id Uid.pp d.user_id);
            messages
        | Some olm_session -> (
            match
              olm_encrypt_for t ~session:olm_session ~recipient:d.user_id d
                ~event_type:"m.room_key" ~content:room_key
            with
            | Error msg ->
                Log.warn (fun m ->
                    m "Could not encrypt the room key for %a: %s" Did.pp
                      d.device_id msg);
                messages
            | Ok content -> add_message messages d content))
      [] devices
  in
  if messages = [] then Ok []
  else
    Ok
      [
        Room_key_share
          {
            room_id = room;
            session_id = Olm.Megolm.Outbound.session_id session;
            txn_id = Random.txn_id t.random;
            messages;
          };
      ]

let withheld_content t ~room ~session_id ~device ~code =
  let sender_key = Curve25519.Public.to_base64 (our_curve25519 t) in
  let reason =
    match code with
    | "m.blacklisted" -> "The sender has blocked you."
    | "m.no_olm" -> "Unable to establish a secure channel."
    | _ -> code
  in
  let mem name value = Jsont.Json.mem (Jsont.Json.name name) value in
  let fields =
    [
      mem "algorithm" (Jsont.Json.string megolm_algorithm);
      mem "code" (Jsont.Json.string code);
      mem "reason" (Jsont.Json.string reason);
      mem "sender_key" (Jsont.Json.string sender_key);
      mem "from_device" (Jsont.Json.string (Did.to_string t.our_device));
    ]
  in
  let fields =
    if String.equal code "m.no_olm" then fields
    else
      mem "room_id" (Jsont.Json.string (Rid.to_string room))
      :: mem "session_id" (Jsont.Json.string (Sid.to_string session_id))
      :: fields
  in
  Jsont.Json.object' fields

let notice_is_same_scope notice ~room ~session_id ~device ~code =
  Rid.equal notice.ow_room_id room
  && Sid.equal notice.ow_session_id session_id
  && Uid.equal notice.ow_user_id device.user_id
  && Did.equal notice.ow_device_id device.device_id
  && String.equal notice.ow_code code

let notice_is_same_device notice ~device ~code =
  Uid.equal notice.ow_user_id device.user_id
  && Did.equal notice.ow_device_id device.device_id
  && String.equal notice.ow_code code

let ensure_withheld_notice t ~room ~session_id ~device ~code ~txn_id =
  let exists =
    if String.equal code "m.no_olm" then
      List.exists
        (fun n -> notice_is_same_device n ~device ~code)
        t.outbound_withhelds
    else
      List.exists
        (fun n -> notice_is_same_scope n ~room ~session_id ~device ~code)
        t.outbound_withhelds
  in
  if not exists then
    t.outbound_withhelds <-
      {
        ow_room_id = room;
        ow_session_id = session_id;
        ow_user_id = device.user_id;
        ow_device_id = device.device_id;
        ow_code = code;
        ow_txn_id = txn_id;
        ow_content = withheld_content t ~room ~session_id ~device ~code;
        ow_sent = false;
      }
      :: t.outbound_withhelds

let withheld_requests t ~room ~session_id ~devices =
  let new_devices =
    List.filter
      (fun (device, code) ->
        let exists =
          if String.equal code "m.no_olm" then
            List.exists
              (fun n -> notice_is_same_device n ~device ~code)
              t.outbound_withhelds
          else
            List.exists
              (fun n -> notice_is_same_scope n ~room ~session_id ~device ~code)
              t.outbound_withhelds
        in
        not exists)
      devices
  in
  let new_txn =
    if new_devices = [] then None else Some (Random.txn_id t.random)
  in
  Option.iter
    (fun txn_id ->
      List.iter
        (fun (device, code) ->
          ensure_withheld_notice t ~room ~session_id ~device ~code ~txn_id)
        new_devices)
    new_txn;
  let pending =
    List.filter
      (fun n ->
        (not n.ow_sent)
        && ((Rid.equal n.ow_room_id room && Sid.equal n.ow_session_id session_id)
           || String.equal n.ow_code "m.no_olm"))
      t.outbound_withhelds
  in
  let add_notice groups notice =
    let messages =
      [
        ( notice.ow_user_id,
          [ (To_device.Device notice.ow_device_id, notice.ow_content) ] );
      ]
    in
    match List.assoc_opt notice.ow_txn_id groups with
    | None -> (notice.ow_txn_id, messages) :: groups
    | Some previous ->
        let merged =
          List.map
            (fun (user, devices) ->
              match List.assoc_opt user messages with
              | None -> (user, devices)
              | Some extra -> (user, devices @ extra))
            previous
        in
        let users = List.map fst previous in
        let merged =
          List.fold_left
            (fun acc (user, devices) ->
              if List.mem user users then acc else (user, devices) :: acc)
            merged messages
        in
        (notice.ow_txn_id, merged) :: List.remove_assoc notice.ow_txn_id groups
  in
  List.rev_map
    (fun (txn_id, messages) ->
      To_device { event_type = "m.room_key.withheld"; txn_id; messages })
    (List.fold_left add_notice [] pending)

let encrypt_room_event t room ~event_type ~content ~members =
  let settings =
    Option.value (find_room_settings t room) ~default:default_room_settings
  in
  let o = get_outbound t ~room ~settings ~members in
  let session = o.os_session in
  let lacking = recipients_lacking t ~members session in
  let no_olm_devices =
    List.filter (fun d -> Option.is_none (olm_session_with t d)) lacking
  in
  let blacklisted =
    List.map (fun d -> (d, "m.blacklisted")) (blacklisted_devices t ~members)
  in
  let no_olm = List.map (fun d -> (d, "m.no_olm")) no_olm_devices in
  let* requests =
    match lacking with
    | [] -> Ok []
    | lacking -> share_requests t ~room ~session lacking
  in
  let requests =
    requests
    @ withheld_requests t ~room
        ~session_id:(Olm.Megolm.Outbound.session_id session)
        ~devices:(blacklisted @ no_olm)
  in
  let payload =
    {
      mp_room_id = Rid.to_string room;
      mp_type = event_type;
      mp_content = content;
    }
  in
  let* plaintext =
    match Jsont_bytesrw.encode_string megolm_payload_jsont payload with
    | Ok s -> Ok s
    | Error m -> json_error m
  in
  let message = Olm.Megolm.Outbound.encrypt session plaintext in
  let* encrypted =
    encode_json Ev.Encrypted.Megolm.jsont
      {
        sender_key = Some (Curve25519.Public.to_base64 (our_curve25519 t));
        session_id = Olm.Megolm.Outbound.session_id session;
        device_id = Some t.our_device;
        ciphertext = message.ciphertext;
      }
  in
  Ok (encrypted, requests)

let gossip_reply t ~sender ~(req : room_key_request_content) =
  let refuse reason =
    Log.info (fun m ->
        m "Refusing m.room_key_request %s from %a: %s" req.kr_request_id Uid.pp
          sender reason);
    None
  in
  let requesting_device = Did.of_string req.kr_requesting_device_id in
  match (req.kr_body, requesting_device) with
  | None, _ -> refuse "no body"
  | _, Error (`Msg _) -> refuse "the requesting device id is malformed"
  | Some body, Ok requesting_device -> (
      if not (Uid.equal sender t.our_user) then
        refuse "key requests are only answered for our own devices"
      else if Did.equal requesting_device t.our_device then
        refuse "the request is our own"
      else
        match find_device t t.our_user ~device_id:requesting_device with
        | None -> refuse "unknown requesting device"
        | Some d -> (
            if d.trust <> Verified then
              refuse "the requesting device is not verified"
            else
              match
                (Rid.of_string body.rq_room_id, Sid.of_string body.rq_session_id)
              with
              | Error (`Msg _), _ | _, Error (`Msg _) ->
                  refuse "the request names no session"
              | Ok room, Ok sid -> (
                  match find_assoc eq_session (room, sid) t.inbound with
                  | None -> refuse "we do not hold that session"
                  | Some session -> (
                      match olm_session_with t d with
                      | None ->
                          refuse "no Olm session with the requesting device"
                      | Some olm_session -> (
                          let built =
                            let* forwarded =
                              Result.map_error Error.to_string
                                (encode_json Ev.Forwarded_room_key_content.jsont
                                   {
                                     room_id =
                                       Olm.Megolm.Inbound.room_id session;
                                     sender_key =
                                       Curve25519.Public.to_base64
                                         (Olm.Megolm.Inbound.sender_key session);
                                     session_id =
                                       Olm.Megolm.Inbound.session_id session;
                                     session_key =
                                       Olm.Megolm.Inbound
                                       .export_at_first_known_index session;
                                     sender_claimed_ed25519_key =
                                       (match
                                          Olm.Megolm.Inbound
                                          .sender_claimed_ed25519_key session
                                        with
                                       | Some k -> Ed25519.Public.to_base64 k
                                       | None -> "");
                                     forwarding_curve25519_key_chain =
                                       forwarding_chain_of t (room, sid);
                                   })
                            in
                            olm_encrypt_for t ~session:olm_session
                              ~recipient:t.our_user d
                              ~event_type:"m.forwarded_room_key"
                              ~content:forwarded
                          in
                          match built with
                          | Error msg -> refuse msg
                          | Ok content ->
                              Some
                                (To_device
                                   {
                                     event_type = "m.room.encrypted";
                                     txn_id = Random.txn_id t.random;
                                     messages = add_message [] d content;
                                   }))))))

type withheld_wire = {
  wh_algorithm : string;
  wh_sender_key : string;
  wh_from_device : Did.t option;
  wh_value : withheld;
}

let withheld_content_jsont : withheld_wire Jsont.t =
  Jsont.Object.(
    map (fun algorithm sender_key room_id session_id code reason from_device ->
        if not (String.equal algorithm megolm_algorithm) then
          Jsont.Error.msg Jsont.Meta.none
            (Printf.sprintf "unsupported withheld-key algorithm %S" algorithm)
        else
          match
            ( Curve25519.Public.of_base64 sender_key,
              Rid.of_string room_id,
              Sid.of_string session_id )
          with
          | Ok _, Ok room_id, Ok session_id ->
              {
                wh_algorithm = algorithm;
                wh_sender_key = sender_key;
                wh_from_device =
                  Option.bind from_device (fun value ->
                      Result.to_option (Did.of_string value));
                wh_value =
                  {
                    room_id;
                    session_id;
                    code;
                    reason;
                    sender_key = Some sender_key;
                    from_device =
                      Option.bind from_device (fun value ->
                          Result.to_option (Did.of_string value));
                    sender_user = None;
                  };
              }
          | Error (`Msg m), _, _ | _, Error (`Msg m), _ | _, _, Error (`Msg m)
            ->
              Jsont.Error.msg Jsont.Meta.none m)
    |> mem "algorithm" Matrix_proto.Json.Codec.string ~enc:(fun w ->
        w.wh_algorithm)
    |> mem "sender_key" Matrix_proto.Json.Codec.string ~enc:(fun w ->
        w.wh_sender_key)
    |> mem "room_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : withheld_wire) -> Rid.to_string w.wh_value.room_id)
    |> mem "session_id" Matrix_proto.Json.Codec.string ~enc:(fun w ->
        Sid.to_string w.wh_value.session_id)
    |> mem "code" Matrix_proto.Json.Codec.string ~enc:(fun w -> w.wh_value.code)
    |> opt_mem "reason" Matrix_proto.Json.Codec.string ~enc:(fun w ->
        w.wh_value.reason)
    |> opt_mem "from_device" Matrix_proto.Json.Codec.string ~enc:(fun w ->
        Option.map Did.to_string w.wh_from_device)
    |> finish)

(* Serves both paths: an event recovered from an Olm envelope, and one that
   arrived in the clear, which passes [sender_key] and [sender_ed25519] as
   [None] because nothing authenticated it. *)
let handle_inner t ~sender ~sender_key ~sender_ed25519 ~sender_device_keys
    ~event_type ~content ~requests ~new_sessions =
  let ignored reason = Ignored { event_type; reason } in
  let bad reason = Undecryptable { sender; reason } in
  let olm_error e = bad (Format.asprintf "%a" Olm.pp_error e) in
  match event_type with
  | "m.room_key" -> (
      match
        (Jsont.Json.decode Ev.Room_key_content.jsont content, sender_key)
      with
      | Error msg, _ -> bad msg
      | Ok _, None -> bad "an m.room_key that did not arrive over Olm"
      | Ok rk, Some sender_key -> (
          match
            Olm.Megolm.Inbound.from_room_key ?signing_key:sender_ed25519
              ~sender_key ~room_id:rk.room_id ~session_id:rk.session_id
              ~session_key:rk.session_key ()
          with
          | Error e -> olm_error e
          | Ok session ->
              if
                store_inbound t ~sender
                  ?sender_ed25519:
                    (Option.map Ed25519.Public.to_base64 sender_ed25519)
                  ~shared_history:rk.shared_history session
              then new_sessions := (rk.room_id, rk.session_id) :: !new_sessions;
              Room_key
                {
                  room_id = rk.room_id;
                  session_id = rk.session_id;
                  sender;
                  sender_key;
                }))
  | "m.forwarded_room_key" -> (
      match Jsont.Json.decode Ev.Forwarded_room_key_content.jsont content with
      | Error msg -> bad msg
      | Ok f -> (
          let claimed_ed25519 =
            Result.to_option
              (Ed25519.Public.of_base64 f.sender_claimed_ed25519_key)
          in
          match
            canonical_forwarding_chain f.forwarding_curve25519_key_chain
          with
          | Error message -> bad message
          | Ok forwarding_chain -> (
              match Curve25519.Public.of_base64 f.sender_key with
              | Error (`Msg m) -> bad m
              | Ok forwarded_sender_key -> (
                  match
                    Olm.Megolm.Inbound.of_exported_session_key ?claimed_ed25519
                      ~sender_key:forwarded_sender_key ~room_id:f.room_id
                      ~session_key:f.session_key ()
                  with
                  | Error e -> olm_error e
                  | Ok session ->
                      if
                        not
                          (Sid.equal f.session_id
                             (Olm.Megolm.Inbound.session_id session))
                      then bad "forwarded room key session ID mismatch"
                      else begin
                        if
                          store_inbound t ~forwarding_chain ~sender
                            ?sender_ed25519:
                              (Option.map Ed25519.Public.to_base64
                                 claimed_ed25519)
                            session
                        then
                          new_sessions :=
                            (f.room_id, f.session_id) :: !new_sessions;
                        Forwarded_room_key
                          {
                            room_id = f.room_id;
                            session_id = f.session_id;
                            sender;
                            sender_key =
                              Curve25519.Public.to_base64 forwarded_sender_key;
                            forwarding_chain;
                          }
                      end))))
  | "m.room_key_bundle" | "io.element.msc4268.room_key_bundle" -> (
      match (sender_key, sender_ed25519) with
      | None, _ -> bad "an m.room_key_bundle that did not arrive over Olm"
      | Some _, None ->
          bad "an m.room_key_bundle had no authenticated Ed25519 key"
      | Some sender_key, Some sender_ed25519 -> (
          match Jsont.Json.decode room_key_bundle_content_jsont content with
          | Error msg -> bad msg
          | Ok bundle -> (
              match
                ( sender_device_keys,
                  Encrypted_attachment.Metadata.of_event_file bundle.file,
                  Media.Mxc.of_string bundle.file.url )
              with
              | None, _, _ ->
                  bad "m.room_key_bundle is missing sender_device_keys"
              | _, Error error, _ ->
                  bad (Format.asprintf "%a" Encrypted_attachment.pp_error error)
              | _, _, Error (`Msg m) -> bad m
              | Some _, Ok _, Ok _ ->
                  let received =
                    {
                      room_id = bundle.room_id;
                      sender;
                      sender_key;
                      sender_ed25519 = Ed25519.Public.to_base64 sender_ed25519;
                      file = bundle.file;
                    }
                  in
                  t.received_key_bundles <-
                    received
                    :: List.filter
                         (fun (old : received_key_bundle) ->
                           not
                             (Rid.equal old.room_id received.room_id
                             && Uid.equal old.sender received.sender))
                         t.received_key_bundles;
                  Room_key_bundle received)))
  | "m.room_key_request" -> (
      match Jsont.Json.decode room_key_request_content_jsont content with
      | Error msg -> Ignored { event_type; reason = msg }
      | Ok req ->
          let answered =
            if req.kr_action <> "request" then None
            else gossip_reply t ~sender ~req
          in
          (match answered with
          | Some r -> requests := r :: !requests
          | None -> ());
          let body =
            Option.value req.kr_body
              ~default:
                {
                  rq_algorithm = megolm_algorithm;
                  rq_room_id = "";
                  rq_session_id = "";
                  rq_sender_key = None;
                }
          in
          Room_key_request
            {
              sender;
              requesting_device_id = req.kr_requesting_device_id;
              request_id = req.kr_request_id;
              room_id = body.rq_room_id;
              session_id = body.rq_session_id;
              action = req.kr_action;
              answered = answered <> None;
            })
  | "m.room_key.withheld" -> (
      match Jsont.Json.decode withheld_content_jsont content with
      | Error msg -> Ignored { event_type; reason = msg }
      | Ok { wh_value = w; _ } ->
          let already_held =
            has_inbound_session t w.room_id ~session_id:w.session_id
          in
          if not already_held then
            t.withheld <-
              { w with sender_user = Some sender }
              :: List.filter
                   (fun (x : withheld) ->
                     not
                       (Rid.equal x.room_id w.room_id
                       && Sid.equal x.session_id w.session_id))
                   t.withheld;
          Ignored
            {
              event_type;
              reason =
                (if already_held then "ignored: room key is already held"
                 else "recorded room-key withholding");
            })
  | "m.secret.request" ->
      handle_secret_request t ~sender ~sender_key content;
      Secret_request { sender; content }
  | "m.secret.send" -> (
      match sender_key with
      | None -> bad "an m.secret.send that did not arrive over Olm"
      | Some _ ->
          handle_secret_send t ~sender ~sender_key content;
          Secret_send { sender; content })
  | "m.dummy" -> ignored "m.dummy carries no payload"
  | et when String.starts_with ~prefix:"m.key.verification." et ->
      Verification
        {
          event_type = et;
          sender;
          sender_device =
            Option.map
              (fun (d : device) -> d.device_id)
              (Option.bind sender_key (find_device_by_curve25519 t));
          content;
        }
  | _ -> ignored "unhandled to-device event type"

let handle_olm t ~now ~sender ~content ~requests ~new_sessions =
  let bad reason = Undecryptable { sender; reason } in
  match Jsont.Json.decode Ev.Encrypted.Olm.jsont content with
  | Error msg -> bad msg
  | Ok fields -> (
      let ours = Curve25519.Public.to_base64 (our_curve25519 t) in
      match
        ( Ev.Olm_ciphertext.find fields.ciphertext ~recipient_key:ours,
          Curve25519.Public.of_base64 fields.sender_key )
      with
      | None, _ ->
          Ignored
            {
              event_type = "m.room.encrypted";
              reason = "the message is addressed to another device";
            }
      | _, Error (`Msg m) -> bad m
      | Some entry, Ok their_identity_key -> (
          let message : Olm.Session.message =
            { message_type = entry.message_type; ciphertext = entry.body }
          in
          match
            Olm.Machine.decrypt_to_device ~random:t.random t.machine
              ~their_identity_key message
          with
          | Error e ->
              mark_wedged t ~now ~sender ~identity_key:their_identity_key;
              bad (Format.asprintf "%a" Olm.pp_error e)
          | Ok plaintext -> (
              match
                Jsont_bytesrw.decode_string Ev.Olm_plaintext.jsont plaintext
              with
              | Error msg -> bad msg
              | Ok p -> (
                  (* Without these an attacker can replay somebody else's
                     message at us, or claim another user's Curve25519 key.

                     @see <https://spec.matrix.org/v1.11/client-server-api/#molmv1curve25519-aes-sha2> *)
                  let claimed_ed25519 =
                    Result.to_option (Ed25519.Public.of_base64 p.sender_ed25519)
                  in
                  if not (Uid.equal p.sender sender) then
                    bad "the decrypted sender does not match the event"
                  else if not (Uid.equal p.recipient t.our_user) then
                    bad "the message is addressed to another user"
                  else if
                    not
                      (String.equal p.recipient_ed25519
                         (Ed25519.Public.to_base64 (our_ed25519 t)))
                  then bad "the message is addressed to another device"
                  else if
                    match find_device_by_curve25519 t their_identity_key with
                    | None -> false
                    | Some d -> (
                        match (device_ed25519 d, claimed_ed25519) with
                        | None, None -> false
                        | Some published, Some claimed ->
                            not (Ed25519.Public.equal published claimed)
                        | _ -> true)
                  then bad "the claimed Ed25519 key is not the sender's"
                  else
                    let validated_device_keys =
                      match p.sender_device_keys with
                      | None -> Ok None
                      | Some device_keys -> (
                          match claimed_ed25519 with
                          | None ->
                              Error
                                "sender_device_keys has no valid Ed25519 claim"
                          | Some sender_ed25519 ->
                              Result.map
                                (fun _ -> Some device_keys)
                                (validate_sender_device_keys ~sender
                                   ~sender_key:their_identity_key
                                   ~sender_ed25519 device_keys))
                    in
                    match validated_device_keys with
                    | Error reason -> bad reason
                    | Ok sender_device_keys ->
                        handle_inner t ~sender
                          ~sender_key:(Some their_identity_key)
                          ~sender_ed25519:claimed_ed25519 ~sender_device_keys
                          ~event_type:p.event_type ~content:p.content ~requests
                          ~new_sessions))))

let handle_to_device t (raw : to_device_raw) ~now ~requests ~new_sessions =
  match Uid.of_string raw.td_sender with
  | Error _ -> Ignored { event_type = raw.td_type; reason = "invalid sender" }
  | Ok sender -> (
      match raw.td_type with
      | "m.room.encrypted" ->
          handle_olm t ~now ~sender ~content:raw.td_content ~requests
            ~new_sessions
      | et ->
          (* [m.room_key_request] and the verification events are sent in the
             clear; everything else that matters arrives over Olm. *)
          handle_inner t ~sender ~sender_key:None ~sender_ed25519:None
            ~sender_device_keys:None ~event_type:et ~content:raw.td_content
            ~requests ~new_sessions)

let scan_room_encryption t room event_type content =
  if Ev.Event_type.to_string event_type = "m.room.encryption" then
    match set_room_encryption_settings t room content with
    | Ok () -> ()
    | Error err ->
        Log.warn (fun m ->
            m "Ignoring m.room.encryption in %a: %s" Rid.pp room
              (Error.to_string err))

let scan_room_events t ?room_id (events : Ev.Raw_event.t list) =
  let handle (e : Ev.Raw_event.t) =
    match e.room_id with
    | Some room -> scan_room_encryption t room e.type_ e.content
    | None ->
        Option.iter
          (fun room -> scan_room_encryption t room e.type_ e.content)
          room_id
  in
  List.iter handle events

let scan_stripped_room_events t room (events : Ev.Stripped_event.t list) =
  List.iter
    (fun (event : Ev.Stripped_event.t) ->
      scan_room_encryption t room event.type_ event.content)
    events

let scan_room_state t (r : Matrix_proto.Sync.Response.t) =
  match r.rooms with
  | None -> ()
  | Some rooms ->
      let scan_full room_str state timeline =
        let room_id = Result.to_option (Rid.of_string room_str) in
        let with_room (e : Ev.Raw_event.t) =
          scan_room_events t ?room_id [ e ]
        in
        Option.iter
          (fun (s : Matrix_proto.Sync.Room_state.t) ->
            List.iter with_room s.events)
          state;
        Option.iter
          (fun (tl : Matrix_proto.Sync.Timeline.t) ->
            List.iter with_room tl.events)
          timeline
      in
      List.iter
        (fun (room_str, (jr : Matrix_proto.Sync.Joined_room.t)) ->
          scan_full room_str jr.state jr.timeline)
        rooms.join;
      List.iter
        (fun (room_str, (lr : Matrix_proto.Sync.Left_room.t)) ->
          scan_full room_str lr.state lr.timeline)
        rooms.leave;
      let scan_stripped room_str events =
        match Rid.of_string room_str with
        | Error _ -> ()
        | Ok room -> scan_stripped_room_events t room events
      in
      List.iter
        (fun (room_str, (ir : Matrix_proto.Sync.Invited_room.t)) ->
          Option.iter
            (fun (state : Matrix_proto.Sync.Stripped_events.t) ->
              scan_stripped room_str state.events)
            ir.invite_state)
        rooms.invite;
      List.iter
        (fun (room_str, (kr : Matrix_proto.Sync.Knocked_room.t)) ->
          Option.iter
            (fun (state : Matrix_proto.Sync.Stripped_events.t) ->
              scan_stripped room_str state.events)
            kr.knock_state)
        rooms.knock

let scan_sliding_room_state t (r : Matrix_proto.Sliding_sync.Response.t) =
  List.iter
    (fun (room_id, (room : Matrix_proto.Sliding_sync.Response.room)) ->
      scan_room_events t ~room_id (room.timeline @ room.required_state))
    r.rooms

let process_response ?now t ~scan
    ~(device_lists : Matrix_proto.Sync.Device_lists.t) ~otk_counts
    ~fallback_types ~to_device ~classic_sync =
  scan ();
  let changed, left = (device_lists.changed, device_lists.left) in
  List.iter
    (fun u ->
      if mem_uid u t.tracked && not (mem_uid u t.outdated) then
        t.outdated <- u :: t.outdated)
    changed;
  untrack_users t left;
  (match List.assoc_opt otk_algorithm otk_counts with
  | Some n -> t.server_otk_count <- n
  | None when classic_sync -> t.server_otk_count <- 0
  | None -> ());
  let at = Option.value now ~default:(wall_clock ()) in
  maybe_generate_fallback_key t ~at ~fallback_types;
  let requests = ref [] in
  let new_sessions = ref [] in
  let events =
    List.map
      (fun json ->
        match
          Jsont_bytesrw.decode_string to_device_raw_jsont
            (Result.value ~default:"null"
               (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json json))
        with
        | Error msg -> Ignored { event_type = "?"; reason = msg }
        | Ok raw -> handle_to_device t raw ~now:at ~requests ~new_sessions)
      to_device
  in
  {
    requests = List.rev !requests @ outgoing_requests t;
    events;
    changed_users = List.filter (fun u -> mem_uid u t.tracked) changed;
    left_users = left;
    new_sessions = List.rev !new_sessions;
  }

let process_sync ?now t (r : Matrix_proto.Sync.Response.t) =
  let device_lists =
    Option.value r.device_lists ~default:{ changed = []; left = [] }
  in
  let to_device = match r.to_device with None -> [] | Some td -> td.events in
  process_response ?now t
    ~scan:(fun () -> scan_room_state t r)
    ~device_lists ~otk_counts:r.device_one_time_keys_count
    ~fallback_types:r.device_unused_fallback_key_types ~to_device
    ~classic_sync:true

let process_sliding_sync ?now t (r : Matrix_proto.Sliding_sync.Response.t) =
  let e2ee = r.extensions.e2ee in
  let to_device =
    match r.extensions.to_device with None -> [] | Some td -> td.events
  in
  process_response ?now t
    ~scan:(fun () -> scan_sliding_room_state t r)
    ~device_lists:e2ee.device_lists ~otk_counts:e2ee.device_one_time_keys_count
    ~fallback_types:e2ee.device_unused_fallback_key_types ~to_device
    ~classic_sync:false
