module B = Matrix_client.Backup

let of_result = function
  | Ok v -> v
  | Error (`Msg msg) -> raise (Error.err (Error.Json msg))

let backup_algorithm = B.backup_algorithm

type encryption_key = B.encryption_key

type signature_state = B.signature_state =
  | Missing
  | Invalid
  | Valid_but_not_trusted
  | Valid_and_trusted

type encrypted_session_data = B.encrypted_session_data = {
  ephemeral : string;
  ciphertext : string;
  mac : string;
}

type key_backup_data = B.key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : encrypted_session_data;
}

type sessions = B.sessions
type rooms = B.rooms

type backed_up_session_data = B.backed_up_session_data = {
  algorithm : string;
  forwarding_curve25519_key_chain : string list;
  sender_key : string;
  sender_claimed_keys : (string * string) list;
  session_key : string;
  shared_history : bool;
}

type megolm_v1_auth_data = B.megolm_v1_auth_data = {
  public_key : encryption_key;
  signatures : Matrix_client.Keys.signatures;
}

type recovered_room_key = B.recovered_room_key = {
  room_id : Matrix_proto.Id.Room_id.t;
  session_id : Matrix_proto.Id.Session_id.t;
  session_key : string;
  sender_key : string;
  algorithm : string;
  forwarded : bool;
  sender_claimed_keys : (string * string) list;
  forwarding_curve25519_key_chain : string list;
  shared_history : bool;
}

module Decryption_key = struct
  type t = B.Decryption_key.t

  let generate = B.Decryption_key.generate
  let of_bytes b = of_result (B.Decryption_key.of_bytes b)
  let of_base64 s = of_result (B.Decryption_key.of_base64 s)
  let to_base64 = B.Decryption_key.to_base64
  let public = B.Decryption_key.public
end

module Recovery_key = struct
  let encode = B.Recovery_key.encode
  let decode s = of_result (B.Recovery_key.decode s)
end

let encrypt_session_data ~random encryption_key plaintext =
  of_result (B.encrypt_session_data ~random encryption_key plaintext)

let encrypt_room_key ~random encryption_key ~session_key ~sender_key =
  of_result (B.encrypt_room_key ~random encryption_key ~session_key ~sender_key)

let decrypt_room_key decryption_key session_data =
  of_result (B.decrypt_room_key decryption_key session_data)

let parse_recovered_key ~room_id ~session_id plaintext =
  of_result (B.parse_recovered_key ~room_id ~session_id plaintext)

let encrypted_session_data_jsont = B.encrypted_session_data_jsont
let key_backup_data_jsont = B.key_backup_data_jsont
let backed_up_session_data_jsont = B.backed_up_session_data_jsont
let megolm_v1_auth_data_jsont = B.megolm_v1_auth_data_jsont
let auth_data_to_json = B.auth_data_to_json
let sign_auth_data = B.sign_auth_data
let verify_auth_data_signature = B.verify_auth_data_signature
