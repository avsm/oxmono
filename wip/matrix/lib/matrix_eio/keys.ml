module K = Matrix_client.Keys

type signatures = K.signatures
type device_keys = K.device_keys
type one_time_key = K.one_time_key

let one_time_key_signing_json = K.one_time_key_signing_json

type key_usage = K.key_usage =
  | Master
  | Self_signing
  | User_signing
  | Other of string

type cross_signing_key = K.cross_signing_key = {
  user_id : Matrix_proto.Id.User_id.t;
  usage : key_usage list;
  keys : (Matrix_client.Crypto_key.Key_id.t * string) list;
  signatures : signatures;
}

type upload_keys_response = K.upload_keys_response = {
  one_time_key_counts : (string * int) list;
}

let upload_keys client ?device_keys ?(one_time_keys = []) ?(fallback_keys = [])
    () =
  Error.unwrap ~context:"uploading keys"
    (K.upload_keys (Client.base client) ?device_keys ~one_time_keys
       ~fallback_keys ())

type query_keys_response = K.query_keys_response = {
  failures : K.failures;
  device_keys :
    (Matrix_proto.Id.User_id.t
    * (Matrix_proto.Id.Device_id.t * device_keys) list)
    list;
  master_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
  self_signing_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
  user_signing_keys : (Matrix_proto.Id.User_id.t * cross_signing_key) list;
}

let query_keys client ?timeout ~users () =
  Error.unwrap ~context:"querying keys"
    (K.query_keys (Client.base client) ?timeout ~users ())

type claim_keys_response = K.claim_keys_response = {
  failures : K.failures;
  one_time_keys :
    (Matrix_proto.Id.User_id.t
    * (Matrix_proto.Id.Device_id.t
      * (Matrix_client.Crypto_key.Key_id.t * one_time_key) list)
      list)
    list;
}

let claim_keys client ?timeout ~keys () =
  Error.unwrap ~context:"claiming keys"
    (K.claim_keys (Client.base client) ?timeout ~keys ())

type key_changes_response = K.key_changes_response = {
  changed : Matrix_proto.Id.User_id.t list;
  left : Matrix_proto.Id.User_id.t list;
}

let get_key_changes client ~from ~until =
  Error.unwrap ~context:"getting key changes"
    (K.get_key_changes (Client.base client) ~from ~until)

let upload_signing_keys client ?master_key ?self_signing_key ?user_signing_key
    ?auth () =
  Error.unwrap ~context:"uploading signing keys"
    (K.upload_signing_keys (Client.base client) ?master_key ?self_signing_key
       ?user_signing_key ?auth ())

let upload_signing_keys_uiaa client ?master_key ?self_signing_key
    ?user_signing_key ~auth_callback () =
  match
    K.upload_signing_keys_uiaa (Client.base client) ?master_key
      ?self_signing_key ?user_signing_key ~auth_callback ()
  with
  | Matrix_client.Uiaa.Uiaa_success () -> None
  | Matrix_client.Uiaa.Uiaa_auth_required challenge -> Some challenge
  | Matrix_client.Uiaa.Uiaa_error e ->
      Error.raise_client_error ~context:"uploading signing keys with UIAA" e

type upload_signatures_response = K.upload_signatures_response = {
  failures : (Matrix_proto.Id.User_id.t * (string * Jsont.json) list) list;
}

let upload_signatures client signatures =
  Error.unwrap ~context:"uploading key signatures"
    (K.upload_signatures (Client.base client) signatures)
