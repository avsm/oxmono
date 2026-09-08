module User_id = Matrix_proto.Id.User_id
module Device_id = Matrix_proto.Id.Device_id
module Server_name = Matrix_proto.Id.Server_name
module Key_id = Crypto_key.Key_id
module Signature = Crypto_key.Signature

let keyed_map = Json_codec.keyed_map

let user_map v =
  keyed_map ~what:"user id" ~of_string:User_id.of_string
    ~to_string:User_id.to_string v

let device_map v =
  keyed_map ~what:"device id" ~of_string:Device_id.of_string
    ~to_string:Device_id.to_string v

let key_id_map v =
  keyed_map ~what:"key id" ~of_string:Key_id.of_string
    ~to_string:Key_id.to_string v

let string_map = Json_codec.string_map
let js_safe_uint_jsont = Matrix_proto.Json.Codec.uint

type signatures = (User_id.t * (Key_id.t * Signature.t) list) list

let signatures_jsont : signatures Jsont.t =
  user_map (key_id_map Signature.jsont)

let failures_jsont =
  keyed_map ~what:"server name" ~of_string:Server_name.of_string
    ~to_string:Server_name.to_string Matrix_proto.Json.Codec.json

type device_keys = {
  user_id : User_id.t;
  device_id : Device_id.t;
  algorithms : string list;
  keys : (Key_id.t * string) list;
  signatures : signatures;
  dehydrated : bool option;
  unsigned : Jsont.json option;
}

let device_keys_jsont =
  Jsont.Object.(
    map (fun user_id device_id algorithms keys signatures dehydrated unsigned ->
        {
          user_id;
          device_id;
          algorithms;
          keys;
          signatures;
          dehydrated;
          unsigned;
        })
    |> mem "user_id" User_id.jsont ~enc:(fun t -> t.user_id)
    |> mem "device_id" Device_id.jsont ~enc:(fun t -> t.device_id)
    |> mem "algorithms"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.algorithms)
    |> mem "keys"
         (key_id_map Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.keys)
    |> mem "signatures" signatures_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.signatures)
    |> opt_mem "dehydrated" Jsont.bool ~enc:(fun t -> t.dehydrated)
    |> opt_mem "unsigned" Matrix_proto.Json.Codec.json ~enc:(fun t ->
        t.unsigned)
    |> finish)

type one_time_key = {
  key : string;
  fallback : bool option;
  signatures : signatures option;
}

let one_time_key_jsont =
  Jsont.Object.(
    map (fun key fallback signatures -> { key; fallback; signatures })
    |> mem "key" Matrix_proto.Json.Codec.string ~enc:(fun (t : one_time_key) ->
        t.key)
    |> opt_mem "fallback" Jsont.bool ~enc:(fun (t : one_time_key) -> t.fallback)
    |> opt_mem "signatures" signatures_jsont ~enc:(fun (t : one_time_key) ->
        t.signatures)
    |> finish)

(* Build a JSON value rather than interpolating so arbitrary input is escaped,
   then canonicalize because the fallback marker sorts before [key]. *)
let one_time_key_signing_json ?fallback key =
  let fallback =
    match fallback with
    | None -> []
    | Some value ->
        [ Jsont.Json.mem (Jsont.Json.name "fallback") (Jsont.Json.bool value) ]
  in
  let json =
    Jsont.Json.object'
      (Jsont.Json.mem (Jsont.Json.name "key") (Jsont.Json.string key)
      :: fallback)
  in
  Matrix_proto.Signed_json.canonical_json json

type key_usage = Master | Self_signing | User_signing | Other of string

let key_usage_to_string = function
  | Master -> "master"
  | Self_signing -> "self_signing"
  | User_signing -> "user_signing"
  | Other s -> s

let key_usage_of_string = function
  | "master" -> Master
  | "self_signing" -> Self_signing
  | "user_signing" -> User_signing
  | s -> Other s

let key_usage_jsont =
  Jsont.map ~dec:key_usage_of_string ~enc:key_usage_to_string
    Matrix_proto.Json.Codec.string

type cross_signing_key = {
  user_id : User_id.t;
  usage : key_usage list;
  keys : (Key_id.t * string) list;
  signatures : signatures;
}

let cross_signing_key_jsont =
  Jsont.Object.(
    map (fun user_id usage keys signatures ->
        { user_id; usage; keys; signatures })
    |> mem "user_id" User_id.jsont ~enc:(fun t -> t.user_id)
    |> mem "usage"
         (Jsont.list key_usage_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.usage)
    |> mem "keys"
         (key_id_map Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.keys)
    |> mem "signatures" signatures_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.signatures)
    |> finish)

let cross_signing_key_map_jsont = user_map cross_signing_key_jsont

type failures = (Server_name.t * Jsont.json) list

type upload_keys_request = {
  device_keys : device_keys option;
  one_time_keys : (Key_id.t * one_time_key) list;
  fallback_keys : (Key_id.t * one_time_key) list;
}

let upload_keys_request_jsont =
  Jsont.Object.(
    map (fun device_keys one_time_keys fallback_keys ->
        { device_keys; one_time_keys; fallback_keys })
    |> opt_mem "device_keys" device_keys_jsont ~enc:(fun t -> t.device_keys)
    |> mem "one_time_keys"
         (key_id_map one_time_key_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.one_time_keys)
    |> mem "fallback_keys"
         (key_id_map one_time_key_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.fallback_keys)
    |> finish)

type upload_keys_response = { one_time_key_counts : (string * int) list }

let upload_keys_response_jsont =
  Jsont.Object.(
    map (fun one_time_key_counts -> { one_time_key_counts })
    |> mem "one_time_key_counts"
         (string_map js_safe_uint_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.one_time_key_counts)
    |> finish)

let upload_keys client ?device_keys ?(one_time_keys = []) ?(fallback_keys = [])
    () =
  let request = { device_keys; one_time_keys; fallback_keys } in
  match Client.Http.encode_body upload_keys_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.post client ~path:"/keys/upload" ~body () with
      | Error e -> Error e
      | Ok body -> Client.Http.decode_response upload_keys_response_jsont body)

type query_keys_request = {
  timeout : int option;
  device_keys : (User_id.t * Device_id.t list) list;
}

let query_keys_request_jsont =
  Jsont.Object.(
    map (fun timeout device_keys -> { timeout; device_keys })
    |> opt_mem "timeout" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.timeout)
    |> mem "device_keys"
         (user_map (Jsont.list Device_id.jsont))
         ~enc:(fun t -> t.device_keys)
    |> finish)

type query_keys_response = {
  failures : failures;
  device_keys : (User_id.t * (Device_id.t * device_keys) list) list;
  master_keys : (User_id.t * cross_signing_key) list;
  self_signing_keys : (User_id.t * cross_signing_key) list;
  user_signing_keys : (User_id.t * cross_signing_key) list;
}

let query_keys_response_jsont =
  Jsont.Object.(
    map
      (fun
        failures device_keys master_keys self_signing_keys user_signing_keys ->
        {
          failures;
          device_keys;
          master_keys;
          self_signing_keys;
          user_signing_keys;
        })
    |> mem "failures" failures_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.failures)
    |> mem "device_keys"
         (user_map (device_map device_keys_jsont))
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.device_keys)
    |> mem "master_keys" cross_signing_key_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.master_keys)
    |> mem "self_signing_keys" cross_signing_key_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.self_signing_keys)
    |> mem "user_signing_keys" cross_signing_key_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.user_signing_keys)
    |> finish)

let query_keys client ?timeout ~users () =
  let request = { timeout; device_keys = users } in
  match Client.Http.encode_body query_keys_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.post client ~path:"/keys/query" ~body () with
      | Error e -> Error e
      | Ok body -> Client.Http.decode_response query_keys_response_jsont body)

type claim_keys_request = {
  timeout : int option;
  one_time_keys : (User_id.t * (Device_id.t * string) list) list;
}

let claim_keys_request_jsont =
  Jsont.Object.(
    map (fun timeout one_time_keys -> { timeout; one_time_keys })
    |> opt_mem "timeout" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.timeout)
    |> mem "one_time_keys"
         (user_map (device_map Matrix_proto.Json.Codec.string))
         ~enc:(fun t -> t.one_time_keys)
    |> finish)

type claim_keys_response = {
  failures : failures;
  one_time_keys :
    (User_id.t * (Device_id.t * (Key_id.t * one_time_key) list) list) list;
}

let claim_keys_response_jsont =
  Jsont.Object.(
    map (fun failures one_time_keys -> { failures; one_time_keys })
    |> mem "failures" failures_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.failures)
    |> mem "one_time_keys"
         (user_map (device_map (key_id_map one_time_key_jsont)))
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.one_time_keys)
    |> finish)

let claim_keys client ?timeout ~keys () =
  let request = { timeout; one_time_keys = keys } in
  match Client.Http.encode_body claim_keys_request_jsont request with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.post client ~path:"/keys/claim" ~body () with
      | Error e -> Error e
      | Ok body -> Client.Http.decode_response claim_keys_response_jsont body)

type key_changes_response = { changed : User_id.t list; left : User_id.t list }

let key_changes_response_jsont =
  Jsont.Object.(
    map (fun changed left -> { changed; left })
    |> mem "changed" (Jsont.list User_id.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.changed)
    |> mem "left" (Jsont.list User_id.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.left)
    |> finish)

let get_key_changes client ~from ~until =
  let query = [ ("from", from); ("to", until) ] in
  match Client.Http.get client ~path:"/keys/changes" ~query () with
  | Error e -> Error e
  | Ok body -> Client.Http.decode_response key_changes_response_jsont body

type signing_keys_request = {
  master_key : cross_signing_key option;
  self_signing_key : cross_signing_key option;
  user_signing_key : cross_signing_key option;
}

let signing_keys_request_jsont =
  Jsont.Object.(
    map (fun master_key self_signing_key user_signing_key ->
        { master_key; self_signing_key; user_signing_key })
    |> opt_mem "master_key" cross_signing_key_jsont ~enc:(fun t -> t.master_key)
    |> opt_mem "self_signing_key" cross_signing_key_jsont ~enc:(fun t ->
        t.self_signing_key)
    |> opt_mem "user_signing_key" cross_signing_key_jsont ~enc:(fun t ->
        t.user_signing_key)
    |> finish)

let signing_keys_body ?master_key ?self_signing_key ?user_signing_key () =
  Client.Http.encode_body signing_keys_request_jsont
    { master_key; self_signing_key; user_signing_key }

let post_signing_keys client ~body ~auth_json =
  let body =
    match auth_json with
    | None -> Ok body
    | Some auth -> Uiaa.add_auth_to_body ~body ~auth
  in
  match
    Result.bind body (fun body ->
        Client.Http.post client ~path:"/keys/device_signing/upload" ~body ())
  with
  | Error e -> Error e
  | Ok _ -> Ok ()

let upload_signing_keys client ?master_key ?self_signing_key ?user_signing_key
    ?auth () =
  match
    signing_keys_body ?master_key ?self_signing_key ?user_signing_key ()
  with
  | Error e -> Error e
  | Ok body ->
      let auth_json = Option.map Uiaa.auth_data_to_json auth in
      post_signing_keys client ~body ~auth_json

let upload_signing_keys_uiaa client ?master_key ?self_signing_key
    ?user_signing_key ~auth_callback () =
  match
    signing_keys_body ?master_key ?self_signing_key ?user_signing_key ()
  with
  | Error e -> Uiaa.Uiaa_error e
  | Ok body ->
      Uiaa.with_uiaa ~auth_callback ~make_request:(fun auth_json ->
          post_signing_keys client ~body ~auth_json)

type upload_signatures_response = {
  failures : (User_id.t * (string * Jsont.json) list) list;
}

let signatures_upload_jsont = user_map (string_map Matrix_proto.Json.Codec.json)

let upload_signatures_response_jsont =
  Jsont.Object.(
    map (fun failures -> { failures })
    |> mem "failures" signatures_upload_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.failures)
    |> finish)

let upload_signatures client signatures =
  let signatures = List.filter (fun (_, keys) -> keys <> []) signatures in
  match Client.Http.encode_body signatures_upload_jsont signatures with
  | Error e -> Error e
  | Ok body -> (
      match
        Client.Http.post client ~path:"/keys/signatures/upload" ~body ()
      with
      | Error e -> Error e
      | Ok body ->
          Client.Http.decode_response upload_signatures_response_jsont body)
