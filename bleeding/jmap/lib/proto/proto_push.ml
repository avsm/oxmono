(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The "@type" member of a push payload is a constant string the sender MUST
   emit and the receiver can therefore check (RFC 8620 Sections 7.1 and
   7.2.2).  It carries no information beyond that, so it is not a field of
   the decoded record. *)
let type_mem expected map =
  let jsont = Jsont.enum ~kind:expected [ (expected, ()) ] in
  Jsont.Object.mem "@type" jsont ~enc:(fun _ -> ()) map

module State_change = struct
  type type_state = { type_name : string; state : string }
  type t = { changed : (Proto_id.t * type_state list) list }

  let changed_jsont =
    let kind = "Changed" in
    let type_states_jsont = Proto_json_map.of_string Jsont.string in
    let decode_type_states pairs =
      List.map (fun (type_name, state) -> { type_name; state }) pairs
    in
    let encode_type_states states =
      List.map (fun ts -> (ts.type_name, ts.state)) states
    in
    Proto_json_map.of_id
      (Jsont.map ~kind ~dec:decode_type_states ~enc:encode_type_states
         type_states_jsont)

  let type_name = "StateChange"
  let v changed = { changed }

  let jsont =
    let kind = type_name in
    Jsont.Object.map ~kind (fun () changed -> { changed })
    |> type_mem type_name
    |> Jsont.Object.mem "changed" changed_jsont ~enc:(fun t -> t.changed)
    |> Jsont.Object.finish
end

module Push_verification = struct
  type t = { push_subscription_id : string; verification_code : string }

  let type_name = "PushVerification"

  let v ~push_subscription_id ~verification_code =
    { push_subscription_id; verification_code }

  let jsont =
    let kind = type_name in
    Jsont.Object.map ~kind (fun () push_subscription_id verification_code ->
        { push_subscription_id; verification_code })
    |> type_mem type_name
    |> Jsont.Object.mem "pushSubscriptionId" Jsont.string ~enc:(fun t ->
        t.push_subscription_id)
    |> Jsont.Object.mem "verificationCode" Jsont.string ~enc:(fun t ->
        t.verification_code)
    |> Jsont.Object.finish
end

type push_keys = { p256dh : string; auth : string }

let push_keys_make p256dh auth = { p256dh; auth }

(* RFC 7515 Section 2, which RFC 8620 Section 7.2 cites for these values,
   omits the base64url padding. *)
let validate_key ~name ~length value =
  if String.contains value '=' then
    Error (Printf.sprintf "%s is padded base64url" name)
  else
    match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet value with
    | Error (`Msg message) ->
        Error (Printf.sprintf "%s is not base64url: %s" name message)
    | Ok decoded when String.length decoded <> length ->
        Error
          (Printf.sprintf "%s decodes to %d octets instead of %d" name
             (String.length decoded) length)
    | Ok decoded -> Ok decoded

let validate_push_keys keys =
  match validate_key ~name:"p256dh" ~length:65 keys.p256dh with
  | Error _ as error -> error
  | Ok decoded when decoded.[0] <> '\x04' ->
      Error "p256dh is not an uncompressed P-256 public key"
  | Ok _ -> (
      match validate_key ~name:"auth" ~length:16 keys.auth with
      | Error _ as error -> error
      | Ok _ -> Ok keys)

let push_keys ~p256dh ~auth = validate_push_keys { p256dh; auth }

let check_push_keys meta keys =
  match validate_push_keys keys with
  | Ok _ -> ()
  | Error message -> Jsont.Error.msg meta message

let push_keys_jsont =
  let kind = "PushKeys" in
  Jsont.Object.map' ~kind (fun meta p256dh auth ->
      let keys = push_keys_make p256dh auth in
      check_push_keys meta keys;
      keys)
  |> Jsont.Object.mem "p256dh" Jsont.string ~enc:(fun k -> k.p256dh)
  |> Jsont.Object.mem "auth" Jsont.string ~enc:(fun k -> k.auth)
  |> Jsont.Object.finish
  |> Jsont.iter ~kind ~enc:(check_push_keys Jsont.Meta.none)

type t = {
  id : Proto_id.t;
  device_client_id : string option;
  url : string option;
  keys : push_keys option;
  verification_code : string option;
  expires : Ptime.t option;
  types : string list option;
}

let make id device_client_id url keys verification_code expires types =
  { id; device_client_id; url; keys; verification_code; expires; types }

let jsont =
  let kind = "PushSubscription" in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "id" Proto_id.jsont ~enc:(fun s -> s.id)
  |> Jsont.Object.opt_mem "deviceClientId" Jsont.string ~enc:(fun s ->
      s.device_client_id)
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun s -> s.url)
  |> Proto_json_map.nullable_mem "keys" push_keys_jsont ~enc:(fun s -> s.keys)
  |> Proto_json_map.nullable_mem "verificationCode" Jsont.string ~enc:(fun s ->
      s.verification_code)
  |> Proto_json_map.nullable_mem "expires" Proto_date.utc_jsont ~enc:(fun s ->
      s.expires)
  |> Proto_json_map.nullable_mem "types" (Jsont.list Jsont.string)
       ~enc:(fun s -> s.types)
  |> Jsont.Object.finish

type get_args = {
  ids : Proto_id.t list option;
  properties : string list option;
}

let get_args ?ids ?properties () = { ids; properties }
let get_args_make ids properties = { ids; properties }

let get_args_jsont =
  let kind = "PushSubscription/get args" in
  Jsont.Object.map ~kind get_args_make
  |> Proto_json_map.nullable_mem_null "ids" (Jsont.list Proto_id.jsont)
       ~enc:(fun a -> a.ids)
  |> Proto_json_map.nullable_mem "properties" (Jsont.list Jsont.string)
       ~enc:(fun a -> a.properties)
  |> Jsont.Object.finish

type get_response = { list : t list; not_found : Proto_id.t list }

let get_response_make list not_found = { list; not_found }

let get_response_jsont =
  let kind = "PushSubscription/get response" in
  Jsont.Object.map ~kind get_response_make
  |> Jsont.Object.mem "list" (Jsont.list jsont) ~enc:(fun r -> r.list)
  |> Jsont.Object.mem "notFound" (Jsont.list Proto_id.jsont) ~enc:(fun r ->
      r.not_found)
  |> Jsont.Object.finish

type create_args = {
  device_client_id : string;
  url : string;
  keys : push_keys option;
  verification_code : string option;
  expires : Ptime.t option;
  types : string list option;
}

let https_prefix = "https://"

let validate_create_url args =
  let url = args.url in
  if not (String.starts_with ~prefix:https_prefix url) then
    Error (Printf.sprintf "url must begin with %S: %S" https_prefix url)
  else
    match Httpz_uri.of_string url with
    | Null -> Error (Printf.sprintf "url is not a valid absolute URL: %S" url)
    | This uri -> (
        let host = (match Httpz_uri.decoded_host uri with This h -> h | Null -> "") in
        if String.equal host "" then
          Error (Printf.sprintf "url has no host: %S" url)
        else
          match Httpz_uri.port uri with
          | This port when port < 1 || port > 65535 ->
              Error (Printf.sprintf "url has an out-of-range port: %S" url)
          | Null when Httpz_uri.has_port uri ->
              Error (Printf.sprintf "url has an invalid port: %S" url)
          | This _ | Null -> Ok args)

let validate_create_args args =
  if String.equal args.device_client_id "" then
    Error "deviceClientId must not be empty"
  else if Option.is_some args.verification_code then
    Error "verificationCode must be absent when creating a subscription"
  else validate_create_url args

(* The keys given here have not been through [push_keys_jsont], which is what
   checks them when a create is decoded. *)
let create_args ~device_client_id ~url ?keys ?expires ?types () =
  let args =
    { device_client_id; url; keys; verification_code = None; expires; types }
  in
  match keys with
  | Some keys ->
      Result.bind (validate_push_keys keys) (fun _ -> validate_create_args args)
  | None -> validate_create_args args

let check_create_args meta args =
  match validate_create_args args with
  | Ok _ -> ()
  | Error message -> Jsont.Error.msg meta message

let create_args_jsont =
  let kind = "PushSubscription create" in
  Jsont.Object.map' ~kind
    (fun meta device_client_id url keys verification_code expires types ->
      let args =
        { device_client_id; url; keys; verification_code; expires; types }
      in
      check_create_args meta args;
      args)
  |> Jsont.Object.mem "deviceClientId" Jsont.string ~enc:(fun a ->
      a.device_client_id)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun a -> a.url)
  |> Proto_json_map.nullable_mem "keys" push_keys_jsont ~enc:(fun a -> a.keys)
  |> Proto_json_map.nullable_mem "verificationCode" Jsont.string ~enc:(fun a ->
      a.verification_code)
  |> Proto_json_map.nullable_mem "expires" Proto_date.utc_jsont ~enc:(fun a ->
      a.expires)
  |> Proto_json_map.nullable_mem "types" (Jsont.list Jsont.string)
       ~enc:(fun a -> a.types)
  |> Jsont.Object.finish
  |> Jsont.iter ~kind ~enc:(check_create_args Jsont.Meta.none)

type set_args = {
  create : (create_args Proto_id.creation * create_args) list option;
  update : (Proto_id.t * Proto_patch.t) list option;
  destroy : Proto_id.t list option;
}

let set_args ?create ?update ?destroy () = { create; update; destroy }
let set_args_make create update destroy = { create; update; destroy }

let set_args_jsont =
  let kind = "PushSubscription/set args" in
  Jsont.Object.map ~kind set_args_make
  |> Proto_json_map.nullable_mem "create"
       (Proto_json_map.of_creation create_args_jsont) ~enc:(fun a -> a.create)
  |> Proto_json_map.nullable_mem "update"
       (Proto_json_map.of_id_or_creation Proto_patch.jsont) ~enc:(fun a ->
         a.update)
  |> Proto_json_map.nullable_mem "destroy"
       (Jsont.list Proto_id.jsont_or_creation) ~enc:(fun a -> a.destroy)
  |> Jsont.Object.finish

type update_response = { expires : Ptime.t option }

type set_response = {
  created : (Proto_id.t * t) list option;
  updated : (Proto_id.t * update_response option) list option;
  destroyed : Proto_id.t list option;
  not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
  not_updated : (Proto_id.t * Proto_error.Set_error.t) list option;
  not_destroyed : (Proto_id.t * Proto_error.Set_error.t) list option;
}

let update_response_jsont =
  let kind = "PushSubscription updated properties" in
  Jsont.Object.map ~kind (fun expires -> { expires })
  |> Proto_json_map.nullable_mem "expires" Proto_date.utc_jsont ~enc:(fun r ->
      r.expires)
  |> Jsont.Object.finish

let set_response_make created updated destroyed not_created not_updated
    not_destroyed =
  { created; updated; destroyed; not_created; not_updated; not_destroyed }

let set_response_jsont =
  let kind = "PushSubscription/set response" in
  let errors = Proto_json_map.of_id Proto_error.Set_error.jsont in
  Jsont.Object.map ~kind set_response_make
  |> Proto_json_map.nullable_mem "created" (Proto_json_map.of_id jsont)
       ~enc:(fun r -> r.created)
  |> Proto_json_map.nullable_mem "updated"
       (Proto_json_map.of_id (Jsont.option update_response_jsont))
       ~enc:(fun r -> r.updated)
  |> Proto_json_map.nullable_mem "destroyed" (Jsont.list Proto_id.jsont)
       ~enc:(fun r -> r.destroyed)
  |> Proto_json_map.nullable_mem "notCreated" errors ~enc:(fun r ->
      r.not_created)
  |> Proto_json_map.nullable_mem "notUpdated" errors ~enc:(fun r ->
      r.not_updated)
  |> Proto_json_map.nullable_mem "notDestroyed" errors ~enc:(fun r ->
      r.not_destroyed)
  |> Jsont.Object.finish
