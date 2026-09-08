module Curve25519 = Crypto_key.Curve25519

type intent = Login | Reciprocate

type t = {
  intent : intent;
  public_key : Curve25519.Public.t;
  rendezvous_id : string;
  base_url : Uriz.t;
}

type codec_error =
  | Not_enough_data
  | Invalid_prefix
  | Invalid_type of int
  | Invalid_intent of int
  | Invalid_utf8 of string
  | Invalid_base_url of string
  | Field_too_long of string
  | Invalid_base64 of string

let pp_codec_error ppf = function
  | Not_enough_data -> Format.pp_print_string ppf "QR data is missing fields"
  | Invalid_prefix -> Format.pp_print_string ppf "unexpected QR data prefix"
  | Invalid_type got -> Format.fprintf ppf "unsupported QR data type 0x%02x" got
  | Invalid_intent got -> Format.fprintf ppf "invalid QR intent 0x%02x" got
  | Invalid_utf8 field -> Format.fprintf ppf "%s is not valid UTF-8" field
  | Invalid_base_url value ->
      Format.fprintf ppf "%S is not an HTTP(S) homeserver base URL" value
  | Field_too_long field ->
      Format.fprintf ppf "%s exceeds the QR format's 65535-byte limit" field
  | Invalid_base64 msg -> Format.fprintf ppf "invalid QR Base64: %s" msg

let prefix = "IO_ELEMENT_MSC4388"
let qr_type = 0x03
let valid_utf_8 = String.is_valid_utf_8

let validate ~rendezvous_id ~base_url =
  let base_url_string = Uriz.to_string base_url in
  if not (valid_utf_8 rendezvous_id) then Error (Invalid_utf8 "rendezvous ID")
  else if not (valid_utf_8 base_url_string) then
    Error (Invalid_utf8 "homeserver base URL")
  else if String.length rendezvous_id > 0xffff then
    Error (Field_too_long "rendezvous ID")
  else if String.length base_url_string > 0xffff then
    Error (Field_too_long "homeserver base URL")
  else
    match Client.Url.homeserver base_url with
    | Error _ -> Error (Invalid_base_url base_url_string)
    | Ok url -> Ok url

let validate_string ~rendezvous_id ~base_url =
  if not (valid_utf_8 rendezvous_id) then Error (Invalid_utf8 "rendezvous ID")
  else if not (valid_utf_8 base_url) then
    Error (Invalid_utf8 "homeserver base URL")
  else if String.length rendezvous_id > 0xffff then
    Error (Field_too_long "rendezvous ID")
  else if String.length base_url > 0xffff then
    Error (Field_too_long "homeserver base URL")
  else
    match Client.Url.homeserver_string base_url with
    | Error _ -> Error (Invalid_base_url base_url)
    | Ok url -> Ok url

let value ~intent ~public_key ~rendezvous_id url =
  {
    intent;
    public_key;
    rendezvous_id;
    base_url =
      (if Client.Url.path_segments url = [] then
         Uriz.of_string_exn (Client.Url.origin url)
       else Client.Url.to_uri url);
  }

let make ~intent ~public_key ~rendezvous_id ~base_url =
  match validate ~rendezvous_id ~base_url with
  | Error _ as e -> e
  | Ok url -> Ok (value ~intent ~public_key ~rendezvous_id url)

let intent_of_byte = function
  | 0x00 -> Ok Login
  | 0x01 -> Ok Reciprocate
  | byte -> Error (Invalid_intent byte)

let intent_byte = function Login -> 0x00 | Reciprocate -> 0x01

let of_bytes bytes =
  let length = String.length bytes in
  let offset = ref 0 in
  let read size =
    if size < 0 || !offset > length - size then Error Not_enough_data
    else
      let value = String.sub bytes !offset size in
      offset := !offset + size;
      Ok value
  in
  let read_byte () =
    match read 1 with Ok value -> Ok (Char.code value.[0]) | Error _ as e -> e
  in
  let read_u16 () =
    match read 2 with
    | Error _ as e -> e
    | Ok value -> Ok (String.get_uint16_be value 0)
  in
  let ( let* ) result f = Result.bind result f in
  let* got_prefix = read (String.length prefix) in
  if not (String.equal got_prefix prefix) then Error Invalid_prefix
  else
    let* got_type = read_byte () in
    if got_type <> qr_type then Error (Invalid_type got_type)
    else
      let* intent_byte = read_byte () in
      let* intent = intent_of_byte intent_byte in
      let* public_key_bytes = read 32 in
      let* public_key =
        match Curve25519.Public.of_bytes public_key_bytes with
        | Ok key -> Ok key
        | Error _ -> Error Not_enough_data
      in
      let* rendezvous_length = read_u16 () in
      let* rendezvous_id = read rendezvous_length in
      if not (valid_utf_8 rendezvous_id) then
        Error (Invalid_utf8 "rendezvous ID")
      else
        let* base_url_length = read_u16 () in
        let* base_url_string = read base_url_length in
        if not (valid_utf_8 base_url_string) then
          Error (Invalid_utf8 "homeserver base URL")
        else
          match validate_string ~rendezvous_id ~base_url:base_url_string with
          | Error _ as error -> error
          | Ok url -> Ok (value ~intent ~public_key ~rendezvous_id url)

let encoded_base_url url =
  let value = Client.Url.to_string url in
  if String.equal value (Client.Url.origin url ^ "/") then Client.Url.origin url
  else value

let add_u16 buffer value =
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.chr (value land 0xff))

let to_bytes value =
  match
    validate ~rendezvous_id:value.rendezvous_id ~base_url:value.base_url
  with
  | Error _ as e -> e
  | Ok url ->
      let base_url = encoded_base_url url in
      if String.length base_url > 0xffff then
        Error (Field_too_long "homeserver base URL")
      else
        let buffer =
          Buffer.create
            (String.length prefix + 38
            + String.length value.rendezvous_id
            + String.length base_url)
        in
        Buffer.add_string buffer prefix;
        Buffer.add_char buffer (Char.chr qr_type);
        Buffer.add_char buffer (Char.chr (intent_byte value.intent));
        Buffer.add_string buffer (Curve25519.Public.to_bytes value.public_key);
        add_u16 buffer (String.length value.rendezvous_id);
        Buffer.add_string buffer value.rendezvous_id;
        add_u16 buffer (String.length base_url);
        Buffer.add_string buffer base_url;
        Ok (Buffer.contents buffer)

let of_base64 value =
  match Matrix_proto.Base64.decode value with
  | Ok bytes -> of_bytes bytes
  | Error (`Msg msg) -> Error (Invalid_base64 msg)

let to_base64 value = Result.map Matrix_proto.Base64.encode (to_bytes value)
let rendezvous_path = "/_matrix/client/unstable/io.element.msc4388/rendezvous"

type discovery = { create_available : bool }

let discovery_jsont =
  Jsont.Object.(
    map (fun create_available -> { create_available })
    |> mem "create_available" Jsont.bool ~enc:(fun value ->
        value.create_available)
    |> finish)

let unavailable = function
  | Error.Http_error { status = 403 | 404; _ } -> true
  | Error.Matrix_error { errcode = Error.M_FORBIDDEN | Error.M_NOT_FOUND; _ } ->
      true
  | _ -> false

let rendezvous_server_supported client =
  match
    Client.Http.get_bytes_unauthenticated client ~path:rendezvous_path ()
  with
  | Ok (body, _) ->
      Result.map
        (fun response -> response.create_available)
        (Client.Http.decode_response discovery_jsont body)
  | Error error when unavailable error -> Ok false
  | Error _ as error -> error

type secure_channel_error = Invalid_channel_intent | Unsupported_qr_code_type

let establish_secure_channel ~expected_intent value =
  if value.intent = expected_intent then Error Invalid_channel_intent
  else Error Unsupported_qr_code_type

module Msc4108 = struct
  module Curve25519 = Crypto_key.Curve25519

  let rendezvous_path = "/_matrix/client/unstable/org.matrix.msc4108/rendezvous"

  type intent = Login | Reciprocate of Uriz.t

  type code = {
    intent : intent;
    public_key : Curve25519.Public.t;
    rendezvous_url : Uriz.t;
  }

  type t = code

  type codec_error =
    | Not_enough_data
    | Invalid_prefix
    | Invalid_version of int
    | Invalid_intent of int
    | Invalid_public_key
    | Invalid_utf8 of string
    | Invalid_url of string
    | Field_too_long of string
    | Trailing_data
    | Invalid_base64 of string

  let pp_codec_error ppf = function
    | Not_enough_data -> Format.pp_print_string ppf "QR data is missing fields"
    | Invalid_prefix -> Format.pp_print_string ppf "unexpected QR data prefix"
    | Invalid_version got -> Format.fprintf ppf "unsupported QR version %d" got
    | Invalid_intent got -> Format.fprintf ppf "invalid QR intent 0x%02x" got
    | Invalid_public_key ->
        Format.pp_print_string ppf "invalid Curve25519 public key"
    | Invalid_utf8 field -> Format.fprintf ppf "%s is not valid UTF-8" field
    | Invalid_url value ->
        Format.fprintf ppf "%S is not an absolute HTTP(S) URL" value
    | Field_too_long field ->
        Format.fprintf ppf "%s exceeds the QR format's 65535-byte limit" field
    | Trailing_data -> Format.pp_print_string ppf "QR data has trailing bytes"
    | Invalid_base64 msg -> Format.fprintf ppf "invalid QR Base64: %s" msg

  let prefix = "MATRIX"
  let version = 0x02

  let validated_http_url value =
    match Client.Url.of_uri value with
    | Ok url when not (Client.Url.has_fragment url) -> Some url
    | Ok _ | Error _ -> None

  let validated_http_url_string value =
    match Client.Url.of_string value with
    | Ok url when not (Client.Url.has_fragment url) -> Some url
    | Ok _ | Error _ -> None

  let has_ascii_control value =
    String.exists
      (fun byte ->
        let byte = Char.code byte in
        byte <= 0x20 || byte = 0x7f)
      value

  let valid_server_name uri =
    let value = Uriz.to_string uri in
    Option.is_some (validated_http_url uri)
    || Result.is_ok (Matrix_proto.Id.Server_name.of_string value)

  let validate_url ?(server_name = false) field uri =
    let value = Uriz.to_string uri in
    if not (valid_utf_8 value) then Error (Invalid_utf8 field)
    else if String.length value > 0xffff then Error (Field_too_long field)
    else if has_ascii_control value then Error (Invalid_url value)
    else if
      not
        (((not server_name) && Option.is_some (validated_http_url uri))
        || (server_name && valid_server_name uri))
    then Error (Invalid_url value)
    else Ok ()

  let validate ~intent ~public_key ~rendezvous_url =
    if String.length (Curve25519.Public.to_bytes public_key) <> 32 then
      Error Invalid_public_key
    else
      match validate_url "rendezvous URL" rendezvous_url with
      | Error _ as error -> error
      | Ok () -> (
          match intent with
          | Login -> Ok ()
          | Reciprocate server_name ->
              validate_url ~server_name:true "server name" server_name)

  let make ~intent ~public_key ~rendezvous_url =
    match validate ~intent ~public_key ~rendezvous_url with
    | Error _ as error -> error
    | Ok () ->
        let rendezvous_url =
          Client.Url.to_uri (Option.get (validated_http_url rendezvous_url))
        in
        let intent =
          match intent with
          | Login -> Login
          | Reciprocate server_name -> (
              match validated_http_url server_name with
              | Some url -> Reciprocate (Client.Url.to_uri url)
              | None -> Reciprocate server_name)
        in
        Ok { intent; public_key; rendezvous_url }

  let add_u16 buffer value =
    Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
    Buffer.add_char buffer (Char.chr (value land 0xff))

  let to_bytes value =
    match
      validate ~intent:value.intent ~public_key:value.public_key
        ~rendezvous_url:value.rendezvous_url
    with
    | Error _ as error -> error
    | Ok () ->
        let rendezvous_url =
          Client.Url.to_string
            (Option.get (validated_http_url value.rendezvous_url))
        in
        let server_name =
          match value.intent with
          | Login -> None
          | Reciprocate uri ->
              Some
                (match validated_http_url uri with
                | Some url -> Client.Url.to_string url
                | None -> Uriz.to_string uri)
        in
        let buffer = Buffer.create (64 + String.length rendezvous_url) in
        Buffer.add_string buffer prefix;
        Buffer.add_char buffer (Char.chr version);
        Buffer.add_char buffer
          (Char.chr
             (match value.intent with Login -> 0x03 | Reciprocate _ -> 0x04));
        Buffer.add_string buffer (Curve25519.Public.to_bytes value.public_key);
        add_u16 buffer (String.length rendezvous_url);
        Buffer.add_string buffer rendezvous_url;
        Option.iter
          (fun server_name ->
            add_u16 buffer (String.length server_name);
            Buffer.add_string buffer server_name)
          server_name;
        Ok (Buffer.contents buffer)

  let of_bytes bytes =
    let length = String.length bytes in
    let offset = ref 0 in
    let read size =
      if size < 0 || !offset > length - size then Error Not_enough_data
      else
        let value = String.sub bytes !offset size in
        offset := !offset + size;
        Ok value
    in
    let read_byte () =
      match read 1 with
      | Ok value -> Ok (Char.code value.[0])
      | Error _ as error -> error
    in
    let read_u16 () =
      match read 2 with
      | Error _ as error -> error
      | Ok value -> Ok (String.get_uint16_be value 0)
    in
    let ( let* ) result f = Result.bind result f in
    let* got_prefix = read (String.length prefix) in
    if not (String.equal got_prefix prefix) then Error Invalid_prefix
    else
      let* got_version = read_byte () in
      if got_version <> version then Error (Invalid_version got_version)
      else
        let* got_intent = read_byte () in
        let* reciprocate =
          match got_intent with
          | 0x03 -> Ok false
          | 0x04 -> Ok true
          | byte -> Error (Invalid_intent byte)
        in
        let* public_key_bytes = read 32 in
        let* public_key =
          match Curve25519.Public.of_bytes public_key_bytes with
          | Ok key -> Ok key
          | Error _ -> Error Invalid_public_key
        in
        let* rendezvous_length = read_u16 () in
        let* rendezvous_string = read rendezvous_length in
        if not (valid_utf_8 rendezvous_string) then
          Error (Invalid_utf8 "rendezvous URL")
        else if has_ascii_control rendezvous_string then
          Error (Invalid_url rendezvous_string)
        else
          let* rendezvous_url =
            match validated_http_url_string rendezvous_string with
            | Some url -> Ok (Client.Url.to_uri url)
            | None -> Error (Invalid_url rendezvous_string)
          in
          let* server_name =
            match reciprocate with
            | false -> Ok None
            | true -> (
                let* length = read_u16 () in
                let* value = read length in
                if not (valid_utf_8 value) then
                  Error (Invalid_utf8 "server name")
                else if has_ascii_control value then Error (Invalid_url value)
                else
                  match validated_http_url_string value with
                  | Some url -> Ok (Some (Client.Url.to_uri url))
                  | None -> (
                      match Matrix_proto.Id.Server_name.of_string value with
                      | Ok _ -> Ok (Some (Uriz.of_string_exn value))
                      | Error _ -> Error (Invalid_url value)))
          in
          if !offset <> length then Error Trailing_data
          else
            let intent =
              match server_name with
              | None -> Login
              | Some uri -> Reciprocate uri
            in
            make ~intent ~public_key ~rendezvous_url

  let of_base64 value =
    match Matrix_proto.Base64.decode value with
    | Ok bytes -> of_bytes bytes
    | Error (`Msg msg) -> Error (Invalid_base64 msg)

  let to_base64 value = Result.map Matrix_proto.Base64.encode (to_bytes value)

  (** The JSON messages carried by the established MSC4108 channel. This is kept
      next to the channel rather than in the OAuth module: the messages are also
      useful to callers implementing a different OAuth frontend. *)
  module Messages = struct
    type login_protocol =
      | Device_authorization_grant
      | Custom_protocol of string

    type protocol = login_protocol

    type login_failure_reason =
      | Authorization_expired
      | Device_already_exists
      | Device_not_found
      | Unexpected_message_received
      | Unsupported_protocol
      | User_cancelled
      | Custom_failure of string

    type failure_reason = login_failure_reason

    type authorization_grant = {
      verification_uri : Uriz.t;
      verification_uri_complete : Uriz.t option;
    }

    type grant = authorization_grant

    type login_protocols = {
      protocols : login_protocol list;
      homeserver : Uriz.t;
    }

    type login_protocol_message = {
      device_authorization_grant : authorization_grant;
      protocol : login_protocol;
      device_id : string;
    }

    type cross_signing_secrets = {
      master_key : string;
      user_signing_key : string;
      self_signing_key : string;
    }

    type backup_secrets = {
      algorithm : string;
      backup_version : string;
      key : string;
    }

    type backup_secret = backup_secrets

    type secrets_bundle = {
      cross_signing : cross_signing_secrets;
      backup : backup_secrets option;
    }

    type secret_bundle = secrets_bundle

    type t =
      | Login_protocols of login_protocols
      | Login_protocol of login_protocol_message
      | Login_protocol_accepted
      | Login_success
      | Login_declined
      | Login_failure of {
          reason : login_failure_reason;
          homeserver : Uriz.t option;
        }
      | Login_secrets of secrets_bundle

    let login_protocol_of_string = function
      | "device_authorization_grant" -> Device_authorization_grant
      | value -> Custom_protocol value

    let login_protocol_to_string = function
      | Device_authorization_grant -> "device_authorization_grant"
      | Custom_protocol value -> value

    let login_failure_reason_of_string = function
      | "authorization_expired" -> Authorization_expired
      | "device_already_exists" -> Device_already_exists
      | "device_not_found" -> Device_not_found
      | "unexpected_message_received" -> Unexpected_message_received
      | "unsupported_protocol" -> Unsupported_protocol
      | "user_cancelled" -> User_cancelled
      | value -> Custom_failure value

    let login_failure_reason_to_string = function
      | Authorization_expired -> "authorization_expired"
      | Device_already_exists -> "device_already_exists"
      | Device_not_found -> "device_not_found"
      | Unexpected_message_received -> "unexpected_message_received"
      | Unsupported_protocol -> "unsupported_protocol"
      | User_cancelled -> "user_cancelled"
      | Custom_failure value -> value

    let object' members = Jsont.Json.object' members
    let member name value = Jsont.Json.mem (Jsont.Json.name name) value
    let string ?meta value = Jsont.Json.string ?meta value

    let find name = function
      | Jsont.Object (members, _) ->
          Option.map snd (Jsont.Json.find_mem name members)
      | _ -> None

    (* [Jsont.Json.find_mem] intentionally returns the first matching member.
       Authentication messages are a security boundary, however, and the Rust
       serde representation rejects duplicate fields.  Keep the same rule so
       a peer cannot smuggle two interpretations of a message past the typed
       decoder. *)
    let reject_duplicate_members = function
      | Jsont.Object (members, _) ->
          let rec check seen = function
            | [] -> ()
            | ((name, _), _) :: rest ->
                if List.mem name seen then
                  Jsont.Error.msgf Jsont.Meta.none
                    "auth message has duplicate member %S" name
                else check (name :: seen) rest
          in
          check [] members
      | _ -> ()

    let required name json =
      match find name json with
      | Some value -> value
      | None ->
          Jsont.Error.msgf Jsont.Meta.none "missing auth message member %S" name

    let string_member name json =
      match required name json with
      | Jsont.String (value, _) -> value
      | _ ->
          Jsont.Error.msgf Jsont.Meta.none
            "auth message member %S is not a string" name

    (* Every URL crosses the same Fetch validator used by request policy.
       Authentication messages additionally reject fragments because a peer
       must not sign bytes that the HTTP request will silently omit. *)
    let validated_http_url uri =
      match Client.Url.of_uri uri with
      | Ok url when not (Client.Url.has_fragment url) -> Some url
      | Ok _ | Error _ -> None

    let validated_http_url_string value =
      match Client.Url.of_string value with
      | Ok url when not (Client.Url.has_fragment url) -> Some url
      | Ok _ | Error _ -> None

    let has_ascii_control value =
      String.exists
        (fun byte ->
          let byte = Char.code byte in
          byte <= 0x20 || byte = 0x7f)
        value

    let uri ?meta value =
      let raw = Uriz.to_string value in
      if not (valid_utf_8 raw) then
        Jsont.Error.msg Jsont.Meta.none "auth message URL is not UTF-8"
      else if has_ascii_control raw then
        Jsont.Error.msg Jsont.Meta.none
          "auth message URL has control characters"
      else
        match validated_http_url value with
        | Some _ -> Jsont.Json.string ?meta raw
        | None ->
            Jsont.Error.msg Jsont.Meta.none
              "auth message URL is not an HTTP(S) URL"

    let backup_algorithm = "m.megolm_backup.v1.curve25519-aes-sha2"

    let url_member name json =
      let value = string_member name json in
      if not (valid_utf_8 value) then
        Jsont.Error.msgf Jsont.Meta.none "auth message member %S is not UTF-8"
          name
      else if has_ascii_control value then
        Jsont.Error.msgf Jsont.Meta.none
          "auth message member %S has control characters" name
      else
        match validated_http_url_string value with
        | Some _ -> Uriz.of_string_exn value
        | None ->
            Jsont.Error.msgf Jsont.Meta.none
              "auth message member %S is not an HTTP(S) URL" name

    let optional_url_member name json =
      match find name json with
      | None | Some (Jsont.Null _) -> None
      | Some value ->
          let uri = url_member name (object' [ member name value ]) in
          Some uri

    let list_member name json =
      match required name json with
      | Jsont.Array (values, _) -> values
      | _ ->
          Jsont.Error.msgf Jsont.Meta.none
            "auth message member %S is not an array" name

    let protocol_json value = string (login_protocol_to_string value)

    let authorization_grant_of_json json =
      reject_duplicate_members json;
      {
        verification_uri = url_member "verification_uri" json;
        verification_uri_complete =
          optional_url_member "verification_uri_complete" json;
      }

    let authorization_grant_to_json value =
      object'
        [
          member "verification_uri" (uri value.verification_uri);
          member "verification_uri_complete"
            (Jsont.Json.option uri value.verification_uri_complete);
        ]

    let cross_signing_of_json json =
      reject_duplicate_members json;
      {
        master_key = string_member "master_key" json;
        user_signing_key = string_member "user_signing_key" json;
        self_signing_key = string_member "self_signing_key" json;
      }

    let cross_signing_to_json value =
      object'
        [
          member "master_key" (string value.master_key);
          member "user_signing_key" (string value.user_signing_key);
          member "self_signing_key" (string value.self_signing_key);
        ]

    let backup_key value =
      match Matrix_proto.Base64.decode value with
      | Ok value when String.length value = 32 ->
          Matrix_proto.Base64.encode value
      | _ ->
          Jsont.Error.msg Jsont.Meta.none
            "backup key is not a 32-byte Base64 value"

    let validate_backup value =
      if not (String.equal value.algorithm backup_algorithm) then
        Jsont.Error.msgf Jsont.Meta.none "unsupported backup algorithm %S"
          value.algorithm;
      { value with key = backup_key value.key }

    let backup_of_json json =
      reject_duplicate_members json;
      let value =
        {
          algorithm = string_member "algorithm" json;
          backup_version = string_member "backup_version" json;
          key = string_member "key" json;
        }
      in
      validate_backup value

    let backup_to_json ?meta:_ value =
      let value = validate_backup value in
      object'
        [
          member "algorithm" (string value.algorithm);
          member "backup_version" (string value.backup_version);
          member "key" (string value.key);
        ]

    let secrets_bundle_of_json json =
      reject_duplicate_members json;
      {
        cross_signing = cross_signing_of_json (required "cross_signing" json);
        backup =
          (match find "backup" json with
          | None | Some (Jsont.Null _) -> None
          | Some value -> Some (backup_of_json value));
      }

    let secrets_bundle_members value =
      [
        member "cross_signing" (cross_signing_to_json value.cross_signing);
        member "backup" (Jsont.Json.option backup_to_json value.backup);
      ]

    let secrets_bundle_to_json value = object' (secrets_bundle_members value)

    let decode json =
      reject_duplicate_members json;
      match string_member "type" json with
      | "m.login.protocols" ->
          let protocols =
            List.map
              (fun value ->
                match value with
                | Jsont.String (value, _) -> login_protocol_of_string value
                | _ ->
                    Jsont.Error.msg Jsont.Meta.none
                      "auth message protocol is not a string")
              (list_member "protocols" json)
          in
          Login_protocols
            { protocols; homeserver = url_member "homeserver" json }
      | "m.login.protocol" ->
          Login_protocol
            {
              device_authorization_grant =
                authorization_grant_of_json
                  (required "device_authorization_grant" json);
              protocol =
                login_protocol_of_string (string_member "protocol" json);
              device_id = string_member "device_id" json;
            }
      | "m.login.protocol_accepted" -> Login_protocol_accepted
      | "m.login.success" -> Login_success
      | "m.login.declined" -> Login_declined
      | "m.login.failure" ->
          Login_failure
            {
              reason =
                login_failure_reason_of_string (string_member "reason" json);
              homeserver = optional_url_member "homeserver" json;
            }
      | "m.login.secrets" -> Login_secrets (secrets_bundle_of_json json)
      | value ->
          Jsont.Error.msgf Jsont.Meta.none "unknown auth message type %S" value

    let encode = function
      | Login_protocols value ->
          object'
            [
              member "type" (string "m.login.protocols");
              member "protocols"
                (Jsont.Json.list (List.map protocol_json value.protocols));
              member "homeserver" (uri value.homeserver);
            ]
      | Login_protocol value ->
          object'
            [
              member "type" (string "m.login.protocol");
              member "device_authorization_grant"
                (authorization_grant_to_json value.device_authorization_grant);
              member "protocol" (protocol_json value.protocol);
              member "device_id" (string value.device_id);
            ]
      | Login_protocol_accepted ->
          object' [ member "type" (string "m.login.protocol_accepted") ]
      | Login_success -> object' [ member "type" (string "m.login.success") ]
      | Login_declined -> object' [ member "type" (string "m.login.declined") ]
      | Login_failure { reason; homeserver } ->
          object'
            [
              member "type" (string "m.login.failure");
              member "reason" (string (login_failure_reason_to_string reason));
              member "homeserver" (Jsont.Json.option uri homeserver);
            ]
      | Login_secrets value ->
          object'
            (member "type" (string "m.login.secrets")
            :: secrets_bundle_members value)

    let jsont : t Jsont.t =
      Jsont.map ~dec:decode ~enc:encode Matrix_proto.Json.Codec.json

    let message_jsont = jsont
    let auth_message_jsont = jsont
    let of_json json = Jsont.Json.decode jsont json
    let to_json value = Jsont.Json.encode jsont value
    let of_string value = Jsont_bytesrw.decode_string jsont value
    let to_string value = Jsont_bytesrw.encode_string jsont value

    let authorization_grant_login_protocol
        (device_authorization_grant : authorization_grant) ~device_key =
      Login_protocol
        {
          device_authorization_grant;
          protocol = Device_authorization_grant;
          device_id = Crypto_key.Curve25519.Public.to_base64 device_key;
        }
  end

  module Auth_message = Messages
  module Qr_auth_message = Messages

  module Secrets = struct
    type backup = {
      backup_version : string;
      decryption_key : Backup.Decryption_key.t;
    }

    type imported = {
      private_identity : Cross_signing.private_identity;
      backup : backup option;
    }

    type error =
      | Missing_cross_signing_secret of Cross_signing.role
      | Invalid_cross_signing_secret of
          Cross_signing.private_identity_import_error
      | Unsupported_backup_algorithm of string
      | Invalid_backup_key of string

    let pp_error ppf = function
      | Missing_cross_signing_secret role ->
          Format.fprintf ppf "missing %s cross-signing secret"
            (Keys.key_usage_to_string role)
      | Invalid_cross_signing_secret error ->
          Cross_signing.pp_private_identity_import_error ppf error
      | Unsupported_backup_algorithm algorithm ->
          Format.fprintf ppf "unsupported backup algorithm %S" algorithm
      | Invalid_backup_key reason ->
          Format.fprintf ppf "invalid room-key backup decryption key: %s" reason

    let encode_cross_signing_secret secret =
      Matrix_proto.Base64.encode (Crypto_key.Ed25519.Private.to_bytes secret)

    let export ~private_identity ?backup () =
      match Cross_signing.master_secret private_identity with
      | None -> Error (Missing_cross_signing_secret Cross_signing.Master)
      | Some master_key -> (
          match Cross_signing.user_signing_secret private_identity with
          | None ->
              Error (Missing_cross_signing_secret Cross_signing.User_signing)
          | Some user_signing_key -> (
              match Cross_signing.self_signing_secret private_identity with
              | None ->
                  Error
                    (Missing_cross_signing_secret Cross_signing.Self_signing)
              | Some self_signing_key ->
                  let cross_signing : Messages.cross_signing_secrets =
                    {
                      master_key = encode_cross_signing_secret master_key;
                      user_signing_key =
                        encode_cross_signing_secret user_signing_key;
                      self_signing_key =
                        encode_cross_signing_secret self_signing_key;
                    }
                  in
                  let backup =
                    Option.map
                      (fun backup ->
                        ({
                           algorithm = Backup.backup_algorithm;
                           backup_version = backup.backup_version;
                           key =
                             Backup.Decryption_key.to_base64
                               backup.decryption_key;
                         }
                          : Messages.backup_secrets))
                      backup
                  in
                  Ok ({ cross_signing; backup } : Messages.secrets_bundle)))

    let import ~user_id (bundle : Messages.secrets_bundle) =
      let cross_signing = bundle.cross_signing in
      let private_identity =
        Cross_signing.private_identity_of_secrets_unchecked ~user_id
          ~master:cross_signing.master_key
          ~self_signing:cross_signing.self_signing_key
          ~user_signing:cross_signing.user_signing_key
        |> Result.map_error (fun error -> Invalid_cross_signing_secret error)
      in
      let backup =
        match bundle.backup with
        | None -> Ok None
        | Some backup -> (
            if not (String.equal backup.algorithm Backup.backup_algorithm) then
              Error (Unsupported_backup_algorithm backup.algorithm)
            else
              match Backup.Decryption_key.of_base64 backup.key with
              | Error (`Msg reason) -> Error (Invalid_backup_key reason)
              | Ok decryption_key ->
                  Ok
                    (Some
                       {
                         backup_version = backup.backup_version;
                         decryption_key;
                       }))
      in
      match (private_identity, backup) with
      | Error error, _ | _, Error error -> Error error
      | Ok private_identity, Ok backup -> Ok { private_identity; backup }
  end

  module Rendezvous = Qr_login_rendezvous
  module Ecies = Qr_login_ecies

  module Secure_channel = struct
    (** The typed MSC4108 rendezvous/ECIES handshake. OAuth, login approval, and
        handing secrets to the newly logged-in device are intentionally outside
        this boundary. *)
    type error =
      | Rendezvous_error of Rendezvous.error
      | Ecies_error of Ecies.error
      | Invalid_qr_code of codec_error
      | Invalid_intent
      | Secure_channel_message of { expected : string; received : string }
      | Invalid_utf8 of string
      | Invalid_auth_message of string
      | Invalid_check_code
      | Busy
      | Closed
      | Consumed

    let pp_rendezvous_error ppf = function
      | Rendezvous.Transport_error error ->
          Format.fprintf ppf "transport: %a" Error.pp error
      | Rendezvous.Http_error { status; body } ->
          Format.fprintf ppf "HTTP %d: %s" status body
      | Rendezvous.Invalid_response value ->
          Format.fprintf ppf "invalid response: %s" value
      | Rendezvous.Invalid_url value ->
          Format.fprintf ppf "invalid URL: %s" value
      | Rendezvous.Missing_etag -> Format.pp_print_string ppf "missing ETag"
      | Rendezvous.Invalid_content_type value ->
          Format.fprintf ppf "invalid content type: %s" value
      | Rendezvous.Empty_message -> Format.pp_print_string ppf "empty message"
      | Rendezvous.Expired -> Format.pp_print_string ppf "expired"
      | Rendezvous.Closed -> Format.pp_print_string ppf "closed"

    let pp_error ppf = function
      | Rendezvous_error error ->
          Format.fprintf ppf "rendezvous: %a" pp_rendezvous_error error
      | Ecies_error error -> Format.fprintf ppf "ECIES: %a" Ecies.pp_error error
      | Invalid_qr_code _ -> Format.pp_print_string ppf "invalid QR code"
      | Invalid_intent ->
          Format.pp_print_string ppf "QR code has the local intent"
      | Secure_channel_message { expected; received } ->
          Format.fprintf ppf "expected %S, received %S" expected received
      | Invalid_utf8 field -> Format.fprintf ppf "%s is not valid UTF-8" field
      | Invalid_auth_message message ->
          Format.fprintf ppf "invalid QR-login authentication message: %s"
            message
      | Invalid_check_code -> Format.pp_print_string ppf "invalid check code"
      | Busy -> Format.pp_print_string ppf "secure channel is busy"
      | Closed -> Format.pp_print_string ppf "secure channel is closed"
      | Consumed ->
          Format.pp_print_string ppf "secure channel state already consumed"

    type established = {
      rendezvous : Rendezvous.t;
      ecies : Ecies.t;
      mutable busy : bool;
      mutable closed : bool;
    }

    type almost_established = {
      mutable channel : established option;
      mutable invalidated : bool;
    }

    type displayed_session = {
      rendezvous : Rendezvous.t;
      pending : Ecies.pending;
      code : code;
    }

    type displayed = {
      mutable session : displayed_session option;
      mutable busy : bool;
    }

    let rendezvous_error error = Error (Rendezvous_error error)
    let ecies_error error = Error (Ecies_error error)

    let close_rendezvous_best_effort rendezvous =
      (* Cleanup must not replace the exception being handled if the caller is
         already cancelled. [Cancel.protect] still lets the pending
         cancellation propagate at the next operation boundary. *)
      Eio.Cancel.protect (fun () ->
          try
            match Rendezvous.close rendezvous with
            | Ok () -> true
            | Error _ -> false
          with Eio.Io _ -> false)

    let same_intent left right =
      match (left, right) with
      | Login, Login | Reciprocate _, Reciprocate _ -> true
      | _ -> false

    let create ~transport ~rendezvous_server ~random ~intent () =
      match Rendezvous.create transport ~rendezvous_server () with
      | Error error -> rendezvous_error error
      | Ok rendezvous -> (
          try
            let pending = Ecies.create ~random () in
            match
              make ~intent ~public_key:(Ecies.public_key pending)
                ~rendezvous_url:(Rendezvous.rendezvous_url rendezvous)
            with
            | Error error ->
                ignore (close_rendezvous_best_effort rendezvous);
                Error (Invalid_qr_code error)
            | Ok code ->
                Ok
                  { session = Some { rendezvous; pending; code }; busy = false }
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            ignore (close_rendezvous_best_effort rendezvous);
            Printexc.raise_with_backtrace exn bt)

    let login ~transport ~rendezvous_server ~random () =
      create ~transport ~rendezvous_server ~random ~intent:Login ()

    let reciprocate ~transport ~rendezvous_server ~random ~server_name () =
      create ~transport ~rendezvous_server ~random
        ~intent:(Reciprocate server_name) ()

    let qr_code t =
      match t.session with
      | Some session -> session.code
      | None -> invalid_arg "MSC4108 secure channel QR code was consumed"

    let qr_code_base64 t =
      match to_base64 (qr_code t) with
      | Ok value -> value
      | Error _ -> invalid_arg "MSC4108 secure channel QR code is invalid"

    let connect t =
      match t.session with
      | None -> Error Consumed
      | Some _ when t.busy -> Error Busy
      | Some session -> (
          t.busy <- true;
          try
            Fun.protect
              ~finally:(fun () -> t.busy <- false)
              (fun () ->
                let fail error =
                  if close_rendezvous_best_effort session.rendezvous then
                    t.session <- None;
                  Error error
                in
                match Rendezvous.receive session.rendezvous with
                | Error error -> fail (Rendezvous_error error)
                | Ok message -> (
                    match Ecies.establish_inbound session.pending message with
                    | Error error -> fail (Ecies_error error)
                    | Ok (ecies, plaintext) -> (
                        if not (valid_utf_8 plaintext) then
                          fail (Invalid_utf8 "secure-channel message")
                        else if
                          not
                            (String.equal plaintext
                               "MATRIX_QR_CODE_LOGIN_INITIATE")
                        then
                          fail
                            (Secure_channel_message
                               {
                                 expected = "MATRIX_QR_CODE_LOGIN_INITIATE";
                                 received = plaintext;
                               })
                        else
                          match
                            Ecies.encrypt ecies "MATRIX_QR_CODE_LOGIN_OK"
                          with
                          | Error error -> fail (Ecies_error error)
                          | Ok encrypted -> (
                              match
                                Rendezvous.send session.rendezvous encrypted
                              with
                              | Error error -> fail (Rendezvous_error error)
                              | Ok () ->
                                  t.session <- None;
                                  Ok
                                    {
                                      channel =
                                        Some
                                          {
                                            rendezvous = session.rendezvous;
                                            ecies;
                                            busy = false;
                                            closed = false;
                                          };
                                      invalidated = false;
                                    }))))
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            ignore (close_rendezvous_best_effort session.rendezvous);
            Printexc.raise_with_backtrace exn bt)

    let confirm t ~check_code =
      match t.channel with
      | None -> Error Consumed
      | Some _ when t.invalidated -> Error Consumed
      | Some channel ->
          if check_code = Ecies.check_code channel.ecies then begin
            t.channel <- None;
            Ok channel
          end
          else begin
            t.invalidated <- true;
            if close_rendezvous_best_effort channel.rendezvous then
              t.channel <- None;
            Error Invalid_check_code
          end

    let check_code t =
      match t.channel with
      | Some channel when not t.invalidated -> Ecies.check_code channel.ecies
      | None -> invalid_arg "MSC4108 almost-established channel was consumed"
      | Some _ ->
          invalid_arg "MSC4108 almost-established channel was invalidated"

    let from_qr_code ~transport ~random ~expected_intent qr =
      if same_intent expected_intent qr.intent then Error Invalid_intent
      else
        let pending = Ecies.create ~random () in
        match
          Rendezvous.accept transport ~rendezvous_url:qr.rendezvous_url ()
        with
        | Error error -> rendezvous_error error
        | Ok (rendezvous, _initial) -> (
            let fail error =
              ignore (close_rendezvous_best_effort rendezvous);
              Error error
            in
            try
              match
                Ecies.establish_outbound pending ~recipient:qr.public_key
                  ~initial_plaintext:"MATRIX_QR_CODE_LOGIN_INITIATE"
              with
              | Error error -> fail (Ecies_error error)
              | Ok (ecies, encrypted) -> (
                  match Rendezvous.send rendezvous encrypted with
                  | Error error -> fail (Rendezvous_error error)
                  | Ok () -> (
                      let channel =
                        { rendezvous; ecies; busy = false; closed = false }
                      in
                      match Rendezvous.receive rendezvous with
                      | Error error -> fail (Rendezvous_error error)
                      | Ok response -> (
                          match Ecies.decrypt ecies response with
                          | Error error -> fail (Ecies_error error)
                          | Ok plaintext ->
                              if not (valid_utf_8 plaintext) then
                                fail (Invalid_utf8 "secure-channel message")
                              else if
                                String.equal plaintext "MATRIX_QR_CODE_LOGIN_OK"
                              then Ok channel
                              else
                                fail
                                  (Secure_channel_message
                                     {
                                       expected = "MATRIX_QR_CODE_LOGIN_OK";
                                       received = plaintext;
                                     }))))
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              ignore (close_rendezvous_best_effort rendezvous);
              Printexc.raise_with_backtrace exn bt)

    let with_busy (t : established) f =
      if t.closed then Error Closed
      else if t.busy then Error Busy
      else begin
        t.busy <- true;
        Fun.protect ~finally:(fun () -> t.busy <- false) f
      end

    let poison (t : established) =
      t.closed <- true;
      ignore (close_rendezvous_best_effort t.rendezvous)

    let send (t : established) plaintext =
      with_busy t (fun () ->
          if not (valid_utf_8 plaintext) then Error (Invalid_utf8 "message")
          else
            match Ecies.encrypt t.ecies plaintext with
            | Error error -> ecies_error error
            | Ok encrypted -> (
                try
                  match Rendezvous.send t.rendezvous encrypted with
                  | Error error ->
                      poison t;
                      rendezvous_error error
                  | Ok () -> Ok ()
                with exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  poison t;
                  Printexc.raise_with_backtrace exn bt))

    let receive (t : established) =
      with_busy t (fun () ->
          match Rendezvous.receive t.rendezvous with
          | Error error -> rendezvous_error error
          | Ok encrypted -> (
              match Ecies.decrypt t.ecies encrypted with
              | Error
                  ((Ecies.Authentication_failed | Ecies.Counter_exhausted) as
                   error) ->
                  poison t;
                  ecies_error error
              | Error error -> ecies_error error
              | Ok plaintext ->
                  if valid_utf_8 plaintext then Ok plaintext
                  else begin
                    poison t;
                    Error (Invalid_utf8 "secure-channel message")
                  end))

    let send_json t message =
      match Messages.to_string message with
      | Error error -> Error (Invalid_auth_message error)
      | Ok plaintext -> send t plaintext

    let receive_json t =
      match receive t with
      | Error error -> Error error
      | Ok plaintext -> (
          match Messages.of_string plaintext with
          | Ok message -> Ok message
          | Error error -> Error (Invalid_auth_message error))

    let send_message = send_json
    let receive_message = receive_json
    let check_code_established (t : established) = Ecies.check_code t.ecies

    let close (t : established) =
      if t.closed then Ok ()
      else if t.busy then Error Busy
      else begin
        t.busy <- true;
        Fun.protect
          ~finally:(fun () -> t.busy <- false)
          (fun () ->
            match Rendezvous.close t.rendezvous with
            | Ok () ->
                t.closed <- true;
                Ok ()
            | Error error -> rendezvous_error error)
      end

    let cancel_displayed t =
      if t.busy then Error Busy
      else
        match t.session with
        | None -> Ok ()
        | Some session ->
            t.busy <- true;
            Fun.protect
              ~finally:(fun () -> t.busy <- false)
              (fun () ->
                match Rendezvous.close session.rendezvous with
                | Ok () ->
                    t.session <- None;
                    Ok ()
                | Error error -> rendezvous_error error)

    let cancel_almost t =
      match t.channel with
      | None -> Ok ()
      | Some channel -> (
          t.invalidated <- true;
          match close channel with
          | Ok () ->
              t.channel <- None;
              Ok ()
          | Error error -> Error error)
  end

  (** The transport-independent part of MSC4108. This module deliberately does
      not establish, close, or cancel a secure channel. OAuth, device creation
      and secret storage are supplied by the [*_hooks] records. *)
  module Application = struct
    let ( let* ) result f = Result.bind result f

    type 'e channel = {
      send : Messages.t -> (unit, 'e) result;
      receive : unit -> (Messages.t, 'e) result;
    }

    let secure_channel (channel : Secure_channel.established) =
      {
        send = (fun message -> Secure_channel.send_json channel message);
        receive = (fun () -> Secure_channel.receive_json channel);
      }

    type login_start = Await_protocols | Homeserver_known of Uriz.t
    type token_failure = Access_denied | Expired | Token_error of string

    type 'token authorization = {
      grant : Messages.authorization_grant;
      device_id : string;
      user_code : string;
      await_token : unit -> ('token, token_failure) result;
    }

    type login_progress =
      | Starting
      | Waiting_for_token of { user_code : string }
      | Syncing_secrets
      | Done

    type 'token login_hooks = {
      prepare :
        homeserver:Uriz.t option -> ('token authorization, string) result;
      activate : device_id:string -> 'token -> (unit, string) result;
      import_secrets : Messages.secrets_bundle -> (unit, string) result;
      on_progress : login_progress -> unit;
    }

    type grant_start = Advertise_protocols of Uriz.t | Protocols_already_known
    type grant_decision = Confirm | Cancel

    type grant_progress =
      | Grant_starting
      | Waiting_for_authorization of { verification_uri : Uriz.t }
      | Grant_syncing_secrets
      | Grant_done

    type grant_hooks = {
      export_secrets : unit -> (Messages.secrets_bundle, string) result;
      device_exists : string -> (bool, string) result;
      authorize : Uriz.t -> (grant_decision, string) result;
      await_device : string -> (bool, string) result;
      on_progress : grant_progress -> unit;
    }

    type 'e error =
      | Channel_error of 'e
      | Unexpected_message of { expected : string; received : Messages.t }
      | Peer_failure of {
          reason : Messages.login_failure_reason;
          homeserver : Uriz.t option;
        }
      | Unsupported_protocol of Messages.login_protocol
      | No_supported_protocol
      | Device_already_exists
      | Device_not_found
      | User_cancelled
      | Authorization_denied
      | Authorization_expired
      | Local_error of string

    let message_name = function
      | Messages.Login_protocols _ -> "m.login.protocols"
      | Messages.Login_protocol _ -> "m.login.protocol"
      | Messages.Login_protocol_accepted -> "m.login.protocol_accepted"
      | Messages.Login_success -> "m.login.success"
      | Messages.Login_declined -> "m.login.declined"
      | Messages.Login_failure _ -> "m.login.failure"
      | Messages.Login_secrets _ -> "m.login.secrets"

    let pp_error pp_channel_error ppf = function
      | Channel_error error ->
          Format.fprintf ppf "channel: %a" pp_channel_error error
      | Unexpected_message { expected; received } ->
          Format.fprintf ppf "expected %s, received %s" expected
            (message_name received)
      | Peer_failure { reason; homeserver } ->
          Format.fprintf ppf "peer failure %s%s"
            (Messages.login_failure_reason_to_string reason)
            (match homeserver with
            | None -> ""
            | Some uri ->
                Format.asprintf " (homeserver %s)" (Uriz.to_string uri))
      | Unsupported_protocol protocol ->
          Format.fprintf ppf "unsupported protocol %s"
            (Messages.login_protocol_to_string protocol)
      | No_supported_protocol ->
          Format.pp_print_string ppf "no supported login protocol"
      | Device_already_exists ->
          Format.pp_print_string ppf "device already exists"
      | Device_not_found -> Format.pp_print_string ppf "device not found"
      | User_cancelled -> Format.pp_print_string ppf "user cancelled"
      | Authorization_denied ->
          Format.pp_print_string ppf "authorization denied"
      | Authorization_expired ->
          Format.pp_print_string ppf "authorization expired"
      | Local_error error -> Format.fprintf ppf "local error: %s" error

    let send channel message =
      match channel.send message with
      | Ok () -> Ok ()
      | Error error -> Error (Channel_error error)

    let receive channel =
      match channel.receive () with
      | Ok message -> Ok message
      | Error error -> Error (Channel_error error)

    let failure ~reason ?homeserver () =
      Messages.Login_failure { reason; homeserver }

    let report_login (hooks : 'token login_hooks) (progress : login_progress) =
      hooks.on_progress progress

    let report_grant (hooks : grant_hooks) (progress : grant_progress) =
      hooks.on_progress progress

    let run_login ~start ~channel ~hooks =
      report_login hooks Starting;
      let* homeserver =
        match start with
        | Homeserver_known homeserver -> Ok (Some homeserver)
        | Await_protocols -> (
            match receive channel with
            | Error error -> Error error
            | Ok (Messages.Login_protocols { protocols; homeserver }) ->
                if
                  List.exists
                    (function
                      | Messages.Device_authorization_grant -> true
                      | Messages.Custom_protocol _ -> false)
                    protocols
                then Ok (Some homeserver)
                else
                  let* () =
                    send channel
                      (failure ~reason:Messages.Unsupported_protocol ())
                  in
                  Error No_supported_protocol
            | Ok message ->
                let* () =
                  send channel
                    (failure ~reason:Messages.Unexpected_message_received ())
                in
                Error
                  (Unexpected_message
                     { expected = "m.login.protocols"; received = message }))
      in
      let* authorization =
        match hooks.prepare ~homeserver with
        | Ok authorization -> Ok authorization
        | Error error -> Error (Local_error error)
      in
      let* () =
        send channel
          (Messages.Login_protocol
             {
               device_authorization_grant = authorization.grant;
               protocol = Messages.Device_authorization_grant;
               device_id = authorization.device_id;
             })
      in
      let* () =
        match receive channel with
        | Error error -> Error error
        | Ok Messages.Login_protocol_accepted -> Ok ()
        | Ok (Messages.Login_failure { reason; homeserver }) ->
            Error (Peer_failure { reason; homeserver })
        | Ok message ->
            let* () =
              send channel
                (failure ~reason:Messages.Unexpected_message_received ())
            in
            Error
              (Unexpected_message
                 { expected = "m.login.protocol_accepted"; received = message })
      in
      report_login hooks
        (Waiting_for_token { user_code = authorization.user_code });
      let* token =
        match authorization.await_token () with
        | Ok token -> Ok token
        | Error Access_denied ->
            let* () = send channel Messages.Login_declined in
            Error Authorization_denied
        | Error Expired ->
            let* () =
              send channel (failure ~reason:Messages.Authorization_expired ())
            in
            Error Authorization_expired
        | Error (Token_error error) -> Error (Local_error error)
      in
      let* () =
        match hooks.activate ~device_id:authorization.device_id token with
        | Ok () -> Ok ()
        | Error error -> Error (Local_error error)
      in
      report_login hooks Syncing_secrets;
      let* () = send channel Messages.Login_success in
      let* secrets =
        match receive channel with
        | Error error -> Error error
        | Ok (Messages.Login_secrets secrets) -> Ok secrets
        | Ok (Messages.Login_failure { reason; homeserver }) ->
            Error (Peer_failure { reason; homeserver })
        | Ok message ->
            let* () =
              send channel
                (failure ~reason:Messages.Unexpected_message_received ())
            in
            Error
              (Unexpected_message
                 { expected = "m.login.secrets"; received = message })
      in
      let* () =
        match hooks.import_secrets secrets with
        | Ok () -> Ok ()
        | Error error -> Error (Local_error error)
      in
      report_login hooks Done;
      Ok ()

    let run_grant ~start ~channel ~hooks =
      report_grant hooks Grant_starting;
      let* secrets =
        match hooks.export_secrets () with
        | Ok secrets -> Ok secrets
        | Error error -> Error (Local_error error)
      in
      let* () =
        match start with
        | Protocols_already_known -> Ok ()
        | Advertise_protocols homeserver ->
            send channel
              (Messages.Login_protocols
                 {
                   protocols = [ Messages.Device_authorization_grant ];
                   homeserver;
                 })
      in
      let* protocol =
        match receive channel with
        | Error error -> Error error
        | Ok
            (Messages.Login_protocol
               { device_authorization_grant; protocol; device_id }) ->
            Ok (device_authorization_grant, protocol, device_id)
        | Ok (Messages.Login_failure { reason; homeserver }) ->
            Error (Peer_failure { reason; homeserver })
        | Ok message ->
            Error
              (Unexpected_message
                 { expected = "m.login.protocol"; received = message })
      in
      let grant, selected_protocol, device_id = protocol in
      if selected_protocol <> Messages.Device_authorization_grant then begin
        let* () =
          send channel (failure ~reason:Messages.Unsupported_protocol ())
        in
        Error (Unsupported_protocol selected_protocol)
      end
      else
        let* exists =
          match hooks.device_exists device_id with
          | Ok exists -> Ok exists
          | Error error -> Error (Local_error error)
        in
        if exists then begin
          let* () =
            send channel (failure ~reason:Messages.Device_already_exists ())
          in
          Error Device_already_exists
        end
        else
          let verification_uri =
            Option.value grant.verification_uri_complete
              ~default:grant.verification_uri
          in
          report_grant hooks (Waiting_for_authorization { verification_uri });
          let* decision =
            match hooks.authorize verification_uri with
            | Ok decision -> Ok decision
            | Error error -> Error (Local_error error)
          in
          match decision with
          | Cancel ->
              let* () =
                send channel (failure ~reason:Messages.User_cancelled ())
              in
              Error User_cancelled
          | Confirm ->
              let* () = send channel Messages.Login_protocol_accepted in
              let* () =
                match receive channel with
                | Error error -> Error error
                | Ok Messages.Login_success -> Ok ()
                | Ok (Messages.Login_failure { reason; homeserver }) ->
                    Error (Peer_failure { reason; homeserver })
                | Ok message ->
                    Error
                      (Unexpected_message
                         { expected = "m.login.success"; received = message })
              in
              let* present =
                match hooks.await_device device_id with
                | Ok present -> Ok present
                | Error error -> Error (Local_error error)
              in
              if not present then begin
                let* () =
                  send channel (failure ~reason:Messages.Device_not_found ())
                in
                Error Device_not_found
              end
              else begin
                report_grant hooks Grant_syncing_secrets;
                let* () = send channel (Messages.Login_secrets secrets) in
                report_grant hooks Grant_done;
                Ok ()
              end
  end
end
