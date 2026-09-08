type versions = Matrix_client.Server.versions = {
  versions : string list;
  unstable_features : (string * bool) list;
}

type room_version_stability = Matrix_client.Server.room_version_stability

type room_versions_capability =
      Matrix_client.Server.room_versions_capability = {
  default : string;
  available : (string * room_version_stability) list;
}

type capabilities = Matrix_client.Server.capabilities = {
  change_password : bool option;
  room_versions : room_versions_capability option;
  set_displayname : bool option;
  set_avatar_url : bool option;
  thirdparty_id_changes : bool option;
  get_login_token : bool option;
  custom : (string * Jsont.json) list;
}

type account_moderation_capability =
      Matrix_client.Server.account_moderation_capability = {
  suspend : bool;
  lock : bool;
}

type profile_fields_capability =
      Matrix_client.Server.profile_fields_capability = {
  enabled : bool;
  allowed : string list option;
  disallowed : string list option;
}

type server_info = Matrix_client.Server.server_info = { base_url : Uriz.t }

type authentication_info = Matrix_client.Server.authentication_info = {
  issuer : string;
  account : string option;
}

type well_known = Matrix_client.Server.well_known = {
  homeserver : server_info;
  identity_server : server_info option;
  authentication : authentication_info option;
}

type discovery = Matrix_client.Server.discovery = {
  base_url : Uriz.t;
  well_known : well_known option;
  server_versions : versions option;
}

let get_versions client =
  Error.unwrap ~context:"getting server versions"
    (Matrix_client.Server.get_versions (Client.base client))

let supports_version = Matrix_client.Server.supports_version
let supports_version_at_least = Matrix_client.Server.supports_version_at_least
let has_unstable_feature = Matrix_client.Server.has_unstable_feature

let invalidate_cache client =
  Matrix_client.Server.invalidate_cache (Client.base client)

let get_capabilities client =
  Error.unwrap ~context:"getting server capabilities"
    (Matrix_client.Server.get_capabilities (Client.base client))

let refresh_capabilities client =
  Error.unwrap ~context:"refreshing server capabilities"
    (Matrix_client.Server.refresh_capabilities (Client.base client))

let find_capability = Matrix_client.Server.find_capability

let can_change_password client =
  Error.unwrap ~context:"checking password-change capability"
    (Matrix_client.Server.can_change_password (Client.base client))

let can_change_thirdparty_ids client =
  Error.unwrap ~context:"checking third-party-id capability"
    (Matrix_client.Server.can_change_thirdparty_ids (Client.base client))

let can_get_login_token client =
  Error.unwrap ~context:"checking login-token capability"
    (Matrix_client.Server.can_get_login_token (Client.base client))

let room_versions client =
  Error.unwrap ~context:"getting room-version capability"
    (Matrix_client.Server.room_versions (Client.base client))

let account_moderation client =
  Error.unwrap ~context:"getting account-moderation capability"
    (Matrix_client.Server.account_moderation (Client.base client))

let forgets_room_when_leaving client =
  Error.unwrap ~context:"checking leave-room behavior"
    (Matrix_client.Server.forgets_room_when_leaving (Client.base client))

let can_change_displayname client =
  Error.unwrap ~context:"checking display-name capability"
    (Matrix_client.Server.can_change_displayname (Client.base client))

let can_change_avatar client =
  Error.unwrap ~context:"checking avatar capability"
    (Matrix_client.Server.can_change_avatar (Client.base client))

let extended_profile_fields client =
  Error.unwrap ~context:"getting extended-profile capability"
    (Matrix_client.Server.extended_profile_fields (Client.base client))

let get_well_known client =
  Error.unwrap ~context:"getting server well-known"
    (Matrix_client.Server.get_well_known (Client.base client))

let discover client =
  let get_versions_at uri =
    let config_result =
      try
        Ok
          (Matrix_client.Client.config ~homeserver:uri
             ~well_known_policy:Matrix_client.Client.Do_not_query ())
      with Invalid_argument msg ->
        Error (Matrix_client.Error.Policy_denied msg)
    in
    match config_result with
    | Error _ as e -> e
    | Ok config ->
        let delegated =
          Matrix_client.Client.create ~config ~fetch:(Client.http client)
            ~random:(Matrix_client.Client.random (Client.base client))
        in
        Matrix_client.Server.get_versions delegated
  in
  Error.unwrap ~context:"discovering server"
    (Matrix_client.Server.discover ~get_versions_at (Client.base client))
