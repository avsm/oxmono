open Result.Syntax

let src = Logs.Src.create "matrix.server" ~doc:"Matrix server discovery"

module Log = (val Logs.src_log src : Logs.LOG)
module String_map = Map.MakePortable (String)

(* A JSON object of uniform values, as an association list sorted by key. *)
let string_map_of = Matrix_proto.Json.Codec.string_map

type versions = {
  versions : string list;
  unstable_features : (string * bool) list;
}

let versions_jsont =
  Jsont.Object.(
    map ~kind:"versions" (fun versions unstable_features ->
        { versions; unstable_features })
    |> mem "versions"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.versions)
    |> mem "unstable_features" (string_map_of Jsont.bool)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.unstable_features)
    |> finish)

(* [/versions] hangs off the client API root, not off [/_matrix/client/v3],
   so it goes through the absolute-path helper. *)
let versions_path = "/_matrix/client/versions"

let get_versions client =
  let cache = Client.server_metadata_cache client in
  match Client.Server_metadata_cache.get cache `Versions with
  | Some body -> Client.Http.decode_response versions_jsont body
  | None ->
      let* body, _content_type =
        Client.Http.get_bytes client ~path:versions_path ()
      in
      let* versions = Client.Http.decode_response versions_jsont body in
      Client.Server_metadata_cache.set cache `Versions body;
      Ok versions

let supports_version v version = List.mem version v.versions

let stable_version_number version =
  match String.split_on_char '.' version with
  | [ major; minor ] when String.starts_with ~prefix:"v" major -> (
      let major = String.sub major 1 (String.length major - 1) in
      match (int_of_string_opt major, int_of_string_opt minor) with
      | Some major, Some minor -> Some (major, minor)
      | _ -> None)
  | _ -> None

let supports_version_at_least v ~major ~minor =
  List.exists
    (fun version ->
      match stable_version_number version with
      | Some (got_major, got_minor) ->
          got_major > major || (got_major = major && got_minor >= minor)
      | None -> false)
    v.versions

let has_unstable_feature v feature =
  match List.assoc_opt feature v.unstable_features with
  | Some enabled -> enabled
  | None -> false

type room_version_stability = [ `Stable | `Unstable | `Other of string ]

let room_version_stability_of_string = function
  | "stable" -> `Stable
  | "unstable" -> `Unstable
  | s -> `Other s

let room_version_stability_to_string = function
  | `Stable -> "stable"
  | `Unstable -> "unstable"
  | `Other s -> s

let room_version_stability_jsont =
  Jsont.of_of_string ~kind:"room_version_stability"
    ~enc:room_version_stability_to_string (fun s ->
      Ok (room_version_stability_of_string s))

type room_versions_capability = {
  default : string;
  available : (string * room_version_stability) list;
}

let room_versions_capability_jsont =
  Jsont.Object.(
    map ~kind:"m.room_versions" (fun default available ->
        { default; available })
    |> mem "default" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.default)
    |> mem "available" (string_map_of room_version_stability_jsont)
         ~enc:(fun t -> t.available)
    |> finish)

(* Every spec capability but [m.room_versions] is an object with a single
   [enabled] boolean. *)
let enabled_capability_jsont =
  Jsont.Object.(
    map ~kind:"capability" (fun enabled -> enabled)
    |> mem "enabled" Jsont.bool ~enc:Fun.id
    |> finish)

type capabilities = {
  change_password : bool option;
  room_versions : room_versions_capability option;
  set_displayname : bool option;
  set_avatar_url : bool option;
  thirdparty_id_changes : bool option;
  get_login_token : bool option;
  custom : (string * Jsont.json) list;
}

let capabilities_jsont =
  Jsont.Object.(
    map ~kind:"capabilities"
      (fun
        change_password
        room_versions
        set_displayname
        set_avatar_url
        thirdparty_id_changes
        get_login_token
        custom
      ->
        {
          change_password;
          room_versions;
          set_displayname;
          set_avatar_url;
          thirdparty_id_changes;
          get_login_token;
          custom = String_map.bindings custom;
        })
    |> opt_mem "m.change_password" enabled_capability_jsont ~enc:(fun t ->
        t.change_password)
    |> opt_mem "m.room_versions" room_versions_capability_jsont ~enc:(fun t ->
        t.room_versions)
    |> opt_mem "m.set_displayname" enabled_capability_jsont ~enc:(fun t ->
        t.set_displayname)
    |> opt_mem "m.set_avatar_url" enabled_capability_jsont ~enc:(fun t ->
        t.set_avatar_url)
    |> opt_mem "m.3pid_changes" enabled_capability_jsont ~enc:(fun t ->
        t.thirdparty_id_changes)
    |> opt_mem "m.get_login_token" enabled_capability_jsont ~enc:(fun t ->
        t.get_login_token)
    (* Anything the server sends that this module does not model — a
       [m.forget_forced_upon_leave], a vendor [com.example.*] — is kept
       verbatim rather than dropped. *)
    |> keep_unknown
         (Matrix_proto.Json.Codec.string_map_mems Matrix_proto.Json.Codec.json)
         ~enc:(fun t -> String_map.of_seq (List.to_seq t.custom))
    |> finish)

let capabilities_response_jsont =
  Jsont.Object.(
    map ~kind:"capabilities_response" Fun.id
    |> mem "capabilities" capabilities_jsont ~enc:Fun.id
    |> finish)

let refresh_capabilities client =
  let cache = Client.server_metadata_cache client in
  let* body = Client.Http.get client ~path:"/capabilities" () in
  let* capabilities =
    Client.Http.decode_response capabilities_response_jsont body
  in
  Client.Server_metadata_cache.set cache `Capabilities body;
  Ok capabilities

let get_capabilities client =
  let cache = Client.server_metadata_cache client in
  match Client.Server_metadata_cache.get cache `Capabilities with
  | Some body -> Client.Http.decode_response capabilities_response_jsont body
  | None -> refresh_capabilities client

let invalidate_cache client =
  Client.Server_metadata_cache.clear (Client.server_metadata_cache client)

let find_capability c ~name = List.assoc_opt name c.custom
let default_room_versions = { default = "1"; available = [ ("1", `Stable) ] }

type account_moderation_capability = { suspend : bool; lock : bool }

let account_moderation_capability_jsont =
  Jsont.Object.(
    map ~kind:"m.account_moderation" (fun suspend lock -> { suspend; lock })
    |> mem "suspend" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.suspend)
    |> mem "lock" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.lock)
    |> finish)

let decode_custom_capability capabilities ~name jsont =
  match find_capability capabilities ~name with
  | None -> Ok None
  | Some raw -> (
      match Jsont.Json.decode jsont raw with
      | Ok capability -> Ok (Some capability)
      | Error message -> Error (Error.Json_error message))

let can_change_password client =
  let* capabilities = get_capabilities client in
  Ok (Option.value ~default:true capabilities.change_password)

let can_change_thirdparty_ids client =
  let* capabilities = get_capabilities client in
  Ok (Option.value ~default:true capabilities.thirdparty_id_changes)

let can_get_login_token client =
  let* capabilities = get_capabilities client in
  Ok (Option.value ~default:false capabilities.get_login_token)

let room_versions client =
  let* capabilities = get_capabilities client in
  Ok (Option.value ~default:default_room_versions capabilities.room_versions)

let account_moderation client =
  let* capabilities = get_capabilities client in
  let* moderation =
    decode_custom_capability capabilities ~name:"m.account_moderation"
      account_moderation_capability_jsont
  in
  Ok (Option.value ~default:{ suspend = false; lock = false } moderation)

let forgets_room_when_leaving client =
  let* capabilities = get_capabilities client in
  let* forgets =
    decode_custom_capability capabilities ~name:"m.forget_forced_upon_leave"
      enabled_capability_jsont
  in
  Ok (Option.value ~default:false forgets)

(* [m.profile_fields] is newer than the other profile capabilities, so keep
   it raw in [capabilities] for forwards compatibility and decode it only when
   one of the profile policy helpers is used. *)
type profile_fields_capability = {
  enabled : bool;
  allowed : string list option;
  disallowed : string list option;
}

let profile_fields_capability_jsont =
  Jsont.Object.(
    map ~kind:"m.profile_fields" (fun enabled allowed disallowed ->
        { enabled; allowed; disallowed })
    |> mem "enabled" Jsont.bool ~enc:(fun t -> t.enabled)
    |> opt_mem "allowed" (Jsont.list Matrix_proto.Json.Codec.string)
         ~enc:(fun t -> t.allowed)
    |> opt_mem "disallowed" (Jsont.list Matrix_proto.Json.Codec.string)
         ~enc:(fun t -> t.disallowed)
    |> finish)

let decode_profile_fields = function
  | None -> Ok None
  | Some raw -> (
      match Jsont.Json.decode profile_fields_capability_jsont raw with
      | Ok capability -> Ok (Some capability)
      | Error message -> Error (Error.Json_error message))

let profile_field_allowed capability field =
  if not capability.enabled then false
  else
    match capability.allowed with
    | Some allowed -> List.mem field allowed
    | None -> (
        match capability.disallowed with
        | Some disallowed -> not (List.mem field disallowed)
        | None -> true)

let can_change_profile_field client ~field ~legacy =
  let* capabilities = get_capabilities client in
  let* profile_fields =
    decode_profile_fields
      (find_capability capabilities ~name:"m.profile_fields")
  in
  match profile_fields with
  | Some capability -> Ok (profile_field_allowed capability field)
  | None ->
      let* versions = get_versions client in
      if supports_version_at_least versions ~major:1 ~minor:16 then Ok true
      else Ok (Option.value ~default:true (legacy capabilities))

let can_change_displayname client =
  can_change_profile_field client ~field:"displayname"
    ~legacy:(fun capabilities -> capabilities.set_displayname)

let can_change_avatar client =
  can_change_profile_field client ~field:"avatar_url"
    ~legacy:(fun capabilities -> capabilities.set_avatar_url)

let extended_profile_fields client =
  let* capabilities = get_capabilities client in
  let* profile_fields =
    decode_profile_fields
      (find_capability capabilities ~name:"m.profile_fields")
  in
  match profile_fields with
  | Some capability -> Ok capability
  | None ->
      let* versions = get_versions client in
      Ok
        {
          enabled = supports_version_at_least versions ~major:1 ~minor:16;
          allowed = None;
          disallowed = None;
        }

type server_info = { base_url : Uriz.t }

let homeserver_uri_jsont =
  Jsont.of_of_string ~kind:"Matrix homeserver URL" ~enc:Uriz.to_string
    (fun value ->
      match Client.Url.homeserver_string value with
      | Ok url ->
          Ok
            (if Client.Url.path_segments url = [] then
               Uriz.of_string_exn (Client.Url.origin url)
             else Client.Url.to_uri url)
      | Error reason -> Error (Printf.sprintf "%S: %s" value reason))

let server_info_jsont =
  Jsont.Object.(
    map ~kind:"server_info" (fun base_url -> { base_url })
    |> mem "base_url" homeserver_uri_jsont ~enc:(fun t -> t.base_url)
    |> finish)

type authentication_info = { issuer : string; account : string option }

let http_url_string_jsont ?(issuer = false) kind =
  Jsont.of_of_string ~kind ~enc:Fun.id (fun value ->
      match Client.Url.of_string value with
      | Error reason -> Error (Printf.sprintf "%S: %s" value reason)
      | Ok url
        when issuer && (Client.Url.has_query url || Client.Url.has_fragment url)
        ->
          Error "issuer URL must not contain a query or fragment"
      | Ok url -> Ok (Client.Url.effective_string url))

let authentication_info_jsont =
  Jsont.Object.(
    map ~kind:"authentication_info" (fun issuer account -> { issuer; account })
    |> mem "issuer" (http_url_string_jsont ~issuer:true "OAuth issuer URL")
         ~enc:(fun t -> t.issuer)
    |> opt_mem "account" (http_url_string_jsont "account-management URL")
         ~enc:(fun t -> t.account)
    |> finish)

type well_known = {
  homeserver : server_info;
  identity_server : server_info option;
  authentication : authentication_info option;
}

(* [m.authentication] is the Matrix 1.15 name; servers that predate it use
   the MSC2965 unstable one. Both are decoded, the stable one wins, and only
   the stable one is written back. *)
let well_known_jsont =
  Jsont.Object.(
    map ~kind:"well_known"
      (fun homeserver identity_server authentication unstable_authentication ->
        let authentication =
          match authentication with
          | Some _ as a -> a
          | None -> unstable_authentication
        in
        { homeserver; identity_server; authentication })
    |> mem "m.homeserver" server_info_jsont ~enc:(fun t -> t.homeserver)
    |> opt_mem "m.identity_server" server_info_jsont ~enc:(fun t ->
        t.identity_server)
    |> opt_mem "m.authentication" authentication_info_jsont ~enc:(fun t ->
        t.authentication)
    |> opt_mem "org.matrix.msc2965.authentication" authentication_info_jsont
         ~enc:(fun _ -> None)
    |> finish)

let well_known_path = "/.well-known/matrix/client"

let get_well_known client =
  match Client.well_known_policy client with
  | Client.Do_not_query -> Ok None
  | Client.Query -> (
      (* Well-known is public discovery metadata. Never send a bearer token,
         even when the caller is already logged in. *)
      match
        Client.Http.get_bytes_unauthenticated client ~path:well_known_path ()
      with
      | Error (Error.Http_error { status = 404; _ })
      | Error (Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }) ->
          Log.debug (fun m -> m "No %s on this origin" well_known_path);
          Ok None
      | Error e -> Error e
      | Ok (body, _content_type) ->
          let+ wk = Client.Http.decode_response well_known_jsont body in
          Some wk)

type discovery = {
  base_url : Uriz.t;
  well_known : well_known option;
  server_versions : versions option;
}

type get_versions_at = Uriz.t -> (versions, Error.t) result

let get_versions_at_same_origin client base_url =
  match Client.Url.homeserver base_url with
  | Error reason ->
      Error (Error.Json_error ("invalid discovered base URL: " ^ reason))
  | Ok base -> (
      match Client.Url.append_path base ~path:versions_path () with
      | Error reason ->
          Error
            (Error.Json_error ("invalid discovered versions URL: " ^ reason))
      | Ok url ->
          let* body, _content_type, _cache_control =
            Client.Http.get_url_with_cache_control client ~url ()
          in
          Client.Http.decode_response versions_jsont body)

let discover ?get_versions_at client =
  let configured = Client.homeserver client in
  let* well_known = get_well_known client in
  let base_url =
    match well_known with
    | Some wk -> wk.homeserver.base_url
    | None -> configured
  in
  Log.info (fun m -> m "Discovered base URL %s" (Uriz.to_string base_url));
  let discovered_url =
    match Client.Url.homeserver base_url with
    | Ok url -> url
    | Error reason ->
        (* [well_known_jsont] and [Client.config] both validate this first. *)
        invalid_arg ("Matrix_client.Server.discover: " ^ reason)
  in
  if
    String.equal
      (Client.Url.to_string discovered_url)
      (Client.Url.to_string (Client.homeserver_url client))
  then
    let+ v = get_versions client in
    { base_url; well_known; server_versions = Some v }
  else if Client.Url.same_origin discovered_url (Client.homeserver_url client)
  then
    let+ v = get_versions_at_same_origin client base_url in
    { base_url; well_known; server_versions = Some v }
  else
    match get_versions_at with
    | Some get_versions_at ->
        let+ v = get_versions_at base_url in
        { base_url; well_known; server_versions = Some v }
    | None -> begin
        (* [Client.create] restricted this client to [configured]'s origin, so
           it cannot legally reach the base URL the well-known named. *)
        Log.info (fun m ->
            m "Base URL %s is off-origin; caller must rebuild the client"
              (Uriz.to_string base_url));
        Ok { base_url; well_known; server_versions = None }
      end
