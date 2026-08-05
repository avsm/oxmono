(** {1 Immich}

    Immich API

    @version 2.4.1 *)

type t = {
  session : Requests.t;
  base_url : string;
}

let create ?session ~sw env ~base_url =
  let session = match session with
    | Some s -> s
    | None -> Requests.create ~sw env
  in
  { session; base_url }

let base_url t = t.base_url
let session t = t.session

module WorkflowFilterItem = struct
  module Types = struct
    module Dto = struct
      type t = {
        filter_config : Jsont.json option;
        plugin_filter_id : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~plugin_filter_id ?filter_config () = { filter_config; plugin_filter_id }
    
    let filter_config t = t.filter_config
    let plugin_filter_id t = t.plugin_filter_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowFilterItemDto"
        (fun filter_config plugin_filter_id -> { filter_config; plugin_filter_id })
      |> Jsont.Object.opt_mem "filterConfig" Jsont.json ~enc:(fun r -> r.filter_config)
      |> Jsont.Object.mem "pluginFilterId" Jsont.string ~enc:(fun r -> r.plugin_filter_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module WorkflowFilter = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        filter_config : Jsont.json option;
        id : string;
        order : float;
        plugin_filter_id : string;
        workflow_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~id ~order ~plugin_filter_id ~workflow_id ?filter_config () = { filter_config; id; order; plugin_filter_id; workflow_id }
    
    let filter_config t = t.filter_config
    let id t = t.id
    let order t = t.order
    let plugin_filter_id t = t.plugin_filter_id
    let workflow_id t = t.workflow_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowFilterResponseDto"
        (fun filter_config id order plugin_filter_id workflow_id -> { filter_config; id; order; plugin_filter_id; workflow_id })
      |> Jsont.Object.mem "filterConfig" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.filter_config)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "order" Jsont.number ~enc:(fun r -> r.order)
      |> Jsont.Object.mem "pluginFilterId" Jsont.string ~enc:(fun r -> r.plugin_filter_id)
      |> Jsont.Object.mem "workflowId" Jsont.string ~enc:(fun r -> r.workflow_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module WorkflowActionItem = struct
  module Types = struct
    module Dto = struct
      type t = {
        action_config : Jsont.json option;
        plugin_action_id : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~plugin_action_id ?action_config () = { action_config; plugin_action_id }
    
    let action_config t = t.action_config
    let plugin_action_id t = t.plugin_action_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowActionItemDto"
        (fun action_config plugin_action_id -> { action_config; plugin_action_id })
      |> Jsont.Object.opt_mem "actionConfig" Jsont.json ~enc:(fun r -> r.action_config)
      |> Jsont.Object.mem "pluginActionId" Jsont.string ~enc:(fun r -> r.plugin_action_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module WorkflowAction = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        action_config : Jsont.json option;
        id : string;
        order : float;
        plugin_action_id : string;
        workflow_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~id ~order ~plugin_action_id ~workflow_id ?action_config () = { action_config; id; order; plugin_action_id; workflow_id }
    
    let action_config t = t.action_config
    let id t = t.id
    let order t = t.order
    let plugin_action_id t = t.plugin_action_id
    let workflow_id t = t.workflow_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowActionResponseDto"
        (fun action_config id order plugin_action_id workflow_id -> { action_config; id; order; plugin_action_id; workflow_id })
      |> Jsont.Object.mem "actionConfig" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.action_config)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "order" Jsont.number ~enc:(fun r -> r.order)
      |> Jsont.Object.mem "pluginActionId" Jsont.string ~enc:(fun r -> r.plugin_action_id)
      |> Jsont.Object.mem "workflowId" Jsont.string ~enc:(fun r -> r.workflow_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoContainer = struct
  module Types = struct
    module T = struct
      type t = [
        | `Mov
        | `Mp4
        | `Ogg
        | `Webm
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"VideoContainer"
        ~dec:(function
          | "mov" -> `Mov
          | "mp4" -> `Mp4
          | "ogg" -> `Ogg
          | "webm" -> `Webm
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Mov -> "mov"
          | `Mp4 -> "mp4"
          | `Ogg -> "ogg"
          | `Webm -> "webm")
  end
end

module VideoCodec = struct
  module Types = struct
    module T = struct
      type t = [
        | `H264
        | `Hevc
        | `Vp9
        | `Av1
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"VideoCodec"
        ~dec:(function
          | "h264" -> `H264
          | "hevc" -> `Hevc
          | "vp9" -> `Vp9
          | "av1" -> `Av1
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `H264 -> "h264"
          | `Hevc -> "hevc"
          | `Vp9 -> "vp9"
          | `Av1 -> "av1")
  end
end

module VersionCheckState = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        checked_at : string option;
        release_version : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?checked_at ?release_version () = { checked_at; release_version }
    
    let checked_at t = t.checked_at
    let release_version t = t.release_version
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VersionCheckStateResponseDto"
        (fun checked_at release_version -> { checked_at; release_version })
      |> Jsont.Object.mem "checkedAt" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.checked_at)
      |> Jsont.Object.mem "releaseVersion" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.release_version)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get version check status
  
      Retrieve information about the last time the version check ran. *)
  let get_version_check client () =
    let op_name = "get_version_check" in
    let url_path = "/server/version-check" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve version check state
  
      Retrieve the current state of the version check process. *)
  let get_version_check_state client () =
    let op_name = "get_version_check_state" in
    let url_path = "/system-metadata/version-check-state" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ValidateLibraryImportPath = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        import_path : string;
        is_valid : bool;
        message : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~import_path ?(is_valid=false) ?message () = { import_path; is_valid; message }
    
    let import_path t = t.import_path
    let is_valid t = t.is_valid
    let message t = t.message
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ValidateLibraryImportPathResponseDto"
        (fun import_path is_valid message -> { import_path; is_valid; message })
      |> Jsont.Object.mem "importPath" Jsont.string ~enc:(fun r -> r.import_path)
      |> Jsont.Object.mem "isValid" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.is_valid)
      |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ValidateLibrary = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        import_paths : ValidateLibraryImportPath.ResponseDto.t list option;
      }
    end
  
    module Dto = struct
      type t = {
        exclusion_patterns : string list option;
        import_paths : string list option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?import_paths () = { import_paths }
    
    let import_paths t = t.import_paths
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ValidateLibraryResponseDto"
        (fun import_paths -> { import_paths })
      |> Jsont.Object.opt_mem "importPaths" (Jsont.list ValidateLibraryImportPath.ResponseDto.jsont) ~enc:(fun r -> r.import_paths)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?exclusion_patterns ?import_paths () = { exclusion_patterns; import_paths }
    
    let exclusion_patterns t = t.exclusion_patterns
    let import_paths t = t.import_paths
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ValidateLibraryDto"
        (fun exclusion_patterns import_paths -> { exclusion_patterns; import_paths })
      |> Jsont.Object.opt_mem "exclusionPatterns" (Openapi.Runtime.validated_list ~max_items:128 ~unique_items:true Jsont.string) ~enc:(fun r -> r.exclusion_patterns)
      |> Jsont.Object.opt_mem "importPaths" (Openapi.Runtime.validated_list ~max_items:128 ~unique_items:true Jsont.string) ~enc:(fun r -> r.import_paths)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Validate library settings
  
      Validate the settings of an external library. *)
  let validate ~id ~body client () =
    let op_name = "validate" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/libraries/{id}/validate" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ValidateAccessToken = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        auth_status : bool;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~auth_status () = { auth_status }
    
    let auth_status t = t.auth_status
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ValidateAccessTokenResponseDto"
        (fun auth_status -> { auth_status })
      |> Jsont.Object.mem "authStatus" Jsont.bool ~enc:(fun r -> r.auth_status)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Validate access token
  
      Validate the current authorization method is still valid. *)
  let validate_access_token client () =
    let op_name = "validate_access_token" in
    let url_path = "/auth/validateToken" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module UserMetadataKey = struct
  module Types = struct
    module T = struct
      type t = [
        | `Preferences
        | `License
        | `Onboarding
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"UserMetadataKey"
        ~dec:(function
          | "preferences" -> `Preferences
          | "license" -> `License
          | "onboarding" -> `Onboarding
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Preferences -> "preferences"
          | `License -> "license"
          | `Onboarding -> "onboarding")
  end
end

module SyncUserMetadataV1 = struct
  module Types = struct
    module T = struct
      type t = {
        key : UserMetadataKey.T.t;
        user_id : string;
        value : Jsont.json;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~key ~user_id ~value () = { key; user_id; value }
    
    let key t = t.key
    let user_id t = t.user_id
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncUserMetadataV1"
        (fun key user_id value -> { key; user_id; value })
      |> Jsont.Object.mem "key" UserMetadataKey.T.jsont ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncUserMetadataDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        key : UserMetadataKey.T.t;
        user_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~key ~user_id () = { key; user_id }
    
    let key t = t.key
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncUserMetadataDeleteV1"
        (fun key user_id -> { key; user_id })
      |> Jsont.Object.mem "key" UserMetadataKey.T.jsont ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserLicense = struct
  module Types = struct
    module T = struct
      type t = {
        activated_at : Ptime.t;
        activation_key : string;
        license_key : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~activated_at ~activation_key ~license_key () = { activated_at; activation_key; license_key }
    
    let activated_at t = t.activated_at
    let activation_key t = t.activation_key
    let license_key t = t.license_key
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserLicense"
        (fun activated_at activation_key license_key -> { activated_at; activation_key; license_key })
      |> Jsont.Object.mem "activatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.activated_at)
      |> Jsont.Object.mem "activationKey" Jsont.string ~enc:(fun r -> r.activation_key)
      |> Jsont.Object.mem "licenseKey" Jsont.string ~enc:(fun r -> r.license_key)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserAvatarColor = struct
  module Types = struct
    module T = struct
      type t = [
        | `Primary
        | `Pink
        | `Red
        | `Yellow
        | `Blue
        | `Green
        | `Purple
        | `Orange
        | `Gray
        | `Amber
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"UserAvatarColor"
        ~dec:(function
          | "primary" -> `Primary
          | "pink" -> `Pink
          | "red" -> `Red
          | "yellow" -> `Yellow
          | "blue" -> `Blue
          | "green" -> `Green
          | "purple" -> `Purple
          | "orange" -> `Orange
          | "gray" -> `Gray
          | "amber" -> `Amber
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Primary -> "primary"
          | `Pink -> "pink"
          | `Red -> "red"
          | `Yellow -> "yellow"
          | `Blue -> "blue"
          | `Green -> "green"
          | `Purple -> "purple"
          | `Orange -> "orange"
          | `Gray -> "gray"
          | `Amber -> "amber")
  end
end

module UserUpdateMe = struct
  module Types = struct
    module Dto = struct
      type t = {
        avatar_color : UserAvatarColor.T.t option;
        email : string option;
        name : string option;
        password : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?avatar_color ?email ?name ?password () = { avatar_color; email; name; password }
    
    let avatar_color t = t.avatar_color
    let email t = t.email
    let name t = t.name
    let password t = t.password
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserUpdateMeDto"
        (fun avatar_color email name password -> { avatar_color; email; name; password })
      |> Jsont.Object.opt_mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module User = struct
  module Types = struct
    module Status = struct
      type t = [
        | `Active
        | `Removing
        | `Deleted
      ]
    end
  
    module ResponseDto = struct
      type t = {
        avatar_color : UserAvatarColor.T.t;
        email : string;
        id : string;
        name : string;
        profile_changed_at : Ptime.t;
        profile_image_path : string;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"UserStatus"
        ~dec:(function
          | "active" -> `Active
          | "removing" -> `Removing
          | "deleted" -> `Deleted
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Active -> "active"
          | `Removing -> "removing"
          | `Deleted -> "deleted")
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~avatar_color ~email ~id ~name ~profile_changed_at ~profile_image_path () = { avatar_color; email; id; name; profile_changed_at; profile_image_path }
    
    let avatar_color t = t.avatar_color
    let email t = t.email
    let id t = t.id
    let name t = t.name
    let profile_changed_at t = t.profile_changed_at
    let profile_image_path t = t.profile_image_path
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserResponseDto"
        (fun avatar_color email id name profile_changed_at profile_image_path -> { avatar_color; email; id; name; profile_changed_at; profile_image_path })
      |> Jsont.Object.mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "profileChangedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.profile_changed_at)
      |> Jsont.Object.mem "profileImagePath" Jsont.string ~enc:(fun r -> r.profile_image_path)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get all users
  
      Retrieve a list of all users on the server. *)
  let search_users client () =
    let op_name = "search_users" in
    let url_path = "/users" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a user
  
      Retrieve a specific user by their ID. *)
  let get_user ~id client () =
    let op_name = "get_user" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/users/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SyncUserV1 = struct
  module Types = struct
    module T = struct
      type t = {
        avatar_color : UserAvatarColor.T.t;
        deleted_at : Ptime.t option;
        email : string;
        has_profile_image : bool;
        id : string;
        name : string;
        profile_changed_at : Ptime.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~avatar_color ~email ~has_profile_image ~id ~name ~profile_changed_at ?deleted_at () = { avatar_color; deleted_at; email; has_profile_image; id; name; profile_changed_at }
    
    let avatar_color t = t.avatar_color
    let deleted_at t = t.deleted_at
    let email t = t.email
    let has_profile_image t = t.has_profile_image
    let id t = t.id
    let name t = t.name
    let profile_changed_at t = t.profile_changed_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncUserV1"
        (fun avatar_color deleted_at email has_profile_image id name profile_changed_at -> { avatar_color; deleted_at; email; has_profile_image; id; name; profile_changed_at })
      |> Jsont.Object.mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.mem "deletedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "hasProfileImage" Jsont.bool ~enc:(fun r -> r.has_profile_image)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "profileChangedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.profile_changed_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAuthUserV1 = struct
  module Types = struct
    module T = struct
      type t = {
        avatar_color : UserAvatarColor.T.t;
        deleted_at : Ptime.t option;
        email : string;
        has_profile_image : bool;
        id : string;
        is_admin : bool;
        name : string;
        oauth_id : string;
        pin_code : string option;
        profile_changed_at : Ptime.t;
        quota_size_in_bytes : int option;
        quota_usage_in_bytes : int;
        storage_label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~avatar_color ~email ~has_profile_image ~id ~is_admin ~name ~oauth_id ~profile_changed_at ~quota_usage_in_bytes ?deleted_at ?pin_code ?quota_size_in_bytes ?storage_label () = { avatar_color; deleted_at; email; has_profile_image; id; is_admin; name; oauth_id; pin_code; profile_changed_at; quota_size_in_bytes; quota_usage_in_bytes; storage_label }
    
    let avatar_color t = t.avatar_color
    let deleted_at t = t.deleted_at
    let email t = t.email
    let has_profile_image t = t.has_profile_image
    let id t = t.id
    let is_admin t = t.is_admin
    let name t = t.name
    let oauth_id t = t.oauth_id
    let pin_code t = t.pin_code
    let profile_changed_at t = t.profile_changed_at
    let quota_size_in_bytes t = t.quota_size_in_bytes
    let quota_usage_in_bytes t = t.quota_usage_in_bytes
    let storage_label t = t.storage_label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAuthUserV1"
        (fun avatar_color deleted_at email has_profile_image id is_admin name oauth_id pin_code profile_changed_at quota_size_in_bytes quota_usage_in_bytes storage_label -> { avatar_color; deleted_at; email; has_profile_image; id; is_admin; name; oauth_id; pin_code; profile_changed_at; quota_size_in_bytes; quota_usage_in_bytes; storage_label })
      |> Jsont.Object.mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.mem "deletedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "hasProfileImage" Jsont.bool ~enc:(fun r -> r.has_profile_image)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isAdmin" Jsont.bool ~enc:(fun r -> r.is_admin)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "oauthId" Jsont.string ~enc:(fun r -> r.oauth_id)
      |> Jsont.Object.mem "pinCode" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.mem "profileChangedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.profile_changed_at)
      |> Jsont.Object.mem "quotaSizeInBytes" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.quota_size_in_bytes)
      |> Jsont.Object.mem "quotaUsageInBytes" Jsont.int ~enc:(fun r -> r.quota_usage_in_bytes)
      |> Jsont.Object.mem "storageLabel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.storage_label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Partner = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        in_timeline : bool;
      }
    end
  
    module ResponseDto = struct
      type t = {
        avatar_color : UserAvatarColor.T.t;
        email : string;
        id : string;
        in_timeline : bool option;
        name : string;
        profile_changed_at : Ptime.t;
        profile_image_path : string;
      }
    end
  
    module CreateDto = struct
      type t = {
        shared_with_id : string;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ~in_timeline () = { in_timeline }
    
    let in_timeline t = t.in_timeline
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PartnerUpdateDto"
        (fun in_timeline -> { in_timeline })
      |> Jsont.Object.mem "inTimeline" Jsont.bool ~enc:(fun r -> r.in_timeline)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~avatar_color ~email ~id ~name ~profile_changed_at ~profile_image_path ?in_timeline () = { avatar_color; email; id; in_timeline; name; profile_changed_at; profile_image_path }
    
    let avatar_color t = t.avatar_color
    let email t = t.email
    let id t = t.id
    let in_timeline t = t.in_timeline
    let name t = t.name
    let profile_changed_at t = t.profile_changed_at
    let profile_image_path t = t.profile_image_path
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PartnerResponseDto"
        (fun avatar_color email id in_timeline name profile_changed_at profile_image_path -> { avatar_color; email; id; in_timeline; name; profile_changed_at; profile_image_path })
      |> Jsont.Object.mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "inTimeline" Jsont.bool ~enc:(fun r -> r.in_timeline)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "profileChangedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.profile_changed_at)
      |> Jsont.Object.mem "profileImagePath" Jsont.string ~enc:(fun r -> r.profile_image_path)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~shared_with_id () = { shared_with_id }
    
    let shared_with_id t = t.shared_with_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PartnerCreateDto"
        (fun shared_with_id -> { shared_with_id })
      |> Jsont.Object.mem "sharedWithId" Jsont.string ~enc:(fun r -> r.shared_with_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve partners
  
      Retrieve a list of partners with whom assets are shared. *)
  let get_partners ~direction client () =
    let op_name = "get_partners" in
    let url_path = "/partners" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"direction" ~value:direction]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a partner
  
      Create a new partner to share assets with. *)
  let create_partner ~body client () =
    let op_name = "create_partner" in
    let url_path = "/partners" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a partner
  
      Create a new partner to share assets with. *)
  let create_partner_deprecated ~id client () =
    let op_name = "create_partner_deprecated" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/partners/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a partner
  
      Specify whether a partner's assets should appear in the user's timeline. *)
  let update_partner ~id ~body client () =
    let op_name = "update_partner" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/partners/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Avatar = struct
  module Types = struct
    module Update = struct
      type t = {
        color : UserAvatarColor.T.t option;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?color () = { color }
    
    let color t = t.color
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AvatarUpdate"
        (fun color -> { color })
      |> Jsont.Object.opt_mem "color" UserAvatarColor.T.jsont ~enc:(fun r -> r.color)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserAdminDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        force : bool option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?force () = { force }
    
    let force t = t.force
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserAdminDeleteDto"
        (fun force -> { force })
      |> Jsont.Object.opt_mem "force" Jsont.bool ~enc:(fun r -> r.force)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UsageByUser = struct
  module Types = struct
    module Dto = struct
      type t = {
        photos : int;
        quota_size_in_bytes : int64 option;
        usage : int64;
        usage_photos : int64;
        usage_videos : int64;
        user_id : string;
        user_name : string;
        videos : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~photos ~usage ~usage_photos ~usage_videos ~user_id ~user_name ~videos ?quota_size_in_bytes () = { photos; quota_size_in_bytes; usage; usage_photos; usage_videos; user_id; user_name; videos }
    
    let photos t = t.photos
    let quota_size_in_bytes t = t.quota_size_in_bytes
    let usage t = t.usage
    let usage_photos t = t.usage_photos
    let usage_videos t = t.usage_videos
    let user_id t = t.user_id
    let user_name t = t.user_name
    let videos t = t.videos
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UsageByUserDto"
        (fun photos quota_size_in_bytes usage usage_photos usage_videos user_id user_name videos -> { photos; quota_size_in_bytes; usage; usage_photos; usage_videos; user_id; user_name; videos })
      |> Jsont.Object.mem "photos" Jsont.int ~enc:(fun r -> r.photos)
      |> Jsont.Object.mem "quotaSizeInBytes" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.quota_size_in_bytes)
      |> Jsont.Object.mem "usage" Jsont.int64 ~enc:(fun r -> r.usage)
      |> Jsont.Object.mem "usagePhotos" Jsont.int64 ~enc:(fun r -> r.usage_photos)
      |> Jsont.Object.mem "usageVideos" Jsont.int64 ~enc:(fun r -> r.usage_videos)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.mem "userName" Jsont.string ~enc:(fun r -> r.user_name)
      |> Jsont.Object.mem "videos" Jsont.int ~enc:(fun r -> r.videos)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ServerStats = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        photos : int;
        usage : int64;
        usage_by_user : UsageByUser.Dto.t list;
        usage_photos : int64;
        usage_videos : int64;
        videos : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?(photos=0) ?(usage=0L) ?(usage_by_user=[]) ?(usage_photos=0L) ?(usage_videos=0L) ?(videos=0) () = { photos; usage; usage_by_user; usage_photos; usage_videos; videos }
    
    let photos t = t.photos
    let usage t = t.usage
    let usage_by_user t = t.usage_by_user
    let usage_photos t = t.usage_photos
    let usage_videos t = t.usage_videos
    let videos t = t.videos
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerStatsResponseDto"
        (fun photos usage usage_by_user usage_photos usage_videos videos -> { photos; usage; usage_by_user; usage_photos; usage_videos; videos })
      |> Jsont.Object.mem "photos" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.photos)
      |> Jsont.Object.mem "usage" Jsont.int64 ~dec_absent:0L ~enc:(fun r -> r.usage)
      |> Jsont.Object.mem "usageByUser" (Jsont.list UsageByUser.Dto.jsont) ~dec_absent:[] ~enc:(fun r -> r.usage_by_user)
      |> Jsont.Object.mem "usagePhotos" Jsont.int64 ~dec_absent:0L ~enc:(fun r -> r.usage_photos)
      |> Jsont.Object.mem "usageVideos" Jsont.int64 ~dec_absent:0L ~enc:(fun r -> r.usage_videos)
      |> Jsont.Object.mem "videos" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.videos)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get statistics
  
      Retrieve statistics about the entire Immich instance such as asset counts. *)
  let get_server_statistics client () =
    let op_name = "get_server_statistics" in
    let url_path = "/server/statistics" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module UpdateLibrary = struct
  module Types = struct
    module Dto = struct
      type t = {
        exclusion_patterns : string list option;
        import_paths : string list option;
        name : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?exclusion_patterns ?import_paths ?name () = { exclusion_patterns; import_paths; name }
    
    let exclusion_patterns t = t.exclusion_patterns
    let import_paths t = t.import_paths
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UpdateLibraryDto"
        (fun exclusion_patterns import_paths name -> { exclusion_patterns; import_paths; name })
      |> Jsont.Object.opt_mem "exclusionPatterns" (Openapi.Runtime.validated_list ~max_items:128 ~unique_items:true Jsont.string) ~enc:(fun r -> r.exclusion_patterns)
      |> Jsont.Object.opt_mem "importPaths" (Openapi.Runtime.validated_list ~max_items:128 ~unique_items:true Jsont.string) ~enc:(fun r -> r.import_paths)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module TranscodePolicy = struct
  module Types = struct
    module T = struct
      type t = [
        | `All
        | `Optimal
        | `Bitrate
        | `Required
        | `Disabled
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"TranscodePolicy"
        ~dec:(function
          | "all" -> `All
          | "optimal" -> `Optimal
          | "bitrate" -> `Bitrate
          | "required" -> `Required
          | "disabled" -> `Disabled
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `All -> "all"
          | `Optimal -> "optimal"
          | `Bitrate -> "bitrate"
          | `Required -> "required"
          | `Disabled -> "disabled")
  end
end

module TranscodeHwaccel = struct
  module Types = struct
    module T = struct
      type t = [
        | `Nvenc
        | `Qsv
        | `Vaapi
        | `Rkmpp
        | `Disabled
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"TranscodeHWAccel"
        ~dec:(function
          | "nvenc" -> `Nvenc
          | "qsv" -> `Qsv
          | "vaapi" -> `Vaapi
          | "rkmpp" -> `Rkmpp
          | "disabled" -> `Disabled
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Nvenc -> "nvenc"
          | `Qsv -> "qsv"
          | `Vaapi -> "vaapi"
          | `Rkmpp -> "rkmpp"
          | `Disabled -> "disabled")
  end
end

module ToneMapping = struct
  module Types = struct
    module T = struct
      type t = [
        | `Hable
        | `Mobius
        | `Reinhard
        | `Disabled
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"ToneMapping"
        ~dec:(function
          | "hable" -> `Hable
          | "mobius" -> `Mobius
          | "reinhard" -> `Reinhard
          | "disabled" -> `Disabled
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Hable -> "hable"
          | `Mobius -> "mobius"
          | `Reinhard -> "reinhard"
          | `Disabled -> "disabled")
  end
end

module TimeBuckets = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        count : int;  (** Number of assets in this time bucket *)
        time_bucket : string;  (** Time bucket identifier in YYYY-MM-DD format representing the start of the time period *)
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~count ~time_bucket () = { count; time_bucket }
    
    let count t = t.count
    let time_bucket t = t.time_bucket
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TimeBucketsResponseDto"
        (fun count time_bucket -> { count; time_bucket })
      |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
      |> Jsont.Object.mem "timeBucket" Jsont.string ~enc:(fun r -> r.time_bucket)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get time buckets
  
      Retrieve a list of all minimal time buckets. 
      @param album_id Filter assets belonging to a specific album
      @param is_favorite Filter by favorite status (true for favorites only, false for non-favorites only)
      @param is_trashed Filter by trash status (true for trashed assets only, false for non-trashed only)
      @param order Sort order for assets within time buckets (ASC for oldest first, DESC for newest first)
      @param person_id Filter assets containing a specific person (face recognition)
      @param tag_id Filter assets with a specific tag
      @param user_id Filter assets by specific user ID
      @param visibility Filter by asset visibility status (ARCHIVE, TIMELINE, HIDDEN, LOCKED)
      @param with_coordinates Include location data in the response
      @param with_partners Include assets shared by partners
      @param with_stacked Include stacked assets in the response. When true, only primary assets from stacks are returned.
  *)
  let get_time_buckets ?album_id ?is_favorite ?is_trashed ?key ?order ?person_id ?slug ?tag_id ?user_id ?visibility ?with_coordinates ?with_partners ?with_stacked client () =
    let op_name = "get_time_buckets" in
    let url_path = "/timeline/buckets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"albumId" ~value:album_id; Openapi.Runtime.Query.optional ~key:"isFavorite" ~value:is_favorite; Openapi.Runtime.Query.optional ~key:"isTrashed" ~value:is_trashed; Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"order" ~value:order; Openapi.Runtime.Query.optional ~key:"personId" ~value:person_id; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug; Openapi.Runtime.Query.optional ~key:"tagId" ~value:tag_id; Openapi.Runtime.Query.optional ~key:"userId" ~value:user_id; Openapi.Runtime.Query.optional ~key:"visibility" ~value:visibility; Openapi.Runtime.Query.optional ~key:"withCoordinates" ~value:with_coordinates; Openapi.Runtime.Query.optional ~key:"withPartners" ~value:with_partners; Openapi.Runtime.Query.optional ~key:"withStacked" ~value:with_stacked]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Template = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        html : string;
        name : string;
      }
    end
  
    module Dto = struct
      type t = {
        template : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~html ~name () = { html; name }
    
    let html t = t.html
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TemplateResponseDto"
        (fun html name -> { html; name })
      |> Jsont.Object.mem "html" Jsont.string ~enc:(fun r -> r.html)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~template () = { template }
    
    let template t = t.template
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TemplateDto"
        (fun template -> { template })
      |> Jsont.Object.mem "template" Jsont.string ~enc:(fun r -> r.template)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Render email template
  
      Retrieve a preview of the provided email template. *)
  let get_notification_template_admin ~name ~body client () =
    let op_name = "get_notification_template_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/admin/notifications/templates/{name}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Tags = struct
  module Types = struct
    module Update = struct
      type t = {
        enabled : bool option;
        sidebar_web : bool option;
      }
    end
  
    module Response = struct
      type t = {
        enabled : bool;
        sidebar_web : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?enabled ?sidebar_web () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagsUpdate"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.opt_mem "sidebarWeb" Jsont.bool ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(enabled=true) ?(sidebar_web=true) () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagsResponse"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.mem "enabled" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "sidebarWeb" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module TagUpsert = struct
  module Types = struct
    module Dto = struct
      type t = {
        tags : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~tags () = { tags }
    
    let tags t = t.tags
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagUpsertDto"
        (fun tags -> { tags })
      |> Jsont.Object.mem "tags" (Jsont.list Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Tag = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        color : string option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        color : string option;
        created_at : Ptime.t;
        id : string;
        name : string;
        parent_id : string option;
        updated_at : Ptime.t;
        value : string;
      }
    end
  
    module CreateDto = struct
      type t = {
        color : string option;
        name : string;
        parent_id : string option;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?color () = { color }
    
    let color t = t.color
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagUpdateDto"
        (fun color -> { color })
      |> Jsont.Object.mem "color" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.color)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~created_at ~id ~name ~updated_at ~value ?color ?parent_id () = { color; created_at; id; name; parent_id; updated_at; value }
    
    let color t = t.color
    let created_at t = t.created_at
    let id t = t.id
    let name t = t.name
    let parent_id t = t.parent_id
    let updated_at t = t.updated_at
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagResponseDto"
        (fun color created_at id name parent_id updated_at value -> { color; created_at; id; name; parent_id; updated_at; value })
      |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "parentId" Jsont.string ~enc:(fun r -> r.parent_id)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~name ?color ?parent_id () = { color; name; parent_id }
    
    let color t = t.color
    let name t = t.name
    let parent_id t = t.parent_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagCreateDto"
        (fun color name parent_id -> { color; name; parent_id })
      |> Jsont.Object.opt_mem "color" (Openapi.Runtime.validated_string ~pattern:"^#?([0-9A-F]{3}|[0-9A-F]{4}|[0-9A-F]{6}|[0-9A-F]{8})$" Jsont.string) ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "parentId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.parent_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve tags
  
      Retrieve a list of all tags. *)
  let get_all_tags client () =
    let op_name = "get_all_tags" in
    let url_path = "/tags" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a tag
  
      Create a new tag by providing a name and optional color. *)
  let create_tag ~body client () =
    let op_name = "create_tag" in
    let url_path = "/tags" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Upsert tags
  
      Create or update multiple tags in a single request. *)
  let upsert_tags ~body client () =
    let op_name = "upsert_tags" in
    let url_path = "/tags" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json TagUpsert.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a tag
  
      Retrieve a specific tag by its ID. *)
  let get_tag_by_id ~id client () =
    let op_name = "get_tag_by_id" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/tags/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a tag
  
      Update an existing tag identified by its ID. *)
  let update_tag ~id ~body client () =
    let op_name = "update_tag" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/tags/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module TagBulkAssets = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        count : int;
      }
    end
  
    module Dto = struct
      type t = {
        asset_ids : string list;
        tag_ids : string list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~count () = { count }
    
    let count t = t.count
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagBulkAssetsResponseDto"
        (fun count -> { count })
      |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_ids ~tag_ids () = { asset_ids; tag_ids }
    
    let asset_ids t = t.asset_ids
    let tag_ids t = t.tag_ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TagBulkAssetsDto"
        (fun asset_ids tag_ids -> { asset_ids; tag_ids })
      |> Jsont.Object.mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.mem "tagIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.tag_ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Tag assets
  
      Add multiple tags to multiple assets in a single request. *)
  let bulk_tag_assets ~body client () =
    let op_name = "bulk_tag_assets" in
    let url_path = "/tags/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SystemConfigUser = struct
  module Types = struct
    module Dto = struct
      type t = {
        delete_delay : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~delete_delay () = { delete_delay }
    
    let delete_delay t = t.delete_delay
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigUserDto"
        (fun delete_delay -> { delete_delay })
      |> Jsont.Object.mem "deleteDelay" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.delete_delay)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigTrash = struct
  module Types = struct
    module Dto = struct
      type t = {
        days : int;
        enabled : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~days ~enabled () = { days; enabled }
    
    let days t = t.days
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigTrashDto"
        (fun days enabled -> { days; enabled })
      |> Jsont.Object.mem "days" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.days)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigTheme = struct
  module Types = struct
    module Dto = struct
      type t = {
        custom_css : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~custom_css () = { custom_css }
    
    let custom_css t = t.custom_css
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigThemeDto"
        (fun custom_css -> { custom_css })
      |> Jsont.Object.mem "customCss" Jsont.string ~enc:(fun r -> r.custom_css)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigTemplateStorageOption = struct
  module Types = struct
    module Dto = struct
      type t = {
        day_options : string list;
        hour_options : string list;
        minute_options : string list;
        month_options : string list;
        preset_options : string list;
        second_options : string list;
        week_options : string list;
        year_options : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~day_options ~hour_options ~minute_options ~month_options ~preset_options ~second_options ~week_options ~year_options () = { day_options; hour_options; minute_options; month_options; preset_options; second_options; week_options; year_options }
    
    let day_options t = t.day_options
    let hour_options t = t.hour_options
    let minute_options t = t.minute_options
    let month_options t = t.month_options
    let preset_options t = t.preset_options
    let second_options t = t.second_options
    let week_options t = t.week_options
    let year_options t = t.year_options
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigTemplateStorageOptionDto"
        (fun day_options hour_options minute_options month_options preset_options second_options week_options year_options -> { day_options; hour_options; minute_options; month_options; preset_options; second_options; week_options; year_options })
      |> Jsont.Object.mem "dayOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.day_options)
      |> Jsont.Object.mem "hourOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.hour_options)
      |> Jsont.Object.mem "minuteOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.minute_options)
      |> Jsont.Object.mem "monthOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.month_options)
      |> Jsont.Object.mem "presetOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.preset_options)
      |> Jsont.Object.mem "secondOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.second_options)
      |> Jsont.Object.mem "weekOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.week_options)
      |> Jsont.Object.mem "yearOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.year_options)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get storage template options
  
      Retrieve exemplary storage template options. *)
  let get_storage_template_options client () =
    let op_name = "get_storage_template_options" in
    let url_path = "/system-config/storage-template-options" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SystemConfigTemplateEmails = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_invite_template : string;
        album_update_template : string;
        welcome_template : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~album_invite_template ~album_update_template ~welcome_template () = { album_invite_template; album_update_template; welcome_template }
    
    let album_invite_template t = t.album_invite_template
    let album_update_template t = t.album_update_template
    let welcome_template t = t.welcome_template
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigTemplateEmailsDto"
        (fun album_invite_template album_update_template welcome_template -> { album_invite_template; album_update_template; welcome_template })
      |> Jsont.Object.mem "albumInviteTemplate" Jsont.string ~enc:(fun r -> r.album_invite_template)
      |> Jsont.Object.mem "albumUpdateTemplate" Jsont.string ~enc:(fun r -> r.album_update_template)
      |> Jsont.Object.mem "welcomeTemplate" Jsont.string ~enc:(fun r -> r.welcome_template)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigTemplates = struct
  module Types = struct
    module Dto = struct
      type t = {
        email : SystemConfigTemplateEmails.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~email () = { email }
    
    let email t = t.email
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigTemplatesDto"
        (fun email -> { email })
      |> Jsont.Object.mem "email" SystemConfigTemplateEmails.Dto.jsont ~enc:(fun r -> r.email)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigStorageTemplate = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
        hash_verification_enabled : bool;
        template : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled ~hash_verification_enabled ~template () = { enabled; hash_verification_enabled; template }
    
    let enabled t = t.enabled
    let hash_verification_enabled t = t.hash_verification_enabled
    let template t = t.template
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigStorageTemplateDto"
        (fun enabled hash_verification_enabled template -> { enabled; hash_verification_enabled; template })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "hashVerificationEnabled" Jsont.bool ~enc:(fun r -> r.hash_verification_enabled)
      |> Jsont.Object.mem "template" Jsont.string ~enc:(fun r -> r.template)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigSmtpTransport = struct
  module Types = struct
    module Dto = struct
      type t = {
        host : string;
        ignore_cert : bool;
        password : string;
        port : float;
        secure : bool;
        username : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~host ~ignore_cert ~password ~port ~secure ~username () = { host; ignore_cert; password; port; secure; username }
    
    let host t = t.host
    let ignore_cert t = t.ignore_cert
    let password t = t.password
    let port t = t.port
    let secure t = t.secure
    let username t = t.username
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigSmtpTransportDto"
        (fun host ignore_cert password port secure username -> { host; ignore_cert; password; port; secure; username })
      |> Jsont.Object.mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.mem "ignoreCert" Jsont.bool ~enc:(fun r -> r.ignore_cert)
      |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "port" (Openapi.Runtime.validated_float ~minimum:0. ~maximum:65535. Jsont.number) ~enc:(fun r -> r.port)
      |> Jsont.Object.mem "secure" Jsont.bool ~enc:(fun r -> r.secure)
      |> Jsont.Object.mem "username" Jsont.string ~enc:(fun r -> r.username)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigSmtp = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
        from : string;
        reply_to : string;
        transport : SystemConfigSmtpTransport.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled ~from ~reply_to ~transport () = { enabled; from; reply_to; transport }
    
    let enabled t = t.enabled
    let from t = t.from
    let reply_to t = t.reply_to
    let transport t = t.transport
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigSmtpDto"
        (fun enabled from reply_to transport -> { enabled; from; reply_to; transport })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "from" Jsont.string ~enc:(fun r -> r.from)
      |> Jsont.Object.mem "replyTo" Jsont.string ~enc:(fun r -> r.reply_to)
      |> Jsont.Object.mem "transport" SystemConfigSmtpTransport.Dto.jsont ~enc:(fun r -> r.transport)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module TestEmail = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        message_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~message_id () = { message_id }
    
    let message_id t = t.message_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TestEmailResponseDto"
        (fun message_id -> { message_id })
      |> Jsont.Object.mem "messageId" Jsont.string ~enc:(fun r -> r.message_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Send test email
  
      Send a test email using the provided SMTP configuration. *)
  let send_test_email_admin ~body client () =
    let op_name = "send_test_email_admin" in
    let url_path = "/admin/notifications/test-email" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SystemConfigSmtp.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SystemConfigNotifications = struct
  module Types = struct
    module Dto = struct
      type t = {
        smtp : SystemConfigSmtp.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~smtp () = { smtp }
    
    let smtp t = t.smtp
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigNotificationsDto"
        (fun smtp -> { smtp })
      |> Jsont.Object.mem "smtp" SystemConfigSmtp.Dto.jsont ~enc:(fun r -> r.smtp)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigServer = struct
  module Types = struct
    module Dto = struct
      type t = {
        external_domain : string;
        login_page_message : string;
        public_users : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~external_domain ~login_page_message ~public_users () = { external_domain; login_page_message; public_users }
    
    let external_domain t = t.external_domain
    let login_page_message t = t.login_page_message
    let public_users t = t.public_users
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigServerDto"
        (fun external_domain login_page_message public_users -> { external_domain; login_page_message; public_users })
      |> Jsont.Object.mem "externalDomain" Jsont.string ~enc:(fun r -> r.external_domain)
      |> Jsont.Object.mem "loginPageMessage" Jsont.string ~enc:(fun r -> r.login_page_message)
      |> Jsont.Object.mem "publicUsers" Jsont.bool ~enc:(fun r -> r.public_users)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigReverseGeocoding = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled () = { enabled }
    
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigReverseGeocodingDto"
        (fun enabled -> { enabled })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigPasswordLogin = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled () = { enabled }
    
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigPasswordLoginDto"
        (fun enabled -> { enabled })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigNightlyTasks = struct
  module Types = struct
    module Dto = struct
      type t = {
        cluster_new_faces : bool;
        database_cleanup : bool;
        generate_memories : bool;
        missing_thumbnails : bool;
        start_time : string;
        sync_quota_usage : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~cluster_new_faces ~database_cleanup ~generate_memories ~missing_thumbnails ~start_time ~sync_quota_usage () = { cluster_new_faces; database_cleanup; generate_memories; missing_thumbnails; start_time; sync_quota_usage }
    
    let cluster_new_faces t = t.cluster_new_faces
    let database_cleanup t = t.database_cleanup
    let generate_memories t = t.generate_memories
    let missing_thumbnails t = t.missing_thumbnails
    let start_time t = t.start_time
    let sync_quota_usage t = t.sync_quota_usage
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigNightlyTasksDto"
        (fun cluster_new_faces database_cleanup generate_memories missing_thumbnails start_time sync_quota_usage -> { cluster_new_faces; database_cleanup; generate_memories; missing_thumbnails; start_time; sync_quota_usage })
      |> Jsont.Object.mem "clusterNewFaces" Jsont.bool ~enc:(fun r -> r.cluster_new_faces)
      |> Jsont.Object.mem "databaseCleanup" Jsont.bool ~enc:(fun r -> r.database_cleanup)
      |> Jsont.Object.mem "generateMemories" Jsont.bool ~enc:(fun r -> r.generate_memories)
      |> Jsont.Object.mem "missingThumbnails" Jsont.bool ~enc:(fun r -> r.missing_thumbnails)
      |> Jsont.Object.mem "startTime" Jsont.string ~enc:(fun r -> r.start_time)
      |> Jsont.Object.mem "syncQuotaUsage" Jsont.bool ~enc:(fun r -> r.sync_quota_usage)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigNewVersionCheck = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled () = { enabled }
    
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigNewVersionCheckDto"
        (fun enabled -> { enabled })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigMap = struct
  module Types = struct
    module Dto = struct
      type t = {
        dark_style : string;
        enabled : bool;
        light_style : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~dark_style ~enabled ~light_style () = { dark_style; enabled; light_style }
    
    let dark_style t = t.dark_style
    let enabled t = t.enabled
    let light_style t = t.light_style
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigMapDto"
        (fun dark_style enabled light_style -> { dark_style; enabled; light_style })
      |> Jsont.Object.mem "darkStyle" Jsont.string ~enc:(fun r -> r.dark_style)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "lightStyle" Jsont.string ~enc:(fun r -> r.light_style)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigLibraryWatch = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled () = { enabled }
    
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigLibraryWatchDto"
        (fun enabled -> { enabled })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigLibraryScan = struct
  module Types = struct
    module Dto = struct
      type t = {
        cron_expression : string;
        enabled : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~cron_expression ~enabled () = { cron_expression; enabled }
    
    let cron_expression t = t.cron_expression
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigLibraryScanDto"
        (fun cron_expression enabled -> { cron_expression; enabled })
      |> Jsont.Object.mem "cronExpression" Jsont.string ~enc:(fun r -> r.cron_expression)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigLibrary = struct
  module Types = struct
    module Dto = struct
      type t = {
        scan : SystemConfigLibraryScan.Dto.t;
        watch : SystemConfigLibraryWatch.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~scan ~watch () = { scan; watch }
    
    let scan t = t.scan
    let watch t = t.watch
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigLibraryDto"
        (fun scan watch -> { scan; watch })
      |> Jsont.Object.mem "scan" SystemConfigLibraryScan.Dto.jsont ~enc:(fun r -> r.scan)
      |> Jsont.Object.mem "watch" SystemConfigLibraryWatch.Dto.jsont ~enc:(fun r -> r.watch)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigFaces = struct
  module Types = struct
    module Dto = struct
      type t = {
        import : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~import () = { import }
    
    let import t = t.import
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigFacesDto"
        (fun import -> { import })
      |> Jsont.Object.mem "import" Jsont.bool ~enc:(fun r -> r.import)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigMetadata = struct
  module Types = struct
    module Dto = struct
      type t = {
        faces : SystemConfigFaces.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~faces () = { faces }
    
    let faces t = t.faces
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigMetadataDto"
        (fun faces -> { faces })
      |> Jsont.Object.mem "faces" SystemConfigFaces.Dto.jsont ~enc:(fun r -> r.faces)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncUserDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        user_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~user_id () = { user_id }
    
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncUserDeleteV1"
        (fun user_id -> { user_id })
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncStackV1 = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t;
        id : string;
        owner_id : string;
        primary_asset_id : string;
        updated_at : Ptime.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~created_at ~id ~owner_id ~primary_asset_id ~updated_at () = { created_at; id; owner_id; primary_asset_id; updated_at }
    
    let created_at t = t.created_at
    let id t = t.id
    let owner_id t = t.owner_id
    let primary_asset_id t = t.primary_asset_id
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncStackV1"
        (fun created_at id owner_id primary_asset_id updated_at -> { created_at; id; owner_id; primary_asset_id; updated_at })
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "primaryAssetId" Jsont.string ~enc:(fun r -> r.primary_asset_id)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncStackDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        stack_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~stack_id () = { stack_id }
    
    let stack_id t = t.stack_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncStackDeleteV1"
        (fun stack_id -> { stack_id })
      |> Jsont.Object.mem "stackId" Jsont.string ~enc:(fun r -> r.stack_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncResetV1 = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module SyncRequest = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Albums_v1
        | `Album_users_v1
        | `Album_to_assets_v1
        | `Album_assets_v1
        | `Album_asset_exifs_v1
        | `Assets_v1
        | `Asset_exifs_v1
        | `Asset_metadata_v1
        | `Auth_users_v1
        | `Memories_v1
        | `Memory_to_assets_v1
        | `Partners_v1
        | `Partner_assets_v1
        | `Partner_asset_exifs_v1
        | `Partner_stacks_v1
        | `Stacks_v1
        | `Users_v1
        | `People_v1
        | `Asset_faces_v1
        | `User_metadata_v1
      ]
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"SyncRequestType"
        ~dec:(function
          | "AlbumsV1" -> `Albums_v1
          | "AlbumUsersV1" -> `Album_users_v1
          | "AlbumToAssetsV1" -> `Album_to_assets_v1
          | "AlbumAssetsV1" -> `Album_assets_v1
          | "AlbumAssetExifsV1" -> `Album_asset_exifs_v1
          | "AssetsV1" -> `Assets_v1
          | "AssetExifsV1" -> `Asset_exifs_v1
          | "AssetMetadataV1" -> `Asset_metadata_v1
          | "AuthUsersV1" -> `Auth_users_v1
          | "MemoriesV1" -> `Memories_v1
          | "MemoryToAssetsV1" -> `Memory_to_assets_v1
          | "PartnersV1" -> `Partners_v1
          | "PartnerAssetsV1" -> `Partner_assets_v1
          | "PartnerAssetExifsV1" -> `Partner_asset_exifs_v1
          | "PartnerStacksV1" -> `Partner_stacks_v1
          | "StacksV1" -> `Stacks_v1
          | "UsersV1" -> `Users_v1
          | "PeopleV1" -> `People_v1
          | "AssetFacesV1" -> `Asset_faces_v1
          | "UserMetadataV1" -> `User_metadata_v1
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Albums_v1 -> "AlbumsV1"
          | `Album_users_v1 -> "AlbumUsersV1"
          | `Album_to_assets_v1 -> "AlbumToAssetsV1"
          | `Album_assets_v1 -> "AlbumAssetsV1"
          | `Album_asset_exifs_v1 -> "AlbumAssetExifsV1"
          | `Assets_v1 -> "AssetsV1"
          | `Asset_exifs_v1 -> "AssetExifsV1"
          | `Asset_metadata_v1 -> "AssetMetadataV1"
          | `Auth_users_v1 -> "AuthUsersV1"
          | `Memories_v1 -> "MemoriesV1"
          | `Memory_to_assets_v1 -> "MemoryToAssetsV1"
          | `Partners_v1 -> "PartnersV1"
          | `Partner_assets_v1 -> "PartnerAssetsV1"
          | `Partner_asset_exifs_v1 -> "PartnerAssetExifsV1"
          | `Partner_stacks_v1 -> "PartnerStacksV1"
          | `Stacks_v1 -> "StacksV1"
          | `Users_v1 -> "UsersV1"
          | `People_v1 -> "PeopleV1"
          | `Asset_faces_v1 -> "AssetFacesV1"
          | `User_metadata_v1 -> "UserMetadataV1")
  end
end

module SyncStream = struct
  module Types = struct
    module Dto = struct
      type t = {
        reset : bool option;
        types : SyncRequest.Type.t list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~types ?reset () = { reset; types }
    
    let reset t = t.reset
    let types t = t.types
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncStreamDto"
        (fun reset types -> { reset; types })
      |> Jsont.Object.opt_mem "reset" Jsont.bool ~enc:(fun r -> r.reset)
      |> Jsont.Object.mem "types" (Jsont.list SyncRequest.Type.jsont) ~enc:(fun r -> r.types)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncPersonV1 = struct
  module Types = struct
    module T = struct
      type t = {
        birth_date : Ptime.t option;
        color : string option;
        created_at : Ptime.t;
        face_asset_id : string option;
        id : string;
        is_favorite : bool;
        is_hidden : bool;
        name : string;
        owner_id : string;
        updated_at : Ptime.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~created_at ~id ~is_favorite ~is_hidden ~name ~owner_id ~updated_at ?birth_date ?color ?face_asset_id () = { birth_date; color; created_at; face_asset_id; id; is_favorite; is_hidden; name; owner_id; updated_at }
    
    let birth_date t = t.birth_date
    let color t = t.color
    let created_at t = t.created_at
    let face_asset_id t = t.face_asset_id
    let id t = t.id
    let is_favorite t = t.is_favorite
    let is_hidden t = t.is_hidden
    let name t = t.name
    let owner_id t = t.owner_id
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncPersonV1"
        (fun birth_date color created_at face_asset_id id is_favorite is_hidden name owner_id updated_at -> { birth_date; color; created_at; face_asset_id; id; is_favorite; is_hidden; name; owner_id; updated_at })
      |> Jsont.Object.mem "birthDate" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.birth_date)
      |> Jsont.Object.mem "color" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "faceAssetId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.face_asset_id)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.mem "isHidden" Jsont.bool ~enc:(fun r -> r.is_hidden)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncPersonDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        person_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~person_id () = { person_id }
    
    let person_id t = t.person_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncPersonDeleteV1"
        (fun person_id -> { person_id })
      |> Jsont.Object.mem "personId" Jsont.string ~enc:(fun r -> r.person_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncPartnerV1 = struct
  module Types = struct
    module T = struct
      type t = {
        in_timeline : bool;
        shared_by_id : string;
        shared_with_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~in_timeline ~shared_by_id ~shared_with_id () = { in_timeline; shared_by_id; shared_with_id }
    
    let in_timeline t = t.in_timeline
    let shared_by_id t = t.shared_by_id
    let shared_with_id t = t.shared_with_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncPartnerV1"
        (fun in_timeline shared_by_id shared_with_id -> { in_timeline; shared_by_id; shared_with_id })
      |> Jsont.Object.mem "inTimeline" Jsont.bool ~enc:(fun r -> r.in_timeline)
      |> Jsont.Object.mem "sharedById" Jsont.string ~enc:(fun r -> r.shared_by_id)
      |> Jsont.Object.mem "sharedWithId" Jsont.string ~enc:(fun r -> r.shared_with_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncPartnerDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        shared_by_id : string;
        shared_with_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~shared_by_id ~shared_with_id () = { shared_by_id; shared_with_id }
    
    let shared_by_id t = t.shared_by_id
    let shared_with_id t = t.shared_with_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncPartnerDeleteV1"
        (fun shared_by_id shared_with_id -> { shared_by_id; shared_with_id })
      |> Jsont.Object.mem "sharedById" Jsont.string ~enc:(fun r -> r.shared_by_id)
      |> Jsont.Object.mem "sharedWithId" Jsont.string ~enc:(fun r -> r.shared_with_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncMemoryDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        memory_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~memory_id () = { memory_id }
    
    let memory_id t = t.memory_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncMemoryDeleteV1"
        (fun memory_id -> { memory_id })
      |> Jsont.Object.mem "memoryId" Jsont.string ~enc:(fun r -> r.memory_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncMemoryAssetV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        memory_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ~memory_id () = { asset_id; memory_id }
    
    let asset_id t = t.asset_id
    let memory_id t = t.memory_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncMemoryAssetV1"
        (fun asset_id memory_id -> { asset_id; memory_id })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "memoryId" Jsont.string ~enc:(fun r -> r.memory_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncMemoryAssetDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        memory_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ~memory_id () = { asset_id; memory_id }
    
    let asset_id t = t.asset_id
    let memory_id t = t.memory_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncMemoryAssetDeleteV1"
        (fun asset_id memory_id -> { asset_id; memory_id })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "memoryId" Jsont.string ~enc:(fun r -> r.memory_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncEntity = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Auth_user_v1
        | `User_v1
        | `User_delete_v1
        | `Asset_v1
        | `Asset_delete_v1
        | `Asset_exif_v1
        | `Asset_metadata_v1
        | `Asset_metadata_delete_v1
        | `Partner_v1
        | `Partner_delete_v1
        | `Partner_asset_v1
        | `Partner_asset_backfill_v1
        | `Partner_asset_delete_v1
        | `Partner_asset_exif_v1
        | `Partner_asset_exif_backfill_v1
        | `Partner_stack_backfill_v1
        | `Partner_stack_delete_v1
        | `Partner_stack_v1
        | `Album_v1
        | `Album_delete_v1
        | `Album_user_v1
        | `Album_user_backfill_v1
        | `Album_user_delete_v1
        | `Album_asset_create_v1
        | `Album_asset_update_v1
        | `Album_asset_backfill_v1
        | `Album_asset_exif_create_v1
        | `Album_asset_exif_update_v1
        | `Album_asset_exif_backfill_v1
        | `Album_to_asset_v1
        | `Album_to_asset_delete_v1
        | `Album_to_asset_backfill_v1
        | `Memory_v1
        | `Memory_delete_v1
        | `Memory_to_asset_v1
        | `Memory_to_asset_delete_v1
        | `Stack_v1
        | `Stack_delete_v1
        | `Person_v1
        | `Person_delete_v1
        | `Asset_face_v1
        | `Asset_face_delete_v1
        | `User_metadata_v1
        | `User_metadata_delete_v1
        | `Sync_ack_v1
        | `Sync_reset_v1
        | `Sync_complete_v1
      ]
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"SyncEntityType"
        ~dec:(function
          | "AuthUserV1" -> `Auth_user_v1
          | "UserV1" -> `User_v1
          | "UserDeleteV1" -> `User_delete_v1
          | "AssetV1" -> `Asset_v1
          | "AssetDeleteV1" -> `Asset_delete_v1
          | "AssetExifV1" -> `Asset_exif_v1
          | "AssetMetadataV1" -> `Asset_metadata_v1
          | "AssetMetadataDeleteV1" -> `Asset_metadata_delete_v1
          | "PartnerV1" -> `Partner_v1
          | "PartnerDeleteV1" -> `Partner_delete_v1
          | "PartnerAssetV1" -> `Partner_asset_v1
          | "PartnerAssetBackfillV1" -> `Partner_asset_backfill_v1
          | "PartnerAssetDeleteV1" -> `Partner_asset_delete_v1
          | "PartnerAssetExifV1" -> `Partner_asset_exif_v1
          | "PartnerAssetExifBackfillV1" -> `Partner_asset_exif_backfill_v1
          | "PartnerStackBackfillV1" -> `Partner_stack_backfill_v1
          | "PartnerStackDeleteV1" -> `Partner_stack_delete_v1
          | "PartnerStackV1" -> `Partner_stack_v1
          | "AlbumV1" -> `Album_v1
          | "AlbumDeleteV1" -> `Album_delete_v1
          | "AlbumUserV1" -> `Album_user_v1
          | "AlbumUserBackfillV1" -> `Album_user_backfill_v1
          | "AlbumUserDeleteV1" -> `Album_user_delete_v1
          | "AlbumAssetCreateV1" -> `Album_asset_create_v1
          | "AlbumAssetUpdateV1" -> `Album_asset_update_v1
          | "AlbumAssetBackfillV1" -> `Album_asset_backfill_v1
          | "AlbumAssetExifCreateV1" -> `Album_asset_exif_create_v1
          | "AlbumAssetExifUpdateV1" -> `Album_asset_exif_update_v1
          | "AlbumAssetExifBackfillV1" -> `Album_asset_exif_backfill_v1
          | "AlbumToAssetV1" -> `Album_to_asset_v1
          | "AlbumToAssetDeleteV1" -> `Album_to_asset_delete_v1
          | "AlbumToAssetBackfillV1" -> `Album_to_asset_backfill_v1
          | "MemoryV1" -> `Memory_v1
          | "MemoryDeleteV1" -> `Memory_delete_v1
          | "MemoryToAssetV1" -> `Memory_to_asset_v1
          | "MemoryToAssetDeleteV1" -> `Memory_to_asset_delete_v1
          | "StackV1" -> `Stack_v1
          | "StackDeleteV1" -> `Stack_delete_v1
          | "PersonV1" -> `Person_v1
          | "PersonDeleteV1" -> `Person_delete_v1
          | "AssetFaceV1" -> `Asset_face_v1
          | "AssetFaceDeleteV1" -> `Asset_face_delete_v1
          | "UserMetadataV1" -> `User_metadata_v1
          | "UserMetadataDeleteV1" -> `User_metadata_delete_v1
          | "SyncAckV1" -> `Sync_ack_v1
          | "SyncResetV1" -> `Sync_reset_v1
          | "SyncCompleteV1" -> `Sync_complete_v1
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Auth_user_v1 -> "AuthUserV1"
          | `User_v1 -> "UserV1"
          | `User_delete_v1 -> "UserDeleteV1"
          | `Asset_v1 -> "AssetV1"
          | `Asset_delete_v1 -> "AssetDeleteV1"
          | `Asset_exif_v1 -> "AssetExifV1"
          | `Asset_metadata_v1 -> "AssetMetadataV1"
          | `Asset_metadata_delete_v1 -> "AssetMetadataDeleteV1"
          | `Partner_v1 -> "PartnerV1"
          | `Partner_delete_v1 -> "PartnerDeleteV1"
          | `Partner_asset_v1 -> "PartnerAssetV1"
          | `Partner_asset_backfill_v1 -> "PartnerAssetBackfillV1"
          | `Partner_asset_delete_v1 -> "PartnerAssetDeleteV1"
          | `Partner_asset_exif_v1 -> "PartnerAssetExifV1"
          | `Partner_asset_exif_backfill_v1 -> "PartnerAssetExifBackfillV1"
          | `Partner_stack_backfill_v1 -> "PartnerStackBackfillV1"
          | `Partner_stack_delete_v1 -> "PartnerStackDeleteV1"
          | `Partner_stack_v1 -> "PartnerStackV1"
          | `Album_v1 -> "AlbumV1"
          | `Album_delete_v1 -> "AlbumDeleteV1"
          | `Album_user_v1 -> "AlbumUserV1"
          | `Album_user_backfill_v1 -> "AlbumUserBackfillV1"
          | `Album_user_delete_v1 -> "AlbumUserDeleteV1"
          | `Album_asset_create_v1 -> "AlbumAssetCreateV1"
          | `Album_asset_update_v1 -> "AlbumAssetUpdateV1"
          | `Album_asset_backfill_v1 -> "AlbumAssetBackfillV1"
          | `Album_asset_exif_create_v1 -> "AlbumAssetExifCreateV1"
          | `Album_asset_exif_update_v1 -> "AlbumAssetExifUpdateV1"
          | `Album_asset_exif_backfill_v1 -> "AlbumAssetExifBackfillV1"
          | `Album_to_asset_v1 -> "AlbumToAssetV1"
          | `Album_to_asset_delete_v1 -> "AlbumToAssetDeleteV1"
          | `Album_to_asset_backfill_v1 -> "AlbumToAssetBackfillV1"
          | `Memory_v1 -> "MemoryV1"
          | `Memory_delete_v1 -> "MemoryDeleteV1"
          | `Memory_to_asset_v1 -> "MemoryToAssetV1"
          | `Memory_to_asset_delete_v1 -> "MemoryToAssetDeleteV1"
          | `Stack_v1 -> "StackV1"
          | `Stack_delete_v1 -> "StackDeleteV1"
          | `Person_v1 -> "PersonV1"
          | `Person_delete_v1 -> "PersonDeleteV1"
          | `Asset_face_v1 -> "AssetFaceV1"
          | `Asset_face_delete_v1 -> "AssetFaceDeleteV1"
          | `User_metadata_v1 -> "UserMetadataV1"
          | `User_metadata_delete_v1 -> "UserMetadataDeleteV1"
          | `Sync_ack_v1 -> "SyncAckV1"
          | `Sync_reset_v1 -> "SyncResetV1"
          | `Sync_complete_v1 -> "SyncCompleteV1")
  end
end

module SyncAckDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        types : SyncEntity.Type.t list option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?types () = { types }
    
    let types t = t.types
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAckDeleteDto"
        (fun types -> { types })
      |> Jsont.Object.opt_mem "types" (Jsont.list SyncEntity.Type.jsont) ~enc:(fun r -> r.types)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAck = struct
  module Types = struct
    module Dto = struct
      type t = {
        ack : string;
        type_ : SyncEntity.Type.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~ack ~type_ () = { ack; type_ }
    
    let ack t = t.ack
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAckDto"
        (fun ack type_ -> { ack; type_ })
      |> Jsont.Object.mem "ack" Jsont.string ~enc:(fun r -> r.ack)
      |> Jsont.Object.mem "type" SyncEntity.Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve acknowledgements
  
      Retrieve the synchronization acknowledgments for the current session. *)
  let get_sync_ack client () =
    let op_name = "get_sync_ack" in
    let url_path = "/sync/ack" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SyncCompleteV1 = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module SyncAssetMetadataV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        key : string;
        value : Jsont.json;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ~key ~value () = { asset_id; key; value }
    
    let asset_id t = t.asset_id
    let key t = t.key
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetMetadataV1"
        (fun asset_id key value -> { asset_id; key; value })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAssetMetadataDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        key : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ~key () = { asset_id; key }
    
    let asset_id t = t.asset_id
    let key t = t.key
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetMetadataDeleteV1"
        (fun asset_id key -> { asset_id; key })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAssetFaceV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        bounding_box_x1 : int;
        bounding_box_x2 : int;
        bounding_box_y1 : int;
        bounding_box_y2 : int;
        id : string;
        image_height : int;
        image_width : int;
        person_id : string option;
        source_type : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ~bounding_box_x1 ~bounding_box_x2 ~bounding_box_y1 ~bounding_box_y2 ~id ~image_height ~image_width ~source_type ?person_id () = { asset_id; bounding_box_x1; bounding_box_x2; bounding_box_y1; bounding_box_y2; id; image_height; image_width; person_id; source_type }
    
    let asset_id t = t.asset_id
    let bounding_box_x1 t = t.bounding_box_x1
    let bounding_box_x2 t = t.bounding_box_x2
    let bounding_box_y1 t = t.bounding_box_y1
    let bounding_box_y2 t = t.bounding_box_y2
    let id t = t.id
    let image_height t = t.image_height
    let image_width t = t.image_width
    let person_id t = t.person_id
    let source_type t = t.source_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetFaceV1"
        (fun asset_id bounding_box_x1 bounding_box_x2 bounding_box_y1 bounding_box_y2 id image_height image_width person_id source_type -> { asset_id; bounding_box_x1; bounding_box_x2; bounding_box_y1; bounding_box_y2; id; image_height; image_width; person_id; source_type })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "boundingBoxX1" Jsont.int ~enc:(fun r -> r.bounding_box_x1)
      |> Jsont.Object.mem "boundingBoxX2" Jsont.int ~enc:(fun r -> r.bounding_box_x2)
      |> Jsont.Object.mem "boundingBoxY1" Jsont.int ~enc:(fun r -> r.bounding_box_y1)
      |> Jsont.Object.mem "boundingBoxY2" Jsont.int ~enc:(fun r -> r.bounding_box_y2)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "imageHeight" Jsont.int ~enc:(fun r -> r.image_height)
      |> Jsont.Object.mem "imageWidth" Jsont.int ~enc:(fun r -> r.image_width)
      |> Jsont.Object.mem "personId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.person_id)
      |> Jsont.Object.mem "sourceType" Jsont.string ~enc:(fun r -> r.source_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAssetFaceDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_face_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_face_id () = { asset_face_id }
    
    let asset_face_id t = t.asset_face_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetFaceDeleteV1"
        (fun asset_face_id -> { asset_face_id })
      |> Jsont.Object.mem "assetFaceId" Jsont.string ~enc:(fun r -> r.asset_face_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAssetExifV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        city : string option;
        country : string option;
        date_time_original : Ptime.t option;
        description : string option;
        exif_image_height : int option;
        exif_image_width : int option;
        exposure_time : string option;
        f_number : float option;
        file_size_in_byte : int option;
        focal_length : float option;
        fps : float option;
        iso : int option;
        latitude : float option;
        lens_model : string option;
        longitude : float option;
        make : string option;
        model : string option;
        modify_date : Ptime.t option;
        orientation : string option;
        profile_description : string option;
        projection_type : string option;
        rating : int option;
        state : string option;
        time_zone : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ?city ?country ?date_time_original ?description ?exif_image_height ?exif_image_width ?exposure_time ?f_number ?file_size_in_byte ?focal_length ?fps ?iso ?latitude ?lens_model ?longitude ?make ?model ?modify_date ?orientation ?profile_description ?projection_type ?rating ?state ?time_zone () = { asset_id; city; country; date_time_original; description; exif_image_height; exif_image_width; exposure_time; f_number; file_size_in_byte; focal_length; fps; iso; latitude; lens_model; longitude; make; model; modify_date; orientation; profile_description; projection_type; rating; state; time_zone }
    
    let asset_id t = t.asset_id
    let city t = t.city
    let country t = t.country
    let date_time_original t = t.date_time_original
    let description t = t.description
    let exif_image_height t = t.exif_image_height
    let exif_image_width t = t.exif_image_width
    let exposure_time t = t.exposure_time
    let f_number t = t.f_number
    let file_size_in_byte t = t.file_size_in_byte
    let focal_length t = t.focal_length
    let fps t = t.fps
    let iso t = t.iso
    let latitude t = t.latitude
    let lens_model t = t.lens_model
    let longitude t = t.longitude
    let make t = t.make
    let model t = t.model
    let modify_date t = t.modify_date
    let orientation t = t.orientation
    let profile_description t = t.profile_description
    let projection_type t = t.projection_type
    let rating t = t.rating
    let state t = t.state
    let time_zone t = t.time_zone
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetExifV1"
        (fun asset_id city country date_time_original description exif_image_height exif_image_width exposure_time f_number file_size_in_byte focal_length fps iso latitude lens_model longitude make model modify_date orientation profile_description projection_type rating state time_zone -> { asset_id; city; country; date_time_original; description; exif_image_height; exif_image_width; exposure_time; f_number; file_size_in_byte; focal_length; fps; iso; latitude; lens_model; longitude; make; model; modify_date; orientation; profile_description; projection_type; rating; state; time_zone })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.mem "dateTimeOriginal" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.date_time_original)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "exifImageHeight" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.exif_image_height)
      |> Jsont.Object.mem "exifImageWidth" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.exif_image_width)
      |> Jsont.Object.mem "exposureTime" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.exposure_time)
      |> Jsont.Object.mem "fNumber" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.f_number)
      |> Jsont.Object.mem "fileSizeInByte" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.file_size_in_byte)
      |> Jsont.Object.mem "focalLength" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.focal_length)
      |> Jsont.Object.mem "fps" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.fps)
      |> Jsont.Object.mem "iso" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.iso)
      |> Jsont.Object.mem "latitude" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.latitude)
      |> Jsont.Object.mem "lensModel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.lens_model)
      |> Jsont.Object.mem "longitude" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.longitude)
      |> Jsont.Object.mem "make" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.make)
      |> Jsont.Object.mem "model" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.model)
      |> Jsont.Object.mem "modifyDate" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.modify_date)
      |> Jsont.Object.mem "orientation" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.orientation)
      |> Jsont.Object.mem "profileDescription" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.profile_description)
      |> Jsont.Object.mem "projectionType" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.projection_type)
      |> Jsont.Object.mem "rating" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.rating)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "timeZone" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.time_zone)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAssetDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id () = { asset_id }
    
    let asset_id t = t.asset_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetDeleteV1"
        (fun asset_id -> { asset_id })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAlbumUserDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        album_id : string;
        user_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~album_id ~user_id () = { album_id; user_id }
    
    let album_id t = t.album_id
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAlbumUserDeleteV1"
        (fun album_id user_id -> { album_id; user_id })
      |> Jsont.Object.mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAlbumToAssetV1 = struct
  module Types = struct
    module T = struct
      type t = {
        album_id : string;
        asset_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~album_id ~asset_id () = { album_id; asset_id }
    
    let album_id t = t.album_id
    let asset_id t = t.asset_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAlbumToAssetV1"
        (fun album_id asset_id -> { album_id; asset_id })
      |> Jsont.Object.mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAlbumToAssetDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        album_id : string;
        asset_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~album_id ~asset_id () = { album_id; asset_id }
    
    let album_id t = t.album_id
    let asset_id t = t.asset_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAlbumToAssetDeleteV1"
        (fun album_id asset_id -> { album_id; asset_id })
      |> Jsont.Object.mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAlbumDeleteV1 = struct
  module Types = struct
    module T = struct
      type t = {
        album_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~album_id () = { album_id }
    
    let album_id t = t.album_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAlbumDeleteV1"
        (fun album_id -> { album_id })
      |> Jsont.Object.mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAckV1 = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module SyncAckSet = struct
  module Types = struct
    module Dto = struct
      type t = {
        acks : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~acks () = { acks }
    
    let acks t = t.acks
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAckSetDto"
        (fun acks -> { acks })
      |> Jsont.Object.mem "acks" (Openapi.Runtime.validated_list ~max_items:1000 Jsont.string) ~enc:(fun r -> r.acks)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module StorageFolder = struct
  module Types = struct
    module T = struct
      type t = [
        | `Encoded_video
        | `Library
        | `Upload
        | `Profile
        | `Thumbs
        | `Backups
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"StorageFolder"
        ~dec:(function
          | "encoded-video" -> `Encoded_video
          | "library" -> `Library
          | "upload" -> `Upload
          | "profile" -> `Profile
          | "thumbs" -> `Thumbs
          | "backups" -> `Backups
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Encoded_video -> "encoded-video"
          | `Library -> "library"
          | `Upload -> "upload"
          | `Profile -> "profile"
          | `Thumbs -> "thumbs"
          | `Backups -> "backups")
  end
end

module MaintenanceDetectInstallStorageFolder = struct
  module Types = struct
    module Dto = struct
      type t = {
        files : float;
        folder : StorageFolder.T.t;
        readable : bool;
        writable : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~files ~folder ~readable ~writable () = { files; folder; readable; writable }
    
    let files t = t.files
    let folder t = t.folder
    let readable t = t.readable
    let writable t = t.writable
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MaintenanceDetectInstallStorageFolderDto"
        (fun files folder readable writable -> { files; folder; readable; writable })
      |> Jsont.Object.mem "files" Jsont.number ~enc:(fun r -> r.files)
      |> Jsont.Object.mem "folder" StorageFolder.T.jsont ~enc:(fun r -> r.folder)
      |> Jsont.Object.mem "readable" Jsont.bool ~enc:(fun r -> r.readable)
      |> Jsont.Object.mem "writable" Jsont.bool ~enc:(fun r -> r.writable)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MaintenanceDetectInstall = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        storage : MaintenanceDetectInstallStorageFolder.Dto.t list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~storage () = { storage }
    
    let storage t = t.storage
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MaintenanceDetectInstallResponseDto"
        (fun storage -> { storage })
      |> Jsont.Object.mem "storage" (Jsont.list MaintenanceDetectInstallStorageFolder.Dto.jsont) ~enc:(fun r -> r.storage)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Detect existing install
  
      Collect integrity checks and other heuristics about local data. *)
  let detect_prior_install client () =
    let op_name = "detect_prior_install" in
    let url_path = "/admin/maintenance/detect-install" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Source = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Machine_learning
        | `Exif
        | `Manual
      ]
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"SourceType"
        ~dec:(function
          | "machine-learning" -> `Machine_learning
          | "exif" -> `Exif
          | "manual" -> `Manual
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Machine_learning -> "machine-learning"
          | `Exif -> "exif"
          | `Manual -> "manual")
  end
end

module AssetFaceWithoutPerson = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        bounding_box_x1 : int;
        bounding_box_x2 : int;
        bounding_box_y1 : int;
        bounding_box_y2 : int;
        id : string;
        image_height : int;
        image_width : int;
        source_type : Source.Type.t option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~bounding_box_x1 ~bounding_box_x2 ~bounding_box_y1 ~bounding_box_y2 ~id ~image_height ~image_width ?source_type () = { bounding_box_x1; bounding_box_x2; bounding_box_y1; bounding_box_y2; id; image_height; image_width; source_type }
    
    let bounding_box_x1 t = t.bounding_box_x1
    let bounding_box_x2 t = t.bounding_box_x2
    let bounding_box_y1 t = t.bounding_box_y1
    let bounding_box_y2 t = t.bounding_box_y2
    let id t = t.id
    let image_height t = t.image_height
    let image_width t = t.image_width
    let source_type t = t.source_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFaceWithoutPersonResponseDto"
        (fun bounding_box_x1 bounding_box_x2 bounding_box_y1 bounding_box_y2 id image_height image_width source_type -> { bounding_box_x1; bounding_box_x2; bounding_box_y1; bounding_box_y2; id; image_height; image_width; source_type })
      |> Jsont.Object.mem "boundingBoxX1" Jsont.int ~enc:(fun r -> r.bounding_box_x1)
      |> Jsont.Object.mem "boundingBoxX2" Jsont.int ~enc:(fun r -> r.bounding_box_x2)
      |> Jsont.Object.mem "boundingBoxY1" Jsont.int ~enc:(fun r -> r.bounding_box_y1)
      |> Jsont.Object.mem "boundingBoxY2" Jsont.int ~enc:(fun r -> r.bounding_box_y2)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "imageHeight" Jsont.int ~enc:(fun r -> r.image_height)
      |> Jsont.Object.mem "imageWidth" Jsont.int ~enc:(fun r -> r.image_width)
      |> Jsont.Object.opt_mem "sourceType" Source.Type.jsont ~enc:(fun r -> r.source_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PersonWithFaces = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        birth_date : string option;
        color : string option;
        faces : AssetFaceWithoutPerson.ResponseDto.t list;
        id : string;
        is_favorite : bool option;
        is_hidden : bool;
        name : string;
        thumbnail_path : string;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~faces ~id ~is_hidden ~name ~thumbnail_path ?birth_date ?color ?is_favorite ?updated_at () = { birth_date; color; faces; id; is_favorite; is_hidden; name; thumbnail_path; updated_at }
    
    let birth_date t = t.birth_date
    let color t = t.color
    let faces t = t.faces
    let id t = t.id
    let is_favorite t = t.is_favorite
    let is_hidden t = t.is_hidden
    let name t = t.name
    let thumbnail_path t = t.thumbnail_path
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PersonWithFacesResponseDto"
        (fun birth_date color faces id is_favorite is_hidden name thumbnail_path updated_at -> { birth_date; color; faces; id; is_favorite; is_hidden; name; thumbnail_path; updated_at })
      |> Jsont.Object.mem "birthDate" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.birth_date)
      |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "faces" (Jsont.list AssetFaceWithoutPerson.ResponseDto.jsont) ~enc:(fun r -> r.faces)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.mem "isHidden" Jsont.bool ~enc:(fun r -> r.is_hidden)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SignUp = struct
  module Types = struct
    module Dto = struct
      type t = {
        email : string;
        name : string;
        password : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~email ~name ~password () = { email; name; password }
    
    let email t = t.email
    let name t = t.name
    let password t = t.password
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SignUpDto"
        (fun email name password -> { email; name; password })
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SharedLinks = struct
  module Types = struct
    module Update = struct
      type t = {
        enabled : bool option;
        sidebar_web : bool option;
      }
    end
  
    module Response = struct
      type t = {
        enabled : bool;
        sidebar_web : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?enabled ?sidebar_web () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SharedLinksUpdate"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.opt_mem "sidebarWeb" Jsont.bool ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(enabled=true) ?(sidebar_web=false) () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SharedLinksResponse"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.mem "enabled" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "sidebarWeb" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SharedLinkEdit = struct
  module Types = struct
    module Dto = struct
      type t = {
        allow_download : bool option;
        allow_upload : bool option;
        change_expiry_time : bool option;  (** Few clients cannot send null to set the expiryTime to never.
      Setting this flag and not sending expiryAt is considered as null instead.
      Clients that can send null values can ignore this. *)
        description : string option;
        expires_at : Ptime.t option;
        password : string option;
        show_metadata : bool option;
        slug : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?allow_download ?allow_upload ?change_expiry_time ?description ?expires_at ?password ?show_metadata ?slug () = { allow_download; allow_upload; change_expiry_time; description; expires_at; password; show_metadata; slug }
    
    let allow_download t = t.allow_download
    let allow_upload t = t.allow_upload
    let change_expiry_time t = t.change_expiry_time
    let description t = t.description
    let expires_at t = t.expires_at
    let password t = t.password
    let show_metadata t = t.show_metadata
    let slug t = t.slug
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SharedLinkEditDto"
        (fun allow_download allow_upload change_expiry_time description expires_at password show_metadata slug -> { allow_download; allow_upload; change_expiry_time; description; expires_at; password; show_metadata; slug })
      |> Jsont.Object.opt_mem "allowDownload" Jsont.bool ~enc:(fun r -> r.allow_download)
      |> Jsont.Object.opt_mem "allowUpload" Jsont.bool ~enc:(fun r -> r.allow_upload)
      |> Jsont.Object.opt_mem "changeExpiryTime" Jsont.bool ~enc:(fun r -> r.change_expiry_time)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "expiresAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.mem "password" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "showMetadata" Jsont.bool ~enc:(fun r -> r.show_metadata)
      |> Jsont.Object.mem "slug" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.slug)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SessionUnlock = struct
  module Types = struct
    module Dto = struct
      type t = {
        password : string option;
        pin_code : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?password ?pin_code () = { password; pin_code }
    
    let password t = t.password
    let pin_code t = t.pin_code
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SessionUnlockDto"
        (fun password pin_code -> { password; pin_code })
      |> Jsont.Object.opt_mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "pinCode" Jsont.string ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Session = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        is_pending_sync_reset : bool option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        app_version : string option;
        created_at : string;
        current : bool;
        device_os : string;
        device_type : string;
        expires_at : string option;
        id : string;
        is_pending_sync_reset : bool;
        updated_at : string;
      }
    end
  
    module CreateDto = struct
      type t = {
        device_os : string option;
        device_type : string option;
        duration : float option;  (** session duration, in seconds *)
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?is_pending_sync_reset () = { is_pending_sync_reset }
    
    let is_pending_sync_reset t = t.is_pending_sync_reset
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SessionUpdateDto"
        (fun is_pending_sync_reset -> { is_pending_sync_reset })
      |> Jsont.Object.opt_mem "isPendingSyncReset" Jsont.bool ~enc:(fun r -> r.is_pending_sync_reset)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~created_at ~current ~device_os ~device_type ~id ~is_pending_sync_reset ~updated_at ?app_version ?expires_at () = { app_version; created_at; current; device_os; device_type; expires_at; id; is_pending_sync_reset; updated_at }
    
    let app_version t = t.app_version
    let created_at t = t.created_at
    let current t = t.current
    let device_os t = t.device_os
    let device_type t = t.device_type
    let expires_at t = t.expires_at
    let id t = t.id
    let is_pending_sync_reset t = t.is_pending_sync_reset
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SessionResponseDto"
        (fun app_version created_at current device_os device_type expires_at id is_pending_sync_reset updated_at -> { app_version; created_at; current; device_os; device_type; expires_at; id; is_pending_sync_reset; updated_at })
      |> Jsont.Object.mem "appVersion" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.app_version)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "current" Jsont.bool ~enc:(fun r -> r.current)
      |> Jsont.Object.mem "deviceOS" Jsont.string ~enc:(fun r -> r.device_os)
      |> Jsont.Object.mem "deviceType" Jsont.string ~enc:(fun r -> r.device_type)
      |> Jsont.Object.opt_mem "expiresAt" Jsont.string ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isPendingSyncReset" Jsont.bool ~enc:(fun r -> r.is_pending_sync_reset)
      |> Jsont.Object.mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ?device_os ?device_type ?duration () = { device_os; device_type; duration }
    
    let device_os t = t.device_os
    let device_type t = t.device_type
    let duration t = t.duration
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SessionCreateDto"
        (fun device_os device_type duration -> { device_os; device_type; duration })
      |> Jsont.Object.opt_mem "deviceOS" Jsont.string ~enc:(fun r -> r.device_os)
      |> Jsont.Object.opt_mem "deviceType" Jsont.string ~enc:(fun r -> r.device_type)
      |> Jsont.Object.opt_mem "duration" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.duration)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve user sessions
  
      Retrieve all sessions for a specific user. *)
  let get_user_sessions_admin ~id client () =
    let op_name = "get_user_sessions_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}/sessions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve sessions
  
      Retrieve a list of sessions for the user. *)
  let get_sessions client () =
    let op_name = "get_sessions" in
    let url_path = "/sessions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a session
  
      Update a specific session identified by id. *)
  let update_session ~id ~body client () =
    let op_name = "update_session" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/sessions/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SessionCreate = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        app_version : string option;
        created_at : string;
        current : bool;
        device_os : string;
        device_type : string;
        expires_at : string option;
        id : string;
        is_pending_sync_reset : bool;
        token : string;
        updated_at : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~created_at ~current ~device_os ~device_type ~id ~is_pending_sync_reset ~token ~updated_at ?app_version ?expires_at () = { app_version; created_at; current; device_os; device_type; expires_at; id; is_pending_sync_reset; token; updated_at }
    
    let app_version t = t.app_version
    let created_at t = t.created_at
    let current t = t.current
    let device_os t = t.device_os
    let device_type t = t.device_type
    let expires_at t = t.expires_at
    let id t = t.id
    let is_pending_sync_reset t = t.is_pending_sync_reset
    let token t = t.token
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SessionCreateResponseDto"
        (fun app_version created_at current device_os device_type expires_at id is_pending_sync_reset token updated_at -> { app_version; created_at; current; device_os; device_type; expires_at; id; is_pending_sync_reset; token; updated_at })
      |> Jsont.Object.mem "appVersion" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.app_version)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "current" Jsont.bool ~enc:(fun r -> r.current)
      |> Jsont.Object.mem "deviceOS" Jsont.string ~enc:(fun r -> r.device_os)
      |> Jsont.Object.mem "deviceType" Jsont.string ~enc:(fun r -> r.device_type)
      |> Jsont.Object.opt_mem "expiresAt" Jsont.string ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isPendingSyncReset" Jsont.bool ~enc:(fun r -> r.is_pending_sync_reset)
      |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
      |> Jsont.Object.mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a session
  
      Create a session as a child to the current session. This endpoint is used for casting. *)
  let create_session ~body client () =
    let op_name = "create_session" in
    let url_path = "/sessions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Session.CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerVersionHistory = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        created_at : Ptime.t;
        id : string;
        version : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~created_at ~id ~version () = { created_at; id; version }
    
    let created_at t = t.created_at
    let id t = t.id
    let version t = t.version
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerVersionHistoryResponseDto"
        (fun created_at id version -> { created_at; id; version })
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get version history
  
      Retrieve a list of past versions the server has been on. *)
  let get_version_history client () =
    let op_name = "get_version_history" in
    let url_path = "/server/version-history" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerVersion = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        major : int;
        minor : int;
        patch : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~major ~minor ~patch () = { major; minor; patch }
    
    let major t = t.major
    let minor t = t.minor
    let patch t = t.patch
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerVersionResponseDto"
        (fun major minor patch -> { major; minor; patch })
      |> Jsont.Object.mem "major" Jsont.int ~enc:(fun r -> r.major)
      |> Jsont.Object.mem "minor" Jsont.int ~enc:(fun r -> r.minor)
      |> Jsont.Object.mem "patch" Jsont.int ~enc:(fun r -> r.patch)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get server version
  
      Retrieve the current server version in semantic versioning (semver) format. *)
  let get_server_version client () =
    let op_name = "get_server_version" in
    let url_path = "/server/version" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerTheme = struct
  module Types = struct
    module Dto = struct
      type t = {
        custom_css : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~custom_css () = { custom_css }
    
    let custom_css t = t.custom_css
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerThemeDto"
        (fun custom_css -> { custom_css })
      |> Jsont.Object.mem "customCss" Jsont.string ~enc:(fun r -> r.custom_css)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get theme
  
      Retrieve the custom CSS, if existent. *)
  let get_theme client () =
    let op_name = "get_theme" in
    let url_path = "/server/theme" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerStorage = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        disk_available : string;
        disk_available_raw : int64;
        disk_size : string;
        disk_size_raw : int64;
        disk_usage_percentage : float;
        disk_use : string;
        disk_use_raw : int64;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~disk_available ~disk_available_raw ~disk_size ~disk_size_raw ~disk_usage_percentage ~disk_use ~disk_use_raw () = { disk_available; disk_available_raw; disk_size; disk_size_raw; disk_usage_percentage; disk_use; disk_use_raw }
    
    let disk_available t = t.disk_available
    let disk_available_raw t = t.disk_available_raw
    let disk_size t = t.disk_size
    let disk_size_raw t = t.disk_size_raw
    let disk_usage_percentage t = t.disk_usage_percentage
    let disk_use t = t.disk_use
    let disk_use_raw t = t.disk_use_raw
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerStorageResponseDto"
        (fun disk_available disk_available_raw disk_size disk_size_raw disk_usage_percentage disk_use disk_use_raw -> { disk_available; disk_available_raw; disk_size; disk_size_raw; disk_usage_percentage; disk_use; disk_use_raw })
      |> Jsont.Object.mem "diskAvailable" Jsont.string ~enc:(fun r -> r.disk_available)
      |> Jsont.Object.mem "diskAvailableRaw" Jsont.int64 ~enc:(fun r -> r.disk_available_raw)
      |> Jsont.Object.mem "diskSize" Jsont.string ~enc:(fun r -> r.disk_size)
      |> Jsont.Object.mem "diskSizeRaw" Jsont.int64 ~enc:(fun r -> r.disk_size_raw)
      |> Jsont.Object.mem "diskUsagePercentage" Jsont.number ~enc:(fun r -> r.disk_usage_percentage)
      |> Jsont.Object.mem "diskUse" Jsont.string ~enc:(fun r -> r.disk_use)
      |> Jsont.Object.mem "diskUseRaw" Jsont.int64 ~enc:(fun r -> r.disk_use_raw)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get storage
  
      Retrieve the current storage utilization information of the server. *)
  let get_storage client () =
    let op_name = "get_storage" in
    let url_path = "/server/storage" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerPing = struct
  module Types = struct
    module Response = struct
      type t = {
        res : string;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~res () = { res }
    
    let res t = t.res
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerPingResponse"
        (fun res -> { res })
      |> Jsont.Object.mem "res" Jsont.string ~enc:(fun r -> r.res)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Ping
  
      Pong *)
  let ping_server client () =
    let op_name = "ping_server" in
    let url_path = "/server/ping" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerMediaTypes = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        image : string list;
        sidecar : string list;
        video : string list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~image ~sidecar ~video () = { image; sidecar; video }
    
    let image t = t.image
    let sidecar t = t.sidecar
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerMediaTypesResponseDto"
        (fun image sidecar video -> { image; sidecar; video })
      |> Jsont.Object.mem "image" (Jsont.list Jsont.string) ~enc:(fun r -> r.image)
      |> Jsont.Object.mem "sidecar" (Jsont.list Jsont.string) ~enc:(fun r -> r.sidecar)
      |> Jsont.Object.mem "video" (Jsont.list Jsont.string) ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get supported media types
  
      Retrieve all media types supported by the server. *)
  let get_supported_media_types client () =
    let op_name = "get_supported_media_types" in
    let url_path = "/server/media-types" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerFeatures = struct
  module Types = struct
    module Dto = struct
      type t = {
        config_file : bool;
        duplicate_detection : bool;
        email : bool;
        facial_recognition : bool;
        import_faces : bool;
        map : bool;
        oauth : bool;
        oauth_auto_launch : bool;
        ocr : bool;
        password_login : bool;
        reverse_geocoding : bool;
        search : bool;
        sidecar : bool;
        smart_search : bool;
        trash : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~config_file ~duplicate_detection ~email ~facial_recognition ~import_faces ~map ~oauth ~oauth_auto_launch ~ocr ~password_login ~reverse_geocoding ~search ~sidecar ~smart_search ~trash () = { config_file; duplicate_detection; email; facial_recognition; import_faces; map; oauth; oauth_auto_launch; ocr; password_login; reverse_geocoding; search; sidecar; smart_search; trash }
    
    let config_file t = t.config_file
    let duplicate_detection t = t.duplicate_detection
    let email t = t.email
    let facial_recognition t = t.facial_recognition
    let import_faces t = t.import_faces
    let map t = t.map
    let oauth t = t.oauth
    let oauth_auto_launch t = t.oauth_auto_launch
    let ocr t = t.ocr
    let password_login t = t.password_login
    let reverse_geocoding t = t.reverse_geocoding
    let search t = t.search
    let sidecar t = t.sidecar
    let smart_search t = t.smart_search
    let trash t = t.trash
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerFeaturesDto"
        (fun config_file duplicate_detection email facial_recognition import_faces map oauth oauth_auto_launch ocr password_login reverse_geocoding search sidecar smart_search trash -> { config_file; duplicate_detection; email; facial_recognition; import_faces; map; oauth; oauth_auto_launch; ocr; password_login; reverse_geocoding; search; sidecar; smart_search; trash })
      |> Jsont.Object.mem "configFile" Jsont.bool ~enc:(fun r -> r.config_file)
      |> Jsont.Object.mem "duplicateDetection" Jsont.bool ~enc:(fun r -> r.duplicate_detection)
      |> Jsont.Object.mem "email" Jsont.bool ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "facialRecognition" Jsont.bool ~enc:(fun r -> r.facial_recognition)
      |> Jsont.Object.mem "importFaces" Jsont.bool ~enc:(fun r -> r.import_faces)
      |> Jsont.Object.mem "map" Jsont.bool ~enc:(fun r -> r.map)
      |> Jsont.Object.mem "oauth" Jsont.bool ~enc:(fun r -> r.oauth)
      |> Jsont.Object.mem "oauthAutoLaunch" Jsont.bool ~enc:(fun r -> r.oauth_auto_launch)
      |> Jsont.Object.mem "ocr" Jsont.bool ~enc:(fun r -> r.ocr)
      |> Jsont.Object.mem "passwordLogin" Jsont.bool ~enc:(fun r -> r.password_login)
      |> Jsont.Object.mem "reverseGeocoding" Jsont.bool ~enc:(fun r -> r.reverse_geocoding)
      |> Jsont.Object.mem "search" Jsont.bool ~enc:(fun r -> r.search)
      |> Jsont.Object.mem "sidecar" Jsont.bool ~enc:(fun r -> r.sidecar)
      |> Jsont.Object.mem "smartSearch" Jsont.bool ~enc:(fun r -> r.smart_search)
      |> Jsont.Object.mem "trash" Jsont.bool ~enc:(fun r -> r.trash)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get features
  
      Retrieve available features supported by this server. *)
  let get_server_features client () =
    let op_name = "get_server_features" in
    let url_path = "/server/features" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerConfig = struct
  module Types = struct
    module Dto = struct
      type t = {
        external_domain : string;
        is_initialized : bool;
        is_onboarded : bool;
        login_page_message : string;
        maintenance_mode : bool;
        map_dark_style_url : string;
        map_light_style_url : string;
        oauth_button_text : string;
        public_users : bool;
        trash_days : int;
        user_delete_delay : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~external_domain ~is_initialized ~is_onboarded ~login_page_message ~maintenance_mode ~map_dark_style_url ~map_light_style_url ~oauth_button_text ~public_users ~trash_days ~user_delete_delay () = { external_domain; is_initialized; is_onboarded; login_page_message; maintenance_mode; map_dark_style_url; map_light_style_url; oauth_button_text; public_users; trash_days; user_delete_delay }
    
    let external_domain t = t.external_domain
    let is_initialized t = t.is_initialized
    let is_onboarded t = t.is_onboarded
    let login_page_message t = t.login_page_message
    let maintenance_mode t = t.maintenance_mode
    let map_dark_style_url t = t.map_dark_style_url
    let map_light_style_url t = t.map_light_style_url
    let oauth_button_text t = t.oauth_button_text
    let public_users t = t.public_users
    let trash_days t = t.trash_days
    let user_delete_delay t = t.user_delete_delay
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerConfigDto"
        (fun external_domain is_initialized is_onboarded login_page_message maintenance_mode map_dark_style_url map_light_style_url oauth_button_text public_users trash_days user_delete_delay -> { external_domain; is_initialized; is_onboarded; login_page_message; maintenance_mode; map_dark_style_url; map_light_style_url; oauth_button_text; public_users; trash_days; user_delete_delay })
      |> Jsont.Object.mem "externalDomain" Jsont.string ~enc:(fun r -> r.external_domain)
      |> Jsont.Object.mem "isInitialized" Jsont.bool ~enc:(fun r -> r.is_initialized)
      |> Jsont.Object.mem "isOnboarded" Jsont.bool ~enc:(fun r -> r.is_onboarded)
      |> Jsont.Object.mem "loginPageMessage" Jsont.string ~enc:(fun r -> r.login_page_message)
      |> Jsont.Object.mem "maintenanceMode" Jsont.bool ~enc:(fun r -> r.maintenance_mode)
      |> Jsont.Object.mem "mapDarkStyleUrl" Jsont.string ~enc:(fun r -> r.map_dark_style_url)
      |> Jsont.Object.mem "mapLightStyleUrl" Jsont.string ~enc:(fun r -> r.map_light_style_url)
      |> Jsont.Object.mem "oauthButtonText" Jsont.string ~enc:(fun r -> r.oauth_button_text)
      |> Jsont.Object.mem "publicUsers" Jsont.bool ~enc:(fun r -> r.public_users)
      |> Jsont.Object.mem "trashDays" Jsont.int ~enc:(fun r -> r.trash_days)
      |> Jsont.Object.mem "userDeleteDelay" Jsont.int ~enc:(fun r -> r.user_delete_delay)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get config
  
      Retrieve the current server configuration. *)
  let get_server_config client () =
    let op_name = "get_server_config" in
    let url_path = "/server/config" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerApkLinks = struct
  module Types = struct
    module Dto = struct
      type t = {
        arm64v8a : string;
        armeabiv7a : string;
        universal : string;
        x86_64 : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~arm64v8a ~armeabiv7a ~universal ~x86_64 () = { arm64v8a; armeabiv7a; universal; x86_64 }
    
    let arm64v8a t = t.arm64v8a
    let armeabiv7a t = t.armeabiv7a
    let universal t = t.universal
    let x86_64 t = t.x86_64
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerApkLinksDto"
        (fun arm64v8a armeabiv7a universal x86_64 -> { arm64v8a; armeabiv7a; universal; x86_64 })
      |> Jsont.Object.mem "arm64v8a" Jsont.string ~enc:(fun r -> r.arm64v8a)
      |> Jsont.Object.mem "armeabiv7a" Jsont.string ~enc:(fun r -> r.armeabiv7a)
      |> Jsont.Object.mem "universal" Jsont.string ~enc:(fun r -> r.universal)
      |> Jsont.Object.mem "x86_64" Jsont.string ~enc:(fun r -> r.x86_64)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get APK links
  
      Retrieve links to the APKs for the current server version. *)
  let get_apk_links client () =
    let op_name = "get_apk_links" in
    let url_path = "/server/apk-links" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerAbout = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        build : string option;
        build_image : string option;
        build_image_url : string option;
        build_url : string option;
        exiftool : string option;
        ffmpeg : string option;
        imagemagick : string option;
        libvips : string option;
        licensed : bool;
        nodejs : string option;
        repository : string option;
        repository_url : string option;
        source_commit : string option;
        source_ref : string option;
        source_url : string option;
        third_party_bug_feature_url : string option;
        third_party_documentation_url : string option;
        third_party_source_url : string option;
        third_party_support_url : string option;
        version : string;
        version_url : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~licensed ~version ~version_url ?build ?build_image ?build_image_url ?build_url ?exiftool ?ffmpeg ?imagemagick ?libvips ?nodejs ?repository ?repository_url ?source_commit ?source_ref ?source_url ?third_party_bug_feature_url ?third_party_documentation_url ?third_party_source_url ?third_party_support_url () = { build; build_image; build_image_url; build_url; exiftool; ffmpeg; imagemagick; libvips; licensed; nodejs; repository; repository_url; source_commit; source_ref; source_url; third_party_bug_feature_url; third_party_documentation_url; third_party_source_url; third_party_support_url; version; version_url }
    
    let build t = t.build
    let build_image t = t.build_image
    let build_image_url t = t.build_image_url
    let build_url t = t.build_url
    let exiftool t = t.exiftool
    let ffmpeg t = t.ffmpeg
    let imagemagick t = t.imagemagick
    let libvips t = t.libvips
    let licensed t = t.licensed
    let nodejs t = t.nodejs
    let repository t = t.repository
    let repository_url t = t.repository_url
    let source_commit t = t.source_commit
    let source_ref t = t.source_ref
    let source_url t = t.source_url
    let third_party_bug_feature_url t = t.third_party_bug_feature_url
    let third_party_documentation_url t = t.third_party_documentation_url
    let third_party_source_url t = t.third_party_source_url
    let third_party_support_url t = t.third_party_support_url
    let version t = t.version
    let version_url t = t.version_url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerAboutResponseDto"
        (fun build build_image build_image_url build_url exiftool ffmpeg imagemagick libvips licensed nodejs repository repository_url source_commit source_ref source_url third_party_bug_feature_url third_party_documentation_url third_party_source_url third_party_support_url version version_url -> { build; build_image; build_image_url; build_url; exiftool; ffmpeg; imagemagick; libvips; licensed; nodejs; repository; repository_url; source_commit; source_ref; source_url; third_party_bug_feature_url; third_party_documentation_url; third_party_source_url; third_party_support_url; version; version_url })
      |> Jsont.Object.opt_mem "build" Jsont.string ~enc:(fun r -> r.build)
      |> Jsont.Object.opt_mem "buildImage" Jsont.string ~enc:(fun r -> r.build_image)
      |> Jsont.Object.opt_mem "buildImageUrl" Jsont.string ~enc:(fun r -> r.build_image_url)
      |> Jsont.Object.opt_mem "buildUrl" Jsont.string ~enc:(fun r -> r.build_url)
      |> Jsont.Object.opt_mem "exiftool" Jsont.string ~enc:(fun r -> r.exiftool)
      |> Jsont.Object.opt_mem "ffmpeg" Jsont.string ~enc:(fun r -> r.ffmpeg)
      |> Jsont.Object.opt_mem "imagemagick" Jsont.string ~enc:(fun r -> r.imagemagick)
      |> Jsont.Object.opt_mem "libvips" Jsont.string ~enc:(fun r -> r.libvips)
      |> Jsont.Object.mem "licensed" Jsont.bool ~enc:(fun r -> r.licensed)
      |> Jsont.Object.opt_mem "nodejs" Jsont.string ~enc:(fun r -> r.nodejs)
      |> Jsont.Object.opt_mem "repository" Jsont.string ~enc:(fun r -> r.repository)
      |> Jsont.Object.opt_mem "repositoryUrl" Jsont.string ~enc:(fun r -> r.repository_url)
      |> Jsont.Object.opt_mem "sourceCommit" Jsont.string ~enc:(fun r -> r.source_commit)
      |> Jsont.Object.opt_mem "sourceRef" Jsont.string ~enc:(fun r -> r.source_ref)
      |> Jsont.Object.opt_mem "sourceUrl" Jsont.string ~enc:(fun r -> r.source_url)
      |> Jsont.Object.opt_mem "thirdPartyBugFeatureUrl" Jsont.string ~enc:(fun r -> r.third_party_bug_feature_url)
      |> Jsont.Object.opt_mem "thirdPartyDocumentationUrl" Jsont.string ~enc:(fun r -> r.third_party_documentation_url)
      |> Jsont.Object.opt_mem "thirdPartySourceUrl" Jsont.string ~enc:(fun r -> r.third_party_source_url)
      |> Jsont.Object.opt_mem "thirdPartySupportUrl" Jsont.string ~enc:(fun r -> r.third_party_support_url)
      |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
      |> Jsont.Object.mem "versionUrl" Jsont.string ~enc:(fun r -> r.version_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get server information
  
      Retrieve a list of information about the server. *)
  let get_about_info client () =
    let op_name = "get_about_info" in
    let url_path = "/server/about" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SearchSuggestion = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Country
        | `State
        | `City
        | `Camera_make
        | `Camera_model
        | `Camera_lens_model
      ]
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"SearchSuggestionType"
        ~dec:(function
          | "country" -> `Country
          | "state" -> `State
          | "city" -> `City
          | "camera-make" -> `Camera_make
          | "camera-model" -> `Camera_model
          | "camera-lens-model" -> `Camera_lens_model
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Country -> "country"
          | `State -> "state"
          | `City -> "city"
          | `Camera_make -> "camera-make"
          | `Camera_model -> "camera-model"
          | `Camera_lens_model -> "camera-lens-model")
  end
end

module SearchFacetCount = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        count : int;
        value : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~count ~value () = { count; value }
    
    let count t = t.count
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchFacetCountResponseDto"
        (fun count value -> { count; value })
      |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
      |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchFacet = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        counts : SearchFacetCount.ResponseDto.t list;
        field_name : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~counts ~field_name () = { counts; field_name }
    
    let counts t = t.counts
    let field_name t = t.field_name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchFacetResponseDto"
        (fun counts field_name -> { counts; field_name })
      |> Jsont.Object.mem "counts" (Jsont.list SearchFacetCount.ResponseDto.jsont) ~enc:(fun r -> r.counts)
      |> Jsont.Object.mem "fieldName" Jsont.string ~enc:(fun r -> r.field_name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RotateParameters = struct
  module Types = struct
    module T = struct
      type t = {
        angle : float;  (** Rotation angle in degrees *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~angle () = { angle }
    
    let angle t = t.angle
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RotateParameters"
        (fun angle -> { angle })
      |> Jsont.Object.mem "angle" Jsont.number ~enc:(fun r -> r.angle)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ReverseGeocodingState = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        last_import_file_name : string option;
        last_update : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?last_import_file_name ?last_update () = { last_import_file_name; last_update }
    
    let last_import_file_name t = t.last_import_file_name
    let last_update t = t.last_update
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ReverseGeocodingStateResponseDto"
        (fun last_import_file_name last_update -> { last_import_file_name; last_update })
      |> Jsont.Object.mem "lastImportFileName" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.last_import_file_name)
      |> Jsont.Object.mem "lastUpdate" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.last_update)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve reverse geocoding state
  
      Retrieve the current state of the reverse geocoding import. *)
  let get_reverse_geocoding_state client () =
    let op_name = "get_reverse_geocoding_state" in
    let url_path = "/system-metadata/reverse-geocoding-state" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ReactionLevel = struct
  module Types = struct
    module T = struct
      type t = [
        | `Album
        | `Asset
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"ReactionLevel"
        ~dec:(function
          | "album" -> `Album
          | "asset" -> `Asset
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Album -> "album"
          | `Asset -> "asset")
  end
end

module Reaction = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Comment
        | `Like
      ]
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"ReactionType"
        ~dec:(function
          | "comment" -> `Comment
          | "like" -> `Like
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Comment -> "comment"
          | `Like -> "like")
  end
end

module Activity = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_id : string option;
        comment : string option;
        created_at : Ptime.t;
        id : string;
        type_ : Reaction.Type.t;
        user : User.ResponseDto.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        album_id : string;
        asset_id : string option;
        comment : string option;
        type_ : Reaction.Type.t;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~created_at ~id ~type_ ~user ?asset_id ?comment () = { asset_id; comment; created_at; id; type_; user }
    
    let asset_id t = t.asset_id
    let comment t = t.comment
    let created_at t = t.created_at
    let id t = t.id
    let type_ t = t.type_
    let user t = t.user
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ActivityResponseDto"
        (fun asset_id comment created_at id type_ user -> { asset_id; comment; created_at; id; type_; user })
      |> Jsont.Object.mem "assetId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "comment" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.comment)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "type" Reaction.Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "user" User.ResponseDto.jsont ~enc:(fun r -> r.user)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~album_id ~type_ ?asset_id ?comment () = { album_id; asset_id; comment; type_ }
    
    let album_id t = t.album_id
    let asset_id t = t.asset_id
    let comment t = t.comment
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ActivityCreateDto"
        (fun album_id asset_id comment type_ -> { album_id; asset_id; comment; type_ })
      |> Jsont.Object.mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.opt_mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.opt_mem "comment" Jsont.string ~enc:(fun r -> r.comment)
      |> Jsont.Object.mem "type" Reaction.Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all activities
  
      Returns a list of activities for the selected asset or album. The activities are returned in sorted order, with the oldest activities appearing first. *)
  let get_activities ~album_id ?asset_id ?level ?type_ ?user_id client () =
    let op_name = "get_activities" in
    let url_path = "/activities" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"albumId" ~value:album_id; Openapi.Runtime.Query.optional ~key:"assetId" ~value:asset_id; Openapi.Runtime.Query.optional ~key:"level" ~value:level; Openapi.Runtime.Query.optional ~key:"type" ~value:type_; Openapi.Runtime.Query.optional ~key:"userId" ~value:user_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create an activity
  
      Create a like or a comment for an album, or an asset in an album. *)
  let create_activity ~body client () =
    let op_name = "create_activity" in
    let url_path = "/activities" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Ratings = struct
  module Types = struct
    module Update = struct
      type t = {
        enabled : bool option;
      }
    end
  
    module Response = struct
      type t = {
        enabled : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?enabled () = { enabled }
    
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RatingsUpdate"
        (fun enabled -> { enabled })
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(enabled=false) () = { enabled }
    
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RatingsResponse"
        (fun enabled -> { enabled })
      |> Jsont.Object.mem "enabled" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module QueueStatusLegacy = struct
  module Types = struct
    module Dto = struct
      type t = {
        is_active : bool;
        is_paused : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~is_active ~is_paused () = { is_active; is_paused }
    
    let is_active t = t.is_active
    let is_paused t = t.is_paused
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueStatusLegacyDto"
        (fun is_active is_paused -> { is_active; is_paused })
      |> Jsont.Object.mem "isActive" Jsont.bool ~enc:(fun r -> r.is_active)
      |> Jsont.Object.mem "isPaused" Jsont.bool ~enc:(fun r -> r.is_paused)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module QueueStatistics = struct
  module Types = struct
    module Dto = struct
      type t = {
        active : int;
        completed : int;
        delayed : int;
        failed : int;
        paused : int;
        waiting : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~active ~completed ~delayed ~failed ~paused ~waiting () = { active; completed; delayed; failed; paused; waiting }
    
    let active t = t.active
    let completed t = t.completed
    let delayed t = t.delayed
    let failed t = t.failed
    let paused t = t.paused
    let waiting t = t.waiting
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueStatisticsDto"
        (fun active completed delayed failed paused waiting -> { active; completed; delayed; failed; paused; waiting })
      |> Jsont.Object.mem "active" Jsont.int ~enc:(fun r -> r.active)
      |> Jsont.Object.mem "completed" Jsont.int ~enc:(fun r -> r.completed)
      |> Jsont.Object.mem "delayed" Jsont.int ~enc:(fun r -> r.delayed)
      |> Jsont.Object.mem "failed" Jsont.int ~enc:(fun r -> r.failed)
      |> Jsont.Object.mem "paused" Jsont.int ~enc:(fun r -> r.paused)
      |> Jsont.Object.mem "waiting" Jsont.int ~enc:(fun r -> r.waiting)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module QueueName = struct
  module Types = struct
    module T = struct
      type t = [
        | `Thumbnail_generation
        | `Metadata_extraction
        | `Video_conversion
        | `Face_detection
        | `Facial_recognition
        | `Smart_search
        | `Duplicate_detection
        | `Background_task
        | `Storage_template_migration
        | `Migration
        | `Search
        | `Sidecar
        | `Library
        | `Notifications
        | `Backup_database
        | `Ocr
        | `Workflow
        | `Editor
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"QueueName"
        ~dec:(function
          | "thumbnailGeneration" -> `Thumbnail_generation
          | "metadataExtraction" -> `Metadata_extraction
          | "videoConversion" -> `Video_conversion
          | "faceDetection" -> `Face_detection
          | "facialRecognition" -> `Facial_recognition
          | "smartSearch" -> `Smart_search
          | "duplicateDetection" -> `Duplicate_detection
          | "backgroundTask" -> `Background_task
          | "storageTemplateMigration" -> `Storage_template_migration
          | "migration" -> `Migration
          | "search" -> `Search
          | "sidecar" -> `Sidecar
          | "library" -> `Library
          | "notifications" -> `Notifications
          | "backupDatabase" -> `Backup_database
          | "ocr" -> `Ocr
          | "workflow" -> `Workflow
          | "editor" -> `Editor
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Thumbnail_generation -> "thumbnailGeneration"
          | `Metadata_extraction -> "metadataExtraction"
          | `Video_conversion -> "videoConversion"
          | `Face_detection -> "faceDetection"
          | `Facial_recognition -> "facialRecognition"
          | `Smart_search -> "smartSearch"
          | `Duplicate_detection -> "duplicateDetection"
          | `Background_task -> "backgroundTask"
          | `Storage_template_migration -> "storageTemplateMigration"
          | `Migration -> "migration"
          | `Search -> "search"
          | `Sidecar -> "sidecar"
          | `Library -> "library"
          | `Notifications -> "notifications"
          | `Backup_database -> "backupDatabase"
          | `Ocr -> "ocr"
          | `Workflow -> "workflow"
          | `Editor -> "editor")
  end
end

module Queue = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        is_paused : bool option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        is_paused : bool;
        name : QueueName.T.t;
        statistics : QueueStatistics.Dto.t;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?is_paused () = { is_paused }
    
    let is_paused t = t.is_paused
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueUpdateDto"
        (fun is_paused -> { is_paused })
      |> Jsont.Object.opt_mem "isPaused" Jsont.bool ~enc:(fun r -> r.is_paused)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~is_paused ~name ~statistics () = { is_paused; name; statistics }
    
    let is_paused t = t.is_paused
    let name t = t.name
    let statistics t = t.statistics
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueResponseDto"
        (fun is_paused name statistics -> { is_paused; name; statistics })
      |> Jsont.Object.mem "isPaused" Jsont.bool ~enc:(fun r -> r.is_paused)
      |> Jsont.Object.mem "name" QueueName.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "statistics" QueueStatistics.Dto.jsont ~enc:(fun r -> r.statistics)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all queues
  
      Retrieves a list of queues. *)
  let get_queues client () =
    let op_name = "get_queues" in
    let url_path = "/queues" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a queue
  
      Retrieves a specific queue by its name. *)
  let get_queue ~name client () =
    let op_name = "get_queue" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/queues/{name}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a queue
  
      Change the paused status of a specific queue. *)
  let update_queue ~name ~body client () =
    let op_name = "update_queue" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/queues/{name}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module QueueDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        failed : bool option;  (** If true, will also remove failed jobs from the queue. *)
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?failed () = { failed }
    
    let failed t = t.failed
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueDeleteDto"
        (fun failed -> { failed })
      |> Jsont.Object.opt_mem "failed" Jsont.bool ~enc:(fun r -> r.failed)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module QueueCommand = struct
  module Types = struct
    module T = struct
      type t = [
        | `Start
        | `Pause
        | `Resume
        | `Empty
        | `Clear_failed
      ]
    end
  
    module Dto = struct
      type t = {
        command : T.t;
        force : bool option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"QueueCommand"
        ~dec:(function
          | "start" -> `Start
          | "pause" -> `Pause
          | "resume" -> `Resume
          | "empty" -> `Empty
          | "clear-failed" -> `Clear_failed
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Start -> "start"
          | `Pause -> "pause"
          | `Resume -> "resume"
          | `Empty -> "empty"
          | `Clear_failed -> "clear-failed")
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~command ?force () = { command; force }
    
    let command t = t.command
    let force t = t.force
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueCommandDto"
        (fun command force -> { command; force })
      |> Jsont.Object.mem "command" T.jsont ~enc:(fun r -> r.command)
      |> Jsont.Object.opt_mem "force" Jsont.bool ~enc:(fun r -> r.force)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module QueueResponseLegacy = struct
  module Types = struct
    module Dto = struct
      type t = {
        job_counts : QueueStatistics.Dto.t;
        queue_status : QueueStatusLegacy.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~job_counts ~queue_status () = { job_counts; queue_status }
    
    let job_counts t = t.job_counts
    let queue_status t = t.queue_status
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueResponseLegacyDto"
        (fun job_counts queue_status -> { job_counts; queue_status })
      |> Jsont.Object.mem "jobCounts" QueueStatistics.Dto.jsont ~enc:(fun r -> r.job_counts)
      |> Jsont.Object.mem "queueStatus" QueueStatusLegacy.Dto.jsont ~enc:(fun r -> r.queue_status)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Run jobs
  
      Queue all assets for a specific job type. Defaults to only queueing assets that have not yet been processed, but the force command can be used to re-process all assets. *)
  let run_queue_command_legacy ~name ~body client () =
    let op_name = "run_queue_command_legacy" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/jobs/{name}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json QueueCommand.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module QueuesResponseLegacy = struct
  module Types = struct
    module Dto = struct
      type t = {
        background_task : QueueResponseLegacy.Dto.t;
        backup_database : QueueResponseLegacy.Dto.t;
        duplicate_detection : QueueResponseLegacy.Dto.t;
        editor : QueueResponseLegacy.Dto.t;
        face_detection : QueueResponseLegacy.Dto.t;
        facial_recognition : QueueResponseLegacy.Dto.t;
        library : QueueResponseLegacy.Dto.t;
        metadata_extraction : QueueResponseLegacy.Dto.t;
        migration : QueueResponseLegacy.Dto.t;
        notifications : QueueResponseLegacy.Dto.t;
        ocr : QueueResponseLegacy.Dto.t;
        search : QueueResponseLegacy.Dto.t;
        sidecar : QueueResponseLegacy.Dto.t;
        smart_search : QueueResponseLegacy.Dto.t;
        storage_template_migration : QueueResponseLegacy.Dto.t;
        thumbnail_generation : QueueResponseLegacy.Dto.t;
        video_conversion : QueueResponseLegacy.Dto.t;
        workflow : QueueResponseLegacy.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~background_task ~backup_database ~duplicate_detection ~editor ~face_detection ~facial_recognition ~library ~metadata_extraction ~migration ~notifications ~ocr ~search ~sidecar ~smart_search ~storage_template_migration ~thumbnail_generation ~video_conversion ~workflow () = { background_task; backup_database; duplicate_detection; editor; face_detection; facial_recognition; library; metadata_extraction; migration; notifications; ocr; search; sidecar; smart_search; storage_template_migration; thumbnail_generation; video_conversion; workflow }
    
    let background_task t = t.background_task
    let backup_database t = t.backup_database
    let duplicate_detection t = t.duplicate_detection
    let editor t = t.editor
    let face_detection t = t.face_detection
    let facial_recognition t = t.facial_recognition
    let library t = t.library
    let metadata_extraction t = t.metadata_extraction
    let migration t = t.migration
    let notifications t = t.notifications
    let ocr t = t.ocr
    let search t = t.search
    let sidecar t = t.sidecar
    let smart_search t = t.smart_search
    let storage_template_migration t = t.storage_template_migration
    let thumbnail_generation t = t.thumbnail_generation
    let video_conversion t = t.video_conversion
    let workflow t = t.workflow
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueuesResponseLegacyDto"
        (fun background_task backup_database duplicate_detection editor face_detection facial_recognition library metadata_extraction migration notifications ocr search sidecar smart_search storage_template_migration thumbnail_generation video_conversion workflow -> { background_task; backup_database; duplicate_detection; editor; face_detection; facial_recognition; library; metadata_extraction; migration; notifications; ocr; search; sidecar; smart_search; storage_template_migration; thumbnail_generation; video_conversion; workflow })
      |> Jsont.Object.mem "backgroundTask" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.background_task)
      |> Jsont.Object.mem "backupDatabase" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.backup_database)
      |> Jsont.Object.mem "duplicateDetection" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.duplicate_detection)
      |> Jsont.Object.mem "editor" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.editor)
      |> Jsont.Object.mem "faceDetection" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.face_detection)
      |> Jsont.Object.mem "facialRecognition" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.facial_recognition)
      |> Jsont.Object.mem "library" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.library)
      |> Jsont.Object.mem "metadataExtraction" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.metadata_extraction)
      |> Jsont.Object.mem "migration" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.migration)
      |> Jsont.Object.mem "notifications" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.notifications)
      |> Jsont.Object.mem "ocr" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.ocr)
      |> Jsont.Object.mem "search" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.search)
      |> Jsont.Object.mem "sidecar" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.sidecar)
      |> Jsont.Object.mem "smartSearch" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.smart_search)
      |> Jsont.Object.mem "storageTemplateMigration" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.storage_template_migration)
      |> Jsont.Object.mem "thumbnailGeneration" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.thumbnail_generation)
      |> Jsont.Object.mem "videoConversion" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.video_conversion)
      |> Jsont.Object.mem "workflow" QueueResponseLegacy.Dto.jsont ~enc:(fun r -> r.workflow)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve queue counts and status
  
      Retrieve the counts of the current queue, as well as the current status. *)
  let get_queues_legacy client () =
    let op_name = "get_queues_legacy" in
    let url_path = "/jobs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Purchase = struct
  module Types = struct
    module Update = struct
      type t = {
        hide_buy_button_until : string option;
        show_support_badge : bool option;
      }
    end
  
    module Response = struct
      type t = {
        hide_buy_button_until : string;
        show_support_badge : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?hide_buy_button_until ?show_support_badge () = { hide_buy_button_until; show_support_badge }
    
    let hide_buy_button_until t = t.hide_buy_button_until
    let show_support_badge t = t.show_support_badge
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PurchaseUpdate"
        (fun hide_buy_button_until show_support_badge -> { hide_buy_button_until; show_support_badge })
      |> Jsont.Object.opt_mem "hideBuyButtonUntil" Jsont.string ~enc:(fun r -> r.hide_buy_button_until)
      |> Jsont.Object.opt_mem "showSupportBadge" Jsont.bool ~enc:(fun r -> r.show_support_badge)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ~hide_buy_button_until ~show_support_badge () = { hide_buy_button_until; show_support_badge }
    
    let hide_buy_button_until t = t.hide_buy_button_until
    let show_support_badge t = t.show_support_badge
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PurchaseResponse"
        (fun hide_buy_button_until show_support_badge -> { hide_buy_button_until; show_support_badge })
      |> Jsont.Object.mem "hideBuyButtonUntil" Jsont.string ~enc:(fun r -> r.hide_buy_button_until)
      |> Jsont.Object.mem "showSupportBadge" Jsont.bool ~enc:(fun r -> r.show_support_badge)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PluginContext = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Asset
        | `Album
        | `Person
      ]
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"PluginContextType"
        ~dec:(function
          | "asset" -> `Asset
          | "album" -> `Album
          | "person" -> `Person
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Asset -> "asset"
          | `Album -> "album"
          | `Person -> "person")
  end
end

module PluginTrigger = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Asset_create
        | `Person_recognized
      ]
    end
  
    module ResponseDto = struct
      type t = {
        context_type : PluginContext.Type.t;
        type_ : Type.t;
      }
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"PluginTriggerType"
        ~dec:(function
          | "AssetCreate" -> `Asset_create
          | "PersonRecognized" -> `Person_recognized
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Asset_create -> "AssetCreate"
          | `Person_recognized -> "PersonRecognized")
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~context_type ~type_ () = { context_type; type_ }
    
    let context_type t = t.context_type
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PluginTriggerResponseDto"
        (fun context_type type_ -> { context_type; type_ })
      |> Jsont.Object.mem "contextType" PluginContext.Type.jsont ~enc:(fun r -> r.context_type)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all plugin triggers
  
      Retrieve a list of all available plugin triggers. *)
  let get_plugin_triggers client () =
    let op_name = "get_plugin_triggers" in
    let url_path = "/plugins/triggers" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Workflow = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        actions : WorkflowActionItem.Dto.t list option;
        description : string option;
        enabled : bool option;
        filters : WorkflowFilterItem.Dto.t list option;
        name : string option;
        trigger_type : PluginTrigger.Type.t option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        actions : WorkflowAction.ResponseDto.t list;
        created_at : string;
        description : string;
        enabled : bool;
        filters : WorkflowFilter.ResponseDto.t list;
        id : string;
        name : string option;
        owner_id : string;
        trigger_type : PluginTrigger.Type.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        actions : WorkflowActionItem.Dto.t list;
        description : string option;
        enabled : bool option;
        filters : WorkflowFilterItem.Dto.t list;
        name : string;
        trigger_type : PluginTrigger.Type.t;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?actions ?description ?enabled ?filters ?name ?trigger_type () = { actions; description; enabled; filters; name; trigger_type }
    
    let actions t = t.actions
    let description t = t.description
    let enabled t = t.enabled
    let filters t = t.filters
    let name t = t.name
    let trigger_type t = t.trigger_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowUpdateDto"
        (fun actions description enabled filters name trigger_type -> { actions; description; enabled; filters; name; trigger_type })
      |> Jsont.Object.opt_mem "actions" (Jsont.list WorkflowActionItem.Dto.jsont) ~enc:(fun r -> r.actions)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.opt_mem "filters" (Jsont.list WorkflowFilterItem.Dto.jsont) ~enc:(fun r -> r.filters)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "triggerType" PluginTrigger.Type.jsont ~enc:(fun r -> r.trigger_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~actions ~created_at ~description ~enabled ~filters ~id ~owner_id ~trigger_type ?name () = { actions; created_at; description; enabled; filters; id; name; owner_id; trigger_type }
    
    let actions t = t.actions
    let created_at t = t.created_at
    let description t = t.description
    let enabled t = t.enabled
    let filters t = t.filters
    let id t = t.id
    let name t = t.name
    let owner_id t = t.owner_id
    let trigger_type t = t.trigger_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowResponseDto"
        (fun actions created_at description enabled filters id name owner_id trigger_type -> { actions; created_at; description; enabled; filters; id; name; owner_id; trigger_type })
      |> Jsont.Object.mem "actions" (Jsont.list WorkflowAction.ResponseDto.jsont) ~enc:(fun r -> r.actions)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "filters" (Jsont.list WorkflowFilter.ResponseDto.jsont) ~enc:(fun r -> r.filters)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "triggerType" PluginTrigger.Type.jsont ~enc:(fun r -> r.trigger_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~actions ~filters ~name ~trigger_type ?description ?enabled () = { actions; description; enabled; filters; name; trigger_type }
    
    let actions t = t.actions
    let description t = t.description
    let enabled t = t.enabled
    let filters t = t.filters
    let name t = t.name
    let trigger_type t = t.trigger_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WorkflowCreateDto"
        (fun actions description enabled filters name trigger_type -> { actions; description; enabled; filters; name; trigger_type })
      |> Jsont.Object.mem "actions" (Jsont.list WorkflowActionItem.Dto.jsont) ~enc:(fun r -> r.actions)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "filters" (Jsont.list WorkflowFilterItem.Dto.jsont) ~enc:(fun r -> r.filters)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "triggerType" PluginTrigger.Type.jsont ~enc:(fun r -> r.trigger_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all workflows
  
      Retrieve a list of workflows available to the authenticated user. *)
  let get_workflows client () =
    let op_name = "get_workflows" in
    let url_path = "/workflows" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a workflow
  
      Create a new workflow, the workflow can also be created with empty filters and actions. *)
  let create_workflow ~body client () =
    let op_name = "create_workflow" in
    let url_path = "/workflows" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a workflow
  
      Retrieve information about a specific workflow by its ID. *)
  let get_workflow ~id client () =
    let op_name = "get_workflow" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/workflows/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a workflow
  
      Update the information of a specific workflow by its ID. This endpoint can be used to update the workflow name, description, trigger type, filters and actions order, etc. *)
  let update_workflow ~id ~body client () =
    let op_name = "update_workflow" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/workflows/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PluginFilter = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        description : string;
        id : string;
        method_name : string;
        plugin_id : string;
        schema : Jsont.json option;
        supported_contexts : PluginContext.Type.t list;
        title : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~description ~id ~method_name ~plugin_id ~supported_contexts ~title ?schema () = { description; id; method_name; plugin_id; schema; supported_contexts; title }
    
    let description t = t.description
    let id t = t.id
    let method_name t = t.method_name
    let plugin_id t = t.plugin_id
    let schema t = t.schema
    let supported_contexts t = t.supported_contexts
    let title t = t.title
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PluginFilterResponseDto"
        (fun description id method_name plugin_id schema supported_contexts title -> { description; id; method_name; plugin_id; schema; supported_contexts; title })
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "methodName" Jsont.string ~enc:(fun r -> r.method_name)
      |> Jsont.Object.mem "pluginId" Jsont.string ~enc:(fun r -> r.plugin_id)
      |> Jsont.Object.mem "schema" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.schema)
      |> Jsont.Object.mem "supportedContexts" (Jsont.list PluginContext.Type.jsont) ~enc:(fun r -> r.supported_contexts)
      |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PluginAction = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        description : string;
        id : string;
        method_name : string;
        plugin_id : string;
        schema : Jsont.json option;
        supported_contexts : PluginContext.Type.t list;
        title : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~description ~id ~method_name ~plugin_id ~supported_contexts ~title ?schema () = { description; id; method_name; plugin_id; schema; supported_contexts; title }
    
    let description t = t.description
    let id t = t.id
    let method_name t = t.method_name
    let plugin_id t = t.plugin_id
    let schema t = t.schema
    let supported_contexts t = t.supported_contexts
    let title t = t.title
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PluginActionResponseDto"
        (fun description id method_name plugin_id schema supported_contexts title -> { description; id; method_name; plugin_id; schema; supported_contexts; title })
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "methodName" Jsont.string ~enc:(fun r -> r.method_name)
      |> Jsont.Object.mem "pluginId" Jsont.string ~enc:(fun r -> r.plugin_id)
      |> Jsont.Object.mem "schema" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.schema)
      |> Jsont.Object.mem "supportedContexts" (Jsont.list PluginContext.Type.jsont) ~enc:(fun r -> r.supported_contexts)
      |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Plugin = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        actions : PluginAction.ResponseDto.t list;
        author : string;
        created_at : string;
        description : string;
        filters : PluginFilter.ResponseDto.t list;
        id : string;
        name : string;
        title : string;
        updated_at : string;
        version : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~actions ~author ~created_at ~description ~filters ~id ~name ~title ~updated_at ~version () = { actions; author; created_at; description; filters; id; name; title; updated_at; version }
    
    let actions t = t.actions
    let author t = t.author
    let created_at t = t.created_at
    let description t = t.description
    let filters t = t.filters
    let id t = t.id
    let name t = t.name
    let title t = t.title
    let updated_at t = t.updated_at
    let version t = t.version
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PluginResponseDto"
        (fun actions author created_at description filters id name title updated_at version -> { actions; author; created_at; description; filters; id; name; title; updated_at; version })
      |> Jsont.Object.mem "actions" (Jsont.list PluginAction.ResponseDto.jsont) ~enc:(fun r -> r.actions)
      |> Jsont.Object.mem "author" Jsont.string ~enc:(fun r -> r.author)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "filters" (Jsont.list PluginFilter.ResponseDto.jsont) ~enc:(fun r -> r.filters)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
      |> Jsont.Object.mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all plugins
  
      Retrieve a list of plugins available to the authenticated user. *)
  let get_plugins client () =
    let op_name = "get_plugins" in
    let url_path = "/plugins" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a plugin
  
      Retrieve information about a specific plugin by its ID. *)
  let get_plugin ~id client () =
    let op_name = "get_plugin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/plugins/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Places = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        admin1name : string option;
        admin2name : string option;
        latitude : float;
        longitude : float;
        name : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~latitude ~longitude ~name ?admin1name ?admin2name () = { admin1name; admin2name; latitude; longitude; name }
    
    let admin1name t = t.admin1name
    let admin2name t = t.admin2name
    let latitude t = t.latitude
    let longitude t = t.longitude
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlacesResponseDto"
        (fun admin1name admin2name latitude longitude name -> { admin1name; admin2name; latitude; longitude; name })
      |> Jsont.Object.opt_mem "admin1name" Jsont.string ~enc:(fun r -> r.admin1name)
      |> Jsont.Object.opt_mem "admin2name" Jsont.string ~enc:(fun r -> r.admin2name)
      |> Jsont.Object.mem "latitude" Jsont.number ~enc:(fun r -> r.latitude)
      |> Jsont.Object.mem "longitude" Jsont.number ~enc:(fun r -> r.longitude)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Search places
  
      Search for places by name. *)
  let search_places ~name client () =
    let op_name = "search_places" in
    let url_path = "/search/places" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"name" ~value:name]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PinCodeSetup = struct
  module Types = struct
    module Dto = struct
      type t = {
        pin_code : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~pin_code () = { pin_code }
    
    let pin_code t = t.pin_code
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PinCodeSetupDto"
        (fun pin_code -> { pin_code })
      |> Jsont.Object.mem "pinCode" Jsont.string ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PinCodeReset = struct
  module Types = struct
    module Dto = struct
      type t = {
        password : string option;
        pin_code : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?password ?pin_code () = { password; pin_code }
    
    let password t = t.password
    let pin_code t = t.pin_code
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PinCodeResetDto"
        (fun password pin_code -> { password; pin_code })
      |> Jsont.Object.opt_mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "pinCode" Jsont.string ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PinCodeChange = struct
  module Types = struct
    module Dto = struct
      type t = {
        new_pin_code : string;
        password : string option;
        pin_code : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~new_pin_code ?password ?pin_code () = { new_pin_code; password; pin_code }
    
    let new_pin_code t = t.new_pin_code
    let password t = t.password
    let pin_code t = t.pin_code
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PinCodeChangeDto"
        (fun new_pin_code password pin_code -> { new_pin_code; password; pin_code })
      |> Jsont.Object.mem "newPinCode" Jsont.string ~enc:(fun r -> r.new_pin_code)
      |> Jsont.Object.opt_mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "pinCode" Jsont.string ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PersonStatistics = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        assets : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~assets () = { assets }
    
    let assets t = t.assets
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PersonStatisticsResponseDto"
        (fun assets -> { assets })
      |> Jsont.Object.mem "assets" Jsont.int ~enc:(fun r -> r.assets)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get person statistics
  
      Retrieve statistics about a specific person. *)
  let get_person_statistics ~id client () =
    let op_name = "get_person_statistics" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}/statistics" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Permission = struct
  module Types = struct
    module T = struct
      type t = [
        | `All
        | `Activity_create
        | `Activity_read
        | `Activity_update
        | `Activity_delete
        | `Activity_statistics
        | `Api_key_create
        | `Api_key_read
        | `Api_key_update
        | `Api_key_delete
        | `Asset_read
        | `Asset_update
        | `Asset_delete
        | `Asset_statistics
        | `Asset_share
        | `Asset_view
        | `Asset_download
        | `Asset_upload
        | `Asset_replace
        | `Asset_copy
        | `Asset_derive
        | `Asset_edit_get
        | `Asset_edit_create
        | `Asset_edit_delete
        | `Album_create
        | `Album_read
        | `Album_update
        | `Album_delete
        | `Album_statistics
        | `Album_share
        | `Album_download
        | `Album_asset_create
        | `Album_asset_delete
        | `Album_user_create
        | `Album_user_update
        | `Album_user_delete
        | `Auth_change_password
        | `Auth_device_delete
        | `Archive_read
        | `Backup_list
        | `Backup_download
        | `Backup_upload
        | `Backup_delete
        | `Duplicate_read
        | `Duplicate_delete
        | `Face_create
        | `Face_read
        | `Face_update
        | `Face_delete
        | `Folder_read
        | `Job_create
        | `Job_read
        | `Library_create
        | `Library_read
        | `Library_update
        | `Library_delete
        | `Library_statistics
        | `Timeline_read
        | `Timeline_download
        | `Maintenance
        | `Map_read
        | `Map_search
        | `Memory_create
        | `Memory_read
        | `Memory_update
        | `Memory_delete
        | `Memory_statistics
        | `Memory_asset_create
        | `Memory_asset_delete
        | `Notification_create
        | `Notification_read
        | `Notification_update
        | `Notification_delete
        | `Partner_create
        | `Partner_read
        | `Partner_update
        | `Partner_delete
        | `Person_create
        | `Person_read
        | `Person_update
        | `Person_delete
        | `Person_statistics
        | `Person_merge
        | `Person_reassign
        | `Pin_code_create
        | `Pin_code_update
        | `Pin_code_delete
        | `Plugin_create
        | `Plugin_read
        | `Plugin_update
        | `Plugin_delete
        | `Server_about
        | `Server_apk_links
        | `Server_storage
        | `Server_statistics
        | `Server_version_check
        | `Server_license_read
        | `Server_license_update
        | `Server_license_delete
        | `Session_create
        | `Session_read
        | `Session_update
        | `Session_delete
        | `Session_lock
        | `Shared_link_create
        | `Shared_link_read
        | `Shared_link_update
        | `Shared_link_delete
        | `Stack_create
        | `Stack_read
        | `Stack_update
        | `Stack_delete
        | `Sync_stream
        | `Sync_checkpoint_read
        | `Sync_checkpoint_update
        | `Sync_checkpoint_delete
        | `System_config_read
        | `System_config_update
        | `System_metadata_read
        | `System_metadata_update
        | `Tag_create
        | `Tag_read
        | `Tag_update
        | `Tag_delete
        | `Tag_asset
        | `User_read
        | `User_update
        | `User_license_create
        | `User_license_read
        | `User_license_update
        | `User_license_delete
        | `User_onboarding_read
        | `User_onboarding_update
        | `User_onboarding_delete
        | `User_preference_read
        | `User_preference_update
        | `User_profile_image_create
        | `User_profile_image_read
        | `User_profile_image_update
        | `User_profile_image_delete
        | `Queue_read
        | `Queue_update
        | `Queue_job_create
        | `Queue_job_read
        | `Queue_job_update
        | `Queue_job_delete
        | `Workflow_create
        | `Workflow_read
        | `Workflow_update
        | `Workflow_delete
        | `Admin_user_create
        | `Admin_user_read
        | `Admin_user_update
        | `Admin_user_delete
        | `Admin_session_read
        | `Admin_auth_unlink_all
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"Permission"
        ~dec:(function
          | "all" -> `All
          | "activity.create" -> `Activity_create
          | "activity.read" -> `Activity_read
          | "activity.update" -> `Activity_update
          | "activity.delete" -> `Activity_delete
          | "activity.statistics" -> `Activity_statistics
          | "apiKey.create" -> `Api_key_create
          | "apiKey.read" -> `Api_key_read
          | "apiKey.update" -> `Api_key_update
          | "apiKey.delete" -> `Api_key_delete
          | "asset.read" -> `Asset_read
          | "asset.update" -> `Asset_update
          | "asset.delete" -> `Asset_delete
          | "asset.statistics" -> `Asset_statistics
          | "asset.share" -> `Asset_share
          | "asset.view" -> `Asset_view
          | "asset.download" -> `Asset_download
          | "asset.upload" -> `Asset_upload
          | "asset.replace" -> `Asset_replace
          | "asset.copy" -> `Asset_copy
          | "asset.derive" -> `Asset_derive
          | "asset.edit.get" -> `Asset_edit_get
          | "asset.edit.create" -> `Asset_edit_create
          | "asset.edit.delete" -> `Asset_edit_delete
          | "album.create" -> `Album_create
          | "album.read" -> `Album_read
          | "album.update" -> `Album_update
          | "album.delete" -> `Album_delete
          | "album.statistics" -> `Album_statistics
          | "album.share" -> `Album_share
          | "album.download" -> `Album_download
          | "albumAsset.create" -> `Album_asset_create
          | "albumAsset.delete" -> `Album_asset_delete
          | "albumUser.create" -> `Album_user_create
          | "albumUser.update" -> `Album_user_update
          | "albumUser.delete" -> `Album_user_delete
          | "auth.changePassword" -> `Auth_change_password
          | "authDevice.delete" -> `Auth_device_delete
          | "archive.read" -> `Archive_read
          | "backup.list" -> `Backup_list
          | "backup.download" -> `Backup_download
          | "backup.upload" -> `Backup_upload
          | "backup.delete" -> `Backup_delete
          | "duplicate.read" -> `Duplicate_read
          | "duplicate.delete" -> `Duplicate_delete
          | "face.create" -> `Face_create
          | "face.read" -> `Face_read
          | "face.update" -> `Face_update
          | "face.delete" -> `Face_delete
          | "folder.read" -> `Folder_read
          | "job.create" -> `Job_create
          | "job.read" -> `Job_read
          | "library.create" -> `Library_create
          | "library.read" -> `Library_read
          | "library.update" -> `Library_update
          | "library.delete" -> `Library_delete
          | "library.statistics" -> `Library_statistics
          | "timeline.read" -> `Timeline_read
          | "timeline.download" -> `Timeline_download
          | "maintenance" -> `Maintenance
          | "map.read" -> `Map_read
          | "map.search" -> `Map_search
          | "memory.create" -> `Memory_create
          | "memory.read" -> `Memory_read
          | "memory.update" -> `Memory_update
          | "memory.delete" -> `Memory_delete
          | "memory.statistics" -> `Memory_statistics
          | "memoryAsset.create" -> `Memory_asset_create
          | "memoryAsset.delete" -> `Memory_asset_delete
          | "notification.create" -> `Notification_create
          | "notification.read" -> `Notification_read
          | "notification.update" -> `Notification_update
          | "notification.delete" -> `Notification_delete
          | "partner.create" -> `Partner_create
          | "partner.read" -> `Partner_read
          | "partner.update" -> `Partner_update
          | "partner.delete" -> `Partner_delete
          | "person.create" -> `Person_create
          | "person.read" -> `Person_read
          | "person.update" -> `Person_update
          | "person.delete" -> `Person_delete
          | "person.statistics" -> `Person_statistics
          | "person.merge" -> `Person_merge
          | "person.reassign" -> `Person_reassign
          | "pinCode.create" -> `Pin_code_create
          | "pinCode.update" -> `Pin_code_update
          | "pinCode.delete" -> `Pin_code_delete
          | "plugin.create" -> `Plugin_create
          | "plugin.read" -> `Plugin_read
          | "plugin.update" -> `Plugin_update
          | "plugin.delete" -> `Plugin_delete
          | "server.about" -> `Server_about
          | "server.apkLinks" -> `Server_apk_links
          | "server.storage" -> `Server_storage
          | "server.statistics" -> `Server_statistics
          | "server.versionCheck" -> `Server_version_check
          | "serverLicense.read" -> `Server_license_read
          | "serverLicense.update" -> `Server_license_update
          | "serverLicense.delete" -> `Server_license_delete
          | "session.create" -> `Session_create
          | "session.read" -> `Session_read
          | "session.update" -> `Session_update
          | "session.delete" -> `Session_delete
          | "session.lock" -> `Session_lock
          | "sharedLink.create" -> `Shared_link_create
          | "sharedLink.read" -> `Shared_link_read
          | "sharedLink.update" -> `Shared_link_update
          | "sharedLink.delete" -> `Shared_link_delete
          | "stack.create" -> `Stack_create
          | "stack.read" -> `Stack_read
          | "stack.update" -> `Stack_update
          | "stack.delete" -> `Stack_delete
          | "sync.stream" -> `Sync_stream
          | "syncCheckpoint.read" -> `Sync_checkpoint_read
          | "syncCheckpoint.update" -> `Sync_checkpoint_update
          | "syncCheckpoint.delete" -> `Sync_checkpoint_delete
          | "systemConfig.read" -> `System_config_read
          | "systemConfig.update" -> `System_config_update
          | "systemMetadata.read" -> `System_metadata_read
          | "systemMetadata.update" -> `System_metadata_update
          | "tag.create" -> `Tag_create
          | "tag.read" -> `Tag_read
          | "tag.update" -> `Tag_update
          | "tag.delete" -> `Tag_delete
          | "tag.asset" -> `Tag_asset
          | "user.read" -> `User_read
          | "user.update" -> `User_update
          | "userLicense.create" -> `User_license_create
          | "userLicense.read" -> `User_license_read
          | "userLicense.update" -> `User_license_update
          | "userLicense.delete" -> `User_license_delete
          | "userOnboarding.read" -> `User_onboarding_read
          | "userOnboarding.update" -> `User_onboarding_update
          | "userOnboarding.delete" -> `User_onboarding_delete
          | "userPreference.read" -> `User_preference_read
          | "userPreference.update" -> `User_preference_update
          | "userProfileImage.create" -> `User_profile_image_create
          | "userProfileImage.read" -> `User_profile_image_read
          | "userProfileImage.update" -> `User_profile_image_update
          | "userProfileImage.delete" -> `User_profile_image_delete
          | "queue.read" -> `Queue_read
          | "queue.update" -> `Queue_update
          | "queueJob.create" -> `Queue_job_create
          | "queueJob.read" -> `Queue_job_read
          | "queueJob.update" -> `Queue_job_update
          | "queueJob.delete" -> `Queue_job_delete
          | "workflow.create" -> `Workflow_create
          | "workflow.read" -> `Workflow_read
          | "workflow.update" -> `Workflow_update
          | "workflow.delete" -> `Workflow_delete
          | "adminUser.create" -> `Admin_user_create
          | "adminUser.read" -> `Admin_user_read
          | "adminUser.update" -> `Admin_user_update
          | "adminUser.delete" -> `Admin_user_delete
          | "adminSession.read" -> `Admin_session_read
          | "adminAuth.unlinkAll" -> `Admin_auth_unlink_all
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `All -> "all"
          | `Activity_create -> "activity.create"
          | `Activity_read -> "activity.read"
          | `Activity_update -> "activity.update"
          | `Activity_delete -> "activity.delete"
          | `Activity_statistics -> "activity.statistics"
          | `Api_key_create -> "apiKey.create"
          | `Api_key_read -> "apiKey.read"
          | `Api_key_update -> "apiKey.update"
          | `Api_key_delete -> "apiKey.delete"
          | `Asset_read -> "asset.read"
          | `Asset_update -> "asset.update"
          | `Asset_delete -> "asset.delete"
          | `Asset_statistics -> "asset.statistics"
          | `Asset_share -> "asset.share"
          | `Asset_view -> "asset.view"
          | `Asset_download -> "asset.download"
          | `Asset_upload -> "asset.upload"
          | `Asset_replace -> "asset.replace"
          | `Asset_copy -> "asset.copy"
          | `Asset_derive -> "asset.derive"
          | `Asset_edit_get -> "asset.edit.get"
          | `Asset_edit_create -> "asset.edit.create"
          | `Asset_edit_delete -> "asset.edit.delete"
          | `Album_create -> "album.create"
          | `Album_read -> "album.read"
          | `Album_update -> "album.update"
          | `Album_delete -> "album.delete"
          | `Album_statistics -> "album.statistics"
          | `Album_share -> "album.share"
          | `Album_download -> "album.download"
          | `Album_asset_create -> "albumAsset.create"
          | `Album_asset_delete -> "albumAsset.delete"
          | `Album_user_create -> "albumUser.create"
          | `Album_user_update -> "albumUser.update"
          | `Album_user_delete -> "albumUser.delete"
          | `Auth_change_password -> "auth.changePassword"
          | `Auth_device_delete -> "authDevice.delete"
          | `Archive_read -> "archive.read"
          | `Backup_list -> "backup.list"
          | `Backup_download -> "backup.download"
          | `Backup_upload -> "backup.upload"
          | `Backup_delete -> "backup.delete"
          | `Duplicate_read -> "duplicate.read"
          | `Duplicate_delete -> "duplicate.delete"
          | `Face_create -> "face.create"
          | `Face_read -> "face.read"
          | `Face_update -> "face.update"
          | `Face_delete -> "face.delete"
          | `Folder_read -> "folder.read"
          | `Job_create -> "job.create"
          | `Job_read -> "job.read"
          | `Library_create -> "library.create"
          | `Library_read -> "library.read"
          | `Library_update -> "library.update"
          | `Library_delete -> "library.delete"
          | `Library_statistics -> "library.statistics"
          | `Timeline_read -> "timeline.read"
          | `Timeline_download -> "timeline.download"
          | `Maintenance -> "maintenance"
          | `Map_read -> "map.read"
          | `Map_search -> "map.search"
          | `Memory_create -> "memory.create"
          | `Memory_read -> "memory.read"
          | `Memory_update -> "memory.update"
          | `Memory_delete -> "memory.delete"
          | `Memory_statistics -> "memory.statistics"
          | `Memory_asset_create -> "memoryAsset.create"
          | `Memory_asset_delete -> "memoryAsset.delete"
          | `Notification_create -> "notification.create"
          | `Notification_read -> "notification.read"
          | `Notification_update -> "notification.update"
          | `Notification_delete -> "notification.delete"
          | `Partner_create -> "partner.create"
          | `Partner_read -> "partner.read"
          | `Partner_update -> "partner.update"
          | `Partner_delete -> "partner.delete"
          | `Person_create -> "person.create"
          | `Person_read -> "person.read"
          | `Person_update -> "person.update"
          | `Person_delete -> "person.delete"
          | `Person_statistics -> "person.statistics"
          | `Person_merge -> "person.merge"
          | `Person_reassign -> "person.reassign"
          | `Pin_code_create -> "pinCode.create"
          | `Pin_code_update -> "pinCode.update"
          | `Pin_code_delete -> "pinCode.delete"
          | `Plugin_create -> "plugin.create"
          | `Plugin_read -> "plugin.read"
          | `Plugin_update -> "plugin.update"
          | `Plugin_delete -> "plugin.delete"
          | `Server_about -> "server.about"
          | `Server_apk_links -> "server.apkLinks"
          | `Server_storage -> "server.storage"
          | `Server_statistics -> "server.statistics"
          | `Server_version_check -> "server.versionCheck"
          | `Server_license_read -> "serverLicense.read"
          | `Server_license_update -> "serverLicense.update"
          | `Server_license_delete -> "serverLicense.delete"
          | `Session_create -> "session.create"
          | `Session_read -> "session.read"
          | `Session_update -> "session.update"
          | `Session_delete -> "session.delete"
          | `Session_lock -> "session.lock"
          | `Shared_link_create -> "sharedLink.create"
          | `Shared_link_read -> "sharedLink.read"
          | `Shared_link_update -> "sharedLink.update"
          | `Shared_link_delete -> "sharedLink.delete"
          | `Stack_create -> "stack.create"
          | `Stack_read -> "stack.read"
          | `Stack_update -> "stack.update"
          | `Stack_delete -> "stack.delete"
          | `Sync_stream -> "sync.stream"
          | `Sync_checkpoint_read -> "syncCheckpoint.read"
          | `Sync_checkpoint_update -> "syncCheckpoint.update"
          | `Sync_checkpoint_delete -> "syncCheckpoint.delete"
          | `System_config_read -> "systemConfig.read"
          | `System_config_update -> "systemConfig.update"
          | `System_metadata_read -> "systemMetadata.read"
          | `System_metadata_update -> "systemMetadata.update"
          | `Tag_create -> "tag.create"
          | `Tag_read -> "tag.read"
          | `Tag_update -> "tag.update"
          | `Tag_delete -> "tag.delete"
          | `Tag_asset -> "tag.asset"
          | `User_read -> "user.read"
          | `User_update -> "user.update"
          | `User_license_create -> "userLicense.create"
          | `User_license_read -> "userLicense.read"
          | `User_license_update -> "userLicense.update"
          | `User_license_delete -> "userLicense.delete"
          | `User_onboarding_read -> "userOnboarding.read"
          | `User_onboarding_update -> "userOnboarding.update"
          | `User_onboarding_delete -> "userOnboarding.delete"
          | `User_preference_read -> "userPreference.read"
          | `User_preference_update -> "userPreference.update"
          | `User_profile_image_create -> "userProfileImage.create"
          | `User_profile_image_read -> "userProfileImage.read"
          | `User_profile_image_update -> "userProfileImage.update"
          | `User_profile_image_delete -> "userProfileImage.delete"
          | `Queue_read -> "queue.read"
          | `Queue_update -> "queue.update"
          | `Queue_job_create -> "queueJob.create"
          | `Queue_job_read -> "queueJob.read"
          | `Queue_job_update -> "queueJob.update"
          | `Queue_job_delete -> "queueJob.delete"
          | `Workflow_create -> "workflow.create"
          | `Workflow_read -> "workflow.read"
          | `Workflow_update -> "workflow.update"
          | `Workflow_delete -> "workflow.delete"
          | `Admin_user_create -> "adminUser.create"
          | `Admin_user_read -> "adminUser.read"
          | `Admin_user_update -> "adminUser.update"
          | `Admin_user_delete -> "adminUser.delete"
          | `Admin_session_read -> "adminSession.read"
          | `Admin_auth_unlink_all -> "adminAuth.unlinkAll")
  end
end

module Apikey = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        name : string option;
        permissions : Permission.T.t list option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        created_at : Ptime.t;
        id : string;
        name : string;
        permissions : Permission.T.t list;
        updated_at : Ptime.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        name : string option;
        permissions : Permission.T.t list;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?name ?permissions () = { name; permissions }
    
    let name t = t.name
    let permissions t = t.permissions
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"APIKeyUpdateDto"
        (fun name permissions -> { name; permissions })
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "permissions" (Openapi.Runtime.validated_list ~min_items:1 Permission.T.jsont) ~enc:(fun r -> r.permissions)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~created_at ~id ~name ~permissions ~updated_at () = { created_at; id; name; permissions; updated_at }
    
    let created_at t = t.created_at
    let id t = t.id
    let name t = t.name
    let permissions t = t.permissions
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"APIKeyResponseDto"
        (fun created_at id name permissions updated_at -> { created_at; id; name; permissions; updated_at })
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "permissions" (Jsont.list Permission.T.jsont) ~enc:(fun r -> r.permissions)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~permissions ?name () = { name; permissions }
    
    let name t = t.name
    let permissions t = t.permissions
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"APIKeyCreateDto"
        (fun name permissions -> { name; permissions })
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "permissions" (Openapi.Runtime.validated_list ~min_items:1 Permission.T.jsont) ~enc:(fun r -> r.permissions)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all API keys
  
      Retrieve all API keys of the current user. *)
  let get_api_keys client () =
    let op_name = "get_api_keys" in
    let url_path = "/api-keys" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve the current API key
  
      Retrieve the API key that is used to access this endpoint. *)
  let get_my_api_key client () =
    let op_name = "get_my_api_key" in
    let url_path = "/api-keys/me" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve an API key
  
      Retrieve an API key by its ID. The current user must own this API key. *)
  let get_api_key ~id client () =
    let op_name = "get_api_key" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api-keys/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update an API key
  
      Updates the name and permissions of an API key by its ID. The current user must own this API key. *)
  let update_api_key ~id ~body client () =
    let op_name = "update_api_key" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api-keys/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ApikeyCreate = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        api_key : Apikey.ResponseDto.t;
        secret : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~api_key ~secret () = { api_key; secret }
    
    let api_key t = t.api_key
    let secret t = t.secret
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"APIKeyCreateResponseDto"
        (fun api_key secret -> { api_key; secret })
      |> Jsont.Object.mem "apiKey" Apikey.ResponseDto.jsont ~enc:(fun r -> r.api_key)
      |> Jsont.Object.mem "secret" Jsont.string ~enc:(fun r -> r.secret)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create an API key
  
      Creates a new API key. It will be limited to the permissions specified. *)
  let create_api_key ~body client () =
    let op_name = "create_api_key" in
    let url_path = "/api-keys" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Apikey.CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PeopleUpdate = struct
  module Types = struct
    module Item = struct
      type t = {
        birth_date : string option;  (** Person date of birth.
      Note: the mobile app cannot currently set the birth date to null. *)
        color : string option;
        feature_face_asset_id : string option;  (** Asset is used to get the feature face thumbnail. *)
        id : string;  (** Person id. *)
        is_favorite : bool option;
        is_hidden : bool option;  (** Person visibility *)
        name : string option;  (** Person name. *)
      }
    end
  end
  
  module Item = struct
    include Types.Item
    
    let v ~id ?birth_date ?color ?feature_face_asset_id ?is_favorite ?is_hidden ?name () = { birth_date; color; feature_face_asset_id; id; is_favorite; is_hidden; name }
    
    let birth_date t = t.birth_date
    let color t = t.color
    let feature_face_asset_id t = t.feature_face_asset_id
    let id t = t.id
    let is_favorite t = t.is_favorite
    let is_hidden t = t.is_hidden
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PeopleUpdateItem"
        (fun birth_date color feature_face_asset_id id is_favorite is_hidden name -> { birth_date; color; feature_face_asset_id; id; is_favorite; is_hidden; name })
      |> Jsont.Object.mem "birthDate" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.birth_date)
      |> Jsont.Object.mem "color" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.color)
      |> Jsont.Object.opt_mem "featureFaceAssetId" Jsont.string ~enc:(fun r -> r.feature_face_asset_id)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isHidden" Jsont.bool ~enc:(fun r -> r.is_hidden)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PartnerDirection = struct
  module Types = struct
    module T = struct
      type t = [
        | `Shared_by
        | `Shared_with
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"PartnerDirection"
        ~dec:(function
          | "shared-by" -> `Shared_by
          | "shared-with" -> `Shared_with
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Shared_by -> "shared-by"
          | `Shared_with -> "shared-with")
  end
end

module Onboarding = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        is_onboarded : bool;
      }
    end
  
    module Dto = struct
      type t = {
        is_onboarded : bool;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~is_onboarded () = { is_onboarded }
    
    let is_onboarded t = t.is_onboarded
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OnboardingResponseDto"
        (fun is_onboarded -> { is_onboarded })
      |> Jsont.Object.mem "isOnboarded" Jsont.bool ~enc:(fun r -> r.is_onboarded)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~is_onboarded () = { is_onboarded }
    
    let is_onboarded t = t.is_onboarded
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OnboardingDto"
        (fun is_onboarded -> { is_onboarded })
      |> Jsont.Object.mem "isOnboarded" Jsont.bool ~enc:(fun r -> r.is_onboarded)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve user onboarding
  
      Retrieve the onboarding status of the current user. *)
  let get_user_onboarding client () =
    let op_name = "get_user_onboarding" in
    let url_path = "/users/me/onboarding" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update user onboarding
  
      Update the onboarding status of the current user. *)
  let set_user_onboarding ~body client () =
    let op_name = "set_user_onboarding" in
    let url_path = "/users/me/onboarding" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module OnThisDay = struct
  module Types = struct
    module Dto = struct
      type t = {
        year : float;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~year () = { year }
    
    let year t = t.year
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OnThisDayDto"
        (fun year -> { year })
      |> Jsont.Object.mem "year" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.year)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Ocr = struct
  module Types = struct
    module Config = struct
      type t = {
        enabled : bool;
        max_resolution : int;
        min_detection_score : float;
        min_recognition_score : float;
        model_name : string;
      }
    end
  end
  
  module Config = struct
    include Types.Config
    
    let v ~enabled ~max_resolution ~min_detection_score ~min_recognition_score ~model_name () = { enabled; max_resolution; min_detection_score; min_recognition_score; model_name }
    
    let enabled t = t.enabled
    let max_resolution t = t.max_resolution
    let min_detection_score t = t.min_detection_score
    let min_recognition_score t = t.min_recognition_score
    let model_name t = t.model_name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OcrConfig"
        (fun enabled max_resolution min_detection_score min_recognition_score model_name -> { enabled; max_resolution; min_detection_score; min_recognition_score; model_name })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "maxResolution" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.max_resolution)
      |> Jsont.Object.mem "minDetectionScore" (Openapi.Runtime.validated_float ~minimum:0.1 ~maximum:1. Jsont.number) ~enc:(fun r -> r.min_detection_score)
      |> Jsont.Object.mem "minRecognitionScore" (Openapi.Runtime.validated_float ~minimum:0.1 ~maximum:1. Jsont.number) ~enc:(fun r -> r.min_recognition_score)
      |> Jsont.Object.mem "modelName" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module OauthTokenEndpointAuthMethod = struct
  module Types = struct
    module T = struct
      type t = [
        | `Client_secret_post
        | `Client_secret_basic
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"OAuthTokenEndpointAuthMethod"
        ~dec:(function
          | "client_secret_post" -> `Client_secret_post
          | "client_secret_basic" -> `Client_secret_basic
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Client_secret_post -> "client_secret_post"
          | `Client_secret_basic -> "client_secret_basic")
  end
end

module SystemConfigOauth = struct
  module Types = struct
    module Dto = struct
      type t = {
        auto_launch : bool;
        auto_register : bool;
        button_text : string;
        client_id : string;
        client_secret : string;
        default_storage_quota : int64 option;
        enabled : bool;
        issuer_url : string;
        mobile_override_enabled : bool;
        mobile_redirect_uri : string;
        profile_signing_algorithm : string;
        role_claim : string;
        scope : string;
        signing_algorithm : string;
        storage_label_claim : string;
        storage_quota_claim : string;
        timeout : int;
        token_endpoint_auth_method : OauthTokenEndpointAuthMethod.T.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~auto_launch ~auto_register ~button_text ~client_id ~client_secret ~enabled ~issuer_url ~mobile_override_enabled ~mobile_redirect_uri ~profile_signing_algorithm ~role_claim ~scope ~signing_algorithm ~storage_label_claim ~storage_quota_claim ~timeout ~token_endpoint_auth_method ?default_storage_quota () = { auto_launch; auto_register; button_text; client_id; client_secret; default_storage_quota; enabled; issuer_url; mobile_override_enabled; mobile_redirect_uri; profile_signing_algorithm; role_claim; scope; signing_algorithm; storage_label_claim; storage_quota_claim; timeout; token_endpoint_auth_method }
    
    let auto_launch t = t.auto_launch
    let auto_register t = t.auto_register
    let button_text t = t.button_text
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    let default_storage_quota t = t.default_storage_quota
    let enabled t = t.enabled
    let issuer_url t = t.issuer_url
    let mobile_override_enabled t = t.mobile_override_enabled
    let mobile_redirect_uri t = t.mobile_redirect_uri
    let profile_signing_algorithm t = t.profile_signing_algorithm
    let role_claim t = t.role_claim
    let scope t = t.scope
    let signing_algorithm t = t.signing_algorithm
    let storage_label_claim t = t.storage_label_claim
    let storage_quota_claim t = t.storage_quota_claim
    let timeout t = t.timeout
    let token_endpoint_auth_method t = t.token_endpoint_auth_method
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigOAuthDto"
        (fun auto_launch auto_register button_text client_id client_secret default_storage_quota enabled issuer_url mobile_override_enabled mobile_redirect_uri profile_signing_algorithm role_claim scope signing_algorithm storage_label_claim storage_quota_claim timeout token_endpoint_auth_method -> { auto_launch; auto_register; button_text; client_id; client_secret; default_storage_quota; enabled; issuer_url; mobile_override_enabled; mobile_redirect_uri; profile_signing_algorithm; role_claim; scope; signing_algorithm; storage_label_claim; storage_quota_claim; timeout; token_endpoint_auth_method })
      |> Jsont.Object.mem "autoLaunch" Jsont.bool ~enc:(fun r -> r.auto_launch)
      |> Jsont.Object.mem "autoRegister" Jsont.bool ~enc:(fun r -> r.auto_register)
      |> Jsont.Object.mem "buttonText" Jsont.string ~enc:(fun r -> r.button_text)
      |> Jsont.Object.mem "clientId" Jsont.string ~enc:(fun r -> r.client_id)
      |> Jsont.Object.mem "clientSecret" Jsont.string ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.mem "defaultStorageQuota" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.default_storage_quota)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "issuerUrl" Jsont.string ~enc:(fun r -> r.issuer_url)
      |> Jsont.Object.mem "mobileOverrideEnabled" Jsont.bool ~enc:(fun r -> r.mobile_override_enabled)
      |> Jsont.Object.mem "mobileRedirectUri" Jsont.string ~enc:(fun r -> r.mobile_redirect_uri)
      |> Jsont.Object.mem "profileSigningAlgorithm" Jsont.string ~enc:(fun r -> r.profile_signing_algorithm)
      |> Jsont.Object.mem "roleClaim" Jsont.string ~enc:(fun r -> r.role_claim)
      |> Jsont.Object.mem "scope" Jsont.string ~enc:(fun r -> r.scope)
      |> Jsont.Object.mem "signingAlgorithm" Jsont.string ~enc:(fun r -> r.signing_algorithm)
      |> Jsont.Object.mem "storageLabelClaim" Jsont.string ~enc:(fun r -> r.storage_label_claim)
      |> Jsont.Object.mem "storageQuotaClaim" Jsont.string ~enc:(fun r -> r.storage_quota_claim)
      |> Jsont.Object.mem "timeout" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.timeout)
      |> Jsont.Object.mem "tokenEndpointAuthMethod" OauthTokenEndpointAuthMethod.T.jsont ~enc:(fun r -> r.token_endpoint_auth_method)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module OauthConfig = struct
  module Types = struct
    module Dto = struct
      type t = {
        code_challenge : string option;
        redirect_uri : string;
        state : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~redirect_uri ?code_challenge ?state () = { code_challenge; redirect_uri; state }
    
    let code_challenge t = t.code_challenge
    let redirect_uri t = t.redirect_uri
    let state t = t.state
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OAuthConfigDto"
        (fun code_challenge redirect_uri state -> { code_challenge; redirect_uri; state })
      |> Jsont.Object.opt_mem "codeChallenge" Jsont.string ~enc:(fun r -> r.code_challenge)
      |> Jsont.Object.mem "redirectUri" Jsont.string ~enc:(fun r -> r.redirect_uri)
      |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module OauthAuthorize = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        url : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~url () = { url }
    
    let url t = t.url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OAuthAuthorizeResponseDto"
        (fun url -> { url })
      |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Start OAuth
  
      Initiate the OAuth authorization process. *)
  let start_oauth ~body client () =
    let op_name = "start_oauth" in
    let url_path = "/oauth/authorize" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json OauthConfig.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module OauthCallback = struct
  module Types = struct
    module Dto = struct
      type t = {
        code_verifier : string option;
        state : string option;
        url : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~url ?code_verifier ?state () = { code_verifier; state; url }
    
    let code_verifier t = t.code_verifier
    let state t = t.state
    let url t = t.url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OAuthCallbackDto"
        (fun code_verifier state url -> { code_verifier; state; url })
      |> Jsont.Object.opt_mem "codeVerifier" Jsont.string ~enc:(fun r -> r.code_verifier)
      |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module NotificationUpdateAll = struct
  module Types = struct
    module Dto = struct
      type t = {
        ids : string list;
        read_at : Ptime.t option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~ids ?read_at () = { ids; read_at }
    
    let ids t = t.ids
    let read_at t = t.read_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NotificationUpdateAllDto"
        (fun ids read_at -> { ids; read_at })
      |> Jsont.Object.mem "ids" (Jsont.list Jsont.string) ~enc:(fun r -> r.ids)
      |> Jsont.Object.mem "readAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.read_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module NotificationLevel = struct
  module Types = struct
    module T = struct
      type t = [
        | `Success
        | `Error
        | `Warning
        | `Info
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"NotificationLevel"
        ~dec:(function
          | "success" -> `Success
          | "error" -> `Error
          | "warning" -> `Warning
          | "info" -> `Info
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Success -> "success"
          | `Error -> "error"
          | `Warning -> "warning"
          | `Info -> "info")
  end
end

module Notification = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        read_at : Ptime.t option;
      }
    end
  
    module Type = struct
      type t = [
        | `Job_failed
        | `Backup_failed
        | `System_message
        | `Album_invite
        | `Album_update
        | `Custom
      ]
    end
  
    module Dto = struct
      type t = {
        created_at : Ptime.t;
        data : Jsont.json option;
        description : string option;
        id : string;
        level : NotificationLevel.T.t;
        read_at : Ptime.t option;
        title : string;
        type_ : Type.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        data : Jsont.json option;
        description : string option;
        level : NotificationLevel.T.t option;
        read_at : Ptime.t option;
        title : string;
        type_ : Type.t option;
        user_id : string;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?read_at () = { read_at }
    
    let read_at t = t.read_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NotificationUpdateDto"
        (fun read_at -> { read_at })
      |> Jsont.Object.mem "readAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.read_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"NotificationType"
        ~dec:(function
          | "JobFailed" -> `Job_failed
          | "BackupFailed" -> `Backup_failed
          | "SystemMessage" -> `System_message
          | "AlbumInvite" -> `Album_invite
          | "AlbumUpdate" -> `Album_update
          | "Custom" -> `Custom
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Job_failed -> "JobFailed"
          | `Backup_failed -> "BackupFailed"
          | `System_message -> "SystemMessage"
          | `Album_invite -> "AlbumInvite"
          | `Album_update -> "AlbumUpdate"
          | `Custom -> "Custom")
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~created_at ~id ~level ~title ~type_ ?data ?description ?read_at () = { created_at; data; description; id; level; read_at; title; type_ }
    
    let created_at t = t.created_at
    let data t = t.data
    let description t = t.description
    let id t = t.id
    let level t = t.level
    let read_at t = t.read_at
    let title t = t.title
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NotificationDto"
        (fun created_at data description id level read_at title type_ -> { created_at; data; description; id; level; read_at; title; type_ })
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "data" Jsont.json ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "level" NotificationLevel.T.jsont ~enc:(fun r -> r.level)
      |> Jsont.Object.opt_mem "readAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.read_at)
      |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~title ~user_id ?data ?description ?level ?read_at ?type_ () = { data; description; level; read_at; title; type_; user_id }
    
    let data t = t.data
    let description t = t.description
    let level t = t.level
    let read_at t = t.read_at
    let title t = t.title
    let type_ t = t.type_
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NotificationCreateDto"
        (fun data description level read_at title type_ user_id -> { data; description; level; read_at; title; type_; user_id })
      |> Jsont.Object.opt_mem "data" Jsont.json ~enc:(fun r -> r.data)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "level" NotificationLevel.T.jsont ~enc:(fun r -> r.level)
      |> Jsont.Object.mem "readAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.read_at)
      |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
      |> Jsont.Object.opt_mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a notification
  
      Create a new notification for a specific user. *)
  let create_notification ~body client () =
    let op_name = "create_notification" in
    let url_path = "/admin/notifications" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve notifications
  
      Retrieve a list of notifications. *)
  let get_notifications ?id ?level ?type_ ?unread client () =
    let op_name = "get_notifications" in
    let url_path = "/notifications" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"id" ~value:id; Openapi.Runtime.Query.optional ~key:"level" ~value:level; Openapi.Runtime.Query.optional ~key:"type" ~value:type_; Openapi.Runtime.Query.optional ~key:"unread" ~value:unread]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a notification
  
      Retrieve a specific notification identified by id. *)
  let get_notification ~id client () =
    let op_name = "get_notification" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/notifications/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a notification
  
      Update a specific notification to set its read status. *)
  let update_notification ~id ~body client () =
    let op_name = "update_notification" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/notifications/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module NotificationDeleteAll = struct
  module Types = struct
    module Dto = struct
      type t = {
        ids : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~ids () = { ids }
    
    let ids t = t.ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NotificationDeleteAllDto"
        (fun ids -> { ids })
      |> Jsont.Object.mem "ids" (Jsont.list Jsont.string) ~enc:(fun r -> r.ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MirrorAxis = struct
  module Types = struct
    module T = struct
      (** Axis to mirror along *)
      type t = [
        | `Horizontal
        | `Vertical
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"MirrorAxis"
        ~dec:(function
          | "horizontal" -> `Horizontal
          | "vertical" -> `Vertical
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Horizontal -> "horizontal"
          | `Vertical -> "vertical")
  end
end

module MirrorParameters = struct
  module Types = struct
    module T = struct
      type t = {
        axis : MirrorAxis.T.t;  (** Axis to mirror along *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~axis () = { axis }
    
    let axis t = t.axis
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MirrorParameters"
        (fun axis -> { axis })
      |> Jsont.Object.mem "axis" MirrorAxis.T.jsont ~enc:(fun r -> r.axis)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MergePerson = struct
  module Types = struct
    module Dto = struct
      type t = {
        ids : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~ids () = { ids }
    
    let ids t = t.ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MergePersonDto"
        (fun ids -> { ids })
      |> Jsont.Object.mem "ids" (Jsont.list Jsont.string) ~enc:(fun r -> r.ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MemoryStatistics = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        total : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~total () = { total }
    
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MemoryStatisticsResponseDto"
        (fun total -> { total })
      |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve memories statistics
  
      Retrieve statistics about memories, such as total count and other relevant metrics. 
      @param size Number of memories to return
  *)
  let memories_statistics ?for_ ?is_saved ?is_trashed ?order ?size ?type_ client () =
    let op_name = "memories_statistics" in
    let url_path = "/memories/statistics" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"for" ~value:for_; Openapi.Runtime.Query.optional ~key:"isSaved" ~value:is_saved; Openapi.Runtime.Query.optional ~key:"isTrashed" ~value:is_trashed; Openapi.Runtime.Query.optional ~key:"order" ~value:order; Openapi.Runtime.Query.optional ~key:"size" ~value:size; Openapi.Runtime.Query.optional ~key:"type" ~value:type_]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module MemorySearchOrder = struct
  module Types = struct
    module T = struct
      type t = [
        | `Asc
        | `Desc
        | `Random
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"MemorySearchOrder"
        ~dec:(function
          | "asc" -> `Asc
          | "desc" -> `Desc
          | "random" -> `Random
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Asc -> "asc"
          | `Desc -> "desc"
          | `Random -> "random")
  end
end

module Memories = struct
  module Types = struct
    module Update = struct
      type t = {
        duration : int option;
        enabled : bool option;
      }
    end
  
    module Response = struct
      type t = {
        duration : int;
        enabled : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?duration ?enabled () = { duration; enabled }
    
    let duration t = t.duration
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MemoriesUpdate"
        (fun duration enabled -> { duration; enabled })
      |> Jsont.Object.opt_mem "duration" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(duration=5) ?(enabled=true) () = { duration; enabled }
    
    let duration t = t.duration
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MemoriesResponse"
        (fun duration enabled -> { duration; enabled })
      |> Jsont.Object.mem "duration" Jsont.int ~dec_absent:5 ~enc:(fun r -> r.duration)
      |> Jsont.Object.mem "enabled" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MapReverseGeocode = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        city : string option;
        country : string option;
        state : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?city ?country ?state () = { city; country; state }
    
    let city t = t.city
    let country t = t.country
    let state t = t.state
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MapReverseGeocodeResponseDto"
        (fun city country state -> { city; country; state })
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Reverse geocode coordinates
  
      Retrieve location information (e.g., city, country) for given latitude and longitude coordinates. *)
  let reverse_geocode ~lat ~lon client () =
    let op_name = "reverse_geocode" in
    let url_path = "/map/reverse-geocode" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"lat" ~value:lat; Openapi.Runtime.Query.singleton ~key:"lon" ~value:lon]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module MapMarker = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        city : string option;
        country : string option;
        id : string;
        lat : float;
        lon : float;
        state : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~id ~lat ~lon ?city ?country ?state () = { city; country; id; lat; lon; state }
    
    let city t = t.city
    let country t = t.country
    let id t = t.id
    let lat t = t.lat
    let lon t = t.lon
    let state t = t.state
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MapMarkerResponseDto"
        (fun city country id lat lon state -> { city; country; id; lat; lon; state })
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "lat" Jsont.number ~enc:(fun r -> r.lat)
      |> Jsont.Object.mem "lon" Jsont.number ~enc:(fun r -> r.lon)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve map markers
  
      Retrieve a list of latitude and longitude coordinates for every asset with location data. *)
  let get_map_markers ?file_created_after ?file_created_before ?is_archived ?is_favorite ?with_partners ?with_shared_albums client () =
    let op_name = "get_map_markers" in
    let url_path = "/map/markers" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"fileCreatedAfter" ~value:file_created_after; Openapi.Runtime.Query.optional ~key:"fileCreatedBefore" ~value:file_created_before; Openapi.Runtime.Query.optional ~key:"isArchived" ~value:is_archived; Openapi.Runtime.Query.optional ~key:"isFavorite" ~value:is_favorite; Openapi.Runtime.Query.optional ~key:"withPartners" ~value:with_partners; Openapi.Runtime.Query.optional ~key:"withSharedAlbums" ~value:with_shared_albums]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ManualJobName = struct
  module Types = struct
    module T = struct
      type t = [
        | `Person_cleanup
        | `Tag_cleanup
        | `User_cleanup
        | `Memory_cleanup
        | `Memory_create
        | `Backup_database
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"ManualJobName"
        ~dec:(function
          | "person-cleanup" -> `Person_cleanup
          | "tag-cleanup" -> `Tag_cleanup
          | "user-cleanup" -> `User_cleanup
          | "memory-cleanup" -> `Memory_cleanup
          | "memory-create" -> `Memory_create
          | "backup-database" -> `Backup_database
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Person_cleanup -> "person-cleanup"
          | `Tag_cleanup -> "tag-cleanup"
          | `User_cleanup -> "user-cleanup"
          | `Memory_cleanup -> "memory-cleanup"
          | `Memory_create -> "memory-create"
          | `Backup_database -> "backup-database")
  end
end

module Job = struct
  module Types = struct
    module CreateDto = struct
      type t = {
        name : ManualJobName.T.t;
      }
    end
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~name () = { name }
    
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"JobCreateDto"
        (fun name -> { name })
      |> Jsont.Object.mem "name" ManualJobName.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MaintenanceLogin = struct
  module Types = struct
    module Dto = struct
      type t = {
        token : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?token () = { token }
    
    let token t = t.token
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MaintenanceLoginDto"
        (fun token -> { token })
      |> Jsont.Object.opt_mem "token" Jsont.string ~enc:(fun r -> r.token)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MaintenanceAuth = struct
  module Types = struct
    module Dto = struct
      type t = {
        username : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~username () = { username }
    
    let username t = t.username
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MaintenanceAuthDto"
        (fun username -> { username })
      |> Jsont.Object.mem "username" Jsont.string ~enc:(fun r -> r.username)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Log into maintenance mode
  
      Login with maintenance token or cookie to receive current information and perform further actions. *)
  let maintenance_login ~body client () =
    let op_name = "maintenance_login" in
    let url_path = "/admin/maintenance/login" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json MaintenanceLogin.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module MaintenanceAction = struct
  module Types = struct
    module T = struct
      type t = [
        | `Start
        | `End_
        | `Select_database_restore
        | `Restore_database
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"MaintenanceAction"
        ~dec:(function
          | "start" -> `Start
          | "end" -> `End_
          | "select_database_restore" -> `Select_database_restore
          | "restore_database" -> `Restore_database
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Start -> "start"
          | `End_ -> "end"
          | `Select_database_restore -> "select_database_restore"
          | `Restore_database -> "restore_database")
  end
end

module SetMaintenanceMode = struct
  module Types = struct
    module Dto = struct
      type t = {
        action : MaintenanceAction.T.t;
        restore_backup_filename : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~action ?restore_backup_filename () = { action; restore_backup_filename }
    
    let action t = t.action
    let restore_backup_filename t = t.restore_backup_filename
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SetMaintenanceModeDto"
        (fun action restore_backup_filename -> { action; restore_backup_filename })
      |> Jsont.Object.mem "action" MaintenanceAction.T.jsont ~enc:(fun r -> r.action)
      |> Jsont.Object.opt_mem "restoreBackupFilename" Jsont.string ~enc:(fun r -> r.restore_backup_filename)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MaintenanceStatus = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        action : MaintenanceAction.T.t;
        active : bool;
        error : string option;
        progress : float option;
        task : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~action ~active ?error ?progress ?task () = { action; active; error; progress; task }
    
    let action t = t.action
    let active t = t.active
    let error t = t.error
    let progress t = t.progress
    let task t = t.task
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MaintenanceStatusResponseDto"
        (fun action active error progress task -> { action; active; error; progress; task })
      |> Jsont.Object.mem "action" MaintenanceAction.T.jsont ~enc:(fun r -> r.action)
      |> Jsont.Object.mem "active" Jsont.bool ~enc:(fun r -> r.active)
      |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "progress" Jsont.number ~enc:(fun r -> r.progress)
      |> Jsont.Object.opt_mem "task" Jsont.string ~enc:(fun r -> r.task)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get maintenance mode status
  
      Fetch information about the currently running maintenance action. *)
  let get_maintenance_status client () =
    let op_name = "get_maintenance_status" in
    let url_path = "/admin/maintenance/status" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module MachineLearningAvailabilityChecks = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
        interval : float;
        timeout : float;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled ~interval ~timeout () = { enabled; interval; timeout }
    
    let enabled t = t.enabled
    let interval t = t.interval
    let timeout t = t.timeout
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MachineLearningAvailabilityChecksDto"
        (fun enabled interval timeout -> { enabled; interval; timeout })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "interval" Jsont.number ~enc:(fun r -> r.interval)
      |> Jsont.Object.mem "timeout" Jsont.number ~enc:(fun r -> r.timeout)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Logout = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        redirect_uri : string;
        successful : bool;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~redirect_uri ~successful () = { redirect_uri; successful }
    
    let redirect_uri t = t.redirect_uri
    let successful t = t.successful
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LogoutResponseDto"
        (fun redirect_uri successful -> { redirect_uri; successful })
      |> Jsont.Object.mem "redirectUri" Jsont.string ~enc:(fun r -> r.redirect_uri)
      |> Jsont.Object.mem "successful" Jsont.bool ~enc:(fun r -> r.successful)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Logout
  
      Logout the current user and invalidate the session token. *)
  let logout client () =
    let op_name = "logout" in
    let url_path = "/auth/logout" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module LoginCredential = struct
  module Types = struct
    module Dto = struct
      type t = {
        email : string;
        password : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~email ~password () = { email; password }
    
    let email t = t.email
    let password t = t.password
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LoginCredentialDto"
        (fun email password -> { email; password })
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Login = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        access_token : string;
        is_admin : bool;
        is_onboarded : bool;
        name : string;
        profile_image_path : string;
        should_change_password : bool;
        user_email : string;
        user_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~access_token ~is_admin ~is_onboarded ~name ~profile_image_path ~should_change_password ~user_email ~user_id () = { access_token; is_admin; is_onboarded; name; profile_image_path; should_change_password; user_email; user_id }
    
    let access_token t = t.access_token
    let is_admin t = t.is_admin
    let is_onboarded t = t.is_onboarded
    let name t = t.name
    let profile_image_path t = t.profile_image_path
    let should_change_password t = t.should_change_password
    let user_email t = t.user_email
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LoginResponseDto"
        (fun access_token is_admin is_onboarded name profile_image_path should_change_password user_email user_id -> { access_token; is_admin; is_onboarded; name; profile_image_path; should_change_password; user_email; user_id })
      |> Jsont.Object.mem "accessToken" Jsont.string ~enc:(fun r -> r.access_token)
      |> Jsont.Object.mem "isAdmin" Jsont.bool ~enc:(fun r -> r.is_admin)
      |> Jsont.Object.mem "isOnboarded" Jsont.bool ~enc:(fun r -> r.is_onboarded)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "profileImagePath" Jsont.string ~enc:(fun r -> r.profile_image_path)
      |> Jsont.Object.mem "shouldChangePassword" Jsont.bool ~enc:(fun r -> r.should_change_password)
      |> Jsont.Object.mem "userEmail" Jsont.string ~enc:(fun r -> r.user_email)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Login
  
      Login with username and password and receive a session token. *)
  let login ~body client () =
    let op_name = "login" in
    let url_path = "/auth/login" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json LoginCredential.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Finish OAuth
  
      Complete the OAuth authorization process by exchanging the authorization code for a session token. *)
  let finish_oauth ~body client () =
    let op_name = "finish_oauth" in
    let url_path = "/oauth/callback" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json OauthCallback.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module LogLevel = struct
  module Types = struct
    module T = struct
      type t = [
        | `Verbose
        | `Debug
        | `Log
        | `Warn
        | `Error
        | `Fatal
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"LogLevel"
        ~dec:(function
          | "verbose" -> `Verbose
          | "debug" -> `Debug
          | "log" -> `Log
          | "warn" -> `Warn
          | "error" -> `Error
          | "fatal" -> `Fatal
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Verbose -> "verbose"
          | `Debug -> "debug"
          | `Log -> "log"
          | `Warn -> "warn"
          | `Error -> "error"
          | `Fatal -> "fatal")
  end
end

module SystemConfigLogging = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
        level : LogLevel.T.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled ~level () = { enabled; level }
    
    let enabled t = t.enabled
    let level t = t.level
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigLoggingDto"
        (fun enabled level -> { enabled; level })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "level" LogLevel.T.jsont ~enc:(fun r -> r.level)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module LicenseKey = struct
  module Types = struct
    module Dto = struct
      type t = {
        activation_key : string;
        license_key : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~activation_key ~license_key () = { activation_key; license_key }
    
    let activation_key t = t.activation_key
    let license_key t = t.license_key
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LicenseKeyDto"
        (fun activation_key license_key -> { activation_key; license_key })
      |> Jsont.Object.mem "activationKey" Jsont.string ~enc:(fun r -> r.activation_key)
      |> Jsont.Object.mem "licenseKey" (Openapi.Runtime.validated_string ~pattern:"/IM(SV|CL)(-[\\dA-Za-z]{4}){8}/" Jsont.string) ~enc:(fun r -> r.license_key)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module License = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        activated_at : Ptime.t;
        activation_key : string;
        license_key : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~activated_at ~activation_key ~license_key () = { activated_at; activation_key; license_key }
    
    let activated_at t = t.activated_at
    let activation_key t = t.activation_key
    let license_key t = t.license_key
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LicenseResponseDto"
        (fun activated_at activation_key license_key -> { activated_at; activation_key; license_key })
      |> Jsont.Object.mem "activatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.activated_at)
      |> Jsont.Object.mem "activationKey" Jsont.string ~enc:(fun r -> r.activation_key)
      |> Jsont.Object.mem "licenseKey" (Openapi.Runtime.validated_string ~pattern:"/IM(SV|CL)(-[\\dA-Za-z]{4}){8}/" Jsont.string) ~enc:(fun r -> r.license_key)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get product key
  
      Retrieve information about whether the server currently has a product key registered. *)
  let get_server_license client () =
    let op_name = "get_server_license" in
    let url_path = "/server/license" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Set server product key
  
      Validate and set the server product key if successful. *)
  let set_server_license ~body client () =
    let op_name = "set_server_license" in
    let url_path = "/server/license" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json LicenseKey.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve user product key
  
      Retrieve information about whether the current user has a registered product key. *)
  let get_user_license client () =
    let op_name = "get_user_license" in
    let url_path = "/users/me/license" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Set user product key
  
      Register a product key for the current user. *)
  let set_user_license ~body client () =
    let op_name = "set_user_license" in
    let url_path = "/users/me/license" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json LicenseKey.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module LibraryStats = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        photos : int;
        total : int;
        usage : int64;
        videos : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?(photos=0) ?(total=0) ?(usage=0L) ?(videos=0) () = { photos; total; usage; videos }
    
    let photos t = t.photos
    let total t = t.total
    let usage t = t.usage
    let videos t = t.videos
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LibraryStatsResponseDto"
        (fun photos total usage videos -> { photos; total; usage; videos })
      |> Jsont.Object.mem "photos" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.photos)
      |> Jsont.Object.mem "total" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.total)
      |> Jsont.Object.mem "usage" Jsont.int64 ~dec_absent:0L ~enc:(fun r -> r.usage)
      |> Jsont.Object.mem "videos" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.videos)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve library statistics
  
      Retrieve statistics for a specific external library, including number of videos, images, and storage usage. *)
  let get_library_statistics ~id client () =
    let op_name = "get_library_statistics" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/libraries/{id}/statistics" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module JobSettings = struct
  module Types = struct
    module Dto = struct
      type t = {
        concurrency : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~concurrency () = { concurrency }
    
    let concurrency t = t.concurrency
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"JobSettingsDto"
        (fun concurrency -> { concurrency })
      |> Jsont.Object.mem "concurrency" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.concurrency)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigJob = struct
  module Types = struct
    module Dto = struct
      type t = {
        background_task : JobSettings.Dto.t;
        editor : JobSettings.Dto.t;
        face_detection : JobSettings.Dto.t;
        library : JobSettings.Dto.t;
        metadata_extraction : JobSettings.Dto.t;
        migration : JobSettings.Dto.t;
        notifications : JobSettings.Dto.t;
        ocr : JobSettings.Dto.t;
        search : JobSettings.Dto.t;
        sidecar : JobSettings.Dto.t;
        smart_search : JobSettings.Dto.t;
        thumbnail_generation : JobSettings.Dto.t;
        video_conversion : JobSettings.Dto.t;
        workflow : JobSettings.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~background_task ~editor ~face_detection ~library ~metadata_extraction ~migration ~notifications ~ocr ~search ~sidecar ~smart_search ~thumbnail_generation ~video_conversion ~workflow () = { background_task; editor; face_detection; library; metadata_extraction; migration; notifications; ocr; search; sidecar; smart_search; thumbnail_generation; video_conversion; workflow }
    
    let background_task t = t.background_task
    let editor t = t.editor
    let face_detection t = t.face_detection
    let library t = t.library
    let metadata_extraction t = t.metadata_extraction
    let migration t = t.migration
    let notifications t = t.notifications
    let ocr t = t.ocr
    let search t = t.search
    let sidecar t = t.sidecar
    let smart_search t = t.smart_search
    let thumbnail_generation t = t.thumbnail_generation
    let video_conversion t = t.video_conversion
    let workflow t = t.workflow
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigJobDto"
        (fun background_task editor face_detection library metadata_extraction migration notifications ocr search sidecar smart_search thumbnail_generation video_conversion workflow -> { background_task; editor; face_detection; library; metadata_extraction; migration; notifications; ocr; search; sidecar; smart_search; thumbnail_generation; video_conversion; workflow })
      |> Jsont.Object.mem "backgroundTask" JobSettings.Dto.jsont ~enc:(fun r -> r.background_task)
      |> Jsont.Object.mem "editor" JobSettings.Dto.jsont ~enc:(fun r -> r.editor)
      |> Jsont.Object.mem "faceDetection" JobSettings.Dto.jsont ~enc:(fun r -> r.face_detection)
      |> Jsont.Object.mem "library" JobSettings.Dto.jsont ~enc:(fun r -> r.library)
      |> Jsont.Object.mem "metadataExtraction" JobSettings.Dto.jsont ~enc:(fun r -> r.metadata_extraction)
      |> Jsont.Object.mem "migration" JobSettings.Dto.jsont ~enc:(fun r -> r.migration)
      |> Jsont.Object.mem "notifications" JobSettings.Dto.jsont ~enc:(fun r -> r.notifications)
      |> Jsont.Object.mem "ocr" JobSettings.Dto.jsont ~enc:(fun r -> r.ocr)
      |> Jsont.Object.mem "search" JobSettings.Dto.jsont ~enc:(fun r -> r.search)
      |> Jsont.Object.mem "sidecar" JobSettings.Dto.jsont ~enc:(fun r -> r.sidecar)
      |> Jsont.Object.mem "smartSearch" JobSettings.Dto.jsont ~enc:(fun r -> r.smart_search)
      |> Jsont.Object.mem "thumbnailGeneration" JobSettings.Dto.jsont ~enc:(fun r -> r.thumbnail_generation)
      |> Jsont.Object.mem "videoConversion" JobSettings.Dto.jsont ~enc:(fun r -> r.video_conversion)
      |> Jsont.Object.mem "workflow" JobSettings.Dto.jsont ~enc:(fun r -> r.workflow)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module JobName = struct
  module Types = struct
    module T = struct
      type t = [
        | `Asset_delete
        | `Asset_delete_check
        | `Asset_detect_faces_queue_all
        | `Asset_detect_faces
        | `Asset_detect_duplicates_queue_all
        | `Asset_detect_duplicates
        | `Asset_edit_thumbnail_generation
        | `Asset_encode_video_queue_all
        | `Asset_encode_video
        | `Asset_empty_trash
        | `Asset_extract_metadata_queue_all
        | `Asset_extract_metadata
        | `Asset_file_migration
        | `Asset_generate_thumbnails_queue_all
        | `Asset_generate_thumbnails
        | `Audit_log_cleanup
        | `Audit_table_cleanup
        | `Database_backup
        | `Facial_recognition_queue_all
        | `Facial_recognition
        | `File_delete
        | `File_migration_queue_all
        | `Library_delete_check
        | `Library_delete
        | `Library_remove_asset
        | `Library_scan_assets_queue_all
        | `Library_sync_assets
        | `Library_sync_files_queue_all
        | `Library_sync_files
        | `Library_scan_queue_all
        | `Memory_cleanup
        | `Memory_generate
        | `Notifications_cleanup
        | `Notify_user_signup
        | `Notify_album_invite
        | `Notify_album_update
        | `User_delete
        | `User_delete_check
        | `User_sync_usage
        | `Person_cleanup
        | `Person_file_migration
        | `Person_generate_thumbnail
        | `Session_cleanup
        | `Send_mail
        | `Sidecar_queue_all
        | `Sidecar_check
        | `Sidecar_write
        | `Smart_search_queue_all
        | `Smart_search
        | `Storage_template_migration
        | `Storage_template_migration_single
        | `Tag_cleanup
        | `Version_check
        | `Ocr_queue_all
        | `Ocr
        | `Workflow_run
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"JobName"
        ~dec:(function
          | "AssetDelete" -> `Asset_delete
          | "AssetDeleteCheck" -> `Asset_delete_check
          | "AssetDetectFacesQueueAll" -> `Asset_detect_faces_queue_all
          | "AssetDetectFaces" -> `Asset_detect_faces
          | "AssetDetectDuplicatesQueueAll" -> `Asset_detect_duplicates_queue_all
          | "AssetDetectDuplicates" -> `Asset_detect_duplicates
          | "AssetEditThumbnailGeneration" -> `Asset_edit_thumbnail_generation
          | "AssetEncodeVideoQueueAll" -> `Asset_encode_video_queue_all
          | "AssetEncodeVideo" -> `Asset_encode_video
          | "AssetEmptyTrash" -> `Asset_empty_trash
          | "AssetExtractMetadataQueueAll" -> `Asset_extract_metadata_queue_all
          | "AssetExtractMetadata" -> `Asset_extract_metadata
          | "AssetFileMigration" -> `Asset_file_migration
          | "AssetGenerateThumbnailsQueueAll" -> `Asset_generate_thumbnails_queue_all
          | "AssetGenerateThumbnails" -> `Asset_generate_thumbnails
          | "AuditLogCleanup" -> `Audit_log_cleanup
          | "AuditTableCleanup" -> `Audit_table_cleanup
          | "DatabaseBackup" -> `Database_backup
          | "FacialRecognitionQueueAll" -> `Facial_recognition_queue_all
          | "FacialRecognition" -> `Facial_recognition
          | "FileDelete" -> `File_delete
          | "FileMigrationQueueAll" -> `File_migration_queue_all
          | "LibraryDeleteCheck" -> `Library_delete_check
          | "LibraryDelete" -> `Library_delete
          | "LibraryRemoveAsset" -> `Library_remove_asset
          | "LibraryScanAssetsQueueAll" -> `Library_scan_assets_queue_all
          | "LibrarySyncAssets" -> `Library_sync_assets
          | "LibrarySyncFilesQueueAll" -> `Library_sync_files_queue_all
          | "LibrarySyncFiles" -> `Library_sync_files
          | "LibraryScanQueueAll" -> `Library_scan_queue_all
          | "MemoryCleanup" -> `Memory_cleanup
          | "MemoryGenerate" -> `Memory_generate
          | "NotificationsCleanup" -> `Notifications_cleanup
          | "NotifyUserSignup" -> `Notify_user_signup
          | "NotifyAlbumInvite" -> `Notify_album_invite
          | "NotifyAlbumUpdate" -> `Notify_album_update
          | "UserDelete" -> `User_delete
          | "UserDeleteCheck" -> `User_delete_check
          | "UserSyncUsage" -> `User_sync_usage
          | "PersonCleanup" -> `Person_cleanup
          | "PersonFileMigration" -> `Person_file_migration
          | "PersonGenerateThumbnail" -> `Person_generate_thumbnail
          | "SessionCleanup" -> `Session_cleanup
          | "SendMail" -> `Send_mail
          | "SidecarQueueAll" -> `Sidecar_queue_all
          | "SidecarCheck" -> `Sidecar_check
          | "SidecarWrite" -> `Sidecar_write
          | "SmartSearchQueueAll" -> `Smart_search_queue_all
          | "SmartSearch" -> `Smart_search
          | "StorageTemplateMigration" -> `Storage_template_migration
          | "StorageTemplateMigrationSingle" -> `Storage_template_migration_single
          | "TagCleanup" -> `Tag_cleanup
          | "VersionCheck" -> `Version_check
          | "OcrQueueAll" -> `Ocr_queue_all
          | "Ocr" -> `Ocr
          | "WorkflowRun" -> `Workflow_run
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Asset_delete -> "AssetDelete"
          | `Asset_delete_check -> "AssetDeleteCheck"
          | `Asset_detect_faces_queue_all -> "AssetDetectFacesQueueAll"
          | `Asset_detect_faces -> "AssetDetectFaces"
          | `Asset_detect_duplicates_queue_all -> "AssetDetectDuplicatesQueueAll"
          | `Asset_detect_duplicates -> "AssetDetectDuplicates"
          | `Asset_edit_thumbnail_generation -> "AssetEditThumbnailGeneration"
          | `Asset_encode_video_queue_all -> "AssetEncodeVideoQueueAll"
          | `Asset_encode_video -> "AssetEncodeVideo"
          | `Asset_empty_trash -> "AssetEmptyTrash"
          | `Asset_extract_metadata_queue_all -> "AssetExtractMetadataQueueAll"
          | `Asset_extract_metadata -> "AssetExtractMetadata"
          | `Asset_file_migration -> "AssetFileMigration"
          | `Asset_generate_thumbnails_queue_all -> "AssetGenerateThumbnailsQueueAll"
          | `Asset_generate_thumbnails -> "AssetGenerateThumbnails"
          | `Audit_log_cleanup -> "AuditLogCleanup"
          | `Audit_table_cleanup -> "AuditTableCleanup"
          | `Database_backup -> "DatabaseBackup"
          | `Facial_recognition_queue_all -> "FacialRecognitionQueueAll"
          | `Facial_recognition -> "FacialRecognition"
          | `File_delete -> "FileDelete"
          | `File_migration_queue_all -> "FileMigrationQueueAll"
          | `Library_delete_check -> "LibraryDeleteCheck"
          | `Library_delete -> "LibraryDelete"
          | `Library_remove_asset -> "LibraryRemoveAsset"
          | `Library_scan_assets_queue_all -> "LibraryScanAssetsQueueAll"
          | `Library_sync_assets -> "LibrarySyncAssets"
          | `Library_sync_files_queue_all -> "LibrarySyncFilesQueueAll"
          | `Library_sync_files -> "LibrarySyncFiles"
          | `Library_scan_queue_all -> "LibraryScanQueueAll"
          | `Memory_cleanup -> "MemoryCleanup"
          | `Memory_generate -> "MemoryGenerate"
          | `Notifications_cleanup -> "NotificationsCleanup"
          | `Notify_user_signup -> "NotifyUserSignup"
          | `Notify_album_invite -> "NotifyAlbumInvite"
          | `Notify_album_update -> "NotifyAlbumUpdate"
          | `User_delete -> "UserDelete"
          | `User_delete_check -> "UserDeleteCheck"
          | `User_sync_usage -> "UserSyncUsage"
          | `Person_cleanup -> "PersonCleanup"
          | `Person_file_migration -> "PersonFileMigration"
          | `Person_generate_thumbnail -> "PersonGenerateThumbnail"
          | `Session_cleanup -> "SessionCleanup"
          | `Send_mail -> "SendMail"
          | `Sidecar_queue_all -> "SidecarQueueAll"
          | `Sidecar_check -> "SidecarCheck"
          | `Sidecar_write -> "SidecarWrite"
          | `Smart_search_queue_all -> "SmartSearchQueueAll"
          | `Smart_search -> "SmartSearch"
          | `Storage_template_migration -> "StorageTemplateMigration"
          | `Storage_template_migration_single -> "StorageTemplateMigrationSingle"
          | `Tag_cleanup -> "TagCleanup"
          | `Version_check -> "VersionCheck"
          | `Ocr_queue_all -> "OcrQueueAll"
          | `Ocr -> "Ocr"
          | `Workflow_run -> "WorkflowRun")
  end
end

module QueueJob = struct
  module Types = struct
    module Status = struct
      type t = [
        | `Active
        | `Failed
        | `Completed
        | `Delayed
        | `Waiting
        | `Paused
      ]
    end
  
    module ResponseDto = struct
      type t = {
        data : Jsont.json;
        id : string option;
        name : JobName.T.t;
        timestamp : int;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"QueueJobStatus"
        ~dec:(function
          | "active" -> `Active
          | "failed" -> `Failed
          | "completed" -> `Completed
          | "delayed" -> `Delayed
          | "waiting" -> `Waiting
          | "paused" -> `Paused
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Active -> "active"
          | `Failed -> "failed"
          | `Completed -> "completed"
          | `Delayed -> "delayed"
          | `Waiting -> "waiting"
          | `Paused -> "paused")
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~data ~name ~timestamp ?id () = { data; id; name; timestamp }
    
    let data t = t.data
    let id t = t.id
    let name t = t.name
    let timestamp t = t.timestamp
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"QueueJobResponseDto"
        (fun data id name timestamp -> { data; id; name; timestamp })
      |> Jsont.Object.mem "data" Jsont.json ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" JobName.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "timestamp" Jsont.int ~enc:(fun r -> r.timestamp)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve queue jobs
  
      Retrieves a list of queue jobs from the specified queue. *)
  let get_queue_jobs ~name ?status client () =
    let op_name = "get_queue_jobs" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/queues/{name}/jobs" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"status" ~value:status]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ImageFormat = struct
  module Types = struct
    module T = struct
      type t = [
        | `Jpeg
        | `Webp
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"ImageFormat"
        ~dec:(function
          | "jpeg" -> `Jpeg
          | "webp" -> `Webp
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Jpeg -> "jpeg"
          | `Webp -> "webp")
  end
end

module SystemConfigGeneratedImage = struct
  module Types = struct
    module Dto = struct
      type t = {
        format : ImageFormat.T.t;
        progressive : bool;
        quality : int;
        size : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~format ~quality ~size ?(progressive=false) () = { format; progressive; quality; size }
    
    let format t = t.format
    let progressive t = t.progressive
    let quality t = t.quality
    let size t = t.size
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigGeneratedImageDto"
        (fun format progressive quality size -> { format; progressive; quality; size })
      |> Jsont.Object.mem "format" ImageFormat.T.jsont ~enc:(fun r -> r.format)
      |> Jsont.Object.mem "progressive" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.progressive)
      |> Jsont.Object.mem "quality" (Openapi.Runtime.validated_int ~minimum:1. ~maximum:100. Jsont.int) ~enc:(fun r -> r.quality)
      |> Jsont.Object.mem "size" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.size)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigGeneratedFullsizeImage = struct
  module Types = struct
    module Dto = struct
      type t = {
        enabled : bool;
        format : ImageFormat.T.t;
        progressive : bool;
        quality : int;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~enabled ~format ~quality ?(progressive=false) () = { enabled; format; progressive; quality }
    
    let enabled t = t.enabled
    let format t = t.format
    let progressive t = t.progressive
    let quality t = t.quality
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigGeneratedFullsizeImageDto"
        (fun enabled format progressive quality -> { enabled; format; progressive; quality })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "format" ImageFormat.T.jsont ~enc:(fun r -> r.format)
      |> Jsont.Object.mem "progressive" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.progressive)
      |> Jsont.Object.mem "quality" (Openapi.Runtime.validated_int ~minimum:1. ~maximum:100. Jsont.int) ~enc:(fun r -> r.quality)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Folders = struct
  module Types = struct
    module Update = struct
      type t = {
        enabled : bool option;
        sidebar_web : bool option;
      }
    end
  
    module Response = struct
      type t = {
        enabled : bool;
        sidebar_web : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?enabled ?sidebar_web () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"FoldersUpdate"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.opt_mem "sidebarWeb" Jsont.bool ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(enabled=false) ?(sidebar_web=false) () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"FoldersResponse"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.mem "enabled" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "sidebarWeb" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module FacialRecognition = struct
  module Types = struct
    module Config = struct
      type t = {
        enabled : bool;
        max_distance : float;
        min_faces : int;
        min_score : float;
        model_name : string;
      }
    end
  end
  
  module Config = struct
    include Types.Config
    
    let v ~enabled ~max_distance ~min_faces ~min_score ~model_name () = { enabled; max_distance; min_faces; min_score; model_name }
    
    let enabled t = t.enabled
    let max_distance t = t.max_distance
    let min_faces t = t.min_faces
    let min_score t = t.min_score
    let model_name t = t.model_name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"FacialRecognitionConfig"
        (fun enabled max_distance min_faces min_score model_name -> { enabled; max_distance; min_faces; min_score; model_name })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "maxDistance" (Openapi.Runtime.validated_float ~minimum:0.1 ~maximum:2. Jsont.number) ~enc:(fun r -> r.max_distance)
      |> Jsont.Object.mem "minFaces" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.min_faces)
      |> Jsont.Object.mem "minScore" (Openapi.Runtime.validated_float ~minimum:0.1 ~maximum:1. Jsont.number) ~enc:(fun r -> r.min_score)
      |> Jsont.Object.mem "modelName" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Face = struct
  module Types = struct
    module Dto = struct
      type t = {
        id : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"FaceDto"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Exif = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        city : string option;
        country : string option;
        date_time_original : Ptime.t option;
        description : string option;
        exif_image_height : float option;
        exif_image_width : float option;
        exposure_time : string option;
        f_number : float option;
        file_size_in_byte : int64 option;
        focal_length : float option;
        iso : float option;
        latitude : float option;
        lens_model : string option;
        longitude : float option;
        make : string option;
        model : string option;
        modify_date : Ptime.t option;
        orientation : string option;
        projection_type : string option;
        rating : float option;
        state : string option;
        time_zone : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ?(city=None) ?(country=None) ?(date_time_original=None) ?(description=None) ?(exif_image_height=None) ?(exif_image_width=None) ?(exposure_time=None) ?(f_number=None) ?(file_size_in_byte=None) ?(focal_length=None) ?(iso=None) ?(latitude=None) ?(lens_model=None) ?(longitude=None) ?(make=None) ?(model=None) ?(modify_date=None) ?(orientation=None) ?(projection_type=None) ?(rating=None) ?(state=None) ?(time_zone=None) () = { city; country; date_time_original; description; exif_image_height; exif_image_width; exposure_time; f_number; file_size_in_byte; focal_length; iso; latitude; lens_model; longitude; make; model; modify_date; orientation; projection_type; rating; state; time_zone }
    
    let city t = t.city
    let country t = t.country
    let date_time_original t = t.date_time_original
    let description t = t.description
    let exif_image_height t = t.exif_image_height
    let exif_image_width t = t.exif_image_width
    let exposure_time t = t.exposure_time
    let f_number t = t.f_number
    let file_size_in_byte t = t.file_size_in_byte
    let focal_length t = t.focal_length
    let iso t = t.iso
    let latitude t = t.latitude
    let lens_model t = t.lens_model
    let longitude t = t.longitude
    let make t = t.make
    let model t = t.model
    let modify_date t = t.modify_date
    let orientation t = t.orientation
    let projection_type t = t.projection_type
    let rating t = t.rating
    let state t = t.state
    let time_zone t = t.time_zone
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ExifResponseDto"
        (fun city country date_time_original description exif_image_height exif_image_width exposure_time f_number file_size_in_byte focal_length iso latitude lens_model longitude make model modify_date orientation projection_type rating state time_zone -> { city; country; date_time_original; description; exif_image_height; exif_image_width; exposure_time; f_number; file_size_in_byte; focal_length; iso; latitude; lens_model; longitude; make; model; modify_date; orientation; projection_type; rating; state; time_zone })
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.mem "dateTimeOriginal" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.date_time_original)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "exifImageHeight" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.exif_image_height)
      |> Jsont.Object.mem "exifImageWidth" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.exif_image_width)
      |> Jsont.Object.mem "exposureTime" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.exposure_time)
      |> Jsont.Object.mem "fNumber" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.f_number)
      |> Jsont.Object.mem "fileSizeInByte" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.file_size_in_byte)
      |> Jsont.Object.mem "focalLength" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.focal_length)
      |> Jsont.Object.mem "iso" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.iso)
      |> Jsont.Object.mem "latitude" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.latitude)
      |> Jsont.Object.mem "lensModel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.lens_model)
      |> Jsont.Object.mem "longitude" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.longitude)
      |> Jsont.Object.mem "make" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.make)
      |> Jsont.Object.mem "model" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.model)
      |> Jsont.Object.mem "modifyDate" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.modify_date)
      |> Jsont.Object.mem "orientation" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.orientation)
      |> Jsont.Object.mem "projectionType" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.projection_type)
      |> Jsont.Object.mem "rating" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.rating)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "timeZone" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.time_zone)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module EmailNotifications = struct
  module Types = struct
    module Update = struct
      type t = {
        album_invite : bool option;
        album_update : bool option;
        enabled : bool option;
      }
    end
  
    module Response = struct
      type t = {
        album_invite : bool;
        album_update : bool;
        enabled : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?album_invite ?album_update ?enabled () = { album_invite; album_update; enabled }
    
    let album_invite t = t.album_invite
    let album_update t = t.album_update
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"EmailNotificationsUpdate"
        (fun album_invite album_update enabled -> { album_invite; album_update; enabled })
      |> Jsont.Object.opt_mem "albumInvite" Jsont.bool ~enc:(fun r -> r.album_invite)
      |> Jsont.Object.opt_mem "albumUpdate" Jsont.bool ~enc:(fun r -> r.album_update)
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ~album_invite ~album_update ~enabled () = { album_invite; album_update; enabled }
    
    let album_invite t = t.album_invite
    let album_update t = t.album_update
    let enabled t = t.enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"EmailNotificationsResponse"
        (fun album_invite album_update enabled -> { album_invite; album_update; enabled })
      |> Jsont.Object.mem "albumInvite" Jsont.bool ~enc:(fun r -> r.album_invite)
      |> Jsont.Object.mem "albumUpdate" Jsont.bool ~enc:(fun r -> r.album_update)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DuplicateDetection = struct
  module Types = struct
    module Config = struct
      type t = {
        enabled : bool;
        max_distance : float;
      }
    end
  end
  
  module Config = struct
    include Types.Config
    
    let v ~enabled ~max_distance () = { enabled; max_distance }
    
    let enabled t = t.enabled
    let max_distance t = t.max_distance
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DuplicateDetectionConfig"
        (fun enabled max_distance -> { enabled; max_distance })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "maxDistance" (Openapi.Runtime.validated_float ~minimum:0.001 ~maximum:0.1 Jsont.number) ~enc:(fun r -> r.max_distance)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DownloadInfo = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_id : string option;
        archive_size : int option;
        asset_ids : string list option;
        user_id : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?album_id ?archive_size ?asset_ids ?user_id () = { album_id; archive_size; asset_ids; user_id }
    
    let album_id t = t.album_id
    let archive_size t = t.archive_size
    let asset_ids t = t.asset_ids
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DownloadInfoDto"
        (fun album_id archive_size asset_ids user_id -> { album_id; archive_size; asset_ids; user_id })
      |> Jsont.Object.opt_mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.opt_mem "archiveSize" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.archive_size)
      |> Jsont.Object.opt_mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.opt_mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DownloadArchive = struct
  module Types = struct
    module Info = struct
      type t = {
        asset_ids : string list;
        size : int;
      }
    end
  end
  
  module Info = struct
    include Types.Info
    
    let v ~asset_ids ~size () = { asset_ids; size }
    
    let asset_ids t = t.asset_ids
    let size t = t.size
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DownloadArchiveInfo"
        (fun asset_ids size -> { asset_ids; size })
      |> Jsont.Object.mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Download = struct
  module Types = struct
    module Update = struct
      type t = {
        archive_size : int option;
        include_embedded_videos : bool option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        archives : DownloadArchive.Info.t list;
        total_size : int;
      }
    end
  
    module Response = struct
      type t = {
        archive_size : int;
        include_embedded_videos : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?archive_size ?include_embedded_videos () = { archive_size; include_embedded_videos }
    
    let archive_size t = t.archive_size
    let include_embedded_videos t = t.include_embedded_videos
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DownloadUpdate"
        (fun archive_size include_embedded_videos -> { archive_size; include_embedded_videos })
      |> Jsont.Object.opt_mem "archiveSize" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.archive_size)
      |> Jsont.Object.opt_mem "includeEmbeddedVideos" Jsont.bool ~enc:(fun r -> r.include_embedded_videos)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~archives ~total_size () = { archives; total_size }
    
    let archives t = t.archives
    let total_size t = t.total_size
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DownloadResponseDto"
        (fun archives total_size -> { archives; total_size })
      |> Jsont.Object.mem "archives" (Jsont.list DownloadArchive.Info.jsont) ~enc:(fun r -> r.archives)
      |> Jsont.Object.mem "totalSize" Jsont.int ~enc:(fun r -> r.total_size)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ~archive_size ?(include_embedded_videos=false) () = { archive_size; include_embedded_videos }
    
    let archive_size t = t.archive_size
    let include_embedded_videos t = t.include_embedded_videos
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DownloadResponse"
        (fun archive_size include_embedded_videos -> { archive_size; include_embedded_videos })
      |> Jsont.Object.mem "archiveSize" Jsont.int ~enc:(fun r -> r.archive_size)
      |> Jsont.Object.mem "includeEmbeddedVideos" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.include_embedded_videos)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve download information
  
      Retrieve information about how to request a download for the specified assets or album. The response includes groups of assets that can be downloaded together. *)
  let get_download_info ?key ?slug ~body client () =
    let op_name = "get_download_info" in
    let url_path = "/download/info" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json DownloadInfo.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module DatabaseBackupUpload = struct
  module Types = struct
    module Dto = struct
      type t = {
        file : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?file () = { file }
    
    let file t = t.file
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DatabaseBackupUploadDto"
        (fun file -> { file })
      |> Jsont.Object.opt_mem "file" Jsont.string ~enc:(fun r -> r.file)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DatabaseBackupDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        backups : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~backups () = { backups }
    
    let backups t = t.backups
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DatabaseBackupDeleteDto"
        (fun backups -> { backups })
      |> Jsont.Object.mem "backups" (Jsont.list Jsont.string) ~enc:(fun r -> r.backups)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DatabaseBackup = struct
  module Types = struct
    module Dto = struct
      type t = {
        filename : string;
        filesize : float;
      }
    end
  
    module Config = struct
      type t = {
        cron_expression : string;
        enabled : bool;
        keep_last_amount : float;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~filename ~filesize () = { filename; filesize }
    
    let filename t = t.filename
    let filesize t = t.filesize
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DatabaseBackupDto"
        (fun filename filesize -> { filename; filesize })
      |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
      |> Jsont.Object.mem "filesize" Jsont.number ~enc:(fun r -> r.filesize)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Config = struct
    include Types.Config
    
    let v ~cron_expression ~enabled ~keep_last_amount () = { cron_expression; enabled; keep_last_amount }
    
    let cron_expression t = t.cron_expression
    let enabled t = t.enabled
    let keep_last_amount t = t.keep_last_amount
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DatabaseBackupConfig"
        (fun cron_expression enabled keep_last_amount -> { cron_expression; enabled; keep_last_amount })
      |> Jsont.Object.mem "cronExpression" Jsont.string ~enc:(fun r -> r.cron_expression)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "keepLastAmount" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.keep_last_amount)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigBackups = struct
  module Types = struct
    module Dto = struct
      type t = {
        database : DatabaseBackup.Config.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~database () = { database }
    
    let database t = t.database
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigBackupsDto"
        (fun database -> { database })
      |> Jsont.Object.mem "database" DatabaseBackup.Config.jsont ~enc:(fun r -> r.database)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DatabaseBackupList = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        backups : DatabaseBackup.Dto.t list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~backups () = { backups }
    
    let backups t = t.backups
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DatabaseBackupListResponseDto"
        (fun backups -> { backups })
      |> Jsont.Object.mem "backups" (Jsont.list DatabaseBackup.Dto.jsont) ~enc:(fun r -> r.backups)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List database backups
  
      Get the list of the successful and failed backups *)
  let list_database_backups client () =
    let op_name = "list_database_backups" in
    let url_path = "/admin/database-backups" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module CropParameters = struct
  module Types = struct
    module T = struct
      type t = {
        height : float;  (** Height of the crop *)
        width : float;  (** Width of the crop *)
        x : float;  (** Top-Left X coordinate of crop *)
        y : float;  (** Top-Left Y coordinate of crop *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~height ~width ~x ~y () = { height; width; x; y }
    
    let height t = t.height
    let width t = t.width
    let x t = t.x
    let y t = t.y
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CropParameters"
        (fun height width x y -> { height; width; x; y })
      |> Jsont.Object.mem "height" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.height)
      |> Jsont.Object.mem "width" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.width)
      |> Jsont.Object.mem "x" (Openapi.Runtime.validated_float ~minimum:0. Jsont.number) ~enc:(fun r -> r.x)
      |> Jsont.Object.mem "y" (Openapi.Runtime.validated_float ~minimum:0. Jsont.number) ~enc:(fun r -> r.y)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CreateProfileImage = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        profile_changed_at : Ptime.t;
        profile_image_path : string;
        user_id : string;
      }
    end
  
    module Dto = struct
      type t = {
        file : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~profile_changed_at ~profile_image_path ~user_id () = { profile_changed_at; profile_image_path; user_id }
    
    let profile_changed_at t = t.profile_changed_at
    let profile_image_path t = t.profile_image_path
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CreateProfileImageResponseDto"
        (fun profile_changed_at profile_image_path user_id -> { profile_changed_at; profile_image_path; user_id })
      |> Jsont.Object.mem "profileChangedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.profile_changed_at)
      |> Jsont.Object.mem "profileImagePath" Jsont.string ~enc:(fun r -> r.profile_image_path)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~file () = { file }
    
    let file t = t.file
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CreateProfileImageDto"
        (fun file -> { file })
      |> Jsont.Object.mem "file" Jsont.string ~enc:(fun r -> r.file)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create user profile image
  
      Upload and set a new profile image for the current user. *)
  let create_profile_image client () =
    let op_name = "create_profile_image" in
    let url_path = "/users/profile-image" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module CreateLibrary = struct
  module Types = struct
    module Dto = struct
      type t = {
        exclusion_patterns : string list option;
        import_paths : string list option;
        name : string option;
        owner_id : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~owner_id ?exclusion_patterns ?import_paths ?name () = { exclusion_patterns; import_paths; name; owner_id }
    
    let exclusion_patterns t = t.exclusion_patterns
    let import_paths t = t.import_paths
    let name t = t.name
    let owner_id t = t.owner_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CreateLibraryDto"
        (fun exclusion_patterns import_paths name owner_id -> { exclusion_patterns; import_paths; name; owner_id })
      |> Jsont.Object.opt_mem "exclusionPatterns" (Openapi.Runtime.validated_list ~max_items:128 ~unique_items:true Jsont.string) ~enc:(fun r -> r.exclusion_patterns)
      |> Jsont.Object.opt_mem "importPaths" (Openapi.Runtime.validated_list ~max_items:128 ~unique_items:true Jsont.string) ~enc:(fun r -> r.import_paths)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Library = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_count : int;
        created_at : Ptime.t;
        exclusion_patterns : string list;
        id : string;
        import_paths : string list;
        name : string;
        owner_id : string;
        refreshed_at : Ptime.t option;
        updated_at : Ptime.t;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~asset_count ~created_at ~exclusion_patterns ~id ~import_paths ~name ~owner_id ~updated_at ?refreshed_at () = { asset_count; created_at; exclusion_patterns; id; import_paths; name; owner_id; refreshed_at; updated_at }
    
    let asset_count t = t.asset_count
    let created_at t = t.created_at
    let exclusion_patterns t = t.exclusion_patterns
    let id t = t.id
    let import_paths t = t.import_paths
    let name t = t.name
    let owner_id t = t.owner_id
    let refreshed_at t = t.refreshed_at
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LibraryResponseDto"
        (fun asset_count created_at exclusion_patterns id import_paths name owner_id refreshed_at updated_at -> { asset_count; created_at; exclusion_patterns; id; import_paths; name; owner_id; refreshed_at; updated_at })
      |> Jsont.Object.mem "assetCount" Jsont.int ~enc:(fun r -> r.asset_count)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "exclusionPatterns" (Jsont.list Jsont.string) ~enc:(fun r -> r.exclusion_patterns)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "importPaths" (Jsont.list Jsont.string) ~enc:(fun r -> r.import_paths)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "refreshedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.refreshed_at)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve libraries
  
      Retrieve a list of external libraries. *)
  let get_all_libraries client () =
    let op_name = "get_all_libraries" in
    let url_path = "/libraries" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a library
  
      Create a new external library. *)
  let create_library ~body client () =
    let op_name = "create_library" in
    let url_path = "/libraries" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateLibrary.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a library
  
      Retrieve an external library by its ID. *)
  let get_library ~id client () =
    let op_name = "get_library" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/libraries/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a library
  
      Update an existing external library. *)
  let update_library ~id ~body client () =
    let op_name = "update_library" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/libraries/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateLibrary.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Cqmode = struct
  module Types = struct
    module T = struct
      type t = [
        | `Auto
        | `Cqp
        | `Icq
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"CQMode"
        ~dec:(function
          | "auto" -> `Auto
          | "cqp" -> `Cqp
          | "icq" -> `Icq
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Auto -> "auto"
          | `Cqp -> "cqp"
          | `Icq -> "icq")
  end
end

module ContributorCount = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_count : int;
        user_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~asset_count ~user_id () = { asset_count; user_id }
    
    let asset_count t = t.asset_count
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ContributorCountResponseDto"
        (fun asset_count user_id -> { asset_count; user_id })
      |> Jsont.Object.mem "assetCount" Jsont.int ~enc:(fun r -> r.asset_count)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Colorspace = struct
  module Types = struct
    module T = struct
      type t = [
        | `Srgb
        | `P3
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"Colorspace"
        ~dec:(function
          | "srgb" -> `Srgb
          | "p3" -> `P3
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Srgb -> "srgb"
          | `P3 -> "p3")
  end
end

module SystemConfigImage = struct
  module Types = struct
    module Dto = struct
      type t = {
        colorspace : Colorspace.T.t;
        extract_embedded : bool;
        fullsize : SystemConfigGeneratedFullsizeImage.Dto.t;
        preview : SystemConfigGeneratedImage.Dto.t;
        thumbnail : SystemConfigGeneratedImage.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~colorspace ~extract_embedded ~fullsize ~preview ~thumbnail () = { colorspace; extract_embedded; fullsize; preview; thumbnail }
    
    let colorspace t = t.colorspace
    let extract_embedded t = t.extract_embedded
    let fullsize t = t.fullsize
    let preview t = t.preview
    let thumbnail t = t.thumbnail
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigImageDto"
        (fun colorspace extract_embedded fullsize preview thumbnail -> { colorspace; extract_embedded; fullsize; preview; thumbnail })
      |> Jsont.Object.mem "colorspace" Colorspace.T.jsont ~enc:(fun r -> r.colorspace)
      |> Jsont.Object.mem "extractEmbedded" Jsont.bool ~enc:(fun r -> r.extract_embedded)
      |> Jsont.Object.mem "fullsize" SystemConfigGeneratedFullsizeImage.Dto.jsont ~enc:(fun r -> r.fullsize)
      |> Jsont.Object.mem "preview" SystemConfigGeneratedImage.Dto.jsont ~enc:(fun r -> r.preview)
      |> Jsont.Object.mem "thumbnail" SystemConfigGeneratedImage.Dto.jsont ~enc:(fun r -> r.thumbnail)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Clip = struct
  module Types = struct
    module Config = struct
      type t = {
        enabled : bool;
        model_name : string;
      }
    end
  end
  
  module Config = struct
    include Types.Config
    
    let v ~enabled ~model_name () = { enabled; model_name }
    
    let enabled t = t.enabled
    let model_name t = t.model_name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CLIPConfig"
        (fun enabled model_name -> { enabled; model_name })
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "modelName" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfigMachineLearning = struct
  module Types = struct
    module Dto = struct
      type t = {
        availability_checks : MachineLearningAvailabilityChecks.Dto.t;
        clip : Clip.Config.t;
        duplicate_detection : DuplicateDetection.Config.t;
        enabled : bool;
        facial_recognition : FacialRecognition.Config.t;
        ocr : Ocr.Config.t;
        urls : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~availability_checks ~clip ~duplicate_detection ~enabled ~facial_recognition ~ocr ~urls () = { availability_checks; clip; duplicate_detection; enabled; facial_recognition; ocr; urls }
    
    let availability_checks t = t.availability_checks
    let clip t = t.clip
    let duplicate_detection t = t.duplicate_detection
    let enabled t = t.enabled
    let facial_recognition t = t.facial_recognition
    let ocr t = t.ocr
    let urls t = t.urls
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigMachineLearningDto"
        (fun availability_checks clip duplicate_detection enabled facial_recognition ocr urls -> { availability_checks; clip; duplicate_detection; enabled; facial_recognition; ocr; urls })
      |> Jsont.Object.mem "availabilityChecks" MachineLearningAvailabilityChecks.Dto.jsont ~enc:(fun r -> r.availability_checks)
      |> Jsont.Object.mem "clip" Clip.Config.jsont ~enc:(fun r -> r.clip)
      |> Jsont.Object.mem "duplicateDetection" DuplicateDetection.Config.jsont ~enc:(fun r -> r.duplicate_detection)
      |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "facialRecognition" FacialRecognition.Config.jsont ~enc:(fun r -> r.facial_recognition)
      |> Jsont.Object.mem "ocr" Ocr.Config.jsont ~enc:(fun r -> r.ocr)
      |> Jsont.Object.mem "urls" (Openapi.Runtime.validated_list ~min_items:1 Jsont.string) ~enc:(fun r -> r.urls)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CheckExistingAssets = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        existing_ids : string list;
      }
    end
  
    module Dto = struct
      type t = {
        device_asset_ids : string list;
        device_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~existing_ids () = { existing_ids }
    
    let existing_ids t = t.existing_ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CheckExistingAssetsResponseDto"
        (fun existing_ids -> { existing_ids })
      |> Jsont.Object.mem "existingIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.existing_ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~device_asset_ids ~device_id () = { device_asset_ids; device_id }
    
    let device_asset_ids t = t.device_asset_ids
    let device_id t = t.device_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CheckExistingAssetsDto"
        (fun device_asset_ids device_id -> { device_asset_ids; device_id })
      |> Jsont.Object.mem "deviceAssetIds" (Openapi.Runtime.validated_list ~min_items:1 Jsont.string) ~enc:(fun r -> r.device_asset_ids)
      |> Jsont.Object.mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Check existing assets
  
      Checks if multiple assets exist on the server and returns all existing - used by background backup *)
  let check_existing_assets ~body client () =
    let op_name = "check_existing_assets" in
    let url_path = "/assets/exist" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ChangePassword = struct
  module Types = struct
    module Dto = struct
      type t = {
        invalidate_sessions : bool;
        new_password : string;
        password : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~new_password ~password ?(invalidate_sessions=false) () = { invalidate_sessions; new_password; password }
    
    let invalidate_sessions t = t.invalidate_sessions
    let new_password t = t.new_password
    let password t = t.password
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ChangePasswordDto"
        (fun invalidate_sessions new_password password -> { invalidate_sessions; new_password; password })
      |> Jsont.Object.mem "invalidateSessions" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.invalidate_sessions)
      |> Jsont.Object.mem "newPassword" (Openapi.Runtime.validated_string ~min_length:8 Jsont.string) ~enc:(fun r -> r.new_password)
      |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserAdmin = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        avatar_color : UserAvatarColor.T.t option;
        email : string option;
        is_admin : bool option;
        name : string option;
        password : string option;
        pin_code : string option;
        quota_size_in_bytes : int64 option;
        should_change_password : bool option;
        storage_label : string option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        avatar_color : UserAvatarColor.T.t;
        created_at : Ptime.t;
        deleted_at : Ptime.t option;
        email : string;
        id : string;
        is_admin : bool;
        license : UserLicense.T.t;
        name : string;
        oauth_id : string;
        profile_changed_at : Ptime.t;
        profile_image_path : string;
        quota_size_in_bytes : int64 option;
        quota_usage_in_bytes : int64 option;
        should_change_password : bool;
        status : User.Status.t;
        storage_label : string option;
        updated_at : Ptime.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        avatar_color : UserAvatarColor.T.t option;
        email : string;
        is_admin : bool option;
        name : string;
        notify : bool option;
        password : string;
        quota_size_in_bytes : int64 option;
        should_change_password : bool option;
        storage_label : string option;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?avatar_color ?email ?is_admin ?name ?password ?pin_code ?quota_size_in_bytes ?should_change_password ?storage_label () = { avatar_color; email; is_admin; name; password; pin_code; quota_size_in_bytes; should_change_password; storage_label }
    
    let avatar_color t = t.avatar_color
    let email t = t.email
    let is_admin t = t.is_admin
    let name t = t.name
    let password t = t.password
    let pin_code t = t.pin_code
    let quota_size_in_bytes t = t.quota_size_in_bytes
    let should_change_password t = t.should_change_password
    let storage_label t = t.storage_label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserAdminUpdateDto"
        (fun avatar_color email is_admin name password pin_code quota_size_in_bytes should_change_password storage_label -> { avatar_color; email; is_admin; name; password; pin_code; quota_size_in_bytes; should_change_password; storage_label })
      |> Jsont.Object.opt_mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "isAdmin" Jsont.bool ~enc:(fun r -> r.is_admin)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "pinCode" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.mem "quotaSizeInBytes" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.quota_size_in_bytes)
      |> Jsont.Object.opt_mem "shouldChangePassword" Jsont.bool ~enc:(fun r -> r.should_change_password)
      |> Jsont.Object.mem "storageLabel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.storage_label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~avatar_color ~created_at ~email ~id ~is_admin ~license ~name ~oauth_id ~profile_changed_at ~profile_image_path ~should_change_password ~status ~updated_at ?deleted_at ?quota_size_in_bytes ?quota_usage_in_bytes ?storage_label () = { avatar_color; created_at; deleted_at; email; id; is_admin; license; name; oauth_id; profile_changed_at; profile_image_path; quota_size_in_bytes; quota_usage_in_bytes; should_change_password; status; storage_label; updated_at }
    
    let avatar_color t = t.avatar_color
    let created_at t = t.created_at
    let deleted_at t = t.deleted_at
    let email t = t.email
    let id t = t.id
    let is_admin t = t.is_admin
    let license t = t.license
    let name t = t.name
    let oauth_id t = t.oauth_id
    let profile_changed_at t = t.profile_changed_at
    let profile_image_path t = t.profile_image_path
    let quota_size_in_bytes t = t.quota_size_in_bytes
    let quota_usage_in_bytes t = t.quota_usage_in_bytes
    let should_change_password t = t.should_change_password
    let status t = t.status
    let storage_label t = t.storage_label
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserAdminResponseDto"
        (fun avatar_color created_at deleted_at email id is_admin license name oauth_id profile_changed_at profile_image_path quota_size_in_bytes quota_usage_in_bytes should_change_password status storage_label updated_at -> { avatar_color; created_at; deleted_at; email; id; is_admin; license; name; oauth_id; profile_changed_at; profile_image_path; quota_size_in_bytes; quota_usage_in_bytes; should_change_password; status; storage_label; updated_at })
      |> Jsont.Object.mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "deletedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isAdmin" Jsont.bool ~enc:(fun r -> r.is_admin)
      |> Jsont.Object.mem "license" UserLicense.T.jsont ~enc:(fun r -> r.license)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "oauthId" Jsont.string ~enc:(fun r -> r.oauth_id)
      |> Jsont.Object.mem "profileChangedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.profile_changed_at)
      |> Jsont.Object.mem "profileImagePath" Jsont.string ~enc:(fun r -> r.profile_image_path)
      |> Jsont.Object.mem "quotaSizeInBytes" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.quota_size_in_bytes)
      |> Jsont.Object.mem "quotaUsageInBytes" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.quota_usage_in_bytes)
      |> Jsont.Object.mem "shouldChangePassword" Jsont.bool ~enc:(fun r -> r.should_change_password)
      |> Jsont.Object.mem "status" User.Status.jsont ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "storageLabel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.storage_label)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~email ~name ~password ?avatar_color ?is_admin ?notify ?quota_size_in_bytes ?should_change_password ?storage_label () = { avatar_color; email; is_admin; name; notify; password; quota_size_in_bytes; should_change_password; storage_label }
    
    let avatar_color t = t.avatar_color
    let email t = t.email
    let is_admin t = t.is_admin
    let name t = t.name
    let notify t = t.notify
    let password t = t.password
    let quota_size_in_bytes t = t.quota_size_in_bytes
    let should_change_password t = t.should_change_password
    let storage_label t = t.storage_label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserAdminCreateDto"
        (fun avatar_color email is_admin name notify password quota_size_in_bytes should_change_password storage_label -> { avatar_color; email; is_admin; name; notify; password; quota_size_in_bytes; should_change_password; storage_label })
      |> Jsont.Object.opt_mem "avatarColor" UserAvatarColor.T.jsont ~enc:(fun r -> r.avatar_color)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "isAdmin" Jsont.bool ~enc:(fun r -> r.is_admin)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "notify" Jsont.bool ~enc:(fun r -> r.notify)
      |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "quotaSizeInBytes" (Openapi.Runtime.nullable_any Jsont.int64)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.quota_size_in_bytes)
      |> Jsont.Object.opt_mem "shouldChangePassword" Jsont.bool ~enc:(fun r -> r.should_change_password)
      |> Jsont.Object.mem "storageLabel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.storage_label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Search users
  
      Search for users. *)
  let search_users_admin ?id ?with_deleted client () =
    let op_name = "search_users_admin" in
    let url_path = "/admin/users" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"id" ~value:id; Openapi.Runtime.Query.optional ~key:"withDeleted" ~value:with_deleted]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a user
  
      Create a new user. *)
  let create_user_admin ~body client () =
    let op_name = "create_user_admin" in
    let url_path = "/admin/users" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a user
  
      Retrieve  a specific user by their ID. *)
  let get_user_admin ~id client () =
    let op_name = "get_user_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a user
  
      Update an existing user. *)
  let update_user_admin ~id ~body client () =
    let op_name = "update_user_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a user
  
      Delete a user. *)
  let delete_user_admin ~id client () =
    let op_name = "delete_user_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Restore a deleted user
  
      Restore a previously deleted user. *)
  let restore_user_admin ~id client () =
    let op_name = "restore_user_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}/restore" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Register admin
  
      Create the first admin user in the system. *)
  let sign_up_admin ~body client () =
    let op_name = "sign_up_admin" in
    let url_path = "/auth/admin-sign-up" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SignUp.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Change password
  
      Change the password of the current user. *)
  let change_password ~body client () =
    let op_name = "change_password" in
    let url_path = "/auth/change-password" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json ChangePassword.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Link OAuth account
  
      Link an OAuth account to the authenticated user. *)
  let link_oauth_account ~body client () =
    let op_name = "link_oauth_account" in
    let url_path = "/oauth/link" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json OauthCallback.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unlink OAuth account
  
      Unlink the OAuth account from the authenticated user. *)
  let unlink_oauth_account client () =
    let op_name = "unlink_oauth_account" in
    let url_path = "/oauth/unlink" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get current user
  
      Retrieve information about the user making the API request. *)
  let get_my_user client () =
    let op_name = "get_my_user" in
    let url_path = "/users/me" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update current user
  
      Update the current user making teh API request. *)
  let update_my_user ~body client () =
    let op_name = "update_my_user" in
    let url_path = "/users/me" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UserUpdateMe.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Cast = struct
  module Types = struct
    module Update = struct
      type t = {
        g_cast_enabled : bool option;
      }
    end
  
    module Response = struct
      type t = {
        g_cast_enabled : bool;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?g_cast_enabled () = { g_cast_enabled }
    
    let g_cast_enabled t = t.g_cast_enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CastUpdate"
        (fun g_cast_enabled -> { g_cast_enabled })
      |> Jsont.Object.opt_mem "gCastEnabled" Jsont.bool ~enc:(fun r -> r.g_cast_enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(g_cast_enabled=false) () = { g_cast_enabled }
    
    let g_cast_enabled t = t.g_cast_enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CastResponse"
        (fun g_cast_enabled -> { g_cast_enabled })
      |> Jsont.Object.mem "gCastEnabled" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.g_cast_enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module BulkIds = struct
  module Types = struct
    module Dto = struct
      type t = {
        ids : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~ids () = { ids }
    
    let ids t = t.ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"BulkIdsDto"
        (fun ids -> { ids })
      |> Jsont.Object.mem "ids" (Jsont.list Jsont.string) ~enc:(fun r -> r.ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Trash = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        count : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~count () = { count }
    
    let count t = t.count
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TrashResponseDto"
        (fun count -> { count })
      |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Empty trash
  
      Permanently delete all items in the trash. *)
  let empty_trash client () =
    let op_name = "empty_trash" in
    let url_path = "/trash/empty" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Restore trash
  
      Restore all items in the trash. *)
  let restore_trash client () =
    let op_name = "restore_trash" in
    let url_path = "/trash/restore" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Restore assets
  
      Restore specific assets from the trash. *)
  let restore_assets ~body client () =
    let op_name = "restore_assets" in
    let url_path = "/trash/restore/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json BulkIds.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module BulkIdErrorReason = struct
  module Types = struct
    module T = struct
      type t = [
        | `Duplicate
        | `No_permission
        | `Not_found
        | `Unknown
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"BulkIdErrorReason"
        ~dec:(function
          | "duplicate" -> `Duplicate
          | "no_permission" -> `No_permission
          | "not_found" -> `Not_found
          | "unknown" -> `Unknown
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Duplicate -> "duplicate"
          | `No_permission -> "no_permission"
          | `Not_found -> "not_found"
          | `Unknown -> "unknown")
  end
end

module AlbumsAddAssets = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        error : BulkIdErrorReason.T.t option;
        success : bool;
      }
    end
  
    module Dto = struct
      type t = {
        album_ids : string list;
        asset_ids : string list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~success ?error () = { error; success }
    
    let error t = t.error
    let success t = t.success
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumsAddAssetsResponseDto"
        (fun error success -> { error; success })
      |> Jsont.Object.opt_mem "error" BulkIdErrorReason.T.jsont ~enc:(fun r -> r.error)
      |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~album_ids ~asset_ids () = { album_ids; asset_ids }
    
    let album_ids t = t.album_ids
    let asset_ids t = t.asset_ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumsAddAssetsDto"
        (fun album_ids asset_ids -> { album_ids; asset_ids })
      |> Jsont.Object.mem "albumIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.album_ids)
      |> Jsont.Object.mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Add assets to albums
  
      Send a list of asset IDs and album IDs to add each asset to each album. *)
  let add_assets_to_albums ?key ?slug ~body client () =
    let op_name = "add_assets_to_albums" in
    let url_path = "/albums/assets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AuthStatus = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        expires_at : string option;
        is_elevated : bool;
        password : bool;
        pin_code : bool;
        pin_expires_at : string option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~is_elevated ~password ~pin_code ?expires_at ?pin_expires_at () = { expires_at; is_elevated; password; pin_code; pin_expires_at }
    
    let expires_at t = t.expires_at
    let is_elevated t = t.is_elevated
    let password t = t.password
    let pin_code t = t.pin_code
    let pin_expires_at t = t.pin_expires_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AuthStatusResponseDto"
        (fun expires_at is_elevated password pin_code pin_expires_at -> { expires_at; is_elevated; password; pin_code; pin_expires_at })
      |> Jsont.Object.opt_mem "expiresAt" Jsont.string ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.mem "isElevated" Jsont.bool ~enc:(fun r -> r.is_elevated)
      |> Jsont.Object.mem "password" Jsont.bool ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "pinCode" Jsont.bool ~enc:(fun r -> r.pin_code)
      |> Jsont.Object.opt_mem "pinExpiresAt" Jsont.string ~enc:(fun r -> r.pin_expires_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve auth status
  
      Get information about the current session, including whether the user has a password, and if the session can access locked assets. *)
  let get_auth_status client () =
    let op_name = "get_auth_status" in
    let url_path = "/auth/status" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AudioCodec = struct
  module Types = struct
    module T = struct
      type t = [
        | `Mp3
        | `Aac
        | `Libopus
        | `Pcm_s16le
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AudioCodec"
        ~dec:(function
          | "mp3" -> `Mp3
          | "aac" -> `Aac
          | "libopus" -> `Libopus
          | "pcm_s16le" -> `Pcm_s16le
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Mp3 -> "mp3"
          | `Aac -> "aac"
          | `Libopus -> "libopus"
          | `Pcm_s16le -> "pcm_s16le")
  end
end

module SystemConfigFfmpeg = struct
  module Types = struct
    module Dto = struct
      type t = {
        accel : TranscodeHwaccel.T.t;
        accel_decode : bool;
        accepted_audio_codecs : AudioCodec.T.t list;
        accepted_containers : VideoContainer.T.t list;
        accepted_video_codecs : VideoCodec.T.t list;
        bframes : int;
        cq_mode : Cqmode.T.t;
        crf : int;
        gop_size : int;
        max_bitrate : string;
        preferred_hw_device : string;
        preset : string;
        refs : int;
        target_audio_codec : AudioCodec.T.t;
        target_resolution : string;
        target_video_codec : VideoCodec.T.t;
        temporal_aq : bool;
        threads : int;
        tonemap : ToneMapping.T.t;
        transcode : TranscodePolicy.T.t;
        two_pass : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~accel ~accel_decode ~accepted_audio_codecs ~accepted_containers ~accepted_video_codecs ~bframes ~cq_mode ~crf ~gop_size ~max_bitrate ~preferred_hw_device ~preset ~refs ~target_audio_codec ~target_resolution ~target_video_codec ~temporal_aq ~threads ~tonemap ~transcode ~two_pass () = { accel; accel_decode; accepted_audio_codecs; accepted_containers; accepted_video_codecs; bframes; cq_mode; crf; gop_size; max_bitrate; preferred_hw_device; preset; refs; target_audio_codec; target_resolution; target_video_codec; temporal_aq; threads; tonemap; transcode; two_pass }
    
    let accel t = t.accel
    let accel_decode t = t.accel_decode
    let accepted_audio_codecs t = t.accepted_audio_codecs
    let accepted_containers t = t.accepted_containers
    let accepted_video_codecs t = t.accepted_video_codecs
    let bframes t = t.bframes
    let cq_mode t = t.cq_mode
    let crf t = t.crf
    let gop_size t = t.gop_size
    let max_bitrate t = t.max_bitrate
    let preferred_hw_device t = t.preferred_hw_device
    let preset t = t.preset
    let refs t = t.refs
    let target_audio_codec t = t.target_audio_codec
    let target_resolution t = t.target_resolution
    let target_video_codec t = t.target_video_codec
    let temporal_aq t = t.temporal_aq
    let threads t = t.threads
    let tonemap t = t.tonemap
    let transcode t = t.transcode
    let two_pass t = t.two_pass
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigFFmpegDto"
        (fun accel accel_decode accepted_audio_codecs accepted_containers accepted_video_codecs bframes cq_mode crf gop_size max_bitrate preferred_hw_device preset refs target_audio_codec target_resolution target_video_codec temporal_aq threads tonemap transcode two_pass -> { accel; accel_decode; accepted_audio_codecs; accepted_containers; accepted_video_codecs; bframes; cq_mode; crf; gop_size; max_bitrate; preferred_hw_device; preset; refs; target_audio_codec; target_resolution; target_video_codec; temporal_aq; threads; tonemap; transcode; two_pass })
      |> Jsont.Object.mem "accel" TranscodeHwaccel.T.jsont ~enc:(fun r -> r.accel)
      |> Jsont.Object.mem "accelDecode" Jsont.bool ~enc:(fun r -> r.accel_decode)
      |> Jsont.Object.mem "acceptedAudioCodecs" (Jsont.list AudioCodec.T.jsont) ~enc:(fun r -> r.accepted_audio_codecs)
      |> Jsont.Object.mem "acceptedContainers" (Jsont.list VideoContainer.T.jsont) ~enc:(fun r -> r.accepted_containers)
      |> Jsont.Object.mem "acceptedVideoCodecs" (Jsont.list VideoCodec.T.jsont) ~enc:(fun r -> r.accepted_video_codecs)
      |> Jsont.Object.mem "bframes" (Openapi.Runtime.validated_int ~minimum:(-1.) ~maximum:16. Jsont.int) ~enc:(fun r -> r.bframes)
      |> Jsont.Object.mem "cqMode" Cqmode.T.jsont ~enc:(fun r -> r.cq_mode)
      |> Jsont.Object.mem "crf" (Openapi.Runtime.validated_int ~minimum:0. ~maximum:51. Jsont.int) ~enc:(fun r -> r.crf)
      |> Jsont.Object.mem "gopSize" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.gop_size)
      |> Jsont.Object.mem "maxBitrate" Jsont.string ~enc:(fun r -> r.max_bitrate)
      |> Jsont.Object.mem "preferredHwDevice" Jsont.string ~enc:(fun r -> r.preferred_hw_device)
      |> Jsont.Object.mem "preset" Jsont.string ~enc:(fun r -> r.preset)
      |> Jsont.Object.mem "refs" (Openapi.Runtime.validated_int ~minimum:0. ~maximum:6. Jsont.int) ~enc:(fun r -> r.refs)
      |> Jsont.Object.mem "targetAudioCodec" AudioCodec.T.jsont ~enc:(fun r -> r.target_audio_codec)
      |> Jsont.Object.mem "targetResolution" Jsont.string ~enc:(fun r -> r.target_resolution)
      |> Jsont.Object.mem "targetVideoCodec" VideoCodec.T.jsont ~enc:(fun r -> r.target_video_codec)
      |> Jsont.Object.mem "temporalAQ" Jsont.bool ~enc:(fun r -> r.temporal_aq)
      |> Jsont.Object.mem "threads" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.threads)
      |> Jsont.Object.mem "tonemap" ToneMapping.T.jsont ~enc:(fun r -> r.tonemap)
      |> Jsont.Object.mem "transcode" TranscodePolicy.T.jsont ~enc:(fun r -> r.transcode)
      |> Jsont.Object.mem "twoPass" Jsont.bool ~enc:(fun r -> r.two_pass)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SystemConfig = struct
  module Types = struct
    module Dto = struct
      type t = {
        backup : SystemConfigBackups.Dto.t;
        ffmpeg : SystemConfigFfmpeg.Dto.t;
        image : SystemConfigImage.Dto.t;
        job : SystemConfigJob.Dto.t;
        library : SystemConfigLibrary.Dto.t;
        logging : SystemConfigLogging.Dto.t;
        machine_learning : SystemConfigMachineLearning.Dto.t;
        map : SystemConfigMap.Dto.t;
        metadata : SystemConfigMetadata.Dto.t;
        new_version_check : SystemConfigNewVersionCheck.Dto.t;
        nightly_tasks : SystemConfigNightlyTasks.Dto.t;
        notifications : SystemConfigNotifications.Dto.t;
        oauth : SystemConfigOauth.Dto.t;
        password_login : SystemConfigPasswordLogin.Dto.t;
        reverse_geocoding : SystemConfigReverseGeocoding.Dto.t;
        server : SystemConfigServer.Dto.t;
        storage_template : SystemConfigStorageTemplate.Dto.t;
        templates : SystemConfigTemplates.Dto.t;
        theme : SystemConfigTheme.Dto.t;
        trash : SystemConfigTrash.Dto.t;
        user : SystemConfigUser.Dto.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~backup ~ffmpeg ~image ~job ~library ~logging ~machine_learning ~map ~metadata ~new_version_check ~nightly_tasks ~notifications ~oauth ~password_login ~reverse_geocoding ~server ~storage_template ~templates ~theme ~trash ~user () = { backup; ffmpeg; image; job; library; logging; machine_learning; map; metadata; new_version_check; nightly_tasks; notifications; oauth; password_login; reverse_geocoding; server; storage_template; templates; theme; trash; user }
    
    let backup t = t.backup
    let ffmpeg t = t.ffmpeg
    let image t = t.image
    let job t = t.job
    let library t = t.library
    let logging t = t.logging
    let machine_learning t = t.machine_learning
    let map t = t.map
    let metadata t = t.metadata
    let new_version_check t = t.new_version_check
    let nightly_tasks t = t.nightly_tasks
    let notifications t = t.notifications
    let oauth t = t.oauth
    let password_login t = t.password_login
    let reverse_geocoding t = t.reverse_geocoding
    let server t = t.server
    let storage_template t = t.storage_template
    let templates t = t.templates
    let theme t = t.theme
    let trash t = t.trash
    let user t = t.user
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SystemConfigDto"
        (fun backup ffmpeg image job library logging machine_learning map metadata new_version_check nightly_tasks notifications oauth password_login reverse_geocoding server storage_template templates theme trash user -> { backup; ffmpeg; image; job; library; logging; machine_learning; map; metadata; new_version_check; nightly_tasks; notifications; oauth; password_login; reverse_geocoding; server; storage_template; templates; theme; trash; user })
      |> Jsont.Object.mem "backup" SystemConfigBackups.Dto.jsont ~enc:(fun r -> r.backup)
      |> Jsont.Object.mem "ffmpeg" SystemConfigFfmpeg.Dto.jsont ~enc:(fun r -> r.ffmpeg)
      |> Jsont.Object.mem "image" SystemConfigImage.Dto.jsont ~enc:(fun r -> r.image)
      |> Jsont.Object.mem "job" SystemConfigJob.Dto.jsont ~enc:(fun r -> r.job)
      |> Jsont.Object.mem "library" SystemConfigLibrary.Dto.jsont ~enc:(fun r -> r.library)
      |> Jsont.Object.mem "logging" SystemConfigLogging.Dto.jsont ~enc:(fun r -> r.logging)
      |> Jsont.Object.mem "machineLearning" SystemConfigMachineLearning.Dto.jsont ~enc:(fun r -> r.machine_learning)
      |> Jsont.Object.mem "map" SystemConfigMap.Dto.jsont ~enc:(fun r -> r.map)
      |> Jsont.Object.mem "metadata" SystemConfigMetadata.Dto.jsont ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "newVersionCheck" SystemConfigNewVersionCheck.Dto.jsont ~enc:(fun r -> r.new_version_check)
      |> Jsont.Object.mem "nightlyTasks" SystemConfigNightlyTasks.Dto.jsont ~enc:(fun r -> r.nightly_tasks)
      |> Jsont.Object.mem "notifications" SystemConfigNotifications.Dto.jsont ~enc:(fun r -> r.notifications)
      |> Jsont.Object.mem "oauth" SystemConfigOauth.Dto.jsont ~enc:(fun r -> r.oauth)
      |> Jsont.Object.mem "passwordLogin" SystemConfigPasswordLogin.Dto.jsont ~enc:(fun r -> r.password_login)
      |> Jsont.Object.mem "reverseGeocoding" SystemConfigReverseGeocoding.Dto.jsont ~enc:(fun r -> r.reverse_geocoding)
      |> Jsont.Object.mem "server" SystemConfigServer.Dto.jsont ~enc:(fun r -> r.server)
      |> Jsont.Object.mem "storageTemplate" SystemConfigStorageTemplate.Dto.jsont ~enc:(fun r -> r.storage_template)
      |> Jsont.Object.mem "templates" SystemConfigTemplates.Dto.jsont ~enc:(fun r -> r.templates)
      |> Jsont.Object.mem "theme" SystemConfigTheme.Dto.jsont ~enc:(fun r -> r.theme)
      |> Jsont.Object.mem "trash" SystemConfigTrash.Dto.jsont ~enc:(fun r -> r.trash)
      |> Jsont.Object.mem "user" SystemConfigUser.Dto.jsont ~enc:(fun r -> r.user)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get system configuration
  
      Retrieve the current system configuration. *)
  let get_config client () =
    let op_name = "get_config" in
    let url_path = "/system-config" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update system configuration
  
      Update the system configuration with a new system configuration. *)
  let update_config ~body client () =
    let op_name = "update_config" in
    let url_path = "/system-config" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get system configuration defaults
  
      Retrieve the default values for the system configuration. *)
  let get_config_defaults client () =
    let op_name = "get_config_defaults" in
    let url_path = "/system-config/defaults" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetVisibility = struct
  module Types = struct
    module T = struct
      type t = [
        | `Archive
        | `Timeline
        | `Hidden
        | `Locked
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetVisibility"
        ~dec:(function
          | "archive" -> `Archive
          | "timeline" -> `Timeline
          | "hidden" -> `Hidden
          | "locked" -> `Locked
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Archive -> "archive"
          | `Timeline -> "timeline"
          | `Hidden -> "hidden"
          | `Locked -> "locked")
  end
end

module UpdateAsset = struct
  module Types = struct
    module Dto = struct
      type t = {
        date_time_original : string option;
        description : string option;
        is_favorite : bool option;
        latitude : float option;
        live_photo_video_id : string option;
        longitude : float option;
        rating : float option;
        visibility : AssetVisibility.T.t option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?date_time_original ?description ?is_favorite ?latitude ?live_photo_video_id ?longitude ?rating ?visibility () = { date_time_original; description; is_favorite; latitude; live_photo_video_id; longitude; rating; visibility }
    
    let date_time_original t = t.date_time_original
    let description t = t.description
    let is_favorite t = t.is_favorite
    let latitude t = t.latitude
    let live_photo_video_id t = t.live_photo_video_id
    let longitude t = t.longitude
    let rating t = t.rating
    let visibility t = t.visibility
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UpdateAssetDto"
        (fun date_time_original description is_favorite latitude live_photo_video_id longitude rating visibility -> { date_time_original; description; is_favorite; latitude; live_photo_video_id; longitude; rating; visibility })
      |> Jsont.Object.opt_mem "dateTimeOriginal" Jsont.string ~enc:(fun r -> r.date_time_original)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "latitude" Jsont.number ~enc:(fun r -> r.latitude)
      |> Jsont.Object.mem "livePhotoVideoId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.live_photo_video_id)
      |> Jsont.Object.opt_mem "longitude" Jsont.number ~enc:(fun r -> r.longitude)
      |> Jsont.Object.opt_mem "rating" (Openapi.Runtime.validated_float ~minimum:(-1.) ~maximum:5. Jsont.number) ~enc:(fun r -> r.rating)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module TimeBucketAsset = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        city : string list;  (** Array of city names extracted from EXIF GPS data *)
        country : string list;  (** Array of country names extracted from EXIF GPS data *)
        duration : string list;  (** Array of video durations in HH:MM:SS format (null for images) *)
        file_created_at : string list;  (** Array of file creation timestamps in UTC (ISO 8601 format, without timezone) *)
        id : string list;  (** Array of asset IDs in the time bucket *)
        is_favorite : bool list;  (** Array indicating whether each asset is favorited *)
        is_image : bool list;  (** Array indicating whether each asset is an image (false for videos) *)
        is_trashed : bool list;  (** Array indicating whether each asset is in the trash *)
        latitude : float list option;  (** Array of latitude coordinates extracted from EXIF GPS data *)
        live_photo_video_id : string list;  (** Array of live photo video asset IDs (null for non-live photos) *)
        local_offset_hours : float list;  (** Array of UTC offset hours at the time each photo was taken. Positive values are east of UTC, negative values are west of UTC. Values may be fractional (e.g., 5.5 for +05:30, -9.75 for -09:45). Applying this offset to 'fileCreatedAt' will give you the time the photo was taken from the photographer's perspective. *)
        longitude : float list option;  (** Array of longitude coordinates extracted from EXIF GPS data *)
        owner_id : string list;  (** Array of owner IDs for each asset *)
        projection_type : string list;  (** Array of projection types for 360° content (e.g., "EQUIRECTANGULAR", "CUBEFACE", "CYLINDRICAL") *)
        ratio : float list;  (** Array of aspect ratios (width/height) for each asset *)
        stack : string list list option;  (** Array of stack information as [stackId, assetCount] tuples (null for non-stacked assets) *)
        thumbhash : string list;  (** Array of BlurHash strings for generating asset previews (base64 encoded) *)
        visibility : AssetVisibility.T.t list;  (** Array of visibility statuses for each asset (e.g., ARCHIVE, TIMELINE, HIDDEN, LOCKED) *)
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~city ~country ~duration ~file_created_at ~id ~is_favorite ~is_image ~is_trashed ~live_photo_video_id ~local_offset_hours ~owner_id ~projection_type ~ratio ~thumbhash ~visibility ?latitude ?longitude ?stack () = { city; country; duration; file_created_at; id; is_favorite; is_image; is_trashed; latitude; live_photo_video_id; local_offset_hours; longitude; owner_id; projection_type; ratio; stack; thumbhash; visibility }
    
    let city t = t.city
    let country t = t.country
    let duration t = t.duration
    let file_created_at t = t.file_created_at
    let id t = t.id
    let is_favorite t = t.is_favorite
    let is_image t = t.is_image
    let is_trashed t = t.is_trashed
    let latitude t = t.latitude
    let live_photo_video_id t = t.live_photo_video_id
    let local_offset_hours t = t.local_offset_hours
    let longitude t = t.longitude
    let owner_id t = t.owner_id
    let projection_type t = t.projection_type
    let ratio t = t.ratio
    let stack t = t.stack
    let thumbhash t = t.thumbhash
    let visibility t = t.visibility
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TimeBucketAssetResponseDto"
        (fun city country duration file_created_at id is_favorite is_image is_trashed latitude live_photo_video_id local_offset_hours longitude owner_id projection_type ratio stack thumbhash visibility -> { city; country; duration; file_created_at; id; is_favorite; is_image; is_trashed; latitude; live_photo_video_id; local_offset_hours; longitude; owner_id; projection_type; ratio; stack; thumbhash; visibility })
      |> Jsont.Object.mem "city" (Jsont.list Jsont.string) ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" (Jsont.list Jsont.string) ~enc:(fun r -> r.country)
      |> Jsont.Object.mem "duration" (Jsont.list Jsont.string) ~enc:(fun r -> r.duration)
      |> Jsont.Object.mem "fileCreatedAt" (Jsont.list Jsont.string) ~enc:(fun r -> r.file_created_at)
      |> Jsont.Object.mem "id" (Jsont.list Jsont.string) ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isFavorite" (Jsont.list Jsont.bool) ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.mem "isImage" (Jsont.list Jsont.bool) ~enc:(fun r -> r.is_image)
      |> Jsont.Object.mem "isTrashed" (Jsont.list Jsont.bool) ~enc:(fun r -> r.is_trashed)
      |> Jsont.Object.opt_mem "latitude" (Jsont.list Jsont.number) ~enc:(fun r -> r.latitude)
      |> Jsont.Object.mem "livePhotoVideoId" (Jsont.list Jsont.string) ~enc:(fun r -> r.live_photo_video_id)
      |> Jsont.Object.mem "localOffsetHours" (Jsont.list Jsont.number) ~enc:(fun r -> r.local_offset_hours)
      |> Jsont.Object.opt_mem "longitude" (Jsont.list Jsont.number) ~enc:(fun r -> r.longitude)
      |> Jsont.Object.mem "ownerId" (Jsont.list Jsont.string) ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "projectionType" (Jsont.list Jsont.string) ~enc:(fun r -> r.projection_type)
      |> Jsont.Object.mem "ratio" (Jsont.list Jsont.number) ~enc:(fun r -> r.ratio)
      |> Jsont.Object.opt_mem "stack" (Jsont.list (Jsont.list Jsont.string)) ~enc:(fun r -> r.stack)
      |> Jsont.Object.mem "thumbhash" (Jsont.list Jsont.string) ~enc:(fun r -> r.thumbhash)
      |> Jsont.Object.mem "visibility" (Jsont.list AssetVisibility.T.jsont) ~enc:(fun r -> r.visibility)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get time bucket
  
      Retrieve a string of all asset ids in a given time bucket. 
      @param album_id Filter assets belonging to a specific album
      @param is_favorite Filter by favorite status (true for favorites only, false for non-favorites only)
      @param is_trashed Filter by trash status (true for trashed assets only, false for non-trashed only)
      @param order Sort order for assets within time buckets (ASC for oldest first, DESC for newest first)
      @param person_id Filter assets containing a specific person (face recognition)
      @param tag_id Filter assets with a specific tag
      @param time_bucket Time bucket identifier in YYYY-MM-DD format (e.g., "2024-01-01" for January 2024)
      @param user_id Filter assets by specific user ID
      @param visibility Filter by asset visibility status (ARCHIVE, TIMELINE, HIDDEN, LOCKED)
      @param with_coordinates Include location data in the response
      @param with_partners Include assets shared by partners
      @param with_stacked Include stacked assets in the response. When true, only primary assets from stacks are returned.
  *)
  let get_time_bucket ?album_id ?is_favorite ?is_trashed ?key ?order ?person_id ?slug ?tag_id ~time_bucket ?user_id ?visibility ?with_coordinates ?with_partners ?with_stacked client () =
    let op_name = "get_time_bucket" in
    let url_path = "/timeline/bucket" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"albumId" ~value:album_id; Openapi.Runtime.Query.optional ~key:"isFavorite" ~value:is_favorite; Openapi.Runtime.Query.optional ~key:"isTrashed" ~value:is_trashed; Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"order" ~value:order; Openapi.Runtime.Query.optional ~key:"personId" ~value:person_id; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug; Openapi.Runtime.Query.optional ~key:"tagId" ~value:tag_id; Openapi.Runtime.Query.singleton ~key:"timeBucket" ~value:time_bucket; Openapi.Runtime.Query.optional ~key:"userId" ~value:user_id; Openapi.Runtime.Query.optional ~key:"visibility" ~value:visibility; Openapi.Runtime.Query.optional ~key:"withCoordinates" ~value:with_coordinates; Openapi.Runtime.Query.optional ~key:"withPartners" ~value:with_partners; Openapi.Runtime.Query.optional ~key:"withStacked" ~value:with_stacked]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetBulk = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        date_time_original : string option;
        date_time_relative : float option;
        description : string option;
        duplicate_id : string option;
        ids : string list;
        is_favorite : bool option;
        latitude : float option;
        longitude : float option;
        rating : float option;
        time_zone : string option;
        visibility : AssetVisibility.T.t option;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ~ids ?date_time_original ?date_time_relative ?description ?duplicate_id ?is_favorite ?latitude ?longitude ?rating ?time_zone ?visibility () = { date_time_original; date_time_relative; description; duplicate_id; ids; is_favorite; latitude; longitude; rating; time_zone; visibility }
    
    let date_time_original t = t.date_time_original
    let date_time_relative t = t.date_time_relative
    let description t = t.description
    let duplicate_id t = t.duplicate_id
    let ids t = t.ids
    let is_favorite t = t.is_favorite
    let latitude t = t.latitude
    let longitude t = t.longitude
    let rating t = t.rating
    let time_zone t = t.time_zone
    let visibility t = t.visibility
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetBulkUpdateDto"
        (fun date_time_original date_time_relative description duplicate_id ids is_favorite latitude longitude rating time_zone visibility -> { date_time_original; date_time_relative; description; duplicate_id; ids; is_favorite; latitude; longitude; rating; time_zone; visibility })
      |> Jsont.Object.opt_mem "dateTimeOriginal" Jsont.string ~enc:(fun r -> r.date_time_original)
      |> Jsont.Object.opt_mem "dateTimeRelative" Jsont.number ~enc:(fun r -> r.date_time_relative)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "duplicateId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.duplicate_id)
      |> Jsont.Object.mem "ids" (Jsont.list Jsont.string) ~enc:(fun r -> r.ids)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "latitude" Jsont.number ~enc:(fun r -> r.latitude)
      |> Jsont.Object.opt_mem "longitude" Jsont.number ~enc:(fun r -> r.longitude)
      |> Jsont.Object.opt_mem "rating" (Openapi.Runtime.validated_float ~minimum:(-1.) ~maximum:5. Jsont.number) ~enc:(fun r -> r.rating)
      |> Jsont.Object.opt_mem "timeZone" Jsont.string ~enc:(fun r -> r.time_zone)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetTypeEnum = struct
  module Types = struct
    module T = struct
      type t = [
        | `Image
        | `Video
        | `Audio
        | `Other
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetTypeEnum"
        ~dec:(function
          | "IMAGE" -> `Image
          | "VIDEO" -> `Video
          | "AUDIO" -> `Audio
          | "OTHER" -> `Other
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Image -> "IMAGE"
          | `Video -> "VIDEO"
          | `Audio -> "AUDIO"
          | `Other -> "OTHER")
  end
end

module SyncAssetV1 = struct
  module Types = struct
    module T = struct
      type t = {
        checksum : string;
        deleted_at : Ptime.t option;
        duration : string option;
        file_created_at : Ptime.t option;
        file_modified_at : Ptime.t option;
        height : int option;
        id : string;
        is_edited : bool;
        is_favorite : bool;
        library_id : string option;
        live_photo_video_id : string option;
        local_date_time : Ptime.t option;
        original_file_name : string;
        owner_id : string;
        stack_id : string option;
        thumbhash : string option;
        type_ : AssetTypeEnum.T.t;
        visibility : AssetVisibility.T.t;
        width : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~checksum ~id ~is_edited ~is_favorite ~original_file_name ~owner_id ~type_ ~visibility ?deleted_at ?duration ?file_created_at ?file_modified_at ?height ?library_id ?live_photo_video_id ?local_date_time ?stack_id ?thumbhash ?width () = { checksum; deleted_at; duration; file_created_at; file_modified_at; height; id; is_edited; is_favorite; library_id; live_photo_video_id; local_date_time; original_file_name; owner_id; stack_id; thumbhash; type_; visibility; width }
    
    let checksum t = t.checksum
    let deleted_at t = t.deleted_at
    let duration t = t.duration
    let file_created_at t = t.file_created_at
    let file_modified_at t = t.file_modified_at
    let height t = t.height
    let id t = t.id
    let is_edited t = t.is_edited
    let is_favorite t = t.is_favorite
    let library_id t = t.library_id
    let live_photo_video_id t = t.live_photo_video_id
    let local_date_time t = t.local_date_time
    let original_file_name t = t.original_file_name
    let owner_id t = t.owner_id
    let stack_id t = t.stack_id
    let thumbhash t = t.thumbhash
    let type_ t = t.type_
    let visibility t = t.visibility
    let width t = t.width
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAssetV1"
        (fun checksum deleted_at duration file_created_at file_modified_at height id is_edited is_favorite library_id live_photo_video_id local_date_time original_file_name owner_id stack_id thumbhash type_ visibility width -> { checksum; deleted_at; duration; file_created_at; file_modified_at; height; id; is_edited; is_favorite; library_id; live_photo_video_id; local_date_time; original_file_name; owner_id; stack_id; thumbhash; type_; visibility; width })
      |> Jsont.Object.mem "checksum" Jsont.string ~enc:(fun r -> r.checksum)
      |> Jsont.Object.mem "deletedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.mem "duration" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.duration)
      |> Jsont.Object.mem "fileCreatedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.file_created_at)
      |> Jsont.Object.mem "fileModifiedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.file_modified_at)
      |> Jsont.Object.mem "height" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.height)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isEdited" Jsont.bool ~enc:(fun r -> r.is_edited)
      |> Jsont.Object.mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.mem "libraryId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.library_id)
      |> Jsont.Object.mem "livePhotoVideoId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.live_photo_video_id)
      |> Jsont.Object.mem "localDateTime" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.local_date_time)
      |> Jsont.Object.mem "originalFileName" Jsont.string ~enc:(fun r -> r.original_file_name)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "stackId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.stack_id)
      |> Jsont.Object.mem "thumbhash" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.thumbhash)
      |> Jsont.Object.mem "type" AssetTypeEnum.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.mem "width" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module StatisticsSearch = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_ids : string list option;
        city : string option;
        country : string option;
        created_after : Ptime.t option;
        created_before : Ptime.t option;
        description : string option;
        device_id : string option;
        is_encoded : bool option;
        is_favorite : bool option;
        is_motion : bool option;
        is_not_in_album : bool option;
        is_offline : bool option;
        lens_model : string option;
        library_id : string option;
        make : string option;
        model : string option;
        ocr : string option;
        person_ids : string list option;
        rating : float option;
        state : string option;
        tag_ids : string list option;
        taken_after : Ptime.t option;
        taken_before : Ptime.t option;
        trashed_after : Ptime.t option;
        trashed_before : Ptime.t option;
        type_ : AssetTypeEnum.T.t option;
        updated_after : Ptime.t option;
        updated_before : Ptime.t option;
        visibility : AssetVisibility.T.t option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?album_ids ?city ?country ?created_after ?created_before ?description ?device_id ?is_encoded ?is_favorite ?is_motion ?is_not_in_album ?is_offline ?lens_model ?library_id ?make ?model ?ocr ?person_ids ?rating ?state ?tag_ids ?taken_after ?taken_before ?trashed_after ?trashed_before ?type_ ?updated_after ?updated_before ?visibility () = { album_ids; city; country; created_after; created_before; description; device_id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; lens_model; library_id; make; model; ocr; person_ids; rating; state; tag_ids; taken_after; taken_before; trashed_after; trashed_before; type_; updated_after; updated_before; visibility }
    
    let album_ids t = t.album_ids
    let city t = t.city
    let country t = t.country
    let created_after t = t.created_after
    let created_before t = t.created_before
    let description t = t.description
    let device_id t = t.device_id
    let is_encoded t = t.is_encoded
    let is_favorite t = t.is_favorite
    let is_motion t = t.is_motion
    let is_not_in_album t = t.is_not_in_album
    let is_offline t = t.is_offline
    let lens_model t = t.lens_model
    let library_id t = t.library_id
    let make t = t.make
    let model t = t.model
    let ocr t = t.ocr
    let person_ids t = t.person_ids
    let rating t = t.rating
    let state t = t.state
    let tag_ids t = t.tag_ids
    let taken_after t = t.taken_after
    let taken_before t = t.taken_before
    let trashed_after t = t.trashed_after
    let trashed_before t = t.trashed_before
    let type_ t = t.type_
    let updated_after t = t.updated_after
    let updated_before t = t.updated_before
    let visibility t = t.visibility
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StatisticsSearchDto"
        (fun album_ids city country created_after created_before description device_id is_encoded is_favorite is_motion is_not_in_album is_offline lens_model library_id make model ocr person_ids rating state tag_ids taken_after taken_before trashed_after trashed_before type_ updated_after updated_before visibility -> { album_ids; city; country; created_after; created_before; description; device_id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; lens_model; library_id; make; model; ocr; person_ids; rating; state; tag_ids; taken_after; taken_before; trashed_after; trashed_before; type_; updated_after; updated_before; visibility })
      |> Jsont.Object.opt_mem "albumIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.album_ids)
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.opt_mem "createdAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_after)
      |> Jsont.Object.opt_mem "createdBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_before)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.opt_mem "isEncoded" Jsont.bool ~enc:(fun r -> r.is_encoded)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isMotion" Jsont.bool ~enc:(fun r -> r.is_motion)
      |> Jsont.Object.opt_mem "isNotInAlbum" Jsont.bool ~enc:(fun r -> r.is_not_in_album)
      |> Jsont.Object.opt_mem "isOffline" Jsont.bool ~enc:(fun r -> r.is_offline)
      |> Jsont.Object.mem "lensModel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.lens_model)
      |> Jsont.Object.mem "libraryId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.library_id)
      |> Jsont.Object.opt_mem "make" Jsont.string ~enc:(fun r -> r.make)
      |> Jsont.Object.mem "model" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.model)
      |> Jsont.Object.opt_mem "ocr" Jsont.string ~enc:(fun r -> r.ocr)
      |> Jsont.Object.opt_mem "personIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.person_ids)
      |> Jsont.Object.opt_mem "rating" (Openapi.Runtime.validated_float ~minimum:(-1.) ~maximum:5. Jsont.number) ~enc:(fun r -> r.rating)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "tagIds" (Openapi.Runtime.nullable_any (Jsont.list Jsont.string))
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.tag_ids)
      |> Jsont.Object.opt_mem "takenAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_after)
      |> Jsont.Object.opt_mem "takenBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_before)
      |> Jsont.Object.opt_mem "trashedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_after)
      |> Jsont.Object.opt_mem "trashedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_before)
      |> Jsont.Object.opt_mem "type" AssetTypeEnum.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_after)
      |> Jsont.Object.opt_mem "updatedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_before)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchStatistics = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        total : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~total () = { total }
    
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchStatisticsResponseDto"
        (fun total -> { total })
      |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Search asset statistics
  
      Retrieve statistical data about assets based on search criteria, such as the total matching count. *)
  let search_asset_statistics ~body client () =
    let op_name = "search_asset_statistics" in
    let url_path = "/search/statistics" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json StatisticsSearch.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SmartSearch = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_ids : string list option;
        city : string option;
        country : string option;
        created_after : Ptime.t option;
        created_before : Ptime.t option;
        device_id : string option;
        is_encoded : bool option;
        is_favorite : bool option;
        is_motion : bool option;
        is_not_in_album : bool option;
        is_offline : bool option;
        language : string option;
        lens_model : string option;
        library_id : string option;
        make : string option;
        model : string option;
        ocr : string option;
        page : float option;
        person_ids : string list option;
        query : string option;
        query_asset_id : string option;
        rating : float option;
        size : float option;
        state : string option;
        tag_ids : string list option;
        taken_after : Ptime.t option;
        taken_before : Ptime.t option;
        trashed_after : Ptime.t option;
        trashed_before : Ptime.t option;
        type_ : AssetTypeEnum.T.t option;
        updated_after : Ptime.t option;
        updated_before : Ptime.t option;
        visibility : AssetVisibility.T.t option;
        with_deleted : bool option;
        with_exif : bool option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?album_ids ?city ?country ?created_after ?created_before ?device_id ?is_encoded ?is_favorite ?is_motion ?is_not_in_album ?is_offline ?language ?lens_model ?library_id ?make ?model ?ocr ?page ?person_ids ?query ?query_asset_id ?rating ?size ?state ?tag_ids ?taken_after ?taken_before ?trashed_after ?trashed_before ?type_ ?updated_after ?updated_before ?visibility ?with_deleted ?with_exif () = { album_ids; city; country; created_after; created_before; device_id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; language; lens_model; library_id; make; model; ocr; page; person_ids; query; query_asset_id; rating; size; state; tag_ids; taken_after; taken_before; trashed_after; trashed_before; type_; updated_after; updated_before; visibility; with_deleted; with_exif }
    
    let album_ids t = t.album_ids
    let city t = t.city
    let country t = t.country
    let created_after t = t.created_after
    let created_before t = t.created_before
    let device_id t = t.device_id
    let is_encoded t = t.is_encoded
    let is_favorite t = t.is_favorite
    let is_motion t = t.is_motion
    let is_not_in_album t = t.is_not_in_album
    let is_offline t = t.is_offline
    let language t = t.language
    let lens_model t = t.lens_model
    let library_id t = t.library_id
    let make t = t.make
    let model t = t.model
    let ocr t = t.ocr
    let page t = t.page
    let person_ids t = t.person_ids
    let query t = t.query
    let query_asset_id t = t.query_asset_id
    let rating t = t.rating
    let size t = t.size
    let state t = t.state
    let tag_ids t = t.tag_ids
    let taken_after t = t.taken_after
    let taken_before t = t.taken_before
    let trashed_after t = t.trashed_after
    let trashed_before t = t.trashed_before
    let type_ t = t.type_
    let updated_after t = t.updated_after
    let updated_before t = t.updated_before
    let visibility t = t.visibility
    let with_deleted t = t.with_deleted
    let with_exif t = t.with_exif
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SmartSearchDto"
        (fun album_ids city country created_after created_before device_id is_encoded is_favorite is_motion is_not_in_album is_offline language lens_model library_id make model ocr page person_ids query query_asset_id rating size state tag_ids taken_after taken_before trashed_after trashed_before type_ updated_after updated_before visibility with_deleted with_exif -> { album_ids; city; country; created_after; created_before; device_id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; language; lens_model; library_id; make; model; ocr; page; person_ids; query; query_asset_id; rating; size; state; tag_ids; taken_after; taken_before; trashed_after; trashed_before; type_; updated_after; updated_before; visibility; with_deleted; with_exif })
      |> Jsont.Object.opt_mem "albumIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.album_ids)
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.opt_mem "createdAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_after)
      |> Jsont.Object.opt_mem "createdBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_before)
      |> Jsont.Object.opt_mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.opt_mem "isEncoded" Jsont.bool ~enc:(fun r -> r.is_encoded)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isMotion" Jsont.bool ~enc:(fun r -> r.is_motion)
      |> Jsont.Object.opt_mem "isNotInAlbum" Jsont.bool ~enc:(fun r -> r.is_not_in_album)
      |> Jsont.Object.opt_mem "isOffline" Jsont.bool ~enc:(fun r -> r.is_offline)
      |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun r -> r.language)
      |> Jsont.Object.mem "lensModel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.lens_model)
      |> Jsont.Object.mem "libraryId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.library_id)
      |> Jsont.Object.opt_mem "make" Jsont.string ~enc:(fun r -> r.make)
      |> Jsont.Object.mem "model" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.model)
      |> Jsont.Object.opt_mem "ocr" Jsont.string ~enc:(fun r -> r.ocr)
      |> Jsont.Object.opt_mem "page" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "personIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.person_ids)
      |> Jsont.Object.opt_mem "query" Jsont.string ~enc:(fun r -> r.query)
      |> Jsont.Object.opt_mem "queryAssetId" Jsont.string ~enc:(fun r -> r.query_asset_id)
      |> Jsont.Object.opt_mem "rating" (Openapi.Runtime.validated_float ~minimum:(-1.) ~maximum:5. Jsont.number) ~enc:(fun r -> r.rating)
      |> Jsont.Object.opt_mem "size" (Openapi.Runtime.validated_float ~minimum:1. ~maximum:1000. Jsont.number) ~enc:(fun r -> r.size)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "tagIds" (Openapi.Runtime.nullable_any (Jsont.list Jsont.string))
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.tag_ids)
      |> Jsont.Object.opt_mem "takenAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_after)
      |> Jsont.Object.opt_mem "takenBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_before)
      |> Jsont.Object.opt_mem "trashedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_after)
      |> Jsont.Object.opt_mem "trashedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_before)
      |> Jsont.Object.opt_mem "type" AssetTypeEnum.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_after)
      |> Jsont.Object.opt_mem "updatedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_before)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.opt_mem "withDeleted" Jsont.bool ~enc:(fun r -> r.with_deleted)
      |> Jsont.Object.opt_mem "withExif" Jsont.bool ~enc:(fun r -> r.with_exif)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RandomSearch = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_ids : string list option;
        city : string option;
        country : string option;
        created_after : Ptime.t option;
        created_before : Ptime.t option;
        device_id : string option;
        is_encoded : bool option;
        is_favorite : bool option;
        is_motion : bool option;
        is_not_in_album : bool option;
        is_offline : bool option;
        lens_model : string option;
        library_id : string option;
        make : string option;
        model : string option;
        ocr : string option;
        person_ids : string list option;
        rating : float option;
        size : float option;
        state : string option;
        tag_ids : string list option;
        taken_after : Ptime.t option;
        taken_before : Ptime.t option;
        trashed_after : Ptime.t option;
        trashed_before : Ptime.t option;
        type_ : AssetTypeEnum.T.t option;
        updated_after : Ptime.t option;
        updated_before : Ptime.t option;
        visibility : AssetVisibility.T.t option;
        with_deleted : bool option;
        with_exif : bool option;
        with_people : bool option;
        with_stacked : bool option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?album_ids ?city ?country ?created_after ?created_before ?device_id ?is_encoded ?is_favorite ?is_motion ?is_not_in_album ?is_offline ?lens_model ?library_id ?make ?model ?ocr ?person_ids ?rating ?size ?state ?tag_ids ?taken_after ?taken_before ?trashed_after ?trashed_before ?type_ ?updated_after ?updated_before ?visibility ?with_deleted ?with_exif ?with_people ?with_stacked () = { album_ids; city; country; created_after; created_before; device_id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; lens_model; library_id; make; model; ocr; person_ids; rating; size; state; tag_ids; taken_after; taken_before; trashed_after; trashed_before; type_; updated_after; updated_before; visibility; with_deleted; with_exif; with_people; with_stacked }
    
    let album_ids t = t.album_ids
    let city t = t.city
    let country t = t.country
    let created_after t = t.created_after
    let created_before t = t.created_before
    let device_id t = t.device_id
    let is_encoded t = t.is_encoded
    let is_favorite t = t.is_favorite
    let is_motion t = t.is_motion
    let is_not_in_album t = t.is_not_in_album
    let is_offline t = t.is_offline
    let lens_model t = t.lens_model
    let library_id t = t.library_id
    let make t = t.make
    let model t = t.model
    let ocr t = t.ocr
    let person_ids t = t.person_ids
    let rating t = t.rating
    let size t = t.size
    let state t = t.state
    let tag_ids t = t.tag_ids
    let taken_after t = t.taken_after
    let taken_before t = t.taken_before
    let trashed_after t = t.trashed_after
    let trashed_before t = t.trashed_before
    let type_ t = t.type_
    let updated_after t = t.updated_after
    let updated_before t = t.updated_before
    let visibility t = t.visibility
    let with_deleted t = t.with_deleted
    let with_exif t = t.with_exif
    let with_people t = t.with_people
    let with_stacked t = t.with_stacked
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RandomSearchDto"
        (fun album_ids city country created_after created_before device_id is_encoded is_favorite is_motion is_not_in_album is_offline lens_model library_id make model ocr person_ids rating size state tag_ids taken_after taken_before trashed_after trashed_before type_ updated_after updated_before visibility with_deleted with_exif with_people with_stacked -> { album_ids; city; country; created_after; created_before; device_id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; lens_model; library_id; make; model; ocr; person_ids; rating; size; state; tag_ids; taken_after; taken_before; trashed_after; trashed_before; type_; updated_after; updated_before; visibility; with_deleted; with_exif; with_people; with_stacked })
      |> Jsont.Object.opt_mem "albumIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.album_ids)
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.opt_mem "createdAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_after)
      |> Jsont.Object.opt_mem "createdBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_before)
      |> Jsont.Object.opt_mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.opt_mem "isEncoded" Jsont.bool ~enc:(fun r -> r.is_encoded)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isMotion" Jsont.bool ~enc:(fun r -> r.is_motion)
      |> Jsont.Object.opt_mem "isNotInAlbum" Jsont.bool ~enc:(fun r -> r.is_not_in_album)
      |> Jsont.Object.opt_mem "isOffline" Jsont.bool ~enc:(fun r -> r.is_offline)
      |> Jsont.Object.mem "lensModel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.lens_model)
      |> Jsont.Object.mem "libraryId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.library_id)
      |> Jsont.Object.opt_mem "make" Jsont.string ~enc:(fun r -> r.make)
      |> Jsont.Object.mem "model" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.model)
      |> Jsont.Object.opt_mem "ocr" Jsont.string ~enc:(fun r -> r.ocr)
      |> Jsont.Object.opt_mem "personIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.person_ids)
      |> Jsont.Object.opt_mem "rating" (Openapi.Runtime.validated_float ~minimum:(-1.) ~maximum:5. Jsont.number) ~enc:(fun r -> r.rating)
      |> Jsont.Object.opt_mem "size" (Openapi.Runtime.validated_float ~minimum:1. ~maximum:1000. Jsont.number) ~enc:(fun r -> r.size)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "tagIds" (Openapi.Runtime.nullable_any (Jsont.list Jsont.string))
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.tag_ids)
      |> Jsont.Object.opt_mem "takenAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_after)
      |> Jsont.Object.opt_mem "takenBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_before)
      |> Jsont.Object.opt_mem "trashedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_after)
      |> Jsont.Object.opt_mem "trashedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_before)
      |> Jsont.Object.opt_mem "type" AssetTypeEnum.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_after)
      |> Jsont.Object.opt_mem "updatedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_before)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.opt_mem "withDeleted" Jsont.bool ~enc:(fun r -> r.with_deleted)
      |> Jsont.Object.opt_mem "withExif" Jsont.bool ~enc:(fun r -> r.with_exif)
      |> Jsont.Object.opt_mem "withPeople" Jsont.bool ~enc:(fun r -> r.with_people)
      |> Jsont.Object.opt_mem "withStacked" Jsont.bool ~enc:(fun r -> r.with_stacked)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetStats = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        images : int;
        total : int;
        videos : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~images ~total ~videos () = { images; total; videos }
    
    let images t = t.images
    let total t = t.total
    let videos t = t.videos
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetStatsResponseDto"
        (fun images total videos -> { images; total; videos })
      |> Jsont.Object.mem "images" Jsont.int ~enc:(fun r -> r.images)
      |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.mem "videos" Jsont.int ~enc:(fun r -> r.videos)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve user statistics
  
      Retrieve asset statistics for a specific user. *)
  let get_user_statistics_admin ~id ?is_favorite ?is_trashed ?visibility client () =
    let op_name = "get_user_statistics_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}/statistics" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"isFavorite" ~value:is_favorite; Openapi.Runtime.Query.optional ~key:"isTrashed" ~value:is_trashed; Openapi.Runtime.Query.optional ~key:"visibility" ~value:visibility]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get asset statistics
  
      Retrieve various statistics about the assets owned by the authenticated user. *)
  let get_asset_statistics ?is_favorite ?is_trashed ?visibility client () =
    let op_name = "get_asset_statistics" in
    let url_path = "/assets/statistics" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"isFavorite" ~value:is_favorite; Openapi.Runtime.Query.optional ~key:"isTrashed" ~value:is_trashed; Openapi.Runtime.Query.optional ~key:"visibility" ~value:visibility]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetStack = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_count : int;
        id : string;
        primary_asset_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~asset_count ~id ~primary_asset_id () = { asset_count; id; primary_asset_id }
    
    let asset_count t = t.asset_count
    let id t = t.id
    let primary_asset_id t = t.primary_asset_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetStackResponseDto"
        (fun asset_count id primary_asset_id -> { asset_count; id; primary_asset_id })
      |> Jsont.Object.mem "assetCount" Jsont.int ~enc:(fun r -> r.asset_count)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "primaryAssetId" Jsont.string ~enc:(fun r -> r.primary_asset_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetOrder = struct
  module Types = struct
    module T = struct
      type t = [
        | `Asc
        | `Desc
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetOrder"
        ~dec:(function
          | "asc" -> `Asc
          | "desc" -> `Desc
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Asc -> "asc"
          | `Desc -> "desc")
  end
end

module UpdateAlbum = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_name : string option;
        album_thumbnail_asset_id : string option;
        description : string option;
        is_activity_enabled : bool option;
        order : AssetOrder.T.t option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?album_name ?album_thumbnail_asset_id ?description ?is_activity_enabled ?order () = { album_name; album_thumbnail_asset_id; description; is_activity_enabled; order }
    
    let album_name t = t.album_name
    let album_thumbnail_asset_id t = t.album_thumbnail_asset_id
    let description t = t.description
    let is_activity_enabled t = t.is_activity_enabled
    let order t = t.order
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UpdateAlbumDto"
        (fun album_name album_thumbnail_asset_id description is_activity_enabled order -> { album_name; album_thumbnail_asset_id; description; is_activity_enabled; order })
      |> Jsont.Object.opt_mem "albumName" Jsont.string ~enc:(fun r -> r.album_name)
      |> Jsont.Object.opt_mem "albumThumbnailAssetId" Jsont.string ~enc:(fun r -> r.album_thumbnail_asset_id)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "isActivityEnabled" Jsont.bool ~enc:(fun r -> r.is_activity_enabled)
      |> Jsont.Object.opt_mem "order" AssetOrder.T.jsont ~enc:(fun r -> r.order)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAlbumV1 = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t;
        description : string;
        id : string;
        is_activity_enabled : bool;
        name : string;
        order : AssetOrder.T.t;
        owner_id : string;
        thumbnail_asset_id : string option;
        updated_at : Ptime.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~created_at ~description ~id ~is_activity_enabled ~name ~order ~owner_id ~updated_at ?thumbnail_asset_id () = { created_at; description; id; is_activity_enabled; name; order; owner_id; thumbnail_asset_id; updated_at }
    
    let created_at t = t.created_at
    let description t = t.description
    let id t = t.id
    let is_activity_enabled t = t.is_activity_enabled
    let name t = t.name
    let order t = t.order
    let owner_id t = t.owner_id
    let thumbnail_asset_id t = t.thumbnail_asset_id
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAlbumV1"
        (fun created_at description id is_activity_enabled name order owner_id thumbnail_asset_id updated_at -> { created_at; description; id; is_activity_enabled; name; order; owner_id; thumbnail_asset_id; updated_at })
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isActivityEnabled" Jsont.bool ~enc:(fun r -> r.is_activity_enabled)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "order" AssetOrder.T.jsont ~enc:(fun r -> r.order)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "thumbnailAssetId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.thumbnail_asset_id)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MetadataSearch = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_ids : string list option;
        checksum : string option;
        city : string option;
        country : string option;
        created_after : Ptime.t option;
        created_before : Ptime.t option;
        description : string option;
        device_asset_id : string option;
        device_id : string option;
        encoded_video_path : string option;
        id : string option;
        is_encoded : bool option;
        is_favorite : bool option;
        is_motion : bool option;
        is_not_in_album : bool option;
        is_offline : bool option;
        lens_model : string option;
        library_id : string option;
        make : string option;
        model : string option;
        ocr : string option;
        order : AssetOrder.T.t;
        original_file_name : string option;
        original_path : string option;
        page : float option;
        person_ids : string list option;
        preview_path : string option;
        rating : float option;
        size : float option;
        state : string option;
        tag_ids : string list option;
        taken_after : Ptime.t option;
        taken_before : Ptime.t option;
        thumbnail_path : string option;
        trashed_after : Ptime.t option;
        trashed_before : Ptime.t option;
        type_ : AssetTypeEnum.T.t option;
        updated_after : Ptime.t option;
        updated_before : Ptime.t option;
        visibility : AssetVisibility.T.t option;
        with_deleted : bool option;
        with_exif : bool option;
        with_people : bool option;
        with_stacked : bool option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ?(order=`Desc) ?album_ids ?checksum ?city ?country ?created_after ?created_before ?description ?device_asset_id ?device_id ?encoded_video_path ?id ?is_encoded ?is_favorite ?is_motion ?is_not_in_album ?is_offline ?lens_model ?library_id ?make ?model ?ocr ?original_file_name ?original_path ?page ?person_ids ?preview_path ?rating ?size ?state ?tag_ids ?taken_after ?taken_before ?thumbnail_path ?trashed_after ?trashed_before ?type_ ?updated_after ?updated_before ?visibility ?with_deleted ?with_exif ?with_people ?with_stacked () = { album_ids; checksum; city; country; created_after; created_before; description; device_asset_id; device_id; encoded_video_path; id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; lens_model; library_id; make; model; ocr; order; original_file_name; original_path; page; person_ids; preview_path; rating; size; state; tag_ids; taken_after; taken_before; thumbnail_path; trashed_after; trashed_before; type_; updated_after; updated_before; visibility; with_deleted; with_exif; with_people; with_stacked }
    
    let album_ids t = t.album_ids
    let checksum t = t.checksum
    let city t = t.city
    let country t = t.country
    let created_after t = t.created_after
    let created_before t = t.created_before
    let description t = t.description
    let device_asset_id t = t.device_asset_id
    let device_id t = t.device_id
    let encoded_video_path t = t.encoded_video_path
    let id t = t.id
    let is_encoded t = t.is_encoded
    let is_favorite t = t.is_favorite
    let is_motion t = t.is_motion
    let is_not_in_album t = t.is_not_in_album
    let is_offline t = t.is_offline
    let lens_model t = t.lens_model
    let library_id t = t.library_id
    let make t = t.make
    let model t = t.model
    let ocr t = t.ocr
    let order t = t.order
    let original_file_name t = t.original_file_name
    let original_path t = t.original_path
    let page t = t.page
    let person_ids t = t.person_ids
    let preview_path t = t.preview_path
    let rating t = t.rating
    let size t = t.size
    let state t = t.state
    let tag_ids t = t.tag_ids
    let taken_after t = t.taken_after
    let taken_before t = t.taken_before
    let thumbnail_path t = t.thumbnail_path
    let trashed_after t = t.trashed_after
    let trashed_before t = t.trashed_before
    let type_ t = t.type_
    let updated_after t = t.updated_after
    let updated_before t = t.updated_before
    let visibility t = t.visibility
    let with_deleted t = t.with_deleted
    let with_exif t = t.with_exif
    let with_people t = t.with_people
    let with_stacked t = t.with_stacked
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MetadataSearchDto"
        (fun album_ids checksum city country created_after created_before description device_asset_id device_id encoded_video_path id is_encoded is_favorite is_motion is_not_in_album is_offline lens_model library_id make model ocr order original_file_name original_path page person_ids preview_path rating size state tag_ids taken_after taken_before thumbnail_path trashed_after trashed_before type_ updated_after updated_before visibility with_deleted with_exif with_people with_stacked -> { album_ids; checksum; city; country; created_after; created_before; description; device_asset_id; device_id; encoded_video_path; id; is_encoded; is_favorite; is_motion; is_not_in_album; is_offline; lens_model; library_id; make; model; ocr; order; original_file_name; original_path; page; person_ids; preview_path; rating; size; state; tag_ids; taken_after; taken_before; thumbnail_path; trashed_after; trashed_before; type_; updated_after; updated_before; visibility; with_deleted; with_exif; with_people; with_stacked })
      |> Jsont.Object.opt_mem "albumIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.album_ids)
      |> Jsont.Object.opt_mem "checksum" Jsont.string ~enc:(fun r -> r.checksum)
      |> Jsont.Object.mem "city" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.city)
      |> Jsont.Object.mem "country" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.country)
      |> Jsont.Object.opt_mem "createdAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_after)
      |> Jsont.Object.opt_mem "createdBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_before)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "deviceAssetId" Jsont.string ~enc:(fun r -> r.device_asset_id)
      |> Jsont.Object.opt_mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.opt_mem "encodedVideoPath" Jsont.string ~enc:(fun r -> r.encoded_video_path)
      |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isEncoded" Jsont.bool ~enc:(fun r -> r.is_encoded)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isMotion" Jsont.bool ~enc:(fun r -> r.is_motion)
      |> Jsont.Object.opt_mem "isNotInAlbum" Jsont.bool ~enc:(fun r -> r.is_not_in_album)
      |> Jsont.Object.opt_mem "isOffline" Jsont.bool ~enc:(fun r -> r.is_offline)
      |> Jsont.Object.mem "lensModel" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.lens_model)
      |> Jsont.Object.mem "libraryId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.library_id)
      |> Jsont.Object.opt_mem "make" Jsont.string ~enc:(fun r -> r.make)
      |> Jsont.Object.mem "model" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.model)
      |> Jsont.Object.opt_mem "ocr" Jsont.string ~enc:(fun r -> r.ocr)
      |> Jsont.Object.mem "order" AssetOrder.T.jsont ~dec_absent:`Desc ~enc:(fun r -> r.order)
      |> Jsont.Object.opt_mem "originalFileName" Jsont.string ~enc:(fun r -> r.original_file_name)
      |> Jsont.Object.opt_mem "originalPath" Jsont.string ~enc:(fun r -> r.original_path)
      |> Jsont.Object.opt_mem "page" (Openapi.Runtime.validated_float ~minimum:1. Jsont.number) ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "personIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.person_ids)
      |> Jsont.Object.opt_mem "previewPath" Jsont.string ~enc:(fun r -> r.preview_path)
      |> Jsont.Object.opt_mem "rating" (Openapi.Runtime.validated_float ~minimum:(-1.) ~maximum:5. Jsont.number) ~enc:(fun r -> r.rating)
      |> Jsont.Object.opt_mem "size" (Openapi.Runtime.validated_float ~minimum:1. ~maximum:1000. Jsont.number) ~enc:(fun r -> r.size)
      |> Jsont.Object.mem "state" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.state)
      |> Jsont.Object.mem "tagIds" (Openapi.Runtime.nullable_any (Jsont.list Jsont.string))
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.tag_ids)
      |> Jsont.Object.opt_mem "takenAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_after)
      |> Jsont.Object.opt_mem "takenBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.taken_before)
      |> Jsont.Object.opt_mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.opt_mem "trashedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_after)
      |> Jsont.Object.opt_mem "trashedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.trashed_before)
      |> Jsont.Object.opt_mem "type" AssetTypeEnum.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_after)
      |> Jsont.Object.opt_mem "updatedBefore" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_before)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.opt_mem "withDeleted" Jsont.bool ~enc:(fun r -> r.with_deleted)
      |> Jsont.Object.opt_mem "withExif" Jsont.bool ~enc:(fun r -> r.with_exif)
      |> Jsont.Object.opt_mem "withPeople" Jsont.bool ~enc:(fun r -> r.with_people)
      |> Jsont.Object.opt_mem "withStacked" Jsont.bool ~enc:(fun r -> r.with_stacked)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Albums = struct
  module Types = struct
    module Update = struct
      type t = {
        default_asset_order : AssetOrder.T.t option;
      }
    end
  
    module Response = struct
      type t = {
        default_asset_order : AssetOrder.T.t;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?default_asset_order () = { default_asset_order }
    
    let default_asset_order t = t.default_asset_order
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumsUpdate"
        (fun default_asset_order -> { default_asset_order })
      |> Jsont.Object.opt_mem "defaultAssetOrder" AssetOrder.T.jsont ~enc:(fun r -> r.default_asset_order)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(default_asset_order=`Desc) () = { default_asset_order }
    
    let default_asset_order t = t.default_asset_order
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumsResponse"
        (fun default_asset_order -> { default_asset_order })
      |> Jsont.Object.mem "defaultAssetOrder" AssetOrder.T.jsont ~dec_absent:`Desc ~enc:(fun r -> r.default_asset_order)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetOcr = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_id : string;
        box_score : float;  (** Confidence score for text detection box *)
        id : string;
        text : string;  (** Recognized text *)
        text_score : float;  (** Confidence score for text recognition *)
        x1 : float;  (** Normalized x coordinate of box corner 1 (0-1) *)
        x2 : float;  (** Normalized x coordinate of box corner 2 (0-1) *)
        x3 : float;  (** Normalized x coordinate of box corner 3 (0-1) *)
        x4 : float;  (** Normalized x coordinate of box corner 4 (0-1) *)
        y1 : float;  (** Normalized y coordinate of box corner 1 (0-1) *)
        y2 : float;  (** Normalized y coordinate of box corner 2 (0-1) *)
        y3 : float;  (** Normalized y coordinate of box corner 3 (0-1) *)
        y4 : float;  (** Normalized y coordinate of box corner 4 (0-1) *)
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~asset_id ~box_score ~id ~text ~text_score ~x1 ~x2 ~x3 ~x4 ~y1 ~y2 ~y3 ~y4 () = { asset_id; box_score; id; text; text_score; x1; x2; x3; x4; y1; y2; y3; y4 }
    
    let asset_id t = t.asset_id
    let box_score t = t.box_score
    let id t = t.id
    let text t = t.text
    let text_score t = t.text_score
    let x1 t = t.x1
    let x2 t = t.x2
    let x3 t = t.x3
    let x4 t = t.x4
    let y1 t = t.y1
    let y2 t = t.y2
    let y3 t = t.y3
    let y4 t = t.y4
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetOcrResponseDto"
        (fun asset_id box_score id text text_score x1 x2 x3 x4 y1 y2 y3 y4 -> { asset_id; box_score; id; text; text_score; x1; x2; x3; x4; y1; y2; y3; y4 })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "boxScore" Jsont.number ~enc:(fun r -> r.box_score)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "text" Jsont.string ~enc:(fun r -> r.text)
      |> Jsont.Object.mem "textScore" Jsont.number ~enc:(fun r -> r.text_score)
      |> Jsont.Object.mem "x1" Jsont.number ~enc:(fun r -> r.x1)
      |> Jsont.Object.mem "x2" Jsont.number ~enc:(fun r -> r.x2)
      |> Jsont.Object.mem "x3" Jsont.number ~enc:(fun r -> r.x3)
      |> Jsont.Object.mem "x4" Jsont.number ~enc:(fun r -> r.x4)
      |> Jsont.Object.mem "y1" Jsont.number ~enc:(fun r -> r.y1)
      |> Jsont.Object.mem "y2" Jsont.number ~enc:(fun r -> r.y2)
      |> Jsont.Object.mem "y3" Jsont.number ~enc:(fun r -> r.y3)
      |> Jsont.Object.mem "y4" Jsont.number ~enc:(fun r -> r.y4)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve asset OCR data
  
      Retrieve all OCR (Optical Character Recognition) data associated with the specified asset. *)
  let get_asset_ocr ~id client () =
    let op_name = "get_asset_ocr" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/ocr" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetMetadataUpsertItem = struct
  module Types = struct
    module Dto = struct
      type t = {
        key : string;
        value : Jsont.json;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~key ~value () = { key; value }
    
    let key t = t.key
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataUpsertItemDto"
        (fun key value -> { key; value })
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetMetadataUpsert = struct
  module Types = struct
    module Dto = struct
      type t = {
        items : AssetMetadataUpsertItem.Dto.t list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~items () = { items }
    
    let items t = t.items
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataUpsertDto"
        (fun items -> { items })
      |> Jsont.Object.mem "items" (Jsont.list AssetMetadataUpsertItem.Dto.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetMetadata = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        key : string;
        updated_at : Ptime.t;
        value : Jsont.json;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~key ~updated_at ~value () = { key; updated_at; value }
    
    let key t = t.key
    let updated_at t = t.updated_at
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataResponseDto"
        (fun key updated_at value -> { key; updated_at; value })
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get asset metadata
  
      Retrieve all metadata key-value pairs associated with the specified asset. *)
  let get_asset_metadata ~id client () =
    let op_name = "get_asset_metadata" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/metadata" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update asset metadata
  
      Update or add metadata key-value pairs for the specified asset. *)
  let update_asset_metadata ~id ~body client () =
    let op_name = "update_asset_metadata" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/metadata" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetMetadataUpsert.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve asset metadata by key
  
      Retrieve the value of a specific metadata key associated with the specified asset. *)
  let get_asset_metadata_by_key ~id ~key client () =
    let op_name = "get_asset_metadata_by_key" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("key", key)] "/assets/{id}/metadata/{key}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetMedia = struct
  module Types = struct
    module Status = struct
      type t = [
        | `Created
        | `Replaced
        | `Duplicate
      ]
    end
  
    module ResponseDto = struct
      type t = {
        id : string;
        status : Status.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        asset_data : string;
        device_asset_id : string;
        device_id : string;
        duration : string option;
        file_created_at : Ptime.t;
        file_modified_at : Ptime.t;
        filename : string option;
        is_favorite : bool option;
        live_photo_video_id : string option;
        metadata : AssetMetadataUpsertItem.Dto.t list option;
        sidecar_data : string option;
        visibility : AssetVisibility.T.t option;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetMediaStatus"
        ~dec:(function
          | "created" -> `Created
          | "replaced" -> `Replaced
          | "duplicate" -> `Duplicate
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Created -> "created"
          | `Replaced -> "replaced"
          | `Duplicate -> "duplicate")
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~id ~status () = { id; status }
    
    let id t = t.id
    let status t = t.status
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMediaResponseDto"
        (fun id status -> { id; status })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "status" Status.jsont ~enc:(fun r -> r.status)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~asset_data ~device_asset_id ~device_id ~file_created_at ~file_modified_at ?duration ?filename ?is_favorite ?live_photo_video_id ?metadata ?sidecar_data ?visibility () = { asset_data; device_asset_id; device_id; duration; file_created_at; file_modified_at; filename; is_favorite; live_photo_video_id; metadata; sidecar_data; visibility }
    
    let asset_data t = t.asset_data
    let device_asset_id t = t.device_asset_id
    let device_id t = t.device_id
    let duration t = t.duration
    let file_created_at t = t.file_created_at
    let file_modified_at t = t.file_modified_at
    let filename t = t.filename
    let is_favorite t = t.is_favorite
    let live_photo_video_id t = t.live_photo_video_id
    let metadata t = t.metadata
    let sidecar_data t = t.sidecar_data
    let visibility t = t.visibility
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMediaCreateDto"
        (fun asset_data device_asset_id device_id duration file_created_at file_modified_at filename is_favorite live_photo_video_id metadata sidecar_data visibility -> { asset_data; device_asset_id; device_id; duration; file_created_at; file_modified_at; filename; is_favorite; live_photo_video_id; metadata; sidecar_data; visibility })
      |> Jsont.Object.mem "assetData" Jsont.string ~enc:(fun r -> r.asset_data)
      |> Jsont.Object.mem "deviceAssetId" Jsont.string ~enc:(fun r -> r.device_asset_id)
      |> Jsont.Object.mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.opt_mem "duration" Jsont.string ~enc:(fun r -> r.duration)
      |> Jsont.Object.mem "fileCreatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.file_created_at)
      |> Jsont.Object.mem "fileModifiedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.file_modified_at)
      |> Jsont.Object.opt_mem "filename" Jsont.string ~enc:(fun r -> r.filename)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "livePhotoVideoId" Jsont.string ~enc:(fun r -> r.live_photo_video_id)
      |> Jsont.Object.opt_mem "metadata" (Jsont.list AssetMetadataUpsertItem.Dto.jsont) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.opt_mem "sidecarData" Jsont.string ~enc:(fun r -> r.sidecar_data)
      |> Jsont.Object.opt_mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Upload asset
  
      Uploads a new asset to the server. *)
  let upload_asset ?key ?slug client () =
    let op_name = "upload_asset" in
    let url_path = "/assets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Replace asset
  
      Replace the asset with new file, without changing its id. *)
  let replace_asset ~id ?key ?slug client () =
    let op_name = "replace_asset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/original" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetMetadataBulkUpsertItem = struct
  module Types = struct
    module Dto = struct
      type t = {
        asset_id : string;
        key : string;
        value : Jsont.json;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_id ~key ~value () = { asset_id; key; value }
    
    let asset_id t = t.asset_id
    let key t = t.key
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataBulkUpsertItemDto"
        (fun asset_id key value -> { asset_id; key; value })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetMetadataBulkUpsert = struct
  module Types = struct
    module Dto = struct
      type t = {
        items : AssetMetadataBulkUpsertItem.Dto.t list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~items () = { items }
    
    let items t = t.items
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataBulkUpsertDto"
        (fun items -> { items })
      |> Jsont.Object.mem "items" (Jsont.list AssetMetadataBulkUpsertItem.Dto.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetMetadataBulk = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_id : string;
        key : string;
        updated_at : Ptime.t;
        value : Jsont.json;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~asset_id ~key ~updated_at ~value () = { asset_id; key; updated_at; value }
    
    let asset_id t = t.asset_id
    let key t = t.key
    let updated_at t = t.updated_at
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataBulkResponseDto"
        (fun asset_id key updated_at value -> { asset_id; key; updated_at; value })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Upsert asset metadata
  
      Upsert metadata key-value pairs for multiple assets. *)
  let update_bulk_asset_metadata ~body client () =
    let op_name = "update_bulk_asset_metadata" in
    let url_path = "/assets/metadata" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetMetadataBulkUpsert.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetMetadataBulkDeleteItem = struct
  module Types = struct
    module Dto = struct
      type t = {
        asset_id : string;
        key : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_id ~key () = { asset_id; key }
    
    let asset_id t = t.asset_id
    let key t = t.key
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataBulkDeleteItemDto"
        (fun asset_id key -> { asset_id; key })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetMetadataBulkDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        items : AssetMetadataBulkDeleteItem.Dto.t list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~items () = { items }
    
    let items t = t.items
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMetadataBulkDeleteDto"
        (fun items -> { items })
      |> Jsont.Object.mem "items" (Jsont.list AssetMetadataBulkDeleteItem.Dto.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetMediaSize = struct
  module Types = struct
    module T = struct
      type t = [
        | `Original
        | `Fullsize
        | `Preview
        | `Thumbnail
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetMediaSize"
        ~dec:(function
          | "original" -> `Original
          | "fullsize" -> `Fullsize
          | "preview" -> `Preview
          | "thumbnail" -> `Thumbnail
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Original -> "original"
          | `Fullsize -> "fullsize"
          | `Preview -> "preview"
          | `Thumbnail -> "thumbnail")
  end
end

module AssetMediaReplace = struct
  module Types = struct
    module Dto = struct
      type t = {
        asset_data : string;
        device_asset_id : string;
        device_id : string;
        duration : string option;
        file_created_at : Ptime.t;
        file_modified_at : Ptime.t;
        filename : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_data ~device_asset_id ~device_id ~file_created_at ~file_modified_at ?duration ?filename () = { asset_data; device_asset_id; device_id; duration; file_created_at; file_modified_at; filename }
    
    let asset_data t = t.asset_data
    let device_asset_id t = t.device_asset_id
    let device_id t = t.device_id
    let duration t = t.duration
    let file_created_at t = t.file_created_at
    let file_modified_at t = t.file_modified_at
    let filename t = t.filename
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetMediaReplaceDto"
        (fun asset_data device_asset_id device_id duration file_created_at file_modified_at filename -> { asset_data; device_asset_id; device_id; duration; file_created_at; file_modified_at; filename })
      |> Jsont.Object.mem "assetData" Jsont.string ~enc:(fun r -> r.asset_data)
      |> Jsont.Object.mem "deviceAssetId" Jsont.string ~enc:(fun r -> r.device_asset_id)
      |> Jsont.Object.mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.opt_mem "duration" Jsont.string ~enc:(fun r -> r.duration)
      |> Jsont.Object.mem "fileCreatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.file_created_at)
      |> Jsont.Object.mem "fileModifiedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.file_modified_at)
      |> Jsont.Object.opt_mem "filename" Jsont.string ~enc:(fun r -> r.filename)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetJobName = struct
  module Types = struct
    module T = struct
      type t = [
        | `Refresh_faces
        | `Refresh_metadata
        | `Regenerate_thumbnail
        | `Transcode_video
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetJobName"
        ~dec:(function
          | "refresh-faces" -> `Refresh_faces
          | "refresh-metadata" -> `Refresh_metadata
          | "regenerate-thumbnail" -> `Regenerate_thumbnail
          | "transcode-video" -> `Transcode_video
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Refresh_faces -> "refresh-faces"
          | `Refresh_metadata -> "refresh-metadata"
          | `Regenerate_thumbnail -> "regenerate-thumbnail"
          | `Transcode_video -> "transcode-video")
  end
end

module AssetJobs = struct
  module Types = struct
    module Dto = struct
      type t = {
        asset_ids : string list;
        name : AssetJobName.T.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_ids ~name () = { asset_ids; name }
    
    let asset_ids t = t.asset_ids
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetJobsDto"
        (fun asset_ids name -> { asset_ids; name })
      |> Jsont.Object.mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.mem "name" AssetJobName.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetIds = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        asset_id : string;
        error : string option;
        success : bool;
      }
    end
  
    module Dto = struct
      type t = {
        asset_ids : string list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~asset_id ~success ?error () = { asset_id; error; success }
    
    let asset_id t = t.asset_id
    let error t = t.error
    let success t = t.success
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetIdsResponseDto"
        (fun asset_id error success -> { asset_id; error; success })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
      |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_ids () = { asset_ids }
    
    let asset_ids t = t.asset_ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetIdsDto"
        (fun asset_ids -> { asset_ids })
      |> Jsont.Object.mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Add assets to a shared link
  
      Add assets to a specific shared link by its ID. This endpoint is only relevant for shared link of type individual. *)
  let add_shared_link_assets ~id ?key ?slug ~body client () =
    let op_name = "add_shared_link_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/shared-links/{id}/assets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove assets from a shared link
  
      Remove assets from a specific shared link by its ID. This endpoint is only relevant for shared link of type individual. *)
  let remove_shared_link_assets ~id ?key ?slug client () =
    let op_name = "remove_shared_link_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/shared-links/{id}/assets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetFullSync = struct
  module Types = struct
    module Dto = struct
      type t = {
        last_id : string option;
        limit : int;
        updated_until : Ptime.t;
        user_id : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~limit ~updated_until ?last_id ?user_id () = { last_id; limit; updated_until; user_id }
    
    let last_id t = t.last_id
    let limit t = t.limit
    let updated_until t = t.updated_until
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFullSyncDto"
        (fun last_id limit updated_until user_id -> { last_id; limit; updated_until; user_id })
      |> Jsont.Object.opt_mem "lastId" Jsont.string ~enc:(fun r -> r.last_id)
      |> Jsont.Object.mem "limit" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.limit)
      |> Jsont.Object.mem "updatedUntil" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_until)
      |> Jsont.Object.opt_mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Asset = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        checksum : string;  (** base64 encoded sha1 hash *)
        created_at : Ptime.t;  (** The UTC timestamp when the asset was originally uploaded to Immich. *)
        device_asset_id : string;
        device_id : string;
        duplicate_id : string option;
        duration : string;
        exif_info : Exif.ResponseDto.t option;
        file_created_at : Ptime.t;  (** The actual UTC timestamp when the file was created/captured, preserving timezone information. This is the authoritative timestamp for chronological sorting within timeline groups. Combined with timezone data, this can be used to determine the exact moment the photo was taken. *)
        file_modified_at : Ptime.t;  (** The UTC timestamp when the file was last modified on the filesystem. This reflects the last time the physical file was changed, which may be different from when the photo was originally taken. *)
        has_metadata : bool;
        height : float option;
        id : string;
        is_archived : bool;
        is_edited : bool;
        is_favorite : bool;
        is_offline : bool;
        is_trashed : bool;
        library_id : string option;
        live_photo_video_id : string option;
        local_date_time : Ptime.t;  (** The local date and time when the photo/video was taken, derived from EXIF metadata. This represents the photographer's local time regardless of timezone, stored as a timezone-agnostic timestamp. Used for timeline grouping by "local" days and months. *)
        original_file_name : string;
        original_mime_type : string option;
        original_path : string;
        owner : User.ResponseDto.t option;
        owner_id : string;
        people : PersonWithFaces.ResponseDto.t list option;
        resized : bool option;
        stack : AssetStack.ResponseDto.t option;
        tags : Tag.ResponseDto.t list option;
        thumbhash : string option;
        type_ : AssetTypeEnum.T.t;
        unassigned_faces : AssetFaceWithoutPerson.ResponseDto.t list option;
        updated_at : Ptime.t;  (** The UTC timestamp when the asset record was last updated in the database. This is automatically maintained by the database and reflects when any field in the asset was last modified. *)
        visibility : AssetVisibility.T.t;
        width : float option;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~checksum ~created_at ~device_asset_id ~device_id ~duration ~file_created_at ~file_modified_at ~has_metadata ~id ~is_archived ~is_edited ~is_favorite ~is_offline ~is_trashed ~local_date_time ~original_file_name ~original_path ~owner_id ~type_ ~updated_at ~visibility ?duplicate_id ?exif_info ?height ?library_id ?live_photo_video_id ?original_mime_type ?owner ?people ?resized ?stack ?tags ?thumbhash ?unassigned_faces ?width () = { checksum; created_at; device_asset_id; device_id; duplicate_id; duration; exif_info; file_created_at; file_modified_at; has_metadata; height; id; is_archived; is_edited; is_favorite; is_offline; is_trashed; library_id; live_photo_video_id; local_date_time; original_file_name; original_mime_type; original_path; owner; owner_id; people; resized; stack; tags; thumbhash; type_; unassigned_faces; updated_at; visibility; width }
    
    let checksum t = t.checksum
    let created_at t = t.created_at
    let device_asset_id t = t.device_asset_id
    let device_id t = t.device_id
    let duplicate_id t = t.duplicate_id
    let duration t = t.duration
    let exif_info t = t.exif_info
    let file_created_at t = t.file_created_at
    let file_modified_at t = t.file_modified_at
    let has_metadata t = t.has_metadata
    let height t = t.height
    let id t = t.id
    let is_archived t = t.is_archived
    let is_edited t = t.is_edited
    let is_favorite t = t.is_favorite
    let is_offline t = t.is_offline
    let is_trashed t = t.is_trashed
    let library_id t = t.library_id
    let live_photo_video_id t = t.live_photo_video_id
    let local_date_time t = t.local_date_time
    let original_file_name t = t.original_file_name
    let original_mime_type t = t.original_mime_type
    let original_path t = t.original_path
    let owner t = t.owner
    let owner_id t = t.owner_id
    let people t = t.people
    let resized t = t.resized
    let stack t = t.stack
    let tags t = t.tags
    let thumbhash t = t.thumbhash
    let type_ t = t.type_
    let unassigned_faces t = t.unassigned_faces
    let updated_at t = t.updated_at
    let visibility t = t.visibility
    let width t = t.width
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetResponseDto"
        (fun checksum created_at device_asset_id device_id duplicate_id duration exif_info file_created_at file_modified_at has_metadata height id is_archived is_edited is_favorite is_offline is_trashed library_id live_photo_video_id local_date_time original_file_name original_mime_type original_path owner owner_id people resized stack tags thumbhash type_ unassigned_faces updated_at visibility width -> { checksum; created_at; device_asset_id; device_id; duplicate_id; duration; exif_info; file_created_at; file_modified_at; has_metadata; height; id; is_archived; is_edited; is_favorite; is_offline; is_trashed; library_id; live_photo_video_id; local_date_time; original_file_name; original_mime_type; original_path; owner; owner_id; people; resized; stack; tags; thumbhash; type_; unassigned_faces; updated_at; visibility; width })
      |> Jsont.Object.mem "checksum" Jsont.string ~enc:(fun r -> r.checksum)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "deviceAssetId" Jsont.string ~enc:(fun r -> r.device_asset_id)
      |> Jsont.Object.mem "deviceId" Jsont.string ~enc:(fun r -> r.device_id)
      |> Jsont.Object.mem "duplicateId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.duplicate_id)
      |> Jsont.Object.mem "duration" Jsont.string ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "exifInfo" Exif.ResponseDto.jsont ~enc:(fun r -> r.exif_info)
      |> Jsont.Object.mem "fileCreatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.file_created_at)
      |> Jsont.Object.mem "fileModifiedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.file_modified_at)
      |> Jsont.Object.mem "hasMetadata" Jsont.bool ~enc:(fun r -> r.has_metadata)
      |> Jsont.Object.mem "height" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.height)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isArchived" Jsont.bool ~enc:(fun r -> r.is_archived)
      |> Jsont.Object.mem "isEdited" Jsont.bool ~enc:(fun r -> r.is_edited)
      |> Jsont.Object.mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.mem "isOffline" Jsont.bool ~enc:(fun r -> r.is_offline)
      |> Jsont.Object.mem "isTrashed" Jsont.bool ~enc:(fun r -> r.is_trashed)
      |> Jsont.Object.mem "libraryId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.library_id)
      |> Jsont.Object.mem "livePhotoVideoId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.live_photo_video_id)
      |> Jsont.Object.mem "localDateTime" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.local_date_time)
      |> Jsont.Object.mem "originalFileName" Jsont.string ~enc:(fun r -> r.original_file_name)
      |> Jsont.Object.opt_mem "originalMimeType" Jsont.string ~enc:(fun r -> r.original_mime_type)
      |> Jsont.Object.mem "originalPath" Jsont.string ~enc:(fun r -> r.original_path)
      |> Jsont.Object.opt_mem "owner" User.ResponseDto.jsont ~enc:(fun r -> r.owner)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.opt_mem "people" (Jsont.list PersonWithFaces.ResponseDto.jsont) ~enc:(fun r -> r.people)
      |> Jsont.Object.opt_mem "resized" Jsont.bool ~enc:(fun r -> r.resized)
      |> Jsont.Object.opt_mem "stack" AssetStack.ResponseDto.jsont ~enc:(fun r -> r.stack)
      |> Jsont.Object.opt_mem "tags" (Jsont.list Tag.ResponseDto.jsont) ~enc:(fun r -> r.tags)
      |> Jsont.Object.mem "thumbhash" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.thumbhash)
      |> Jsont.Object.mem "type" AssetTypeEnum.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "unassignedFaces" (Jsont.list AssetFaceWithoutPerson.ResponseDto.jsont) ~enc:(fun r -> r.unassigned_faces)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "visibility" AssetVisibility.T.jsont ~enc:(fun r -> r.visibility)
      |> Jsont.Object.mem "width" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get random assets
  
      Retrieve a specified number of random assets for the authenticated user. *)
  let get_random ?count client () =
    let op_name = "get_random" in
    let url_path = "/assets/random" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"count" ~value:count]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve an asset
  
      Retrieve detailed information about a specific asset. *)
  let get_asset_info ~id ?key ?slug client () =
    let op_name = "get_asset_info" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update an asset
  
      Update information of a specific asset. *)
  let update_asset ~id ~body client () =
    let op_name = "update_asset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateAsset.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve assets by city
  
      Retrieve a list of assets with each asset belonging to a different city. This endpoint is used on the places pages to show a single thumbnail for each city the user has assets in. *)
  let get_assets_by_city client () =
    let op_name = "get_assets_by_city" in
    let url_path = "/search/cities" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Search large assets
  
      Search for assets that are considered large based on specified criteria. *)
  let search_large_assets ?album_ids ?city ?country ?created_after ?created_before ?device_id ?is_encoded ?is_favorite ?is_motion ?is_not_in_album ?is_offline ?lens_model ?library_id ?make ?min_file_size ?model ?ocr ?person_ids ?rating ?size ?state ?tag_ids ?taken_after ?taken_before ?trashed_after ?trashed_before ?type_ ?updated_after ?updated_before ?visibility ?with_deleted ?with_exif client () =
    let op_name = "search_large_assets" in
    let url_path = "/search/large-assets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"albumIds" ~value:album_ids; Openapi.Runtime.Query.optional ~key:"city" ~value:city; Openapi.Runtime.Query.optional ~key:"country" ~value:country; Openapi.Runtime.Query.optional ~key:"createdAfter" ~value:created_after; Openapi.Runtime.Query.optional ~key:"createdBefore" ~value:created_before; Openapi.Runtime.Query.optional ~key:"deviceId" ~value:device_id; Openapi.Runtime.Query.optional ~key:"isEncoded" ~value:is_encoded; Openapi.Runtime.Query.optional ~key:"isFavorite" ~value:is_favorite; Openapi.Runtime.Query.optional ~key:"isMotion" ~value:is_motion; Openapi.Runtime.Query.optional ~key:"isNotInAlbum" ~value:is_not_in_album; Openapi.Runtime.Query.optional ~key:"isOffline" ~value:is_offline; Openapi.Runtime.Query.optional ~key:"lensModel" ~value:lens_model; Openapi.Runtime.Query.optional ~key:"libraryId" ~value:library_id; Openapi.Runtime.Query.optional ~key:"make" ~value:make; Openapi.Runtime.Query.optional ~key:"minFileSize" ~value:min_file_size; Openapi.Runtime.Query.optional ~key:"model" ~value:model; Openapi.Runtime.Query.optional ~key:"ocr" ~value:ocr; Openapi.Runtime.Query.optional ~key:"personIds" ~value:person_ids; Openapi.Runtime.Query.optional ~key:"rating" ~value:rating; Openapi.Runtime.Query.optional ~key:"size" ~value:size; Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"tagIds" ~value:tag_ids; Openapi.Runtime.Query.optional ~key:"takenAfter" ~value:taken_after; Openapi.Runtime.Query.optional ~key:"takenBefore" ~value:taken_before; Openapi.Runtime.Query.optional ~key:"trashedAfter" ~value:trashed_after; Openapi.Runtime.Query.optional ~key:"trashedBefore" ~value:trashed_before; Openapi.Runtime.Query.optional ~key:"type" ~value:type_; Openapi.Runtime.Query.optional ~key:"updatedAfter" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"updatedBefore" ~value:updated_before; Openapi.Runtime.Query.optional ~key:"visibility" ~value:visibility; Openapi.Runtime.Query.optional ~key:"withDeleted" ~value:with_deleted; Openapi.Runtime.Query.optional ~key:"withExif" ~value:with_exif]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Search random assets
  
      Retrieve a random selection of assets based on the provided criteria. *)
  let search_random ~body client () =
    let op_name = "search_random" in
    let url_path = "/search/random" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json RandomSearch.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get full sync for user
  
      Retrieve all assets for a full synchronization for the authenticated user. *)
  let get_full_sync_for_user ~body client () =
    let op_name = "get_full_sync_for_user" in
    let url_path = "/sync/full-sync" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetFullSync.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve assets by original path
  
      Retrieve assets that are children of a specific folder. *)
  let get_assets_by_original_path ~path client () =
    let op_name = "get_assets_by_original_path" in
    let url_path = "/view/folder" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"path" ~value:path]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Stack = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        primary_asset_id : string option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        assets : Asset.ResponseDto.t list;
        id : string;
        primary_asset_id : string;
      }
    end
  
    module CreateDto = struct
      type t = {
        asset_ids : string list;  (** first asset becomes the primary *)
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?primary_asset_id () = { primary_asset_id }
    
    let primary_asset_id t = t.primary_asset_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StackUpdateDto"
        (fun primary_asset_id -> { primary_asset_id })
      |> Jsont.Object.opt_mem "primaryAssetId" Jsont.string ~enc:(fun r -> r.primary_asset_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~assets ~id ~primary_asset_id () = { assets; id; primary_asset_id }
    
    let assets t = t.assets
    let id t = t.id
    let primary_asset_id t = t.primary_asset_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StackResponseDto"
        (fun assets id primary_asset_id -> { assets; id; primary_asset_id })
      |> Jsont.Object.mem "assets" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.assets)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "primaryAssetId" Jsont.string ~enc:(fun r -> r.primary_asset_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~asset_ids () = { asset_ids }
    
    let asset_ids t = t.asset_ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StackCreateDto"
        (fun asset_ids -> { asset_ids })
      |> Jsont.Object.mem "assetIds" (Openapi.Runtime.validated_list ~min_items:2 Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve stacks
  
      Retrieve a list of stacks. *)
  let search_stacks ?primary_asset_id client () =
    let op_name = "search_stacks" in
    let url_path = "/stacks" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"primaryAssetId" ~value:primary_asset_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a stack
  
      Create a new stack by providing a name and a list of asset IDs to include in the stack. If any of the provided asset IDs are primary assets of an existing stack, the existing stack will be merged into the newly created stack. *)
  let create_stack ~body client () =
    let op_name = "create_stack" in
    let url_path = "/stacks" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a stack
  
      Retrieve a specific stack by its ID. *)
  let get_stack ~id client () =
    let op_name = "get_stack" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/stacks/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a stack
  
      Update an existing stack by its ID. *)
  let update_stack ~id ~body client () =
    let op_name = "update_stack" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/stacks/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SearchExplore = struct
  module Types = struct
    module Item = struct
      type t = {
        data : Asset.ResponseDto.t;
        value : string;
      }
    end
  
    module ResponseDto = struct
      type t = {
        field_name : string;
        items : Item.t list;
      }
    end
  end
  
  module Item = struct
    include Types.Item
    
    let v ~data ~value () = { data; value }
    
    let data t = t.data
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchExploreItem"
        (fun data value -> { data; value })
      |> Jsont.Object.mem "data" Asset.ResponseDto.jsont ~enc:(fun r -> r.data)
      |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~field_name ~items () = { field_name; items }
    
    let field_name t = t.field_name
    let items t = t.items
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchExploreResponseDto"
        (fun field_name items -> { field_name; items })
      |> Jsont.Object.mem "fieldName" Jsont.string ~enc:(fun r -> r.field_name)
      |> Jsont.Object.mem "items" (Jsont.list Item.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve explore data
  
      Retrieve data for the explore section, such as popular people and places. *)
  let get_explore_data client () =
    let op_name = "get_explore_data" in
    let url_path = "/search/explore" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SearchAsset = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        count : int;
        facets : SearchFacet.ResponseDto.t list;
        items : Asset.ResponseDto.t list;
        next_page : string option;
        total : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~count ~facets ~items ~total ?next_page () = { count; facets; items; next_page; total }
    
    let count t = t.count
    let facets t = t.facets
    let items t = t.items
    let next_page t = t.next_page
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchAssetResponseDto"
        (fun count facets items next_page total -> { count; facets; items; next_page; total })
      |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
      |> Jsont.Object.mem "facets" (Jsont.list SearchFacet.ResponseDto.jsont) ~enc:(fun r -> r.facets)
      |> Jsont.Object.mem "items" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.mem "nextPage" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.next_page)
      |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Memory = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        is_saved : bool option;
        memory_at : Ptime.t option;
        seen_at : Ptime.t option;
      }
    end
  
    module Type = struct
      type t = [
        | `On_this_day
      ]
    end
  
    module ResponseDto = struct
      type t = {
        assets : Asset.ResponseDto.t list;
        created_at : Ptime.t;
        data : OnThisDay.Dto.t;
        deleted_at : Ptime.t option;
        hide_at : Ptime.t option;
        id : string;
        is_saved : bool;
        memory_at : Ptime.t;
        owner_id : string;
        seen_at : Ptime.t option;
        show_at : Ptime.t option;
        type_ : Type.t;
        updated_at : Ptime.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        asset_ids : string list option;
        data : OnThisDay.Dto.t;
        is_saved : bool option;
        memory_at : Ptime.t;
        seen_at : Ptime.t option;
        type_ : Type.t;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?is_saved ?memory_at ?seen_at () = { is_saved; memory_at; seen_at }
    
    let is_saved t = t.is_saved
    let memory_at t = t.memory_at
    let seen_at t = t.seen_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MemoryUpdateDto"
        (fun is_saved memory_at seen_at -> { is_saved; memory_at; seen_at })
      |> Jsont.Object.opt_mem "isSaved" Jsont.bool ~enc:(fun r -> r.is_saved)
      |> Jsont.Object.opt_mem "memoryAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.memory_at)
      |> Jsont.Object.opt_mem "seenAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.seen_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"MemoryType"
        ~dec:(function
          | "on_this_day" -> `On_this_day
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `On_this_day -> "on_this_day")
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~assets ~created_at ~data ~id ~is_saved ~memory_at ~owner_id ~type_ ~updated_at ?deleted_at ?hide_at ?seen_at ?show_at () = { assets; created_at; data; deleted_at; hide_at; id; is_saved; memory_at; owner_id; seen_at; show_at; type_; updated_at }
    
    let assets t = t.assets
    let created_at t = t.created_at
    let data t = t.data
    let deleted_at t = t.deleted_at
    let hide_at t = t.hide_at
    let id t = t.id
    let is_saved t = t.is_saved
    let memory_at t = t.memory_at
    let owner_id t = t.owner_id
    let seen_at t = t.seen_at
    let show_at t = t.show_at
    let type_ t = t.type_
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MemoryResponseDto"
        (fun assets created_at data deleted_at hide_at id is_saved memory_at owner_id seen_at show_at type_ updated_at -> { assets; created_at; data; deleted_at; hide_at; id; is_saved; memory_at; owner_id; seen_at; show_at; type_; updated_at })
      |> Jsont.Object.mem "assets" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.assets)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "data" OnThisDay.Dto.jsont ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "deletedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.opt_mem "hideAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.hide_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isSaved" Jsont.bool ~enc:(fun r -> r.is_saved)
      |> Jsont.Object.mem "memoryAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.memory_at)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.opt_mem "seenAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.seen_at)
      |> Jsont.Object.opt_mem "showAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.show_at)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~data ~memory_at ~type_ ?asset_ids ?is_saved ?seen_at () = { asset_ids; data; is_saved; memory_at; seen_at; type_ }
    
    let asset_ids t = t.asset_ids
    let data t = t.data
    let is_saved t = t.is_saved
    let memory_at t = t.memory_at
    let seen_at t = t.seen_at
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MemoryCreateDto"
        (fun asset_ids data is_saved memory_at seen_at type_ -> { asset_ids; data; is_saved; memory_at; seen_at; type_ })
      |> Jsont.Object.opt_mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.mem "data" OnThisDay.Dto.jsont ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "isSaved" Jsont.bool ~enc:(fun r -> r.is_saved)
      |> Jsont.Object.mem "memoryAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.memory_at)
      |> Jsont.Object.opt_mem "seenAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.seen_at)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve memories
  
      Retrieve a list of memories. Memories are sorted descending by creation date by default, although they can also be sorted in ascending order, or randomly. 
      @param size Number of memories to return
  *)
  let search_memories ?for_ ?is_saved ?is_trashed ?order ?size ?type_ client () =
    let op_name = "search_memories" in
    let url_path = "/memories" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"for" ~value:for_; Openapi.Runtime.Query.optional ~key:"isSaved" ~value:is_saved; Openapi.Runtime.Query.optional ~key:"isTrashed" ~value:is_trashed; Openapi.Runtime.Query.optional ~key:"order" ~value:order; Openapi.Runtime.Query.optional ~key:"size" ~value:size; Openapi.Runtime.Query.optional ~key:"type" ~value:type_]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a memory
  
      Create a new memory by providing a name, description, and a list of asset IDs to include in the memory. *)
  let create_memory ~body client () =
    let op_name = "create_memory" in
    let url_path = "/memories" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a memory
  
      Retrieve a specific memory by its ID. *)
  let get_memory ~id client () =
    let op_name = "get_memory" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/memories/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a memory
  
      Update an existing memory by its ID. *)
  let update_memory ~id ~body client () =
    let op_name = "update_memory" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/memories/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SyncMemoryV1 = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t;
        data : Jsont.json;
        deleted_at : Ptime.t option;
        hide_at : Ptime.t option;
        id : string;
        is_saved : bool;
        memory_at : Ptime.t;
        owner_id : string;
        seen_at : Ptime.t option;
        show_at : Ptime.t option;
        type_ : Memory.Type.t;
        updated_at : Ptime.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~created_at ~data ~id ~is_saved ~memory_at ~owner_id ~type_ ~updated_at ?deleted_at ?hide_at ?seen_at ?show_at () = { created_at; data; deleted_at; hide_at; id; is_saved; memory_at; owner_id; seen_at; show_at; type_; updated_at }
    
    let created_at t = t.created_at
    let data t = t.data
    let deleted_at t = t.deleted_at
    let hide_at t = t.hide_at
    let id t = t.id
    let is_saved t = t.is_saved
    let memory_at t = t.memory_at
    let owner_id t = t.owner_id
    let seen_at t = t.seen_at
    let show_at t = t.show_at
    let type_ t = t.type_
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncMemoryV1"
        (fun created_at data deleted_at hide_at id is_saved memory_at owner_id seen_at show_at type_ updated_at -> { created_at; data; deleted_at; hide_at; id; is_saved; memory_at; owner_id; seen_at; show_at; type_; updated_at })
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "data" Jsont.json ~enc:(fun r -> r.data)
      |> Jsont.Object.mem "deletedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.mem "hideAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.hide_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isSaved" Jsont.bool ~enc:(fun r -> r.is_saved)
      |> Jsont.Object.mem "memoryAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.memory_at)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "seenAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.seen_at)
      |> Jsont.Object.mem "showAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.show_at)
      |> Jsont.Object.mem "type" Memory.Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Duplicate = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        assets : Asset.ResponseDto.t list;
        duplicate_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~assets ~duplicate_id () = { assets; duplicate_id }
    
    let assets t = t.assets
    let duplicate_id t = t.duplicate_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"DuplicateResponseDto"
        (fun assets duplicate_id -> { assets; duplicate_id })
      |> Jsont.Object.mem "assets" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.assets)
      |> Jsont.Object.mem "duplicateId" Jsont.string ~enc:(fun r -> r.duplicate_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve duplicates
  
      Retrieve a list of duplicate assets available to the authenticated user. *)
  let get_asset_duplicates client () =
    let op_name = "get_asset_duplicates" in
    let url_path = "/duplicates" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetDeltaSync = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        deleted : string list;
        needs_full_sync : bool;
        upserted : Asset.ResponseDto.t list;
      }
    end
  
    module Dto = struct
      type t = {
        updated_after : Ptime.t;
        user_ids : string list;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~deleted ~needs_full_sync ~upserted () = { deleted; needs_full_sync; upserted }
    
    let deleted t = t.deleted
    let needs_full_sync t = t.needs_full_sync
    let upserted t = t.upserted
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetDeltaSyncResponseDto"
        (fun deleted needs_full_sync upserted -> { deleted; needs_full_sync; upserted })
      |> Jsont.Object.mem "deleted" (Jsont.list Jsont.string) ~enc:(fun r -> r.deleted)
      |> Jsont.Object.mem "needsFullSync" Jsont.bool ~enc:(fun r -> r.needs_full_sync)
      |> Jsont.Object.mem "upserted" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.upserted)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~updated_after ~user_ids () = { updated_after; user_ids }
    
    let updated_after t = t.updated_after
    let user_ids t = t.user_ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetDeltaSyncDto"
        (fun updated_after user_ids -> { updated_after; user_ids })
      |> Jsont.Object.mem "updatedAfter" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_after)
      |> Jsont.Object.mem "userIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.user_ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get delta sync for user
  
      Retrieve changed assets since the last sync for the authenticated user. *)
  let get_delta_sync ~body client () =
    let op_name = "get_delta_sync" in
    let url_path = "/sync/delta-sync" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetFaceUpdate = struct
  module Types = struct
    module Item = struct
      type t = {
        asset_id : string;
        person_id : string;
      }
    end
  end
  
  module Item = struct
    include Types.Item
    
    let v ~asset_id ~person_id () = { asset_id; person_id }
    
    let asset_id t = t.asset_id
    let person_id t = t.person_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFaceUpdateItem"
        (fun asset_id person_id -> { asset_id; person_id })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "personId" Jsont.string ~enc:(fun r -> r.person_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetFaceDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        force : bool;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~force () = { force }
    
    let force t = t.force
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFaceDeleteDto"
        (fun force -> { force })
      |> Jsont.Object.mem "force" Jsont.bool ~enc:(fun r -> r.force)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetEditActionList = struct
  module Types = struct
    module Dto = struct
      type t = {
        edits : Jsont.json list;  (** list of edits *)
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~edits () = { edits }
    
    let edits t = t.edits
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetEditActionListDto"
        (fun edits -> { edits })
      |> Jsont.Object.mem "edits" (Openapi.Runtime.validated_list ~min_items:1 Jsont.json) ~enc:(fun r -> r.edits)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetEdits = struct
  module Types = struct
    module Dto = struct
      type t = {
        asset_id : string;
        edits : Jsont.json list;  (** list of edits *)
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~asset_id ~edits () = { asset_id; edits }
    
    let asset_id t = t.asset_id
    let edits t = t.edits
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetEditsDto"
        (fun asset_id edits -> { asset_id; edits })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "edits" (Openapi.Runtime.validated_list ~min_items:1 Jsont.json) ~enc:(fun r -> r.edits)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve edits for an existing asset
  
      Retrieve a series of edit actions (crop, rotate, mirror) associated with the specified asset. *)
  let get_asset_edits ~id client () =
    let op_name = "get_asset_edits" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/edits" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Apply edits to an existing asset
  
      Apply a series of edit actions (crop, rotate, mirror) to the specified asset. *)
  let edit_asset ~id ~body client () =
    let op_name = "edit_asset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/edits" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetEditActionList.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Dto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetEditAction = struct
  module Types = struct
    module T = struct
      type t = [
        | `Crop
        | `Rotate
        | `Mirror
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AssetEditAction"
        ~dec:(function
          | "crop" -> `Crop
          | "rotate" -> `Rotate
          | "mirror" -> `Mirror
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Crop -> "crop"
          | `Rotate -> "rotate"
          | `Mirror -> "mirror")
  end
end

module AssetEditActionRotate = struct
  module Types = struct
    module T = struct
      type t = {
        action : AssetEditAction.T.t;
        parameters : RotateParameters.T.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~action ~parameters () = { action; parameters }
    
    let action t = t.action
    let parameters t = t.parameters
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetEditActionRotate"
        (fun action parameters -> { action; parameters })
      |> Jsont.Object.mem "action" AssetEditAction.T.jsont ~enc:(fun r -> r.action)
      |> Jsont.Object.mem "parameters" RotateParameters.T.jsont ~enc:(fun r -> r.parameters)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetEditActionMirror = struct
  module Types = struct
    module T = struct
      type t = {
        action : AssetEditAction.T.t;
        parameters : MirrorParameters.T.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~action ~parameters () = { action; parameters }
    
    let action t = t.action
    let parameters t = t.parameters
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetEditActionMirror"
        (fun action parameters -> { action; parameters })
      |> Jsont.Object.mem "action" AssetEditAction.T.jsont ~enc:(fun r -> r.action)
      |> Jsont.Object.mem "parameters" MirrorParameters.T.jsont ~enc:(fun r -> r.parameters)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetEditActionCrop = struct
  module Types = struct
    module T = struct
      type t = {
        action : AssetEditAction.T.t;
        parameters : CropParameters.T.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~action ~parameters () = { action; parameters }
    
    let action t = t.action
    let parameters t = t.parameters
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetEditActionCrop"
        (fun action parameters -> { action; parameters })
      |> Jsont.Object.mem "action" AssetEditAction.T.jsont ~enc:(fun r -> r.action)
      |> Jsont.Object.mem "parameters" CropParameters.T.jsont ~enc:(fun r -> r.parameters)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetCopy = struct
  module Types = struct
    module Dto = struct
      type t = {
        albums : bool;
        favorite : bool;
        shared_links : bool;
        sidecar : bool;
        source_id : string;
        stack : bool;
        target_id : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~source_id ~target_id ?(albums=true) ?(favorite=true) ?(shared_links=true) ?(sidecar=true) ?(stack=true) () = { albums; favorite; shared_links; sidecar; source_id; stack; target_id }
    
    let albums t = t.albums
    let favorite t = t.favorite
    let shared_links t = t.shared_links
    let sidecar t = t.sidecar
    let source_id t = t.source_id
    let stack t = t.stack
    let target_id t = t.target_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetCopyDto"
        (fun albums favorite shared_links sidecar source_id stack target_id -> { albums; favorite; shared_links; sidecar; source_id; stack; target_id })
      |> Jsont.Object.mem "albums" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.albums)
      |> Jsont.Object.mem "favorite" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.favorite)
      |> Jsont.Object.mem "sharedLinks" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.shared_links)
      |> Jsont.Object.mem "sidecar" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.sidecar)
      |> Jsont.Object.mem "sourceId" Jsont.string ~enc:(fun r -> r.source_id)
      |> Jsont.Object.mem "stack" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.stack)
      |> Jsont.Object.mem "targetId" Jsont.string ~enc:(fun r -> r.target_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AssetBulkUploadCheck = struct
  module Types = struct
    module Result = struct
      type t = {
        action : string;
        asset_id : string option;
        id : string;
        is_trashed : bool option;
        reason : string option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        results : Result.t list;
      }
    end
  
    module Item = struct
      type t = {
        checksum : string;  (** base64 or hex encoded sha1 hash *)
        id : string;
      }
    end
  
    module Dto = struct
      type t = {
        assets : Item.t list;
      }
    end
  end
  
  module Result = struct
    include Types.Result
    
    let v ~action ~id ?asset_id ?is_trashed ?reason () = { action; asset_id; id; is_trashed; reason }
    
    let action t = t.action
    let asset_id t = t.asset_id
    let id t = t.id
    let is_trashed t = t.is_trashed
    let reason t = t.reason
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetBulkUploadCheckResult"
        (fun action asset_id id is_trashed reason -> { action; asset_id; id; is_trashed; reason })
      |> Jsont.Object.mem "action" Jsont.string ~enc:(fun r -> r.action)
      |> Jsont.Object.opt_mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isTrashed" Jsont.bool ~enc:(fun r -> r.is_trashed)
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~results () = { results }
    
    let results t = t.results
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetBulkUploadCheckResponseDto"
        (fun results -> { results })
      |> Jsont.Object.mem "results" (Jsont.list Result.jsont) ~enc:(fun r -> r.results)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Item = struct
    include Types.Item
    
    let v ~checksum ~id () = { checksum; id }
    
    let checksum t = t.checksum
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetBulkUploadCheckItem"
        (fun checksum id -> { checksum; id })
      |> Jsont.Object.mem "checksum" Jsont.string ~enc:(fun r -> r.checksum)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~assets () = { assets }
    
    let assets t = t.assets
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetBulkUploadCheckDto"
        (fun assets -> { assets })
      |> Jsont.Object.mem "assets" (Jsont.list Item.jsont) ~enc:(fun r -> r.assets)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Check bulk upload
  
      Determine which assets have already been uploaded to the server based on their SHA1 checksums. *)
  let check_bulk_upload ~body client () =
    let op_name = "check_bulk_upload" in
    let url_path = "/assets/bulk-upload-check" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetBulkDelete = struct
  module Types = struct
    module Dto = struct
      type t = {
        force : bool option;
        ids : string list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~ids ?force () = { force; ids }
    
    let force t = t.force
    let ids t = t.ids
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetBulkDeleteDto"
        (fun force ids -> { force; ids })
      |> Jsont.Object.opt_mem "force" Jsont.bool ~enc:(fun r -> r.force)
      |> Jsont.Object.mem "ids" (Jsont.list Jsont.string) ~enc:(fun r -> r.ids)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AlbumUserRole = struct
  module Types = struct
    module T = struct
      type t = [
        | `Editor
        | `Viewer
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AlbumUserRole"
        ~dec:(function
          | "editor" -> `Editor
          | "viewer" -> `Viewer
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Editor -> "editor"
          | `Viewer -> "viewer")
  end
end

module UpdateAlbumUser = struct
  module Types = struct
    module Dto = struct
      type t = {
        role : AlbumUserRole.T.t;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~role () = { role }
    
    let role t = t.role
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UpdateAlbumUserDto"
        (fun role -> { role })
      |> Jsont.Object.mem "role" AlbumUserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SyncAlbumUserV1 = struct
  module Types = struct
    module T = struct
      type t = {
        album_id : string;
        role : AlbumUserRole.T.t;
        user_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~album_id ~role ~user_id () = { album_id; role; user_id }
    
    let album_id t = t.album_id
    let role t = t.role
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SyncAlbumUserV1"
        (fun album_id role user_id -> { album_id; role; user_id })
      |> Jsont.Object.mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.mem "role" AlbumUserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AlbumUserAdd = struct
  module Types = struct
    module Dto = struct
      type t = {
        role : AlbumUserRole.T.t;
        user_id : string;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~user_id ?(role=`Editor) () = { role; user_id }
    
    let role t = t.role
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumUserAddDto"
        (fun role user_id -> { role; user_id })
      |> Jsont.Object.mem "role" AlbumUserRole.T.jsont ~dec_absent:`Editor ~enc:(fun r -> r.role)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AddUsers = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_users : AlbumUserAdd.Dto.t list;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~album_users () = { album_users }
    
    let album_users t = t.album_users
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AddUsersDto"
        (fun album_users -> { album_users })
      |> Jsont.Object.mem "albumUsers" (Openapi.Runtime.validated_list ~min_items:1 AlbumUserAdd.Dto.jsont) ~enc:(fun r -> r.album_users)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AlbumUser = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        role : AlbumUserRole.T.t;
        user : User.ResponseDto.t;
      }
    end
  
    module CreateDto = struct
      type t = {
        role : AlbumUserRole.T.t;
        user_id : string;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~role ~user () = { role; user }
    
    let role t = t.role
    let user t = t.user
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumUserResponseDto"
        (fun role user -> { role; user })
      |> Jsont.Object.mem "role" AlbumUserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.mem "user" User.ResponseDto.jsont ~enc:(fun r -> r.user)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~role ~user_id () = { role; user_id }
    
    let role t = t.role
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumUserCreateDto"
        (fun role user_id -> { role; user_id })
      |> Jsont.Object.mem "role" AlbumUserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CreateAlbum = struct
  module Types = struct
    module Dto = struct
      type t = {
        album_name : string;
        album_users : AlbumUser.CreateDto.t list option;
        asset_ids : string list option;
        description : string option;
      }
    end
  end
  
  module Dto = struct
    include Types.Dto
    
    let v ~album_name ?album_users ?asset_ids ?description () = { album_name; album_users; asset_ids; description }
    
    let album_name t = t.album_name
    let album_users t = t.album_users
    let asset_ids t = t.asset_ids
    let description t = t.description
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CreateAlbumDto"
        (fun album_name album_users asset_ids description -> { album_name; album_users; asset_ids; description })
      |> Jsont.Object.mem "albumName" Jsont.string ~enc:(fun r -> r.album_name)
      |> Jsont.Object.opt_mem "albumUsers" (Jsont.list AlbumUser.CreateDto.jsont) ~enc:(fun r -> r.album_users)
      |> Jsont.Object.opt_mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Album = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        album_name : string;
        album_thumbnail_asset_id : string option;
        album_users : AlbumUser.ResponseDto.t list;
        asset_count : int;
        assets : Asset.ResponseDto.t list;
        contributor_counts : ContributorCount.ResponseDto.t list option;
        created_at : Ptime.t;
        description : string;
        end_date : Ptime.t option;
        has_shared_link : bool;
        id : string;
        is_activity_enabled : bool;
        last_modified_asset_timestamp : Ptime.t option;
        order : AssetOrder.T.t option;
        owner : User.ResponseDto.t;
        owner_id : string;
        shared : bool;
        start_date : Ptime.t option;
        updated_at : Ptime.t;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~album_name ~album_users ~asset_count ~assets ~created_at ~description ~has_shared_link ~id ~is_activity_enabled ~owner ~owner_id ~shared ~updated_at ?album_thumbnail_asset_id ?contributor_counts ?end_date ?last_modified_asset_timestamp ?order ?start_date () = { album_name; album_thumbnail_asset_id; album_users; asset_count; assets; contributor_counts; created_at; description; end_date; has_shared_link; id; is_activity_enabled; last_modified_asset_timestamp; order; owner; owner_id; shared; start_date; updated_at }
    
    let album_name t = t.album_name
    let album_thumbnail_asset_id t = t.album_thumbnail_asset_id
    let album_users t = t.album_users
    let asset_count t = t.asset_count
    let assets t = t.assets
    let contributor_counts t = t.contributor_counts
    let created_at t = t.created_at
    let description t = t.description
    let end_date t = t.end_date
    let has_shared_link t = t.has_shared_link
    let id t = t.id
    let is_activity_enabled t = t.is_activity_enabled
    let last_modified_asset_timestamp t = t.last_modified_asset_timestamp
    let order t = t.order
    let owner t = t.owner
    let owner_id t = t.owner_id
    let shared t = t.shared
    let start_date t = t.start_date
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumResponseDto"
        (fun album_name album_thumbnail_asset_id album_users asset_count assets contributor_counts created_at description end_date has_shared_link id is_activity_enabled last_modified_asset_timestamp order owner owner_id shared start_date updated_at -> { album_name; album_thumbnail_asset_id; album_users; asset_count; assets; contributor_counts; created_at; description; end_date; has_shared_link; id; is_activity_enabled; last_modified_asset_timestamp; order; owner; owner_id; shared; start_date; updated_at })
      |> Jsont.Object.mem "albumName" Jsont.string ~enc:(fun r -> r.album_name)
      |> Jsont.Object.mem "albumThumbnailAssetId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.album_thumbnail_asset_id)
      |> Jsont.Object.mem "albumUsers" (Jsont.list AlbumUser.ResponseDto.jsont) ~enc:(fun r -> r.album_users)
      |> Jsont.Object.mem "assetCount" Jsont.int ~enc:(fun r -> r.asset_count)
      |> Jsont.Object.mem "assets" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.assets)
      |> Jsont.Object.opt_mem "contributorCounts" (Jsont.list ContributorCount.ResponseDto.jsont) ~enc:(fun r -> r.contributor_counts)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "endDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.end_date)
      |> Jsont.Object.mem "hasSharedLink" Jsont.bool ~enc:(fun r -> r.has_shared_link)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "isActivityEnabled" Jsont.bool ~enc:(fun r -> r.is_activity_enabled)
      |> Jsont.Object.opt_mem "lastModifiedAssetTimestamp" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_modified_asset_timestamp)
      |> Jsont.Object.opt_mem "order" AssetOrder.T.jsont ~enc:(fun r -> r.order)
      |> Jsont.Object.mem "owner" User.ResponseDto.jsont ~enc:(fun r -> r.owner)
      |> Jsont.Object.mem "ownerId" Jsont.string ~enc:(fun r -> r.owner_id)
      |> Jsont.Object.mem "shared" Jsont.bool ~enc:(fun r -> r.shared)
      |> Jsont.Object.opt_mem "startDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.start_date)
      |> Jsont.Object.mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all albums
  
      Retrieve a list of albums available to the authenticated user. 
      @param asset_id Only returns albums that contain the asset
  Ignores the shared parameter
  undefined: get all albums
  *)
  let get_all_albums ?asset_id ?shared client () =
    let op_name = "get_all_albums" in
    let url_path = "/albums" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"assetId" ~value:asset_id; Openapi.Runtime.Query.optional ~key:"shared" ~value:shared]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create an album
  
      Create a new album. The album can also be created with initial users and assets. *)
  let create_album ~body client () =
    let op_name = "create_album" in
    let url_path = "/albums" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateAlbum.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve an album
  
      Retrieve information about a specific album by its ID. *)
  let get_album_info ~id ?key ?slug ?without_assets client () =
    let op_name = "get_album_info" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/albums/{id}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug; Openapi.Runtime.Query.optional ~key:"withoutAssets" ~value:without_assets]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update an album
  
      Update the information of a specific album by its ID. This endpoint can be used to update the album name, description, sort order, etc. However, it is not used to add or remove assets or users from the album. *)
  let update_album_info ~id ~body client () =
    let op_name = "update_album_info" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/albums/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateAlbum.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Share album with users
  
      Share an album with multiple users. Each user can be given a specific role in the album. *)
  let add_users_to_album ~id ~body client () =
    let op_name = "add_users_to_album" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/albums/{id}/users" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AddUsers.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SharedLink = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Album
        | `Individual
      ]
    end
  
    module ResponseDto = struct
      type t = {
        album : Album.ResponseDto.t option;
        allow_download : bool;
        allow_upload : bool;
        assets : Asset.ResponseDto.t list;
        created_at : Ptime.t;
        description : string option;
        expires_at : Ptime.t option;
        id : string;
        key : string;
        password : string option;
        show_metadata : bool;
        slug : string option;
        token : string option;
        type_ : Type.t;
        user_id : string;
      }
    end
  
    module CreateDto = struct
      type t = {
        album_id : string option;
        allow_download : bool;
        allow_upload : bool option;
        asset_ids : string list option;
        description : string option;
        expires_at : Ptime.t option;
        password : string option;
        show_metadata : bool;
        slug : string option;
        type_ : Type.t;
      }
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"SharedLinkType"
        ~dec:(function
          | "ALBUM" -> `Album
          | "INDIVIDUAL" -> `Individual
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Album -> "ALBUM"
          | `Individual -> "INDIVIDUAL")
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~allow_download ~allow_upload ~assets ~created_at ~id ~key ~show_metadata ~type_ ~user_id ?album ?description ?expires_at ?password ?slug ?token () = { album; allow_download; allow_upload; assets; created_at; description; expires_at; id; key; password; show_metadata; slug; token; type_; user_id }
    
    let album t = t.album
    let allow_download t = t.allow_download
    let allow_upload t = t.allow_upload
    let assets t = t.assets
    let created_at t = t.created_at
    let description t = t.description
    let expires_at t = t.expires_at
    let id t = t.id
    let key t = t.key
    let password t = t.password
    let show_metadata t = t.show_metadata
    let slug t = t.slug
    let token t = t.token
    let type_ t = t.type_
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SharedLinkResponseDto"
        (fun album allow_download allow_upload assets created_at description expires_at id key password show_metadata slug token type_ user_id -> { album; allow_download; allow_upload; assets; created_at; description; expires_at; id; key; password; show_metadata; slug; token; type_; user_id })
      |> Jsont.Object.opt_mem "album" Album.ResponseDto.jsont ~enc:(fun r -> r.album)
      |> Jsont.Object.mem "allowDownload" Jsont.bool ~enc:(fun r -> r.allow_download)
      |> Jsont.Object.mem "allowUpload" Jsont.bool ~enc:(fun r -> r.allow_upload)
      |> Jsont.Object.mem "assets" (Jsont.list Asset.ResponseDto.jsont) ~enc:(fun r -> r.assets)
      |> Jsont.Object.mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "expiresAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
      |> Jsont.Object.mem "password" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "showMetadata" Jsont.bool ~enc:(fun r -> r.show_metadata)
      |> Jsont.Object.mem "slug" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.slug)
      |> Jsont.Object.mem "token" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.token)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~type_ ?(allow_download=true) ?(expires_at=None) ?(show_metadata=true) ?album_id ?allow_upload ?asset_ids ?description ?password ?slug () = { album_id; allow_download; allow_upload; asset_ids; description; expires_at; password; show_metadata; slug; type_ }
    
    let album_id t = t.album_id
    let allow_download t = t.allow_download
    let allow_upload t = t.allow_upload
    let asset_ids t = t.asset_ids
    let description t = t.description
    let expires_at t = t.expires_at
    let password t = t.password
    let show_metadata t = t.show_metadata
    let slug t = t.slug
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SharedLinkCreateDto"
        (fun album_id allow_download allow_upload asset_ids description expires_at password show_metadata slug type_ -> { album_id; allow_download; allow_upload; asset_ids; description; expires_at; password; show_metadata; slug; type_ })
      |> Jsont.Object.opt_mem "albumId" Jsont.string ~enc:(fun r -> r.album_id)
      |> Jsont.Object.mem "allowDownload" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.allow_download)
      |> Jsont.Object.opt_mem "allowUpload" Jsont.bool ~enc:(fun r -> r.allow_upload)
      |> Jsont.Object.opt_mem "assetIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.asset_ids)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "expiresAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.mem "password" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "showMetadata" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.show_metadata)
      |> Jsont.Object.mem "slug" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.slug)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve all shared links
  
      Retrieve a list of all shared links. *)
  let get_all_shared_links ?album_id ?id client () =
    let op_name = "get_all_shared_links" in
    let url_path = "/shared-links" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"albumId" ~value:album_id; Openapi.Runtime.Query.optional ~key:"id" ~value:id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a shared link
  
      Create a new shared link. *)
  let create_shared_link ~body client () =
    let op_name = "create_shared_link" in
    let url_path = "/shared-links" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve current shared link
  
      Retrieve the current shared link associated with authentication method. *)
  let get_my_shared_link ?key ?password ?slug ?token client () =
    let op_name = "get_my_shared_link" in
    let url_path = "/shared-links/me" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"password" ~value:password; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug; Openapi.Runtime.Query.optional ~key:"token" ~value:token]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a shared link
  
      Retrieve a specific shared link by its ID. *)
  let get_shared_link_by_id ~id client () =
    let op_name = "get_shared_link_by_id" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/shared-links/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a shared link
  
      Update an existing shared link by its ID. *)
  let update_shared_link ~id ~body client () =
    let op_name = "update_shared_link" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/shared-links/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SharedLinkEdit.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SearchAlbum = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        count : int;
        facets : SearchFacet.ResponseDto.t list;
        items : Album.ResponseDto.t list;
        total : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~count ~facets ~items ~total () = { count; facets; items; total }
    
    let count t = t.count
    let facets t = t.facets
    let items t = t.items
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchAlbumResponseDto"
        (fun count facets items total -> { count; facets; items; total })
      |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
      |> Jsont.Object.mem "facets" (Jsont.list SearchFacet.ResponseDto.jsont) ~enc:(fun r -> r.facets)
      |> Jsont.Object.mem "items" (Jsont.list Album.ResponseDto.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Search = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        albums : SearchAlbum.ResponseDto.t;
        assets : SearchAsset.ResponseDto.t;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~albums ~assets () = { albums; assets }
    
    let albums t = t.albums
    let assets t = t.assets
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchResponseDto"
        (fun albums assets -> { albums; assets })
      |> Jsont.Object.mem "albums" SearchAlbum.ResponseDto.jsont ~enc:(fun r -> r.albums)
      |> Jsont.Object.mem "assets" SearchAsset.ResponseDto.jsont ~enc:(fun r -> r.assets)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Search assets by metadata
  
      Search for assets based on various metadata criteria. *)
  let search_assets ~body client () =
    let op_name = "search_assets" in
    let url_path = "/search/metadata" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json MetadataSearch.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Smart asset search
  
      Perform a smart search for assets by using machine learning vectors to determine relevance. *)
  let search_smart ~body client () =
    let op_name = "search_smart" in
    let url_path = "/search/smart" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SmartSearch.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AlbumStatistics = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        not_shared : int;
        owned : int;
        shared : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~not_shared ~owned ~shared () = { not_shared; owned; shared }
    
    let not_shared t = t.not_shared
    let owned t = t.owned
    let shared t = t.shared
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AlbumStatisticsResponseDto"
        (fun not_shared owned shared -> { not_shared; owned; shared })
      |> Jsont.Object.mem "notShared" Jsont.int ~enc:(fun r -> r.not_shared)
      |> Jsont.Object.mem "owned" Jsont.int ~enc:(fun r -> r.owned)
      |> Jsont.Object.mem "shared" Jsont.int ~enc:(fun r -> r.shared)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve album statistics
  
      Returns statistics about the albums available to the authenticated user. *)
  let get_album_statistics client () =
    let op_name = "get_album_statistics" in
    let url_path = "/albums/statistics" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AdminOnboarding = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        is_onboarded : bool;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ~is_onboarded () = { is_onboarded }
    
    let is_onboarded t = t.is_onboarded
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AdminOnboardingUpdateDto"
        (fun is_onboarded -> { is_onboarded })
      |> Jsont.Object.mem "isOnboarded" Jsont.bool ~enc:(fun r -> r.is_onboarded)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve admin onboarding
  
      Retrieve the current admin onboarding status. *)
  let get_admin_onboarding client () =
    let op_name = "get_admin_onboarding" in
    let url_path = "/system-metadata/admin-onboarding" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn UpdateDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ActivityStatistics = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        comments : int;
        likes : int;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~comments ~likes () = { comments; likes }
    
    let comments t = t.comments
    let likes t = t.likes
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ActivityStatisticsResponseDto"
        (fun comments likes -> { comments; likes })
      |> Jsont.Object.mem "comments" Jsont.int ~enc:(fun r -> r.comments)
      |> Jsont.Object.mem "likes" Jsont.int ~enc:(fun r -> r.likes)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve activity statistics
  
      Returns the number of likes and comments for a given album or asset in an album. *)
  let get_activity_statistics ~album_id ?asset_id client () =
    let op_name = "get_activity_statistics" in
    let url_path = "/activities/statistics" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"albumId" ~value:album_id; Openapi.Runtime.Query.optional ~key:"assetId" ~value:asset_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module UserPreferences = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        albums : Albums.Update.t option;
        avatar : Avatar.Update.t option;
        cast : Cast.Update.t option;
        download : Download.Update.t option;
        email_notifications : EmailNotifications.Update.t option;
        folders : Folders.Update.t option;
        memories : Memories.Update.t option;
        people : Jsont.json option;
        purchase : Purchase.Update.t option;
        ratings : Ratings.Update.t option;
        shared_links : SharedLinks.Update.t option;
        tags : Tags.Update.t option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        albums : Albums.Response.t;
        cast : Cast.Response.t;
        download : Download.Response.t;
        email_notifications : EmailNotifications.Response.t;
        folders : Folders.Response.t;
        memories : Memories.Response.t;
        people : Jsont.json;
        purchase : Purchase.Response.t;
        ratings : Ratings.Response.t;
        shared_links : SharedLinks.Response.t;
        tags : Tags.Response.t;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?albums ?avatar ?cast ?download ?email_notifications ?folders ?memories ?people ?purchase ?ratings ?shared_links ?tags () = { albums; avatar; cast; download; email_notifications; folders; memories; people; purchase; ratings; shared_links; tags }
    
    let albums t = t.albums
    let avatar t = t.avatar
    let cast t = t.cast
    let download t = t.download
    let email_notifications t = t.email_notifications
    let folders t = t.folders
    let memories t = t.memories
    let people t = t.people
    let purchase t = t.purchase
    let ratings t = t.ratings
    let shared_links t = t.shared_links
    let tags t = t.tags
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserPreferencesUpdateDto"
        (fun albums avatar cast download email_notifications folders memories people purchase ratings shared_links tags -> { albums; avatar; cast; download; email_notifications; folders; memories; people; purchase; ratings; shared_links; tags })
      |> Jsont.Object.opt_mem "albums" Albums.Update.jsont ~enc:(fun r -> r.albums)
      |> Jsont.Object.opt_mem "avatar" Avatar.Update.jsont ~enc:(fun r -> r.avatar)
      |> Jsont.Object.opt_mem "cast" Cast.Update.jsont ~enc:(fun r -> r.cast)
      |> Jsont.Object.opt_mem "download" Download.Update.jsont ~enc:(fun r -> r.download)
      |> Jsont.Object.opt_mem "emailNotifications" EmailNotifications.Update.jsont ~enc:(fun r -> r.email_notifications)
      |> Jsont.Object.opt_mem "folders" Folders.Update.jsont ~enc:(fun r -> r.folders)
      |> Jsont.Object.opt_mem "memories" Memories.Update.jsont ~enc:(fun r -> r.memories)
      |> Jsont.Object.opt_mem "people" Jsont.json ~enc:(fun r -> r.people)
      |> Jsont.Object.opt_mem "purchase" Purchase.Update.jsont ~enc:(fun r -> r.purchase)
      |> Jsont.Object.opt_mem "ratings" Ratings.Update.jsont ~enc:(fun r -> r.ratings)
      |> Jsont.Object.opt_mem "sharedLinks" SharedLinks.Update.jsont ~enc:(fun r -> r.shared_links)
      |> Jsont.Object.opt_mem "tags" Tags.Update.jsont ~enc:(fun r -> r.tags)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~albums ~cast ~download ~email_notifications ~folders ~memories ~people ~purchase ~ratings ~shared_links ~tags () = { albums; cast; download; email_notifications; folders; memories; people; purchase; ratings; shared_links; tags }
    
    let albums t = t.albums
    let cast t = t.cast
    let download t = t.download
    let email_notifications t = t.email_notifications
    let folders t = t.folders
    let memories t = t.memories
    let people t = t.people
    let purchase t = t.purchase
    let ratings t = t.ratings
    let shared_links t = t.shared_links
    let tags t = t.tags
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserPreferencesResponseDto"
        (fun albums cast download email_notifications folders memories people purchase ratings shared_links tags -> { albums; cast; download; email_notifications; folders; memories; people; purchase; ratings; shared_links; tags })
      |> Jsont.Object.mem "albums" Albums.Response.jsont ~enc:(fun r -> r.albums)
      |> Jsont.Object.mem "cast" Cast.Response.jsont ~enc:(fun r -> r.cast)
      |> Jsont.Object.mem "download" Download.Response.jsont ~enc:(fun r -> r.download)
      |> Jsont.Object.mem "emailNotifications" EmailNotifications.Response.jsont ~enc:(fun r -> r.email_notifications)
      |> Jsont.Object.mem "folders" Folders.Response.jsont ~enc:(fun r -> r.folders)
      |> Jsont.Object.mem "memories" Memories.Response.jsont ~enc:(fun r -> r.memories)
      |> Jsont.Object.mem "people" Jsont.json ~enc:(fun r -> r.people)
      |> Jsont.Object.mem "purchase" Purchase.Response.jsont ~enc:(fun r -> r.purchase)
      |> Jsont.Object.mem "ratings" Ratings.Response.jsont ~enc:(fun r -> r.ratings)
      |> Jsont.Object.mem "sharedLinks" SharedLinks.Response.jsont ~enc:(fun r -> r.shared_links)
      |> Jsont.Object.mem "tags" Tags.Response.jsont ~enc:(fun r -> r.tags)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve user preferences
  
      Retrieve the preferences of a specific user. *)
  let get_user_preferences_admin ~id client () =
    let op_name = "get_user_preferences_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}/preferences" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update user preferences
  
      Update the preferences of a specific user. *)
  let update_user_preferences_admin ~id ~body client () =
    let op_name = "update_user_preferences_admin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/admin/users/{id}/preferences" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get my preferences
  
      Retrieve the preferences for the current user. *)
  let get_my_preferences client () =
    let op_name = "get_my_preferences" in
    let url_path = "/users/me/preferences" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update my preferences
  
      Update the preferences of the current user. *)
  let update_my_preferences ~body client () =
    let op_name = "update_my_preferences" in
    let url_path = "/users/me/preferences" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Person = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        birth_date : string option;  (** Person date of birth.
      Note: the mobile app cannot currently set the birth date to null. *)
        color : string option;
        feature_face_asset_id : string option;  (** Asset is used to get the feature face thumbnail. *)
        is_favorite : bool option;
        is_hidden : bool option;  (** Person visibility *)
        name : string option;  (** Person name. *)
      }
    end
  
    module ResponseDto = struct
      type t = {
        birth_date : string option;
        color : string option;
        id : string;
        is_favorite : bool option;
        is_hidden : bool;
        name : string;
        thumbnail_path : string;
        updated_at : Ptime.t option;
      }
    end
  
    module CreateDto = struct
      type t = {
        birth_date : string option;  (** Person date of birth.
      Note: the mobile app cannot currently set the birth date to null. *)
        color : string option;
        is_favorite : bool option;
        is_hidden : bool option;  (** Person visibility *)
        name : string option;  (** Person name. *)
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ?birth_date ?color ?feature_face_asset_id ?is_favorite ?is_hidden ?name () = { birth_date; color; feature_face_asset_id; is_favorite; is_hidden; name }
    
    let birth_date t = t.birth_date
    let color t = t.color
    let feature_face_asset_id t = t.feature_face_asset_id
    let is_favorite t = t.is_favorite
    let is_hidden t = t.is_hidden
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PersonUpdateDto"
        (fun birth_date color feature_face_asset_id is_favorite is_hidden name -> { birth_date; color; feature_face_asset_id; is_favorite; is_hidden; name })
      |> Jsont.Object.mem "birthDate" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.birth_date)
      |> Jsont.Object.mem "color" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.color)
      |> Jsont.Object.opt_mem "featureFaceAssetId" Jsont.string ~enc:(fun r -> r.feature_face_asset_id)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isHidden" Jsont.bool ~enc:(fun r -> r.is_hidden)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~id ~is_hidden ~name ~thumbnail_path ?birth_date ?color ?is_favorite ?updated_at () = { birth_date; color; id; is_favorite; is_hidden; name; thumbnail_path; updated_at }
    
    let birth_date t = t.birth_date
    let color t = t.color
    let id t = t.id
    let is_favorite t = t.is_favorite
    let is_hidden t = t.is_hidden
    let name t = t.name
    let thumbnail_path t = t.thumbnail_path
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PersonResponseDto"
        (fun birth_date color id is_favorite is_hidden name thumbnail_path updated_at -> { birth_date; color; id; is_favorite; is_hidden; name; thumbnail_path; updated_at })
      |> Jsont.Object.mem "birthDate" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.birth_date)
      |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.mem "isHidden" Jsont.bool ~enc:(fun r -> r.is_hidden)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ?birth_date ?color ?is_favorite ?is_hidden ?name () = { birth_date; color; is_favorite; is_hidden; name }
    
    let birth_date t = t.birth_date
    let color t = t.color
    let is_favorite t = t.is_favorite
    let is_hidden t = t.is_hidden
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PersonCreateDto"
        (fun birth_date color is_favorite is_hidden name -> { birth_date; color; is_favorite; is_hidden; name })
      |> Jsont.Object.mem "birthDate" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.birth_date)
      |> Jsont.Object.mem "color" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.color)
      |> Jsont.Object.opt_mem "isFavorite" Jsont.bool ~enc:(fun r -> r.is_favorite)
      |> Jsont.Object.opt_mem "isHidden" Jsont.bool ~enc:(fun r -> r.is_hidden)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Re-assign a face to another person
  
      Re-assign the face provided in the body to the person identified by the id in the path parameter. *)
  let reassign_faces_by_id ~id ~body client () =
    let op_name = "reassign_faces_by_id" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/faces/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Face.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a person
  
      Create a new person that can have multiple faces assigned to them. *)
  let create_person ~body client () =
    let op_name = "create_person" in
    let url_path = "/people" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a person
  
      Retrieve a person by id. *)
  let get_person ~id client () =
    let op_name = "get_person" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update person
  
      Update an individual person. *)
  let update_person ~id ~body client () =
    let op_name = "update_person" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reassign faces
  
      Bulk reassign a list of faces to a different person. *)
  let reassign_faces ~id ~body client () =
    let op_name = "reassign_faces" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}/reassign" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Jsont.json body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Search people
  
      Search for people by name. *)
  let search_person ~name ?with_hidden client () =
    let op_name = "search_person" in
    let url_path = "/search/person" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"name" ~value:name; Openapi.Runtime.Query.optional ~key:"withHidden" ~value:with_hidden]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module People = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        people : PeopleUpdate.Item.t list;
      }
    end
  
    module Update = struct
      type t = {
        enabled : bool option;
        sidebar_web : bool option;
      }
    end
  
    module ResponseDto = struct
      type t = {
        has_next_page : bool option;
        hidden : int;
        people : Person.ResponseDto.t list;
        total : int;
      }
    end
  
    module Response = struct
      type t = {
        enabled : bool;
        sidebar_web : bool;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ~people () = { people }
    
    let people t = t.people
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PeopleUpdateDto"
        (fun people -> { people })
      |> Jsont.Object.mem "people" (Jsont.list PeopleUpdate.Item.jsont) ~enc:(fun r -> r.people)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Update = struct
    include Types.Update
    
    let v ?enabled ?sidebar_web () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PeopleUpdate"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.opt_mem "sidebarWeb" Jsont.bool ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~hidden ~people ~total ?has_next_page () = { has_next_page; hidden; people; total }
    
    let has_next_page t = t.has_next_page
    let hidden t = t.hidden
    let people t = t.people
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PeopleResponseDto"
        (fun has_next_page hidden people total -> { has_next_page; hidden; people; total })
      |> Jsont.Object.opt_mem "hasNextPage" Jsont.bool ~enc:(fun r -> r.has_next_page)
      |> Jsont.Object.mem "hidden" Jsont.int ~enc:(fun r -> r.hidden)
      |> Jsont.Object.mem "people" (Jsont.list Person.ResponseDto.jsont) ~enc:(fun r -> r.people)
      |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?(enabled=true) ?(sidebar_web=false) () = { enabled; sidebar_web }
    
    let enabled t = t.enabled
    let sidebar_web t = t.sidebar_web
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PeopleResponse"
        (fun enabled sidebar_web -> { enabled; sidebar_web })
      |> Jsont.Object.mem "enabled" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enabled)
      |> Jsont.Object.mem "sidebarWeb" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.sidebar_web)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get all people
  
      Retrieve a list of all people. 
      @param page Page number for pagination
      @param size Number of items per page
  *)
  let get_all_people ?closest_asset_id ?closest_person_id ?page ?size ?with_hidden client () =
    let op_name = "get_all_people" in
    let url_path = "/people" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"closestAssetId" ~value:closest_asset_id; Openapi.Runtime.Query.optional ~key:"closestPersonId" ~value:closest_person_id; Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"size" ~value:size; Openapi.Runtime.Query.optional ~key:"withHidden" ~value:with_hidden]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module BulkId = struct
  module Types = struct
    module ResponseDto = struct
      type t = {
        error : string option;
        id : string;
        success : bool;
      }
    end
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~id ~success ?error () = { error; id; success }
    
    let error t = t.error
    let id t = t.id
    let success t = t.success
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"BulkIdResponseDto"
        (fun error id success -> { error; id; success })
      |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Add assets to an album
  
      Add multiple assets to a specific album by its ID. *)
  let add_assets_to_album ~id ?key ?slug ~body client () =
    let op_name = "add_assets_to_album" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/albums/{id}/assets" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json BulkIds.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove assets from an album
  
      Remove multiple assets from a specific album by its ID. *)
  let remove_asset_from_album ~id client () =
    let op_name = "remove_asset_from_album" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/albums/{id}/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add assets to a memory
  
      Add a list of asset IDs to a specific memory. *)
  let add_memory_assets ~id ~body client () =
    let op_name = "add_memory_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/memories/{id}/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json BulkIds.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove assets from a memory
  
      Remove a list of asset IDs from a specific memory. *)
  let remove_memory_assets ~id client () =
    let op_name = "remove_memory_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/memories/{id}/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update people
  
      Bulk update multiple people at once. *)
  let update_people ~body client () =
    let op_name = "update_people" in
    let url_path = "/people" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json People.UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Merge people
  
      Merge a list of people into the person specified in the path parameter. *)
  let merge_person ~id ~body client () =
    let op_name = "merge_person" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}/merge" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json MergePerson.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Tag assets
  
      Add a tag to all the specified assets. *)
  let tag_assets ~id ~body client () =
    let op_name = "tag_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/tags/{id}/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json BulkIds.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Untag assets
  
      Remove a tag from all the specified assets. *)
  let untag_assets ~id client () =
    let op_name = "untag_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/tags/{id}/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AssetFace = struct
  module Types = struct
    module UpdateDto = struct
      type t = {
        data : AssetFaceUpdate.Item.t list;
      }
    end
  
    module ResponseDto = struct
      type t = {
        bounding_box_x1 : int;
        bounding_box_x2 : int;
        bounding_box_y1 : int;
        bounding_box_y2 : int;
        id : string;
        image_height : int;
        image_width : int;
        person : Person.ResponseDto.t;
        source_type : Source.Type.t option;
      }
    end
  
    module CreateDto = struct
      type t = {
        asset_id : string;
        height : int;
        image_height : int;
        image_width : int;
        person_id : string;
        width : int;
        x : int;
        y : int;
      }
    end
  end
  
  module UpdateDto = struct
    include Types.UpdateDto
    
    let v ~data () = { data }
    
    let data t = t.data
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFaceUpdateDto"
        (fun data -> { data })
      |> Jsont.Object.mem "data" (Jsont.list AssetFaceUpdate.Item.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module ResponseDto = struct
    include Types.ResponseDto
    
    let v ~bounding_box_x1 ~bounding_box_x2 ~bounding_box_y1 ~bounding_box_y2 ~id ~image_height ~image_width ~person ?source_type () = { bounding_box_x1; bounding_box_x2; bounding_box_y1; bounding_box_y2; id; image_height; image_width; person; source_type }
    
    let bounding_box_x1 t = t.bounding_box_x1
    let bounding_box_x2 t = t.bounding_box_x2
    let bounding_box_y1 t = t.bounding_box_y1
    let bounding_box_y2 t = t.bounding_box_y2
    let id t = t.id
    let image_height t = t.image_height
    let image_width t = t.image_width
    let person t = t.person
    let source_type t = t.source_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFaceResponseDto"
        (fun bounding_box_x1 bounding_box_x2 bounding_box_y1 bounding_box_y2 id image_height image_width person source_type -> { bounding_box_x1; bounding_box_x2; bounding_box_y1; bounding_box_y2; id; image_height; image_width; person; source_type })
      |> Jsont.Object.mem "boundingBoxX1" Jsont.int ~enc:(fun r -> r.bounding_box_x1)
      |> Jsont.Object.mem "boundingBoxX2" Jsont.int ~enc:(fun r -> r.bounding_box_x2)
      |> Jsont.Object.mem "boundingBoxY1" Jsont.int ~enc:(fun r -> r.bounding_box_y1)
      |> Jsont.Object.mem "boundingBoxY2" Jsont.int ~enc:(fun r -> r.bounding_box_y2)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "imageHeight" Jsont.int ~enc:(fun r -> r.image_height)
      |> Jsont.Object.mem "imageWidth" Jsont.int ~enc:(fun r -> r.image_width)
      |> Jsont.Object.mem "person" Person.ResponseDto.jsont ~enc:(fun r -> r.person)
      |> Jsont.Object.opt_mem "sourceType" Source.Type.jsont ~enc:(fun r -> r.source_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module CreateDto = struct
    include Types.CreateDto
    
    let v ~asset_id ~height ~image_height ~image_width ~person_id ~width ~x ~y () = { asset_id; height; image_height; image_width; person_id; width; x; y }
    
    let asset_id t = t.asset_id
    let height t = t.height
    let image_height t = t.image_height
    let image_width t = t.image_width
    let person_id t = t.person_id
    let width t = t.width
    let x t = t.x
    let y t = t.y
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AssetFaceCreateDto"
        (fun asset_id height image_height image_width person_id width x y -> { asset_id; height; image_height; image_width; person_id; width; x; y })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "height" Jsont.int ~enc:(fun r -> r.height)
      |> Jsont.Object.mem "imageHeight" Jsont.int ~enc:(fun r -> r.image_height)
      |> Jsont.Object.mem "imageWidth" Jsont.int ~enc:(fun r -> r.image_width)
      |> Jsont.Object.mem "personId" Jsont.string ~enc:(fun r -> r.person_id)
      |> Jsont.Object.mem "width" Jsont.int ~enc:(fun r -> r.width)
      |> Jsont.Object.mem "x" Jsont.int ~enc:(fun r -> r.x)
      |> Jsont.Object.mem "y" Jsont.int ~enc:(fun r -> r.y)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve faces for asset
  
      Retrieve all faces belonging to an asset. *)
  let get_faces ~id client () =
    let op_name = "get_faces" in
    let url_path = "/faces" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"id" ~value:id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn ResponseDto.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Client = struct
  (** Delete an activity
  
      Removes a like or comment from a given album or asset in an album. *)
  let delete_activity ~id client () =
    let op_name = "delete_activity" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/activities/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unlink all OAuth accounts
  
      Unlinks all OAuth accounts associated with user accounts in the system. *)
  let unlink_all_oauth_accounts_admin client () =
    let op_name = "unlink_all_oauth_accounts_admin" in
    let url_path = "/admin/auth/unlink-all" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete database backup
  
      Delete a backup by its filename *)
  let delete_database_backup client () =
    let op_name = "delete_database_backup" in
    let url_path = "/admin/database-backups" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Start database backup restore flow
  
      Put Immich into maintenance mode to restore a backup (Immich must not be configured) *)
  let start_database_restore_flow client () =
    let op_name = "start_database_restore_flow" in
    let url_path = "/admin/database-backups/start-restore" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Upload database backup
  
      Uploads .sql/.sql.gz file to restore backup from *)
  let upload_database_backup client () =
    let op_name = "upload_database_backup" in
    let url_path = "/admin/database-backups/upload" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Download database backup
  
      Downloads the database backup file *)
  let download_database_backup ~filename client () =
    let op_name = "download_database_backup" in
    let url_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/admin/database-backups/{filename}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Set maintenance mode
  
      Put Immich into or take it out of maintenance mode *)
  let set_maintenance_mode ~body client () =
    let op_name = "set_maintenance_mode" in
    let url_path = "/admin/maintenance" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SetMaintenanceMode.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete an album
  
      Delete a specific album by its ID. Note the album is initially trashed and then immediately scheduled for deletion, but relies on a background job to complete the process. *)
  let delete_album ~id client () =
    let op_name = "delete_album" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/albums/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update user role
  
      Change the role for a specific user in a specific album. *)
  let update_album_user ~id ~user_id ~body client () =
    let op_name = "update_album_user" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("userId", user_id)] "/albums/{id}/user/{userId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateAlbumUser.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove user from album
  
      Remove a user from an album. Use an ID of "me" to leave a shared album. *)
  let remove_user_from_album ~id ~user_id client () =
    let op_name = "remove_user_from_album" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("userId", user_id)] "/albums/{id}/user/{userId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete an API key
  
      Deletes an API key identified by its ID. The current user must own this API key. *)
  let delete_api_key ~id client () =
    let op_name = "delete_api_key" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api-keys/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update assets
  
      Updates multiple assets at the same time. *)
  let update_assets ~body client () =
    let op_name = "update_assets" in
    let url_path = "/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetBulk.UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete assets
  
      Deletes multiple assets at the same time. *)
  let delete_assets client () =
    let op_name = "delete_assets" in
    let url_path = "/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Copy asset
  
      Copy asset information like albums, tags, etc. from one asset to another. *)
  let copy_asset ~body client () =
    let op_name = "copy_asset" in
    let url_path = "/assets/copy" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetCopy.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve assets by device ID
  
      Get all asset of a device that are in the database, ID only. *)
  let get_all_user_assets_by_device_id ~device_id client () =
    let op_name = "get_all_user_assets_by_device_id" in
    let url_path = Openapi.Runtime.Path.render ~params:[("deviceId", device_id)] "/assets/device/{deviceId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Run an asset job
  
      Run a specific job on a set of assets. *)
  let run_asset_jobs ~body client () =
    let op_name = "run_asset_jobs" in
    let url_path = "/assets/jobs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetJobs.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete asset metadata
  
      Delete metadata key-value pairs for multiple assets. *)
  let delete_bulk_asset_metadata client () =
    let op_name = "delete_bulk_asset_metadata" in
    let url_path = "/assets/metadata" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove edits from an existing asset
  
      Removes all edit actions (crop, rotate, mirror) associated with the specified asset. *)
  let remove_asset_edits ~id client () =
    let op_name = "remove_asset_edits" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/edits" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete asset metadata by key
  
      Delete a specific metadata key-value pair associated with the specified asset. *)
  let delete_asset_metadata ~id ~key client () =
    let op_name = "delete_asset_metadata" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("key", key)] "/assets/{id}/metadata/{key}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Download original asset
  
      Downloads the original file of the specified asset. *)
  let download_asset ~id ?edited ?key ?slug client () =
    let op_name = "download_asset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/original" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"edited" ~value:edited; Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** View asset thumbnail
  
      Retrieve the thumbnail image for the specified asset. Viewing the fullsize thumbnail might redirect to downloadAsset, which requires a different permission. *)
  let view_asset ~id ?edited ?key ?size ?slug client () =
    let op_name = "view_asset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/thumbnail" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"edited" ~value:edited; Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"size" ~value:size; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Play asset video
  
      Streams the video file for the specified asset. This endpoint also supports byte range requests. *)
  let play_asset_video ~id ?key ?slug client () =
    let op_name = "play_asset_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/assets/{id}/video/playback" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Setup pin code
  
      Setup a new pin code for the current user. *)
  let setup_pin_code ~body client () =
    let op_name = "setup_pin_code" in
    let url_path = "/auth/pin-code" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json PinCodeSetup.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Change pin code
  
      Change the pin code for the current user. *)
  let change_pin_code ~body client () =
    let op_name = "change_pin_code" in
    let url_path = "/auth/pin-code" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json PinCodeChange.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reset pin code
  
      Reset the pin code for the current user by providing the account password *)
  let reset_pin_code client () =
    let op_name = "reset_pin_code" in
    let url_path = "/auth/pin-code" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Lock auth session
  
      Remove elevated access to locked assets from the current session. *)
  let lock_auth_session client () =
    let op_name = "lock_auth_session" in
    let url_path = "/auth/session/lock" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unlock auth session
  
      Temporarily grant the session elevated access to locked assets by providing the correct PIN code. *)
  let unlock_auth_session ~body client () =
    let op_name = "unlock_auth_session" in
    let url_path = "/auth/session/unlock" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SessionUnlock.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Download asset archive
  
      Download a ZIP archive containing the specified assets. The assets must have been previously requested via the "getDownloadInfo" endpoint. *)
  let download_archive ?key ?slug ~body client () =
    let op_name = "download_archive" in
    let url_path = "/download/archive" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"key" ~value:key; Openapi.Runtime.Query.optional ~key:"slug" ~value:slug]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetIds.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete duplicates
  
      Delete multiple duplicate assets specified by their IDs. *)
  let delete_duplicates client () =
    let op_name = "delete_duplicates" in
    let url_path = "/duplicates" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a duplicate
  
      Delete a single duplicate asset specified by its ID. *)
  let delete_duplicate ~id client () =
    let op_name = "delete_duplicate" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/duplicates/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a face
  
      Create a new face that has not been discovered by facial recognition. The content of the bounding box is considered a face. *)
  let create_face ~body client () =
    let op_name = "create_face" in
    let url_path = "/faces" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AssetFace.CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a face
  
      Delete a face identified by the id. Optionally can be force deleted. *)
  let delete_face ~id client () =
    let op_name = "delete_face" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/faces/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a manual job
  
      Run a specific job. Most jobs are queued automatically, but this endpoint allows for manual creation of a handful of jobs, including various cleanup tasks, as well as creating a new database backup. *)
  let create_job ~body client () =
    let op_name = "create_job" in
    let url_path = "/jobs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Job.CreateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a library
  
      Delete an external library by its ID. *)
  let delete_library ~id client () =
    let op_name = "delete_library" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/libraries/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Scan a library
  
      Queue a scan for the external library to find and import new assets. *)
  let scan_library ~id client () =
    let op_name = "scan_library" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/libraries/{id}/scan" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a memory
  
      Delete a specific memory by its ID. *)
  let delete_memory ~id client () =
    let op_name = "delete_memory" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/memories/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update notifications
  
      Update a list of notifications. Allows to bulk-set the read status of notifications. *)
  let update_notifications ~body client () =
    let op_name = "update_notifications" in
    let url_path = "/notifications" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json NotificationUpdateAll.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete notifications
  
      Delete a list of notifications at once. *)
  let delete_notifications client () =
    let op_name = "delete_notifications" in
    let url_path = "/notifications" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a notification
  
      Delete a specific notification. *)
  let delete_notification ~id client () =
    let op_name = "delete_notification" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/notifications/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Redirect OAuth to mobile
  
      Requests to this URL are automatically forwarded to the mobile app, and is used in some cases for OAuth redirecting. *)
  let redirect_oauth_to_mobile client () =
    let op_name = "redirect_oauth_to_mobile" in
    let url_path = "/oauth/mobile-redirect" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove a partner
  
      Stop sharing assets with a partner. *)
  let remove_partner ~id client () =
    let op_name = "remove_partner" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/partners/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete people
  
      Bulk delete a list of people at once. *)
  let delete_people client () =
    let op_name = "delete_people" in
    let url_path = "/people" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete person
  
      Delete an individual person. *)
  let delete_person ~id client () =
    let op_name = "delete_person" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get person thumbnail
  
      Retrieve the thumbnail file for a person. *)
  let get_person_thumbnail ~id client () =
    let op_name = "get_person_thumbnail" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/people/{id}/thumbnail" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Empty a queue
  
      Removes all jobs from the specified queue. *)
  let empty_queue ~name client () =
    let op_name = "empty_queue" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/queues/{name}/jobs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve search suggestions
  
      Retrieve search suggestions based on partial input. This endpoint is used for typeahead search features. *)
  let get_search_suggestions ?country ?include_null ?lens_model ?make ?model ?state ~type_ client () =
    let op_name = "get_search_suggestions" in
    let url_path = "/search/suggestions" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"country" ~value:country; Openapi.Runtime.Query.optional ~key:"includeNull" ~value:include_null; Openapi.Runtime.Query.optional ~key:"lensModel" ~value:lens_model; Openapi.Runtime.Query.optional ~key:"make" ~value:make; Openapi.Runtime.Query.optional ~key:"model" ~value:model; Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.singleton ~key:"type" ~value:type_]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete server product key
  
      Delete the currently set server product key. *)
  let delete_server_license client () =
    let op_name = "delete_server_license" in
    let url_path = "/server/license" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete all sessions
  
      Delete all sessions for the user. This will not delete the current session. *)
  let delete_all_sessions client () =
    let op_name = "delete_all_sessions" in
    let url_path = "/sessions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a session
  
      Delete a specific session by id. *)
  let delete_session ~id client () =
    let op_name = "delete_session" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/sessions/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Lock a session
  
      Lock a specific session by id. *)
  let lock_session ~id client () =
    let op_name = "lock_session" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/sessions/{id}/lock" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a shared link
  
      Delete a specific shared link by its ID. *)
  let remove_shared_link ~id client () =
    let op_name = "remove_shared_link" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/shared-links/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete stacks
  
      Delete multiple stacks by providing a list of stack IDs. *)
  let delete_stacks client () =
    let op_name = "delete_stacks" in
    let url_path = "/stacks" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a stack
  
      Delete a specific stack by its ID. *)
  let delete_stack ~id client () =
    let op_name = "delete_stack" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/stacks/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove an asset from a stack
  
      Remove a specific asset from a stack by providing the stack ID and asset ID. *)
  let remove_asset_from_stack ~asset_id ~id client () =
    let op_name = "remove_asset_from_stack" in
    let url_path = Openapi.Runtime.Path.render ~params:[("assetId", asset_id); ("id", id)] "/stacks/{id}/assets/{assetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Acknowledge changes
  
      Send a list of synchronization acknowledgements to confirm that the latest changes have been received. *)
  let send_sync_ack ~body client () =
    let op_name = "send_sync_ack" in
    let url_path = "/sync/ack" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SyncAckSet.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete acknowledgements
  
      Delete specific synchronization acknowledgments. *)
  let delete_sync_ack client () =
    let op_name = "delete_sync_ack" in
    let url_path = "/sync/ack" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Stream sync changes
  
      Retrieve a JSON lines streamed response of changes for synchronization. This endpoint is used by the mobile app to efficiently stay up to date with changes. *)
  let get_sync_stream ~body client () =
    let op_name = "get_sync_stream" in
    let url_path = "/sync/stream" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SyncStream.Dto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update admin onboarding
  
      Update the admin onboarding status. *)
  let update_admin_onboarding ~body client () =
    let op_name = "update_admin_onboarding" in
    let url_path = "/system-metadata/admin-onboarding" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AdminOnboarding.UpdateDto.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a tag
  
      Delete a specific tag by its ID. *)
  let delete_tag ~id client () =
    let op_name = "delete_tag" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/tags/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete user product key
  
      Delete the registered product key for the current user. *)
  let delete_user_license client () =
    let op_name = "delete_user_license" in
    let url_path = "/users/me/license" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete user onboarding
  
      Delete the onboarding status of the current user. *)
  let delete_user_onboarding client () =
    let op_name = "delete_user_onboarding" in
    let url_path = "/users/me/onboarding" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete user profile image
  
      Delete the profile image of the current user. *)
  let delete_profile_image client () =
    let op_name = "delete_profile_image" in
    let url_path = "/users/profile-image" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve user profile image
  
      Retrieve the profile image file for a user. *)
  let get_profile_image ~id client () =
    let op_name = "get_profile_image" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/users/{id}/profile-image" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve unique paths
  
      Retrieve a list of unique folder paths from asset original paths. *)
  let get_unique_original_paths client () =
    let op_name = "get_unique_original_paths" in
    let url_path = "/view/folder/unique-paths" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a workflow
  
      Delete a workflow by its ID. *)
  let delete_workflow ~id client () =
    let op_name = "delete_workflow" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/workflows/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end
