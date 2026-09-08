let ( let* ) = Result.bind
let unit_result = Result.map (Fun.const ())
let user_path user_id = string_of_int (Zulip.Id.User.to_int user_id)

let bool_param name = function
  | None -> []
  | Some value -> [ (name, string_of_bool value) ]

let list_jsont =
  Jsont.Object.map ~kind:"Zulip users response" Fun.id
  |> Jsont.Object.mem "members" (Jsont.list Zulip.User.jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let user_response_jsont =
  Jsont.Object.map ~kind:"Zulip user response" Fun.id
  |> Jsont.Object.mem "user" Zulip.User.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let user_query ?client_gravatar ?include_custom_profile_fields ?user_ids () =
  let* ids =
    match user_ids with
    | None -> Ok []
    | Some ids ->
        let* value = Codec.encode (Jsont.list Zulip.Id.User.jsont) ids in
        Ok [ ("user_ids", value) ]
  in
  Ok
    (bool_param "client_gravatar" client_gravatar
    @ bool_param "include_custom_profile_fields" include_custom_profile_fields
    @ ids)

let list_all client ?client_gravatar ?include_custom_profile_fields ?user_ids ()
    =
  let* params =
    user_query ?client_gravatar ?include_custom_profile_fields ?user_ids ()
  in
  Client.request_typed client ~method_:`GET ~path:"/api/v1/users" ~params
    ~codec:list_jsont ()

let list client = list_all client ()

let decode_user_response json =
  match Codec.decode user_response_jsont json with
  | Ok user -> Ok user
  | Error _ -> Codec.decode Zulip.User.jsont json

let get_by_id client ~user_id ?client_gravatar ?include_custom_profile_fields ()
    =
  let* params = user_query ?client_gravatar ?include_custom_profile_fields () in
  let* json =
    Client.request client ~method_:`GET
      ~path:("/api/v1/users/" ^ user_path user_id)
      ~params ()
  in
  decode_user_response json

let get client ~email ?client_gravatar ?include_custom_profile_fields () =
  let* params = user_query ?client_gravatar ?include_custom_profile_fields () in
  let* json =
    Client.request client ~method_:`GET
      ~path:("/api/v1/users/" ^ Client.path_segment email)
      ~params ()
  in
  decode_user_response json

let me client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/users/me"
    ~codec:Zulip.User.jsont ()

type create_result = { user_id : Zulip.Id.User.t; extensions : Jsont.json }

let create_result_jsont =
  Jsont.Object.map ~kind:"Zulip create-user response" (fun user_id extensions ->
      { user_id; extensions })
  |> Jsont.Object.mem "user_id" Zulip.Id.User.jsont ~enc:(fun r -> r.user_id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun r -> r.extensions)
  |> Jsont.Object.finish

let create_detailed client ~email ~password ~full_name =
  Client.request_typed client ~method_:`POST ~path:"/api/v1/users"
    ~params:
      [ ("email", email); ("password", password); ("full_name", full_name) ]
    ~codec:create_result_jsont ()

let create client ~email ~password ~full_name =
  Result.map
    (fun result -> result.user_id)
    (create_detailed client ~email ~password ~full_name)

type profile_value = Remove | Text of string | Users of Zulip.Id.User.t list

type profile_update = {
  field_id : Zulip.Id.Profile_field.t;
  value : profile_value;
}

let profile_value_jsont =
  let dec = function
    | Jsont.Null _ -> Remove
    | Jsont.String (value, _) -> Text value
    | Jsont.Array (values, _) ->
        let users =
          List.map
            (fun json ->
              match Jsont.Json.decode' Zulip.Id.User.jsont json with
              | Ok user_id -> user_id
              | Error error -> raise (Jsont.Error error))
            values
        in
        Users users
    | json ->
        Jsont.Error.msgf (Jsont.Json.meta json)
          "expected null, a string, or an array of user IDs"
  in
  let enc = function
    | Remove -> Jsont.Json.null ()
    | Text value -> Jsont.Json.string value
    | Users ids ->
        Jsont.Json.list
          (List.map (fun id -> Jsont.Json.int (Zulip.Id.User.to_int id)) ids)
  in
  Jsont.map ~kind:"Zulip profile value update" ~dec ~enc Jsont.json

let profile_update_jsont =
  Jsont.Object.map ~kind:"Zulip profile update" (fun field_id value ->
      { field_id; value })
  |> Jsont.Object.mem "id" Zulip.Id.Profile_field.jsont ~enc:(fun update ->
      update.field_id)
  |> Jsont.Object.mem "value" profile_value_jsont ~enc:(fun update ->
      update.value)
  |> Jsont.Object.finish

let encode_json_param name codec value =
  let* value = Codec.encode codec value in
  Ok [ (name, value) ]

let update client ~user_id ?full_name ?role ?profile_data ?new_email () =
  let* profile_params =
    match profile_data with
    | None -> Ok []
    | Some updates ->
        encode_json_param "profile_data"
          (Jsont.list profile_update_jsont)
          updates
  in
  let params =
    Option.fold ~none:[] ~some:(fun value -> [ ("full_name", value) ]) full_name
    @ Option.fold ~none:[]
        ~some:(fun value ->
          [ ("role", string_of_int (Zulip.User.Role.to_int value)) ])
        role
    @ profile_params
    @ Option.fold ~none:[]
        ~some:(fun value -> [ ("new_email", value) ])
        new_email
  in
  if params = [] then Error (Error.Invalid_request "user update has no changes")
  else
    Client.request client ~method_:`PATCH
      ~path:("/api/v1/users/" ^ user_path user_id)
      ~params ()
    |> unit_result

type deactivation_actions = {
  delete_profile : bool option;
  delete_public_channel_messages : bool option;
  delete_private_channel_messages : bool option;
  delete_direct_messages : bool option;
}

let deactivation_actions_jsont =
  Jsont.Object.map ~kind:"Zulip user deactivation actions"
    (fun
      delete_profile
      delete_public_channel_messages
      delete_private_channel_messages
      delete_direct_messages
    ->
      {
        delete_profile;
        delete_public_channel_messages;
        delete_private_channel_messages;
        delete_direct_messages;
      })
  |> Jsont.Object.opt_mem "delete_profile" Jsont.bool ~enc:(fun actions ->
      actions.delete_profile)
  |> Jsont.Object.opt_mem "delete_public_channel_messages" Jsont.bool
       ~enc:(fun actions -> actions.delete_public_channel_messages)
  |> Jsont.Object.opt_mem "delete_private_channel_messages" Jsont.bool
       ~enc:(fun actions -> actions.delete_private_channel_messages)
  |> Jsont.Object.opt_mem "delete_direct_messages" Jsont.bool
       ~enc:(fun actions -> actions.delete_direct_messages)
  |> Jsont.Object.finish

let deactivate client ~user_id ?actions ?notification_comment () =
  let* action_params =
    match actions with
    | None -> Ok []
    | Some actions ->
        encode_json_param "actions" deactivation_actions_jsont actions
  in
  let params =
    action_params
    @ Option.fold ~none:[]
        ~some:(fun value -> [ ("deactivation_notification_comment", value) ])
        notification_comment
  in
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/users/" ^ user_path user_id)
    ~params ()
  |> unit_result

let deactivate_me client =
  Client.request client ~method_:`DELETE ~path:"/api/v1/users/me" ()
  |> unit_result

let reactivate client ~user_id =
  Client.request client ~method_:`POST
    ~path:("/api/v1/users/" ^ user_path user_id ^ "/reactivate")
    ()
  |> unit_result

let words_jsont =
  Jsont.Object.map ~kind:"Zulip alert words" Fun.id
  |> Jsont.Object.mem "alert_words" (Jsont.list Jsont.string) ~enc:Fun.id
  |> Jsont.Object.finish

let get_alert_words client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/users/me/alert_words"
    ~codec:words_jsont ()

let change_alert_words method_ client words =
  let* words = Codec.encode (Jsont.list Jsont.string) words in
  Client.request_typed client ~method_ ~path:"/api/v1/users/me/alert_words"
    ~params:[ ("alert_words", words) ]
    ~codec:words_jsont ()

let add_alert_words client ~words = change_alert_words `POST client words
let remove_alert_words client ~words = change_alert_words `DELETE client words

type reaction_type =
  | Unicode_emoji
  | Realm_emoji
  | Zulip_extra_emoji
  | Other_reaction_type of string

let reaction_type_to_string = function
  | Unicode_emoji -> "unicode_emoji"
  | Realm_emoji -> "realm_emoji"
  | Zulip_extra_emoji -> "zulip_extra_emoji"
  | Other_reaction_type value -> value

let reaction_type_of_string = function
  | "unicode_emoji" -> Unicode_emoji
  | "realm_emoji" -> Realm_emoji
  | "zulip_extra_emoji" -> Zulip_extra_emoji
  | value -> Other_reaction_type value

let reaction_type_jsont =
  Jsont.map ~kind:"Zulip reaction type" ~dec:reaction_type_of_string
    ~enc:reaction_type_to_string Jsont.string

type status_emoji = {
  emoji_name : string;
  emoji_code : string;
  reaction_type : reaction_type;
}

type status_emoji_update = Clear_emoji | Set_emoji of status_emoji

type user_status = {
  away : bool option;
  status_text : string option;
  emoji : status_emoji option;
  extensions : Jsont.json;
}

type status_wire = {
  away : bool option;
  status_text : string option;
  emoji_name : string option;
  emoji_code : string option;
  reaction_type : reaction_type option;
  extensions : Jsont.json;
}

let status_of_wire wire =
  let emoji =
    match (wire.emoji_name, wire.emoji_code, wire.reaction_type) with
    | None, None, None -> None
    | Some emoji_name, Some emoji_code, Some reaction_type ->
        Some { emoji_name; emoji_code; reaction_type }
    | _ ->
        Jsont.Error.msgf Jsont.Meta.none
          "status emoji_name, emoji_code, and reaction_type must appear \
           together"
  in
  {
    away = wire.away;
    status_text = wire.status_text;
    emoji;
    extensions = wire.extensions;
  }

let wire_of_status status =
  let emoji_name, emoji_code, reaction_type =
    match status.emoji with
    | None -> (None, None, None)
    | Some emoji ->
        (Some emoji.emoji_name, Some emoji.emoji_code, Some emoji.reaction_type)
  in
  {
    away = status.away;
    status_text = status.status_text;
    emoji_name;
    emoji_code;
    reaction_type;
    extensions = status.extensions;
  }

let status_wire_jsont =
  Jsont.Object.map ~kind:"Zulip user status"
    (fun away status_text emoji_name emoji_code reaction_type extensions ->
      { away; status_text; emoji_name; emoji_code; reaction_type; extensions })
  |> Jsont.Object.opt_mem "away" Jsont.bool ~enc:(fun status -> status.away)
  |> Jsont.Object.opt_mem "status_text" Jsont.string ~enc:(fun status ->
      status.status_text)
  |> Jsont.Object.opt_mem "emoji_name" Jsont.string ~enc:(fun status ->
      status.emoji_name)
  |> Jsont.Object.opt_mem "emoji_code" Jsont.string ~enc:(fun status ->
      status.emoji_code)
  |> Jsont.Object.opt_mem "reaction_type" reaction_type_jsont
       ~enc:(fun status -> status.reaction_type)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun status ->
      status.extensions)
  |> Jsont.Object.finish

let user_status_jsont =
  Jsont.map ~kind:"Zulip user status" ~dec:status_of_wire ~enc:wire_of_status
    status_wire_jsont

let status_response_jsont =
  Jsont.Object.map ~kind:"Zulip user-status response" Fun.id
  |> Jsont.Object.mem "status" user_status_jsont ~enc:Fun.id
  |> Jsont.Object.finish

let get_status client ~user_id =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/users/" ^ user_path user_id ^ "/status")
    ~codec:status_response_jsont ()

let status_params ?away ?text ?emoji () =
  bool_param "away" away
  @ Option.fold ~none:[] ~some:(fun value -> [ ("status_text", value) ]) text
  @ Option.fold ~none:[]
      ~some:(function
        | Clear_emoji -> [ ("emoji_name", "") ]
        | Set_emoji emoji ->
            [
              ("emoji_name", emoji.emoji_name);
              ("emoji_code", emoji.emoji_code);
              ("reaction_type", reaction_type_to_string emoji.reaction_type);
            ])
      emoji

let update_status client ?away ?text ?emoji () =
  let params = status_params ?away ?text ?emoji () in
  if params = [] then
    Error (Error.Invalid_request "status update has no changes")
  else
    Client.request client ~method_:`POST ~path:"/api/v1/users/me/status" ~params
      ()
    |> unit_result

let update_status_for_user client ~user_id ?text ?emoji () =
  let params = status_params ?text ?emoji () in
  if params = [] then
    Error (Error.Invalid_request "status update has no changes")
  else
    Client.request client ~method_:`POST
      ~path:("/api/v1/users/" ^ user_path user_id ^ "/status")
      ~params ()
    |> unit_result

let update_profile_data client ~updates =
  let* params =
    encode_json_param "data" (Jsont.list profile_update_jsont) updates
  in
  Client.request client ~method_:`PATCH ~path:"/api/v1/users/me/profile_data"
    ~params ()
  |> unit_result

let remove_profile_data client ~field_ids =
  let* params =
    encode_json_param "data" (Jsont.list Zulip.Id.Profile_field.jsont) field_ids
  in
  Client.request client ~method_:`DELETE ~path:"/api/v1/users/me/profile_data"
    ~params ()
  |> unit_result

let avatar_response_jsont =
  Jsont.Object.map ~kind:"Zulip avatar response" Fun.id
  |> Jsont.Object.mem "avatar_url" Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

let decode_avatar_response result =
  Result.bind result (Codec.decode avatar_response_jsont)

let upload_avatar client ~filename ~content_type content =
  Client.multipart client ~path:"/api/v1/users/me/avatar"
    [ Fetch.Form.file ~name:"file" ~filename ~content_type content ]
  |> decode_avatar_response

let upload_avatar_stream client ~filename ~content_type ?length source =
  if Option.exists (fun n -> n < 0L) length then
    Error (Error.Invalid_request "Upload length must be nonnegative")
  else
    Client.multipart client ~path:"/api/v1/users/me/avatar"
      [ Fetch.Form.stream ~name:"file" ~filename ~content_type ?length source ]
    |> decode_avatar_response

let delete_avatar client =
  Client.request client ~method_:`DELETE ~path:"/api/v1/users/me/avatar" ()
  |> unit_result

let mute_user client ~user_id =
  Client.request client ~method_:`POST
    ~path:("/api/v1/users/me/muted_users/" ^ user_path user_id)
    ()
  |> unit_result

let unmute_user client ~user_id =
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/users/me/muted_users/" ^ user_path user_id)
    ()
  |> unit_result
