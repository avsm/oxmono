let ( let* ) = Result.bind
let unit_result result = Result.map (Fun.const ()) result
let channel_int id = string_of_int (Zulip.Id.Channel.to_int id)

let bool_params name =
  Option.fold ~none:[] ~some:(fun v -> [ (name, string_of_bool v) ])

let string_params name = Option.fold ~none:[] ~some:(fun v -> [ (name, v) ])

let int_params name =
  Option.fold ~none:[] ~some:(fun v -> [ (name, string_of_int v) ])

let nullable_int = function None -> "null" | Some value -> string_of_int value

type retention = Realm_default | Forever | Days of int

let retention_params = function
  | None -> Ok []
  | Some Realm_default -> Ok [ ("message_retention_days", {|"realm_default"|}) ]
  | Some Forever -> Ok [ ("message_retention_days", {|"forever"|}) ]
  | Some (Days days) when days <= 0 ->
      Error (Error.Invalid_request "message retention days must be positive")
  | Some (Days days) -> Ok [ ("message_retention_days", string_of_int days) ]

let json_topics_policy_params = function
  | None -> Ok []
  | Some policy ->
      let* value = Codec.encode Zulip.Channel.Topics_policy.jsont policy in
      Ok [ ("topics_policy", value) ]

let encode_list codec values = Codec.encode (Jsont.list codec) values

let streams_jsont =
  Jsont.Object.map ~kind:"Zulip channels response" Fun.id
  |> Jsont.Object.mem "streams" (Jsont.list Zulip.Channel.jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let list_all client ?include_public ?include_web_public ?include_subscribed
    ?exclude_archived ?include_all_active ?include_all ?include_default
    ?include_owner_subscribed ?include_can_access_content () =
  let params =
    bool_params "include_public" include_public
    @ bool_params "include_web_public" include_web_public
    @ bool_params "include_subscribed" include_subscribed
    @ bool_params "exclude_archived" exclude_archived
    @ bool_params "include_all_active" include_all_active
    @ bool_params "include_all" include_all
    @ bool_params "include_default" include_default
    @ bool_params "include_owner_subscribed" include_owner_subscribed
    @ bool_params "include_can_access_content" include_can_access_content
  in
  Client.request_typed client ~method_:`GET ~path:"/api/v1/streams" ~params
    ~codec:streams_jsont ()

let list client = list_all client ()

let stream_id_jsont =
  Jsont.Object.map ~kind:"Zulip channel ID response" Fun.id
  |> Jsont.Object.mem "stream_id" Zulip.Id.Channel.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let get_id client ~name =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/get_stream_id"
    ~params:[ ("stream", name) ]
    ~codec:stream_id_jsont ()

let stream_jsont =
  Jsont.Object.map ~kind:"Zulip channel response" Fun.id
  |> Jsont.Object.mem "stream" Zulip.Channel.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let get_by_id client ~channel_id =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/streams/" ^ channel_int channel_id)
    ~codec:stream_jsont ()

type include_subscribers = Exclude | Include | Partial

let include_subscribers_string = function
  | Exclude -> "false"
  | Include -> "true"
  | Partial -> "partial"

type subscription_request = {
  name : string;
  color : string option;
  description : string option;
}

let subscription_jsont =
  Jsont.Object.map ~kind:"Zulip subscription request"
    (fun name color description -> { name; color; description })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun s -> s.name)
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun s -> s.color)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun s ->
      s.description)
  |> Jsont.Object.finish

type create_options = {
  name : string;
  description : string option;
  subscribers : Zulip.Id.User.t list;
  announce : bool option;
  invite_only : bool option;
  is_web_public : bool option;
  is_default_stream : bool option;
  history_public_to_subscribers : bool option;
  message_retention_days : retention option;
  folder_id : Zulip.Id.Channel_folder.t option;
  topics_policy : Zulip.Channel.Topics_policy.t option;
  can_add_subscribers_group : Zulip.Group_setting.t option;
  can_create_topic_group : Zulip.Group_setting.t option;
  can_delete_any_message_group : Zulip.Group_setting.t option;
  can_delete_own_message_group : Zulip.Group_setting.t option;
  can_remove_subscribers_group : Zulip.Group_setting.t option;
  can_administer_channel_group : Zulip.Group_setting.t option;
  can_move_messages_out_of_channel_group : Zulip.Group_setting.t option;
  can_move_messages_within_channel_group : Zulip.Group_setting.t option;
  can_send_message_group : Zulip.Group_setting.t option;
  can_subscribe_group : Zulip.Group_setting.t option;
  can_resolve_topics_group : Zulip.Group_setting.t option;
}

let setting_param name = function
  | None -> Ok []
  | Some setting ->
      let* value = Codec.encode Zulip.Group_setting.jsont setting in
      Ok [ (name, value) ]

let initial_params ~description ~announce ~invite_only ~is_web_public
    ~is_default_stream ~history_public_to_subscribers ~message_retention_days
    ~folder_id ~topics_policy ~can_add_subscribers_group ~can_create_topic_group
    ~can_delete_any_message_group ~can_delete_own_message_group
    ~can_remove_subscribers_group ~can_administer_channel_group
    ~can_move_messages_out_of_channel_group
    ~can_move_messages_within_channel_group ~can_send_message_group
    ~can_subscribe_group ~can_resolve_topics_group =
  let* message_retention_days = retention_params message_retention_days in
  let* topics_policy = json_topics_policy_params topics_policy in
  let* can_add_subscribers_group =
    setting_param "can_add_subscribers_group" can_add_subscribers_group
  in
  let* can_create_topic_group =
    setting_param "can_create_topic_group" can_create_topic_group
  in
  let* can_delete_any_message_group =
    setting_param "can_delete_any_message_group" can_delete_any_message_group
  in
  let* can_delete_own_message_group =
    setting_param "can_delete_own_message_group" can_delete_own_message_group
  in
  let* can_remove_subscribers_group =
    setting_param "can_remove_subscribers_group" can_remove_subscribers_group
  in
  let* can_administer_channel_group =
    setting_param "can_administer_channel_group" can_administer_channel_group
  in
  let* can_move_messages_out_of_channel_group =
    setting_param "can_move_messages_out_of_channel_group"
      can_move_messages_out_of_channel_group
  in
  let* can_move_messages_within_channel_group =
    setting_param "can_move_messages_within_channel_group"
      can_move_messages_within_channel_group
  in
  let* can_send_message_group =
    setting_param "can_send_message_group" can_send_message_group
  in
  let* can_subscribe_group =
    setting_param "can_subscribe_group" can_subscribe_group
  in
  let* can_resolve_topics_group =
    setting_param "can_resolve_topics_group" can_resolve_topics_group
  in
  Ok
    (string_params "description" description
    @ bool_params "announce" announce
    @ bool_params "invite_only" invite_only
    @ bool_params "is_web_public" is_web_public
    @ bool_params "is_default_stream" is_default_stream
    @ bool_params "history_public_to_subscribers" history_public_to_subscribers
    @ message_retention_days
    @ int_params "folder_id"
        (Option.map Zulip.Id.Channel_folder.to_int folder_id)
    @ topics_policy @ can_add_subscribers_group @ can_create_topic_group
    @ can_delete_any_message_group @ can_delete_own_message_group
    @ can_remove_subscribers_group @ can_administer_channel_group
    @ can_move_messages_out_of_channel_group
    @ can_move_messages_within_channel_group @ can_send_message_group
    @ can_subscribe_group @ can_resolve_topics_group)

let created_id_jsont =
  Jsont.Object.map ~kind:"Zulip created channel ID" Fun.id
  |> Jsont.Object.mem "id" Zulip.Id.Channel.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let create client options =
  let* subscribers = encode_list Zulip.Id.User.jsont options.subscribers in
  let* params =
    initial_params ~description:options.description ~announce:options.announce
      ~invite_only:options.invite_only ~is_web_public:options.is_web_public
      ~is_default_stream:options.is_default_stream
      ~history_public_to_subscribers:options.history_public_to_subscribers
      ~message_retention_days:options.message_retention_days
      ~folder_id:options.folder_id ~topics_policy:options.topics_policy
      ~can_add_subscribers_group:options.can_add_subscribers_group
      ~can_create_topic_group:options.can_create_topic_group
      ~can_delete_any_message_group:options.can_delete_any_message_group
      ~can_delete_own_message_group:options.can_delete_own_message_group
      ~can_remove_subscribers_group:options.can_remove_subscribers_group
      ~can_administer_channel_group:options.can_administer_channel_group
      ~can_move_messages_out_of_channel_group:
        options.can_move_messages_out_of_channel_group
      ~can_move_messages_within_channel_group:
        options.can_move_messages_within_channel_group
      ~can_send_message_group:options.can_send_message_group
      ~can_subscribe_group:options.can_subscribe_group
      ~can_resolve_topics_group:options.can_resolve_topics_group
  in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/channels/create"
    ~params:(("name", options.name) :: ("subscribers", subscribers) :: params)
    ~codec:created_id_jsont ()

let create_options ~name () =
  {
    name;
    description = None;
    subscribers = [];
    announce = None;
    invite_only = None;
    is_web_public = None;
    is_default_stream = None;
    history_public_to_subscribers = None;
    message_retention_days = None;
    folder_id = None;
    topics_policy = None;
    can_add_subscribers_group = None;
    can_create_topic_group = None;
    can_delete_any_message_group = None;
    can_delete_own_message_group = None;
    can_remove_subscribers_group = None;
    can_administer_channel_group = None;
    can_move_messages_out_of_channel_group = None;
    can_move_messages_within_channel_group = None;
    can_send_message_group = None;
    can_subscribe_group = None;
    can_resolve_topics_group = None;
  }

let create_simple client ~name ?description ?invite_only () =
  create client { (create_options ~name ()) with description; invite_only }

let update_setting name = function
  | None -> Ok []
  | Some setting ->
      let* value = Codec.encode Zulip.Group_setting.update_jsont setting in
      Ok [ (name, value) ]

let update client ~channel_id ?description ?new_name ?is_private ?is_web_public
    ?history_public_to_subscribers ?is_default_stream ?message_retention_days
    ?is_archived ?folder_id ?topics_policy ?can_add_subscribers_group
    ?can_create_topic_group ?can_delete_any_message_group
    ?can_delete_own_message_group ?can_remove_subscribers_group
    ?can_administer_channel_group ?can_move_messages_out_of_channel_group
    ?can_move_messages_within_channel_group ?can_send_message_group
    ?can_subscribe_group ?can_resolve_topics_group ?stream_post_policy () =
  let* a =
    update_setting "can_add_subscribers_group" can_add_subscribers_group
  in
  let* b = update_setting "can_create_topic_group" can_create_topic_group in
  let* c =
    update_setting "can_delete_any_message_group" can_delete_any_message_group
  in
  let* d =
    update_setting "can_delete_own_message_group" can_delete_own_message_group
  in
  let* e =
    update_setting "can_remove_subscribers_group" can_remove_subscribers_group
  in
  let* f =
    update_setting "can_administer_channel_group" can_administer_channel_group
  in
  let* g =
    update_setting "can_move_messages_out_of_channel_group"
      can_move_messages_out_of_channel_group
  in
  let* h =
    update_setting "can_move_messages_within_channel_group"
      can_move_messages_within_channel_group
  in
  let* i = update_setting "can_send_message_group" can_send_message_group in
  let* j = update_setting "can_subscribe_group" can_subscribe_group in
  let* k = update_setting "can_resolve_topics_group" can_resolve_topics_group in
  let* message_retention_days = retention_params message_retention_days in
  let topics_policy =
    Option.fold ~none:[]
      ~some:(fun policy ->
        [ ("topics_policy", Zulip.Channel.Topics_policy.to_string policy) ])
      topics_policy
  in
  let params =
    string_params "description" description
    @ string_params "new_name" new_name
    @ bool_params "is_private" is_private
    @ bool_params "is_web_public" is_web_public
    @ bool_params "history_public_to_subscribers" history_public_to_subscribers
    @ bool_params "is_default_stream" is_default_stream
    @ message_retention_days
    @ bool_params "is_archived" is_archived
    @ Option.fold ~none:[]
        ~some:(fun v ->
          [
            ( "folder_id",
              nullable_int (Option.map Zulip.Id.Channel_folder.to_int v) );
          ])
        folder_id
    @ topics_policy @ a @ b @ c @ d @ e @ f @ g @ h @ i @ j @ k
    @ int_params "stream_post_policy" stream_post_policy
  in
  if params = [] then
    Error (Error.Invalid_request "channel update has no changes")
  else
    Client.request client ~method_:`PATCH
      ~path:("/api/v1/streams/" ^ channel_int channel_id)
      ~params ()
    |> unit_result

let delete client ~channel_id =
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/streams/" ^ channel_int channel_id)
    ()
  |> unit_result

let archive = delete

let set_archived client ~channel_id ~archived =
  update client ~channel_id ~is_archived:archived ()

let change_default method_ client ~channel_id =
  Client.request client ~method_ ~path:"/api/v1/default_streams"
    ~params:[ ("stream_id", channel_int channel_id) ]
    ()
  |> unit_result

let add_default = change_default `POST
let remove_default = change_default `DELETE

type subscription_result = {
  subscribed : (string * string list) list;
  already_subscribed : (string * string list) list;
  unauthorized : string list;
  new_subscription_messages_sent : bool option;
  raw : Jsont.json;
}

type subscription_update_result = {
  subscribed : (string * string list) list;
  already_subscribed : (string * string list) list;
  not_removed : string list;
  removed : string list;
  new_subscription_messages_sent : bool option;
  raw : Jsont.json;
}

type unsubscribe_result = {
  not_removed : string list;
  removed : string list;
  raw : Jsont.json;
}

let member name = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((member_name, _), value) ->
          if String.equal member_name name then Some value else None)
        members
  | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json

let decode codec json =
  match Jsont.Json.decode' codec json with
  | Ok value -> value
  | Error error -> raise (Jsont.Error error)

let string_map = function
  | Jsont.Object (members, _) ->
      List.map
        (fun ((name, _), value) ->
          (name, decode (Jsont.list Jsont.string) value))
        members
  | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json

let optional codec name json = Option.map (decode codec) (member name json)

let default codec name value json =
  Option.value ~default:value (optional codec name json)

let subscription_result_jsont =
  Jsont.map ~kind:"Zulip subscribe response"
    ~dec:(fun raw ->
      {
        subscribed =
          default Jsont.json "subscribed" (Jsont.Json.object' []) raw
          |> string_map;
        already_subscribed =
          default Jsont.json "already_subscribed" (Jsont.Json.object' []) raw
          |> string_map;
        unauthorized = default (Jsont.list Jsont.string) "unauthorized" [] raw;
        new_subscription_messages_sent =
          optional Jsont.bool "new_subscription_messages_sent" raw;
        raw;
      })
    ~enc:(fun result -> result.raw)
    Jsont.json

let subscription_update_result_jsont =
  Jsont.map ~kind:"Zulip subscription update response"
    ~dec:(fun raw ->
      {
        subscribed =
          default Jsont.json "subscribed" (Jsont.Json.object' []) raw
          |> string_map;
        already_subscribed =
          default Jsont.json "already_subscribed" (Jsont.Json.object' []) raw
          |> string_map;
        not_removed = default (Jsont.list Jsont.string) "not_removed" [] raw;
        removed = default (Jsont.list Jsont.string) "removed" [] raw;
        new_subscription_messages_sent =
          optional Jsont.bool "new_subscription_messages_sent" raw;
        raw;
      })
    ~enc:(fun result -> result.raw)
    Jsont.json

let unsubscribe_result_jsont =
  Jsont.map ~kind:"Zulip unsubscribe response"
    ~dec:(fun raw ->
      {
        not_removed = default (Jsont.list Jsont.string) "not_removed" [] raw;
        removed = default (Jsont.list Jsont.string) "removed" [] raw;
        raw;
      })
    ~enc:(fun result -> result.raw)
    Jsont.json

let principal_param = function
  | None -> Ok []
  | Some (`Emails values) ->
      let* value = encode_list Jsont.string values in
      Ok [ ("principals", value) ]
  | Some (`User_ids values) ->
      let* value = encode_list Zulip.Id.User.jsont values in
      Ok [ ("principals", value) ]

let subscribe client ~subscriptions ?principals ?authorization_errors_fatal
    ?announce ?invite_only ?is_web_public ?is_default_stream
    ?history_public_to_subscribers ?message_retention_days ?folder_id
    ?topics_policy ?can_add_subscribers_group ?can_create_topic_group
    ?can_delete_any_message_group ?can_delete_own_message_group
    ?can_remove_subscribers_group ?can_administer_channel_group
    ?can_move_messages_out_of_channel_group
    ?can_move_messages_within_channel_group ?can_send_message_group
    ?can_subscribe_group ?can_resolve_topics_group
    ?send_new_subscription_messages () =
  let* subscriptions = encode_list subscription_jsont subscriptions in
  let* principals = principal_param principals in
  let* params =
    initial_params ~description:None ~announce ~invite_only ~is_web_public
      ~is_default_stream ~history_public_to_subscribers ~message_retention_days
      ~folder_id ~topics_policy ~can_add_subscribers_group
      ~can_create_topic_group ~can_delete_any_message_group
      ~can_delete_own_message_group ~can_remove_subscribers_group
      ~can_administer_channel_group ~can_move_messages_out_of_channel_group
      ~can_move_messages_within_channel_group ~can_send_message_group
      ~can_subscribe_group ~can_resolve_topics_group
  in
  Client.request_typed client ~method_:`POST
    ~path:"/api/v1/users/me/subscriptions"
    ~params:
      ([ ("subscriptions", subscriptions) ]
      @ principals
      @ bool_params "authorization_errors_fatal" authorization_errors_fatal
      @ params
      @ bool_params "send_new_subscription_messages"
          send_new_subscription_messages)
    ~codec:subscription_result_jsont ()

let subscribe_simple client ~channels =
  subscribe client
    ~subscriptions:
      (List.map
         (fun name -> { name; color = None; description = None })
         channels)
    ()
  |> unit_result

let unsubscribe client ~subscriptions ?principals () =
  let* subscriptions = encode_list Jsont.string subscriptions in
  let* principals = principal_param principals in
  Client.request_typed client ~method_:`DELETE
    ~path:"/api/v1/users/me/subscriptions"
    ~params:([ ("subscriptions", subscriptions) ] @ principals)
    ~codec:unsubscribe_result_jsont ()

let unsubscribe_simple client ~channels =
  unsubscribe client ~subscriptions:channels () |> unit_result

let update_subscriptions client ?(add = []) ?(remove = []) () =
  if add = [] && remove = [] then
    Error (Error.Invalid_request "subscription update has no changes")
  else
    let* add = encode_list subscription_jsont add in
    let* remove = encode_list Jsont.string remove in
    Client.request_typed client ~method_:`PATCH
      ~path:"/api/v1/users/me/subscriptions"
      ~params:[ ("add", add); ("delete", remove) ]
      ~codec:subscription_update_result_jsont ()

let subscriptions_jsont =
  Jsont.Object.map ~kind:"Zulip subscriptions response" Fun.id
  |> Jsont.Object.mem "subscriptions"
       (Jsont.list Zulip.Channel.Subscription.jsont)
       ~enc:Fun.id
  |> Jsont.Object.finish

let get_subscriptions_with client ?include_subscribers () =
  let params =
    Option.fold ~none:[]
      ~some:(fun value ->
        [ ("include_subscribers", include_subscribers_string value) ])
      include_subscribers
  in
  Client.request_typed client ~method_:`GET
    ~path:"/api/v1/users/me/subscriptions" ~params ~codec:subscriptions_jsont ()

let get_subscriptions client = get_subscriptions_with client ()

let channel_ids_jsont =
  Jsont.Object.map ~kind:"Zulip subscribed channel IDs" Fun.id
  |> Jsont.Object.mem "subscribed_channel_ids"
       (Jsont.list Zulip.Id.Channel.jsont)
       ~enc:Fun.id
  |> Jsont.Object.finish

let get_user_channels client ~user_id =
  Client.request_typed client ~method_:`GET
    ~path:
      (Printf.sprintf "/api/v1/users/%d/channels"
         (Zulip.Id.User.to_int user_id))
    ~codec:channel_ids_jsont ()

let subscribed_jsont =
  Jsont.Object.map ~kind:"Zulip subscription status" Fun.id
  |> Jsont.Object.mem "is_subscribed" Jsont.bool ~enc:Fun.id
  |> Jsont.Object.finish

let get_subscription_status client ~user_id ~channel_id =
  Client.request_typed client ~method_:`GET
    ~path:
      (Printf.sprintf "/api/v1/users/%d/subscriptions/%d"
         (Zulip.Id.User.to_int user_id)
         (Zulip.Id.Channel.to_int channel_id))
    ~codec:subscribed_jsont ()

type subscription_property =
  | Color of string
  | Is_muted of bool
  | In_home_view of bool
  | Pin_to_top of bool
  | Desktop_notifications of bool
  | Audible_notifications of bool
  | Push_notifications of bool
  | Email_notifications of bool
  | Wildcard_mentions_notify of bool

type property_update = {
  channel_id : Zulip.Id.Channel.t;
  property : subscription_property;
}

let property_name_value = function
  | Color value -> ("color", Jsont.Json.string value)
  | Is_muted value -> ("is_muted", Jsont.Json.bool value)
  | In_home_view value -> ("in_home_view", Jsont.Json.bool value)
  | Pin_to_top value -> ("pin_to_top", Jsont.Json.bool value)
  | Desktop_notifications value ->
      ("desktop_notifications", Jsont.Json.bool value)
  | Audible_notifications value ->
      ("audible_notifications", Jsont.Json.bool value)
  | Push_notifications value -> ("push_notifications", Jsont.Json.bool value)
  | Email_notifications value -> ("email_notifications", Jsont.Json.bool value)
  | Wildcard_mentions_notify value ->
      ("wildcard_mentions_notify", Jsont.Json.bool value)

type property_wire = {
  stream_id : Zulip.Id.Channel.t;
  property : string;
  value : Jsont.json;
}

let property_wire_jsont =
  Jsont.Object.map ~kind:"Zulip subscription property"
    (fun stream_id property value -> { stream_id; property; value })
  |> Jsont.Object.mem "stream_id" Zulip.Id.Channel.jsont ~enc:(fun p ->
      p.stream_id)
  |> Jsont.Object.mem "property" Jsont.string ~enc:(fun p -> p.property)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun p -> p.value)
  |> Jsont.Object.finish

let update_subscription_properties client updates =
  if updates = [] then
    Error (Error.Invalid_request "subscription property update has no changes")
  else
    let wires =
      List.map
        (fun (update : property_update) ->
          let property, value = property_name_value update.property in
          { stream_id = update.channel_id; property; value })
        updates
    in
    let* value = encode_list property_wire_jsont wires in
    Client.request client ~method_:`POST
      ~path:"/api/v1/users/me/subscriptions/properties"
      ~params:[ ("subscription_data", value) ]
      ()
    |> unit_result

let update_subscription_property client ~channel_id property =
  let name, value = property_name_value property in
  let* value = Codec.encode Jsont.json value in
  Client.request client ~method_:`PATCH
    ~path:("/api/v1/users/me/subscriptions/" ^ channel_int channel_id)
    ~params:[ ("property", name); ("value", value) ]
    ()
  |> unit_result

let update_subscription_settings client ~channel_id ?color ?is_muted ?pin_to_top
    ?desktop_notifications ?audible_notifications ?push_notifications
    ?email_notifications ?wildcard_mentions_notify () =
  let opt make =
    Option.fold ~none:[] ~some:(fun value ->
        [ { channel_id; property = make value } ])
  in
  update_subscription_properties client
    (opt (fun value -> Color value) color
    @ opt (fun value -> Is_muted value) is_muted
    @ opt (fun value -> Pin_to_top value) pin_to_top
    @ opt (fun value -> Desktop_notifications value) desktop_notifications
    @ opt (fun value -> Audible_notifications value) audible_notifications
    @ opt (fun value -> Push_notifications value) push_notifications
    @ opt (fun value -> Email_notifications value) email_notifications
    @ opt (fun value -> Wildcard_mentions_notify value) wildcard_mentions_notify
    )

module Topic = struct
  type t = { name : string; max_id : Zulip.Id.Message.t }

  let name t = t.name
  let max_id t = t.max_id

  let jsont =
    Jsont.Object.map ~kind:"Zulip topic" (fun name max_id -> { name; max_id })
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "max_id" Zulip.Id.Message.jsont ~enc:max_id
    |> Jsont.Object.finish
end

let topics_jsont =
  Jsont.Object.map ~kind:"Zulip topics response" Fun.id
  |> Jsont.Object.mem "topics" (Jsont.list Topic.jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let get_topics client ~channel_id ?allow_empty_topic_name () =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/users/me/" ^ channel_int channel_id ^ "/topics")
    ~params:(bool_params "allow_empty_topic_name" allow_empty_topic_name)
    ~codec:topics_jsont ()

let delete_topic_jsont =
  Jsont.Object.map ~kind:"Zulip delete-topic response" (function
    | true -> `Complete
    | false -> `Incomplete)
  |> Jsont.Object.mem "complete" Jsont.bool ~enc:(function
    | `Complete -> true
    | `Incomplete -> false)
  |> Jsont.Object.finish

let delete_topic client ~channel_id ~topic =
  Client.request_typed client ~method_:`POST
    ~path:("/api/v1/streams/" ^ channel_int channel_id ^ "/delete_topic")
    ~params:[ ("topic_name", topic) ]
    ~codec:delete_topic_jsont ()

type mute_op = Mute | Unmute

let set_topic_visibility client ~channel_id ~topic ~visibility_policy =
  Client.request client ~method_:`POST ~path:"/api/v1/user_topics"
    ~params:
      [
        ("stream_id", channel_int channel_id);
        ("topic", topic);
        ( "visibility_policy",
          string_of_int (Zulip.Topic_visibility.to_int visibility_policy) );
      ]
    ()
  |> unit_result

let set_topic_mute client ~channel_id ~topic ~op =
  set_topic_visibility client ~channel_id ~topic
    ~visibility_policy:
      (match op with Mute -> Zulip.Topic_visibility.Muted | Unmute -> Unmuted)

let subscribers_jsont =
  Jsont.Object.map ~kind:"Zulip channel subscribers" Fun.id
  |> Jsont.Object.mem "subscribers" (Jsont.list Zulip.Id.User.jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let get_subscribers client ~channel_id =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/streams/" ^ channel_int channel_id ^ "/members")
    ~codec:subscribers_jsont ()

let get_subscribers_by_name client ~name =
  let* channel_id = get_id client ~name in
  get_subscribers client ~channel_id

let email_jsont =
  Jsont.Object.map ~kind:"Zulip channel email" Fun.id
  |> Jsont.Object.mem "email" Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

let get_email_address client ~channel_id =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/streams/" ^ channel_int channel_id ^ "/email_address")
    ~codec:email_jsont ()
