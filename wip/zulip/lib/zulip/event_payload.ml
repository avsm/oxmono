type op =
  | Add
  | Remove
  | Update
  | Create
  | Delete
  | Peer_add
  | Peer_remove
  | Add_members
  | Remove_members
  | Add_subgroups
  | Remove_subgroups
  | Other_op of string

let op_of_string = function
  | "add" -> Add
  | "remove" -> Remove
  | "update" -> Update
  | "create" -> Create
  | "delete" -> Delete
  | "peer_add" -> Peer_add
  | "peer_remove" -> Peer_remove
  | "add_members" -> Add_members
  | "remove_members" -> Remove_members
  | "add_subgroups" -> Add_subgroups
  | "remove_subgroups" -> Remove_subgroups
  | value -> Other_op value

let op_to_string = function
  | Add -> "add"
  | Remove -> "remove"
  | Update -> "update"
  | Create -> "create"
  | Delete -> "delete"
  | Peer_add -> "peer_add"
  | Peer_remove -> "peer_remove"
  | Add_members -> "add_members"
  | Remove_members -> "remove_members"
  | Add_subgroups -> "add_subgroups"
  | Remove_subgroups -> "remove_subgroups"
  | Other_op value -> value

type change = [ `Add | `Remove | `Other of string ]

let change_to_string = function
  | `Add -> "add"
  | `Remove -> "remove"
  | `Other value -> value

type error = {
  event_type : Event_type.t;
  path : string list;
  message : string;
  cause : Jsont.Error.t option;
}

type problem = {
  path : string list;
  message : string;
  cause : Jsont.Error.t option;
}

let pp_error ppf (error : error) =
  Format.fprintf ppf "%s%s: %s"
    (Event_type.to_string error.event_type)
    (String.concat "" (List.map (fun member -> "." ^ member) error.path))
    error.message

let error_to_string error = Format.asprintf "%a" pp_error error

let problem ?(path = []) ?cause message =
  Error ({ path; message; cause } : problem)

type message = {
  message : Message.t;
  flags : Message_flag.t list;
  raw : Jsont.json;
}

type message_edit = {
  message_id : Id.Message.t;
  message_ids : Id.Message.t list;
  user_id : Id.User.t option;
  edit_timestamp : float option;
  content : string option;
  rendered_content : string option;
  channel_id : Id.Channel.t option;
  new_channel_id : Id.Channel.t option;
  topic : string option;
  raw : Jsont.json;
}

type message_delete = {
  message_ids : Id.Message.t list;
  message_type : Message_type.t option;
  raw : Jsont.json;
}

type reaction = {
  op : change;
  message_id : Id.Message.t;
  user_id : Id.User.t;
  emoji_name : string;
  emoji_code : string option;
  reaction_type : string option;
  raw : Jsont.json;
}

type message_flags = {
  op : change;
  flag : Message_flag.t;
  message_ids : Id.Message.t list;
  all : bool option;
  raw : Jsont.json;
}

type realm_user = {
  op : op;
  user_id : Id.User.t;
  full_name : string option;
  email : string option;
  new_email : string option;
  is_bot : bool option;
  added_user : User.t option;
  person : Jsont.json;
  raw : Jsont.json;
}

type channel = {
  op : op;
  channel_ids : Id.Channel.t list;
  channels : Channel.t list;
  channel_id : Id.Channel.t option;
  property : string option;
  value : Jsont.json option;
  raw : Jsont.json;
}

type subscription = {
  op : op;
  channel_ids : Id.Channel.t list;
  user_ids : Id.User.t list;
  subscriptions : Channel.Subscription.t list;
  property : string option;
  value : Jsont.json option;
  raw : Jsont.json;
}

type user_status = {
  user_id : Id.User.t;
  away : bool option;
  status_text : string option;
  emoji_name : string option;
  emoji_code : string option;
  reaction_type : string option;
  raw : Jsont.json;
}

type topic = {
  channel_id : Id.Channel.t;
  topic_name : string;
  last_updated : float;
  visibility_policy : Topic_visibility.t;
  raw : Jsont.json;
}

type user_group = {
  op : op;
  group_id : Id.User_group.t;
  name : string option;
  user_ids : Id.User.t list;
  direct_subgroup_ids : Id.User_group.t list;
  data : Jsont.json option;
  raw : Jsont.json;
}

type presence = {
  user_id : Id.User.t option;
  email : string option;
  server_timestamp : float option;
  active_timestamp : float option;
  idle_timestamp : float option;
  presence : Jsont.json option;
  presences : Jsont.json option;
  raw : Jsont.json;
}

type unknown = { event_type : Event_type.t; raw : Jsont.json }

type t =
  | Message of message
  | Message_edit of message_edit
  | Message_delete of message_delete
  | Reaction of reaction
  | Message_flags of message_flags
  | Realm_user of realm_user
  | Channel of channel
  | Subscription of subscription
  | User_status of user_status
  | Topic of topic
  | User_group of user_group
  | Presence of presence
  | Unknown of unknown

let raw = function
  | Message event -> event.raw
  | Message_edit event -> event.raw
  | Message_delete event -> event.raw
  | Reaction event -> event.raw
  | Message_flags event -> event.raw
  | Realm_user event -> event.raw
  | Channel event -> event.raw
  | Subscription event -> event.raw
  | User_status event -> event.raw
  | Topic event -> event.raw
  | User_group event -> event.raw
  | Presence event -> event.raw
  | Unknown event -> event.raw

let ( let* ) = Result.bind

type object_view = { fields : Jsont.mem list; path : string list }

let object_members ?(path = []) = function
  | Jsont.Object (fields, _) -> Ok { fields; path }
  | _ -> problem ~path "event payload must be a JSON object"

let find name members =
  match Jsont.Json.find_mem name members.fields with
  | None -> None
  | Some (_, value) -> Some value

let decode_value ?(path = []) name codec json =
  match Jsont.Json.decode' codec json with
  | Ok value -> Ok value
  | Error cause ->
      problem ~path:(path @ [ name ]) ~cause (Jsont.Error.to_string cause)

let required name codec members =
  match find name members with
  | None ->
      problem ~path:(members.path @ [ name ]) "missing required event field"
  | Some json -> decode_value ~path:members.path name codec json

let optional name codec members =
  match find name members with
  | None -> Ok None
  | Some json ->
      Result.map Option.some (decode_value ~path:members.path name codec json)

let optional_json name members = Ok (find name members)

let list_or_empty name codec members =
  match find name members with
  | None -> Ok []
  | Some json -> decode_value ~path:members.path name (Jsont.list codec) json

let required_op allowed members =
  let* value = required "op" Jsont.string members in
  let op = op_of_string value in
  match op with
  | Other_op _ -> Ok op
  | _ when List.mem op allowed -> Ok op
  | _ -> problem ~path:[ "op" ] ("invalid operation for event family: " ^ value)

let required_change members =
  let* op = required_op [ Add; Remove ] members in
  match op with
  | Add -> Ok `Add
  | Remove -> Ok `Remove
  | Other_op value -> Ok (`Other value)
  | _ -> assert false

let decode_message raw members =
  let* message = required "message" Message.jsont members in
  let* flags = optional "flags" (Jsont.list Message_flag.jsont) members in
  let flags = Option.value ~default:(Message.flags message) flags in
  Ok (Message { message; flags; raw })

let decode_message_edit raw members =
  let* message_id = required "message_id" Id.Message.jsont members in
  let* message_ids = list_or_empty "message_ids" Id.Message.jsont members in
  let message_ids = if message_ids = [] then [ message_id ] else message_ids in
  let* user_id = optional "user_id" Id.User.jsont members in
  let* edit_timestamp = optional "edit_timestamp" Jsont.number members in
  let* content = optional "content" Jsont.string members in
  let* rendered_content = optional "rendered_content" Jsont.string members in
  let* channel_id = optional "stream_id" Id.Channel.jsont members in
  let* new_channel_id = optional "new_stream_id" Id.Channel.jsont members in
  let* topic = optional "subject" Jsont.string members in
  Ok
    (Message_edit
       {
         message_id;
         message_ids;
         user_id;
         edit_timestamp;
         content;
         rendered_content;
         channel_id;
         new_channel_id;
         topic;
         raw;
       })

let decode_message_delete raw members =
  let* many = list_or_empty "message_ids" Id.Message.jsont members in
  let* one = optional "message_id" Id.Message.jsont members in
  let message_ids =
    match (many, one) with [], Some id -> [ id ] | ids, _ -> ids
  in
  let* message_type = optional "message_type" Message_type.jsont members in
  if message_ids = [] then
    problem ~path:[ "message_ids" ]
      "delete_message event has neither message_id nor message_ids"
  else Ok (Message_delete { message_ids; message_type; raw })

let decode_reaction raw members =
  let* op = required_change members in
  let* message_id = required "message_id" Id.Message.jsont members in
  let* user_id = required "user_id" Id.User.jsont members in
  let* emoji_name = required "emoji_name" Jsont.string members in
  let* emoji_code = optional "emoji_code" Jsont.string members in
  let* reaction_type = optional "reaction_type" Jsont.string members in
  Ok
    (Reaction
       { op; message_id; user_id; emoji_name; emoji_code; reaction_type; raw })

let decode_message_flags raw members =
  let* op = required_change members in
  let* flag = required "flag" Message_flag.jsont members in
  let* message_ids =
    required "messages" (Jsont.list Id.Message.jsont) members
  in
  let* all = optional "all" Jsont.bool members in
  Ok (Message_flags { op; flag; message_ids; all; raw })

let decode_realm_user raw members =
  let* op = required_op [ Add; Remove; Update ] members in
  let* person = required "person" Jsont.json members in
  let* person_members = object_members ~path:[ "person" ] person in
  let* user_id = required "user_id" Id.User.jsont person_members in
  let* full_name = optional "full_name" Jsont.string person_members in
  let* email = optional "email" Jsont.string person_members in
  let* new_email = optional "new_email" Jsont.string person_members in
  let* is_bot = optional "is_bot" Jsont.bool person_members in
  let* added_user =
    match op with
    | Add -> Result.map Option.some (decode_value "person" User.jsont person)
    | _ -> Ok None
  in
  Ok
    (Realm_user
       {
         op;
         user_id;
         full_name;
         email;
         new_email;
         is_bot;
         added_user;
         person;
         raw;
       })

let raw_list name members =
  match find name members with
  | None -> Ok []
  | Some json -> decode_value name (Jsont.list Jsont.json) json

let ids_from_objects name objects =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest ->
        let* members = object_members json in
        let* id = required name Id.Channel.jsont members in
        loop (id :: acc) rest
  in
  loop [] objects

let decode_channels objects =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest ->
        let* channel = decode_value "streams" Channel.jsont json in
        loop (channel :: acc) rest
  in
  loop [] objects

let decode_channel raw members =
  let* op = required_op [ Create; Delete; Update ] members in
  let* stream_objects = raw_list "streams" members in
  let* object_ids = ids_from_objects "stream_id" stream_objects in
  let* explicit_ids = list_or_empty "stream_ids" Id.Channel.jsont members in
  let* channel_id = optional "stream_id" Id.Channel.jsont members in
  let listed_ids = if explicit_ids = [] then object_ids else explicit_ids in
  let channel_ids =
    listed_ids @ match channel_id with None -> [] | Some id -> [ id ]
  in
  let* channels =
    match op with Create -> decode_channels stream_objects | _ -> Ok []
  in
  let* property = optional "property" Jsont.string members in
  let* value = optional_json "value" members in
  Ok (Channel { op; channel_ids; channels; channel_id; property; value; raw })

let decode_subscriptions objects =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest ->
        let* subscription =
          decode_value "subscriptions" Channel.Subscription.jsont json
        in
        loop (subscription :: acc) rest
  in
  loop [] objects

let decode_subscription raw members =
  let* op =
    required_op [ Add; Remove; Update; Peer_add; Peer_remove ] members
  in
  let* subscription_objects = raw_list "subscriptions" members in
  let* object_ids = ids_from_objects "stream_id" subscription_objects in
  let* explicit_ids = list_or_empty "stream_ids" Id.Channel.jsont members in
  let* channel_id = optional "stream_id" Id.Channel.jsont members in
  let listed_ids = if explicit_ids = [] then object_ids else explicit_ids in
  let channel_ids =
    listed_ids @ match channel_id with None -> [] | Some id -> [ id ]
  in
  let* user_ids = list_or_empty "user_ids" Id.User.jsont members in
  let* subscriptions =
    match op with
    | Add -> decode_subscriptions subscription_objects
    | _ -> Ok []
  in
  let* property = optional "property" Jsont.string members in
  let* value = optional_json "value" members in
  Ok
    (Subscription
       { op; channel_ids; user_ids; subscriptions; property; value; raw })

let decode_user_status raw members =
  let* user_id = required "user_id" Id.User.jsont members in
  let* away = optional "away" Jsont.bool members in
  let* status_text = optional "status_text" Jsont.string members in
  let* emoji_name = optional "emoji_name" Jsont.string members in
  let* emoji_code = optional "emoji_code" Jsont.string members in
  let* reaction_type = optional "reaction_type" Jsont.string members in
  Ok
    (User_status
       {
         user_id;
         away;
         status_text;
         emoji_name;
         emoji_code;
         reaction_type;
         raw;
       })

let decode_topic raw members =
  let* channel_id = required "stream_id" Id.Channel.jsont members in
  let* topic_name = required "topic_name" Jsont.string members in
  let* last_updated = required "last_updated" Jsont.number members in
  let* visibility_policy =
    required "visibility_policy" Topic_visibility.jsont members
  in
  Ok (Topic { channel_id; topic_name; last_updated; visibility_policy; raw })

let decode_user_group raw members =
  let* op =
    required_op
      [
        Add;
        Remove;
        Update;
        Add_members;
        Remove_members;
        Add_subgroups;
        Remove_subgroups;
      ]
      members
  in
  let* group = optional_json "group" members in
  let* group_id =
    match group with
    | Some group ->
        let* group_members = object_members ~path:[ "group" ] group in
        required "id" Id.User_group.jsont group_members
    | None -> required "group_id" Id.User_group.jsont members
  in
  let* name =
    match group with
    | None -> Ok None
    | Some group ->
        let* group_members = object_members ~path:[ "group" ] group in
        optional "name" Jsont.string group_members
  in
  let* user_ids =
    match group with
    | Some group when op = Add ->
        let* group_members = object_members ~path:[ "group" ] group in
        list_or_empty "members" Id.User.jsont group_members
    | _ -> list_or_empty "user_ids" Id.User.jsont members
  in
  let* direct_subgroup_ids =
    match group with
    | Some group when op = Add ->
        let* group_members = object_members ~path:[ "group" ] group in
        list_or_empty "direct_subgroup_ids" Id.User_group.jsont group_members
    | _ -> list_or_empty "direct_subgroup_ids" Id.User_group.jsont members
  in
  let* data = optional_json "data" members in
  Ok
    (User_group { op; group_id; name; user_ids; direct_subgroup_ids; data; raw })

let timestamp_from_presence name = function
  | None -> Ok None
  | Some json ->
      let* members = object_members json in
      optional name Jsont.number members

let decode_presence raw members =
  let* user_id = optional "user_id" Id.User.jsont members in
  let* email = optional "email" Jsont.string members in
  let* server_timestamp = optional "server_timestamp" Jsont.number members in
  let* presence = optional_json "presence" members in
  let* presences = optional_json "presences" members in
  let* active_timestamp = timestamp_from_presence "active_timestamp" presence in
  let* idle_timestamp = timestamp_from_presence "idle_timestamp" presence in
  if Option.is_none user_id && Option.is_none presences then
    problem "presence event has neither user_id nor presences"
  else
    Ok
      (Presence
         {
           user_id;
           email;
           server_timestamp;
           active_timestamp;
           idle_timestamp;
           presence;
           presences;
           raw;
         })

let decode_payload event_type raw =
  match event_type with
  | Event_type.Message ->
      let* members = object_members raw in
      decode_message raw members
  | Event_type.Update_message ->
      let* members = object_members raw in
      decode_message_edit raw members
  | Event_type.Delete_message ->
      let* members = object_members raw in
      decode_message_delete raw members
  | Event_type.Reaction ->
      let* members = object_members raw in
      decode_reaction raw members
  | Event_type.Update_message_flags ->
      let* members = object_members raw in
      decode_message_flags raw members
  | Event_type.Realm_user ->
      let* members = object_members raw in
      decode_realm_user raw members
  | Event_type.Stream ->
      let* members = object_members raw in
      decode_channel raw members
  | Event_type.Subscription ->
      let* members = object_members raw in
      decode_subscription raw members
  | Event_type.User_status ->
      let* members = object_members raw in
      decode_user_status raw members
  | Event_type.User_topic ->
      let* members = object_members raw in
      decode_topic raw members
  | Event_type.User_group ->
      let* members = object_members raw in
      decode_user_group raw members
  | Event_type.Presence ->
      let* members = object_members raw in
      decode_presence raw members
  | _ -> Ok (Unknown { event_type; raw })

let decode event_type raw =
  Result.map_error
    (fun ({ path; message; cause } : problem) ->
      { event_type; path; message; cause })
    (decode_payload event_type raw)

let of_event event = decode (Event.type_ event) (Event.data event)
