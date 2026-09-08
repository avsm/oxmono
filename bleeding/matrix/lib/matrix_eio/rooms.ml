type preset = Matrix_client.Rooms.preset =
  | Private_chat
  | Public_chat
  | Trusted_private_chat

let create client ?name ?topic ?visibility ?preset ?room_alias_local_part
    ?invite ?is_direct ?room_type ?encrypted () =
  Error.unwrap ~context:"creating room"
    (Matrix_client.Rooms.create (Client.base client) ?name ?topic ?visibility
       ?preset ?room_alias_local_part ?invite ?is_direct ?room_type ?encrypted
       ())

let join client ~room_id_or_alias ?via ?reason () =
  Error.unwrap ~context:"joining room"
    (Matrix_client.Rooms.join (Client.base client) ~room_id_or_alias ?via
       ?reason ())

let knock client ~room_id_or_alias ?reason ?via () =
  Error.unwrap ~context:"knocking room"
    (Matrix_client.Rooms.knock (Client.base client) ~room_id_or_alias ?reason
       ?via ())

let leave client ~room_id ?reason () =
  Error.unwrap ~context:"leaving room"
    (Matrix_client.Rooms.leave (Client.base client) ~room_id ?reason ())

let forget client ~room_id =
  Error.unwrap ~context:"forgetting room"
    (Matrix_client.Rooms.forget (Client.base client) ~room_id)

let invite client ~room_id ~user_id ?reason () =
  Error.unwrap ~context:"inviting user to room"
    (Matrix_client.Rooms.invite (Client.base client) ~room_id ~user_id ?reason
       ())

let kick client ~room_id ~user_id ?reason () =
  Error.unwrap ~context:"kicking user from room"
    (Matrix_client.Rooms.kick (Client.base client) ~room_id ~user_id ?reason ())

let ban client ~room_id ~user_id ?reason () =
  Error.unwrap ~context:"banning user from room"
    (Matrix_client.Rooms.ban (Client.base client) ~room_id ~user_id ?reason ())

let unban client ~room_id ~user_id ?reason () =
  Error.unwrap ~context:"unbanning user from room"
    (Matrix_client.Rooms.unban (Client.base client) ~room_id ~user_id ?reason ())

let get_joined_rooms client =
  Error.unwrap ~context:"getting joined rooms"
    (Matrix_client.Rooms.get_joined_rooms (Client.base client))

type member = Matrix_client.Rooms.member = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
  membership : Matrix_proto.Event.Membership.t;
}

let get_members client ~room_id ?membership ?not_membership () =
  Error.unwrap ~context:"getting room members"
    (Matrix_client.Rooms.get_members (Client.base client) ~room_id ?membership
       ?not_membership ())

type joined_member = Matrix_client.Rooms.joined_member = {
  display_name : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
}

let get_joined_members client ~room_id =
  Error.unwrap ~context:"getting joined room members"
    (Matrix_client.Rooms.get_joined_members (Client.base client) ~room_id)

let get_power_levels client ~room_id =
  Error.unwrap ~context:"getting room power levels"
    (Matrix_client.Rooms.get_power_levels (Client.base client) ~room_id)

let set_power_levels client ~room_id ~power_levels =
  Error.unwrap ~context:"setting room power levels"
    (Matrix_client.Rooms.set_power_levels (Client.base client) ~room_id
       ~power_levels)

let set_user_power_level client ~room_id ~user_id ~level =
  Error.unwrap ~context:"setting user power level"
    (Matrix_client.Rooms.set_user_power_level (Client.base client) ~room_id
       ~user_id ~level)

let start_live_location_share client ~room_id ~duration_millis ?description
    ?timestamp () =
  Error.unwrap ~context:"starting live location share"
    (Matrix_client.Rooms.start_live_location_share (Client.base client) ~room_id
       ~duration_millis ?description ?timestamp ())

let stop_live_location_share client ~room_id () =
  Error.unwrap ~context:"stopping live location share"
    (Matrix_client.Rooms.stop_live_location_share (Client.base client) ~room_id
       ())

let send_location_beacon client ~room_id ~geo_uri ?timestamp ?description ?now
    () =
  Error.unwrap ~context:"sending location beacon"
    (Matrix_client.Rooms.send_location_beacon (Client.base client) ~room_id
       ~geo_uri ?timestamp ?description ?now ())

let upgrade client ~room_id ~new_version ?additional_creators () =
  Error.unwrap ~context:"upgrading room"
    (Matrix_client.Rooms.upgrade (Client.base client) ~room_id ~new_version
       ?additional_creators ())

let get_aliases client ~room_id =
  Error.unwrap ~context:"getting room aliases"
    (Matrix_client.Rooms.get_aliases (Client.base client) ~room_id)

type timestamp_event = Matrix_client.Rooms.timestamp_event = {
  event_id : Matrix_proto.Id.Event_id.t;
  origin_server_ts : Matrix_proto.Event.Timestamp.t;
}

let timestamp_to_event client ~room_id ~ts ~dir =
  Error.unwrap ~context:"getting event at room timestamp"
    (Matrix_client.Rooms.timestamp_to_event (Client.base client) ~room_id ~ts
       ~dir)
