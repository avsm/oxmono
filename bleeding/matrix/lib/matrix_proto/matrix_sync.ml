open Matrix_id
open Matrix_event

let js_safe_uint_jsont = Matrix_json.Codec.uint

module Raw_events = struct
  type t = { events : Jsont.json list }

  let pp ppf t = Format.fprintf ppf "%d events" (List.length t.events)

  let jsont =
    Jsont.Object.(
      map (fun events -> { events })
      |> mem "events"
           (Jsont.list Matrix_json.Codec.json)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.events)
      |> finish)
end

module Stripped_events = struct
  type t = { events : Stripped_event.t list }

  let pp ppf t = Format.fprintf ppf "%d events" (List.length t.events)

  let jsont =
    Jsont.Object.(
      map (fun events -> { events })
      |> mem "events"
           (Jsont.list Stripped_event.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.events)
      |> finish)
end

module Timeline = struct
  type t = {
    events : Raw_event.t list;
    limited : bool option;
    prev_batch : string option;
  }

  let pp ppf t =
    Format.fprintf ppf "@[%d events%s@]" (List.length t.events)
      (match t.limited with Some true -> ", limited" | _ -> "")

  let jsont =
    Jsont.Object.(
      map (fun events limited prev_batch -> { events; limited; prev_batch })
      |> mem "events"
           (Jsont.list Raw_event.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.events)
      |> opt_mem "limited" Jsont.bool ~enc:(fun t -> t.limited)
      |> opt_mem "prev_batch" Matrix_json.Codec.string ~enc:(fun t ->
          t.prev_batch)
      |> finish)
end

module Ephemeral = Raw_events
module Account_data = Raw_events

module Room_state = struct
  type t = { events : Raw_event.t list }

  let jsont =
    Jsont.Object.(
      map (fun events -> { events })
      |> mem "events"
           (Jsont.list Raw_event.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.events)
      |> finish)
end

module Unread_notification_counts = struct
  type t = { highlight_count : int option; notification_count : int option }

  let jsont =
    Jsont.Object.(
      map (fun highlight_count notification_count ->
          { highlight_count; notification_count })
      |> opt_mem "highlight_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.highlight_count)
      |> opt_mem "notification_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.notification_count)
      |> finish)
end

module Room_summary = struct
  type t = {
    heroes : User_id.t list option;
    joined_member_count : int option;
    invited_member_count : int option;
  }

  let jsont =
    Jsont.Object.(
      map (fun heroes joined_member_count invited_member_count ->
          { heroes; joined_member_count; invited_member_count })
      |> opt_mem "m.heroes" (Jsont.list User_id.jsont) ~enc:(fun t -> t.heroes)
      |> opt_mem "m.joined_member_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.joined_member_count)
      |> opt_mem "m.invited_member_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.invited_member_count)
      |> finish)
end

module Joined_room = struct
  type t = {
    summary : Room_summary.t option;
    state : Room_state.t option;
    timeline : Timeline.t option;
    ephemeral : Ephemeral.t option;
    account_data : Account_data.t option;
    unread_notifications : Unread_notification_counts.t option;
  }

  let pp ppf t =
    Format.fprintf ppf "@[timeline %a, state %d events@]"
      (Format.pp_print_option
         ~none:(fun ppf () -> Format.pp_print_string ppf "absent")
         Timeline.pp)
      t.timeline
      (match t.state with
      | Some s -> List.length s.Room_state.events
      | None -> 0)

  let jsont =
    Jsont.Object.(
      map
        (fun
          summary state timeline ephemeral account_data unread_notifications ->
          {
            summary;
            state;
            timeline;
            ephemeral;
            account_data;
            unread_notifications;
          })
      |> opt_mem "summary" Room_summary.jsont ~enc:(fun t -> t.summary)
      |> opt_mem "state" Room_state.jsont ~enc:(fun t -> t.state)
      |> opt_mem "timeline" Timeline.jsont ~enc:(fun t -> t.timeline)
      |> opt_mem "ephemeral" Ephemeral.jsont ~enc:(fun t -> t.ephemeral)
      |> opt_mem "account_data" Account_data.jsont ~enc:(fun t ->
          t.account_data)
      |> opt_mem "unread_notifications" Unread_notification_counts.jsont
           ~enc:(fun t -> t.unread_notifications)
      |> finish)
end

module Invited_room = struct
  type t = { invite_state : Stripped_events.t option }

  let jsont =
    Jsont.Object.(
      map (fun invite_state -> { invite_state })
      |> opt_mem "invite_state" Stripped_events.jsont ~enc:(fun t ->
          t.invite_state)
      |> finish)
end

module Left_room = struct
  type t = {
    state : Room_state.t option;
    timeline : Timeline.t option;
    account_data : Account_data.t option;
  }

  let jsont =
    Jsont.Object.(
      map (fun state timeline account_data -> { state; timeline; account_data })
      |> opt_mem "state" Room_state.jsont ~enc:(fun t -> t.state)
      |> opt_mem "timeline" Timeline.jsont ~enc:(fun t -> t.timeline)
      |> opt_mem "account_data" Account_data.jsont ~enc:(fun t ->
          t.account_data)
      |> finish)
end

module Knocked_room = struct
  type t = { knock_state : Stripped_events.t option }

  let jsont =
    Jsont.Object.(
      map (fun knock_state -> { knock_state })
      |> opt_mem "knock_state" Stripped_events.jsont ~enc:(fun t ->
          t.knock_state)
      |> finish)
end

module Rooms = struct
  type t = {
    join : (string * Joined_room.t) list;
    invite : (string * Invited_room.t) list;
    leave : (string * Left_room.t) list;
    knock : (string * Knocked_room.t) list;
  }

  let jsont =
    Jsont.Object.(
      map (fun join invite leave knock -> { join; invite; leave; knock })
      |> mem "join"
           (Matrix_string_map.jsont Joined_room.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.join)
      |> mem "invite"
           (Matrix_string_map.jsont Invited_room.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.invite)
      |> mem "leave"
           (Matrix_string_map.jsont Left_room.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.leave)
      |> mem "knock"
           (Matrix_string_map.jsont Knocked_room.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.knock)
      |> finish)
end

module Device_lists = struct
  type t = { changed : User_id.t list; left : User_id.t list }

  let jsont =
    Jsont.Object.(
      map (fun changed left -> { changed; left })
      |> mem "changed" (Jsont.list User_id.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.changed)
      |> mem "left" (Jsont.list User_id.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.left)
      |> finish)
end

module To_device = Raw_events
module Presence = Raw_events

module Response = struct
  type t = {
    next_batch : string;
    rooms : Rooms.t option;
    presence : Presence.t option;
    account_data : Account_data.t option;
    to_device : To_device.t option;
    device_lists : Device_lists.t option;
    device_one_time_keys_count : (string * int) list;
    device_unused_fallback_key_types : string list option;
  }

  let pp ppf t =
    let count f =
      match t.rooms with Some r -> List.length (f r) | None -> 0
    in
    Format.fprintf ppf "@[next_batch=%s join=%d invite=%d leave=%d knock=%d@]"
      t.next_batch
      (count (fun r -> r.Rooms.join))
      (count (fun r -> r.Rooms.invite))
      (count (fun r -> r.Rooms.leave))
      (count (fun r -> r.Rooms.knock))

  let jsont =
    Jsont.Object.(
      map
        (fun
          next_batch
          rooms
          presence
          account_data
          to_device
          device_lists
          device_one_time_keys_count
          device_unused_fallback_key_types
        ->
          {
            next_batch;
            rooms;
            presence;
            account_data;
            to_device;
            device_lists;
            device_one_time_keys_count;
            device_unused_fallback_key_types;
          })
      |> mem "next_batch" Matrix_json.Codec.string ~enc:(fun t -> t.next_batch)
      |> opt_mem "rooms" Rooms.jsont ~enc:(fun t -> t.rooms)
      |> opt_mem "presence" Presence.jsont ~enc:(fun t -> t.presence)
      |> opt_mem "account_data" Account_data.jsont ~enc:(fun t ->
          t.account_data)
      |> opt_mem "to_device" To_device.jsont ~enc:(fun t -> t.to_device)
      |> opt_mem "device_lists" Device_lists.jsont ~enc:(fun t ->
          t.device_lists)
      |> mem "device_one_time_keys_count"
           (Matrix_string_map.jsont js_safe_uint_jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.device_one_time_keys_count)
      |> opt_mem "device_unused_fallback_key_types"
           (Jsont.list Matrix_json.Codec.string) ~enc:(fun t ->
             t.device_unused_fallback_key_types)
      |> finish)
end
