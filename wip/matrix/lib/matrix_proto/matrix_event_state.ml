open Matrix_id
module Image_info = Matrix_event_core.Image_info

module Membership = struct
  type t = Join | Invite | Leave | Ban | Knock

  let to_string = function
    | Join -> "join"
    | Invite -> "invite"
    | Leave -> "leave"
    | Ban -> "ban"
    | Knock -> "knock"

  let of_string = function
    | "join" -> Ok Join
    | "invite" -> Ok Invite
    | "leave" -> Ok Leave
    | "ban" -> Ok Ban
    | "knock" -> Ok Knock
    | s -> Error (`Msg (Printf.sprintf "unknown membership %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum
      [
        ("join", Join);
        ("invite", Invite);
        ("leave", Leave);
        ("ban", Ban);
        ("knock", Knock);
      ]
end

module Join_rule = struct
  type t = Public | Invite | Knock | Restricted | Knock_restricted | Private

  let to_string = function
    | Public -> "public"
    | Invite -> "invite"
    | Knock -> "knock"
    | Restricted -> "restricted"
    | Knock_restricted -> "knock_restricted"
    | Private -> "private"

  let of_string = function
    | "public" -> Ok Public
    | "invite" -> Ok Invite
    | "knock" -> Ok Knock
    | "restricted" -> Ok Restricted
    | "knock_restricted" -> Ok Knock_restricted
    | "private" -> Ok Private
    | s -> Error (`Msg (Printf.sprintf "unknown join rule %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum
      [
        ("public", Public);
        ("invite", Invite);
        ("knock", Knock);
        ("restricted", Restricted);
        ("knock_restricted", Knock_restricted);
        ("private", Private);
      ]
end

module History_visibility = struct
  type t = Invited | Joined | Shared | World_readable

  let to_string = function
    | Invited -> "invited"
    | Joined -> "joined"
    | Shared -> "shared"
    | World_readable -> "world_readable"

  let of_string = function
    | "invited" -> Ok Invited
    | "joined" -> Ok Joined
    | "shared" -> Ok Shared
    | "world_readable" -> Ok World_readable
    | s -> Error (`Msg (Printf.sprintf "unknown history visibility %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum
      [
        ("invited", Invited);
        ("joined", Joined);
        ("shared", Shared);
        ("world_readable", World_readable);
      ]
end

module Guest_access = struct
  type t = Can_join | Forbidden

  let to_string = function Can_join -> "can_join" | Forbidden -> "forbidden"

  let of_string = function
    | "can_join" -> Ok Can_join
    | "forbidden" -> Ok Forbidden
    | s -> Error (`Msg (Printf.sprintf "unknown guest access %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let jsont = Jsont.enum [ ("can_join", Can_join); ("forbidden", Forbidden) ]
end

module Room_create_content = struct
  module Predecessor = struct
    type t = { room_id : Room_id.t; event_id : Event_id.t }

    let make ~room_id ~event_id = { room_id; event_id }
    let room_id t = t.room_id
    let event_id t = t.event_id

    let pp ppf t =
      Format.fprintf ppf "@[<hov 2>predecessor:@ room_id=%a@ event_id=%a@]"
        Room_id.pp t.room_id Event_id.pp t.event_id

    let jsont =
      Jsont.Object.(
        map (fun room_id event_id -> { room_id; event_id })
        |> mem "room_id" Room_id.jsont ~enc:(fun p -> p.room_id)
        |> mem "event_id" Event_id.jsont ~enc:(fun p -> p.event_id)
        |> finish)
  end

  type t = {
    creator : User_id.t option;
    room_version : string option;
    predecessor : Predecessor.t option;
    type_ : string option;
  }

  let make ?creator ?room_version ?predecessor ?type_ () =
    { creator; room_version; predecessor; type_ }

  let creator t = t.creator
  let room_version t = t.room_version
  let predecessor t = t.predecessor
  let room_type t = t.type_

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.creator with
    | Some u -> Format.fprintf ppf "creator: %a@," User_id.pp u
    | None -> ());
    (match t.room_version with
    | Some v -> Format.fprintf ppf "room_version: %s@," v
    | None -> ());
    (match t.predecessor with
    | Some p -> Format.fprintf ppf "%a@," Predecessor.pp p
    | None -> ());
    (match t.type_ with
    | Some ty -> Format.fprintf ppf "type: %s@," ty
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun creator room_version predecessor type_ ->
          { creator; room_version; predecessor; type_ })
      |> opt_mem "creator" User_id.jsont ~enc:(fun t -> t.creator)
      |> opt_mem "room_version" Matrix_json.Codec.string ~enc:(fun t ->
          t.room_version)
      |> opt_mem "predecessor" Predecessor.jsont ~enc:(fun t -> t.predecessor)
      |> opt_mem "type" Matrix_json.Codec.string ~enc:(fun t -> t.type_)
      |> finish)
end

module Room_name_content = struct
  type t = { name : string }

  let make ~name = { name }
  let name t = t.name
  let pp ppf t = Format.fprintf ppf "name: %s" t.name

  let jsont =
    Jsont.Object.(
      map (fun name -> { name })
      |> mem "name" Matrix_json.Codec.string ~enc:(fun t -> t.name)
      |> finish)
end

module Room_topic_content = struct
  type t = { topic : string }

  let make ~topic = { topic }
  let topic t = t.topic
  let pp ppf t = Format.fprintf ppf "topic: %s" t.topic

  let jsont =
    Jsont.Object.(
      map (fun topic -> { topic })
      |> mem "topic" Matrix_json.Codec.string ~enc:(fun t -> t.topic)
      |> finish)
end

module Room_avatar_content = struct
  type t = { url : string option; info : Image_info.t option }

  let make ?url ?info () = { url; info }
  let url t = t.url
  let info t = t.info

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.url with Some u -> Format.fprintf ppf "url: %s@," u | None -> ());
    (match t.info with
    | Some i -> Format.fprintf ppf "%a@," Image_info.pp i
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun url info -> { url; info })
      |> opt_mem "url" Matrix_json.Codec.string ~enc:(fun t -> t.url)
      |> opt_mem "info" Image_info.jsont ~enc:(fun t -> t.info)
      |> finish)
end

module Room_member_content = struct
  type t = {
    membership : Membership.t;
    displayname : string option;
    avatar_url : string option;
    is_direct : bool option;
    reason : string option;
  }

  let make ~membership ?displayname ?avatar_url ?is_direct ?reason () =
    { membership; displayname; avatar_url; is_direct; reason }

  let membership t = t.membership
  let displayname t = t.displayname
  let avatar_url t = t.avatar_url
  let is_direct t = t.is_direct
  let reason t = t.reason

  let pp ppf t =
    Format.fprintf ppf "@[<v>membership: %a" Membership.pp t.membership;
    (match t.displayname with
    | Some n -> Format.fprintf ppf "@,displayname: %s" n
    | None -> ());
    (match t.avatar_url with
    | Some u -> Format.fprintf ppf "@,avatar_url: %s" u
    | None -> ());
    (match t.is_direct with
    | Some d -> Format.fprintf ppf "@,is_direct: %b" d
    | None -> ());
    (match t.reason with
    | Some r -> Format.fprintf ppf "@,reason: %s" r
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun membership displayname avatar_url is_direct reason ->
          { membership; displayname; avatar_url; is_direct; reason })
      |> mem "membership" Membership.jsont ~enc:(fun t -> t.membership)
      |> opt_mem "displayname" Matrix_json.Codec.string ~enc:(fun t ->
          t.displayname)
      |> opt_mem "avatar_url" Matrix_json.Codec.string ~enc:(fun t ->
          t.avatar_url)
      |> opt_mem "is_direct" Jsont.bool ~enc:(fun t -> t.is_direct)
      |> opt_mem "reason" Matrix_json.Codec.string ~enc:(fun t -> t.reason)
      |> finish)
end

module Room_join_rules_content = struct
  module Allow_condition = struct
    type t = { type_ : string; room_id : Room_id.t option }

    let make ~type_ ?room_id () = { type_; room_id }
    let condition_type t = t.type_
    let room_id t = t.room_id

    let pp ppf t =
      Format.fprintf ppf "@[<hov 2>allow:@ type=%s" t.type_;
      (match t.room_id with
      | Some r -> Format.fprintf ppf "@ room_id=%a" Room_id.pp r
      | None -> ());
      Format.fprintf ppf "@]"

    let jsont =
      Jsont.Object.(
        map (fun type_ room_id -> { type_; room_id })
        |> mem "type" Matrix_json.Codec.string ~enc:(fun c -> c.type_)
        |> opt_mem "room_id" Room_id.jsont ~enc:(fun c -> c.room_id)
        |> finish)
  end

  type t = { join_rule : Join_rule.t; allow : Allow_condition.t list option }

  let make ~join_rule ?allow () = { join_rule; allow }
  let join_rule t = t.join_rule
  let allow t = t.allow

  let pp ppf t =
    Format.fprintf ppf "@[<v>join_rule: %a" Join_rule.pp t.join_rule;
    (match t.allow with
    | Some conditions ->
        Format.fprintf ppf "@,allow: [";
        List.iter
          (fun c -> Format.fprintf ppf "@,%a" Allow_condition.pp c)
          conditions;
        Format.fprintf ppf "]"
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun join_rule allow -> { join_rule; allow })
      |> mem "join_rule" Join_rule.jsont ~enc:(fun t -> t.join_rule)
      |> opt_mem "allow" (Jsont.list Allow_condition.jsont) ~enc:(fun t ->
          t.allow)
      |> finish)
end

module Room_history_visibility_content = struct
  type t = { history_visibility : History_visibility.t }

  let make ~history_visibility = { history_visibility }
  let history_visibility t = t.history_visibility

  let pp ppf t =
    Format.fprintf ppf "history_visibility: %a" History_visibility.pp
      t.history_visibility

  let jsont =
    Jsont.Object.(
      map (fun history_visibility -> { history_visibility })
      |> mem "history_visibility" History_visibility.jsont ~enc:(fun t ->
          t.history_visibility)
      |> finish)
end

module Room_canonical_alias_content = struct
  type t = {
    alias : Room_alias.t option;
    alt_aliases : Room_alias.t list option;
  }

  let make ?alias ?alt_aliases () = { alias; alt_aliases }
  let alias t = t.alias
  let alt_aliases t = t.alt_aliases

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.alias with
    | Some a -> Format.fprintf ppf "alias: %a@," Room_alias.pp a
    | None -> ());
    (match t.alt_aliases with
    | Some alts when alts <> [] ->
        Format.fprintf ppf "alt_aliases: [%a]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             Room_alias.pp)
          alts
    | _ -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun alias alt_aliases -> { alias; alt_aliases })
      |> opt_mem "alias" Room_alias.jsont ~enc:(fun t -> t.alias)
      |> opt_mem "alt_aliases" (Jsont.list Room_alias.jsont) ~enc:(fun t ->
          t.alt_aliases)
      |> finish)
end

module Room_power_levels_content = struct
  type t = {
    ban : int option;
    events : (string * int) list option;
    events_default : int option;
    invite : int option;
    kick : int option;
    redact : int option;
    state_default : int option;
    users : (string * int) list option;
    users_default : int option;
    notifications : (string * int) list option;
  }

  let make ?ban ?events ?events_default ?invite ?kick ?redact ?state_default
      ?users ?users_default ?notifications () =
    {
      ban;
      events;
      events_default;
      invite;
      kick;
      redact;
      state_default;
      users;
      users_default;
      notifications;
    }

  let ban t = t.ban
  let events t = t.events
  let events_default t = t.events_default
  let invite t = t.invite
  let kick t = t.kick
  let redact t = t.redact
  let state_default t = t.state_default
  let users t = t.users
  let users_default t = t.users_default
  let notifications t = t.notifications

  let user_level t user =
    let key = User_id.to_string user in
    match Option.bind t.users (List.assoc_opt key) with
    | Some level -> level
    | None -> Option.value t.users_default ~default:0

  let with_user_level t user level =
    let key = User_id.to_string user in
    let users = Option.value t.users ~default:[] in
    { t with users = Some ((key, level) :: List.remove_assoc key users) }

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.ban with Some v -> Format.fprintf ppf "ban: %d@," v | None -> ());
    (match t.kick with
    | Some v -> Format.fprintf ppf "kick: %d@," v
    | None -> ());
    (match t.invite with
    | Some v -> Format.fprintf ppf "invite: %d@," v
    | None -> ());
    (match t.redact with
    | Some v -> Format.fprintf ppf "redact: %d@," v
    | None -> ());
    (match t.events_default with
    | Some v -> Format.fprintf ppf "events_default: %d@," v
    | None -> ());
    (match t.state_default with
    | Some v -> Format.fprintf ppf "state_default: %d@," v
    | None -> ());
    (match t.users_default with
    | Some v -> Format.fprintf ppf "users_default: %d@," v
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map
        (fun
          ban
          events
          events_default
          invite
          kick
          redact
          state_default
          users
          users_default
          notifications
        ->
          {
            ban;
            events;
            events_default;
            invite;
            kick;
            redact;
            state_default;
            users;
            users_default;
            notifications;
          })
      |> opt_mem "ban" Matrix_json.Codec.int ~enc:(fun t -> t.ban)
      |> opt_mem "events" (Matrix_string_map.jsont Matrix_json.Codec.int)
           ~enc:(fun t -> t.events)
      |> opt_mem "events_default" Matrix_json.Codec.int ~enc:(fun t ->
          t.events_default)
      |> opt_mem "invite" Matrix_json.Codec.int ~enc:(fun t -> t.invite)
      |> opt_mem "kick" Matrix_json.Codec.int ~enc:(fun t -> t.kick)
      |> opt_mem "redact" Matrix_json.Codec.int ~enc:(fun t -> t.redact)
      |> opt_mem "state_default" Matrix_json.Codec.int ~enc:(fun t ->
          t.state_default)
      |> opt_mem "users" (Matrix_string_map.jsont Matrix_json.Codec.int)
           ~enc:(fun t -> t.users)
      |> opt_mem "users_default" Matrix_json.Codec.int ~enc:(fun t ->
          t.users_default)
      |> opt_mem "notifications" (Matrix_string_map.jsont Matrix_json.Codec.int)
           ~enc:(fun t -> t.notifications)
      |> finish)
end

module Room_retention_content = struct
  type t = { min_lifetime : int64 option; max_lifetime : int64 option }

  let make ?min_lifetime ?max_lifetime () = { min_lifetime; max_lifetime }
  let min_lifetime t = t.min_lifetime
  let max_lifetime t = t.max_lifetime

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.min_lifetime with
    | Some ms -> Format.fprintf ppf "min_lifetime: %Ld@," ms
    | None -> ());
    (match t.max_lifetime with
    | Some ms -> Format.fprintf ppf "max_lifetime: %Ld@," ms
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun min_lifetime max_lifetime -> { min_lifetime; max_lifetime })
      |> opt_mem "min_lifetime" Matrix_json.Codec.int64 ~enc:(fun t ->
          t.min_lifetime)
      |> opt_mem "max_lifetime" Matrix_json.Codec.int64 ~enc:(fun t ->
          t.max_lifetime)
      |> finish)
end

module Room_member_hints_content = struct
  type t = { service_members : User_id.t list }

  let make ?(service_members = []) () = { service_members }
  let service_members t = t.service_members

  let pp ppf t =
    Format.fprintf ppf "service_members: [%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         User_id.pp)
      t.service_members

  let jsont =
    Jsont.Object.(
      map (fun service_members -> { service_members })
      |> mem "service_members" (Jsont.list User_id.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.service_members)
      |> finish)
end

module Io_element_functional_members_content = struct
  type t = { service_members : User_id.t list }

  let make ?(service_members = []) () = { service_members }
  let service_members t = t.service_members
  let functional_members t = t.service_members

  let pp ppf t =
    Format.fprintf ppf "service_members: [%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         User_id.pp)
      t.service_members

  let jsont =
    Jsont.Object.(
      map (fun service_members -> { service_members })
      |> mem "service_members" (Jsont.list User_id.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.service_members)
      |> finish)
end

module Room_encryption_content = struct
  type t = {
    algorithm : string;
    rotation_period_ms : int64 option;
    rotation_period_msgs : int option;
  }

  let make ~algorithm ?rotation_period_ms ?rotation_period_msgs () =
    { algorithm; rotation_period_ms; rotation_period_msgs }

  let algorithm t = t.algorithm
  let rotation_period_ms t = t.rotation_period_ms
  let rotation_period_msgs t = t.rotation_period_msgs

  let pp ppf t =
    Format.fprintf ppf "@[<v>algorithm: %s" t.algorithm;
    (match t.rotation_period_ms with
    | Some ms -> Format.fprintf ppf "@,rotation_period_ms: %Ld" ms
    | None -> ());
    (match t.rotation_period_msgs with
    | Some n -> Format.fprintf ppf "@,rotation_period_msgs: %d" n
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun algorithm rotation_period_ms rotation_period_msgs ->
          { algorithm; rotation_period_ms; rotation_period_msgs })
      |> mem "algorithm" Matrix_json.Codec.string ~enc:(fun t -> t.algorithm)
      |> opt_mem "rotation_period_ms" Matrix_json.Codec.int64 ~enc:(fun t ->
          t.rotation_period_ms)
      |> opt_mem "rotation_period_msgs" Matrix_json.Codec.int ~enc:(fun t ->
          t.rotation_period_msgs)
      |> finish)
end

module Room_pinned_events_content = struct
  type t = { pinned : string list }

  let make ?(pinned = []) () = { pinned }
  let pinned t = t.pinned
  let pp ppf t = Format.fprintf ppf "pinned: [%s]" (String.concat ", " t.pinned)

  let jsont =
    Jsont.Object.(
      map (fun pinned -> { pinned })
      |> mem "pinned"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.pinned)
      |> finish)
end

module Room_server_acl_content = struct
  type t = { allow : string list; allow_ip_literals : bool; deny : string list }

  let make ?(allow = []) ?(allow_ip_literals = true) ?(deny = []) () =
    { allow; allow_ip_literals; deny }

  let allow t = t.allow
  let allow_ip_literals t = t.allow_ip_literals
  let deny t = t.deny

  let pp ppf t =
    Format.fprintf ppf "@[<v>allow: [%s]@,allow_ip_literals: %b@,deny: [%s]@]"
      (String.concat ", " t.allow)
      t.allow_ip_literals
      (String.concat ", " t.deny)

  let jsont =
    Jsont.Object.(
      map (fun allow allow_ip_literals deny ->
          { allow; allow_ip_literals; deny })
      |> mem "allow"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.allow)
      |> mem "allow_ip_literals" Jsont.bool
           ~dec_absent:(fun () -> true)
           ~enc:(fun t -> t.allow_ip_literals)
      |> mem "deny"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.deny)
      |> finish)
end

module Room_tombstone_content = struct
  type t = { body : string; replacement_room : Room_id.t }

  let make ~body ~replacement_room = { body; replacement_room }
  let body t = t.body
  let replacement_room t = t.replacement_room

  let pp ppf t =
    Format.fprintf ppf "@[<v>body: %s@,replacement_room: %a@]" t.body Room_id.pp
      t.replacement_room

  let jsont =
    Jsont.Object.(
      map (fun body replacement_room -> { body; replacement_room })
      |> mem "body" Matrix_json.Codec.string ~enc:(fun t -> t.body)
      |> mem "replacement_room" Room_id.jsont ~enc:(fun t -> t.replacement_room)
      |> finish)
end

module Room_guest_access_content = struct
  type t = { guest_access : Guest_access.t }

  let make ~guest_access = { guest_access }
  let guest_access t = t.guest_access

  let pp ppf t =
    Format.fprintf ppf "guest_access: %a" Guest_access.pp t.guest_access

  let jsont =
    Jsont.Object.(
      map (fun guest_access -> { guest_access })
      |> mem "guest_access" Guest_access.jsont ~enc:(fun t -> t.guest_access)
      |> finish)
end
