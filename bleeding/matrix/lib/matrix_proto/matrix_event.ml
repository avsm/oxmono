open Matrix_id
module Timestamp = Matrix_event_core.Timestamp
module Rel_type = Matrix_event_core.Rel_type
module Relates_to = Matrix_event_core.Relates_to
module Image_info = Matrix_event_core.Image_info
module Membership = Matrix_event_state.Membership
module Join_rule = Matrix_event_state.Join_rule
module History_visibility = Matrix_event_state.History_visibility
module Guest_access = Matrix_event_state.Guest_access
module Room_create_content = Matrix_event_state.Room_create_content
module Room_name_content = Matrix_event_state.Room_name_content
module Room_topic_content = Matrix_event_state.Room_topic_content
module Room_avatar_content = Matrix_event_state.Room_avatar_content
module Room_member_content = Matrix_event_state.Room_member_content
module Room_join_rules_content = Matrix_event_state.Room_join_rules_content

module Room_history_visibility_content =
  Matrix_event_state.Room_history_visibility_content

module Room_canonical_alias_content =
  Matrix_event_state.Room_canonical_alias_content

module Room_power_levels_content = Matrix_event_state.Room_power_levels_content
module Room_retention_content = Matrix_event_state.Room_retention_content
module Room_member_hints_content = Matrix_event_state.Room_member_hints_content

module Io_element_functional_members_content =
  Matrix_event_state.Io_element_functional_members_content

module Room_encryption_content = Matrix_event_state.Room_encryption_content

module Room_pinned_events_content =
  Matrix_event_state.Room_pinned_events_content

module Room_server_acl_content = Matrix_event_state.Room_server_acl_content
module Room_tombstone_content = Matrix_event_state.Room_tombstone_content
module Room_guest_access_content = Matrix_event_state.Room_guest_access_content
module Space_child_content = Matrix_event_space.Space_child_content
module Space_parent_content = Matrix_event_space.Space_parent_content
module Sdp = Matrix_event_call.Sdp
module Hangup_reason = Matrix_event_call.Hangup_reason
module Call_invite_content = Matrix_event_call.Call_invite_content
module Call_answer_content = Matrix_event_call.Call_answer_content
module Call_hangup_content = Matrix_event_call.Call_hangup_content
module Call_candidates_content = Matrix_event_call.Call_candidates_content
module Call_member_content = Matrix_event_call.Call_member_content

module Key_verification_request_content =
  Matrix_event_verification.Key_verification_request_content

module Key_verification_request_message_content =
  Matrix_event_verification.Key_verification_request_message_content

module Key_verification_ready_content =
  Matrix_event_verification.Key_verification_ready_content

module Key_verification_start_content =
  Matrix_event_verification.Key_verification_start_content

module Key_verification_accept_content =
  Matrix_event_verification.Key_verification_accept_content

module Key_verification_key_content =
  Matrix_event_verification.Key_verification_key_content

module Key_verification_mac_content =
  Matrix_event_verification.Key_verification_mac_content

module Key_verification_cancel_content =
  Matrix_event_verification.Key_verification_cancel_content

module Key_verification_done_content =
  Matrix_event_verification.Key_verification_done_content

module Msgtype = Matrix_event_message.Msgtype
module Media_info = Matrix_event_message.Media_info
module Text_message_content = Matrix_event_message.Text_message_content
module Media_message_content = Matrix_event_message.Media_message_content
module Sticker_content = Matrix_event_message.Sticker_content
module Location_message_content = Matrix_event_message.Location_message_content
module Recommendation = Matrix_event_extensible.Recommendation
module Policy_rule_content = Matrix_event_extensible.Policy_rule_content
module Marked_unread_content = Matrix_event_extensible.Marked_unread_content
module Encryption_algorithm = Matrix_event_extensible.Encryption_algorithm
module Encrypted_content = Matrix_event_extensible.Encrypted_content
module Olm_message_type = Matrix_event_encrypted.Olm_message_type
module Olm_ciphertext = Matrix_event_encrypted.Olm_ciphertext
module Olm_plaintext = Matrix_event_encrypted.Olm_plaintext
module Encrypted = Matrix_event_encrypted.Encrypted
module Room_key_content = Matrix_event_encrypted.Room_key_content

module Forwarded_room_key_content =
  Matrix_event_encrypted.Forwarded_room_key_content

module Reaction_content = Matrix_event_extensible.Reaction_content
module Beacon_info_content = Matrix_event_extensible.Beacon_info_content
module Beacon_content = Matrix_event_extensible.Beacon_content
module Poll_start_content = Matrix_event_extensible.Poll_start_content
module Poll_response_content = Matrix_event_extensible.Poll_response_content
module Poll_end_content = Matrix_event_extensible.Poll_end_content

module Unsigned = struct
  type t = {
    age : int64 option;
    prev_content : Jsont.json option;
    prev_sender : User_id.t option;
    redacted_because : Jsont.json option;
    transaction_id : Transaction_id.t option;
    membership : string option;
    relations : Jsont.json option;
  }

  let make ?age ?prev_content ?prev_sender ?redacted_because ?transaction_id
      ?membership ?relations () =
    {
      age;
      prev_content;
      prev_sender;
      redacted_because;
      transaction_id;
      membership;
      relations;
    }

  let empty = make ()
  let age t = t.age
  let prev_content t = t.prev_content
  let prev_sender t = t.prev_sender
  let redacted_because t = t.redacted_because
  let transaction_id t = t.transaction_id
  let membership t = t.membership
  let relations t = t.relations
  let without_relations t = { t with relations = None }

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.age with
    | Some a -> Format.fprintf ppf "age: %Ld@," a
    | None -> ());
    (match t.transaction_id with
    | Some tid ->
        Format.fprintf ppf "transaction_id: %s@," (Transaction_id.to_string tid)
    | None -> ());
    (match t.membership with
    | Some membership -> Format.fprintf ppf "membership: %s@," membership
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont_with_age age_jsont =
    Jsont.Object.(
      map
        (fun
          age
          prev_content
          prev_sender
          redacted_because
          transaction_id
          membership
          relations
          unstable_membership
        ->
          let membership =
            match membership with
            | Some _ -> membership
            | None -> unstable_membership
          in
          {
            age;
            prev_content;
            prev_sender;
            redacted_because;
            transaction_id;
            membership;
            relations;
          })
      |> opt_mem "age" age_jsont ~enc:(fun t -> t.age)
      |> opt_mem "prev_content" Matrix_json.Codec.json ~enc:(fun t ->
          t.prev_content)
      |> opt_mem "prev_sender" User_id.jsont ~enc:(fun t -> t.prev_sender)
      |> opt_mem "redacted_because" Matrix_json.Codec.json ~enc:(fun t ->
          t.redacted_because)
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> opt_mem "membership" Matrix_json.Codec.string ~enc:(fun t ->
          t.membership)
      |> opt_mem "m.relations" Matrix_json.Codec.json ~enc:(fun t ->
          t.relations)
      |> opt_mem "io.element.msc4115.membership" Matrix_json.Codec.string
           ~enc:(fun _ -> None)
      |> finish)

  let jsont = jsont_with_age Matrix_json.Codec.int64
  let persisted_jsont = jsont_with_age Matrix_json.Codec.Legacy.int64
end

module Event_type = struct
  type t =
    | Room_create
    | Room_name
    | Room_topic
    | Room_avatar
    | Room_member
    | Room_join_rules
    | Room_history_visibility
    | Room_canonical_alias
    | Room_power_levels
    | Room_retention
    | Room_retention_unstable
    | Room_member_hints
    | Io_element_functional_members
    | Room_encryption
    | Room_pinned_events
    | Room_server_acl
    | Room_tombstone
    | Room_guest_access
    | Space_child
    | Space_parent
    | Room_message
    | Room_message_encrypted
    | Room_redaction
    | Reaction
    | Sticker
    | Call_invite
    | Call_candidates
    | Call_answer
    | Call_hangup
    | Call_reject
    | Call_select_answer
    | Call_negotiate
    | Call_member
    | Key_verification_request
    | Key_verification_ready
    | Key_verification_start
    | Key_verification_accept
    | Key_verification_key
    | Key_verification_mac
    | Key_verification_cancel
    | Key_verification_done
    | Policy_rule_room
    | Policy_rule_server
    | Policy_rule_user
    | Room_key
    | Room_key_request
    | Forwarded_room_key
    | Dummy
    | Typing
    | Receipt
    | Presence
    | Direct
    | Ignored_user_list
    | Fully_read
    | Marked_unread
    | Tag
    | Push_rules
    | Secret_storage_default_key
    | Secret_storage_key
    | Cross_signing_keys
    | Beacon_info
    | Beacon
    | Poll_start
    | Poll_response
    | Poll_end
    | Custom of string

  let to_string = function
    | Room_create -> "m.room.create"
    | Room_name -> "m.room.name"
    | Room_topic -> "m.room.topic"
    | Room_avatar -> "m.room.avatar"
    | Room_member -> "m.room.member"
    | Room_join_rules -> "m.room.join_rules"
    | Room_history_visibility -> "m.room.history_visibility"
    | Room_canonical_alias -> "m.room.canonical_alias"
    | Room_power_levels -> "m.room.power_levels"
    | Room_retention -> "m.room.retention"
    | Room_retention_unstable -> "org.matrix.msc1763.retention"
    | Room_member_hints -> "m.room.member_hints"
    | Io_element_functional_members -> "io.element.functional_members"
    | Room_encryption -> "m.room.encryption"
    | Room_pinned_events -> "m.room.pinned_events"
    | Room_server_acl -> "m.room.server_acl"
    | Room_tombstone -> "m.room.tombstone"
    | Room_guest_access -> "m.room.guest_access"
    | Space_child -> "m.space.child"
    | Space_parent -> "m.space.parent"
    | Room_message -> "m.room.message"
    | Room_message_encrypted -> "m.room.encrypted"
    | Room_redaction -> "m.room.redaction"
    | Reaction -> "m.reaction"
    | Sticker -> "m.sticker"
    | Call_invite -> "m.call.invite"
    | Call_candidates -> "m.call.candidates"
    | Call_answer -> "m.call.answer"
    | Call_hangup -> "m.call.hangup"
    | Call_reject -> "m.call.reject"
    | Call_select_answer -> "m.call.select_answer"
    | Call_negotiate -> "m.call.negotiate"
    | Call_member -> "m.call.member"
    | Key_verification_request -> "m.key.verification.request"
    | Key_verification_ready -> "m.key.verification.ready"
    | Key_verification_start -> "m.key.verification.start"
    | Key_verification_accept -> "m.key.verification.accept"
    | Key_verification_key -> "m.key.verification.key"
    | Key_verification_mac -> "m.key.verification.mac"
    | Key_verification_cancel -> "m.key.verification.cancel"
    | Key_verification_done -> "m.key.verification.done"
    | Policy_rule_room -> "m.policy.rule.room"
    | Policy_rule_server -> "m.policy.rule.server"
    | Policy_rule_user -> "m.policy.rule.user"
    | Room_key -> "m.room_key"
    | Room_key_request -> "m.room_key_request"
    | Forwarded_room_key -> "m.forwarded_room_key"
    | Dummy -> "m.dummy"
    | Typing -> "m.typing"
    | Receipt -> "m.receipt"
    | Presence -> "m.presence"
    | Direct -> "m.direct"
    | Ignored_user_list -> "m.ignored_user_list"
    | Fully_read -> "m.fully_read"
    | Marked_unread -> "m.marked_unread"
    | Tag -> "m.tag"
    | Push_rules -> "m.push_rules"
    | Secret_storage_default_key -> "m.secret_storage.default_key"
    | Secret_storage_key -> "m.secret_storage.key"
    | Cross_signing_keys -> "m.cross_signing.keys"
    | Beacon_info -> "org.matrix.msc3672.beacon_info"
    | Beacon -> "org.matrix.msc3672.beacon"
    | Poll_start -> "m.poll.start"
    | Poll_response -> "m.poll.response"
    | Poll_end -> "m.poll.end"
    | Custom s -> s

  let of_string = function
    | "m.room.create" -> Room_create
    | "m.room.name" -> Room_name
    | "m.room.topic" -> Room_topic
    | "m.room.avatar" -> Room_avatar
    | "m.room.member" -> Room_member
    | "m.room.join_rules" -> Room_join_rules
    | "m.room.history_visibility" -> Room_history_visibility
    | "m.room.canonical_alias" -> Room_canonical_alias
    | "m.room.power_levels" -> Room_power_levels
    | "m.room.retention" -> Room_retention
    | "org.matrix.msc1763.retention" -> Room_retention_unstable
    | "m.room.member_hints" -> Room_member_hints
    (* Some SDK versions used the MSC4171 name without the room namespace. *)
    | "m.member_hints" -> Room_member_hints
    | "io.element.functional_members" -> Io_element_functional_members
    | "m.room.encryption" -> Room_encryption
    | "m.room.pinned_events" -> Room_pinned_events
    | "m.room.server_acl" -> Room_server_acl
    | "m.room.tombstone" -> Room_tombstone
    | "m.room.guest_access" -> Room_guest_access
    | "m.space.child" -> Space_child
    | "m.space.parent" -> Space_parent
    | "m.room.message" -> Room_message
    | "m.room.encrypted" -> Room_message_encrypted
    | "m.room.redaction" -> Room_redaction
    | "m.reaction" -> Reaction
    | "m.sticker" -> Sticker
    | "m.call.invite" -> Call_invite
    | "m.call.candidates" -> Call_candidates
    | "m.call.answer" -> Call_answer
    | "m.call.hangup" -> Call_hangup
    | "m.call.reject" -> Call_reject
    | "m.call.select_answer" -> Call_select_answer
    | "m.call.negotiate" -> Call_negotiate
    | "m.call.member" -> Call_member
    | "m.key.verification.request" -> Key_verification_request
    | "m.key.verification.ready" -> Key_verification_ready
    | "m.key.verification.start" -> Key_verification_start
    | "m.key.verification.accept" -> Key_verification_accept
    | "m.key.verification.key" -> Key_verification_key
    | "m.key.verification.mac" -> Key_verification_mac
    | "m.key.verification.cancel" -> Key_verification_cancel
    | "m.key.verification.done" -> Key_verification_done
    | "m.policy.rule.room" -> Policy_rule_room
    | "m.policy.rule.server" -> Policy_rule_server
    | "m.policy.rule.user" -> Policy_rule_user
    | "m.room_key" -> Room_key
    | "m.room_key_request" -> Room_key_request
    | "m.forwarded_room_key" -> Forwarded_room_key
    | "m.dummy" -> Dummy
    | "m.typing" -> Typing
    | "m.receipt" -> Receipt
    | "m.presence" -> Presence
    | "m.direct" -> Direct
    | "m.ignored_user_list" -> Ignored_user_list
    | "m.fully_read" -> Fully_read
    | "m.marked_unread" -> Marked_unread
    | "m.tag" -> Tag
    | "m.push_rules" -> Push_rules
    | "m.secret_storage.default_key" -> Secret_storage_default_key
    | "m.secret_storage.key" -> Secret_storage_key
    | "m.cross_signing.keys" -> Cross_signing_keys
    | "org.matrix.msc3672.beacon_info" -> Beacon_info
    | "org.matrix.msc3672.beacon" -> Beacon
    | "m.poll.start" -> Poll_start
    | "m.poll.response" -> Poll_response
    | "m.poll.end" -> Poll_end
    | s -> Custom s

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.of_of_string ~kind:"event_type" ~enc:to_string (fun s ->
        Ok (of_string s))
end

module Typed_event = struct
  type 'content t = {
    event_id : Event_id.t;
    sender : User_id.t;
    origin_server_ts : Timestamp.t;
    state_key : string option;
    type_ : Event_type.t;
    content : 'content;
    unsigned : Unsigned.t option;
  }

  let jsont content_jsont =
    Jsont.Object.(
      map
        (fun
          event_id sender origin_server_ts state_key type_ content unsigned ->
          {
            event_id;
            sender;
            origin_server_ts;
            state_key;
            type_;
            content;
            unsigned;
          })
      |> mem "event_id" Event_id.jsont ~enc:(fun t -> t.event_id)
      |> mem "sender" User_id.jsont ~enc:(fun t -> t.sender)
      |> mem "origin_server_ts" Timestamp.jsont ~enc:(fun t ->
          t.origin_server_ts)
      |> opt_mem "state_key" Matrix_json.Codec.string ~enc:(fun t ->
          t.state_key)
      |> mem "type" Event_type.jsont ~enc:(fun t -> t.type_)
      |> mem "content" content_jsont ~enc:(fun t -> t.content)
      |> opt_mem "unsigned" Unsigned.jsont ~enc:(fun t -> t.unsigned)
      |> finish)
end

module Raw_event = struct
  type t = {
    event_id : Event_id.t option;
    sender : User_id.t;
    origin_server_ts : Timestamp.t;
    type_ : Event_type.t;
    state_key : string option;
    redacts : Event_id.t option;
    content : Jsont.json;
    unsigned : Unsigned.t option;
    room_id : Room_id.t option;
  }

  let jsont_with ~timestamp ~unsigned =
    Jsont.Object.(
      map
        (fun
          event_id
          sender
          origin_server_ts
          type_
          state_key
          redacts
          content
          unsigned
          room_id
        ->
          {
            event_id;
            sender;
            origin_server_ts;
            type_;
            state_key;
            redacts;
            content;
            unsigned;
            room_id;
          })
      |> opt_mem "event_id" Event_id.jsont ~enc:(fun t -> t.event_id)
      |> mem "sender" User_id.jsont ~enc:(fun t -> t.sender)
      |> mem "origin_server_ts" timestamp ~enc:(fun t -> t.origin_server_ts)
      |> mem "type" Event_type.jsont ~enc:(fun t -> t.type_)
      |> opt_mem "state_key" Matrix_json.Codec.string ~enc:(fun t ->
          t.state_key)
      |> opt_mem "redacts" Event_id.jsont ~enc:(fun t -> t.redacts)
      |> mem "content" Matrix_json.Codec.json ~enc:(fun t -> t.content)
      |> opt_mem "unsigned" unsigned ~enc:(fun t -> t.unsigned)
      |> opt_mem "room_id" Room_id.jsont ~enc:(fun t -> t.room_id)
      |> finish)

  let jsont = jsont_with ~timestamp:Timestamp.jsont ~unsigned:Unsigned.jsont

  let persisted_jsont =
    let timestamp =
      Jsont.map ~dec:Timestamp.of_ms ~enc:Timestamp.to_ms
        Matrix_json.Codec.Legacy.int64
    in
    jsont_with ~timestamp ~unsigned:Unsigned.persisted_jsont
end

module Stripped_event = struct
  type t = {
    sender : User_id.t;
    type_ : Event_type.t;
    state_key : string;
    content : Jsont.json;
  }

  let jsont =
    Jsont.Object.(
      map (fun sender type_ state_key content ->
          { sender; type_; state_key; content })
      |> mem "sender" User_id.jsont ~enc:(fun t -> t.sender)
      |> mem "type" Event_type.jsont ~enc:(fun t -> t.type_)
      |> mem "state_key" Matrix_json.Codec.string ~enc:(fun t -> t.state_key)
      |> mem "content" Matrix_json.Codec.json ~enc:(fun t -> t.content)
      |> finish)
end
