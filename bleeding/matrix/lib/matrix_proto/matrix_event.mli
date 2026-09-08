@@ portable

(** Matrix events, their envelopes and the content of the standard event types.

    One module holds the content object of each event type the specification
    defines. Some are abstract, with a [make] whose optional arguments are the
    members the specification allows to be absent, accessors named after the
    members, and [pp]. Others expose the record their codec reads. Every one of
    them provides [jsont].

    Decoding is strict about the members the specification requires and ignores
    any it does not name, so an event carrying extra members round-trips through
    neither more nor less than what is declared here. A signature over the
    original bytes will not verify against the re-encoded form.

    @see <https://spec.matrix.org/v1.11/client-server-api/#events> Events *)

(** {1 Shared pieces} *)

module Timestamp = Matrix_event_core.Timestamp
(** @canonical Matrix_proto.Event.Timestamp *)

module Rel_type = Matrix_event_core.Rel_type
(** @canonical Matrix_proto.Event.Rel_type *)

module Relates_to = Matrix_event_core.Relates_to
(** @canonical Matrix_proto.Event.Relates_to *)

module Image_info = Matrix_event_core.Image_info
(** @canonical Matrix_proto.Event.Image_info *)

module Unsigned : sig
  (** The [unsigned] object a homeserver attaches to an event.

      It sits outside the signed content, so a receiving server may add to it or
      drop it. Nothing here should be trusted for anything but display.

      @see <https://spec.matrix.org/v1.11/client-server-api/#definition-clientevent>
        ClientEvent *)

  type t
  (** The type for unsigned data. *)

  val make :
    ?age:int64 ->
    ?prev_content:Jsont.json ->
    ?prev_sender:Matrix_id.User_id.t ->
    ?redacted_because:Jsont.json ->
    ?transaction_id:Matrix_id.Transaction_id.t ->
    ?membership:string ->
    ?relations:Jsont.json ->
    unit ->
    t
  (** [make ()] is unsigned data. Every argument defaults to absent. *)

  val empty : t
  (** [empty] has no member set. *)

  val age : t -> int64 option
  (** [age t] is how many milliseconds ago the event was sent, as the local
      server measured it when it answered. *)

  val prev_content : t -> Jsont.json option
  (** [prev_content t] is, for a state event, the content this one replaced. *)

  val prev_sender : t -> Matrix_id.User_id.t option
  (** [prev_sender t] is, for a state event, who sent the content this one
      replaced. *)

  val redacted_because : t -> Jsont.json option
  (** [redacted_because t] is the [m.room.redaction] event that emptied this
      one's content. It is present exactly when the event has been redacted. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the identifier this client sent the event with. It
      is present only on the client's own events, and is how a local echo is
      matched to the event that comes back from [/sync]. *)

  val membership : t -> string option
  (** [membership t] is the MSC4115 membership at the event's position in the
      room. Both the stable [membership] member and the historical
      [io.element.msc4115.membership] spelling are accepted; encoding uses the
      stable spelling. The value is deliberately retained as a string so that an
      extension value cannot make the enclosing timeline event fail to decode.
  *)

  val relations : t -> Jsont.json option
  (** [relations t] is the raw [unsigned.m.relations] object, when present. It
      is retained so extensions such as bundled thread summaries can be decoded
      without making unknown unsigned fields fatal. *)

  val without_relations : t -> t
  (** [without_relations t] drops bundled relation data before an event is
      persisted. Bundled data is server-derived and may become stale. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the age and the transaction id. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

(** {1 Room state} *)

module Membership = Matrix_event_state.Membership
(** @canonical Matrix_proto.Event.Membership *)

module Join_rule = Matrix_event_state.Join_rule
(** @canonical Matrix_proto.Event.Join_rule *)

module History_visibility = Matrix_event_state.History_visibility
(** @canonical Matrix_proto.Event.History_visibility *)

module Guest_access = Matrix_event_state.Guest_access
(** @canonical Matrix_proto.Event.Guest_access *)

module Room_create_content = Matrix_event_state.Room_create_content
(** @canonical Matrix_proto.Event.Room_create_content *)

module Room_name_content = Matrix_event_state.Room_name_content
(** @canonical Matrix_proto.Event.Room_name_content *)

module Room_topic_content = Matrix_event_state.Room_topic_content
(** @canonical Matrix_proto.Event.Room_topic_content *)

module Room_avatar_content = Matrix_event_state.Room_avatar_content
(** @canonical Matrix_proto.Event.Room_avatar_content *)

module Room_member_content = Matrix_event_state.Room_member_content
(** @canonical Matrix_proto.Event.Room_member_content *)

module Room_join_rules_content = Matrix_event_state.Room_join_rules_content
(** @canonical Matrix_proto.Event.Room_join_rules_content *)

module Room_history_visibility_content =
  Matrix_event_state.Room_history_visibility_content
(** @canonical Matrix_proto.Event.Room_history_visibility_content *)

module Room_canonical_alias_content =
  Matrix_event_state.Room_canonical_alias_content
(** @canonical Matrix_proto.Event.Room_canonical_alias_content *)

module Room_power_levels_content = Matrix_event_state.Room_power_levels_content
(** @canonical Matrix_proto.Event.Room_power_levels_content *)

module Room_retention_content = Matrix_event_state.Room_retention_content
(** @canonical Matrix_proto.Event.Room_retention_content *)

module Room_member_hints_content = Matrix_event_state.Room_member_hints_content
(** @canonical Matrix_proto.Event.Room_member_hints_content *)

module Io_element_functional_members_content =
  Matrix_event_state.Io_element_functional_members_content
(** @canonical Matrix_proto.Event.Io_element_functional_members_content *)

module Room_encryption_content = Matrix_event_state.Room_encryption_content
(** @canonical Matrix_proto.Event.Room_encryption_content *)

module Room_pinned_events_content =
  Matrix_event_state.Room_pinned_events_content
(** @canonical Matrix_proto.Event.Room_pinned_events_content *)

module Room_server_acl_content = Matrix_event_state.Room_server_acl_content
(** @canonical Matrix_proto.Event.Room_server_acl_content *)

module Room_tombstone_content = Matrix_event_state.Room_tombstone_content
(** @canonical Matrix_proto.Event.Room_tombstone_content *)

module Room_guest_access_content = Matrix_event_state.Room_guest_access_content
(** @canonical Matrix_proto.Event.Room_guest_access_content *)

(** {1 Spaces} *)

module Space_child_content = Matrix_event_space.Space_child_content
(** @canonical Matrix_proto.Event.Space_child_content *)

module Space_parent_content = Matrix_event_space.Space_parent_content
(** @canonical Matrix_proto.Event.Space_parent_content *)

(** {1 Calls} *)

module Sdp = Matrix_event_call.Sdp
(** @canonical Matrix_proto.Event.Sdp *)

module Hangup_reason = Matrix_event_call.Hangup_reason
(** @canonical Matrix_proto.Event.Hangup_reason *)

module Call_invite_content = Matrix_event_call.Call_invite_content
(** @canonical Matrix_proto.Event.Call_invite_content *)

module Call_answer_content = Matrix_event_call.Call_answer_content
(** @canonical Matrix_proto.Event.Call_answer_content *)

module Call_hangup_content = Matrix_event_call.Call_hangup_content
(** @canonical Matrix_proto.Event.Call_hangup_content *)

module Call_candidates_content = Matrix_event_call.Call_candidates_content
(** @canonical Matrix_proto.Event.Call_candidates_content *)

module Call_member_content = Matrix_event_call.Call_member_content
(** @canonical Matrix_proto.Event.Call_member_content *)

(** {1 Key verification} *)

module Key_verification_request_content =
  Matrix_event_verification.Key_verification_request_content
(** @canonical Matrix_proto.Event.Key_verification_request_content *)

module Key_verification_request_message_content =
  Matrix_event_verification.Key_verification_request_message_content
(** @canonical Matrix_proto.Event.Key_verification_request_message_content *)

module Key_verification_ready_content =
  Matrix_event_verification.Key_verification_ready_content
(** @canonical Matrix_proto.Event.Key_verification_ready_content *)

module Key_verification_start_content =
  Matrix_event_verification.Key_verification_start_content
(** @canonical Matrix_proto.Event.Key_verification_start_content *)

module Key_verification_accept_content =
  Matrix_event_verification.Key_verification_accept_content
(** @canonical Matrix_proto.Event.Key_verification_accept_content *)

module Key_verification_key_content =
  Matrix_event_verification.Key_verification_key_content
(** @canonical Matrix_proto.Event.Key_verification_key_content *)

module Key_verification_mac_content =
  Matrix_event_verification.Key_verification_mac_content
(** @canonical Matrix_proto.Event.Key_verification_mac_content *)

module Key_verification_cancel_content =
  Matrix_event_verification.Key_verification_cancel_content
(** @canonical Matrix_proto.Event.Key_verification_cancel_content *)

module Key_verification_done_content =
  Matrix_event_verification.Key_verification_done_content
(** @canonical Matrix_proto.Event.Key_verification_done_content *)

(** {1 Messages} *)

module Msgtype = Matrix_event_message.Msgtype
(** @canonical Matrix_proto.Event.Msgtype *)

module Media_info = Matrix_event_message.Media_info
(** @canonical Matrix_proto.Event.Media_info *)

module Text_message_content = Matrix_event_message.Text_message_content
(** @canonical Matrix_proto.Event.Text_message_content *)

module Media_message_content = Matrix_event_message.Media_message_content
(** @canonical Matrix_proto.Event.Media_message_content *)

module Sticker_content = Matrix_event_message.Sticker_content
(** @canonical Matrix_proto.Event.Sticker_content *)

module Location_message_content = Matrix_event_message.Location_message_content
(** @canonical Matrix_proto.Event.Location_message_content *)

(** {1 Moderation, encryption, reactions and polls} *)

module Recommendation = Matrix_event_extensible.Recommendation
(** @canonical Matrix_proto.Event.Recommendation *)

module Policy_rule_content = Matrix_event_extensible.Policy_rule_content
(** @canonical Matrix_proto.Event.Policy_rule_content *)

module Marked_unread_content = Matrix_event_extensible.Marked_unread_content
(** @canonical Matrix_proto.Event.Marked_unread_content *)

module Encryption_algorithm = Matrix_event_extensible.Encryption_algorithm
(** @canonical Matrix_proto.Event.Encryption_algorithm *)

module Encrypted_content = Matrix_event_extensible.Encrypted_content
(** @canonical Matrix_proto.Event.Encrypted_content *)

module Olm_message_type = Matrix_event_encrypted.Olm_message_type
(** @canonical Matrix_proto.Event.Olm_message_type *)

module Olm_ciphertext = Matrix_event_encrypted.Olm_ciphertext
(** @canonical Matrix_proto.Event.Olm_ciphertext *)

module Olm_plaintext = Matrix_event_encrypted.Olm_plaintext
(** @canonical Matrix_proto.Event.Olm_plaintext *)

module Encrypted = Matrix_event_encrypted.Encrypted
(** @canonical Matrix_proto.Event.Encrypted *)

module Room_key_content = Matrix_event_encrypted.Room_key_content
(** @canonical Matrix_proto.Event.Room_key_content *)

module Forwarded_room_key_content =
  Matrix_event_encrypted.Forwarded_room_key_content
(** @canonical Matrix_proto.Event.Forwarded_room_key_content *)

module Reaction_content = Matrix_event_extensible.Reaction_content
(** @canonical Matrix_proto.Event.Reaction_content *)

module Beacon_info_content = Matrix_event_extensible.Beacon_info_content
(** @canonical Matrix_proto.Event.Beacon_info_content *)

module Beacon_content = Matrix_event_extensible.Beacon_content
(** @canonical Matrix_proto.Event.Beacon_content *)

module Poll_start_content = Matrix_event_extensible.Poll_start_content
(** @canonical Matrix_proto.Event.Poll_start_content *)

module Poll_response_content = Matrix_event_extensible.Poll_response_content
(** @canonical Matrix_proto.Event.Poll_response_content *)

module Poll_end_content = Matrix_event_extensible.Poll_end_content
(** @canonical Matrix_proto.Event.Poll_end_content *)

(** {1 Event envelopes} *)

module Event_type : sig
  (** The [type] of an event, with anything unrecognised in {!Custom}.

      @see <https://spec.matrix.org/v1.11/client-server-api/#events> Events *)

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
    | Custom of string  (** An event type this type does not name. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> t
  (** [of_string s] is the event type [s] names, or [Custom s]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same event type. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Typed_event : sig
  (** An event whose content has been decoded, parameterised by the content type
      of one of the modules above. *)

  type 'content t = {
    event_id : Matrix_id.Event_id.t;
    sender : Matrix_id.User_id.t;
    origin_server_ts : Timestamp.t;
    state_key : string option;
        (** With {!field-type_}, the key this event is filed under in the room's
            state. It is absent for a message event, and often empty, a user id
            or a room id for a state event. *)
    type_ : Event_type.t;
    content : 'content;
    unsigned : Unsigned.t option;
  }

  val jsont : 'content Jsont.t -> 'content t Jsont.t
  (** [jsont content] is the envelope codec that reads [content] for the event's
      [content] member. *)
end

module Raw_event : sig
  (** An event of any type, with its content left as JSON.

      This is what [/sync] and the message endpoints decode to, since a room's
      timeline mixes types and one unrecognised event must not fail the batch.
      Read {!field-content} with the codec of the matching module once
      {!field-type_} says which it is.

      A stripped state event, of the kind an invite carries, has neither an
      [origin_server_ts] nor an [event_id] and so does not decode to this type.
  *)

  type t = {
    event_id : Matrix_id.Event_id.t option;  (** Absent on a local echo. *)
    sender : Matrix_id.User_id.t;
    origin_server_ts : Timestamp.t;
    type_ : Event_type.t;
    state_key : string option;  (** Present exactly for a state event. *)
    redacts : Matrix_id.Event_id.t option;
        (** The target of a room-version 1–10 [m.room.redaction]. Room version
            11 and later put this in [content.redacts] instead. *)
    content : Jsont.json;
    unsigned : Unsigned.t option;
    room_id : Matrix_id.Room_id.t option;
        (** Absent where the room is implied, as in a [/sync] response. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)

  val persisted_jsont : t Jsont.t
  (** [persisted_jsont] is {!jsont} with migration decoding for the event
      timestamp and [unsigned.age]. It is only for existing on-disk snapshots;
      Matrix responses must use {!jsont}. *)
end

module Stripped_event : sig
  (** Current state carried for a room the user has been invited to or knocked
      on. It deliberately lacks the event identifier and timestamp of a full
      room event. *)

  type t = {
    sender : Matrix_id.User_id.t;
    type_ : Event_type.t;
    state_key : string;
    content : Jsont.json;
  }

  val jsont : t Jsont.t
  (** [jsont] is the stripped-state event envelope. Its four members are
      required, and malformed user identifiers fail decoding. *)
end
