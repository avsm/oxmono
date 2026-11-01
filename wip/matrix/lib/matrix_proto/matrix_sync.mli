@@ portable

(** The [/sync] response.

    One {!Response.t} is everything the server has for the client since the
    token it was given. It holds the new events per room, the ephemeral and
    account-data payloads, and the encryption bookkeeping. Members the server
    omits decode to [None], or to the empty list where the member is a
    collection, so a sparse response needs no special handling.

    @see <https://spec.matrix.org/v1.11/client-server-api/#get_matrixclientv3sync>
      GET /sync *)

(** {1 Event lists} *)

module Raw_events : sig
  (** A list of events left as raw JSON.

      The ephemeral, account-data, to-device and presence payloads all take this
      shape. Their content differs per event type and nothing in their common
      envelope is useful to this layer. *)

  type t = { events : Jsont.json list }

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints how many events the list holds. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. An absent [events] member decodes to
      the empty list. *)
end

module Stripped_events : sig
  (** A list of typed stripped-state event envelopes. *)

  type t = { events : Matrix_event.Stripped_event.t list }

  val pp : Format.formatter -> t -> unit
  val jsont : t Jsont.t
end

module Timeline : sig
  (** A room's new message-like and state events, oldest first. *)

  type t = {
    events : Matrix_event.Raw_event.t list;
    limited : bool option;
        (** The server truncated the batch, so events between the previous sync
            and {!field-events} are missing. Fill the gap by paginating from
            {!field-prev_batch}. *)
    prev_batch : string option;
        (** Token for [GET /rooms/{roomId}/messages] with [dir=b], naming the
            point just before {!field-events}. *)
  }

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the event count and whether the batch was limited. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_state : sig
  (** State events that changed outside the timeline, since the previous sync.
  *)

  type t = { events : Matrix_event.Raw_event.t list }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. An absent [events] member decodes to
      the empty list. *)
end

module Ephemeral = Raw_events
(** A room's typing notifications and read receipts, which the server does not
    persist.

    @canonical Matrix_proto.Sync.Raw_events *)

module Account_data = Raw_events
(** Per-user configuration, global or scoped to one room. It carries tags, the
    direct message map and the push rules.

    @canonical Matrix_proto.Sync.Raw_events *)

module To_device = Raw_events
(** Messages addressed to this device rather than to a room, which is how Olm
    key exchange and device verification travel. Each is delivered once and is
    gone from the next sync.

    @canonical Matrix_proto.Sync.Raw_events *)

module Presence = Raw_events
(** [m.presence] events for users this client shares a room with.

    @canonical Matrix_proto.Sync.Raw_events *)

(** {1 Rooms} *)

module Unread_notification_counts : sig
  (** What the server counts as unread for a room.

      It cannot be right for an encrypted room, where the server cannot read the
      content the push rules test. A client that needs those counts evaluates
      the rules itself. *)

  type t = { highlight_count : int option; notification_count : int option }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_summary : sig
  (** The material for naming a room that has no [m.room.name]. It is a few
      members the server nominated, and how many there are in total.

      @see <https://spec.matrix.org/v1.11/client-server-api/#_matrixclientv3sync_roomsummary>
        RoomSummary *)

  type t = {
    heroes : Matrix_id.User_id.t list option;
        (** The [m.heroes] member, a few members to name the room after. *)
    joined_member_count : int option;
        (** The [m.joined_member_count] member, sent only when it changed. *)
    invited_member_count : int option;
        (** The [m.invited_member_count] member, sent only when it changed. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails if any hero is not a
      well-formed user id. *)
end

module Joined_room : sig
  (** A room the user is in. *)

  type t = {
    summary : Room_summary.t option;  (** Material for naming the room. *)
    state : Room_state.t option;  (** State changed outside the timeline. *)
    timeline : Timeline.t option;  (** New events in the room. *)
    ephemeral : Ephemeral.t option;  (** Typing notifications and receipts. *)
    account_data : Account_data.t option;  (** Configuration for this room. *)
    unread_notifications : Unread_notification_counts.t option;
        (** The server's unread counts. *)
  }

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints which members are present and the timeline size. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Invited_room : sig
  (** A room the user has been invited to but has not joined. *)

  type t = {
    invite_state : Stripped_events.t option;
        (** Stripped state events, enough to show who is inviting and to what.
            They have no [event_id] and no [origin_server_ts], so they are not
            {!Matrix_event.Raw_event.t} values. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Left_room : sig
  (** A room the user has left, or was kicked or banned from. The events are
      those up to the moment membership ended. *)

  type t = {
    state : Room_state.t option;  (** State changed outside the timeline. *)
    timeline : Timeline.t option;  (** Events up to the end of membership. *)
    account_data : Account_data.t option;  (** Configuration for this room. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Knocked_room : sig
  (** A room the user has knocked on and not yet been admitted to. *)

  type t = {
    knock_state : Stripped_events.t option;
        (** Stripped state events, as in {!Invited_room.field-invite_state}. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Rooms : sig
  (** Rooms grouped by the user's membership, each list keyed by room id.

      The keys are the ids as the server spelled them, unparsed, so that one
      unrecognised room cannot fail the whole response. Bindings are sorted by
      key. *)

  type t = {
    join : (string * Joined_room.t) list;
    invite : (string * Invited_room.t) list;
    leave : (string * Left_room.t) list;
    knock : (string * Knocked_room.t) list;
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. An absent group decodes to the empty
      list. *)
end

(** {1 Response} *)

module Device_lists : sig
  (** Whose device list changed since the last sync, so that a client with
      encryption enabled knows whose keys to re-fetch. *)

  type t = {
    changed : Matrix_id.User_id.t list;
        (** Users who added, removed or altered a device. *)
    left : Matrix_id.User_id.t list;
        (** Users who no longer share an encrypted room, and whose devices may
            be forgotten. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails if any entry is not a
      well-formed user id. *)
end

module Response : sig
  (** One [/sync] response. *)

  type t = {
    next_batch : string;  (** Pass as [since] to continue from here. *)
    rooms : Rooms.t option;
    presence : Presence.t option;
    account_data : Account_data.t option;  (** Global, not per room. *)
    to_device : To_device.t option;
    device_lists : Device_lists.t option;
    device_one_time_keys_count : (string * int) list;
        (** One-time keys the server still holds, by algorithm. Upload more when
            the count falls below half the device's maximum. *)
    device_unused_fallback_key_types : string list option;
        (** Algorithms whose fallback key has not been used. An algorithm absent
            from the list needs a fresh fallback key. *)
  }

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the batch token and how many rooms are in each group. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end
