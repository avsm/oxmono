@@ portable

(** The simplified sliding sync wire shapes.

    Sliding sync replaces the [/sync] "give me everything since [since]" with
    "give me the rooms in these windows, at this level of detail". The server
    keeps a session, identified by [conn_id] and advanced by [pos], so a client
    with thousands of rooms pays only for the ones on screen.

    The shapes here are MSC4186, simplified sliding sync. They are not MSC3575,
    the sliding-sync-proxy protocol of the same name, and carry no member that
    only a proxy reads. [Matrix_client.Sliding_sync] posts a {!Request.t} and
    decodes a {!Response.t}.

    @see <https://github.com/matrix-org/matrix-spec-proposals/pull/4186>
      MSC4186: Simplified Sliding Sync *)

module Required_state : sig
  (** A state event a window asks the server to return with each of its rooms.

      Three state keys are wildcards rather than literal keys. {!any_state_key}
      matches every key of the event type, {!lazy_members} asks for the members
      the timeline mentions, and {!own_membership} for the caller's own
      membership. {!any_event_type} matches every event type. *)

  type t = { event_type : Matrix_event.Event_type.t; state_key : string }
  (** The type for required-state keys. *)

  val v : ?state_key:string -> Matrix_event.Event_type.t -> t
  (** [v event_type] is the key naming [event_type]. [state_key] defaults to the
      empty string. *)

  val any_event_type : Matrix_event.Event_type.t
  (** The event type ["*"], which matches every type. *)

  val any_state_key : string
  (** The state key ["*"], which matches every key. *)

  val lazy_members : string
  (** The state key ["$LAZY"], which asks for lazy member loading. *)

  val own_membership : string
  (** The state key ["$ME"], which asks for the caller's own membership. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] name the same event type and state
      key. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. MSC4186 writes a key as a two-element
      array, not an object. *)
end

(** {1 Requests} *)

module Request : sig
  (** The sliding sync request body.

      Every builder returns a new value and takes the request last, so they
      compose with [|>].

      {[
      Request.v ~conn_id:"room-list" ()
      |> Request.add_list ~name:"all" ~timeline_limit:1
      |> Request.enable_e2ee
      ]}

      Adding a list or a subscription that is already present replaces it in
      place, so a request assembled the same way twice encodes to the same
      bytes. *)

  (** {2 Windows} *)

  type filters = {
    is_dm : bool option;
        (** Only DM rooms ([Some true]), only non-DM rooms ([Some false]) or
            both ([None]). DM-ness comes from the [m.direct] account-data event.
        *)
    is_encrypted : bool option;
        (** Only rooms with an [m.room.encryption] state event, only rooms
            without, or both. *)
    is_invite : bool option;  (** Only invited rooms, only joined, or both. *)
    room_types : string list;
        (** Only rooms whose [m.room.create] carries one of these [type]s. Empty
            means no constraint. *)
    not_room_types : string list;
        (** Exclude these create-types. A type in both lists is excluded, since
            [not_room_types] wins. *)
  }
  (** What a window admits. All fields are ANDed, and an absent field is no
      constraint rather than [false]. MSC4186 defines no filter on spaces, room
      name or tags. *)

  val no_filters : filters
  (** [no_filters] has every field unset, so it constrains nothing. *)

  type list_request = {
    ranges : (int * int) list;
        (** Inclusive [(start, end)] index windows into the sorted room list,
            such as [[ (0, 19) ]] for the first twenty rooms. *)
    required_state : Required_state.t list;
        (** The state to return for each room in the window. *)
    timeline_limit : int;  (** Maximum timeline events per room. Always sent. *)
    filters : filters option;  (** Applied before sorting. *)
  }
  (** One sliding window, with the room details flattened into it as they are on
      the wire. *)

  type room_subscription = {
    required_state : Required_state.t list;
    timeline_limit : int;
  }
  (** A room to follow regardless of any list window. A subscription is sticky
      for the life of the session, so the server remembers it whether or not it
      is resent. *)

  (** {2 Extensions} *)

  type extension_room =
    | All_subscribed  (** The wildcard ["*"], every subscribed room. *)
    | Room of Matrix_id.Room_id.t  (** One named room. *)

  type e2ee = { enabled : bool option }
  (** The E2EE extension. *)

  type to_device = {
    enabled : bool option;
    limit : int option;  (** Maximum to-device messages per response. *)
    since : string option;
        (** Only messages after this token, taken from the previous response's
            [extensions.to_device.next_batch]. *)
  }
  (** The to-device extension. *)

  type scoped = {
    enabled : bool option;
    lists : string list option;
        (** List names the extension applies to. [None] means every list in the
            request and [Some []] means none. *)
    rooms : extension_room list option;
        (** Rooms the extension applies to. [None] means every subscribed room
            and [Some []] means none. *)
  }
  (** The shape shared by the account-data, receipts and typing extensions,
      which are field-for-field identical. *)

  type profiles = { enabled : bool option; fields : string list option }
  (** MSC4262 profile updates. [fields], when present, limits the profile fields
      returned by the server. *)

  type thread_subscriptions = { enabled : bool option; limit : int option }
  (** MSC4308 thread-subscription updates. [limit] bounds the number returned in
      one response. *)

  type extensions = {
    to_device : to_device;
    e2ee : e2ee;
    account_data : scoped;
    receipts : scoped;
    typing : scoped;
    profiles : profiles;
    thread_subscriptions : thread_subscriptions;
    other : (string * Jsont.json) list;
        (** Unknown extension members, ordered by name. Known member names in
            this list are ignored when encoding. *)
  }
  (** Every extension's configuration. Each known extension is omitted from the
      JSON when empty, and unknown extension members are retained in [other] and
      re-emitted. The whole object is omitted when all of them are empty. *)

  val no_extensions : extensions
  (** [no_extensions] has no extension enabled. *)

  (** {2 The body} *)

  type t = {
    conn_id : string option;
        (** Identifies this connection so a client can hold more than one
            sliding-sync session at a time. At most 16 characters. *)
    txn_id : string option;
        (** Echoed back in the response, so a client can tell which request
            parameters arrived. Normally absent. *)
    lists : (string * list_request) list;
        (** Windows, by caller-chosen name, ordered as built. *)
    room_subscriptions : (Matrix_id.Room_id.t * room_subscription) list;
    extensions : extensions;
  }
  (** The request body, less the query parameters. [pos] and [timeout] are
      arguments of [Matrix_client.Sliding_sync.sync_once]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Empty members are omitted, so an empty
      request encodes to [{}]. *)

  val v : ?conn_id:string -> unit -> t
  (** [v ()] is the empty request, with no list, no subscription and no
      extension. [conn_id] defaults to absent. *)

  val add_list :
    name:string ->
    ?ranges:(int * int) list ->
    ?required_state:Required_state.t list ->
    ?timeline_limit:int ->
    ?filters:filters ->
    t ->
    t
  (** [add_list ~name t] adds or replaces the list [name]. [ranges] defaults to
      [[ (0, 19) ]]. [required_state] defaults to the empty list.
      [timeline_limit] defaults to [10]. [filters] defaults to absent. *)

  val remove_list : name:string -> t -> t
  (** [remove_list ~name t] drops the list [name], if present. *)

  val subscribe_room :
    room_id:Matrix_id.Room_id.t ->
    ?required_state:Required_state.t list ->
    ?timeline_limit:int ->
    t ->
    t
  (** [subscribe_room ~room_id t] adds or replaces a subscription to [room_id].
      [required_state] defaults to the empty list and [timeline_limit] to [10].
  *)

  val unsubscribe_room : room_id:Matrix_id.Room_id.t -> t -> t
  (** [unsubscribe_room ~room_id t] drops the subscription to [room_id]. It only
      stops sending it. The server keeps the subscription for the life of the
      session, which only a new session ends. *)

  val clear_room_subscriptions : t -> t
  (** [clear_room_subscriptions t] drops every subscription. *)

  val enable_e2ee : t -> t
  (** [enable_e2ee t] asks for [device_lists], one-time-key counts and unused
      fallback key types in [extensions.e2ee]. *)

  val enable_to_device : ?limit:int -> ?since:string -> t -> t
  (** [enable_to_device t] asks for to-device messages. [limit] and [since]
      default to absent. *)

  val with_to_device_since : since:string option -> t -> t
  (** [with_to_device_since ~since t] sets [extensions.to_device.since]. [None]
      removes it. *)

  val to_device_enabled : t -> bool
  (** [to_device_enabled t] is [true] when [extensions.to_device.enabled] is
      [Some true]. *)

  val enable_account_data :
    ?lists:string list -> ?rooms:extension_room list -> t -> t
  (** [enable_account_data t] asks for global and per-room account data. [lists]
      and [rooms] default to absent, which is every list and every subscribed
      room. *)

  val enable_receipts :
    ?lists:string list -> ?rooms:extension_room list -> t -> t
  (** [enable_receipts t] asks for read receipts. [lists] and [rooms] default as
      they do in {!enable_account_data}. *)

  val enable_typing : ?lists:string list -> ?rooms:extension_room list -> t -> t
  (** [enable_typing t] asks for typing notifications. [lists] and [rooms]
      default as they do in {!enable_account_data}. *)

  val enable_profiles : ?fields:string list -> t -> t
  (** [enable_profiles t] asks for MSC4262 profile updates. [fields] defaults to
      absent, meaning all fields supported by the server. *)

  val enable_thread_subscriptions : ?limit:int -> t -> t
  (** [enable_thread_subscriptions t] asks for MSC4308 subscription changes.
      [limit] must be a JavaScript-safe unsigned integer when supplied. *)

  val with_txn_id : txn_id:string option -> t -> t
  (** [with_txn_id ~txn_id t] sets the transaction id. [None] removes it. *)
end

(** {1 Responses} *)

module Response : sig
  (** The sliding sync response body. *)

  type list_response = { count : int }
  (** The total number of rooms a list matches, which is what a UI needs to size
      its scrollbar. *)

  type hero = {
    user_id : Matrix_id.User_id.t;
    displayname : string option;
    avatar_url : string option;
  }
  (** A member the server offers for computing a room's display name when the
      room has no [m.room.name]. *)

  type profile_update =
    | Updated of (string * Jsont.json) list
    | Dropped
        (** A profile delta. The [Updated] map is a patch: a field whose value
            is JSON null is removed, while an absent field is left unchanged.
            [Dropped] removes the user's complete profile. *)

  type profiles = { users : (Matrix_id.User_id.t * profile_update) list }
  (** MSC4262 profile updates, keyed by validated user ID. *)

  (** What a response says about a room's avatar. The three cases are distinct
      because collapsing the first two would lose a removal. *)
  type avatar =
    | Unchanged  (** The member was absent, so the avatar did not change. *)
    | Removed  (** An explicit JSON null, so the avatar was removed. *)
    | Set of string  (** The new [mxc://] URI. *)

  type room = {
    name : string option;  (** The name as computed by the server. *)
    avatar : avatar;
    initial : bool option;
        (** Whether this is the first time the room appears in this session. *)
    is_dm : bool option;
    invite_state : Matrix_event.Stripped_event.t list option;
        (** Present when the room is an unaccepted invite. *)
    highlight_count : int option;
        (** Unread events matching a highlight push rule. *)
    notification_count : int option;
        (** Unread events matching a notifying push rule. *)
    timeline : Matrix_event.Raw_event.t list;
        (** Message-like events and live state events, oldest first. *)
    required_state : Matrix_event.Raw_event.t list;
        (** The state the request asked for. It is preferable to state events
            found in [timeline], which may be incomplete or stale. *)
    prev_batch : string option;
        (** Pagination token for the events before [timeline]. *)
    limited : bool;
        (** The server truncated [timeline] to [timeline_limit], so there is a
            gap and what came before must be paginated with [prev_batch]. *)
    joined_count : int option;
    invited_count : int option;
    num_live : int option;
        (** How many of [timeline]'s events are live rather than historical. *)
    bump_stamp : int option;
        (** A recency stamp. A room that receives an update gets a stamp higher
            than every other room's, so sorting by it descending gives a
            recently-active-first room list. *)
    heroes : hero list option;
  }
  (** What the server reports about one room. Absent members are deltas left
      unchanged rather than values cleared. *)

  type e2ee = {
    device_lists : Matrix_sync.Device_lists.t;
        (** Users whose devices changed, or who no longer share an encrypted
            room. Deltas since [pos], not since the beginning. *)
    device_one_time_keys_count : (string * int) list;
        (** Unclaimed one-time keys held by the server, by algorithm. *)
    device_unused_fallback_key_types : string list option;
        (** Present only if the server supports fallback keys. *)
  }
  (** The E2EE extension's answer. *)

  type to_device = { next_batch : string; events : Jsont.json list }
  (** The to-device extension's answer. [next_batch] becomes the next request's
      [extensions.to_device.since]. *)

  type account_data = {
    global : Jsont.json list;
    rooms : (Matrix_id.Room_id.t * Jsont.json list) list;
  }
  (** The account-data extension's answer. *)

  type ephemeral = { rooms : (Matrix_id.Room_id.t * Jsont.json) list }
  (** One ephemeral event per room. The receipts and typing extensions answer
      with this shape. *)

  type thread_subscription = { automatic : bool; bump_stamp : int64 }
  (** A subscribed thread and its server ordering stamp. *)

  type thread_unsubscription = { bump_stamp : int64 }
  (** An unsubscribed thread and its server ordering stamp. *)

  type thread_subscriptions = {
    subscribed :
      (Matrix_id.Room_id.t * (Matrix_id.Event_id.t * thread_subscription) list)
      list;
    unsubscribed :
      (Matrix_id.Room_id.t
      * (Matrix_id.Event_id.t * thread_unsubscription) list)
      list;
    prev_batch : string option;
  }
  (** MSC4308 changes. [prev_batch] requests older changes through the companion
      endpoint, bounded above by the position preceding this response. *)

  type extensions = {
    to_device : to_device option;
    e2ee : e2ee;
    account_data : account_data;
    receipts : ephemeral;
    typing : ephemeral;
    profiles : profiles;
    thread_subscriptions : thread_subscriptions;
    other : (string * Jsont.json) list;
        (** Unknown extension members, ordered by name. Known member names in
            this list are ignored when encoding. *)
  }
  (** Every extension's answer. Unknown members are retained and re-emitted. *)

  type t = {
    pos : string;
        (** The token for the next request. Sending the same [pos] twice gives
            the same response back, so it is safe to replay after a failure. *)
    txn_id : string option;  (** Echoes the request's [txn_id]. *)
    lists : (string * list_response) list;
    rooms : (Matrix_id.Room_id.t * room) list;
    extensions : extensions;
  }
  (** One sliding sync response. Everything in it is a delta against what
      earlier responses on the same [pos] chain carried. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)

  val to_device_next_batch : t -> string option
  (** [to_device_next_batch r] is [r.extensions.to_device]'s [next_batch], and
      [None] when the extension did not answer. *)
end
