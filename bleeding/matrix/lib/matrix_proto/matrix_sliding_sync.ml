module Room_id = Matrix_id.Room_id
module User_id = Matrix_id.User_id
module Event_id = Matrix_id.Event_id
module Raw_event = Matrix_event.Raw_event
module Stripped_event = Matrix_event.Stripped_event
module Event_type = Matrix_event.Event_type
module Device_lists = Matrix_sync.Device_lists
module String_map = Map.MakePortable (String)

(* A JSON object whose members are uniformly typed, as an association list
   ordered by key. Association lists rather than a [Map.Make] keep the public
   types free of a functor application. *)
let string_map_jsont = Matrix_json.Codec.string_map

(* The same, with room ids for keys. A key that is not a room id is a
   protocol violation and fails the decode. *)
let room_map_jsont t =
  Matrix_json.Codec.keyed_map ~what:"room id" ~of_string:Room_id.of_string
    ~to_string:Room_id.to_string t

(* The profiles extension uses user IDs as object keys.  Validate those keys
   while decoding instead of allowing malformed IDs to leak into state. *)
let user_map_jsont t =
  Matrix_json.Codec.keyed_map ~what:"user id" ~of_string:User_id.of_string
    ~to_string:User_id.to_string t

(* Thread subscriptions use event IDs as the keys of the inner object. *)
let event_map_jsont t =
  Matrix_json.Codec.keyed_map ~what:"event id" ~of_string:Event_id.of_string
    ~to_string:Event_id.to_string t

let max_safe_integer = 9007199254740991L
let js_safe_uint64_jsont = Matrix_json.Codec.uint64
let js_safe_uint_jsont = Matrix_json.Codec.uint

(* MSC4186 writes ranges and required-state entries as two-element JSON
   arrays, not objects. *)
let pair_jsont ~kind elt =
  Jsont.list elt
  |> Jsont.map ~kind
       ~dec:(function
         | [ a; b ] -> (a, b)
         | l ->
             Jsont.Error.msgf Jsont.Meta.none
               "%s: expected a 2-element array, found %d elements" kind
               (List.length l))
       ~enc:(fun (a, b) -> [ a; b ])

let range_jsont : (int * int) Jsont.t =
  pair_jsont ~kind:"range" Matrix_json.Codec.int

let is_empty_list l = l = []

module Required_state = struct
  type t = { event_type : Event_type.t; state_key : string }

  let any_event_type = Event_type.of_string "*"
  let any_state_key = "*"
  let lazy_members = "$LAZY"
  let own_membership = "$ME"
  let v ?(state_key = "") event_type = { event_type; state_key }

  let equal a b =
    Event_type.equal a.event_type b.event_type
    && String.equal a.state_key b.state_key

  let jsont : t Jsont.t =
    pair_jsont ~kind:"required_state" Matrix_json.Codec.string
    |> Jsont.map
         ~dec:(fun (t, k) ->
           { event_type = Event_type.of_string t; state_key = k })
         ~enc:(fun t -> (Event_type.to_string t.event_type, t.state_key))
end

module Request = struct
  type extension_room = All_subscribed | Room of Room_id.t

  let extension_room_jsont =
    Jsont.of_of_string ~kind:"extension_room"
      ~enc:(function All_subscribed -> "*" | Room r -> Room_id.to_string r)
      (function
        | "*" -> Ok All_subscribed
        | s ->
            Result.map
              (fun r -> Room r)
              (Result.map_error (fun (`Msg m) -> m) (Room_id.of_string s)))

  type filters = {
    is_dm : bool option;
    is_encrypted : bool option;
    is_invite : bool option;
    room_types : string list;
    not_room_types : string list;
  }

  let no_filters =
    {
      is_dm = None;
      is_encrypted = None;
      is_invite = None;
      room_types = [];
      not_room_types = [];
    }

  let filters_jsont =
    Jsont.Object.(
      map (fun is_dm is_encrypted is_invite room_types not_room_types ->
          { is_dm; is_encrypted; is_invite; room_types; not_room_types })
      |> opt_mem "is_dm" Jsont.bool ~enc:(fun t -> t.is_dm)
      |> opt_mem "is_encrypted" Jsont.bool ~enc:(fun t -> t.is_encrypted)
      |> opt_mem "is_invite" Jsont.bool ~enc:(fun t -> t.is_invite)
      |> mem "room_types"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.room_types)
      |> mem "not_room_types"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.not_room_types)
      |> finish)

  type list_request = {
    ranges : (int * int) list;
    required_state : Required_state.t list;
    timeline_limit : int;
    filters : filters option;
  }

  let list_request_jsont =
    Jsont.Object.(
      map (fun ranges required_state timeline_limit filters ->
          { ranges; required_state; timeline_limit; filters })
      |> mem "ranges" (Jsont.list range_jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.ranges)
      |> mem "required_state"
           (Jsont.list Required_state.jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.required_state)
      |> mem "timeline_limit" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun t -> t.timeline_limit)
      |> opt_mem "filters" filters_jsont ~enc:(fun t -> t.filters)
      |> finish)

  type room_subscription = {
    required_state : Required_state.t list;
    timeline_limit : int;
  }

  let room_subscription_jsont =
    Jsont.Object.(
      map (fun required_state timeline_limit ->
          { required_state; timeline_limit })
      |> mem "required_state"
           (Jsont.list Required_state.jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : room_subscription) -> t.required_state)
      |> mem "timeline_limit" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun (t : room_subscription) -> t.timeline_limit)
      |> finish)

  type e2ee = { enabled : bool option }

  let e2ee_off = { enabled = None }
  let e2ee_is_empty (t : e2ee) = t.enabled = None

  let e2ee_jsont =
    Jsont.Object.(
      map (fun enabled -> { enabled })
      |> opt_mem "enabled" Jsont.bool ~enc:(fun (t : e2ee) -> t.enabled)
      |> finish)

  type to_device = {
    enabled : bool option;
    limit : int option;
    since : string option;
  }

  let to_device_off = { enabled = None; limit = None; since = None }

  let to_device_is_empty (t : to_device) =
    t.enabled = None && t.limit = None && t.since = None

  let to_device_jsont =
    Jsont.Object.(
      map (fun enabled limit since -> { enabled; limit; since })
      |> opt_mem "enabled" Jsont.bool ~enc:(fun (t : to_device) -> t.enabled)
      |> opt_mem "limit" Matrix_json.Codec.int ~enc:(fun (t : to_device) ->
          t.limit)
      |> opt_mem "since" Matrix_json.Codec.string ~enc:(fun (t : to_device) ->
          t.since)
      |> finish)

  type scoped = {
    enabled : bool option;
    lists : string list option;
    rooms : extension_room list option;
  }

  let scoped_off = { enabled = None; lists = None; rooms = None }

  (* The scope members alone do not make the extension non-empty; only
     [enabled] does, so an extension configured but not enabled is omitted. *)
  let scoped_is_empty (t : scoped) = t.enabled = None

  let scoped_jsont =
    Jsont.Object.(
      map (fun enabled lists rooms -> { enabled; lists; rooms })
      |> opt_mem "enabled" Jsont.bool ~enc:(fun (t : scoped) -> t.enabled)
      |> opt_mem "lists" (Jsont.list Matrix_json.Codec.string)
           ~enc:(fun (t : scoped) -> t.lists)
      |> opt_mem "rooms" (Jsont.list extension_room_jsont)
           ~enc:(fun (t : scoped) -> t.rooms)
      |> finish)

  type profiles = { enabled : bool option; fields : string list option }

  let profiles_off = { enabled = None; fields = None }
  let profiles_is_empty (t : profiles) = t.enabled = None

  let profiles_jsont =
    Jsont.Object.(
      map (fun enabled fields -> { enabled; fields })
      |> opt_mem "enabled" Jsont.bool ~enc:(fun (t : profiles) -> t.enabled)
      |> opt_mem "fields" (Jsont.list Matrix_json.Codec.string)
           ~enc:(fun (t : profiles) -> t.fields)
      |> finish)

  type thread_subscriptions = { enabled : bool option; limit : int option }

  let thread_subscriptions_off = { enabled = None; limit = None }

  let thread_subscriptions_is_empty (t : thread_subscriptions) =
    t.enabled = None && t.limit = None

  let thread_subscriptions_jsont =
    Jsont.Object.(
      map (fun enabled limit -> { enabled; limit })
      |> opt_mem "enabled" Jsont.bool ~enc:(fun (t : thread_subscriptions) ->
          t.enabled)
      |> opt_mem "limit" js_safe_uint_jsont
           ~enc:(fun (t : thread_subscriptions) -> t.limit)
      |> finish)

  type extensions = {
    to_device : to_device;
    e2ee : e2ee;
    account_data : scoped;
    receipts : scoped;
    typing : scoped;
    profiles : profiles;
    thread_subscriptions : thread_subscriptions;
    other : (string * Jsont.json) list;
  }

  let no_extensions =
    {
      to_device = to_device_off;
      e2ee = e2ee_off;
      account_data = scoped_off;
      receipts = scoped_off;
      typing = scoped_off;
      profiles = profiles_off;
      thread_subscriptions = thread_subscriptions_off;
      other = [];
    }

  let known_extension_member = function
    | "to_device" | "e2ee" | "account_data" | "receipts" | "typing"
    | "org.matrix.msc4262.profiles" | "io.element.msc4308.thread_subscriptions"
      ->
        true
    | _ -> false

  let unknown_extension_members (t : extensions) =
    t.other
    |> List.filter (fun (name, _) -> not (known_extension_member name))
    |> List.to_seq |> String_map.of_seq

  let extensions_is_empty t =
    to_device_is_empty t.to_device
    && e2ee_is_empty t.e2ee
    && scoped_is_empty t.account_data
    && scoped_is_empty t.receipts && scoped_is_empty t.typing
    && profiles_is_empty t.profiles
    && thread_subscriptions_is_empty t.thread_subscriptions
    && String_map.is_empty (unknown_extension_members t)

  let extensions_jsont =
    Jsont.Object.(
      map
        (fun
          to_device
          e2ee
          account_data
          receipts
          typing
          profiles
          thread_subscriptions
          other
        ->
          {
            to_device;
            e2ee;
            account_data;
            receipts;
            typing;
            profiles;
            thread_subscriptions;
            other = String_map.bindings other;
          })
      |> mem "to_device" to_device_jsont
           ~dec_absent:(fun () -> to_device_off)
           ~enc_omit:to_device_is_empty
           ~enc:(fun (t : extensions) -> t.to_device)
      |> mem "e2ee" e2ee_jsont
           ~dec_absent:(fun () -> e2ee_off)
           ~enc_omit:e2ee_is_empty
           ~enc:(fun (t : extensions) -> t.e2ee)
      |> mem "account_data" scoped_jsont
           ~dec_absent:(fun () -> scoped_off)
           ~enc_omit:scoped_is_empty
           ~enc:(fun (t : extensions) -> t.account_data)
      |> mem "receipts" scoped_jsont
           ~dec_absent:(fun () -> scoped_off)
           ~enc_omit:scoped_is_empty
           ~enc:(fun (t : extensions) -> t.receipts)
      |> mem "typing" scoped_jsont
           ~dec_absent:(fun () -> scoped_off)
           ~enc_omit:scoped_is_empty
           ~enc:(fun (t : extensions) -> t.typing)
      |> mem "org.matrix.msc4262.profiles" profiles_jsont
           ~dec_absent:(fun () -> profiles_off)
           ~enc_omit:profiles_is_empty
           ~enc:(fun (t : extensions) -> t.profiles)
      |> mem "io.element.msc4308.thread_subscriptions"
           thread_subscriptions_jsont
           ~dec_absent:(fun () -> thread_subscriptions_off)
           ~enc_omit:thread_subscriptions_is_empty
           ~enc:(fun (t : extensions) -> t.thread_subscriptions)
      |> keep_unknown
           (Matrix_json.Codec.string_map_mems Matrix_json.Codec.json)
           ~enc:unknown_extension_members
      |> finish)

  type t = {
    conn_id : string option;
    txn_id : string option;
    lists : (string * list_request) list;
    room_subscriptions : (Room_id.t * room_subscription) list;
    extensions : extensions;
  }

  let jsont =
    Jsont.Object.(
      map (fun conn_id txn_id lists room_subscriptions extensions ->
          { conn_id; txn_id; lists; room_subscriptions; extensions })
      |> opt_mem "conn_id" Matrix_json.Codec.string ~enc:(fun t -> t.conn_id)
      |> opt_mem "txn_id" Matrix_json.Codec.string ~enc:(fun t -> t.txn_id)
      |> mem "lists"
           (string_map_jsont list_request_jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.lists)
      |> mem "room_subscriptions"
           (room_map_jsont room_subscription_jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.room_subscriptions)
      |> mem "extensions" extensions_jsont
           ~dec_absent:(fun () -> no_extensions)
           ~enc_omit:extensions_is_empty
           ~enc:(fun t -> t.extensions)
      |> finish)

  let v ?conn_id () =
    {
      conn_id;
      txn_id = None;
      lists = [];
      room_subscriptions = [];
      extensions = no_extensions;
    }

  (* Replace an existing entry in place rather than appending a duplicate, so
     that a request built the same way twice encodes byte-identically. *)
  let set_assoc ~eq key value l =
    if List.exists (fun (k, _) -> eq k key) l then
      List.map (fun (k, v) -> if eq k key then (k, value) else (k, v)) l
    else l @ [ (key, value) ]

  let add_list ~name ?(ranges = [ (0, 19) ]) ?(required_state = [])
      ?(timeline_limit = 10) ?filters t =
    let l = { ranges; required_state; timeline_limit; filters } in
    { t with lists = set_assoc ~eq:String.equal name l t.lists }

  let remove_list ~name t =
    { t with lists = List.filter (fun (k, _) -> k <> name) t.lists }

  let subscribe_room ~room_id ?(required_state = []) ?(timeline_limit = 10) t =
    let s = { required_state; timeline_limit } in
    {
      t with
      room_subscriptions =
        set_assoc ~eq:Room_id.equal room_id s t.room_subscriptions;
    }

  let unsubscribe_room ~room_id t =
    {
      t with
      room_subscriptions =
        List.filter
          (fun (k, _) -> not (Room_id.equal k room_id))
          t.room_subscriptions;
    }

  let clear_room_subscriptions t = { t with room_subscriptions = [] }

  let enable_e2ee t =
    { t with extensions = { t.extensions with e2ee = { enabled = Some true } } }

  let enable_to_device ?limit ?since t =
    {
      t with
      extensions =
        { t.extensions with to_device = { enabled = Some true; limit; since } };
    }

  let with_to_device_since ~since t =
    {
      t with
      extensions =
        { t.extensions with to_device = { t.extensions.to_device with since } };
    }

  let to_device_enabled t = t.extensions.to_device.enabled = Some true

  let enable_account_data ?lists ?rooms t =
    {
      t with
      extensions =
        {
          t.extensions with
          account_data = { enabled = Some true; lists; rooms };
        };
    }

  let enable_receipts ?lists ?rooms t =
    {
      t with
      extensions =
        { t.extensions with receipts = { enabled = Some true; lists; rooms } };
    }

  let enable_typing ?lists ?rooms t =
    {
      t with
      extensions =
        { t.extensions with typing = { enabled = Some true; lists; rooms } };
    }

  let enable_profiles ?fields t =
    {
      t with
      extensions =
        { t.extensions with profiles = { enabled = Some true; fields } };
    }

  let enable_thread_subscriptions ?limit t =
    Option.iter
      (fun limit ->
        if limit < 0 || Int64.of_int limit > max_safe_integer then
          invalid_arg
            "Sliding_sync.Request.enable_thread_subscriptions: limit is not a \
             JavaScript-safe unsigned integer")
      limit;
    {
      t with
      extensions =
        {
          t.extensions with
          thread_subscriptions = { enabled = Some true; limit };
        };
    }

  let with_txn_id ~txn_id t = { t with txn_id }
end

module Response = struct
  type list_response = { count : int }

  let list_response_jsont =
    Jsont.Object.(
      map (fun count -> { count })
      |> mem "count" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun t -> t.count)
      |> finish)

  type hero = {
    user_id : User_id.t;
    displayname : string option;
    avatar_url : string option;
  }

  let hero_jsont =
    Jsont.Object.(
      map (fun user_id displayname avatar_url ->
          { user_id; displayname; avatar_url })
      |> mem "user_id" User_id.jsont ~enc:(fun t -> t.user_id)
      |> opt_mem "displayname" Matrix_json.Codec.string ~enc:(fun t ->
          t.displayname)
      |> opt_mem "avatar_url" Matrix_json.Codec.string ~enc:(fun t ->
          t.avatar_url)
      |> finish)

  type profile_update = Updated of (string * Jsont.json) list | Dropped
  type profiles = { users : (User_id.t * profile_update) list }

  (* Ruma's [UserProfileUpdate] serializes the update arm as
     { ["updated"] = map }. Keeping that map untyped permits new profile fields
     without a client release. *)
  let profile_update_object_jsont =
    Jsont.Object.(
      map (fun fields -> Updated fields)
      |> mem "updated" (string_map_jsont Matrix_json.Codec.json) ~enc:(function
        | Updated fields -> fields
        (* [profile_update_jsont]'s encoder selects [dropped_jsont] for this
           case. Keep this projection total so a future codec refactor cannot
           turn a typed value into a process abort. *)
        | Dropped -> [])
      |> finish)

  let dropped_jsont =
    Jsont.null ()
    |> Jsont.map ~kind:"profile_update"
         ~dec:(fun () -> Dropped)
         ~enc:(fun _ -> ())

  let profile_update_jsont =
    Jsont.any ~kind:"profile_update" ~dec_null:dropped_jsont
      ~dec_object:profile_update_object_jsont
      ~enc:(function
        | Dropped -> dropped_jsont | Updated _ -> profile_update_object_jsont)
      ()

  let profiles_jsont =
    Jsont.Object.(
      map (fun users -> { users })
      |> mem "users"
           (user_map_jsont profile_update_jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : profiles) -> t.users)
      |> finish)

  type avatar = Unchanged | Removed | Set of string

  (* [Unchanged] is the absent member, [Removed] an explicit JSON null. The
     encoder never sees [Unchanged] because [enc_omit] fires first. *)
  let avatar_jsont =
    Jsont.option Matrix_json.Codec.string
    |> Jsont.map ~kind:"avatar"
         ~dec:(function None -> Removed | Some s -> Set s)
         ~enc:(function Set s -> Some s | Removed | Unchanged -> None)

  type room = {
    name : string option;
    avatar : avatar;
    initial : bool option;
    is_dm : bool option;
    invite_state : Stripped_event.t list option;
    highlight_count : int option;
    notification_count : int option;
    timeline : Raw_event.t list;
    required_state : Raw_event.t list;
    prev_batch : string option;
    limited : bool;
    joined_count : int option;
    invited_count : int option;
    num_live : int option;
    bump_stamp : int option;
    heroes : hero list option;
  }

  let room_jsont =
    Jsont.Object.(
      map
        (fun
          name
          avatar
          initial
          is_dm
          invite_state
          highlight_count
          notification_count
          timeline
          required_state
          prev_batch
          limited
          joined_count
          invited_count
          num_live
          bump_stamp
          heroes
        ->
          {
            name;
            avatar;
            initial;
            is_dm;
            invite_state;
            highlight_count;
            notification_count;
            timeline;
            required_state;
            prev_batch;
            limited;
            joined_count;
            invited_count;
            num_live;
            bump_stamp;
            heroes;
          })
      |> opt_mem "name" Matrix_json.Codec.string ~enc:(fun t -> t.name)
      |> mem "avatar" avatar_jsont
           ~dec_absent:(fun () -> Unchanged)
           ~enc_omit:(fun a -> a = Unchanged)
           ~enc:(fun t -> t.avatar)
      |> opt_mem "initial" Jsont.bool ~enc:(fun t -> t.initial)
      |> opt_mem "is_dm" Jsont.bool ~enc:(fun t -> t.is_dm)
      |> opt_mem "invite_state" (Jsont.list Stripped_event.jsont) ~enc:(fun t ->
          t.invite_state)
      (* MSC4186 flattens the unread-notification counts into the room
         object rather than nesting them. *)
      |> opt_mem "highlight_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.highlight_count)
      |> opt_mem "notification_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.notification_count)
      |> mem "timeline"
           (Jsont.list Raw_event.jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.timeline)
      |> mem "required_state"
           (Jsont.list Raw_event.jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.required_state)
      |> opt_mem "prev_batch" Matrix_json.Codec.string ~enc:(fun t ->
          t.prev_batch)
      |> mem "limited" Jsont.bool
           ~dec_absent:(fun () -> false)
           ~enc_omit:not
           ~enc:(fun t -> t.limited)
      |> opt_mem "joined_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.joined_count)
      |> opt_mem "invited_count" Matrix_json.Codec.int ~enc:(fun t ->
          t.invited_count)
      |> opt_mem "num_live" Matrix_json.Codec.int ~enc:(fun t -> t.num_live)
      |> opt_mem "bump_stamp" Matrix_json.Codec.int ~enc:(fun t -> t.bump_stamp)
      |> opt_mem "heroes" (Jsont.list hero_jsont) ~enc:(fun t -> t.heroes)
      |> finish)

  type e2ee = {
    device_lists : Device_lists.t;
    device_one_time_keys_count : (string * int) list;
    device_unused_fallback_key_types : string list option;
  }

  let no_device_lists : Device_lists.t = { changed = []; left = [] }

  let e2ee_empty =
    {
      device_lists = no_device_lists;
      device_one_time_keys_count = [];
      device_unused_fallback_key_types = None;
    }

  let e2ee_is_empty (t : e2ee) =
    t.device_lists.changed = []
    && t.device_lists.left = []
    && t.device_one_time_keys_count = []
    && t.device_unused_fallback_key_types = None

  let e2ee_jsont =
    Jsont.Object.(
      map
        (fun
          device_lists
          device_one_time_keys_count
          device_unused_fallback_key_types
        ->
          {
            device_lists;
            device_one_time_keys_count;
            device_unused_fallback_key_types;
          })
      |> mem "device_lists" Device_lists.jsont
           ~dec_absent:(fun () -> no_device_lists)
           ~enc:(fun (t : e2ee) -> t.device_lists)
      |> mem "device_one_time_keys_count"
           (string_map_jsont js_safe_uint_jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : e2ee) -> t.device_one_time_keys_count)
      |> opt_mem "device_unused_fallback_key_types"
           (Jsont.list Matrix_json.Codec.string) ~enc:(fun (t : e2ee) ->
             t.device_unused_fallback_key_types)
      |> finish)

  type to_device = { next_batch : string; events : Jsont.json list }

  let to_device_jsont =
    Jsont.Object.(
      map (fun next_batch events -> { next_batch; events })
      |> mem "next_batch" Matrix_json.Codec.string ~enc:(fun (t : to_device) ->
          t.next_batch)
      |> mem "events"
           (Jsont.list Matrix_json.Codec.json)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : to_device) -> t.events)
      |> finish)

  type account_data = {
    global : Jsont.json list;
    rooms : (Room_id.t * Jsont.json list) list;
  }

  let account_data_empty = { global = []; rooms = [] }
  let account_data_is_empty (t : account_data) = t.global = [] && t.rooms = []

  let account_data_jsont =
    Jsont.Object.(
      map (fun global rooms -> { global; rooms })
      |> mem "global"
           (Jsont.list Matrix_json.Codec.json)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : account_data) -> t.global)
      |> mem "rooms"
           (room_map_jsont (Jsont.list Matrix_json.Codec.json))
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : account_data) -> t.rooms)
      |> finish)

  type ephemeral = { rooms : (Room_id.t * Jsont.json) list }

  let ephemeral_empty = { rooms = [] }
  let ephemeral_is_empty (t : ephemeral) = t.rooms = []

  let ephemeral_jsont =
    Jsont.Object.(
      map (fun rooms -> { rooms })
      |> mem "rooms"
           (room_map_jsont Matrix_json.Codec.json)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : ephemeral) -> t.rooms)
      |> finish)

  type thread_subscription = { automatic : bool; bump_stamp : int64 }
  type thread_unsubscription = { bump_stamp : int64 }

  type thread_subscriptions = {
    subscribed : (Room_id.t * (Event_id.t * thread_subscription) list) list;
    unsubscribed : (Room_id.t * (Event_id.t * thread_unsubscription) list) list;
    prev_batch : string option;
  }

  let thread_subscription_jsont =
    Jsont.Object.(
      map (fun automatic bump_stamp -> { automatic; bump_stamp })
      |> mem "automatic" Jsont.bool ~enc:(fun (t : thread_subscription) ->
          t.automatic)
      |> mem "bump_stamp" js_safe_uint64_jsont
           ~enc:(fun (t : thread_subscription) -> t.bump_stamp)
      |> finish)

  let thread_unsubscription_jsont =
    Jsont.Object.(
      map (fun bump_stamp -> { bump_stamp })
      |> mem "bump_stamp" js_safe_uint64_jsont
           ~enc:(fun (t : thread_unsubscription) -> t.bump_stamp)
      |> finish)

  let thread_subscriptions_empty =
    { subscribed = []; unsubscribed = []; prev_batch = None }

  let thread_subscriptions_is_empty (t : thread_subscriptions) =
    t.subscribed = [] && t.unsubscribed = [] && t.prev_batch = None

  let thread_subscriptions_jsont =
    Jsont.Object.(
      map (fun subscribed unsubscribed prev_batch ->
          { subscribed; unsubscribed; prev_batch })
      |> mem "subscribed"
           (room_map_jsont (event_map_jsont thread_subscription_jsont))
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : thread_subscriptions) -> t.subscribed)
      |> mem "unsubscribed"
           (room_map_jsont (event_map_jsont thread_unsubscription_jsont))
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun (t : thread_subscriptions) -> t.unsubscribed)
      |> opt_mem "prev_batch" Matrix_json.Codec.string
           ~enc:(fun (t : thread_subscriptions) -> t.prev_batch)
      |> finish)

  type extensions = {
    to_device : to_device option;
    e2ee : e2ee;
    account_data : account_data;
    receipts : ephemeral;
    typing : ephemeral;
    profiles : profiles;
    thread_subscriptions : thread_subscriptions;
    other : (string * Jsont.json) list;
  }

  let no_extensions =
    {
      to_device = None;
      e2ee = e2ee_empty;
      account_data = account_data_empty;
      receipts = ephemeral_empty;
      typing = ephemeral_empty;
      profiles = { users = [] };
      thread_subscriptions = thread_subscriptions_empty;
      other = [];
    }

  let known_extension_member = function
    | "to_device" | "e2ee" | "account_data" | "receipts" | "typing"
    | "org.matrix.msc4262.profiles" | "io.element.msc4308.thread_subscriptions"
      ->
        true
    | _ -> false

  let unknown_extension_members (t : extensions) =
    t.other
    |> List.filter (fun (name, _) -> not (known_extension_member name))
    |> List.to_seq |> String_map.of_seq

  let extensions_is_empty (t : extensions) =
    t.to_device = None && e2ee_is_empty t.e2ee
    && account_data_is_empty t.account_data
    && ephemeral_is_empty t.receipts
    && ephemeral_is_empty t.typing
    && t.profiles.users = []
    && thread_subscriptions_is_empty t.thread_subscriptions
    && String_map.is_empty (unknown_extension_members t)

  let extensions_jsont =
    Jsont.Object.(
      map
        (fun
          to_device
          e2ee
          account_data
          receipts
          typing
          profiles
          thread_subscriptions
          other
        ->
          {
            to_device;
            e2ee;
            account_data;
            receipts;
            typing;
            profiles;
            thread_subscriptions;
            other = String_map.bindings other;
          })
      |> opt_mem "to_device" to_device_jsont ~enc:(fun (t : extensions) ->
          t.to_device)
      |> mem "e2ee" e2ee_jsont
           ~dec_absent:(fun () -> e2ee_empty)
           ~enc_omit:e2ee_is_empty
           ~enc:(fun (t : extensions) -> t.e2ee)
      |> mem "account_data" account_data_jsont
           ~dec_absent:(fun () -> account_data_empty)
           ~enc_omit:account_data_is_empty
           ~enc:(fun (t : extensions) -> t.account_data)
      |> mem "receipts" ephemeral_jsont
           ~dec_absent:(fun () -> ephemeral_empty)
           ~enc_omit:ephemeral_is_empty
           ~enc:(fun (t : extensions) -> t.receipts)
      |> mem "typing" ephemeral_jsont
           ~dec_absent:(fun () -> ephemeral_empty)
           ~enc_omit:ephemeral_is_empty
           ~enc:(fun (t : extensions) -> t.typing)
      |> mem "org.matrix.msc4262.profiles" profiles_jsont
           ~dec_absent:(fun () -> { users = [] })
           ~enc_omit:(fun p -> p.users = [])
           ~enc:(fun (t : extensions) -> t.profiles)
      |> mem "io.element.msc4308.thread_subscriptions"
           thread_subscriptions_jsont
           ~dec_absent:(fun () -> thread_subscriptions_empty)
           ~enc_omit:thread_subscriptions_is_empty
           ~enc:(fun (t : extensions) -> t.thread_subscriptions)
      |> keep_unknown
           (Matrix_json.Codec.string_map_mems Matrix_json.Codec.json)
           ~enc:unknown_extension_members
      |> finish)

  type t = {
    pos : string;
    txn_id : string option;
    lists : (string * list_response) list;
    rooms : (Room_id.t * room) list;
    extensions : extensions;
  }

  let jsont =
    Jsont.Object.(
      map (fun pos txn_id lists rooms extensions ->
          { pos; txn_id; lists; rooms; extensions })
      |> mem "pos" Matrix_json.Codec.string ~enc:(fun t -> t.pos)
      |> opt_mem "txn_id" Matrix_json.Codec.string ~enc:(fun t -> t.txn_id)
      |> mem "lists"
           (string_map_jsont list_response_jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.lists)
      |> mem "rooms"
           (room_map_jsont room_jsont)
           ~dec_absent:(fun () -> [])
           ~enc_omit:is_empty_list
           ~enc:(fun t -> t.rooms)
      |> mem "extensions" extensions_jsont
           ~dec_absent:(fun () -> no_extensions)
           ~enc_omit:extensions_is_empty
           ~enc:(fun t -> t.extensions)
      |> finish)

  let to_device_next_batch t =
    Option.map (fun (d : to_device) -> d.next_batch) t.extensions.to_device
end
