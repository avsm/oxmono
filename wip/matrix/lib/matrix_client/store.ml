module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module String_map = Map.Make (String)

let src = Logs.Src.create "matrix.store" ~doc:"Matrix base client state store"

module Log = (val Logs.src_log src : Logs.LOG)

type membership = Joined | Invited | Left | Knocked

let membership_to_string = function
  | Joined -> "join"
  | Invited -> "invite"
  | Left -> "leave"
  | Knocked -> "knock"

let membership_of_string s =
  match Matrix_proto.Event.Membership.of_string s with
  | Ok Join -> Ok Joined
  | Ok Invite -> Ok Invited
  | Ok (Leave | Ban) -> Ok Left
  | Ok Knock -> Ok Knocked
  | Error _ as e -> e

let membership_jsont =
  Jsont.of_of_string ~kind:"membership" ~enc:membership_to_string (fun s ->
      Result.map_error (fun (`Msg m) -> m) (membership_of_string s))

type hero = {
  user_id : Id.User_id.t;
  display_name : string option;
  avatar_url : string option;
}

let hero_jsont : hero Jsont.t =
  Jsont.Object.(
    map (fun user_id display_name avatar_url ->
        { user_id; display_name; avatar_url })
    |> mem "user_id" Id.User_id.jsont ~enc:(fun t -> t.user_id)
    |> opt_mem "display_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.display_name)
    |> opt_mem "avatar_url" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.avatar_url)
    |> finish)

type display_name =
  | Named of string
  | Aliased of string
  | Calculated of string
  | Empty_was of string
  | Empty

let display_name_to_string = function
  | Named s | Aliased s | Calculated s -> s
  | Empty_was s -> "Empty Room (was " ^ s ^ ")"
  | Empty -> "Empty Room"

type state_completeness = No_state | Partial | Complete
type plaintext_policy = Store_plaintext | Ciphertext_only
type profile = (string * Jsont.json) list

(* The spelling that last supplied a room's manual unread marker.  This is
   persisted because the stable event takes precedence over the old unstable
   spelling across sync responses and process restarts. *)
type marked_unread_source = Stable | Unstable

let marked_unread_source_jsont =
  Jsont.enum [ ("stable", Stable); ("unstable", Unstable) ]

let state_completeness_jsont =
  Jsont.enum
    [ ("no_state", No_state); ("partial", Partial); ("complete", Complete) ]

type state_event = {
  event_type : Event.Event_type.t;
  state_key : string;
  content : Jsont.json;
  sender : Id.User_id.t option;
  event_id : Id.Event_id.t option;
  origin_server_ts : Event.Timestamp.t option;
}

let state_event_jsont : state_event Jsont.t =
  Jsont.Object.(
    map (fun event_type state_key content sender event_id origin_server_ts ->
        { event_type; state_key; content; sender; event_id; origin_server_ts })
    |> mem "type" Event.Event_type.jsont ~enc:(fun t -> t.event_type)
    |> mem "state_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.state_key)
    |> mem "content" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.content)
    |> opt_mem "sender" Id.User_id.jsont ~enc:(fun t -> t.sender)
    |> opt_mem "event_id" Id.Event_id.jsont ~enc:(fun t -> t.event_id)
    |> opt_mem "origin_server_ts" Json_codec.persisted_timestamp ~enc:(fun t ->
        t.origin_server_ts)
    |> finish)

(* Persisted as a tagged object so the provenance survives a restart. *)
let display_name_jsont : display_name Jsont.t =
  let dec = function
    | Jsont.Object (o, _) -> (
        let get n =
          match Jsont.Json.find_mem n o with
          | Some (_, Jsont.String (s, _)) -> Some s
          | _ -> None
        in
        match (get "kind", get "value") with
        | Some "named", Some v -> Named v
        | Some "aliased", Some v -> Aliased v
        | Some "calculated", Some v -> Calculated v
        | Some "empty_was", Some v -> Empty_was v
        | _ -> Empty)
    | _ -> Empty
  in
  let enc v =
    let obj kind value =
      Jsont.Json.object'
        [
          Jsont.Json.mem (Jsont.Json.name "kind") (Jsont.Json.string kind);
          Jsont.Json.mem (Jsont.Json.name "value") (Jsont.Json.string value);
        ]
    in
    match v with
    | Named s -> obj "named" s
    | Aliased s -> obj "aliased" s
    | Calculated s -> obj "calculated" s
    | Empty_was s -> obj "empty_was" s
    | Empty ->
        Jsont.Json.object'
          [
            Jsont.Json.mem (Jsont.Json.name "kind") (Jsont.Json.string "empty");
          ]
  in
  Jsont.map ~kind:"room display name" ~dec ~enc Matrix_proto.Json.Codec.json

type room_info = {
  room_id : Id.Room_id.t;
  membership : membership;
  name : string option;
  canonical_alias : Id.Room_alias.t option;
  topic : string option;
  avatar_url : string option;
  encryption : Jsont.json option;
  heroes : hero list;
  joined_member_count : int;
  invited_member_count : int;
  is_dm : bool;
  display_name : display_name;
  notification_count : int;
  highlight_count : int;
  local_unread_count : int;
  local_notification_count : int;
  local_highlight_count : int;
  marked_unread : bool;
  marked_unread_source : marked_unread_source;
  latest_event : Event.Raw_event.t option;
  prev_batch : string option;
  tags : (string * float option) list;
  last_active_ts : int64;
  recency_stamp : int option;
  state_events : state_event list;
  state_completeness : state_completeness;
  members_complete : bool;
  encryption_state_complete : bool;
}

let empty_room_info ~room_id ~membership =
  {
    room_id;
    membership;
    name = None;
    canonical_alias = None;
    topic = None;
    avatar_url = None;
    encryption = None;
    heroes = [];
    joined_member_count = 0;
    invited_member_count = 0;
    is_dm = false;
    display_name = Empty;
    notification_count = 0;
    highlight_count = 0;
    local_unread_count = 0;
    local_notification_count = 0;
    local_highlight_count = 0;
    marked_unread = false;
    marked_unread_source = Unstable;
    latest_event = None;
    prev_batch = None;
    tags = [];
    last_active_ts = 0L;
    recency_stamp = None;
    state_events = [];
    state_completeness = No_state;
    members_complete = false;
    encryption_state_complete = false;
  }

let tags_jsont : (string * float option) list Jsont.t =
  Json_codec.string_map (Jsont.option Matrix_proto.Json.Codec.number)

(* jsont's object combinators cap out well below this record's arity, so the
   codec is split in two halves that are re-joined by a pair. *)
type room_info_core = {
  c_room_id : Id.Room_id.t;
  c_membership : membership;
  c_name : string option;
  c_canonical_alias : Id.Room_alias.t option;
  c_topic : string option;
  c_avatar_url : string option;
  c_encryption : Jsont.json option;
  c_heroes : hero list;
  c_display_name : display_name;
}

type room_info_counts = {
  n_joined : int;
  n_invited : int;
  n_is_dm : bool;
  n_notification : int;
  n_highlight : int;
  n_local_unread : int;
  n_local_notification : int;
  n_local_highlight : int;
  n_marked_unread : bool;
  n_marked_unread_source : marked_unread_source;
  n_latest_event : Event.Raw_event.t option;
  n_prev_batch : string option;
  n_tags : (string * float option) list;
  n_last_active_ts : int64;
  n_recency_stamp : int option;
}

type room_info_state = {
  s_format_version : int;
  s_events : state_event list;
  s_completeness : state_completeness;
  s_members_complete : bool;
  s_encryption_complete : bool;
}

let empty_room_info_state () =
  {
    s_format_version = 2;
    s_events = [];
    s_completeness = No_state;
    s_members_complete = false;
    s_encryption_complete = false;
  }

let room_info_state_jsont : room_info_state Jsont.t =
  Jsont.Object.(
    map
      (fun
        s_format_version
        s_events
        s_completeness
        s_members_complete
        s_encryption_complete
      ->
        {
          s_format_version;
          s_events;
          s_completeness;
          s_members_complete;
          s_encryption_complete;
        })
    |> mem "format_version" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 1)
         ~enc:(fun t -> t.s_format_version)
    |> mem "events"
         (Jsont.list state_event_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.s_events)
    |> mem "completeness" state_completeness_jsont
         ~dec_absent:(fun () -> No_state)
         ~enc:(fun t -> t.s_completeness)
    |> mem "members_complete" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.s_members_complete)
    |> mem "encryption_complete" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.s_encryption_complete)
    |> finish)

let room_info_core_jsont : room_info_core Jsont.t =
  Jsont.Object.(
    map
      (fun
        c_room_id
        c_membership
        c_name
        c_canonical_alias
        c_topic
        c_avatar_url
        c_encryption
        c_heroes
        c_display_name
      ->
        {
          c_room_id;
          c_membership;
          c_name;
          c_canonical_alias;
          c_topic;
          c_avatar_url;
          c_encryption;
          c_heroes;
          c_display_name;
        })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.c_room_id)
    |> mem "membership" membership_jsont
         ~dec_absent:(fun () -> Joined)
         ~enc:(fun t -> t.c_membership)
    |> opt_mem "name" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.c_name)
    |> opt_mem "canonical_alias" Id.Room_alias.jsont ~enc:(fun t ->
        t.c_canonical_alias)
    |> opt_mem "topic" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.c_topic)
    |> opt_mem "avatar_url" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.c_avatar_url)
    |> opt_mem "encryption" Matrix_proto.Json.Codec.json ~enc:(fun t ->
        t.c_encryption)
    |> mem "heroes" (Jsont.list hero_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.c_heroes)
    |> mem "display_name" display_name_jsont
         ~dec_absent:(fun () -> Empty)
         ~enc:(fun t -> t.c_display_name)
    |> finish)

let room_info_counts_jsont : room_info_counts Jsont.t =
  Jsont.Object.(
    map
      (fun
        n_joined
        n_invited
        n_is_dm
        n_notification
        n_highlight
        n_local_unread
        n_local_notification
        n_local_highlight
        n_marked_unread
        n_marked_unread_source
        n_latest_event
        n_prev_batch
        n_tags
        n_last_active_ts
        n_recency_stamp
      ->
        {
          n_joined;
          n_invited;
          n_is_dm;
          n_notification;
          n_highlight;
          n_local_unread;
          n_local_notification;
          n_local_highlight;
          n_marked_unread;
          n_marked_unread_source;
          n_latest_event;
          n_prev_batch;
          n_tags;
          n_last_active_ts;
          n_recency_stamp;
        })
    |> mem "joined_member_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_joined)
    |> mem "invited_member_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_invited)
    |> mem "is_dm" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.n_is_dm)
    |> mem "notification_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_notification)
    |> mem "highlight_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_highlight)
    |> mem "local_unread_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_local_unread)
    |> mem "local_notification_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_local_notification)
    |> mem "local_highlight_count" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.n_local_highlight)
    (* Absent in a file written before [m.marked_unread] was carried, and
       false is what such a file meant. *)
    |> mem "marked_unread" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.n_marked_unread)
    |> mem "marked_unread_source" marked_unread_source_jsont
         ~dec_absent:(fun () -> Unstable)
         ~enc:(fun t -> t.n_marked_unread_source)
    |> opt_mem "latest_event" Event.Raw_event.persisted_jsont ~enc:(fun t ->
        t.n_latest_event)
    |> opt_mem "prev_batch" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.n_prev_batch)
    |> mem "tags" tags_jsont ~dec_absent:(fun () -> []) ~enc:(fun t -> t.n_tags)
    |> mem "last_active_ts" Matrix_proto.Json.Codec.Legacy.int64
         ~dec_absent:(fun () -> 0L)
         ~enc:(fun t -> t.n_last_active_ts)
    |> opt_mem "recency_stamp" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun t ->
        t.n_recency_stamp)
    |> finish)

let split (t : room_info) =
  ( {
      c_room_id = t.room_id;
      c_membership = t.membership;
      c_name = t.name;
      c_canonical_alias = t.canonical_alias;
      c_topic = t.topic;
      c_avatar_url = t.avatar_url;
      c_encryption = t.encryption;
      c_heroes = t.heroes;
      c_display_name = t.display_name;
    },
    {
      n_joined = t.joined_member_count;
      n_invited = t.invited_member_count;
      n_is_dm = t.is_dm;
      n_notification = t.notification_count;
      n_highlight = t.highlight_count;
      n_local_unread = t.local_unread_count;
      n_local_notification = t.local_notification_count;
      n_local_highlight = t.local_highlight_count;
      n_marked_unread = t.marked_unread;
      n_marked_unread_source = t.marked_unread_source;
      n_latest_event = t.latest_event;
      n_prev_batch = t.prev_batch;
      n_tags = t.tags;
      n_last_active_ts = t.last_active_ts;
      n_recency_stamp = t.recency_stamp;
    } )

let join (c, n) =
  {
    room_id = c.c_room_id;
    membership = c.c_membership;
    name = c.c_name;
    canonical_alias = c.c_canonical_alias;
    topic = c.c_topic;
    avatar_url = c.c_avatar_url;
    encryption = c.c_encryption;
    heroes = c.c_heroes;
    display_name = c.c_display_name;
    joined_member_count = n.n_joined;
    invited_member_count = n.n_invited;
    is_dm = n.n_is_dm;
    notification_count = n.n_notification;
    highlight_count = n.n_highlight;
    local_unread_count = n.n_local_unread;
    local_notification_count = n.n_local_notification;
    local_highlight_count = n.n_local_highlight;
    marked_unread = n.n_marked_unread;
    marked_unread_source = n.n_marked_unread_source;
    latest_event = n.n_latest_event;
    prev_batch = n.n_prev_batch;
    tags = n.n_tags;
    last_active_ts = n.n_last_active_ts;
    recency_stamp = n.n_recency_stamp;
    state_events = [];
    state_completeness = No_state;
    members_complete = false;
    encryption_state_complete = false;
  }

let room_info_jsont : room_info Jsont.t =
  Jsont.Object.(
    map (fun c n s ->
        let t = join (c, n) in
        {
          t with
          state_events = s.s_events;
          state_completeness = s.s_completeness;
          members_complete = s.s_members_complete;
          encryption_state_complete = s.s_encryption_complete;
        })
    |> mem "core" room_info_core_jsont ~enc:(fun t -> fst (split t))
    |> mem "counts" room_info_counts_jsont ~enc:(fun t -> snd (split t))
    |> mem "state" room_info_state_jsont ~dec_absent:empty_room_info_state
         ~enc:(fun t ->
           {
             s_format_version = 2;
             s_events = t.state_events;
             s_completeness = t.state_completeness;
             s_members_complete = t.members_complete;
             s_encryption_complete = t.encryption_state_complete;
           })
    |> finish)

let find_state_event info ~event_type ?(state_key = "") () =
  List.find_opt
    (fun e ->
      Event.Event_type.equal e.event_type event_type
      && String.equal e.state_key state_key)
    info.state_events

let state_events_of_type info event_type =
  List.filter
    (fun e -> Event.Event_type.equal e.event_type event_type)
    info.state_events
  |> List.sort (fun a b -> String.compare a.state_key b.state_key)

type document = {
  d_format_version : int;
  d_next_batch : string option;
  d_sliding_pos : string option;
  d_sliding_to_device_since : string option;
  d_sliding_lists : (string * int) list;
  d_rooms : room_info list;
  d_account_data : (string * Jsont.json) list;
  d_receipts : (string * Read_state.t) list;
  d_profiles : (Id.User_id.t * profile) list;
  d_kv : (string * Jsont.json) list;
}

let json_map_jsont : (string * Jsont.json) list Jsont.t =
  Json_codec.string_map Matrix_proto.Json.Codec.json

let receipts_map_jsont : (string * Read_state.t) list Jsont.t =
  Json_codec.string_map Read_state.jsont

let profiles_map_jsont : (Id.User_id.t * profile) list Jsont.t =
  Json_codec.keyed_map ~what:"user id" ~of_string:Id.User_id.of_string
    ~to_string:Id.User_id.to_string json_map_jsont

let document_jsont : document Jsont.t =
  Jsont.Object.(
    map
      (fun
        d_format_version
        d_next_batch
        d_sliding_pos
        d_sliding_to_device_since
        d_sliding_lists
        d_rooms
        d_account_data
        d_receipts
        d_profiles
        d_kv
      ->
        {
          d_format_version;
          d_next_batch;
          d_sliding_pos;
          d_sliding_to_device_since;
          d_sliding_lists;
          d_rooms;
          d_account_data;
          d_receipts;
          d_profiles;
          d_kv;
        })
    |> mem "format_version" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 1)
         ~enc:(fun t -> t.d_format_version)
    |> opt_mem "next_batch" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.d_next_batch)
    |> opt_mem "sliding_pos" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.d_sliding_pos)
    |> opt_mem "sliding_to_device_since" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.d_sliding_to_device_since)
    |> mem "sliding_lists"
         (Json_codec.string_map Matrix_proto.Json.Codec.Legacy.int)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.d_sliding_lists)
    |> mem "rooms"
         (Jsont.list room_info_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.d_rooms)
    |> mem "account_data" json_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.d_account_data)
    |> mem "receipts" receipts_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.d_receipts)
    |> mem "profiles" profiles_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.d_profiles)
    |> mem "kv" json_map_jsont ~dec_absent:(fun () -> []) ~enc:(fun t -> t.d_kv)
    |> finish)

type t = {
  root : Eio.Fs.dir_ty Eio.Path.t option;
  plaintext_policy : plaintext_policy;
  mutable v_fingerprint : string option;
  mutable v_next_batch : string option;
  mutable v_sliding_pos : string option;
  mutable v_sliding_to_device_since : string option;
  mutable v_sliding_lists : (string * int) list;
  mutable v_rooms : room_info String_map.t;
  mutable v_account_data : Jsont.json String_map.t;
  mutable v_receipts : Read_state.t String_map.t;
  mutable v_profiles : profile String_map.t;
  mutable v_kv : Jsont.json String_map.t;
  mutable v_dirty : bool;
}

type snapshot = {
  snap_owner : t;
  snap_fingerprint : string option;
  snap_next_batch : string option;
  snap_sliding_pos : string option;
  snap_sliding_to_device_since : string option;
  snap_sliding_lists : (string * int) list;
  snap_rooms : room_info String_map.t;
  snap_account_data : Jsont.json String_map.t;
  snap_receipts : Read_state.t String_map.t;
  snap_profiles : profile String_map.t;
  snap_kv : Jsont.json String_map.t;
  snap_dirty : bool;
}

let file_name = "base_state.json"

let empty_store ?(plaintext_policy = Ciphertext_only) root =
  {
    root;
    plaintext_policy;
    v_fingerprint = None;
    v_next_batch = None;
    v_sliding_pos = None;
    v_sliding_to_device_since = None;
    v_sliding_lists = [];
    v_rooms = String_map.empty;
    v_account_data = String_map.empty;
    v_receipts = String_map.empty;
    v_profiles = String_map.empty;
    v_kv = String_map.empty;
    v_dirty = false;
  }

let memory ?plaintext_policy () = empty_store ?plaintext_policy None

let load_document path data =
  let file =
    Option.value (Eio.Path.native path) ~default:Jsont.Textloc.file_none
  in
  match Jsont_bytesrw.decode_string ~file document_jsont data with
  | Ok d
    when d.d_format_version = 1 || d.d_format_version = 2
         || d.d_format_version = 3 || d.d_format_version = 4 ->
      Some d
  | Ok d ->
      Log.warn (fun m ->
          m "Ignoring base state %a with unsupported format version %d"
            Eio.Path.pp path d.d_format_version);
      None
  | Error msg ->
      Log.warn (fun m ->
          m "Ignoring unreadable base state %a: %s" Eio.Path.pp path msg);
      None

let of_seq l = List.to_seq l |> String_map.of_seq

let normalize_profile profile =
  List.fold_left
    (fun fields (name, value) -> String_map.add name value fields)
    String_map.empty profile
  |> String_map.bindings

let on_disk_with_policy ~dir ~plaintext_policy =
  let path = Eio.Path.(dir / file_name) in
  let t = empty_store ~plaintext_policy (Some dir) in
  let fingerprint, document =
    Io_context.with_context "loading Matrix base state" (fun () ->
        if Eio.Path.is_file path then
          let data = Eio.Path.load path in
          ( Some Digestif.SHA256.(digest_string data |> to_raw_string),
            load_document path data )
        else (None, None))
  in
  t.v_fingerprint <- fingerprint;
  (match document with
  | None -> ()
  | Some d ->
      t.v_next_batch <- d.d_next_batch;
      t.v_sliding_pos <- d.d_sliding_pos;
      t.v_sliding_to_device_since <- d.d_sliding_to_device_since;
      t.v_sliding_lists <- d.d_sliding_lists;
      t.v_rooms <-
        of_seq
          (List.map (fun r -> (Id.Room_id.to_string r.room_id, r)) d.d_rooms);
      t.v_account_data <- of_seq d.d_account_data;
      t.v_receipts <- of_seq d.d_receipts;
      t.v_profiles <-
        (if d.d_format_version < 3 then String_map.empty
         else
           of_seq
             (List.map
                (fun (user_id, profile) ->
                  (Id.User_id.to_string user_id, normalize_profile profile))
                d.d_profiles));
      t.v_kv <- of_seq d.d_kv);
  t

let plaintext_policy t = t.plaintext_policy
let on_disk ~dir = on_disk_with_policy ~dir ~plaintext_policy:Ciphertext_only
let dir t = t.root

let snapshot t =
  {
    snap_owner = t;
    snap_fingerprint = t.v_fingerprint;
    snap_next_batch = t.v_next_batch;
    snap_sliding_pos = t.v_sliding_pos;
    snap_sliding_to_device_since = t.v_sliding_to_device_since;
    snap_sliding_lists = t.v_sliding_lists;
    snap_rooms = t.v_rooms;
    snap_account_data = t.v_account_data;
    snap_receipts = t.v_receipts;
    snap_profiles = t.v_profiles;
    snap_kv = t.v_kv;
    snap_dirty = t.v_dirty;
  }

let restore t before =
  if t != before.snap_owner then
    invalid_arg "Store.restore: snapshot belongs to another store";
  t.v_fingerprint <- before.snap_fingerprint;
  t.v_next_batch <- before.snap_next_batch;
  t.v_sliding_pos <- before.snap_sliding_pos;
  t.v_sliding_to_device_since <- before.snap_sliding_to_device_since;
  t.v_sliding_lists <- before.snap_sliding_lists;
  t.v_rooms <- before.snap_rooms;
  t.v_account_data <- before.snap_account_data;
  t.v_receipts <- before.snap_receipts;
  t.v_profiles <- before.snap_profiles;
  t.v_kv <- before.snap_kv;
  t.v_dirty <- before.snap_dirty

let touch t = t.v_dirty <- true
let next_batch t = t.v_next_batch

let set_next_batch t s =
  t.v_next_batch <- Some s;
  touch t

let sliding_pos t = t.v_sliding_pos

let set_sliding_pos t value =
  t.v_sliding_pos <- value;
  touch t

let sliding_to_device_since t = t.v_sliding_to_device_since

let set_sliding_to_device_since t value =
  t.v_sliding_to_device_since <- value;
  touch t

let normalize_sliding_lists lists =
  List.fold_left
    (fun acc (name, count) -> String_map.add name count acc)
    String_map.empty lists
  |> String_map.bindings

let sliding_lists t = t.v_sliding_lists

let replace_sliding_session t ~pos ~to_device_since ~lists =
  t.v_sliding_pos <- pos;
  t.v_sliding_to_device_since <- to_device_since;
  t.v_sliding_lists <- normalize_sliding_lists lists;
  touch t

let rooms t = String_map.bindings t.v_rooms |> List.map snd
let find_room t id = String_map.find_opt (Id.Room_id.to_string id) t.v_rooms

let set_room t info =
  t.v_rooms <- String_map.add (Id.Room_id.to_string info.room_id) info t.v_rooms;
  touch t

let remove_room t id =
  t.v_rooms <- String_map.remove (Id.Room_id.to_string id) t.v_rooms;
  touch t

let find_account_data t ty = String_map.find_opt ty t.v_account_data

let set_account_data t ty json =
  t.v_account_data <- String_map.add ty json t.v_account_data;
  touch t

let remove_account_data t ty =
  t.v_account_data <- String_map.remove ty t.v_account_data;
  touch t

let all_account_data t = String_map.bindings t.v_account_data
let receipts t id = String_map.find_opt (Id.Room_id.to_string id) t.v_receipts

let set_receipts t id r =
  t.v_receipts <- String_map.add (Id.Room_id.to_string id) r t.v_receipts;
  touch t

let remove_receipts t id =
  t.v_receipts <- String_map.remove (Id.Room_id.to_string id) t.v_receipts;
  touch t

(* A key that will not parse back cannot name a room this store knows about,
   so dropping it loses nothing that {!receipts} could have returned. *)
let all_receipts t =
  String_map.bindings t.v_receipts
  |> List.filter_map (fun (k, v) ->
      match Id.Room_id.of_string k with
      | Ok id -> Some (id, v)
      | Error _ -> None)

let profiles t =
  String_map.bindings t.v_profiles
  |> List.filter_map (fun (key, profile) ->
      match Id.User_id.of_string key with
      | Ok user_id -> Some (user_id, profile)
      | Error _ -> None)
  |> List.sort (fun (left, _) (right, _) -> Id.User_id.compare left right)

let replace_profiles t profiles =
  t.v_profiles <-
    List.fold_left
      (fun acc (user_id, profile) ->
        let profile = normalize_profile profile in
        String_map.add (Id.User_id.to_string user_id) profile acc)
      String_map.empty profiles;
  touch t

module Slot = struct
  type 'a key = { name : string; codec : 'a Jsont.t }

  let v ~name codec = { name; codec }

  let find t k =
    match String_map.find_opt k.name t.v_kv with
    | None -> Ok None
    | Some json -> (
        match Jsont.Json.decode k.codec json with
        | Ok v -> Ok (Some v)
        | Error msg -> Error (Error.Json_error msg))

  let set t k value =
    match Jsont.Json.encode k.codec value with
    | Error msg -> Error (Error.Json_error msg)
    | Ok json ->
        t.v_kv <- String_map.add k.name json t.v_kv;
        touch t;
        Ok ()

  let remove t k =
    t.v_kv <- String_map.remove k.name t.v_kv;
    touch t
end

let dirty t = t.v_dirty

let document t =
  {
    d_format_version = 4;
    d_next_batch = t.v_next_batch;
    d_sliding_pos = t.v_sliding_pos;
    d_sliding_to_device_since = t.v_sliding_to_device_since;
    d_sliding_lists = t.v_sliding_lists;
    d_rooms = rooms t;
    d_account_data = all_account_data t;
    d_receipts = String_map.bindings t.v_receipts;
    d_profiles = profiles t;
    d_kv = String_map.bindings t.v_kv;
  }

let flush t =
  match (t.root, t.v_dirty) with
  | None, _ | _, false -> Ok ()
  | Some root, true -> (
      let path = Eio.Path.(root / file_name) in
      match
        Jsont_bytesrw.encode_string ~format:Jsont.Indent document_jsont
          (document t)
      with
      | Error msg -> Error (Error.Json_error msg)
      | Ok data -> (
          let result =
            Profile_store.with_dir_lock root (fun () ->
                Io_context.with_context "flushing Matrix base state" (fun () ->
                    let current_fingerprint =
                      if Eio.Path.is_file path then
                        Some
                          Digestif.SHA256.(
                            digest_string (Eio.Path.load path) |> to_raw_string)
                      else None
                    in
                    if current_fingerprint <> t.v_fingerprint then
                      Error
                        (Error.Policy_denied
                           "base state changed since this store was opened")
                    else
                      (* Keep the last complete snapshot intact if the process
                         is stopped during a write. The shared atomic writer
                         syncs the file before this same-directory rename. *)
                      match Profile_store.atomic_write ~path ~data with
                      | Error _ as error -> error
                      | Ok () ->
                          t.v_fingerprint <-
                            Some
                              Digestif.SHA256.(
                                digest_string data |> to_raw_string);
                          t.v_dirty <- false;
                          Ok ()))
          in
          match result with Ok result -> result | Error error -> Error error))

let clear t =
  t.v_next_batch <- None;
  t.v_sliding_pos <- None;
  t.v_sliding_to_device_since <- None;
  t.v_sliding_lists <- [];
  t.v_rooms <- String_map.empty;
  t.v_account_data <- String_map.empty;
  t.v_receipts <- String_map.empty;
  t.v_profiles <- String_map.empty;
  t.v_kv <- String_map.empty;
  touch t
