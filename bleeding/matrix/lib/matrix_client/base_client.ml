module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Proto_sync = Matrix_proto.Sync
module Proto_sliding = Matrix_proto.Sliding_sync
module Legacy_sliding = Sliding_sync_state
open Matrix_proto.Json
module String_map = Map.Make (String)
module String_set = Set.Make (String)

let src = Logs.Src.create "matrix.base_client" ~doc:"Folding /sync into state"

module Log = (val Logs.src_log src : Logs.LOG)

type membership = Store.membership = Joined | Invited | Left | Knocked

(* Repeated from {!Store} so that the field labels are in scope here. The
   equality is checked, so the two cannot drift. *)
type room_info = Store.room_info = {
  room_id : Id.Room_id.t;
  membership : membership;
  name : string option;
  canonical_alias : Id.Room_alias.t option;
  topic : string option;
  avatar_url : string option;
  encryption : Jsont.json option;
  heroes : Store.hero list;
  joined_member_count : int;
  invited_member_count : int;
  is_dm : bool;
  display_name : Store.display_name;
  notification_count : int;
  highlight_count : int;
  local_unread_count : int;
  local_notification_count : int;
  local_highlight_count : int;
  marked_unread : bool;
  marked_unread_source : Store.marked_unread_source;
  latest_event : Event.Raw_event.t option;
  prev_batch : string option;
  tags : (string * float option) list;
  last_active_ts : int64;
  recency_stamp : int option;
  state_events : Store.state_event list;
  state_completeness : Store.state_completeness;
  members_complete : bool;
  encryption_state_complete : bool;
}

let display_name (i : room_info) = Store.display_name_to_string i.display_name

(* An account-data or ephemeral event as ["type"] and ["content"]. *)
let typed_events events =
  List.filter_map
    (fun j ->
      match (find_string "type" j, find_mem "content" j) with
      | Some ty, Some content -> Some (ty, content)
      | Some ty, None -> Some (ty, Jsont.Json.object' [])
      | None, _ -> None)
    events

(* A state event, from either a full or a stripped representation. *)
type se = {
  se_type : string;
  se_key : string;
  se_content : Jsont.json;
  se_sender : Id.User_id.t option;
  se_event_id : Id.Event_id.t option;
  se_origin_server_ts : Event.Timestamp.t option;
}

let se_of_raw (e : Event.Raw_event.t) =
  match e.state_key with
  | None -> None
  | Some se_key ->
      Some
        {
          se_type = Event.Event_type.to_string e.type_;
          se_key;
          se_content = e.content;
          se_sender = Some e.sender;
          se_event_id = e.event_id;
          se_origin_server_ts = Some e.origin_server_ts;
        }

let se_of_stripped (e : Event.Stripped_event.t) =
  {
    se_type = Event.Event_type.to_string e.type_;
    se_key = e.state_key;
    se_content = e.content;
    se_sender = Some e.sender;
    se_event_id = None;
    se_origin_server_ts = None;
  }

let state_event_of_se e : Store.state_event =
  {
    event_type = Event.Event_type.of_string e.se_type;
    state_key = e.se_key;
    content = e.se_content;
    sender = e.se_sender;
    event_id = e.se_event_id;
    origin_server_ts = e.se_origin_server_ts;
  }

let compare_state_event (a : Store.state_event) (b : Store.state_event) =
  match
    String.compare
      (Event.Event_type.to_string a.event_type)
      (Event.Event_type.to_string b.event_type)
  with
  | 0 -> String.compare a.state_key b.state_key
  | n -> n

let update_state_events previous deltas =
  List.fold_left
    (fun events delta ->
      let event = state_event_of_se delta in
      event
      :: List.filter
           (fun (old : Store.state_event) ->
             not
               (Event.Event_type.equal old.event_type event.event_type
               && String.equal old.state_key event.state_key))
           events)
    previous deltas
  |> List.sort compare_state_event

let active_members_of_state state_events =
  List.fold_left
    (fun set (e : Store.state_event) ->
      if String.equal (Event.Event_type.to_string e.event_type) "m.room.member"
      then
        match find_string "membership" e.content with
        | Some ("join" | "invite") -> String_set.add e.state_key set
        | Some _ -> String_set.remove e.state_key set
        | None -> set
      else set)
    String_set.empty state_events

let power_levels_of_state state_events =
  List.find_map
    (fun (e : Store.state_event) ->
      if
        String.equal
          (Event.Event_type.to_string e.event_type)
          "m.room.power_levels"
        && String.equal e.state_key ""
      then Some (Push_evaluator.Power_levels.of_json e.content)
      else None)
    state_events

type state = {
  s_user_id : Id.User_id.t;
  s_plaintext_policy : Store.plaintext_policy;
  s_display_name : string;
  s_ruleset : Matrix_proto.Push.Ruleset.t;
  s_next_batch : string option;
  s_sliding_pos : string option;
  s_sliding_to_device_since : string option;
  s_sliding_lists : (string * int) list;
  s_rooms : room_info String_map.t;
  s_account_data : Jsont.json String_map.t;
  s_receipts : Read_state.t String_map.t;
  s_presence : Jsont.json list;
  s_power_levels : Push_evaluator.Power_levels.t String_map.t;
  s_direct : String_set.t;
  s_members : String_set.t String_map.t;
  s_profiles : Store.profile String_map.t;
}

type profile_change = {
  changed_user_id : Id.User_id.t;
  previous_profile : Store.profile option;
  current_profile : Store.profile option;
}

let normalize_profile profile =
  List.fold_left
    (fun fields (name, value) -> String_map.add name value fields)
    String_map.empty profile
  |> String_map.bindings

let profile_equal left right =
  List.equal
    (fun (left_name, left_value) (right_name, right_value) ->
      String.equal left_name right_name
      && Jsont.Json.equal left_value right_value)
    left right

let profile_bindings profiles =
  String_map.bindings profiles
  |> List.filter_map (fun (key, profile) ->
      match Id.User_id.of_string key with
      | Ok user_id -> Some (user_id, profile)
      | Error _ -> None)
  |> List.sort (fun (left, _) (right, _) -> Id.User_id.compare left right)

type state_coverage = {
  state_complete : bool;
  members_complete : bool;
  encryption_state_complete : bool;
}

let unknown_state_coverage =
  {
    state_complete = false;
    members_complete = false;
    encryption_state_complete = false;
  }

let complete_state_coverage =
  {
    state_complete = true;
    members_complete = true;
    encryption_state_complete = true;
  }

let create ~user_id ?display_name ?ruleset
    ?(plaintext_policy = Store.Ciphertext_only) () =
  {
    s_user_id = user_id;
    s_plaintext_policy = plaintext_policy;
    s_display_name =
      (match display_name with
      | Some d -> d
      | None -> Id.User_id.localpart user_id);
    s_ruleset =
      (match ruleset with
      | Some r -> r
      | None -> Matrix_proto.Push.default_ruleset ~user_id);
    s_next_batch = None;
    s_sliding_pos = None;
    s_sliding_to_device_since = None;
    s_sliding_lists = [];
    s_rooms = String_map.empty;
    s_account_data = String_map.empty;
    s_receipts = String_map.empty;
    s_presence = [];
    s_power_levels = String_map.empty;
    s_direct = String_set.empty;
    s_members = String_map.empty;
    s_profiles = String_map.empty;
  }

(* Push rules arrive as global account data. Keep the active ruleset in the
   pure state, but derive it from the stored event when a state is restored so
   callers do not have to fetch and install the rules twice. A malformed event
   is deliberately non-fatal. Rust falls back to server defaults when its
   stored value is absent or malformed; preserving an explicitly supplied
   in-memory fallback here is an OCaml API guarantee. *)
let ruleset_from_account_data ~fallback account_data =
  match String_map.find_opt "m.push_rules" account_data with
  | None -> fallback
  | Some content -> (
      match
        Jsont.Json.decode Matrix_proto.Push.Ruleset.global_jsont content
      with
      | Ok ruleset -> ruleset
      | Error error ->
          Log.warn (fun m ->
              m "Ignoring malformed stored m.push_rules account data: %s" error);
          fallback)

(* [m.direct] maps a user id to the rooms that are a DM with them; only the
   set of rooms matters here. *)
let direct_rooms content =
  match as_object content with
  | None -> String_set.empty
  | Some o ->
      List.fold_left
        (fun acc (_, v) ->
          match as_array v with
          | None -> acc
          | Some items ->
              List.fold_left
                (fun acc i ->
                  match as_string i with
                  | Some s -> String_set.add s acc
                  | None -> acc)
                acc items)
        String_set.empty o

let of_store store ~user_id ?display_name ?ruleset () =
  let st =
    create ~user_id ?display_name ?ruleset
      ~plaintext_policy:(Store.plaintext_policy store)
      ()
  in
  let s_rooms =
    List.fold_left
      (fun m (r : room_info) ->
        String_map.add (Id.Room_id.to_string r.room_id) r m)
      String_map.empty (Store.rooms store)
  in
  let s_account_data =
    List.fold_left
      (fun m (k, v) -> String_map.add k v m)
      String_map.empty
      (Store.all_account_data store)
  in
  let s_receipts =
    List.fold_left
      (fun m (id, v) -> String_map.add (Id.Room_id.to_string id) v m)
      String_map.empty (Store.all_receipts store)
  in
  let s_direct =
    match String_map.find_opt "m.direct" s_account_data with
    | Some j -> direct_rooms j
    | None -> String_set.empty
  in
  let s_members =
    String_map.map
      (fun room -> active_members_of_state room.state_events)
      s_rooms
  in
  let s_power_levels =
    String_map.fold
      (fun room_id room acc ->
        match power_levels_of_state room.state_events with
        | None -> acc
        | Some levels -> String_map.add room_id levels acc)
      s_rooms String_map.empty
  in
  let s_profiles =
    List.fold_left
      (fun profiles (user_id, profile) ->
        String_map.add
          (Id.User_id.to_string user_id)
          (normalize_profile profile)
          profiles)
      String_map.empty (Store.profiles store)
  in
  {
    st with
    s_ruleset = ruleset_from_account_data ~fallback:st.s_ruleset s_account_data;
    s_next_batch = Store.next_batch store;
    s_sliding_pos = Store.sliding_pos store;
    s_sliding_to_device_since = Store.sliding_to_device_since store;
    s_sliding_lists = Store.sliding_lists store;
    s_rooms;
    s_account_data;
    s_receipts;
    s_direct;
    s_members;
    s_power_levels;
    s_profiles;
  }

let persist store st =
  if Store.plaintext_policy store <> st.s_plaintext_policy then
    invalid_arg "Base_client.persist: store and state plaintext policies differ";
  (match st.s_next_batch with
  | Some b -> Store.set_next_batch store b
  | None -> ());
  Store.replace_sliding_session store ~pos:st.s_sliding_pos
    ~to_device_since:st.s_sliding_to_device_since ~lists:st.s_sliding_lists;
  String_map.iter (fun _ r -> Store.set_room store r) st.s_rooms;
  String_map.iter
    (fun k v -> Store.set_account_data store k v)
    st.s_account_data;
  String_map.iter
    (fun k v ->
      match Id.Room_id.of_string k with
      | Ok id -> Store.set_receipts store id v
      | Error _ -> ())
    st.s_receipts;
  Store.replace_profiles store (profile_bindings st.s_profiles)

let with_ruleset st r = { st with s_ruleset = r }

(* Install the server's complete push-rules event as durable account data as
   well as making it active.  [with_ruleset] intentionally remains a fallback
   override for callers that do not have an account-data event; bootstrap code
   must use this transition so a restart does not fetch the rules again. *)
let with_push_rules st ruleset =
  let content =
    match Jsont.Json.encode Matrix_proto.Push.Ruleset.global_jsont ruleset with
    | Ok content -> content
    | Error error ->
        invalid_arg (Printf.sprintf "Base_client.with_push_rules: %s" error)
  in
  {
    st with
    s_ruleset = ruleset;
    s_account_data = String_map.add "m.push_rules" content st.s_account_data;
  }

let with_display_name st d = { st with s_display_name = d }
let user_id st = st.s_user_id
let next_batch st = st.s_next_batch
let sliding_pos st = st.s_sliding_pos
let sliding_to_device_since st = st.s_sliding_to_device_since
let sliding_lists st = st.s_sliding_lists

let reset_sliding_session st =
  {
    st with
    s_sliding_pos = None;
    s_sliding_to_device_since = None;
    s_sliding_lists = [];
  }

let ruleset st = st.s_ruleset

let rooms st =
  String_map.bindings st.s_rooms
  |> List.map snd
  |> List.sort (fun (a : room_info) b ->
      match (a.recency_stamp, b.recency_stamp) with
      | Some left, Some right when left <> right -> Int.compare right left
      | Some _, None -> -1
      | None, Some _ -> 1
      | _ -> compare b.last_active_ts a.last_active_ts)

let find_room st id = String_map.find_opt (Id.Room_id.to_string id) st.s_rooms

let forget_room st room_id =
  let key = Id.Room_id.to_string room_id in
  {
    st with
    s_rooms = String_map.remove key st.s_rooms;
    s_receipts = String_map.remove key st.s_receipts;
    s_power_levels = String_map.remove key st.s_power_levels;
    s_members = String_map.remove key st.s_members;
  }

(* Keep the local account-data projection in step with a successful remote
   [m.direct] update. Do this on raw JSON so malformed or unknown entries are
   preserved, and forgetting one room cannot discard unrelated associations. *)
let remove_direct_room st room_id =
  let key = Id.Room_id.to_string room_id in
  match String_map.find_opt "m.direct" st.s_account_data with
  | None -> { st with s_direct = String_set.remove key st.s_direct }
  | Some content -> (
      match as_object content with
      | None -> st
      | Some bindings ->
          let changed = ref false in
          let bindings =
            List.filter_map
              (fun (name, value) ->
                match as_array value with
                | None -> Some (name, value)
                | Some rooms ->
                    let rooms' =
                      List.filter (fun room -> as_string room <> Some key) rooms
                    in
                    if List.length rooms' <> List.length rooms then
                      changed := true;
                    if rooms' = [] then None
                    else Some (name, Jsont.Json.list rooms'))
              bindings
          in
          if not !changed then st
          else
            {
              st with
              s_account_data =
                String_map.add "m.direct"
                  (Jsont.Json.object' bindings)
                  st.s_account_data;
              s_direct = String_set.remove key st.s_direct;
            })

let inviter st room_id =
  match find_room st room_id with
  | Some { membership = Invited; state_events; _ } ->
      List.find_map
        (fun (event : Store.state_event) ->
          if
            Event.Event_type.equal event.event_type Event.Event_type.Room_member
            && String.equal event.state_key (Id.User_id.to_string st.s_user_id)
            && find_string "membership" event.content = Some "invite"
          then event.sender
          else None)
        state_events
  | Some _ | None -> None

let rooms_with st m =
  List.filter (fun (r : room_info) -> r.membership = m) (rooms st)

let find_account_data st ty = String_map.find_opt ty st.s_account_data
let all_account_data st = String_map.bindings st.s_account_data
let profiles st = profile_bindings st.s_profiles

let find_profile st user_id =
  String_map.find_opt (Id.User_id.to_string user_id) st.s_profiles

let find_profile_field st user_id field =
  Option.bind (find_profile st user_id) (List.assoc_opt field)

let apply_profile_updates st
    (updates : Matrix_proto.Sliding_sync.Response.profiles) =
  let touched =
    List.fold_left
      (fun touched (user_id, _) ->
        String_map.add (Id.User_id.to_string user_id) user_id touched)
      String_map.empty updates.users
  in
  let s_profiles =
    List.fold_left
      (fun profiles (user_id, update) ->
        let key = Id.User_id.to_string user_id in
        match update with
        | Matrix_proto.Sliding_sync.Response.Dropped ->
            String_map.remove key profiles
        | Matrix_proto.Sliding_sync.Response.Updated fields ->
            let previous =
              Option.value (String_map.find_opt key profiles) ~default:[]
            in
            let profile =
              List.fold_left
                (fun profile (name, value) ->
                  match value with
                  | Jsont.Null _ -> String_map.remove name profile
                  | _ -> String_map.add name value profile)
                (List.to_seq previous |> String_map.of_seq)
                fields
              |> String_map.bindings
            in
            String_map.add key profile profiles)
      st.s_profiles updates.users
  in
  let profile_changes =
    String_map.bindings touched
    |> List.filter_map (fun (key, user_id) ->
        let previous_profile = String_map.find_opt key st.s_profiles in
        let current_profile = String_map.find_opt key s_profiles in
        if Option.equal profile_equal previous_profile current_profile then None
        else
          Some { changed_user_id = user_id; previous_profile; current_profile })
    |> List.sort (fun left right ->
        Id.User_id.compare left.changed_user_id right.changed_user_id)
  in
  match profile_changes with
  | [] -> (st, [])
  | _ -> ({ st with s_profiles }, profile_changes)

let receipts st id =
  match String_map.find_opt (Id.Room_id.to_string id) st.s_receipts with
  | Some r -> r
  | None -> Read_state.empty

let counts_equal left right =
  left.Read_state.unread = right.Read_state.unread
  && left.notifications = right.notifications
  && left.highlights = right.highlights

let with_local_unread_counts st ~room_id counts =
  let key = Id.Room_id.to_string room_id in
  match String_map.find_opt key st.s_rooms with
  | None -> st
  | Some info ->
      let current_counts : Read_state.counts =
        {
          unread = info.local_unread_count;
          notifications = info.local_notification_count;
          highlights = info.local_highlight_count;
        }
      in
      if counts_equal current_counts counts then st
      else
        {
          st with
          s_rooms =
            String_map.add key
              {
                info with
                local_unread_count = counts.unread;
                local_notification_count = counts.notifications;
                local_highlight_count = counts.highlights;
              }
              st.s_rooms;
        }

let presence st = st.s_presence

let members st id =
  match String_map.find_opt (Id.Room_id.to_string id) st.s_members with
  | None -> []
  | Some set ->
      String_set.elements set
      |> List.filter_map (fun u -> Result.to_option (Id.User_id.of_string u))

let state_events st id =
  match String_map.find_opt (Id.Room_id.to_string id) st.s_rooms with
  | None -> []
  | Some room -> room.state_events

let find_state_event st id ~event_type ?state_key () =
  Option.bind
    (String_map.find_opt (Id.Room_id.to_string id) st.s_rooms)
    (fun room -> Store.find_state_event room ~event_type ?state_key ())

let retention (room : room_info) =
  let event_type = Event.Event_type.Room_retention in
  let legacy_type = Event.Event_type.Room_retention_unstable in
  let decode = function
    | None -> None
    | Some (event : Store.state_event) ->
        Result.to_option
          (Jsont.Json.decode Retention.policy_jsont event.content)
  in
  match decode (Store.find_state_event room ~event_type ()) with
  | Some _ as retention -> retention
  | None -> decode (Store.find_state_event room ~event_type:legacy_type ())

let ids_in_content member content =
  match Option.bind (find_mem member content) as_array with
  | None -> []
  | Some values ->
      List.filter_map
        (fun value ->
          Option.bind (as_string value) (fun s ->
              Result.to_option (Id.User_id.of_string s)))
        values

let service_members (info : room_info) =
  let find ty =
    List.find_opt
      (fun (e : Store.state_event) ->
        String.equal (Event.Event_type.to_string e.event_type) ty
        && String.equal e.state_key "")
      info.state_events
  in
  let event =
    match find "m.room.member_hints" with
    | Some _ as stable -> stable
    | None -> find "io.element.functional_members"
  in
  match event with
  | None -> []
  | Some e ->
      let first_nonempty names =
        List.find_map
          (fun name ->
            match ids_in_content name e.content with
            | [] -> None
            | ids -> Some ids)
          names
      in
      Option.value
        (first_nonempty [ "service_members"; "functional_members"; "members" ])
        ~default:[]

let human_members st id =
  let services =
    match find_room st id with
    | None -> String_set.empty
    | Some room ->
        List.fold_left
          (fun set user -> String_set.add (Id.User_id.to_string user) set)
          String_set.empty (service_members room)
  in
  List.filter
    (fun user -> not (String_set.mem (Id.User_id.to_string user) services))
    (members st id)

let active_service_member_count (info : room_info) =
  let active = active_members_of_state info.state_events in
  List.fold_left
    (fun count user ->
      if String_set.mem (Id.User_id.to_string user) active then count + 1
      else count)
    0 (service_members info)

let human_member_count info =
  max 0
    (info.joined_member_count + info.invited_member_count
    - active_service_member_count info)

(* Push rules use the account's membership display name in this room, not its
   global profile name.  This mirrors matrix-rust-sdk's push context: an
   explicit member event without [displayname] falls back to the user-id
   localpart.  Keep the constructor-supplied name only for partial rooms where
   no own membership state has arrived yet. *)
let own_display_name st (info : room_info) =
  let own_user = Id.User_id.to_string st.s_user_id in
  match
    List.find_opt
      (fun (event : Store.state_event) ->
        Event.Event_type.equal event.event_type Event.Event_type.Room_member
        && String.equal event.state_key own_user)
      info.state_events
  with
  | None -> st.s_display_name
  | Some event ->
      Option.value
        (find_string "displayname" event.content)
        ~default:(Id.User_id.localpart st.s_user_id)

let push_context st room_id =
  let key = Id.Room_id.to_string room_id in
  let info = String_map.find_opt key st.s_rooms in
  let member_count =
    match info with Some r -> r.joined_member_count | None -> 0
  in
  let display_name =
    match info with
    | Some room -> own_display_name st room
    | None -> st.s_display_name
  in
  Push_evaluator.Context.v ~user_id:st.s_user_id ~room_id ~display_name
    ~member_count
    ?power_levels:(String_map.find_opt key st.s_power_levels)
    ()

let compute_display_name ~user_id (i : room_info) =
  match i.name with
  | Some n when String.trim n <> "" -> Store.Named (String.trim n)
  | _ -> (
      match i.canonical_alias with
      | Some a -> Store.Aliased (Id.Room_alias.to_string a)
      | None ->
          let me = Id.User_id.to_string user_id in
          let services =
            List.fold_left
              (fun set user -> String_set.add (Id.User_id.to_string user) set)
              String_set.empty (service_members i)
          in
          let names =
            i.heroes
            |> List.filter (fun (h : Store.hero) ->
                let user = Id.User_id.to_string h.user_id in
                (not (String.equal user me))
                && not (String_set.mem user services))
            |> List.map (fun (h : Store.hero) ->
                match h.display_name with
                | Some d when String.trim d <> "" -> String.trim d
                | _ -> Id.User_id.to_string h.user_id)
            |> List.sort String.compare
          in
          let num_heroes = List.length names in
          let num_joined_invited =
            if i.membership = Invited then num_heroes + 1
            else human_member_count i
          in
          let except_self = max 0 (num_joined_invited - 1) in
          let joined = String.concat ", " names in
          let rendered =
            if num_heroes = 0 && num_joined_invited > 1 then
              Printf.sprintf "%d people" num_joined_invited
            else if num_heroes >= except_self then joined
            else if num_joined_invited > 1 then
              Printf.sprintf "%s, and %d others" joined
                (num_joined_invited - num_heroes)
            else ""
          in
          if num_joined_invited <= 1 then
            if rendered = "" then Store.Empty else Store.Empty_was rendered
          else Store.Calculated rendered)

let member_content (member : Rooms.member) =
  let string_mem name value =
    Jsont.Json.mem (Jsont.Json.name name) (Jsont.Json.string value)
  in
  let optional name f = function
    | None -> []
    | Some value -> [ string_mem name (f value) ]
  in
  Jsont.Json.object'
    (string_mem "membership" (Event.Membership.to_string member.membership)
     :: optional "displayname" Fun.id member.display_name
    @ optional "avatar_url" Media.Mxc.to_string member.avatar_url)

(* [/members] is an authoritative snapshot. Keep its normalized current member
   events in the durable projection too: updating only [s_members] would make a
   restart silently turn the complete recipient set partial again. *)
let replace_members st room_id (members : Rooms.member list) =
  let key = Id.Room_id.to_string room_id in
  match String_map.find_opt key st.s_rooms with
  | None -> st
  | Some info ->
      let active, joined, invited, profiles, state_events =
        List.fold_left
          (fun (active, joined, invited, profiles, events) member ->
            let user = Id.User_id.to_string member.Rooms.user_id in
            let active, joined, invited =
              match member.membership with
              | Event.Membership.Join ->
                  (String_set.add user active, joined + 1, invited)
              | Event.Membership.Invite ->
                  (String_set.add user active, joined, invited + 1)
              | Event.Membership.Leave | Event.Membership.Ban
              | Event.Membership.Knock ->
                  (active, joined, invited)
            in
            let event : Store.state_event =
              {
                event_type = Event.Event_type.Room_member;
                state_key = user;
                content = member_content member;
                sender = None;
                event_id = None;
                origin_server_ts = None;
              }
            in
            ( active,
              joined,
              invited,
              String_map.add user
                ( member.display_name,
                  Option.map Media.Mxc.to_string member.avatar_url )
                profiles,
              event :: events ))
          (String_set.empty, 0, 0, String_map.empty, [])
          members
      in
      let non_member_state =
        List.filter
          (fun (event : Store.state_event) ->
            not
              (Event.Event_type.equal event.event_type
                 Event.Event_type.Room_member))
          info.state_events
      in
      let state_events =
        List.sort
          (fun (a : Store.state_event) b ->
            String.compare a.state_key b.state_key)
          state_events
        @ non_member_state
      in
      let heroes =
        List.map
          (fun (hero : Store.hero) ->
            match
              String_map.find_opt (Id.User_id.to_string hero.user_id) profiles
            with
            | None -> hero
            | Some (display_name, avatar_url) ->
                { hero with display_name; avatar_url })
          info.heroes
      in
      let info =
        {
          info with
          state_events;
          heroes;
          joined_member_count = joined;
          invited_member_count = invited;
          members_complete = info.membership = Joined;
        }
      in
      let info =
        {
          info with
          display_name = compute_display_name ~user_id:st.s_user_id info;
        }
      in
      {
        st with
        s_rooms = String_map.add key info st.s_rooms;
        s_members = String_map.add key active st.s_members;
      }

(* [m.room.encrypted] counts as a latest event even though its content
   cannot be rendered: a room list with no latest event at all in every
   encrypted room is worse than one showing a placeholder. *)

let latest_event_types =
  [
    "m.room.message";
    "m.room.encrypted";
    "m.sticker";
    "m.call.invite";
    "m.poll.start";
    "org.matrix.msc3381.poll.start";
  ]

let is_replacement (e : Event.Raw_event.t) =
  match
    Option.bind (find_mem "m.relates_to" e.content) (find_mem "rel_type")
  with
  | Some (Jsont.String ("m.replace", _)) -> true
  | _ -> false

let suitable_for_latest ~user_id (e : Event.Raw_event.t) =
  let ty = Event.Event_type.to_string e.type_ in
  if String.equal ty "m.room.member" then
    (* The user's own join or invite gives a new room its first entry. *)
    match e.state_key with
    | Some sk when String.equal sk (Id.User_id.to_string user_id) -> (
        match find_string "membership" e.content with
        | Some ("join" | "invite") -> true
        | _ -> false)
    | _ -> false
  else List.mem ty latest_event_types && not (is_replacement e)

(* [apply] takes a [?decrypt] function rather than an encryption machine, so
   that this module depends on none and a test can supply a table. The
   plaintext keeps the envelope the server sent — event id, sender,
   timestamp, room — and swaps in the type and content Megolm recovered,
   which is what makes an encrypted room's push rules evaluable. *)

let is_encrypted_event (e : Event.Raw_event.t) =
  String.equal (Event.Event_type.to_string e.type_) "m.room.encrypted"

let plaintext_of (e : Event.Raw_event.t) (d : Encryption.decrypted_event) =
  {
    e with
    Event.Raw_event.type_ = Event.Event_type.of_string d.decrypted_type;
    content = d.decrypted_content;
  }

type decrypted = {
  encrypted : Event.Raw_event.t;
  plaintext : Event.Raw_event.t;
  info : Encryption.decrypted_event;
}

type room_change = {
  changed_room_id : Id.Room_id.t;
  info : room_info;
  previous : room_info option;
  timeline : Event.Raw_event.t list;
  decrypted : decrypted list;
  undecrypted : (Event.Raw_event.t * Encryption.decrypt_error) list;
  state_events : Event.Raw_event.t list;
  ephemeral : Jsont.json list;
  room_account_data : (string * Jsont.json) list;
  limited : bool;
  unread : Read_state.counts;
}

type changes = {
  batch : string;
  room_changes : room_change list;
  profile_changes : profile_change list;
  global_account_data : (string * Jsont.json) list;
  to_device : Jsont.json list;
  device_lists : Proto_sync.Device_lists.t option;
  one_time_keys_count : (string * int) list;
  unused_fallback_key_types : string list option;
  presence_events : Jsont.json list;
}

(* Fold one state event into a room summary, and collect member profiles on the
   side for the hero computation. *)
type hero_profile = {
  hp_display_name : string option;
  hp_avatar_url : string option;
}

let apply_state_event (info, members, powers) e =
  let members =
    if String.equal e.se_type "m.room.member" then
      String_map.add e.se_key
        {
          hp_display_name = find_string "displayname" e.se_content;
          hp_avatar_url = find_string "avatar_url" e.se_content;
        }
        members
    else members
  in
  let info =
    match e.se_type with
    | "m.room.name" -> (
        match find_string "name" e.se_content with
        | Some "" -> { info with name = None }
        | Some n -> { info with name = Some n }
        | None -> { info with name = None })
    | "m.room.topic" -> { info with topic = find_string "topic" e.se_content }
    | "m.room.avatar" ->
        { info with avatar_url = find_string "url" e.se_content }
    | "m.room.canonical_alias" -> (
        match find_string "alias" e.se_content with
        | None -> { info with canonical_alias = None }
        | Some a -> (
            match Id.Room_alias.of_string a with
            | Ok alias -> { info with canonical_alias = Some alias }
            | Error _ -> { info with canonical_alias = None }))
    | "m.room.encryption" -> { info with encryption = Some e.se_content }
    | _ -> info
  in
  let powers =
    if String.equal e.se_type "m.room.power_levels" then
      Some (Push_evaluator.Power_levels.of_json e.se_content)
    else powers
  in
  (info, members, powers)

let tags_of_content content =
  match Option.bind (find_mem "tags" content) as_object with
  | None -> []
  | Some o ->
      List.map
        (fun ((name, _), v) ->
          (name, Option.bind (find_mem "order" v) as_float))
        o

let update_hero_from_members members (hero : Store.hero) =
  match String_map.find_opt (Id.User_id.to_string hero.user_id) members with
  | None -> hero
  | Some profile ->
      {
        hero with
        display_name = profile.hp_display_name;
        avatar_url = profile.hp_avatar_url;
      }

let heroes_of ~previous ~members (summary : Proto_sync.Room_summary.t option) =
  let previous = List.map (update_hero_from_members members) previous in
  match summary with
  | None -> previous
  | Some s -> (
      match s.heroes with
      | None -> previous
      | Some ids ->
          List.map
            (fun uid ->
              let key = Id.User_id.to_string uid in
              match String_map.find_opt key members with
              | Some profile ->
                  ({
                     user_id = uid;
                     display_name = profile.hp_display_name;
                     avatar_url = profile.hp_avatar_url;
                   }
                    : Store.hero)
              | None -> (
                  match
                    List.find_opt
                      (fun (hero : Store.hero) ->
                        String.equal (Id.User_id.to_string hero.user_id) key)
                      previous
                  with
                  | Some hero -> hero
                  | None ->
                      { user_id = uid; display_name = None; avatar_url = None }))
            ids)

let invited_is_direct ~user_id (info : room_info) =
  match
    Store.find_state_event info ~event_type:Event.Event_type.Room_member
      ~state_key:(Id.User_id.to_string user_id)
      ()
  with
  | None -> false
  | Some event ->
      Option.value
        (Option.bind (find_mem "is_direct" event.content) as_bool)
        ~default:false

type avatar_update = Keep_avatar | Remove_avatar | Replace_avatar of string

type room_input = {
  i_room_id : Id.Room_id.t;
  i_membership : membership;
  i_state : se list;
  i_raw_state : Event.Raw_event.t list;
  i_timeline : Event.Raw_event.t list;
  i_limited : bool;
  i_prev_batch : string option;
  i_clear_missing_prev_batch : bool;
  i_summary : Proto_sync.Room_summary.t option;
  i_unread : Proto_sync.Unread_notification_counts.t option;
  i_ephemeral : Jsont.json list;
  i_account_data : (string * Jsont.json) list;
  i_include_timeline_state : bool;
  i_state_is_partial : bool;
  i_members_incomplete : bool;
  i_num_live : int option;
  i_avatar : avatar_update;
  i_sliding_heroes : Store.hero list option;
  i_recency_stamp : int option;
}

let process_room ~decrypt ~coverage st
    (rooms, receipts_map, powers_map, members_map, acc) input =
  let key = Id.Room_id.to_string input.i_room_id in
  let previous = String_map.find_opt key rooms in
  let base =
    match previous with
    | Some p -> { p with membership = input.i_membership }
    | None ->
        Store.empty_room_info ~room_id:input.i_room_id
          ~membership:input.i_membership
  in
  (* State events first, then the ones the timeline carries: the timeline is
     the more recent of the two. *)
  let timeline_state =
    if input.i_include_timeline_state then
      List.filter_map se_of_raw input.i_timeline
    else []
  in
  let state_delta = input.i_state @ timeline_state in
  let state_is_authoritative =
    input.i_membership = Joined && coverage.state_complete
  in
  let base =
    (* A full-state response is a snapshot, not a delta. Keeping an event that
       is absent from it would turn a deleted/redacted state key into current
       state after a refresh or restart. Projection-derived summary fields are
       reset for the same reason and rebuilt from [state_delta] below. *)
    let previous_state =
      if state_is_authoritative then [] else base.state_events
    in
    let base =
      if state_is_authoritative then
        {
          base with
          name = None;
          canonical_alias = None;
          topic = None;
          avatar_url = None;
          encryption = None;
        }
      else base
    in
    let has_any_state = previous_state <> [] || state_delta <> [] in
    let state_completeness =
      match input.i_membership with
      (* Invite and knock state is stripped, and a leave carries only a
         partial historical view, so none of these establishes a complete
         current-state projection. *)
      | Invited | Knocked | Left ->
          if has_any_state then Store.Partial else Store.No_state
      | Joined when coverage.state_complete -> Store.Complete
      | Joined -> (
          match base.state_completeness with
          | Store.No_state when state_delta = [] -> Store.No_state
          | Store.No_state -> Store.Partial
          | known -> known)
    in
    let state_completeness =
      if input.i_state_is_partial then
        if previous_state = [] && state_delta = [] then Store.No_state
        else Store.Partial
      else state_completeness
    in
    let members_complete =
      match input.i_membership with
      | Joined when coverage.members_complete -> true
      | Joined -> (
          match previous with
          | None -> false
          | Some p when p.membership <> Joined -> false
          | Some p -> p.members_complete)
      | Invited | Left | Knocked -> false
    in
    let members_complete =
      if input.i_members_incomplete then false else members_complete
    in
    let encryption_in_delta =
      List.exists
        (fun e -> String.equal e.se_type "m.room.encryption")
        state_delta
    in
    (* A leave, invite, or knock does not carry authoritative current state,
       and its projection must not keep claiming that encryption absence or
       presence was established while joined. An explicit encryption event is
       still useful evidence even in such a partial response. *)
    let encryption_state_complete =
      if encryption_in_delta then true
      else
        match input.i_membership with
        | Joined ->
            base.encryption_state_complete || coverage.encryption_state_complete
        | Invited | Knocked | Left -> false
    in
    {
      base with
      state_events = update_state_events previous_state state_delta;
      state_completeness;
      members_complete;
      encryption_state_complete;
    }
  in
  (* Room membership, for a caller that has to name everybody who should be
     able to read an encrypted message. Kept as the join/invite set; a leave
     or a ban takes the member out again. *)
  let member_set =
    List.fold_left
      (fun set e ->
        if String.equal e.se_type "m.room.member" then
          match find_string "membership" e.se_content with
          | Some ("join" | "invite") -> String_set.add e.se_key set
          | Some _ -> String_set.remove e.se_key set
          | None -> set
        else set)
      (if state_is_authoritative then String_set.empty
       else
         Option.value
           (String_map.find_opt key members_map)
           ~default:String_set.empty)
      state_delta
  in
  let members_map = String_map.add key member_set members_map in
  (* Decryption comes before everything that reads a timeline event's type or
     content, so that an encrypted room's push rules see the plaintext. The
     raw event is what [room_change.timeline] and [room_info.latest_event]
     keep: only this batch's plaintext is handed out, and none of it is
     written to the store. *)
  let decrypted, undecrypted, plain_timeline =
    match decrypt with
    | None -> ([], [], input.i_timeline)
    | Some f ->
        List.fold_left
          (fun (dec, undec, plain) e ->
            if not (is_encrypted_event e) then (dec, undec, e :: plain)
            else
              match f input.i_room_id e with
              | Ok d ->
                  let p = plaintext_of e d in
                  ( { encrypted = e; plaintext = p; info = d } :: dec,
                    undec,
                    p :: plain )
              | Error err -> (dec, (e, err) :: undec, e :: plain))
          ([], [], []) input.i_timeline
        |> fun (d, u, p) -> (List.rev d, List.rev u, List.rev p)
  in
  let info, members, powers =
    List.fold_left apply_state_event
      ( base,
        String_map.empty,
        if state_is_authoritative then None
        else String_map.find_opt key powers_map )
      state_delta
  in
  let info =
    match input.i_summary with
    | None -> info
    | Some s ->
        {
          info with
          joined_member_count =
            Option.value s.joined_member_count ~default:info.joined_member_count;
          invited_member_count =
            Option.value s.invited_member_count
              ~default:info.invited_member_count;
        }
  in
  let info =
    {
      info with
      heroes = heroes_of ~previous:info.heroes ~members input.i_summary;
    }
  in
  let info =
    match input.i_sliding_heroes with
    | None -> info
    | Some heroes -> { info with heroes }
  in
  let info =
    match input.i_unread with
    | None -> info
    | Some u ->
        {
          info with
          notification_count =
            Option.value u.notification_count ~default:info.notification_count;
          highlight_count =
            Option.value u.highlight_count ~default:info.highlight_count;
        }
  in
  let info =
    match input.i_prev_batch with
    | None when input.i_clear_missing_prev_batch ->
        { info with prev_batch = None }
    | None -> info
    | Some _ as pb -> { info with prev_batch = pb }
  in
  let info =
    match input.i_avatar with
    | Keep_avatar -> info
    | Remove_avatar -> { info with avatar_url = None }
    | Replace_avatar avatar_url -> { info with avatar_url = Some avatar_url }
  in
  let info =
    match input.i_recency_stamp with
    | None -> info
    | Some recency_stamp -> { info with recency_stamp = Some recency_stamp }
  in
  let info =
    List.fold_left
      (fun info (ty, content) ->
        if String.equal ty "m.tag" then
          { info with tags = tags_of_content content }
        else info)
      info input.i_account_data
  in
  (* Both the stable [m.marked_unread] (MSC2867) and the older
     [com.famedly.marked_unread] spelling are honoured. The stable type wins
     within a response and, once seen, wins over unstable events in later
     responses as well, matching matrix-rust-sdk. *)
  let marked_unread_in_batch =
    List.fold_left
      (fun current (ty, content) ->
        match ty with
        | "com.famedly.marked_unread" when current = None ->
            Some
              ( Option.value (find_bool "unread" content) ~default:false,
                Store.Unstable )
        | "m.marked_unread" ->
            Some
              ( Option.value (find_bool "unread" content) ~default:false,
                Store.Stable )
        | _ -> current)
      None input.i_account_data
  in
  (* Receipts: the ephemeral m.receipt events, plus m.fully_read from the
     room's account data. *)
  let old_receipts =
    match String_map.find_opt key receipts_map with
    | Some r -> r
    | None -> Read_state.empty
  in
  let new_receipts =
    Read_state.ingest_ephemeral ~user_id:st.s_user_id old_receipts
      input.i_ephemeral
  in
  let new_receipts =
    List.fold_left
      (fun r (ty, content) ->
        if String.equal ty "m.fully_read" then
          Read_state.ingest_fully_read r
            (Jsont.Json.object'
               [
                 Jsont.Json.mem (Jsont.Json.name "type")
                   (Jsont.Json.string "m.fully_read");
                 Jsont.Json.mem (Jsont.Json.name "content") content;
               ])
        else r)
      new_receipts input.i_account_data
  in
  (* Thread receipts advance a separate read horizon. They must not reset the
     room-wide unread accumulator or clear [m.marked_unread]; only a change to
     the main public/private receipt or fully-read marker has that meaning. *)
  let main_receipts_changed =
    Read_state.public_read new_receipts <> Read_state.public_read old_receipts
    || Read_state.private_read new_receipts
       <> Read_state.private_read old_receipts
    || Read_state.fully_read new_receipts <> Read_state.fully_read old_receipts
  in
  (* Local unread counts. The push context uses the counts this response
     just established, so a room that grew to three members stops matching
     the one-to-one rules straight away. It also uses the own membership event
     folded above, so per-room display names affect this same batch. *)
  let ctx =
    Push_evaluator.Context.v ~user_id:st.s_user_id ~room_id:input.i_room_id
      ~display_name:(own_display_name st info)
      ~member_count:info.joined_member_count ?power_levels:powers ()
  in
  let notification e =
    Push_evaluator.notification_for_event st.s_ruleset ctx e
  in
  let unread_timeline =
    match input.i_num_live with
    | None -> plain_timeline
    | Some count ->
        let count = max 0 count in
        let length = List.length plain_timeline in
        if count >= length then plain_timeline
        else
          List.filteri (fun index _ -> index >= length - count) plain_timeline
  in
  let unread =
    Read_state.count_unread ~user_id:st.s_user_id ~notification new_receipts
      unread_timeline
  in
  let carry =
    if main_receipts_changed || input.i_limited || previous = None then
      Read_state.zero_counts
    else
      {
        Read_state.unread = info.local_unread_count;
        notifications = info.local_notification_count;
        highlights = info.local_highlight_count;
      }
  in
  let info =
    {
      info with
      local_unread_count = carry.unread + unread.unread;
      local_notification_count = carry.notifications + unread.notifications;
      local_highlight_count = carry.highlights + unread.highlights;
      (* [room_receipts] holds the user's own receipts only, so
         [main_receipts_changed] means the user has read further in this room,
         which is what clears the flag. An explicit flag in the same response
         wins: it is the newer statement of intent. *)
      marked_unread =
        (match marked_unread_in_batch with
        | Some (flag, Store.Unstable)
          when info.marked_unread_source = Store.Stable ->
            info.marked_unread && not main_receipts_changed
        | Some (flag, _) -> flag
        | None -> info.marked_unread && not main_receipts_changed);
      marked_unread_source =
        (match marked_unread_in_batch with
        | Some (_, Store.Unstable) when info.marked_unread_source = Store.Stable
          ->
            info.marked_unread_source
        | Some (_, source) -> source
        | None -> info.marked_unread_source);
    }
  in
  let info =
    List.fold_left2
      (fun info (e : Event.Raw_event.t) (p : Event.Raw_event.t) ->
        let ts = Event.Timestamp.to_ms e.origin_server_ts in
        let info =
          if ts > info.last_active_ts then { info with last_active_ts = ts }
          else info
        in
        (* Suitability is judged on the plaintext. The selected store policy
           then chooses whether the room summary keeps the opened event or the
           ciphertext envelope. *)
        if suitable_for_latest ~user_id:st.s_user_id p then
          {
            info with
            latest_event =
              Some
                (match st.s_plaintext_policy with
                | Store.Store_plaintext -> p
                | Store.Ciphertext_only -> e);
          }
        else info)
      info input.i_timeline plain_timeline
  in
  let info =
    let is_dm =
      match input.i_membership with
      | Joined | Left -> String_set.mem key st.s_direct
      | Invited -> invited_is_direct ~user_id:st.s_user_id info
      | Knocked -> false
    in
    {
      info with
      (* Joined and left rooms follow global [m.direct]. Invites instead use
         the authenticated user's stripped membership event, and knocks are
         never considered direct until they become an invite. *)
      is_dm;
    }
  in
  let info =
    { info with display_name = compute_display_name ~user_id:st.s_user_id info }
  in
  let change =
    {
      changed_room_id = input.i_room_id;
      info;
      previous;
      timeline = input.i_timeline;
      decrypted;
      undecrypted;
      state_events = input.i_raw_state;
      ephemeral = input.i_ephemeral;
      room_account_data = input.i_account_data;
      limited = input.i_limited;
      unread;
    }
  in
  ( String_map.add key info rooms,
    String_map.add key new_receipts receipts_map,
    (match powers with
    | Some p -> String_map.add key p powers_map
    | None when state_is_authoritative -> String_map.remove key powers_map
    | None -> powers_map),
    members_map,
    change :: acc )

(* A room key the server sent that does not parse as a room id names a room
   this client cannot address anyway. It is dropped rather than failing the
   whole response, but silently vanishing from the client's view of its own
   room list is worth a log line. *)
let room_id_of s =
  match Id.Room_id.of_string s with
  | Ok _ as ok -> ok
  | Error (`Msg msg) as e ->
      Log.warn (fun m -> m "Dropping room %S from /sync: %s" s msg);
      e

(* [m.direct] is global account data, so one update can change joined or left
   rooms omitted from this response. Invited rooms derive directness from their
   own stripped membership event, while knocked rooms are never direct.
   Reconcile the applicable known rooms and synthesize an otherwise-empty room
   change for consumers which project the room list. A room already present in
   [room_changes] has had the same authoritative value applied by
   [process_room], and must not be reported twice. *)
let reconcile_direct_rooms st rooms room_changes =
  if not (String_map.mem "m.direct" st.s_account_data) then (rooms, room_changes)
  else
    let already_changed =
      List.fold_left
        (fun ids (change : room_change) ->
          String_set.add (Id.Room_id.to_string change.changed_room_id) ids)
        String_set.empty room_changes
    in
    let rooms, extra =
      String_map.fold
        (fun key (info : room_info) (rooms, extra) ->
          match info.membership with
          | Invited | Knocked -> (rooms, extra)
          | Joined | Left ->
              let is_dm = String_set.mem key st.s_direct in
              if Bool.equal info.is_dm is_dm then (rooms, extra)
              else
                let updated = { info with is_dm } in
                let rooms = String_map.add key updated rooms in
                if String_set.mem key already_changed then (rooms, extra)
                else
                  let change =
                    {
                      changed_room_id = info.room_id;
                      info = updated;
                      previous = Some info;
                      timeline = [];
                      decrypted = [];
                      undecrypted = [];
                      state_events = [];
                      ephemeral = [];
                      room_account_data = [];
                      limited = false;
                      unread = Read_state.zero_counts;
                    }
                  in
                  (rooms, change :: extra))
        rooms (rooms, [])
    in
    (rooms, room_changes @ List.rev extra)

let apply_global_account_data st global_ad =
  let s_account_data =
    List.fold_left
      (fun account_data (event_type, content) ->
        String_map.add event_type content account_data)
      st.s_account_data global_ad
  in
  (* The last event in this response wins over stored account data when it has
     a valid ruleset, matching the last-write-wins map fold above. If the
     response has no push-rules event, use the valid stored event; a malformed
     final current event preserves the ruleset that was already active. *)
  let s_ruleset =
    match
      List.find_map
        (fun (event_type, content) ->
          if String.equal event_type "m.push_rules" then Some content else None)
        (List.rev global_ad)
    with
    | Some content -> (
        match
          Jsont.Json.decode Matrix_proto.Push.Ruleset.global_jsont content
        with
        | Ok ruleset -> ruleset
        | Error error ->
            Log.warn (fun m ->
                m "Ignoring malformed m.push_rules account data: %s" error);
            st.s_ruleset)
    | None -> ruleset_from_account_data ~fallback:st.s_ruleset st.s_account_data
  in
  let s_direct =
    match String_map.find_opt "m.direct" s_account_data with
    | Some json -> direct_rooms json
    | None -> st.s_direct
  in
  { st with s_ruleset; s_account_data; s_direct }

let apply ?decrypt ?(coverage = unknown_state_coverage) st
    (resp : Proto_sync.Response.t) =
  let global_ad =
    match resp.account_data with None -> [] | Some a -> typed_events a.events
  in
  let st = apply_global_account_data st global_ad in
  let inputs =
    match resp.rooms with
    | None -> []
    | Some rooms ->
        let joined =
          List.filter_map
            (fun (rid, (r : Proto_sync.Joined_room.t)) ->
              match room_id_of rid with
              | Error _ -> None
              | Ok i_room_id ->
                  let raw_state =
                    match r.state with Some s -> s.events | None -> []
                  in
                  let timeline =
                    match r.timeline with Some t -> t.events | None -> []
                  in
                  Some
                    {
                      i_room_id;
                      i_membership = Joined;
                      i_state = List.filter_map se_of_raw raw_state;
                      i_raw_state = raw_state;
                      i_timeline = timeline;
                      i_limited =
                        (match r.timeline with
                        | Some t -> Option.value t.limited ~default:false
                        | None -> false);
                      i_prev_batch =
                        (match r.timeline with
                        | Some t -> t.prev_batch
                        | None -> None);
                      i_clear_missing_prev_batch = false;
                      i_summary = r.summary;
                      i_unread = r.unread_notifications;
                      i_ephemeral =
                        (match r.ephemeral with
                        | Some e -> e.events
                        | None -> []);
                      i_account_data =
                        (match r.account_data with
                        | Some a -> typed_events a.events
                        | None -> []);
                      i_include_timeline_state = true;
                      i_state_is_partial = false;
                      i_members_incomplete = false;
                      i_num_live = None;
                      i_avatar = Keep_avatar;
                      i_sliding_heroes = None;
                      i_recency_stamp = None;
                    })
            rooms.join
        in
        let left =
          List.filter_map
            (fun (rid, (r : Proto_sync.Left_room.t)) ->
              match room_id_of rid with
              | Error _ -> None
              | Ok i_room_id ->
                  let raw_state =
                    match r.state with Some s -> s.events | None -> []
                  in
                  let timeline =
                    match r.timeline with Some t -> t.events | None -> []
                  in
                  Some
                    {
                      i_room_id;
                      i_membership = Left;
                      i_state = List.filter_map se_of_raw raw_state;
                      i_raw_state = raw_state;
                      i_timeline = timeline;
                      i_limited =
                        (match r.timeline with
                        | Some t -> Option.value t.limited ~default:false
                        | None -> false);
                      i_prev_batch =
                        (match r.timeline with
                        | Some t -> t.prev_batch
                        | None -> None);
                      i_clear_missing_prev_batch = false;
                      i_summary = None;
                      i_unread = None;
                      i_ephemeral = [];
                      i_account_data =
                        (match r.account_data with
                        | Some a -> typed_events a.events
                        | None -> []);
                      i_include_timeline_state = true;
                      i_state_is_partial = false;
                      i_members_incomplete = false;
                      i_num_live = None;
                      i_avatar = Keep_avatar;
                      i_sliding_heroes = None;
                      i_recency_stamp = None;
                    })
            rooms.leave
        in
        let invited =
          List.filter_map
            (fun (rid, (r : Proto_sync.Invited_room.t)) ->
              match room_id_of rid with
              | Error _ -> None
              | Ok i_room_id ->
                  let events =
                    match r.invite_state with Some s -> s.events | None -> []
                  in
                  Some
                    {
                      i_room_id;
                      i_membership = Invited;
                      i_state = List.map se_of_stripped events;
                      i_raw_state = [];
                      i_timeline = [];
                      i_limited = false;
                      i_prev_batch = None;
                      i_clear_missing_prev_batch = false;
                      i_summary = None;
                      i_unread = None;
                      i_ephemeral = [];
                      i_account_data = [];
                      i_include_timeline_state = false;
                      i_state_is_partial = false;
                      i_members_incomplete = false;
                      i_num_live = None;
                      i_avatar = Keep_avatar;
                      i_sliding_heroes = None;
                      i_recency_stamp = None;
                    })
            rooms.invite
        in
        let knocked =
          List.filter_map
            (fun (rid, (r : Proto_sync.Knocked_room.t)) ->
              match room_id_of rid with
              | Error _ -> None
              | Ok i_room_id ->
                  let events =
                    match r.knock_state with Some s -> s.events | None -> []
                  in
                  Some
                    {
                      i_room_id;
                      i_membership = Knocked;
                      i_state = List.map se_of_stripped events;
                      i_raw_state = [];
                      i_timeline = [];
                      i_limited = false;
                      i_prev_batch = None;
                      i_clear_missing_prev_batch = false;
                      i_summary = None;
                      i_unread = None;
                      i_ephemeral = [];
                      i_account_data = [];
                      i_include_timeline_state = false;
                      i_state_is_partial = false;
                      i_members_incomplete = false;
                      i_num_live = None;
                      i_avatar = Keep_avatar;
                      i_sliding_heroes = None;
                      i_recency_stamp = None;
                    })
            rooms.knock
        in
        joined @ left @ invited @ knocked
  in
  let s_rooms, s_receipts, s_power_levels, s_members, room_changes =
    List.fold_left
      (process_room ~decrypt ~coverage st)
      (st.s_rooms, st.s_receipts, st.s_power_levels, st.s_members, [])
      inputs
  in
  let room_changes = List.rev room_changes in
  let s_rooms, room_changes = reconcile_direct_rooms st s_rooms room_changes in
  let presence_events =
    match resp.presence with None -> [] | Some p -> p.events
  in
  let st =
    {
      st with
      s_next_batch = Some resp.next_batch;
      s_rooms;
      s_receipts;
      s_power_levels;
      s_members;
      s_presence = presence_events;
    }
  in
  let changes =
    {
      batch = resp.next_batch;
      room_changes;
      profile_changes = [];
      global_account_data = global_ad;
      to_device = (match resp.to_device with None -> [] | Some t -> t.events);
      device_lists = resp.device_lists;
      one_time_keys_count = resp.device_one_time_keys_count;
      unused_fallback_key_types = resp.device_unused_fallback_key_types;
      presence_events;
    }
  in
  (st, changes)

type sliding_room_parts = {
  sr_room_id : Id.Room_id.t;
  sr_room : Proto_sliding.Response.room option;
  sr_account_data : Jsont.json list;
  sr_receipt : Jsont.json option;
  sr_typing : Jsont.json option;
}

let empty_sliding_room_parts room_id =
  {
    sr_room_id = room_id;
    sr_room = None;
    sr_account_data = [];
    sr_receipt = None;
    sr_typing = None;
  }

let update_sliding_room room_id update rooms =
  let key = Id.Room_id.to_string room_id in
  let current =
    Option.value
      (String_map.find_opt key rooms)
      ~default:(empty_sliding_room_parts room_id)
  in
  String_map.add key (update current) rooms

let sliding_membership ~user_id (room : Proto_sliding.Response.room) =
  let own = Id.User_id.to_string user_id in
  match room.invite_state with
  | Some invite_state ->
      let knocked =
        List.exists
          (fun (event : Event.Stripped_event.t) ->
            Event.Event_type.equal event.type_ Event.Event_type.Room_member
            && String.equal event.state_key own
            && find_string "membership" event.content = Some "knock")
          invite_state
      in
      if knocked then Knocked else Invited
  | None ->
      List.fold_left
        (fun membership (event : Event.Raw_event.t) ->
          if
            Event.Event_type.equal event.type_ Event.Event_type.Room_member
            && event.state_key = Some own
          then
            match find_string "membership" event.content with
            | Some "join" -> Joined
            | Some "invite" -> Invited
            | Some ("leave" | "ban") -> Left
            | Some "knock" -> Knocked
            | Some _ | None -> membership
          else membership)
        Joined room.required_state

let merge_sliding_lists previous updates =
  List.fold_left
    (fun lists (name, (response : Proto_sliding.Response.list_response)) ->
      String_map.add name response.count lists)
    (List.to_seq previous |> String_map.of_seq)
    updates
  |> String_map.bindings

let sliding_room_input st (parts : sliding_room_parts) =
  let key = Id.Room_id.to_string parts.sr_room_id in
  let previous = String_map.find_opt key st.s_rooms in
  match (parts.sr_room, previous) with
  | None, None -> None
  | room, previous -> (
      let account_data = typed_events parts.sr_account_data in
      let ephemeral =
        Option.to_list parts.sr_receipt @ Option.to_list parts.sr_typing
      in
      let defaults membership =
        {
          i_room_id = parts.sr_room_id;
          i_membership = membership;
          i_state = [];
          i_raw_state = [];
          i_timeline = [];
          i_limited = false;
          i_prev_batch = None;
          i_clear_missing_prev_batch = false;
          i_summary = None;
          i_unread = None;
          i_ephemeral = ephemeral;
          i_account_data = account_data;
          i_include_timeline_state = false;
          i_state_is_partial = false;
          i_members_incomplete = false;
          i_num_live = Some 0;
          i_avatar = Keep_avatar;
          i_sliding_heroes = None;
          i_recency_stamp = None;
        }
      in
      match room with
      | None -> Option.map (fun info -> defaults info.membership) previous
      | Some room ->
          let membership = sliding_membership ~user_id:st.s_user_id room in
          let required_state = List.filter_map se_of_raw room.required_state in
          let invite_state =
            Option.value room.invite_state ~default:[]
            |> List.map se_of_stripped
          in
          let summary : Proto_sync.Room_summary.t =
            {
              heroes = None;
              joined_member_count = room.joined_count;
              invited_member_count = room.invited_count;
            }
          in
          let unread : Proto_sync.Unread_notification_counts.t =
            {
              highlight_count = room.highlight_count;
              notification_count = room.notification_count;
            }
          in
          let avatar =
            match room.avatar with
            | Proto_sliding.Response.Unchanged -> Keep_avatar
            | Proto_sliding.Response.Removed -> Remove_avatar
            | Proto_sliding.Response.Set uri -> Replace_avatar uri
          in
          let heroes =
            Option.map
              (List.map (fun (hero : Proto_sliding.Response.hero) ->
                   ({
                      Store.user_id = hero.user_id;
                      display_name = hero.displayname;
                      avatar_url = hero.avatar_url;
                    }
                     : Store.hero)))
              room.heroes
          in
          Some
            {
              (defaults membership) with
              i_state = required_state @ invite_state;
              i_raw_state = room.required_state;
              i_timeline = room.timeline;
              i_limited = room.limited;
              i_prev_batch = room.prev_batch;
              i_clear_missing_prev_batch = true;
              i_summary = Some summary;
              i_unread = Some unread;
              i_state_is_partial = true;
              i_members_incomplete = room.limited;
              i_num_live = room.num_live;
              i_avatar = avatar;
              i_sliding_heroes = heroes;
              i_recency_stamp = room.bump_stamp;
            })

let apply_sliding ?decrypt ?(to_device_enabled = true) st
    (response : Proto_sliding.Response.t) =
  let global_account_data =
    typed_events response.extensions.account_data.global
  in
  let st = apply_global_account_data st global_account_data in
  let room_parts =
    List.fold_left
      (fun rooms (room_id, room) ->
        update_sliding_room room_id
          (fun current -> { current with sr_room = Some room })
          rooms)
      String_map.empty response.rooms
  in
  let room_parts =
    List.fold_left
      (fun rooms (room_id, account_data) ->
        update_sliding_room room_id
          (fun current -> { current with sr_account_data = account_data })
          rooms)
      room_parts response.extensions.account_data.rooms
  in
  let room_parts =
    List.fold_left
      (fun rooms (room_id, receipt) ->
        update_sliding_room room_id
          (fun current -> { current with sr_receipt = Some receipt })
          rooms)
      room_parts response.extensions.receipts.rooms
  in
  let room_parts =
    List.fold_left
      (fun rooms (room_id, typing) ->
        update_sliding_room room_id
          (fun current -> { current with sr_typing = Some typing })
          rooms)
      room_parts response.extensions.typing.rooms
  in
  let inputs =
    String_map.bindings room_parts
    |> List.filter_map (fun (_, parts) -> sliding_room_input st parts)
  in
  let s_rooms, s_receipts, s_power_levels, s_members, room_changes =
    List.fold_left
      (process_room ~decrypt ~coverage:unknown_state_coverage st)
      (st.s_rooms, st.s_receipts, st.s_power_levels, st.s_members, [])
      inputs
  in
  let room_changes = List.rev room_changes in
  let s_rooms, room_changes = reconcile_direct_rooms st s_rooms room_changes in
  let s_sliding_lists = merge_sliding_lists st.s_sliding_lists response.lists in
  let s_sliding_to_device_since =
    if to_device_enabled then
      match Proto_sliding.Response.to_device_next_batch response with
      | Some token -> Some token
      | None -> st.s_sliding_to_device_since
    else st.s_sliding_to_device_since
  in
  let state =
    {
      st with
      s_sliding_pos = Some response.pos;
      s_sliding_to_device_since;
      s_sliding_lists;
      s_rooms;
      s_receipts;
      s_power_levels;
      s_members;
      s_presence = [];
    }
  in
  let state, profile_changes =
    apply_profile_updates state response.extensions.profiles
  in
  let e2ee = response.extensions.e2ee in
  let changes =
    {
      batch = response.pos;
      room_changes;
      profile_changes;
      global_account_data;
      to_device =
        (match response.extensions.to_device with
        | None -> []
        | Some to_device -> to_device.events);
      device_lists = Some e2ee.device_lists;
      one_time_keys_count = e2ee.device_one_time_keys_count;
      unused_fallback_key_types = e2ee.device_unused_fallback_key_types;
      presence_events = [];
    }
  in
  (state, changes)

module Hooks = struct
  type t = {
    mutable h_response :
      (state -> Proto_sync.Response.t -> changes -> unit) list;
    mutable h_sliding_response :
      (state -> Proto_sliding.Response.t -> changes -> unit) list;
    mutable h_room_event : (Id.Room_id.t -> Event.Raw_event.t -> unit) list;
  }

  let create () =
    { h_response = []; h_sliding_response = []; h_room_event = [] }

  let on_response h f = h.h_response <- h.h_response @ [ f ]

  let on_sliding_response h f =
    h.h_sliding_response <- h.h_sliding_response @ [ f ]

  let on_room_event h f = h.h_room_event <- h.h_room_event @ [ f ]

  let run_room_events h changes =
    List.iter
      (fun c ->
        List.iter
          (fun e -> List.iter (fun f -> f c.changed_room_id e) h.h_room_event)
          c.timeline)
      changes.room_changes

  let run h st resp changes =
    run_room_events h changes;
    List.iter (fun f -> f st resp changes) h.h_response

  let run_sliding h st resp changes =
    run_room_events h changes;
    List.iter (fun f -> f st resp changes) h.h_sliding_response
end

(* Decode the old private sliding-sync snapshot into the common state.  This
   lives here, next to the transaction which consumes the slot, so an Eio
   driver cannot accidentally run a second, independent fold. *)
let legacy_sliding_response ~pos state : Proto_sliding.Response.t =
  let lists =
    Legacy_sliding.lists state
    |> List.map (fun (name, count) ->
        (name, ({ count } : Proto_sliding.Response.list_response)))
  in
  let rooms =
    Legacy_sliding.rooms_by_recency state
    |> List.map (fun room ->
        let avatar =
          match Legacy_sliding.avatar_url room with
          | None -> Proto_sliding.Response.Removed
          | Some avatar -> Proto_sliding.Response.Set avatar
        in
        let update : Proto_sliding.Response.room =
          {
            name = None;
            avatar;
            initial = None;
            is_dm = None;
            invite_state =
              (if Legacy_sliding.is_invite room then Some [] else None);
            highlight_count = Some (Legacy_sliding.highlight_count room);
            notification_count = Some (Legacy_sliding.notification_count room);
            timeline = Legacy_sliding.timeline room;
            required_state = List.map snd (Legacy_sliding.required_state room);
            prev_batch = Legacy_sliding.prev_batch room;
            limited = false;
            joined_count = Legacy_sliding.joined_count room;
            invited_count = Legacy_sliding.invited_count room;
            num_live = Some 0;
            bump_stamp = Legacy_sliding.bump_stamp room;
            heroes = Some (Legacy_sliding.heroes room);
          }
        in
        (Legacy_sliding.room_id room, update))
  in
  let profiles : Proto_sliding.Response.profiles =
    {
      users =
        List.map
          (fun (user_id, fields) ->
            (user_id, Proto_sliding.Response.Updated fields))
          (Legacy_sliding.profiles state);
    }
  in
  let to_device =
    Option.map
      (fun next_batch ->
        ({ next_batch; events = [] } : Proto_sliding.Response.to_device))
      (Legacy_sliding.to_device_since state)
  in
  let device_lists : Proto_sync.Device_lists.t = { changed = []; left = [] } in
  let e2ee : Proto_sliding.Response.e2ee =
    {
      device_lists;
      device_one_time_keys_count = [];
      device_unused_fallback_key_types = None;
    }
  in
  let account_data : Proto_sliding.Response.account_data =
    { global = []; rooms = [] }
  in
  let ephemeral : Proto_sliding.Response.ephemeral = { rooms = [] } in
  let thread_subscriptions : Proto_sliding.Response.thread_subscriptions =
    { subscribed = []; unsubscribed = []; prev_batch = None }
  in
  let extensions : Proto_sliding.Response.extensions =
    {
      to_device;
      e2ee;
      account_data;
      receipts = ephemeral;
      typing = ephemeral;
      profiles;
      thread_subscriptions;
      other = [];
    }
  in
  { pos; txn_id = None; lists; rooms; extensions }

let migrate_legacy_sliding_state store state =
  match Legacy_sliding.load_opt store with
  | Error _ as error -> error
  | Ok None -> Ok (state, false)
  | Ok (Some legacy) -> (
      let previous_store = Store.snapshot store in
      try
        let candidate =
          match sliding_pos state with
          | Some _ -> state
          | None -> (
              match Legacy_sliding.pos legacy with
              | None -> state
              | Some pos ->
                  let response = legacy_sliding_response ~pos legacy in
                  apply_sliding (reset_sliding_session state) response |> fst)
        in
        Legacy_sliding.discard store;
        persist store candidate;
        match Store.flush store with
        | Ok () -> Ok (candidate, true)
        | Error _ as error ->
            Store.restore store previous_store;
            error
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        (try Store.restore store previous_store with _ -> ());
        Printexc.raise_with_backtrace exn bt)
