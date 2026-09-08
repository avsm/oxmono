module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Sync = Matrix_client.Base_client

type section =
  | Invites
  | Favourites
  | People
  | Rooms
  | Low_priority
  | Historical

type order = Activity | Name

type room = {
  id : Id.Room_id.t;
  name : string;
  avatar_url : string option;
  topic : string option;
  membership : Sync.membership;
  section : section;
  is_dm : bool;
  is_space : bool;
  latest : string option;
  latest_sender : Id.User_id.t option;
  latest_timestamp : Event.Timestamp.t option;
  latest_is_unsent : bool;
  last_active : Event.Timestamp.t;
  notification_count : int;
  highlight_count : int;
  unread_messages : int;
  marked_unread : bool;
  encrypted : bool;
  tags : (string * float option) list;
}

let has_tag name tags = List.exists (fun (tag, _) -> String.equal name tag) tags

let unread room =
  room.notification_count > 0 || room.unread_messages > 0 || room.marked_unread

(* Filters are data rather than boxed closures so that a [Filter.t] can be
   compared, which is what lets [set_filter] skip a refilter that would
   change nothing. *)

module Filter = struct
  type unread = Mentions | Notifications | Messages

  type t =
    | Everything
    | Nothing
    | All of t list
    | Any of t list
    | Not of t
    | Membership of Sync.membership
    | Non_left
    | Dm
    | Favourite
    | Low_priority
    | Unread of unread
    | Name of string
    | Fuzzy of string
    | Search of string
    | Room_ids of Id.Room_id.t list
    | In_section of section
    | Space
    | Deduplicate_versions

  let rec matches filter room =
    match filter with
    | Everything -> true
    | Nothing -> false
    | All filters -> List.for_all (fun f -> matches f room) filters
    | Any filters -> List.exists (fun f -> matches f room) filters
    | Not filter -> not (matches filter room)
    | Membership membership -> room.membership = membership
    | Non_left -> (
        match room.membership with
        | Sync.Joined | Sync.Invited | Sync.Knocked -> true
        | Sync.Left -> false)
    | Dm -> room.is_dm
    | Space -> room.is_space
    | Favourite -> has_tag "m.favourite" room.tags
    | Low_priority -> has_tag "m.lowpriority" room.tags
    | Unread Mentions -> room.highlight_count > 0 || room.marked_unread
    | Unread Notifications -> room.notification_count > 0 || room.marked_unread
    | Unread Messages -> room.unread_messages > 0 || room.marked_unread
    | Name query -> Matching.contains ~haystack:room.name ~needle:query
    | Fuzzy query ->
        Option.is_some (Matching.fuzzy_score ~haystack:room.name ~needle:query)
    | Search query ->
        Matching.contains ~haystack:room.name ~needle:query
        || Matching.contains
             ~haystack:(Id.Room_id.to_string room.id)
             ~needle:query
    | Room_ids ids ->
        let key = Id.Room_id.to_string room.id in
        List.exists (fun id -> String.equal (Id.Room_id.to_string id) key) ids
    | In_section section -> room.section = section
    (* The successor is a property of the whole room-list projection, so the
       list's refiltering path handles this constructor with that context. A
       standalone predicate cannot see the other rooms and is conservative. *)
    | Deduplicate_versions -> true

  let rec everything = function
    | Everything -> true
    | All filters -> List.for_all everything filters
    | Any filters -> filters <> [] && List.exists everything filters
    | Name "" | Fuzzy "" | Search "" -> true
    | _ -> false

  let equal = ( = )

  let rec pp ppf = function
    | Everything -> Format.pp_print_string ppf "everything"
    | Nothing -> Format.pp_print_string ppf "nothing"
    | All filters -> Format.fprintf ppf "@[<1>(all%a)@]" pp_list filters
    | Any filters -> Format.fprintf ppf "@[<1>(any%a)@]" pp_list filters
    | Not filter -> Format.fprintf ppf "@[<1>(not@ %a)@]" pp filter
    | Membership Sync.Joined -> Format.pp_print_string ppf "joined"
    | Membership Sync.Invited -> Format.pp_print_string ppf "invited"
    | Membership Sync.Knocked -> Format.pp_print_string ppf "knocked"
    | Membership Sync.Left -> Format.pp_print_string ppf "left"
    | Non_left -> Format.pp_print_string ppf "non-left"
    | Dm -> Format.pp_print_string ppf "dm"
    | Space -> Format.pp_print_string ppf "space"
    | Favourite -> Format.pp_print_string ppf "favourite"
    | Low_priority -> Format.pp_print_string ppf "low-priority"
    | Unread Mentions -> Format.pp_print_string ppf "unread-mentions"
    | Unread Notifications -> Format.pp_print_string ppf "unread-notifications"
    | Unread Messages -> Format.pp_print_string ppf "unread-messages"
    | Name query -> Format.fprintf ppf "@[<1>(name@ %S)@]" query
    | Fuzzy query -> Format.fprintf ppf "@[<1>(fuzzy@ %S)@]" query
    | Search query -> Format.fprintf ppf "@[<1>(search@ %S)@]" query
    | Room_ids ids ->
        Format.fprintf ppf "@[<1>(room-ids%a)@]"
          (fun ppf ids ->
            List.iter
              (fun id -> Format.fprintf ppf "@ %s" (Id.Room_id.to_string id))
              ids)
          ids
    | In_section section ->
        Format.fprintf ppf "@[<1>(section@ %s)@]"
          (match section with
          | Invites -> "invites"
          | Favourites -> "favourites"
          | People -> "people"
          | Rooms -> "rooms"
          | Low_priority -> "low-priority"
          | Historical -> "historical")
    | Deduplicate_versions -> Format.pp_print_string ppf "deduplicate-versions"

  and pp_list ppf filters =
    List.iter (fun filter -> Format.fprintf ppf "@ %a" pp filter) filters

  (* Invariant: [Option.is_some (score f room) = matches f room]. *)
  let rec score filter room =
    match filter with
    | Fuzzy query -> Matching.fuzzy_score ~haystack:room.name ~needle:query
    | All filters ->
        List.fold_left
          (fun total filter ->
            match (total, score filter room) with
            | Some total, Some this -> Some (total + this)
            | _, _ -> None)
          (Some 0) filters
    | Any filters ->
        List.fold_left
          (fun best filter ->
            match (best, score filter room) with
            | None, this -> this
            | Some best, Some this -> Some (Int.max best this)
            | (Some _ as best), None -> best)
          None filters
    | Not filter -> if Option.is_some (score filter room) then None else Some 0
    | filter -> if matches filter room then Some 0 else None
end

(* An event timestamp is never compared against a recency stamp: a room with
   a latest event sorts above every room without one, and only when neither
   has an event do both fall back to the recency stamp. *)

let section_rank = function
  | Invites -> 0
  | Favourites -> 1
  | People -> 2
  | Rooms -> 3
  | Low_priority -> 4
  | Historical -> 5

let compare_latest_event left right =
  (* Unsent local echoes first: [true < false]. *)
  Bool.compare right.latest_is_unsent left.latest_is_unsent

let compare_recency left right =
  match (left.latest_timestamp, right.latest_timestamp) with
  | Some left_ts, Some right_ts -> Event.Timestamp.compare right_ts left_ts
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> Event.Timestamp.compare right.last_active left.last_active

let compare_name left right = String.compare left.name right.name

let rec first = function
  | [] -> 0
  | compare :: rest -> (
      match compare () with 0 -> first rest | order -> order)

let compare_rooms order left right =
  first
    [
      (fun () ->
        Int.compare (section_rank left.section) (section_rank right.section));
      (fun () ->
        match order with
        | Activity ->
            first
              [
                (fun () -> compare_latest_event left right);
                (fun () -> compare_recency left right);
                (fun () -> compare_name left right);
              ]
        | Name ->
            first
              [
                (fun () -> compare_name left right);
                (fun () -> compare_latest_event left right);
                (fun () -> compare_recency left right);
              ]);
      (fun () ->
        String.compare
          (Id.Room_id.to_string left.id)
          (Id.Room_id.to_string right.id));
    ]

let is_unsent (event : Event_cache.event) =
  match event.delivery with
  | Event_cache.Synced -> false
  | Event_cache.Sending | Event_cache.Queued | Event_cache.Failed _ -> true

let preview_of presented ~unsent =
  ( Presentation.preview presented,
    Some presented.Presentation.sender,
    Some presented.Presentation.timestamp,
    unsent )

let latest_from_cache ?own_user ?can_accept_knock cache room_id =
  let events = Event_cache.snapshot cache room_id in
  (* The newest edit of every event is collected on the way back, so that
     when the scan stops on a preview-worthy event it can substitute the edit
     that replaced it. The cache event stays beside its presentation: its
     [clear_event] is the provenance the encryption clauses need, which a
     [Presentation.t] deliberately does not carry. *)
  let edits :
      ( string,
        Event_cache.event * Presentation.t * Presentation.t * bool )
      Hashtbl.t =
    Hashtbl.create 4
  in
  let record (event : Event_cache.event) (presented : Presentation.t) ~unsent =
    match presented.relation with
    | Some { kind = Replacement; target } -> (
        let key = Id.Event_id.to_string target in
        if not (Hashtbl.mem edits key) then
          match Presentation.replacement presented with
          | Some resolved
            when Presentation.is_preview_worthy ?own_user ?can_accept_knock
                   resolved ->
              Hashtbl.replace edits key (event, presented, resolved, unsent)
          | Some _ | None -> ())
    | Some _ | None -> ()
  in
  let resolve (event : Event_cache.event) (presented : Presentation.t) ~unsent =
    match presented.event_id with
    | None -> (presented, unsent)
    | Some id -> (
        match Hashtbl.find_opt edits (Id.Event_id.to_string id) with
        | Some (edit_event, edit, resolved, edit_unsent)
          when Presentation.is_valid_replacement_with_encryption
                 ~original_encrypted:(Option.is_some event.clear_event)
                 ~replacement_encrypted:(Option.is_some edit_event.clear_event)
                 ~original:presented ~replacement:edit ->
            (resolved, edit_unsent)
        | Some _ | None -> (presented, unsent))
  in
  let rec find index =
    if index < 0 then None
    else
      let event = events.(index) in
      let unsent = is_unsent event in
      let presented = Presentation.of_event (Event_cache.effective event) in
      if Presentation.is_preview_worthy ?own_user ?can_accept_knock presented
      then
        let presented, unsent = resolve event presented ~unsent in
        Some (preview_of presented ~unsent)
      else (
        record event presented ~unsent;
        find (index - 1))
  in
  find (Array.length events - 1)

let latest_of_info ?own_user ?can_accept_knock cache (info : Sync.room_info) =
  match latest_from_cache ?own_user ?can_accept_knock cache info.room_id with
  | Some values -> values
  | None -> (
      (* The cache has nothing worth previewing. [room_info.latest_event] is
         the server's idea of the newest timeline event, which is only a
         preview if it passes the same test. There is no edit table here:
         one event cannot replace itself. *)
      match info.latest_event with
      | None -> (None, None, None, false)
      | Some event ->
          let presented = Presentation.of_event event in
          if
            Presentation.is_preview_worthy ?own_user ?can_accept_knock presented
          then preview_of presented ~unsent:false
          else (None, None, None, false))

let section (info : Sync.room_info) =
  match info.membership with
  | Invited -> Invites
  | Left | Knocked -> Historical
  | Joined ->
      if has_tag "m.favourite" info.tags then Favourites
      else if has_tag "m.lowpriority" info.tags then Low_priority
      else if info.is_dm then People
      else Rooms

(* The room type is durable current state, so deriving this at projection time
   also gives the same answer after a store reload.  A missing or malformed
   [m.room.create] is an ordinary room. *)
let is_space_of_info (info : Sync.room_info) =
  match
    Matrix_client.Store.find_state_event info
      ~event_type:Event.Event_type.Room_create ()
  with
  | None -> false
  | Some event -> (
      try
        match
          Jsont.Json.decode Event.Room_create_content.jsont event.content
        with
        | Ok content ->
            Event.Room_create_content.room_type content = Some "m.space"
        | Error _ -> false
      with _ -> false)

(* Rust admits a knock as a latest event only when the current user can act on
   it. A moderator can do that by inviting the requester, or by kicking them;
   kicking additionally requires a strictly greater user power level. *)
let can_accept_knock_of_info own_user (info : Sync.room_info) =
  let power_levels =
    Option.bind
      (Matrix_client.Store.find_state_event info
         ~event_type:Event.Event_type.Room_power_levels ()) (fun event ->
        try
          Result.to_option
            (Jsont.Json.decode Event.Room_power_levels_content.jsont
               event.content)
        with _ -> None)
  in
  match power_levels with
  | None -> fun _ -> false
  | Some levels ->
      let own_level =
        Event.Room_power_levels_content.user_level levels own_user
      in
      let invite =
        Option.value (Event.Room_power_levels_content.invite levels) ~default:0
      in
      let kick =
        Option.value (Event.Room_power_levels_content.kick levels) ~default:50
      in
      fun requester ->
        own_level >= invite
        || own_level >= kick
           && own_level
              > Event.Room_power_levels_content.user_level levels requester

let of_info ?own_user cache (info : Sync.room_info) =
  let can_accept_knock =
    Option.map (fun own -> can_accept_knock_of_info own info) own_user
  in
  let latest, latest_sender, latest_timestamp, latest_is_unsent =
    latest_of_info ?own_user ?can_accept_knock cache info
  in
  let notification_count =
    Int.max info.notification_count info.local_notification_count
  in
  let highlight_count =
    Int.max info.highlight_count info.local_highlight_count
  in
  {
    id = info.room_id;
    name = Sync.display_name info;
    avatar_url = info.avatar_url;
    topic = info.topic;
    membership = info.membership;
    section = section info;
    is_dm = info.is_dm;
    is_space = is_space_of_info info;
    latest;
    latest_sender;
    latest_timestamp;
    latest_is_unsent;
    last_active = Event.Timestamp.of_ms info.last_active_ts;
    notification_count;
    highlight_count;
    unread_messages = info.local_unread_count;
    marked_unread = info.marked_unread;
    encrypted = Option.is_some info.encryption;
    tags = info.tags;
  }

(* The tombstone is deliberately decoded at projection time.  It is already
   part of [room_info.state_events], and therefore survives a store reload;
   malformed content is simply treated as if there were no successor. *)
let successor_of_info (info : Sync.room_info) =
  let tombstone =
    Matrix_client.Store.find_state_event info
      ~event_type:Event.Event_type.Room_tombstone ()
  in
  Option.bind tombstone (fun event ->
      try
        Result.to_option
          (Jsont.Json.decode Event.Room_tombstone_content.jsont event.content)
        |> Option.map Event.Room_tombstone_content.replacement_room
      with _ -> None)

type t = {
  cache : Event_cache.t;
  all_rooms : room Observable.List.t;
  rooms : room Observable.List.t;
  mutable filter : Filter.t;
  mutable order : order;
  mutable successors : (string * Id.Room_id.t) list;
}

let all_rooms t = t.all_rooms
let rooms t = t.rooms
let filter t = t.filter
let sort t = t.order
let room_key room = Id.Room_id.to_string room.id

let find t room_id =
  let wanted = Id.Room_id.to_string room_id in
  Array.find_opt
    (fun room -> String.equal (room_key room) wanted)
    (Observable.List.snapshot t.all_rooms)

let reconcile observable rooms =
  Observable.List.reconcile_by ~key:room_key ~equal:( = ) observable rooms

let successor_membership all successors room =
  Option.bind
    (List.assoc_opt (room_key room) successors)
    (fun successor ->
      List.find_map
        (fun (candidate : room) ->
          if String.equal (room_key candidate) (Id.Room_id.to_string successor)
          then Some candidate.membership
          else None)
        all)

let active_version room successor =
  match (room.membership, successor) with
  | _, None -> true
  | Sync.Joined, Some (Sync.Invited | Sync.Knocked) -> true
  | Sync.Joined, Some (Sync.Joined | Sync.Left) -> false
  | (Sync.Invited | Sync.Left | Sync.Knocked), Some _ -> false

let rec matches_with_context ~all ~successors filter room =
  match filter with
  | Filter.Deduplicate_versions ->
      active_version room (successor_membership all successors room)
  | Filter.All filters ->
      List.for_all
        (fun f -> matches_with_context ~all ~successors f room)
        filters
  | Filter.Any filters ->
      List.exists
        (fun f -> matches_with_context ~all ~successors f room)
        filters
  | Filter.Not filter -> not (matches_with_context ~all ~successors filter room)
  | filter -> Filter.matches filter room

let refilter t =
  let all = Observable.List.snapshot t.all_rooms |> Array.to_list in
  all
  |> List.filter (matches_with_context ~all ~successors:t.successors t.filter)
  |> List.sort (compare_rooms t.order)
  |> reconcile t.rooms

let refresh t state =
  (* The own user comes from the state rather than from an argument of its
     own, so that a caller cannot pass one the sync service disagrees with. *)
  let own_user = Sync.user_id state in
  let infos = Sync.rooms state in
  t.successors <-
    List.filter_map
      (fun (info : Sync.room_info) ->
        Option.map
          (fun successor -> (Id.Room_id.to_string info.room_id, successor))
          (successor_of_info info))
      infos;
  let all = List.map (of_info ~own_user t.cache) infos in
  reconcile t.all_rooms all;
  refilter t

let create cache state =
  let t =
    {
      cache;
      all_rooms = Observable.List.create [];
      rooms = Observable.List.create [];
      filter = Filter.Non_left;
      order = Activity;
      successors = [];
    }
  in
  refresh t state;
  t

let set_filter t filter =
  if not (Filter.equal t.filter filter) then (
    t.filter <- filter;
    refilter t)

let set_sort t order =
  if t.order <> order then (
    t.order <- order;
    refilter t)
