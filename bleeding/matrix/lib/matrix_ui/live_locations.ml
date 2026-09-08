module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Base = Matrix_client.Base_client

type last_location = {
  location : Event.Beacon_content.location;
  timestamp : Event.Timestamp.t;
}

type share = {
  user_id : Id.User_id.t;
  beacon_id : Id.Event_id.t;
  beacon_info : Event.Beacon_info_content.t;
  last_location : last_location option;
}

type t = {
  event_cache : Event_cache.t;
  room_id : Id.Room_id.t;
  mutable state : Base.state;
  now : unit -> int64;
  shares : share Observable.List.t;
  mutable close : unit -> unit;
}

let default_now () = Event.Timestamp.(Ptime_clock.now () |> of_ptime |> to_ms)
let shares t = t.shares
let user_id share = share.user_id
let beacon_id share = share.beacon_id
let last_location share = share.last_location

let is_expired ~now ~origin ~timeout =
  Int64.compare timeout 0L <= 0
  || Int64.compare now origin >= 0
     && Int64.compare (Int64.sub now origin) timeout >= 0

let decode_info (state_event : Matrix_client.Store.state_event) =
  match state_event.event_id with
  | None -> None
  | Some event_id -> (
      match
        Jsont.Json.decode Event.Beacon_info_content.jsont state_event.content
      with
      | Error _ -> None
      | Ok beacon_info -> (
          if not beacon_info.live then None
          else
            match Id.User_id.of_string state_event.state_key with
            | Error _ -> None
            | Ok user_id ->
                Some
                  (user_id, event_id, beacon_info, state_event.origin_server_ts)
          ))

let event_is_redacted (event : Event_cache.event) =
  match (Event_cache.effective event).unsigned with
  | Some unsigned -> Option.is_some (Event.Unsigned.redacted_because unsigned)
  | None -> false

let redaction_target (event : Event.Raw_event.t) =
  if not (Event.Event_type.equal event.type_ Event.Event_type.Room_redaction)
  then None
  else
    let string_member name = function
      | Jsont.Object (members, _) -> (
          match Jsont.Json.find_mem name members with
          | Some (_, Jsont.String (value, _)) -> Some value
          | _ -> None)
      | _ -> None
    in
    match string_member "redacts" event.content with
    | Some target -> Id.Event_id.of_string target |> Result.to_option
    | None -> event.redacts

let newest_location cache room_id beacon_id =
  let events = Event_cache.snapshot cache room_id in
  let redacted =
    Array.fold_left
      (fun ids (cached : Event_cache.event) ->
        match redaction_target (Event_cache.effective cached) with
        | Some id -> id :: ids
        | None -> ids)
      [] events
  in
  events
  |> Array.fold_left
       (fun best (cached : Event_cache.event) ->
         let event = Event_cache.effective cached in
         if
           event_is_redacted cached
           || (match event.event_id with
             | Some id ->
                 List.exists
                   (fun target -> Id.Event_id.equal id target)
                   redacted
             | None -> false)
           || not (Event.Event_type.equal event.type_ Event.Event_type.Beacon)
         then best
         else
           match Jsont.Json.decode Event.Beacon_content.jsont event.content with
           | Error _ -> best
           | Ok content -> (
               let relation = content.relates_to in
               if
                 (not
                    (Event.Rel_type.equal relation.rel_type
                       Event.Rel_type.Reference))
                 || not (Id.Event_id.equal relation.event_id beacon_id)
               then best
               else
                 let candidate =
                   {
                     location = content.location;
                     timestamp = event.origin_server_ts;
                   }
                 in
                 match best with
                 | None -> Some candidate
                 | Some prior
                   when Event.Timestamp.compare candidate.timestamp
                          prior.timestamp
                        >= 0 ->
                     Some candidate
                 | Some prior -> Some prior))
       None

let equal_share a b =
  Id.User_id.equal a.user_id b.user_id
  && Id.Event_id.equal a.beacon_id b.beacon_id
  && a.beacon_info = b.beacon_info
  && a.last_location = b.last_location

let compute t =
  let now = t.now () in
  match Base.find_room t.state t.room_id with
  | None -> []
  | Some _room ->
      Base.state_events t.state t.room_id
      |> List.filter_map (fun (state_event : Matrix_client.Store.state_event) ->
          if
            not
              (Event.Event_type.equal state_event.event_type
                 Event.Event_type.Beacon_info)
          then None
          else
            match decode_info state_event with
            | None -> None
            | Some (user_id, beacon_id, beacon_info, origin) -> (
                let origin =
                  match beacon_info.timestamp with
                  | Some timestamp -> Some timestamp
                  | None -> origin
                in
                match origin with
                | None -> None
                | Some origin ->
                    if
                      is_expired ~now
                        ~origin:(Event.Timestamp.to_ms origin)
                        ~timeout:beacon_info.timeout
                    then None
                    else
                      Some
                        {
                          user_id;
                          beacon_id;
                          beacon_info;
                          last_location =
                            newest_location t.event_cache t.room_id beacon_id;
                        }))
      |> List.sort (fun a b -> Id.User_id.compare a.user_id b.user_id)

let refresh t state =
  t.state <- state;
  Observable.List.reconcile_by ~key:user_id ~equal:equal_share (shares t)
    (compute t)

let refresh_state = refresh

let refresh_time t =
  Observable.List.reconcile_by ~key:user_id ~equal:equal_share (shares t)
    (compute t)

let close t = t.close ()

let create ~event_cache ~state ~room_id ?sw ?(now = default_now) () =
  let t =
    {
      event_cache;
      state;
      room_id;
      now;
      shares = Observable.List.create [];
      close = (fun () -> ());
    }
  in
  (match sw with
  | None -> ()
  | Some sw ->
      let _, subscription =
        Observable.List.subscribe ~sw (Event_cache.events event_cache room_id)
      in
      t.close <- (fun () -> Observable.List.unsubscribe subscription);
      Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec loop () =
            match Observable.List.next subscription with
            | None -> ()
            | Some _ ->
                refresh_time t;
                loop ()
          in
          loop ();
          `Stop_daemon));
  refresh t state;
  t
