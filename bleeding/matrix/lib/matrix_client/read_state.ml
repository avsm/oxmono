module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Timestamp = Matrix_proto.Event.Timestamp
open Matrix_proto.Json

type receipt = { event_id : Id.Event_id.t; ts : Timestamp.t option }

type thread = {
  thread_id : Id.Event_id.t;
  public_read : receipt option;
  private_read : receipt option;
}

type t = {
  public_read : receipt option;
  private_read : receipt option;
  fully_read : Id.Event_id.t option;
  threads : thread list;
}

let sort_threads threads =
  let compare_receipts left right =
    match (left.ts, right.ts) with
    | Some left_ts, Some right_ts ->
        let by_timestamp = Timestamp.compare left_ts right_ts in
        if by_timestamp <> 0 then by_timestamp
        else Id.Event_id.compare left.event_id right.event_id
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> 0
  in
  let merge_receipt left right =
    match (left, right) with
    | None, value | value, None -> value
    | Some left, Some right ->
        Some (if compare_receipts left right >= 0 then left else right)
  in
  let merge (left : thread) (right : thread) =
    {
      left with
      public_read = merge_receipt left.public_read right.public_read;
      private_read = merge_receipt left.private_read right.private_read;
    }
  in
  let sorted =
    List.sort
      (fun left right -> Id.Event_id.compare left.thread_id right.thread_id)
      threads
  in
  let rec deduplicate acc = function
    | [] -> List.rev acc
    | thread :: rest -> (
        match acc with
        | previous :: acc_tail
          when Id.Event_id.equal previous.thread_id thread.thread_id ->
            deduplicate (merge previous thread :: acc_tail) rest
        | _ -> deduplicate (thread :: acc) rest)
  in
  deduplicate [] sorted

let empty =
  { public_read = None; private_read = None; fully_read = None; threads = [] }

let v ?public_read ?private_read ?fully_read () =
  { public_read; private_read; fully_read; threads = [] }

let public_read t = t.public_read
let private_read t = t.private_read
let fully_read t = t.fully_read
let thread_ids t = List.map (fun thread -> thread.thread_id) t.threads

let find_thread t thread_id =
  List.find_opt
    (fun thread -> Id.Event_id.equal thread.thread_id thread_id)
    t.threads

let thread_public_read t ~thread_id =
  Option.bind (find_thread t thread_id) (fun thread -> thread.public_read)

let thread_private_read t ~thread_id =
  Option.bind (find_thread t thread_id) (fun thread -> thread.private_read)

let receipt_jsont : receipt Jsont.t =
  Jsont.Object.(
    map (fun event_id ts -> { event_id; ts })
    |> mem "event_id" Id.Event_id.jsont ~enc:(fun t -> t.event_id)
    |> opt_mem "ts" Json_codec.persisted_timestamp ~enc:(fun t -> t.ts)
    |> finish)

let thread_jsont : thread Jsont.t =
  Jsont.Object.(
    map (fun thread_id public_read private_read ->
        ({ thread_id; public_read; private_read } : thread))
    |> mem "thread_id" Id.Event_id.jsont ~enc:(fun (t : thread) -> t.thread_id)
    |> opt_mem "public_read" receipt_jsont ~enc:(fun (t : thread) ->
        t.public_read)
    |> opt_mem "private_read" receipt_jsont ~enc:(fun (t : thread) ->
        t.private_read)
    |> finish)

let jsont : t Jsont.t =
  Jsont.Object.(
    map (fun public_read private_read fully_read threads ->
        {
          public_read;
          private_read;
          fully_read;
          threads = sort_threads threads;
        })
    |> opt_mem "public_read" receipt_jsont ~enc:(fun t -> t.public_read)
    |> opt_mem "private_read" receipt_jsont ~enc:(fun t -> t.private_read)
    |> opt_mem "fully_read" Id.Event_id.jsont ~enc:(fun t -> t.fully_read)
    |> mem "threads" (Jsont.list thread_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.threads)
    |> finish)

let latest_read t =
  match (t.public_read, t.private_read) with
  | None, None -> None
  | Some r, None | None, Some r -> Some r.event_id
  | Some a, Some b -> (
      match (a.ts, b.ts) with
      | Some x, Some y ->
          Some (if Timestamp.compare y x >= 0 then b.event_id else a.event_id)
      | None, Some _ -> Some b.event_id
      | Some _, None -> Some a.event_id
      | None, None -> Some b.event_id)

let latest_read_of ~public_read ~private_read =
  match (public_read, private_read) with
  | None, None -> None
  | Some r, None | None, Some r -> Some r.event_id
  | Some a, Some b -> (
      match (a.ts, b.ts) with
      | Some x, Some y ->
          Some (if Timestamp.compare y x >= 0 then b.event_id else a.event_id)
      | None, Some _ -> Some b.event_id
      | Some _, None -> Some a.event_id
      | None, None -> Some b.event_id)

let thread_latest_read t ~thread_id =
  Option.bind (find_thread t thread_id) (fun thread ->
      latest_read_of ~public_read:thread.public_read
        ~private_read:thread.private_read)

let event_type j = Option.bind (find_mem "type" j) as_string

(* A threaded receipt belongs to a thread's own unread count, not the room's;
   the main timeline's receipts are the unthreaded ones and those explicitly
   in the "main" thread. *)
let thread_of_body body =
  match find_mem "thread_id" body with
  | None -> `Main
  | Some thread_id -> (
      match as_string thread_id with
      | Some "main" -> `Main
      | Some thread_id -> (
          match Id.Event_id.of_string thread_id with
          | Ok thread_id -> `Thread thread_id
          | Error _ -> `Invalid)
      | None -> `Invalid)

let newer ~existing candidate =
  match existing with
  | None -> true
  | Some e -> (
      match (e.ts, candidate.ts) with
      | Some old_ts, Some new_ts -> Timestamp.compare new_ts old_ts >= 0
      | None, _ -> true
      | Some _, None ->
          (* The one held has a known timestamp and the candidate has none:
             there is no way to establish the candidate is not older, so it
             does not replace it. *)
          false)

let ingest_receipt_event ~user_id t event =
  match event_type event with
  | Some "m.receipt" -> (
      match Option.bind (find_mem "content" event) as_object with
      | None -> t
      | Some by_event ->
          let uid = Id.User_id.to_string user_id in
          List.fold_left
            (fun t ((event_id_s, _), types) ->
              match Id.Event_id.of_string event_id_s with
              | Error _ -> t
              | Ok event_id -> (
                  let read_of field =
                    match Option.bind (find_mem field types) (find_mem uid) with
                    | None -> None
                    | Some body ->
                        Some
                          ( thread_of_body body,
                            {
                              event_id;
                              ts =
                                Option.map Timestamp.of_ms
                                  (Option.bind (find_mem "ts" body) as_int64);
                            } )
                  in
                  let update_thread t thread_id field receipt =
                    let old = find_thread t thread_id in
                    let existing =
                      Option.bind old (fun thread ->
                          match field with
                          | `Public -> thread.public_read
                          | `Private -> thread.private_read)
                    in
                    if not (newer ~existing receipt) then t
                    else
                      let updated =
                        match old with
                        | Some thread -> (
                            match field with
                            | `Public ->
                                { thread with public_read = Some receipt }
                            | `Private ->
                                { thread with private_read = Some receipt })
                        | None ->
                            {
                              thread_id;
                              public_read =
                                (match field with
                                | `Public -> Some receipt
                                | `Private -> None);
                              private_read =
                                (match field with
                                | `Public -> None
                                | `Private -> Some receipt);
                            }
                      in
                      let threads =
                        updated
                        :: List.filter
                             (fun thread ->
                               not
                                 (Id.Event_id.equal thread.thread_id thread_id))
                             t.threads
                      in
                      {
                        t with
                        threads =
                          List.sort
                            (fun left right ->
                              Id.Event_id.compare left.thread_id right.thread_id)
                            threads;
                      }
                  in
                  let update t field = function
                    | `Main, receipt -> (
                        match field with
                        | `Public when newer ~existing:t.public_read receipt ->
                            { t with public_read = Some receipt }
                        | `Private when newer ~existing:t.private_read receipt
                          ->
                            { t with private_read = Some receipt }
                        | _ -> t)
                    | `Thread thread_id, receipt ->
                        update_thread t thread_id field receipt
                    | `Invalid, _ -> t
                  in
                  let t =
                    match read_of "m.read" with
                    | Some read -> update t `Public read
                    | None -> t
                  in
                  match read_of "m.read.private" with
                  | Some read -> update t `Private read
                  | None -> t))
            t by_event)
  | _ -> t

let ingest_ephemeral ~user_id t events =
  List.fold_left (ingest_receipt_event ~user_id) t events

let ingest_fully_read t event =
  match event_type event with
  | Some "m.fully_read" -> (
      match
        Option.bind
          (Option.bind (find_mem "content" event) (find_mem "event_id"))
          as_string
      with
      | None -> t
      | Some s -> (
          match Id.Event_id.of_string s with
          | Ok id -> { t with fully_read = Some id }
          | Error _ -> t))
  | _ -> t

type counts = { unread : int; notifications : int; highlights : int }

let zero_counts = { unread = 0; notifications = 0; highlights = 0 }

let unread_types =
  [
    "m.room.message";
    "m.room.encrypted";
    "m.sticker";
    "m.poll.start";
    "m.poll.end";
    "org.matrix.msc3381.poll.start";
    "org.matrix.msc3381.poll.end";
  ]

let is_edit (e : Event.Raw_event.t) =
  match
    Option.bind (find_mem "m.relates_to" e.content) (find_mem "rel_type")
  with
  | Some (Jsont.String ("m.replace", _)) -> true
  | _ -> false

let is_redacted (e : Event.Raw_event.t) =
  match e.unsigned with
  | None -> false
  | Some u -> Option.is_some (Event.Unsigned.redacted_because u)

let is_direct_thread_reply (e : Event.Raw_event.t) =
  match Option.bind (find_mem "m.relates_to" e.content) as_object with
  | Some relates_to ->
      Option.equal String.equal
        (Option.bind (Jsont.Json.find_mem "rel_type" relates_to)
           (fun (_, value) -> as_string value))
        (Some "m.thread")
  | None -> false

let marks_as_unread_event ~user_id (e : Event.Raw_event.t) =
  (not (Id.User_id.equal e.sender user_id))
  && List.mem (Event.Event_type.to_string e.type_) unread_types
  && (not (is_edit e))
  && not (is_redacted e)

let marks_as_unread ~user_id (e : Event.Raw_event.t) =
  marks_as_unread_event ~user_id e

let is_main_timeline_event e = not (is_direct_thread_reply e)

let index_of_event_id events id =
  let rec go i = function
    | [] -> None
    | (e : Event.Raw_event.t) :: tl -> (
        match e.event_id with
        | Some eid when Id.Event_id.equal eid id -> Some i
        | _ -> go (i + 1) tl)
  in
  go 0 events

let last_own_index ~user_id events =
  let rec go i best = function
    | [] -> best
    | (e : Event.Raw_event.t) :: tl ->
        let best = if Id.User_id.equal e.sender user_id then Some i else best in
        go (i + 1) best tl
  in
  go 0 None events

let latest_read_in_timeline ~user_id t events =
  let events = List.filter is_main_timeline_event events in
  let candidates =
    List.filter_map
      (fun r -> Option.bind r (fun r -> index_of_event_id events r.event_id))
      [ t.public_read; t.private_read ]
    @ Option.to_list (last_own_index ~user_id events)
  in
  match candidates with [] -> None | l -> Some (List.fold_left max 0 l)

let event_in_thread ~thread_id (e : Event.Raw_event.t) =
  match e.event_id with
  | Some event_id when Id.Event_id.equal event_id thread_id -> true
  | _ -> (
      match Option.bind (find_mem "m.relates_to" e.content) as_object with
      | None -> false
      | Some relates_to -> (
          match
            ( Option.bind
                (Option.map snd (Jsont.Json.find_mem "rel_type" relates_to))
                as_string,
              Option.bind
                (Option.map snd (Jsont.Json.find_mem "event_id" relates_to))
                as_string )
          with
          | Some "m.thread", Some event_id -> (
              match Id.Event_id.of_string event_id with
              | Ok event_id -> Id.Event_id.equal event_id thread_id
              | Error _ -> false)
          | _ -> false))

let latest_read_in_thread ~user_id ~thread_id t events =
  let events = List.filter (event_in_thread ~thread_id) events in
  let candidates =
    List.filter_map
      (fun r -> Option.bind r (fun r -> index_of_event_id events r.event_id))
      [ thread_public_read t ~thread_id; thread_private_read t ~thread_id ]
    @ Option.to_list
        (last_own_index ~user_id
           (List.filter (event_in_thread ~thread_id) events))
  in
  match candidates with [] -> None | l -> Some (List.fold_left max 0 l)

let count_unread ~user_id ~notification receipts events =
  let events = List.filter is_main_timeline_event events in
  let start =
    match latest_read_in_timeline ~user_id receipts events with
    | None -> 0
    | Some i -> i + 1
  in
  List.fold_left
    (fun acc e ->
      let acc =
        if marks_as_unread ~user_id e then { acc with unread = acc.unread + 1 }
        else acc
      in
      let n = notification e in
      let acc =
        if n.Push_evaluator.notify then
          { acc with notifications = acc.notifications + 1 }
        else acc
      in
      if n.Push_evaluator.highlight then
        { acc with highlights = acc.highlights + 1 }
      else acc)
    zero_counts
    (List.filteri (fun i _ -> i >= start) events)

let count_unread_in_thread ~user_id ~thread_id ~notification receipts events =
  let events = List.filter (event_in_thread ~thread_id) events in
  let start =
    match latest_read_in_thread ~user_id ~thread_id receipts events with
    | None -> 0
    | Some i -> i + 1
  in
  List.fold_left
    (fun acc e ->
      let acc =
        if marks_as_unread ~user_id e then { acc with unread = acc.unread + 1 }
        else acc
      in
      let n = notification e in
      let acc =
        if n.Push_evaluator.notify then
          { acc with notifications = acc.notifications + 1 }
        else acc
      in
      if n.Push_evaluator.highlight then
        { acc with highlights = acc.highlights + 1 }
      else acc)
    zero_counts
    (List.filteri (fun i _ -> i >= start) events)
