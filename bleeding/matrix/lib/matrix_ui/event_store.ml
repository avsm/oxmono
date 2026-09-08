module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Raw = Matrix_proto.Event.Raw_event

module Error = struct
  type t = Closed | Codec of string | Backend of string

  let to_string = function
    | Closed -> "the event store is closed"
    | Codec message -> "event store codec failure: " ^ message
    | Backend message -> "event store backend failure: " ^ message

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Internal = struct
  type delivery = Synced | Sending | Queued | Failed of string

  type event = {
    stable_id : string;
    event : Raw.t;
    clear_event : Raw.t option;
    delivery : delivery;
  }

  type events_chunk = {
    chunk_id : int;
    prev_token : string option;
    next_token : string option;
    events : event list;
  }

  type gap_chunk = { gap_id : int; token : string }
  type chunk = Events of events_chunk | Gap of gap_chunk

  type chunk_metadata =
    | Events_metadata of {
        chunk_id : int;
        prev_token : string option;
        next_token : string option;
        stable_ids : string list;
      }
    | Gap_metadata of gap_chunk

  type room_metadata = {
    chunks : chunk_metadata list;
    next_chunk_id : int;
    external_events : event list;
  }

  let max_external_events = 256

  type room = {
    chunks : chunk list;
    next_chunk_id : int;
    external_events : event list;
  }

  type initial =
    | Full of room
    | Tail of { metadata : room_metadata; newest : events_chunk option }

  let validate_layout (room : room) =
    let seen = Hashtbl.create (List.length room.chunks) in
    let rec validate previous_gap = function
      | [] ->
          if previous_gap then Error "persisted room layout ends with a gap"
          else Ok ()
      | chunk :: rest ->
          let chunk_id, is_gap =
            match chunk with
            | Events chunk -> (chunk.chunk_id, false)
            | Gap gap -> (gap.gap_id, true)
          in
          if chunk_id < 0 || chunk_id >= room.next_chunk_id then
            Error "persisted chunk id is outside the room counter"
          else if Hashtbl.mem seen chunk_id then
            Error "persisted room layout has duplicate chunk ids"
          else if previous_gap && is_gap then
            Error "persisted room layout has adjacent gaps"
          else begin
            Hashtbl.add seen chunk_id ();
            validate is_gap rest
          end
    in
    if room.next_chunk_id < 0 then Error "persisted room counter is negative"
    else validate false room.chunks

  let validate_room (room : room) =
    match validate_layout room with
    | Error _ as error -> error
    | Ok () ->
        let seen = Hashtbl.create 32 in
        let rec validate = function
          | [] -> Ok ()
          | Gap _ :: rest -> validate rest
          | Events chunk :: rest ->
              if chunk.events = [] then Error "persisted events chunk is empty"
              else if
                List.exists
                  (fun event ->
                    event.stable_id = ""
                    ||
                    if Hashtbl.mem seen event.stable_id then true
                    else begin
                      Hashtbl.add seen event.stable_id ();
                      false
                    end)
                  chunk.events
              then Error "persisted room has invalid or duplicate event ids"
              else validate rest
        in
        validate room.chunks

  let validate_metadata (metadata : room_metadata) =
    let seen_chunks = Hashtbl.create (List.length metadata.chunks) in
    let seen_events = Hashtbl.create 32 in
    let rec validate previous_gap = function
      | [] ->
          if previous_gap then Error "persisted room layout ends with a gap"
          else Ok ()
      | chunk :: rest ->
          let chunk_id, is_gap, stable_ids =
            match chunk with
            | Gap_metadata gap -> (gap.gap_id, true, [])
            | Events_metadata chunk -> (chunk.chunk_id, false, chunk.stable_ids)
          in
          if chunk_id < 0 || chunk_id >= metadata.next_chunk_id then
            Error "persisted chunk id is outside the room counter"
          else if Hashtbl.mem seen_chunks chunk_id then
            Error "persisted room layout has duplicate chunk ids"
          else if previous_gap && is_gap then
            Error "persisted room layout has adjacent gaps"
          else if (not is_gap) && stable_ids = [] then
            Error "persisted events metadata is empty"
          else if
            List.exists
              (fun stable_id ->
                stable_id = ""
                ||
                if Hashtbl.mem seen_events stable_id then true
                else begin
                  Hashtbl.add seen_events stable_id ();
                  false
                end)
              stable_ids
          then Error "persisted room has invalid or duplicate event ids"
          else begin
            Hashtbl.add seen_chunks chunk_id ();
            validate is_gap rest
          end
    in
    if metadata.next_chunk_id < 0 then
      Error "persisted room counter is negative"
    else validate false metadata.chunks

  let validate_initial = function
    | Full room -> validate_room room
    | Tail { metadata; newest } -> (
        match validate_metadata metadata with
        | Error _ as error -> error
        | Ok () -> (
            match (List.rev metadata.chunks, newest) with
            | [], None -> Ok ()
            | Events_metadata expected :: _, Some loaded
              when expected.chunk_id = loaded.chunk_id
                   && expected.prev_token = loaded.prev_token
                   && expected.next_token = loaded.next_token
                   && expected.stable_ids
                      = List.map (fun event -> event.stable_id) loaded.events ->
                Ok ()
            | _ -> Error "persisted tail does not match its final metadata"))

  let room_events room =
    List.concat_map
      (function Events chunk -> chunk.events | Gap _ -> [])
      room.chunks

  let room_prev_batch room =
    match room.chunks with
    | Gap gap :: _ -> Some gap.token
    | Events chunk :: _ -> chunk.prev_token
    | [] -> None

  let room_has_gap room =
    List.exists (function Gap _ -> true | Events _ -> false) room.chunks

  type change =
    | Layout of room
    | Put_event of { chunk_id : int; position : int; event : event }
    | Replace_events_chunk of { chunk_id : int; events : event list }
    | Delete_event of { stable_id : string }
end

module type S = sig
  type t

  val load_room : t -> Id.Room_id.t -> (Internal.room option, Error.t) result
  val save_room : t -> Id.Room_id.t -> Internal.room -> (unit, Error.t) result

  val apply :
    t -> Id.Room_id.t -> Internal.change list -> (unit, Error.t) result

  val remove_room : t -> Id.Room_id.t -> (unit, Error.t) result
  val close : t -> unit
end

module type Lazy_S = sig
  include S

  val load_room_initial :
    t -> Id.Room_id.t -> (Internal.initial option, Error.t) result

  val load_events_chunk :
    t -> Id.Room_id.t -> int -> (Internal.events_chunk option, Error.t) result
end

type plaintext_policy = Store_plaintext | Ciphertext_only

type t = {
  load : Id.Room_id.t -> (Internal.room option, Error.t) result;
  load_initial : Id.Room_id.t -> (Internal.initial option, Error.t) result;
  load_chunk :
    Id.Room_id.t -> int -> (Internal.events_chunk option, Error.t) result;
  save : Id.Room_id.t -> Internal.room -> (unit, Error.t) result;
  write : Id.Room_id.t -> Internal.change list -> (unit, Error.t) result;
  remove : Id.Room_id.t -> (unit, Error.t) result;
  shutdown : unit -> unit;
  plaintext_policy : plaintext_policy;
  mutex : Eio.Mutex.t;
  mutable closed : bool;
}

let v (type backend) ?(plaintext_policy = Ciphertext_only)
    (module Backend : S with type t = backend) (backend : backend) =
  {
    load = Backend.load_room backend;
    load_initial =
      (fun room_id ->
        Result.map
          (Option.map (fun room -> Internal.Full room))
          (Backend.load_room backend room_id));
    load_chunk = (fun _room_id _chunk_id -> Ok None);
    save = Backend.save_room backend;
    write = Backend.apply backend;
    remove = Backend.remove_room backend;
    shutdown = (fun () -> Backend.close backend);
    plaintext_policy;
    mutex = Eio.Mutex.create ();
    closed = false;
  }

let v_lazy (type backend) ?(plaintext_policy = Ciphertext_only)
    (module Backend : Lazy_S with type t = backend) (backend : backend) =
  {
    load = Backend.load_room backend;
    load_initial = Backend.load_room_initial backend;
    load_chunk = Backend.load_events_chunk backend;
    save = Backend.save_room backend;
    write = Backend.apply backend;
    remove = Backend.remove_room backend;
    shutdown = (fun () -> Backend.close backend);
    plaintext_policy;
    mutex = Eio.Mutex.create ();
    closed = false;
  }

(* Bundled relations are a server-side projection and become stale as soon as
   a new reply arrives. Keep them in the live aggregate, never in the event
   chunks, just as matrix-rust-sdk does. *)
let strip_bundled (event : Raw.t) =
  {
    event with
    unsigned = Option.map Event.Unsigned.without_relations event.unsigned;
  }

let strip_event (event : Internal.event) =
  { event with Internal.event = strip_bundled event.event; clear_event = None }

let strip_relations_event (event : Internal.event) =
  {
    event with
    Internal.event = strip_bundled event.event;
    clear_event = Option.map strip_bundled event.clear_event;
  }

let bound_external_events events =
  let excess = List.length events - Internal.max_external_events in
  if excess <= 0 then events else List.drop excess events

let external_for_policy policy (event : Internal.event) =
  let event =
    match policy with
    | Store_plaintext -> strip_relations_event event
    | Ciphertext_only -> strip_event event
  in
  { event with delivery = Internal.Synced }

let copy_for_policy policy (room : Internal.room) =
  {
    room with
    Internal.chunks =
      List.map
        (function
          | Internal.Gap gap -> Internal.Gap gap
          | Internal.Events chunk ->
              Internal.Events
                {
                  chunk with
                  events =
                    (match policy with
                    | Store_plaintext ->
                        List.map strip_relations_event chunk.events
                    | Ciphertext_only -> List.map strip_event chunk.events);
                })
        room.chunks;
    Internal.external_events =
      room.external_events
      |> List.filter_map (fun (event : Internal.event) ->
          (* Detached records are keyed by the server event id. Match the
             event-cache registration path by ignoring unidentifiable input. *)
          if Option.is_some event.event.event_id then
            Some (external_for_policy policy event)
          else None)
      |> bound_external_events;
  }

let change_for_policy policy = function
  | Internal.Layout room -> Internal.Layout (copy_for_policy policy room)
  | Internal.Put_event put ->
      Internal.Put_event
        {
          put with
          event =
            (match policy with
            | Store_plaintext -> strip_relations_event put.event
            | Ciphertext_only -> strip_event put.event);
        }
  | Internal.Replace_events_chunk replace ->
      Internal.Replace_events_chunk
        {
          replace with
          events =
            (match policy with
            | Store_plaintext -> List.map strip_relations_event replace.events
            | Ciphertext_only -> List.map strip_event replace.events);
        }
  | Internal.Delete_event delete -> Internal.Delete_event delete

module Memory = struct
  type t = (string, Internal.room) Hashtbl.t

  let empty_room =
    { Internal.chunks = []; next_chunk_id = 0; external_events = [] }

  let without_event stable_id (chunk : Internal.events_chunk) =
    {
      chunk with
      Internal.events =
        List.filter
          (fun (held : Internal.event) ->
            not (String.equal held.stable_id stable_id))
          chunk.events;
    }

  let insert_at position event events =
    let position = Int.min position (List.length events) in
    List.take position events @ (event :: List.drop position events)

  (* The same shape as a persistent backend so that behaviour matches:
     [Layout] rewrites the structure and drops orphaned events, [Put_event]
     upserts by [stable_id] wherever the event was held. *)
  let replace_events_chunk (room : Internal.room) chunk_id events =
    let duplicate = Hashtbl.create (List.length events) in
    let bad_duplicate =
      List.exists
        (fun (event : Internal.event) ->
          if Hashtbl.mem duplicate event.stable_id then true
          else (
            Hashtbl.add duplicate event.stable_id ();
            false))
        events
    in
    let target =
      List.exists
        (function
          | Internal.Events chunk -> chunk.chunk_id = chunk_id
          | Internal.Gap _ -> false)
        room.chunks
    in
    let held_elsewhere = Hashtbl.create 16 in
    List.iter
      (function
        | Internal.Gap _ -> ()
        | Internal.Events chunk when chunk.chunk_id = chunk_id -> ()
        | Internal.Events chunk ->
            List.iter
              (fun (event : Internal.event) ->
                Hashtbl.replace held_elsewhere event.stable_id ())
              chunk.events)
      room.chunks;
    if events = [] then
      Error (Error.Backend "replacement events chunk is empty")
    else if not target then Error (Error.Backend "events chunk does not exist")
    else if bad_duplicate then Error (Error.Backend "duplicate stable event id")
    else if
      List.exists
        (fun (event : Internal.event) ->
          Hashtbl.mem held_elsewhere event.stable_id)
        events
    then Error (Error.Backend "stable event id belongs to another chunk")
    else
      Ok
        {
          room with
          Internal.chunks =
            List.map
              (function
                | Internal.Gap gap -> Internal.Gap gap
                | Internal.Events chunk when chunk.chunk_id = chunk_id ->
                    Internal.Events { chunk with events }
                | Internal.Events chunk -> Internal.Events chunk)
              room.chunks;
        }

  let apply_change (room : Internal.room) = function
    | Internal.Layout layout ->
        let held =
          List.filter_map
            (function
              | Internal.Events chunk -> Some (chunk.chunk_id, chunk.events)
              | Internal.Gap _ -> None)
            room.chunks
        in
        Ok
          {
            Internal.next_chunk_id = layout.next_chunk_id;
            external_events = bound_external_events layout.external_events;
            chunks =
              List.map
                (function
                  | Internal.Gap gap -> Internal.Gap gap
                  | Internal.Events chunk ->
                      Internal.Events
                        {
                          chunk with
                          events =
                            Option.value
                              (List.assoc_opt chunk.chunk_id held)
                              ~default:[];
                        })
                layout.chunks;
          }
    | Internal.Put_event { chunk_id; position; event } ->
        if
          not
            (List.exists
               (function
                 | Internal.Events chunk -> chunk.chunk_id = chunk_id
                 | Internal.Gap _ -> false)
               room.chunks)
        then Error (Error.Backend "events chunk does not exist")
        else
          Ok
            {
              room with
              Internal.chunks =
                List.map
                  (function
                    | Internal.Gap gap -> Internal.Gap gap
                    | Internal.Events chunk ->
                        let chunk = without_event event.stable_id chunk in
                        if chunk.chunk_id = chunk_id then
                          Internal.Events
                            {
                              chunk with
                              events = insert_at position event chunk.events;
                            }
                        else Internal.Events chunk)
                  room.chunks;
            }
    | Internal.Delete_event { stable_id } ->
        Ok
          {
            room with
            Internal.chunks =
              List.map
                (function
                  | Internal.Gap gap -> Internal.Gap gap
                  | Internal.Events chunk ->
                      Internal.Events (without_event stable_id chunk))
                room.chunks;
          }
    | Internal.Replace_events_chunk { chunk_id; events } ->
        replace_events_chunk room chunk_id events

  let metadata (room : Internal.room) : Internal.room_metadata =
    {
      Internal.chunks =
        List.map
          (function
            | Internal.Gap gap -> Internal.Gap_metadata gap
            | Internal.Events chunk ->
                Internal.Events_metadata
                  {
                    chunk_id = chunk.chunk_id;
                    prev_token = chunk.prev_token;
                    next_token = chunk.next_token;
                    stable_ids =
                      List.map
                        (fun (e : Internal.event) -> e.stable_id)
                        chunk.events;
                  })
          room.chunks;
      next_chunk_id = room.next_chunk_id;
      external_events = room.external_events;
    }

  let load_room_initial rooms room_id =
    match Hashtbl.find_opt rooms (Id.Room_id.to_string room_id) with
    | None -> Ok None
    | Some (room : Internal.room) -> (
        match List.rev room.chunks with
        | Internal.Gap _ :: _ ->
            Error (Error.Codec "persisted room layout ends with a gap")
        | _ ->
            let newest =
              List.rev room.chunks
              |> List.find_map (function
                | Internal.Events chunk -> Some chunk
                | Internal.Gap _ -> None)
            in
            Ok (Some (Internal.Tail { metadata = metadata room; newest })))

  let load_events_chunk rooms room_id chunk_id =
    match Hashtbl.find_opt rooms (Id.Room_id.to_string room_id) with
    | None -> Ok None
    | Some (room : Internal.room) ->
        Ok
          (List.find_map
             (function
               | Internal.Events chunk when chunk.chunk_id = chunk_id ->
                   Some chunk
               | _ -> None)
             room.chunks)

  let load_room rooms room_id =
    Ok (Hashtbl.find_opt rooms (Id.Room_id.to_string room_id))

  let save_room rooms room_id room =
    match Internal.validate_room room with
    | Error message -> Error (Error.Backend message)
    | Ok () ->
        Hashtbl.replace rooms
          (Id.Room_id.to_string room_id)
          {
            room with
            Internal.external_events =
              bound_external_events room.Internal.external_events;
          };
        Ok ()

  let apply rooms room_id changes =
    let key = Id.Room_id.to_string room_id in
    let room = Option.value (Hashtbl.find_opt rooms key) ~default:empty_room in
    let rec apply_all room = function
      | [] -> Ok room
      | change :: rest -> (
          match apply_change room change with
          | Error _ as error -> error
          | Ok room -> apply_all room rest)
    in
    match apply_all room changes with
    | Error _ as error -> error
    | Ok room -> (
        match Internal.validate_room room with
        | Error message -> Error (Error.Backend message)
        | Ok () ->
            Hashtbl.replace rooms key room;
            Ok ())

  let remove_room rooms room_id =
    Hashtbl.remove rooms (Id.Room_id.to_string room_id);
    Ok ()

  let close _ = ()
end

let memory ?plaintext_policy () =
  v_lazy ?plaintext_policy (module Memory) (Hashtbl.create 32)

let plaintext_policy t = t.plaintext_policy

let with_store t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if t.closed then Error Error.Closed else f ())

let load_room t room_id =
  with_store t (fun () ->
      Result.map
        (Option.map (copy_for_policy t.plaintext_policy))
        (t.load room_id))

let initial_for_policy policy = function
  | Internal.Full room -> Internal.Full (copy_for_policy policy room)
  | Internal.Tail { metadata; newest } ->
      let metadata =
        {
          metadata with
          Internal.external_events =
            metadata.external_events
            |> List.filter_map (fun (event : Internal.event) ->
                if Option.is_some event.event.event_id then
                  Some (external_for_policy policy event)
                else None)
            |> bound_external_events;
        }
      in
      let newest =
        Option.map
          (fun (chunk : Internal.events_chunk) ->
            {
              chunk with
              Internal.events =
                (match policy with
                | Store_plaintext ->
                    List.map
                      (fun (event : Internal.event) ->
                        strip_relations_event event)
                      chunk.events
                | Ciphertext_only ->
                    List.map
                      (fun (event : Internal.event) -> strip_event event)
                      chunk.events);
            })
          newest
      in
      Internal.Tail { metadata; newest }

let load_room_initial t room_id =
  with_store t (fun () ->
      match t.load_initial room_id with
      | Error _ as error -> error
      | Ok None -> Ok None
      | Ok (Some initial) -> (
          match Internal.validate_initial initial with
          | Error message -> Error (Error.Codec message)
          | Ok () -> Ok (Some (initial_for_policy t.plaintext_policy initial))))

let load_events_chunk t room_id chunk_id =
  with_store t (fun () ->
      Result.map
        (Option.map (fun (chunk : Internal.events_chunk) ->
             {
               chunk with
               Internal.events =
                 (match t.plaintext_policy with
                 | Store_plaintext ->
                     List.map
                       (fun (event : Internal.event) ->
                         strip_relations_event event)
                       chunk.events
                 | Ciphertext_only ->
                     List.map
                       (fun (event : Internal.event) -> strip_event event)
                       chunk.events);
             }))
        (t.load_chunk room_id chunk_id))

let save_room t room_id room =
  with_store t (fun () ->
      t.save room_id (copy_for_policy t.plaintext_policy room))

let apply t room_id changes =
  if changes = [] then Ok ()
  else
    with_store t (fun () ->
        t.write room_id
          (List.map (change_for_policy t.plaintext_policy) changes))

let remove_room t room_id = with_store t (fun () -> t.remove room_id)

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if not t.closed then (
        t.shutdown ();
        t.closed <- true))
