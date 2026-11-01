module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Paginator = Matrix_client.Thread_paginator

type state = Paginator.state =
  | Start
  | Loading
  | Next of string
  | End
  | Failed of Matrix_client.Error.t

type t = {
  room_id : Id.Room_id.t;
  thread_info : Thread_info.t;
  paginator : Paginator.t;
  admitted : (string, unit) Hashtbl.t;
  admitted_order : string list ref;
  rich : Thread_info.info Observable.List.t;
  mutable detach : (unit -> unit) option;
  mutable forget_subscription : (unit -> unit) option;
  closed : bool ref;
}

let clear_admitted t =
  Hashtbl.clear t.admitted;
  t.admitted_order := [];
  Observable.List.reconcile_by ~key:(fun _ -> ()) ~equal:( = ) t.rich []

let close_view t =
  if not !(t.closed) then (
    t.closed := true;
    Paginator.close t.paginator;
    Option.iter (fun detach -> detach ()) t.detach;
    t.detach <- None;
    Option.iter (fun unsubscribe -> unsubscribe ()) t.forget_subscription;
    t.forget_subscription <- None;
    clear_admitted t)

let create ~client ~thread_info ~room_id ?event_cache ?thread_cache () =
  let admitted = Hashtbl.create 32 in
  let admitted_order = ref [] in
  let rich = Observable.List.create [] in
  let reconcile values =
    let by_id =
      List.filter_map
        (fun (value : Thread_info.info) ->
          Option.map
            (fun id -> (Matrix_proto.Id.Event_id.to_string id, value))
            value.root.event_id)
        (Array.to_list values)
    in
    let values =
      List.filter_map (fun id -> List.assoc_opt id by_id) !admitted_order
    in
    Observable.List.reconcile_by
      ~key:(fun (value : Thread_info.info) ->
        Option.value
          (Option.map Matrix_proto.Id.Event_id.to_string value.root.event_id)
          ~default:"")
      ~equal:( = ) rich values
  in
  let closed = ref false in
  let on_info values = if not !closed then reconcile values in
  let on_root (root : Event.Raw_event.t) =
    if not !closed then
      match root.event_id with
      | None -> ()
      | Some event_id ->
          let id = Matrix_proto.Id.Event_id.to_string event_id in
          if not (Hashtbl.mem admitted id) then (
            Hashtbl.add admitted id ();
            admitted_order := !admitted_order @ [ id ]);
          Thread_info.ingest_root thread_info ~room_id root;
          (* Thread roots and the bundled latest reply are fetched outside the
             room timeline. Keep them in the shared detached-event index so
             cache-only consumers can resolve the same IDs. [ingest_root]
             performs the summary validation/merge, so use its resulting info
             rather than trusting malformed bundled data. *)
          Option.iter
            (fun cache ->
              if not (Event_cache.is_forgotten cache room_id) then begin
                Event_cache.register_external_event cache room_id ~event:root;
                match
                  Array.to_list (Thread_info.snapshot thread_info room_id)
                  |> List.find_opt (fun (value : Thread_info.info) ->
                      Option.equal Id.Event_id.equal value.root.event_id
                        (Some event_id))
                with
                | None -> ()
                | Some value ->
                    Option.iter
                      (fun latest ->
                        Event_cache.register_external_event cache room_id
                          ~event:latest)
                      value.latest_reply
              end)
            event_cache;
          Option.iter
            (fun cache ->
              let latest =
                Array.to_list (Thread_info.snapshot thread_info room_id)
                |> List.find_map (fun (value : Thread_info.info) ->
                    if
                      Option.equal Id.Event_id.equal value.root.event_id
                        (Some event_id)
                    then value.latest_reply
                    else None)
              in
              Thread_cache.ingest_thread cache ~room_id ~root_id:event_id
                ~events:(root :: Option.to_list latest))
            thread_cache
  in
  let paginator = Paginator.create ~on_root ~client ~room_id () in
  let t =
    {
      room_id;
      thread_info;
      paginator;
      admitted;
      admitted_order;
      rich;
      detach = None;
      forget_subscription = None;
      closed;
    }
  in
  t.detach <- Some (Thread_info.subscribe thread_info room_id on_info);
  Option.iter
    (fun cache ->
      let unsubscribe =
        Event_cache.subscribe_forget_room cache room_id (fun () -> close_view t)
      in
      (* Registration may observe an already completed forget and invoke the
         callback before it returns. Do not retain a stale subscription in
         that race. *)
      if !closed then unsubscribe ()
      else t.forget_subscription <- Some unsubscribe)
    event_cache;
  t

let set_filter t filter =
  if not !(t.closed) then (
    Paginator.set_filter t.paginator filter;
    clear_admitted t)

let reset t =
  if not !(t.closed) then (
    Paginator.reset t.paginator;
    clear_admitted t)

let close = close_view
let state t = Paginator.state t.paginator

let continuation t =
  match state t with
  | Next token -> Some token
  | Start | Loading | End | Failed _ -> None

let roots t = Paginator.roots t.paginator
let loaded_pages t = Paginator.loaded_pages t.paginator
let is_at_last_page t = Paginator.is_at_last_page t.paginator
let subscribe t callback = Paginator.subscribe t.paginator callback

let next_page t ?limit () =
  if !(t.closed) then Ok () else Paginator.next_page t.paginator ?limit ()

let infos t = t.rich
let snapshot t = Observable.List.snapshot t.rich
