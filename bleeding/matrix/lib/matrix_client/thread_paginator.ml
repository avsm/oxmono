module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type state = Start | Loading | Next of string | End | Failed of Error.t

type t = {
  client : Client.t;
  room_id : Id.Room_id.t;
  on_root : (Event.Raw_event.t -> unit) option;
  mutable filter : Relations.thread_filter;
  mutable token : string option;
  mutable terminal : bool;
  mutable loading : bool;
  mutable status : state;
  mutable pages : int;
  mutable roots : Event.Raw_event.t list;
  seen : (string, unit) Hashtbl.t;
  mutable next_subscription : int;
  subscribers : (int, state -> unit) Hashtbl.t;
  mutable closed : bool;
}

let create ?on_root ~client ~room_id () =
  {
    client;
    room_id;
    on_root;
    filter = Relations.All;
    token = None;
    terminal = false;
    loading = false;
    status = Start;
    pages = 0;
    roots = [];
    seen = Hashtbl.create 64;
    next_subscription = 0;
    subscribers = Hashtbl.create 4;
    closed = false;
  }

let publish t state =
  if t.status <> state then (
    t.status <- state;
    Hashtbl.to_seq_values t.subscribers
    |> List.of_seq
    |> List.iter (fun callback -> callback state))

let subscribe t callback =
  let id = t.next_subscription in
  t.next_subscription <- id + 1;
  Hashtbl.replace t.subscribers id callback;
  callback t.status;
  let subscribed = ref true in
  fun () ->
    if !subscribed then (
      subscribed := false;
      Hashtbl.remove t.subscribers id)

let set_filter t filter =
  if not t.closed then begin
    if t.loading then invalid_arg "Thread_paginator.set_filter: loading";
    t.filter <- filter;
    t.token <- None;
    t.terminal <- false;
    t.pages <- 0;
    t.roots <- [];
    Hashtbl.clear t.seen;
    publish t Start
  end

let reset t = set_filter t t.filter

let close t =
  if not t.closed then begin
    t.closed <- true;
    t.loading <- false;
    t.token <- None;
    t.terminal <- false;
    t.status <- Start;
    t.pages <- 0;
    t.roots <- [];
    Hashtbl.clear t.seen;
    Hashtbl.clear t.subscribers
  end

let state t = t.status
let roots t = t.roots
let loaded_pages t = t.pages
let is_at_last_page t = t.closed || t.terminal

let event_key (e : Event.Raw_event.t) =
  Option.map Id.Event_id.to_string e.event_id

let next_page t ?(limit = 30) () =
  if limit < 1 then invalid_arg "Thread_paginator.next_page: limit";
  if t.closed || t.loading || t.terminal then Ok ()
  else begin
    t.loading <- true;
    let preceding = t.status in
    publish t Loading;
    let result =
      try
        Relations.list_threads t.client ~room_id:t.room_id ~filter:t.filter
          ?from:t.token ~limit ()
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        t.loading <- false;
        if not t.closed then publish t preceding;
        Printexc.raise_with_backtrace exn bt
    in
    t.loading <- false;
    if t.closed then Ok ()
    else
      match result with
      | Error e ->
          publish t (Failed e);
          Error e
      | Ok page ->
          let identified =
            List.filter
              (fun (e : Event.Raw_event.t) -> Option.is_some (event_key e))
              page.chunk
          in
          let page_seen = Hashtbl.create (List.length identified) in
          let fresh =
            List.filter
              (fun (e : Event.Raw_event.t) ->
                match event_key e with
                | None -> false
                | Some id ->
                    if Hashtbl.mem t.seen id || Hashtbl.mem page_seen id then
                      false
                    else (
                      Hashtbl.add page_seen id ();
                      true))
              page.chunk
          in
          (try
             Option.iter (fun on_root -> List.iter on_root identified) t.on_root
           with exn ->
             let bt = Printexc.get_raw_backtrace () in
             (* A UI callback is outside the transport transaction. Leave the
              token, roots, seen IDs and page count untouched so a retry sees
              the same page, but never leave observers stuck in [Loading]. *)
             publish t preceding;
             Printexc.raise_with_backtrace exn bt);
          List.iter
            (fun (event : Event.Raw_event.t) ->
              match event_key event with
              | Some id -> Hashtbl.replace t.seen id ()
              | None -> ())
            fresh;
          t.roots <- t.roots @ fresh;
          t.pages <- t.pages + 1;
          (match page.next_batch with
          | None ->
              t.token <- None;
              t.terminal <- true;
              publish t End
          | Some token ->
              t.token <- Some token;
              t.terminal <- false;
              publish t (Next token));
          Ok ()
  end
