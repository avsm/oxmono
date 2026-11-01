module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Paginator = Matrix_client.Paginator

type state = Initial | Fetching_target | Idle | Paginating | Closed
type thread_mode = Paginator.thread_mode = Automatic | Force
type page = Paginator.page = { events : Event.Raw_event.t list; hit_end : bool }

type start_result = Paginator.start_result = {
  events : Event.Raw_event.t list;
  has_previous : bool;
  has_next : bool;
}

type error =
  | Event_not_found of Id.Event_id.t
  | Invalid_state of { expected : state; actual : state }
  | Client_error of Matrix_client.Error.t

type t = {
  paginator : Paginator.t;
  room_id : Id.Room_id.t;
  event_id : Id.Event_id.t;
  event_cache : Event_cache.t option;
  thread_cache : Thread_cache.t option;
  context_limit : int;
  events : Event.Raw_event.t Observable.List.t;
  mutex : Eio.Mutex.t;
  mutable state : state;
  mutable generation : int;
  mutable next_subscription : int;
  subscribers : (int, state -> unit) Hashtbl.t;
  mutable forget_subscription : (unit -> unit) option;
}

let callback_snapshot t = Hashtbl.to_seq_values t.subscribers |> List.of_seq

let publish callbacks state =
  List.iter
    (fun callback ->
      try callback state with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Logs.warn (fun m ->
              m "ui: event-focused listener failed: %s" (Printexc.to_string exn)))
    callbacks

let close_view t =
  let outcome, unsubscribe =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.state = Closed then (`Closed, None)
        else (
          t.generation <- t.generation + 1;
          Observable.List.reconcile_by
            ~key:(fun event ->
              Option.value
                (Option.map Id.Event_id.to_string event.Event.Raw_event.event_id)
                ~default:"")
            ~equal:( = ) t.events [];
          t.state <- Closed;
          let unsubscribe = t.forget_subscription in
          t.forget_subscription <- None;
          (`Done (callback_snapshot t), unsubscribe)))
  in
  Option.iter (fun unsubscribe -> unsubscribe ()) unsubscribe;
  match outcome with
  | `Closed -> ()
  | `Done callbacks -> publish callbacks Closed

let create ~client ~room_id ~event_id ?event_cache ?thread_cache ?(limit = 10)
    ?(thread_mode = Automatic) () =
  if limit < 0 then invalid_arg "Matrix_ui.Event_focused.create: limit";
  let t =
    {
      paginator = Paginator.create ~client ~room_id ~thread_mode ();
      room_id;
      event_id;
      event_cache;
      thread_cache;
      context_limit = limit;
      events = Observable.List.create [];
      mutex = Eio.Mutex.create ();
      state = Initial;
      generation = 0;
      next_subscription = 0;
      subscribers = Hashtbl.create 4;
      forget_subscription = None;
    }
  in
  Option.iter
    (fun cache ->
      let unsubscribe =
        Event_cache.subscribe_forget_room cache room_id (fun () -> close_view t)
      in
      (* [forget_room] can win between registering the callback and installing
         its unsubscribe handle. Keep the view closed in that case and avoid
         retaining a stale callback. *)
      let retain =
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
            if t.state = Closed then false
            else (
              t.forget_subscription <- Some unsubscribe;
              true))
      in
      if not retain then unsubscribe ())
    event_cache;
  t

let events t = t.events
let snapshot t = Observable.List.snapshot t.events
let state t = Eio.Mutex.use_ro t.mutex (fun () -> t.state)

let subscribe t callback =
  let id, current =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let id = t.next_subscription in
        t.next_subscription <- id + 1;
        Hashtbl.replace t.subscribers id callback;
        (id, t.state))
  in
  (try callback current
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
         Hashtbl.remove t.subscribers id);
     Printexc.raise_with_backtrace exn bt);
  let subscribed = ref true in
  fun () ->
    if !subscribed then (
      subscribed := false;
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          Hashtbl.remove t.subscribers id))

let pp_state ppf = function
  | Initial -> Format.pp_print_string ppf "initial"
  | Fetching_target -> Format.pp_print_string ppf "fetching target"
  | Idle -> Format.pp_print_string ppf "idle"
  | Paginating -> Format.pp_print_string ppf "paginating"
  | Closed -> Format.pp_print_string ppf "closed"

let pp_error ppf = function
  | Event_not_found event_id ->
      Format.fprintf ppf "target event %s was not found"
        (Id.Event_id.to_string event_id)
  | Invalid_state { expected; actual } ->
      Format.fprintf ppf "expected event-focused state %a, observed %a" pp_state
        expected pp_state actual
  | Client_error error -> Matrix_client.Error.pp ppf error

let map_state = function
  | Paginator.Initial -> Initial
  | Paginator.Fetching_target -> Fetching_target
  | Paginator.Idle -> Idle
  | Paginator.Paginating -> Paginating

let map_error = function
  | Paginator.Event_not_found event_id -> Event_not_found event_id
  | Paginator.Invalid_state { expected; actual } ->
      Invalid_state { expected = map_state expected; actual = map_state actual }
  | Paginator.Client_error error -> Client_error error

let invalid_state ~expected actual = Invalid_state { expected; actual }

(* Registering while holding the view lock closes the small race between the
   generation check and [close]/[reset].  [Event_cache.register_external_event]
   takes its own lock but never calls back into this view, so this lock order is
   safe.  A forgotten room remains authoritative: the cache ignores the write
   even if a request completed just before the forget. *)
let register_if_active t generation expected events =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if t.generation = generation && t.state = expected then begin
        Option.iter
          (fun cache ->
            List.iter
              (fun (event : Event.Raw_event.t) ->
                Event_cache.register_external_event cache t.room_id ~event)
              events)
          t.event_cache;
        Option.iter
          (fun cache ->
            let thread_root = Paginator.thread_root t.paginator in
            (match thread_root with
            | Some root_id ->
                Thread_cache.ingest_thread cache ~room_id:t.room_id ~root_id
                  ~events
            | None -> Thread_cache.ingest cache ~room_id:t.room_id ~events);
            let tokens = Paginator.tokens t.paginator in
            Option.iter
              (fun root_id ->
                Thread_cache.set_pagination cache ~room_id:t.room_id ~root_id
                  ~backward:tokens.previous ~forward:tokens.next)
              thread_root)
          t.thread_cache
      end)

(* Claiming the wrapper state is atomic.  The network call happens after the
   lock is released, while [generation] identifies the completion allowed to
   publish. *)
let begin_call t ~expected ~next =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match t.state with
        | Closed -> `Closed
        | actual when actual <> expected ->
            `Error (invalid_state ~expected actual)
        | _ ->
            t.state <- next;
            t.generation <- t.generation + 1;
            `Run (t.generation, callback_snapshot t))
  in
  match outcome with
  | `Run (generation, callbacks) ->
      (try publish callbacks next
       with Eio.Cancel.Cancelled _ as exn ->
         let bt = Printexc.get_raw_backtrace () in
         Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
             if t.generation = generation && t.state = next then
               t.state <- expected);
         Printexc.raise_with_backtrace exn bt);
      `Run generation
  | `Closed -> `Closed
  | `Error error -> `Error error

let still_active t generation expected =
  Eio.Mutex.use_ro t.mutex (fun () ->
      t.generation = generation && t.state = expected)

let closed_start = { events = []; has_previous = false; has_next = false }
let closed_page = { events = []; hit_end = true }

(* Reconcile while holding [mutex].  [close] consequently cannot clear the
   list between the generation check and this write. *)
let finish t generation ~next_state ~events ~result =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.state = Closed || t.generation <> generation then `Closed
        else (
          Option.iter
            (fun events ->
              Observable.List.reconcile_by
                ~key:(fun event ->
                  Option.value
                    (Option.map Id.Event_id.to_string
                       event.Event.Raw_event.event_id)
                    ~default:"")
                ~equal:( = ) t.events events)
            events;
          t.state <- next_state;
          `Done (callback_snapshot t)))
  in
  match outcome with
  | `Closed -> `Closed
  | `Done callbacks ->
      publish callbacks next_state;
      `Done result

let restore_after_exception t generation next_state =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.state = Closed || t.generation <> generation then `Closed
        else (
          t.state <- next_state;
          `Restored (callback_snapshot t)))
  in
  match outcome with
  | `Closed -> ()
  | `Restored callbacks -> publish callbacks next_state

let start t () =
  match begin_call t ~expected:Initial ~next:Fetching_target with
  | `Closed -> Ok closed_start
  | `Error error -> Error error
  | `Run generation -> (
      if not (still_active t generation Fetching_target) then Ok closed_start
      else
        let result =
          try
            Paginator.start_from t.paginator ~event_id:t.event_id
              ~limit:t.context_limit ()
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            restore_after_exception t generation Initial;
            Printexc.raise_with_backtrace exn bt
        in
        match result with
        | Error error -> (
            match
              finish t generation ~next_state:Initial ~events:None ~result:()
            with
            | `Closed -> Ok closed_start
            | `Done () -> Error (map_error error))
        | Ok result -> (
            register_if_active t generation Fetching_target result.events;
            match
              finish t generation ~next_state:Idle ~events:(Some result.events)
                ~result
            with
            | `Closed -> Ok closed_start
            | `Done result -> Ok result))

let check_limit name = function
  | Some limit when limit < 0 -> invalid_arg name
  | _ -> ()

let paginate_backward t ?limit () =
  check_limit "Matrix_ui.Event_focused.paginate_backward" limit;
  match begin_call t ~expected:Idle ~next:Paginating with
  | `Closed -> Ok closed_page
  | `Error error -> Error error
  | `Run generation -> (
      if not (still_active t generation Paginating) then Ok closed_page
      else
        let result =
          try Paginator.paginate_backward t.paginator ?limit ()
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            restore_after_exception t generation Idle;
            Printexc.raise_with_backtrace exn bt
        in
        match result with
        | Error error -> (
            match
              finish t generation ~next_state:Idle ~events:None ~result:()
            with
            | `Closed -> Ok closed_page
            | `Done () -> Error (map_error error))
        | Ok page -> (
            let events = List.rev page.events in
            register_if_active t generation Paginating page.events;
            let current = Array.to_list (snapshot t) in
            match
              finish t generation ~next_state:Idle
                ~events:(Some (events @ current))
                ~result:{ page with events }
            with
            | `Closed -> Ok closed_page
            | `Done page -> Ok page))

let paginate_forward t ?limit () =
  check_limit "Matrix_ui.Event_focused.paginate_forward" limit;
  match begin_call t ~expected:Idle ~next:Paginating with
  | `Closed -> Ok closed_page
  | `Error error -> Error error
  | `Run generation -> (
      if not (still_active t generation Paginating) then Ok closed_page
      else
        let result =
          try Paginator.paginate_forward t.paginator ?limit ()
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            restore_after_exception t generation Idle;
            Printexc.raise_with_backtrace exn bt
        in
        match result with
        | Error error -> (
            match
              finish t generation ~next_state:Idle ~events:None ~result:()
            with
            | `Closed -> Ok closed_page
            | `Done () -> Error (map_error error))
        | Ok page -> (
            register_if_active t generation Paginating page.events;
            let current = Array.to_list (snapshot t) in
            match
              finish t generation ~next_state:Idle
                ~events:(Some (current @ page.events))
                ~result:page
            with
            | `Closed -> Ok closed_page
            | `Done page -> Ok page))

let reset t =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match t.state with
        | Closed -> `Closed
        | Initial | Idle -> (
            match Paginator.reset t.paginator with
            | Error error -> `Error (map_error error)
            | Ok () ->
                Observable.List.reconcile_by
                  ~key:(fun event ->
                    Option.value
                      (Option.map Id.Event_id.to_string
                         event.Event.Raw_event.event_id)
                      ~default:"")
                  ~equal:( = ) t.events [];
                t.generation <- t.generation + 1;
                t.state <- Initial;
                `Done (callback_snapshot t))
        | actual -> `Error (invalid_state ~expected:Idle actual))
  in
  match outcome with
  | `Closed -> Ok ()
  | `Error error -> Error error
  | `Done callbacks ->
      publish callbacks Initial;
      Ok ()

let close = close_view
