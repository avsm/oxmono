module Id = Matrix_proto.Id
module Direction = Matrix_proto.Common.Direction
module Error = Matrix_client.Error

type priority = Low | Normal | High

type stop_reason =
  | Reached_start
  | Stop_condition
  | Batch_limit
  | No_data
  | Failed of Error.t
  | Stale
  | Cancelled
  | Forgotten
  | Closed

type run_result = {
  reason : stop_reason;
  events : Event_cache.event list;
  batches : int;
}

type request = {
  room_id : Id.Room_id.t;
  priority : priority;
  batch_size : int;
  max_batches : int option;
  stop : Event_cache.event list -> reached_start:bool -> bool;
}

type waiter = {
  promise : run_result Eio.Promise.t;
  resolver : run_result Eio.Promise.u;
  mutable active : bool;
}

type job = {
  request : request;
  sequence : int;
  mutable waiters : waiter list;
  mutable running : bool;
  mutable forced : stop_reason option;
  mutable cancel_run : (unit -> unit) option;
  mutable progress_events : Event_cache.event list;
  mutable progress_batches : int;
}

type t = {
  sw : Eio.Switch.t;
  client : Matrix_client.Client.t;
  event_cache : Event_cache.t;
  max_concurrent : int;
  mutex : Eio.Mutex.t;
  condition : Eio.Condition.t;
  mutable closed : bool;
  mutable next_sequence : int;
  mutable pending : job list;
  mutable jobs : job list;
  running_rooms : (string, unit) Hashtbl.t;
  room_subscriptions : (string, unit -> unit) Hashtbl.t;
}

type handle = { queue : t; job : job option; waiter : waiter }

let room_key room_id = Id.Room_id.to_string room_id

let compare_priority left right =
  match (left, right) with
  | High, High | Normal, Normal | Low, Low -> 0
  | High, _ -> 1
  | _, High -> -1
  | Normal, Low -> 1
  | Low, Normal -> -1

let better left right =
  let by_priority =
    compare_priority left.request.priority right.request.priority
  in
  if by_priority <> 0 then by_priority > 0 else left.sequence < right.sequence

let make_result reason events batches = { reason; events; batches }

let resolve_waiter waiter result =
  if waiter.active then begin
    waiter.active <- false;
    Eio.Promise.resolve waiter.resolver result
  end

let all_waiters_cancelled job =
  List.for_all (fun waiter -> not waiter.active) job.waiters

let remove_physically value values =
  List.filter (fun candidate -> candidate != value) values

let finish_job t job result =
  let waiters, unsubscribe =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        t.jobs <- remove_physically job t.jobs;
        if job.running then begin
          job.running <- false;
          Hashtbl.remove t.running_rooms (room_key job.request.room_id)
        end;
        job.cancel_run <- None;
        Eio.Condition.broadcast t.condition;
        let waiters = List.filter (fun waiter -> waiter.active) job.waiters in
        List.iter (fun waiter -> waiter.active <- false) waiters;
        let key = room_key job.request.room_id in
        let unsubscribe =
          if
            List.exists
              (fun other -> String.equal key (room_key other.request.room_id))
              t.jobs
          then None
          else
            let result = Hashtbl.find_opt t.room_subscriptions key in
            Option.iter
              (fun _ -> Hashtbl.remove t.room_subscriptions key)
              result;
            result
        in
        (waiters, unsubscribe))
  in
  List.iter (fun waiter -> Eio.Promise.resolve waiter.resolver result) waiters;
  Option.iter (fun unsubscribe -> unsubscribe ()) unsubscribe

let forced_reason job default = Option.value job.forced ~default

let rec select_pending t =
  List.fold_left
    (fun selected job ->
      let room = room_key job.request.room_id in
      if Hashtbl.mem t.running_rooms room then selected
      else
        match selected with
        | None -> Some job
        | Some current -> if better job current then Some job else selected)
    None t.pending

let take_job t =
  Eio.Mutex.lock t.mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock t.mutex)
    (fun () ->
      let rec wait () =
        if t.closed then None
        else
          match select_pending t with
          | None ->
              Eio.Condition.await t.condition t.mutex;
              wait ()
          | Some job ->
              t.pending <- remove_physically job t.pending;
              job.running <- true;
              Hashtbl.replace t.running_rooms (room_key job.request.room_id) ();
              Some job
      in
      wait ())

let current_token t room_id =
  Observable.Value.get (Event_cache.prev_batch t.event_cache room_id)

let execute_job t job =
  let request = job.request in
  let rec loop batches accumulated =
    match job.forced with
    | Some reason -> make_result reason accumulated batches
    | None -> (
        (* A persisted prefix is local history, not a server gap. Hydrate
           exactly one chunk before considering /messages. *)
        match Event_cache.hydrate_previous t.event_cache request.room_id with
        | Event_cache.Hydrated inserted -> (
            let batches = batches + 1 in
            let accumulated = inserted @ accumulated in
            job.progress_events <- accumulated;
            job.progress_batches <- batches;
            let reached_start =
              Option.is_none (current_token t request.room_id)
              && not
                   (Event_cache.has_unloaded_history t.event_cache
                      request.room_id)
            in
            if request.stop inserted ~reached_start then
              make_result Stop_condition accumulated batches
            else if reached_start then
              make_result Reached_start accumulated batches
            else
              match request.max_batches with
              | Some max when batches >= max ->
                  make_result Batch_limit accumulated batches
              | Some _ | None -> loop batches accumulated)
        | Event_cache.Hydration_failed error ->
            make_result
              (Failed (Error.Json_error (Event_store.Error.to_string error)))
              accumulated batches
        | Event_cache.No_persisted_history -> (
            match current_token t request.room_id with
            | None
              when Event_cache.has_unloaded_history t.event_cache
                     request.room_id ->
                make_result
                  (Failed
                     (Error.Json_error
                        "persisted Matrix history has no reachable predecessor"))
                  accumulated batches
            | None -> make_result Reached_start accumulated batches
            | Some token -> (
                match
                  Matrix_client.Messages.get_messages t.client
                    ~room_id:request.room_id ~from:token ~dir:Direction.Backward
                    ~limit:request.batch_size ()
                with
                | Error error -> make_result (Failed error) accumulated batches
                | Ok response -> (
                    match job.forced with
                    | Some reason -> make_result reason accumulated batches
                    | None -> (
                        let events = List.rev response.page.chunk in
                        match
                          Event_cache.prepend_if_token t.event_cache
                            request.room_id ~expected_prev_batch:token ~events
                            ~prev_batch:response.page.next_batch
                        with
                        | Event_cache.Forgotten ->
                            make_result Forgotten accumulated batches
                        | Event_cache.Stale ->
                            make_result Stale accumulated batches
                        | Event_cache.Applied applied -> (
                            let inserted = applied.inserted in
                            let batches = batches + 1 in
                            let accumulated = inserted @ accumulated in
                            job.progress_events <- accumulated;
                            job.progress_batches <- batches;
                            let reached_start = applied.reached_start in
                            if request.stop inserted ~reached_start then
                              make_result Stop_condition accumulated batches
                            else if reached_start then
                              make_result Reached_start accumulated batches
                            else if inserted = [] then
                              make_result No_data accumulated batches
                            else
                              match request.max_batches with
                              | Some max when batches >= max ->
                                  make_result Batch_limit accumulated batches
                              | Some _ | None -> loop batches accumulated))))))
  in
  loop 0 []

let run_job t job =
  try
    Eio.Cancel.sub (fun cc ->
        job.cancel_run <- Some (fun () -> Eio.Cancel.cancel cc Exit);
        Fun.protect
          ~finally:(fun () -> job.cancel_run <- None)
          (fun () -> execute_job t job))
  with
  | Eio.Cancel.Cancelled _ ->
      make_result
        (forced_reason job Cancelled)
        job.progress_events job.progress_batches
  | exn ->
      make_result
        (Failed (Error.Json_error (Printexc.to_string exn)))
        job.progress_events job.progress_batches

let worker t () =
  let rec loop () =
    match take_job t with
    | None -> ()
    | Some job ->
        let result = run_job t job in
        finish_job t job result;
        loop ()
  in
  try loop () with Eio.Cancel.Cancelled _ -> ()

let cancel_running jobs reason =
  List.iter
    (fun job ->
      job.forced <- Some reason;
      Option.iter (fun cancel -> cancel ()) job.cancel_run)
    jobs

let force_room t room_id reason =
  let key = room_key room_id in
  let queued_waiters, running, unsubscribe =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let matching =
          List.filter
            (fun job -> String.equal key (room_key job.request.room_id))
            t.jobs
        in
        let running = List.filter (fun job -> job.running) matching in
        let queued = List.filter (fun job -> not job.running) matching in
        List.iter (fun job -> job.forced <- Some reason) running;
        t.pending <-
          List.filter (fun job -> not (List.memq job queued)) t.pending;
        t.jobs <- List.filter (fun job -> List.memq job running) t.jobs;
        let waiters =
          List.concat_map (fun job -> job.waiters) queued
          |> List.filter (fun waiter -> waiter.active)
        in
        List.iter (fun waiter -> waiter.active <- false) waiters;
        let unsubscribe =
          if running <> [] then None
          else
            let result = Hashtbl.find_opt t.room_subscriptions key in
            Option.iter
              (fun _ -> Hashtbl.remove t.room_subscriptions key)
              result;
            result
        in
        Eio.Condition.broadcast t.condition;
        (waiters, running, unsubscribe))
  in
  List.iter
    (fun waiter ->
      Eio.Promise.resolve waiter.resolver (make_result reason [] 0))
    queued_waiters;
  cancel_running running reason;
  Option.iter (fun unsubscribe -> unsubscribe ()) unsubscribe

let ensure_room_subscription t room_id =
  let key = room_key room_id in
  let existing =
    Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.mem t.room_subscriptions key)
  in
  if not existing then begin
    let immediate = ref false in
    let unsubscribe =
      Event_cache.subscribe_forget_room t.event_cache room_id (fun () ->
          immediate := true;
          force_room t room_id Forgotten)
    in
    let retain =
      if !immediate then false
      else
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
            if t.closed || Hashtbl.mem t.room_subscriptions key then false
            else begin
              Hashtbl.replace t.room_subscriptions key unsubscribe;
              true
            end)
    in
    if not retain then unsubscribe ()
  end

let enqueue t request =
  if request.batch_size < 1 then
    invalid_arg "Matrix_ui.Back_pagination.enqueue: batch_size";
  Option.iter
    (fun max ->
      if max < 1 then
        invalid_arg "Matrix_ui.Back_pagination.enqueue: max_batches")
    request.max_batches;
  ensure_room_subscription t request.room_id;
  let promise, resolver = Eio.Promise.create () in
  let waiter = { promise; resolver; active = true } in
  let result =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then `Closed
        else if Event_cache.is_forgotten t.event_cache request.room_id then
          `Forgotten
        else
          match
            List.find_opt
              (fun job ->
                String.equal
                  (room_key job.request.room_id)
                  (room_key request.room_id)
                && job.request.priority = request.priority)
              t.jobs
          with
          | Some job ->
              job.waiters <- waiter :: job.waiters;
              `Job job
          | None ->
              let job =
                {
                  request;
                  sequence = t.next_sequence;
                  waiters = [ waiter ];
                  running = false;
                  forced = None;
                  cancel_run = None;
                  progress_events = [];
                  progress_batches = 0;
                }
              in
              t.next_sequence <- t.next_sequence + 1;
              t.jobs <- job :: t.jobs;
              t.pending <- job :: t.pending;
              Eio.Condition.broadcast t.condition;
              `Job job)
  in
  match result with
  | `Job job -> { queue = t; job = Some job; waiter }
  | `Closed ->
      resolve_waiter waiter (make_result Closed [] 0);
      { queue = t; job = None; waiter }
  | `Forgotten ->
      resolve_waiter waiter (make_result Forgotten [] 0);
      { queue = t; job = None; waiter }

let cancel handle =
  let t = handle.queue in
  let should_resolve, cancel_run, unsubscribe =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if not handle.waiter.active then (false, None, None)
        else begin
          handle.waiter.active <- false;
          match handle.job with
          | None -> (true, None, None)
          | Some job when all_waiters_cancelled job ->
              job.forced <- Some Cancelled;
              let unsubscribe =
                if not job.running then begin
                  t.pending <- remove_physically job t.pending;
                  t.jobs <- remove_physically job t.jobs;
                  Eio.Condition.broadcast t.condition;
                  let key = room_key job.request.room_id in
                  if
                    not
                      (List.exists
                         (fun other ->
                           String.equal key (room_key other.request.room_id))
                         t.jobs)
                  then begin
                    let result = Hashtbl.find_opt t.room_subscriptions key in
                    Option.iter
                      (fun _ -> Hashtbl.remove t.room_subscriptions key)
                      result;
                    result
                  end
                  else None
                end
                else begin
                  (* Detach a cancelled run immediately.  Its worker still owns
                   the room until transport cancellation unwinds, but a new
                   request may queue behind that occupancy instead of
                   coalescing onto a run which can no longer produce a result. *)
                  t.jobs <- remove_physically job t.jobs;
                  Eio.Condition.broadcast t.condition;
                  None
                end
              in
              (true, job.cancel_run, unsubscribe)
          | Some _ -> (true, None, None)
        end)
  in
  if should_resolve then
    Eio.Promise.resolve handle.waiter.resolver (make_result Cancelled [] 0);
  Option.iter (fun cancel -> cancel ()) cancel_run;
  Option.iter (fun unsubscribe -> unsubscribe ()) unsubscribe

let await handle =
  try Eio.Promise.await handle.waiter.promise
  with Eio.Cancel.Cancelled _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    cancel handle;
    Printexc.raise_with_backtrace exn bt

let close t =
  let queued_waiters, running, subscriptions =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then ([], [], [])
        else begin
          t.closed <- true;
          let queued = List.filter (fun job -> not job.running) t.jobs in
          let running = List.filter (fun job -> job.running) t.jobs in
          List.iter (fun job -> job.forced <- Some Closed) running;
          t.pending <- [];
          t.jobs <- running;
          let waiters =
            List.concat_map (fun job -> job.waiters) queued
            |> List.filter (fun waiter -> waiter.active)
          in
          List.iter (fun waiter -> waiter.active <- false) waiters;
          let subscriptions =
            Hashtbl.to_seq_values t.room_subscriptions |> List.of_seq
          in
          Hashtbl.clear t.room_subscriptions;
          Eio.Condition.broadcast t.condition;
          (waiters, running, subscriptions)
        end)
  in
  List.iter
    (fun waiter ->
      Eio.Promise.resolve waiter.resolver (make_result Closed [] 0))
    queued_waiters;
  cancel_running running Closed;
  List.iter (fun unsubscribe -> unsubscribe ()) subscriptions

let create ~sw ~client ~event_cache ?(max_concurrent = 3) () =
  if max_concurrent < 1 then invalid_arg "Matrix_ui.Back_pagination.create";
  let t =
    {
      sw;
      client;
      event_cache;
      max_concurrent;
      mutex = Eio.Mutex.create ();
      condition = Eio.Condition.create ();
      closed = false;
      next_sequence = 0;
      pending = [];
      jobs = [];
      running_rooms = Hashtbl.create 16;
      room_subscriptions = Hashtbl.create 16;
    }
  in
  for _ = 1 to max_concurrent do
    Eio.Fiber.fork_daemon ~sw (fun () ->
        worker t ();
        `Stop_daemon)
  done;
  Eio.Switch.on_release sw (fun () -> close t);
  t
