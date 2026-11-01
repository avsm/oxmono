type outcome =
  | Sent of Zulip.Id.Message.t
  | Failed of Zulip_eio.Error.t
  | Indeterminate of Zulip_eio.Error.t option
  | Cancelled

type status = Queued | Sending | Done of outcome

type tracker = {
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  lock : Eio.Mutex.t;
  mutable open_ : bool;
  pending : t list ref;
  closed : unit Eio.Promise.t;
  close_ : unit Eio.Promise.u;
}

and t = {
  tracker : tracker;
  promise : outcome Eio.Promise.t;
  resolve_ : outcome Eio.Promise.u;
  mutable state : status;
}

let tracker ~clock =
  let closed, close_ = Eio.Promise.create () in
  {
    clock;
    lock = Eio.Mutex.create ();
    open_ = true;
    pending = ref [];
    closed;
    close_;
  }

let await_closed tracker = Eio.Promise.await tracker.closed

let v tracker =
  let promise, resolve_ = Eio.Promise.create () in
  let t = { tracker; promise; resolve_; state = Queued } in
  Eio.Mutex.use_rw ~protect:true tracker.lock (fun () ->
      if tracker.open_ then tracker.pending := t :: !(tracker.pending)
      else (
        t.state <- Done Cancelled;
        Eio.Promise.resolve resolve_ Cancelled));
  t

let status t = Eio.Mutex.use_ro t.tracker.lock (fun () -> t.state)

let resolve t outcome =
  Eio.Mutex.use_rw ~protect:true t.tracker.lock @@ fun () ->
  match t.state with
  | Done _ -> ()
  | Queued | Sending ->
      t.state <- Done outcome;
      t.tracker.pending :=
        List.filter (fun pending -> pending != t) !(t.tracker.pending);
      Eio.Promise.resolve t.resolve_ outcome

let begin_send t =
  Eio.Mutex.use_rw ~protect:true t.tracker.lock @@ fun () ->
  match t.state with
  | Queued when t.tracker.open_ ->
      t.state <- Sending;
      true
  | Queued | Sending | Done _ -> false

let cancel t =
  Eio.Mutex.use_rw ~protect:true t.tracker.lock @@ fun () ->
  match t.state with
  | Queued ->
      t.state <- Done Cancelled;
      t.tracker.pending :=
        List.filter (fun pending -> pending != t) !(t.tracker.pending);
      Eio.Promise.resolve t.resolve_ Cancelled;
      `Cancelled
  | Sending -> `In_flight
  | Done outcome -> `Settled outcome

let await ?(timeout = 60.) t =
  if timeout < 0. || Float.is_nan timeout then
    invalid_arg "Zulip_bot.Sent.await";
  match
    Eio.Time.with_timeout t.tracker.clock timeout (fun () ->
        Ok (Eio.Promise.await t.promise))
  with
  | Ok value -> `Done value
  | Error `Timeout -> `Timed_out

let close tracker =
  let pending =
    Eio.Mutex.use_rw ~protect:true tracker.lock @@ fun () ->
    if tracker.open_ then (
      tracker.open_ <- false;
      Eio.Promise.resolve tracker.close_ ());
    let pending = !(tracker.pending) in
    tracker.pending := [];
    List.map
      (fun t ->
        ( t,
          match t.state with
          | Queued -> Cancelled
          | Sending -> Indeterminate None
          | Done outcome -> outcome ))
      pending
  in
  List.iter (fun (t, outcome) -> resolve t outcome) pending
