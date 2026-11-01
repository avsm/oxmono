module Queue = Matrix_client.Send_queue

type outcome =
  | Sent of Matrix_proto.Id.Event_id.t
  | Uploaded of Matrix_client.Send_queue.upload_result
  | Failed of Matrix_client.Error.t option
  | Cancelled
  | Timed_out

type status = Queued | Sending | Done of outcome

(* [Send_queue.on_change] has no counterpart that unregisters, so a bot
   installs one callback for its queue and this dispatches it to whichever
   sends are outstanding. Entries leave the table as they resolve, and the
   switch that made the tracker empties it on the way out. *)
type tracker = {
  queue : Queue.t;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  pending : (int, outcome Eio.Promise.u) Hashtbl.t;
  lock : Mutex.t;
  mutable live : bool;
}

type t = {
  request : Queue.request;
  tracker : tracker;
  promise : outcome Eio.Promise.t;
}

let outcome_of_status request = function
  | Queue.Sent event_id -> Some (Sent event_id)
  | Queue.Uploaded result -> Some (Uploaded result)
  | Queue.Wedged -> Some (Failed (Queue.last_error request))
  | Queue.Cancelled -> Some Cancelled
  | Queue.Pending | Queue.Sending -> None

let resolve tracker (request : Queue.request) =
  match outcome_of_status request (Queue.status request) with
  | None -> ()
  | Some outcome ->
      let resolver =
        Mutex.protect tracker.lock @@ fun () ->
        if not tracker.live then None
        else
          let id = Queue.id request in
          let found = Hashtbl.find_opt tracker.pending id in
          if Option.is_some found then Hashtbl.remove tracker.pending id;
          found
      in
      Option.iter
        (fun resolver -> Eio.Promise.resolve resolver outcome)
        resolver

let tracker ~sw ~clock queue =
  let tracker =
    {
      queue;
      clock;
      pending = Hashtbl.create 16;
      lock = Mutex.create ();
      live = true;
    }
  in
  Queue.on_change queue (resolve tracker);
  Eio.Switch.on_release sw (fun () ->
      Mutex.protect tracker.lock (fun () ->
          tracker.live <- false;
          Hashtbl.reset tracker.pending));
  tracker

let v tracker request =
  let promise, resolver = Eio.Promise.create () in
  (* Registered before the status is read, so that a request the queue
     finishes between the two resolves through the callback rather than
     being missed by both. *)
  Mutex.protect tracker.lock (fun () ->
      if tracker.live then
        Hashtbl.replace tracker.pending (Queue.id request) resolver);
  resolve tracker request;
  { request; tracker; promise }

let request t = t.request

let status t =
  match Queue.status t.request with
  | Queue.Pending -> Queued
  | Queue.Sending -> Sending
  | Queue.Sent event_id -> Done (Sent event_id)
  | Queue.Uploaded result -> Done (Uploaded result)
  | Queue.Wedged -> Done (Failed (Queue.last_error t.request))
  | Queue.Cancelled -> Done Cancelled

let await ?(timeout = 60.) t =
  match
    Eio.Time.with_timeout t.tracker.clock timeout (fun () ->
        Ok (Eio.Promise.await t.promise))
  with
  | Ok outcome -> outcome
  | Error `Timeout -> Timed_out

let cancel t =
  match Queue.cancel t.tracker.queue t.request with
  | `Cancelled ->
      resolve t.tracker t.request;
      `Cancelled
  | (`Already_sent | `In_flight) as answer -> answer

module Internal = struct
  type nonrec tracker = tracker

  let tracker = tracker
  let v = v
end
