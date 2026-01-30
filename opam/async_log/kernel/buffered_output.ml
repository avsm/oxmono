open! Core
open! Async_kernel
open! Import

module Update = struct
  type t =
    | Msg of Message_event.Unstable.t
    | Flush of unit Ivar.t
    | Rotate of unit Ivar.t
  [@@deriving sexp_of]
end

module State = struct
  type t =
    { write : Message_event.t Queue.t -> unit Deferred.t
    ; rotate : unit -> unit Deferred.t
    ; flush : unit -> unit Deferred.t
    ; msgs : Message_event.t Queue.t
    }
  [@@deriving fields ~getters]

  (*=Experimentally
     - based on 100M log lines generated in 1k async cycles
     - long async cycles is the longest cycle >1ms and ignoring long async cycles from
       the initial log generation


                              long async cycles 
     | batch_size | runtime | 1-10ms | 100ms | 1s | 10s | 100s |
     +------------+---------+--------+-------+----+-----+------+
     |     legacy | 1m37s   |      3 |    68 |  4 |   2 |    1 |
     |         10 | 3m26s   |    591 |    18 |  4 |     |      |
     |         20 | 2m33s   |    216 |     3 |  5 |     |      |
     |         50 | 2m02s   |    162 |     3 |  4 |     |      |
     |        100 | 1m49s   |    541 |    10 |  5 |     |      |
     |        200 | 1m44s   |    148 |     3 |  4 |     |      |
     |        500 | 1m43s   |    306 |    18 |  5 |     |      |
     |       1000 | 1m43s   |  18365 |    13 |  4 |     |      |
     |       2000 | 1m46s   |  48984 |    24 |  5 |     |      |
     |       5000 | 1m41s   |  18971 |    36 |  4 |     |      |
     |      10000 | 1m38s   |   8584 |   426 |  3 |     |      |
  *)
  let batch_size = 500
  let create ~flush ~write ~rotate = { flush; write; rotate; msgs = Queue.create () }

  let write_msgs_to_output t ~and_then =
    if Queue.length t.msgs = 0
    then and_then ()
    else (
      let%bind () = t.write t.msgs in
      Queue.clear t.msgs;
      and_then ())
  ;;

  let process_exn t (updates : Update.t Queue.t) =
    (* Some special scheduling properties of this processor loop that may explain why it's
       not a [Deferred.repeat_until_finished] is
       (1) if a [Msg] is processed, there's no new async job created
       (2) If [Queue.length msgs = 0], then [f] is called immediately. *)
    let rec loop yield_every =
      if yield_every = 0
      then
        (* this introduces a yield point so that other async jobs have a chance to run
           under circumstances when large batches of logs are delivered in bursts. *)
        write_msgs_to_output t ~and_then:(fun () ->
          let%bind () = Async_kernel_scheduler.yield () in
          loop batch_size)
      else (
        (* only decrement [yield_every] inside this branch so we actually consume
           [batch_size] items from the pipe before yielding. *)
        let yield_every = yield_every - 1 in
        match Queue.dequeue updates with
        | None -> write_msgs_to_output t ~and_then:return
        | Some update ->
          (match update with
           | Rotate i ->
             write_msgs_to_output t ~and_then:(fun () ->
               let%bind () = t.rotate () in
               Ivar.fill_exn i ();
               loop yield_every)
           | Flush i ->
             write_msgs_to_output t ~and_then:(fun () ->
               let%bind () = t.flush () in
               Ivar.fill_exn i ();
               loop yield_every)
           | Msg msg ->
             Queue.enqueue t.msgs msg;
             loop yield_every))
    in
    loop batch_size
  ;;
end

type t =
  { updates : Update.t Pipe.Writer.t
  ; background_error : exn Deferred.t
      (* Some clients call [flushed] so much that merging consecutive [flush] operations
         makes a big difference to their performance. *)
  ; mutable last_update : [ `Flush of unit Deferred.t | `Not_a_flush ]
  }
[@@deriving fields ~getters]

let create ~flush ~rotate ~write =
  let r, w = Pipe.create () in
  let process_log = State.create ~write ~rotate ~flush in
  let background_error =
    match%map
      Monitor.try_with ~rest:`Log (fun () ->
        Pipe.iter' r ~f:(State.process_exn process_log))
    with
    | Ok () -> raise_s [%message "Bug: Log processor pipe closed unexpectedly"]
    | Error exn -> exn
  in
  { updates = w; background_error; last_update = `Not_a_flush }
;;

let push_update t (update : Update.t) =
  t.last_update
  <- (match update with
      | Flush i -> `Flush (Ivar.read i)
      | Msg _ | Rotate _ -> `Not_a_flush);
  Pipe.write_without_pushback t.updates update
;;

let flushed t =
  match t.last_update with
  | `Flush f -> f
  | `Not_a_flush -> Deferred.create (fun i -> push_update t (Flush i))
;;

let rotate t = Deferred.create (fun i -> push_update t (Rotate i))
let write t msg = push_update t (Msg msg)

module Private = struct
  let batch_size = State.batch_size
end
