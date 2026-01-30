open! Core
open! Async_kernel
open! Import
module Id = Unique_id.Int ()

module Control_event = struct
  type t = Set_level of Level.t [@@deriving globalize, sexp_of]
end

type t =
  { id : Id.t
  ; on_error : On_error.t ref
  ; mutable is_closed : bool
  ; mutable level : Level.t
  ; output : Mutable_outputs.t
  ; mutable time_source : Synchronous_time_source.t
  ; transforms : (Message_event.t -> Message_event.t option) Doubly_linked.t
  ; control_events : (local_ Control_event.t -> unit) Bus.Read_write.t
  }

let control_events t = Bus.read_only t.control_events

let assert_open t tag =
  if t.is_closed then failwithf "Log: can't %s because this log has been closed" tag ()
;;

let flushed t =
  (* 2025-10 - We don't [assert_open] here. In a way, it's not necessary, but we also
     found that if you have a log A with an output that writes and flushes to another log
     B, and then stop using both, [flush_and_close] can be called on both logs, but in a
     nondeterministic order. If B is closed before A, then A tries to flush, an
     [assert_open] here would raise. *)
  Mutable_outputs.flushed t.output
;;

let is_closed t = t.is_closed

let flush_and_close t =
  if not (is_closed t)
  then (
    let finished = flushed t in
    t.is_closed <- true;
    upon finished (fun () -> Bus.close t.control_events);
    finished)
  else return ()
;;

let close = flush_and_close

let live_logs =
  lazy
    (Live_entry_registry.create
       (module struct
         type nonrec t = t

         let equal t1 t2 = Id.equal t1.id t2.id
         let hash t1 = Id.hash t1.id
         let flushed = flushed
         let is_closed = is_closed
         let flush_and_close = flush_and_close
       end))
;;

let create ~level ~default_outputs ~named_outputs ~on_error ~time_source ~transforms =
  let time_source =
    match Option.map time_source ~f:Synchronous_time_source.read_only with
    | Some time_source -> time_source
    | None ->
      if Ppx_inline_test_lib.am_running
      then Synchronous_time_source.(read_only (create ~now:Time_ns.epoch ()))
      else Synchronous_time_source.wall_clock ()
  in
  let on_error = ref on_error in
  let output =
    Mutable_outputs.create
      ~default_outputs
      ~named_outputs
      ~on_background_output_error:(fun exn -> On_error.handle_error !on_error exn)
  in
  let id = Id.create () in
  let control_events =
    Bus.create_exn
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:(ignore : Error.t -> unit)
      ()
  in
  let transforms = Doubly_linked.of_list transforms in
  let t =
    { id
    ; on_error
    ; level
    ; output
    ; time_source
    ; transforms
    ; is_closed = false
    ; control_events
    }
  in
  Live_entry_registry.register (force live_logs) t;
  t
;;

let set_output t new_outputs =
  assert_open t "set output";
  Mutable_outputs.update_default_outputs t.output new_outputs
;;

let get_output t = Mutable_outputs.current_default_outputs t.output
let get_named_outputs t = Mutable_outputs.current_named_outputs t.output
let get_on_error t = !(t.on_error)
let set_on_error t handler = t.on_error := handler
let level t = t.level

let set_level t level =
  match Level.equal level t.level with
  | true -> ()
  | false ->
    t.level <- level;
    let local_ control_event = Control_event.Set_level level in
    Bus.write_local t.control_events control_event [@nontail]
;;

let get_time_source t = t.time_source

let set_time_source t time_source =
  t.time_source <- Synchronous_time_source.read_only time_source
;;

let has_transform t = not (Doubly_linked.is_empty t.transforms)

module Transform = struct
  type t = (Message_event.t -> Message_event.t option) Doubly_linked.Elt.t

  let add log f = function
    | `Before -> Doubly_linked.insert_first log.transforms f
    | `After -> Doubly_linked.insert_last log.transforms f
  ;;

  let remove_exn log t =
    (* [Doubly_linked.remove] can raise if the transform is not a part of the log's
       transforms. *)
    Doubly_linked.remove log.transforms t
  ;;
end

let clear_transforms t = Doubly_linked.clear t.transforms

let transform t msg =
  Doubly_linked.fold_until
    t.transforms
    ~init:msg
    ~f:(fun msg transform ->
      match transform msg with
      | Some msg -> Continue msg
      | None -> Stop None)
    ~finish:Option.return
;;

let get_transform t =
  (* This doesn’t use [transform] function above as [transforms] is mutable and this takes
     a snapshot of it *)
  match Doubly_linked.to_list t.transforms with
  | [] -> None
  | [ f ] -> Some f
  | fs ->
    Some
      (fun msg ->
        let rec loop fs msg =
          match fs with
          | [] -> Some msg
          | f :: fs ->
            (match f msg with
             | None -> None
             | Some msg -> loop fs msg)
        in
        loop fs msg)
;;

let set_transform t f =
  clear_transforms t;
  match f with
  | None -> ()
  | Some f ->
    let (_ : Transform.t) = Transform.add t f `Before in
    ()
;;

let copy t =
  create
    ~level:(level t)
    ~default_outputs:(get_output t)
    ~named_outputs:(get_named_outputs t)
    ~on_error:(get_on_error t)
    ~time_source:(Some (get_time_source t))
    ~transforms:(Doubly_linked.to_list t.transforms)
;;

(* would_log is broken out and tested separately for every sending function to avoid the
   overhead of message allocation when we are just going to drop the message. *)
let would_log t msg_level =
  let output_or_transform_is_enabled =
    (not (Mutable_outputs.is_empty t.output)) || has_transform t
  in
  output_or_transform_is_enabled
  && Level.as_or_more_verbose_than ~log_level:(level t) ~msg_level
;;

let push_message_event t msg =
  (* We want to call [transform], even if we don't end up pushing the message to an
     output. This allows for someone to listen to all messages that would theoretically be
     logged by this log (respecting level), and then maybe log them somewhere else. *)
  match transform t msg with
  | Some msg ->
    if not (Mutable_outputs.is_empty t.output)
    then (
      assert_open t "write message";
      Mutable_outputs.write t.output msg)
  | None -> ()
;;

let all_live_logs_flushed () =
  match Lazy.peek live_logs with
  | Some live_logs -> Live_entry_registry.live_entries_flushed live_logs
  | None -> Deferred.unit
;;

module Private = struct
  let set_named_output t name output =
    assert_open t "set named output";
    Mutable_outputs.set_named_output t.output name output
  ;;

  let get_named_output t name =
    assert_open t "get named output";
    Mutable_outputs.current_named_outputs t.output |> Fn.flip Map.find name
  ;;

  let remove_named_output t name =
    assert_open t "remove named output";
    Mutable_outputs.remove_named_output t.output name
  ;;

  let with_temporary_outputs t outputs ~f =
    assert_open t "with temporary outputs";
    let original_outputs = get_output t in
    let original_named_outputs = get_named_outputs t in
    Mutable_outputs.update_named_outputs t.output Output_name.Map.empty;
    set_output t outputs;
    f ();
    set_output t original_outputs;
    Mutable_outputs.update_named_outputs t.output original_named_outputs
  ;;

  module For_testing = struct
    let get_named_outputs = get_named_outputs

    let update_named_outputs t named_outputs =
      assert_open t "update named outputs";
      Mutable_outputs.update_named_outputs t.output named_outputs
    ;;
  end
end

module For_testing = struct
  let transform = transform
end
