open! Core
open Async_kernel
open! Import

(* The reason for the tricky handling of background errors is that historically,
   [on_error] is specified in [Log.create], not [Log.Output.create], AND a log can change
   its outputs.

   As a result, a bg loop error may be associated with a dynamically changing set of logs
   / [on_error] handlers, and -technically- within a single loop iteration (i.e., one
   handling of a [Message_event.t Queue.t]), different messages may only apply to diff
   subsets of logs.

   We figured it was slightly dirty but practically fine to be a bit overly 'loud' in
   triggering on_error handlers.

   - If 2 logs are writing to the same output at the same time and one log's message
     causes the write to raise, both [on_error]s will be called.

   - If a log just added an output to its list of outputs, it immediately starts watching
     for the new output's background error.

   - If a log /just/ removed the output from its list of outputs, the [on_error] handler
     will remain registered until the old output is flushed (+ maybe 1 async cycle).
*)
module State = struct
  type t =
    { default_outputs : Output.t list
    ; named_outputs : Output.t Output_name.Map.t
    ; previous_outputs_flushed : unit Deferred.t
    ; stop_watching_for_background_errors : unit Ivar.t
    }
  [@@deriving fields ~getters ~iterators:create]

  let all_outputs { default_outputs; named_outputs; _ } =
    default_outputs @ Map.data named_outputs
  ;;

  let watch_for_background_errors t ~stop ~on_background_output_error:on_error =
    List.iter (all_outputs t) ~f:(fun output ->
      match Output.Private.buffered_background_error output with
      | `Error e -> don't_wait_for (choose [ choice stop Fn.id; choice e on_error ])
      | `Output_is_unbuffered -> ())
  ;;

  let flushed t =
    let open Eager_deferred.Use in
    let%bind () = t.previous_outputs_flushed in
    Deferred.List.iter (all_outputs t) ~how:`Sequential ~f:Output.flush
  ;;

  let create ~default_outputs ~named_outputs ~previous ~on_background_output_error =
    let stop_watching_for_background_errors = Ivar.create () in
    let previous_outputs_flushed =
      match previous with
      | None -> Deferred.unit
      | Some previous ->
        let%map.Eager_deferred () = flushed previous in
        Ivar.fill_if_empty previous.stop_watching_for_background_errors ()
    in
    let t =
      { default_outputs
      ; named_outputs
      ; previous_outputs_flushed
      ; stop_watching_for_background_errors
      }
    in
    watch_for_background_errors
      t
      ~stop:(Ivar.read stop_watching_for_background_errors)
      ~on_background_output_error;
    t
  ;;

  let is_empty t = t |> all_outputs |> List.is_empty

  let write t msg =
    t |> all_outputs |> List.iter ~f:(fun output -> Output.write output msg)
  ;;
end

type t =
  { mutable state : State.t
      (* [Buffered_output] also does caching of flushes, but in case multiple logs are
         hooked up to the same outputs, this allows further caching that's easier to test.
         We can probably get rid of one of them at some point. *)
  ; mutable last_update : [ `Flush of unit Deferred.t | `Not_a_flush ]
  ; on_background_output_error : exn -> unit
  }

let create ~default_outputs ~named_outputs ~on_background_output_error =
  { state =
      State.create
        ~default_outputs
        ~named_outputs
        ~previous:None
        ~on_background_output_error
  ; last_update = `Not_a_flush
  ; on_background_output_error
  }
;;

let is_empty t = State.is_empty t.state

let write t =
  t.last_update <- `Not_a_flush;
  State.write t.state
;;

let flushed t =
  match t.last_update with
  | `Flush flush -> flush
  | `Not_a_flush ->
    let flush = State.flushed t.state in
    t.last_update <- `Flush flush;
    flush
;;

let update_default_outputs t outputs =
  t.last_update <- `Not_a_flush;
  t.state
  <- State.create
       ~default_outputs:outputs
       ~named_outputs:t.state.named_outputs
       ~previous:(Some t.state)
       ~on_background_output_error:t.on_background_output_error
;;

let update_named_outputs t named_outputs =
  t.last_update <- `Not_a_flush;
  t.state
  <- State.create
       ~default_outputs:t.state.default_outputs
       ~named_outputs
       ~previous:(Some t.state)
       ~on_background_output_error:t.on_background_output_error
;;

let set_named_output t name output =
  let named_outputs = Map.set t.state.named_outputs ~key:name ~data:output in
  update_named_outputs t named_outputs
;;

let remove_named_output t name =
  let named_outputs = Map.remove t.state.named_outputs name in
  update_named_outputs t named_outputs
;;

let current_default_outputs t = t.state.default_outputs
let current_named_outputs t = t.state.named_outputs
