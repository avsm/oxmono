open! Core
open! Async_kernel
open! Import

module Definitely_a_heap_block : sig
  type t

  val the_one_and_only : t
end = struct
  type t = string

  let the_one_and_only = String.make 1 ' '
end

type t =
  { write : Message_event.t -> unit
  ; rotate : unit -> unit Deferred.t
  ; flush : unit -> unit Deferred.t
  ; buffered_background_error : [ `Output_is_unbuffered | `Error of exn Deferred.t ]
      (* experimentation shows that this record, without this field, can sometimes raise
         when passed to Heap_block.create_exn, which we need to do to add a finalizer.
         This seems to occur when the functions are top-level and/or constant. More
         investigation is probably worthwhile. *)
  ; heap_block : Definitely_a_heap_block.t
  }

let aux_create ~finalize ~rotate ~flush ~buffered_background_error ~write =
  let heap_block = Definitely_a_heap_block.the_one_and_only in
  let t = { write; rotate; flush; buffered_background_error; heap_block } in
  Option.iter finalize ~f:(fun finalize ->
    Gc.add_finalizer (Heap_block.create_exn t) (fun t ->
      let t = Heap_block.value t in
      don't_wait_for
        (let%bind () = t.flush () in
         finalize ())));
  t
;;

let create_expert ?(rotate = return) ?finalize ~flush write =
  let output = Buffered_output.create ~rotate ~flush ~write in
  aux_create
    ~finalize
    ~rotate:(fun () -> Buffered_output.rotate output)
    ~flush:(fun () -> Buffered_output.flushed output)
    ~buffered_background_error:(`Error (Buffered_output.background_error output))
    ~write:(Buffered_output.write output)
;;

let create_unbuffered ?finalize ~flush write =
  aux_create
    ~finalize
    ~rotate:return
    ~flush
    ~buffered_background_error:`Output_is_unbuffered
    ~write
;;

let create ?rotate ?finalize ~flush write =
  create_expert ?rotate ?finalize ~flush (fun messages ->
    Queue.map messages ~f:Message_event.to_serialized_message_lossy |> write)
;;

let create_expert = create_expert ?rotate:None
let empty = create_unbuffered (ignore : Message_event.t -> unit) ~flush:return
let write t = t.write
let rotate t = t.rotate ()
let flush t = t.flush ()
let sexp_of_t _ = Sexp.Atom "<opaque>"

let filter_to_level t ~level =
  let write message =
    if Level.as_or_more_verbose_than
         ~log_level:level
         ~msg_level:(Message_event.level message)
    then t.write message
  in
  { t with write }
;;

let transform_message t ~f =
  let write message = f message |> t.write in
  { t with write }
;;

let stderr_sync =
  lazy
    (let zone =
       (* Set all the tests to run in the NYC time zone to keep this deterministic in
          tests, and preserve compatibility with [Time_ns_unix].
       *)
       if am_running_test then Timezone.find_exn "nyc" else force Timezone.local
     in
     create_unbuffered ~flush:return (fun msg ->
       Message.to_write_only_text (Message_event.to_serialized_message_lossy msg) zone
       |> Core.prerr_endline))
;;

let stderr_async = Set_once.create ()

let stderr =
  lazy
    (match Set_once.get stderr_async with
     | None -> force stderr_sync
     | Some stderr -> force stderr)
;;

module Private = struct
  let buffered_background_error t = t.buffered_background_error
  let set_async_stderr_output t ~here = Set_once.set_exn stderr_async ~here t
  let buffered_batch_size = Buffered_output.Private.batch_size
end

module For_testing = struct
  let create
    ?(map_output = Fn.id)
    ?(time : [ `Keep | `Omit ] = `Omit)
    ?(tags : [ `Keep | `Omit ] = `Omit)
    ?(level : [ `Keep | `Omit ] = `Omit)
    ()
    =
    create_unbuffered ~flush:return (fun message_event ->
      let message = Message_event.to_serialized_message_lossy message_event in
      let zone = Timezone.find_exn "nyc" in
      let string_message =
        Message.For_testing.to_string message zone ~time ~tags ~level
      in
      let mapped_output = map_output string_message in
      print_endline mapped_output)
  ;;

  let is_async_stderr_output_set () = Set_once.is_some stderr_async
end
