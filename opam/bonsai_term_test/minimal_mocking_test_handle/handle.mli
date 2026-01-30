open! Core
open Async
open Bonsai_term
module Frame_outcome := Bonsai_term.Private.Frame_outcome

type 'exit t

type 'ret common_handle_args :=
  ?dispose:bool
  -> ?nosig:bool
  -> ?mouse:bool
  -> ?bpaste:bool
  -> ?time_source:Time_source.t
  -> ?optimize:bool
  -> ?target_frames_per_second:int
  -> ?cap:Bonsai_term_test.Capability.t
  -> ?file_descriptors:(unit -> File_descriptors.t Deferred.t)
  -> dimensions:Dimensions.t
  -> 'ret

type common_app_fn :=
  dimensions:Dimensions.t Bonsai_term.Bonsai.t
  -> local_ Bonsai_term.Bonsai.graph
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

(** [with_handle_with_exit] is like [Bonsai_term.start_with_exit], but instead of
    "loop-ing" forever, it let's you "control" the loop. *)
val with_handle_with_exit
  : ((exit:('exit -> unit Effect.t) -> common_app_fn)
     -> ('exit t -> 'a Deferred.Or_error.t)
     -> 'a Deferred.t)
      common_handle_args

(** [with_handle] is like [Bonsai_term.start]. It will default to closing upon hitting
    [Ctrl-C] *)
val with_handle
  : (common_app_fn -> (unit t -> 'a Deferred.Or_error.t) -> 'a Deferred.t)
      common_handle_args

(** Dispose of this handle *)
val release : _ t -> unit Deferred.t

(** [stdin_writer] is the stdin that bonsai_term is reading to parse events. You can feed
    events using this pipe. *)
val stdin_writer : 'exit t -> string Pipe.Writer.t

(** [stdout_reader] is the stdout that bonsai_term is using. It contains the ascii bytes
    that we are feeding to stdout. Using this correctly is tricky, as if you [Pipe.read]
    when stdout has not been written to, you will end up forever waiting in your test, so
    use this sparringly, only when you care about testing the raw bytes that we give to
    stdout.

    For _normal_ printing (at the expense of it being "mocked"), instead use [show] which
    prints out the most recent [View.t]. *)
val stdout_reader : 'exit t -> string Pipe.Reader.t

(** [time_source] gives you access to the async time source. Use this to advance the time
    in a test. *)
val time_source : 'exit t -> Async.Time_source.t

(** [start_frame] is a two-staged deferred. It is the primitive that lets you control the
    "bonsai_frame loop". What is the "bonsai frame loop"?

    1. Compute the up-to-date [View.t] of the bonsai app.
    2. Draw it to stdout.
    3. Wait for the next event (keyboard/mouse input to stdin, timer, window resize, stdin
       closing)

    The first "layer" of the deferred will return after the frame has been painted.

    The second "layer" of deferred returns after the next event has been processed.

    NOTE: the _second_ "layer" will wait forever until the next event happens, so it's
    relatively easy to write a test that will wait forever. As such, please prefer the
    [bonsai_term_test] library for your "app-specific-logic" states. [bonsai_term_test] is
    a "mock" of the real bonsai_term event loop. [bonsai_term_integration_test] is similar
    to [bonsai_term_test], but with as minimal amount of mocking as possible, letting you
    assert things that the mock library can't like:

    - The actual ascii codes/text that we write to stdout.
    - The logic inside of [bonsai_term]
    - The specific order and frames in which events are process.

    [compute_frame] fails if the previous frame has not finished. Make sure to bind on the
    inner deferred before scheduling another call to [compute_frame]. *)
val start_frame
  :  here:[%call_pos]
  -> 'exit t
  -> ([ `Frame_painted ] * [ `Frame_finished of 'exit Frame_outcome.t ] Deferred.t)
       Deferred.t

val do_frame : unit t -> unit Frame_outcome.t Deferred.t
val do_frame_and_continue : here:[%call_pos] -> 'exit t -> unit Deferred.t

val finish_frame_and_continue
  :  here:[%call_pos]
  -> [ `Frame_finished of 'exit Frame_outcome.t ] Deferred.t
  -> unit Deferred.t

val finish_frame_and_expect_shutdown
  :  here:[%call_pos]
  -> [ `Frame_finished of 'exit Frame_outcome.t ] Deferred.t
  -> unit Deferred.t

val change_dimensions : 'exit t -> Dimensions.t -> unit
val do_frame_and_expect_shutdown : here:[%call_pos] -> unit t -> unit Deferred.t
val show : here:[%call_pos] -> ?cap:Bonsai_term_test.Capability.t -> 'exit t -> unit
val show_mocked : here:[%call_pos] -> 'exit t -> unit
