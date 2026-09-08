(** Queued sends and their eventual server outcomes. *)

type outcome =
  | Sent of Zulip.Id.Message.t
  | Failed of Zulip_eio.Error.t
  | Indeterminate of Zulip_eio.Error.t option
  | Cancelled
      (** [Sent id] confirms server acceptance. [Failed error] is an explicit
          failure. [Indeterminate error] means the request may have reached
          Zulip without a confirmed response; [None] denotes switch shutdown
          during a send. Retrying an indeterminate send can duplicate a message.
          [Cancelled] was never sent. *)

type status =
  | Queued
  | Sending
  | Done of outcome
      (** [Done outcome] is terminal. Waiting with a timeout does not change
          status. *)

type t
(** A handle owned by the switch supplied to {!Context.v}. *)

val status : t -> status
(** [status t] is the current send state. *)

val await : ?timeout:float -> t -> [ `Done of outcome | `Timed_out ]
(** [await t] waits for a terminal outcome for at most [timeout] seconds
    (default [60.]). [`Timed_out] leaves the send running. Positive infinity
    waits without a deadline. Fiber cancellation propagates and leaves the send
    running.
    @raise Invalid_argument if [timeout] is negative or NaN. *)

val cancel : t -> [ `Cancelled | `Settled of outcome | `In_flight ]
(** [cancel t] cancels a queued send atomically with respect to starting it.
    [`In_flight] leaves an active request running. [`Settled outcome] reports an
    existing terminal outcome, including cancellation or failure. *)

type tracker

val tracker : clock:float Eio.Time.clock_ty Eio.Resource.t -> tracker
val v : tracker -> t
val begin_send : t -> bool
val resolve : t -> outcome -> unit
val close : tracker -> unit
val await_closed : tracker -> unit
