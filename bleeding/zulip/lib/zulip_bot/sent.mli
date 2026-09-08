(** Asynchronous message-send results.

    A send handle distinguishes admission to the context queue from delivery by
    Zulip. Handles remain valid after their owning context switch is released.
*)

type outcome = Sent_runtime.outcome =
  | Sent of Zulip.Id.Message.t
  | Failed of Zulip_eio.Error.t
  | Indeterminate of Zulip_eio.Error.t option
  | Cancelled
      (** The type for terminal send outcomes. [Sent id] confirms acceptance by
          Zulip. [Failed error] confirms that the request failed.
          [Indeterminate error] means that the request may have reached Zulip
          without a confirmed response. [Indeterminate None] means that the
          context switch was released while the request was being sent. Retrying
          an indeterminate request can send a duplicate message. [Cancelled]
          means that transmission did not begin. *)

type status = Sent_runtime.status =
  | Queued
  | Sending
  | Done of outcome
      (** The type for send states. [Queued] has been admitted but has not
          started. [Sending] may already have reached Zulip. [Done outcome] is
          terminal. *)

type t = Sent_runtime.t
(** The type for send handles. A handle is created under the switch passed to
    {!Context.v} or {!Context.connect}. Releasing that switch settles queued
    handles as [Cancelled] and sending handles as [Indeterminate None]. *)

val status : t -> status
(** [status sent] is the current state of [sent]. *)

val await : ?timeout:float -> t -> [ `Done of outcome | `Timed_out ]
(** [await ~timeout sent] is [`Done outcome] when [sent] reaches [outcome]
    within [timeout] seconds, or [`Timed_out] otherwise. [timeout] defaults to
    [60.]. Positive infinity waits without a deadline. Timing out and fiber
    cancellation leave the send running.

    @raise Stdlib.exception-Invalid_argument if [timeout] is negative or NaN. *)

val cancel : t -> [ `Cancelled | `Settled of outcome | `In_flight ]
(** [cancel sent] atomically cancels [sent] if it is still queued. [`Cancelled]
    confirms the cancellation. [`In_flight] means that sending has begun and
    continues. [`Settled outcome] reports the existing terminal outcome. *)
