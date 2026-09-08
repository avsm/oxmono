(** sent — the outcome of a send, as a promise a handler may await or drop.

    A send is queued by the runtime's send queue and answered by the homeserver
    later. Nothing here blocks unless {!await} is called. *)

(** The type for how a send ended. *)
type outcome =
  | Sent of Matrix_proto.Id.Event_id.t  (** The homeserver assigned this id. *)
  | Uploaded of Matrix_client.Send_queue.upload_result
      (** A media upload completed. *)
  | Failed of Matrix_client.Error.t option
      (** The queue gave up and the event was not sent. The error is [None] for
          a request the queue restored from a store, which does not keep it. *)
  | Cancelled  (** Withdrawn before the queue sent it. *)
  | Timed_out  (** {!await} gave up while the send was still in the queue. *)

(** The type for where a send has got to. *)
type status =
  | Queued  (** Waiting its turn. *)
  | Sending  (** An attempt is in flight. *)
  | Done of outcome
      (** The send is over. {!Timed_out} never appears here. It belongs to a
          wait rather than to a request. *)

type t
(** The type for sends in progress. *)

val status : t -> status
(** [status t] is where [t] has got to, without blocking. *)

val await : ?timeout:float -> t -> outcome
(** [await t] blocks until the send is over and is how it ended. [timeout] is in
    seconds and defaults to 60, after which the answer is {!Timed_out} and the
    send is left in the queue. *)

val request : t -> Matrix_client.Send_queue.request
(** [request t] is the queued request behind [t], for a caller that wants its
    transaction id or its attempt count. *)

val cancel : t -> [ `Cancelled | `Already_sent | `In_flight ]
(** [cancel t] withdraws a request the queue has not yet sent, and answers as
    {!Matrix_client.Send_queue.cancel} does. On [`Cancelled] a waiting {!await}
    returns {!Cancelled}. *)

module Internal : sig
  (** How {!Bot} and {!Room} make one. Not for a handler to call. *)

  type tracker
  (** The type for the one queue watcher a bot shares between its sends. *)

  val tracker :
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Std.r ->
    Matrix_client.Send_queue.t ->
    tracker
  (** [tracker ~sw ~clock queue] watches [queue] until [sw] is released, after
      which it resolves nothing and holds nothing. [clock] is what {!await}
      times out against. Call it once per queue.
      {!Matrix_client.Send_queue.on_change} answers through a single callback
      rather than one per send, and registers it for the life of the queue. *)

  val v : tracker -> Matrix_client.Send_queue.request -> t
  (** [v tracker request] is the promise for a request already on [tracker]'s
      queue, resolved whether the request finishes before or after this call. *)
end
