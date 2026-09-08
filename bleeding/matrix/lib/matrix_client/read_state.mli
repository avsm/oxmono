(** read_state — the user's own read position in a room, and the unread counts
    derived from it.

    A room's read state is the pair of receipts the user's own devices have sent
    plus the fully-read marker. It is ingested from the [m.receipt] ephemeral
    events and the [m.fully_read] account-data event a sync delivers, and it
    moves forwards only, so an out-of-order delivery cannot rewind it.

    The counts computed here are the client's own. They live beside the server's
    [unread_notifications], which cannot be right for an encrypted room because
    the server cannot read the content the push rules test. {!Receipts} sends
    receipts and moves the marker.

    @see <https://spec.matrix.org/v1.11/client-server-api/#receipts> Receipts *)

(** {1 Read state} *)

type receipt = {
  event_id : Matrix_proto.Id.Event_id.t;  (** The event read up to. *)
  ts : Matrix_proto.Event.Timestamp.t option;
      (** When the sending server says the receipt was made. Absent when it did
          not say. *)
}
(** One receipt a device sent. *)

type t
(** The type for a room's read state. Its receipts only ever move forwards, so
    it cannot be built field by field. *)

val empty : t
(** [empty] is the read state of a room nothing has been read in, with no
    receipt and no marker. *)

val v :
  ?public_read:receipt ->
  ?private_read:receipt ->
  ?fully_read:Matrix_proto.Id.Event_id.t ->
  unit ->
  t
(** [v ()] is a read state holding the given positions. Every argument defaults
    to absent, so [v ()] is {!empty}. *)

val public_read : t -> receipt option
(** [public_read t] is the [m.read] receipt, and [None] if none was sent. *)

val private_read : t -> receipt option
(** [private_read t] is the [m.read.private] receipt, and [None] if none was
    sent. *)

val fully_read : t -> Matrix_proto.Id.Event_id.t option
(** [fully_read t] is the [m.fully_read] marker from the room's account data,
    and [None] if it has never been set. *)

val thread_ids : t -> Matrix_proto.Id.Event_id.t list
(** [thread_ids t] is the sorted list of thread roots with a retained receipt.
*)

val thread_public_read :
  t -> thread_id:Matrix_proto.Id.Event_id.t -> receipt option
(** [thread_public_read t ~thread_id] is the thread-scoped [m.read] receipt. *)

val thread_private_read :
  t -> thread_id:Matrix_proto.Id.Event_id.t -> receipt option
(** [thread_private_read t ~thread_id] is the thread-scoped [m.read.private]
    receipt. *)

val thread_latest_read :
  t -> thread_id:Matrix_proto.Id.Event_id.t -> Matrix_proto.Id.Event_id.t option
(** [thread_latest_read t ~thread_id] chooses the newer thread-scoped receipt.
*)

val latest_read : t -> Matrix_proto.Id.Event_id.t option
(** [latest_read t] is the more recent of the public and private receipts by
    timestamp, or whichever of the two exists, and [None] if neither does.
    {!latest_read_in_timeline} is more accurate where a timeline is at hand,
    since position in the timeline is authoritative and a timestamp only
    approximates it. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for {!t}, as {!Store} persists it. *)

(** {1 Ingestion} *)

val ingest_receipt_event :
  user_id:Matrix_proto.Id.User_id.t -> t -> Jsont.json -> t
(** [ingest_receipt_event ~user_id t event] folds one ephemeral event into [t].
    An event whose type is not [m.receipt] is ignored, as are receipts for users
    other than [user_id]. Threaded receipts are retained under their validated
    thread root; [public_read] and [private_read] continue to expose only the
    unthreaded/main-timeline positions.

    A receipt replaces the one held only when its timestamp is not older, so an
    out-of-order delivery cannot move the read position backwards. *)

val ingest_ephemeral :
  user_id:Matrix_proto.Id.User_id.t -> t -> Jsont.json list -> t
(** [ingest_ephemeral ~user_id t events] is {!ingest_receipt_event} folded over
    a room's [ephemeral.events]. *)

val ingest_fully_read : t -> Jsont.json -> t
(** [ingest_fully_read t event] reads an [m.fully_read] room account-data event
    and records its [event_id]. Other event types are ignored. *)

(** {1 Unread counts} *)

type counts = {
  unread : int;  (** Messages the user has not read. *)
  notifications : int;
      (** Of those, the ones the push rules would notify on. *)
  highlights : int;  (** Of those, the ones carrying the [highlight] tweak. *)
}
(** What is unread in one room. *)

val zero_counts : counts
(** [zero_counts] has every count at zero, which is what a fully read room
    holds. *)

val marks_as_unread :
  user_id:Matrix_proto.Id.User_id.t -> Matrix_proto.Event.Raw_event.t -> bool
(** [marks_as_unread ~user_id e] is [true] when [e] counts towards
    {!counts.unread}. It does when it was sent by someone other than [user_id],
    its type is one of [m.room.message], [m.room.encrypted], [m.sticker],
    [m.poll.start] or [m.poll.end] or their [org.matrix.msc3381] unstable
    spellings, it is not an edit, and it is not redacted. *)

val is_main_timeline_event : Matrix_proto.Event.Raw_event.t -> bool
(** [is_main_timeline_event e] is false for a direct [m.thread] reply. *)

val latest_read_in_timeline :
  user_id:Matrix_proto.Id.User_id.t ->
  t ->
  Matrix_proto.Event.Raw_event.t list ->
  int option
(** [latest_read_in_timeline ~user_id t events] is the index in [events] of the
    user's read position, which is the last of the public receipt, the private
    receipt and an event [user_id] sent, the last being an implicit receipt.
    [None] means the position is not in [events], in which case every event is
    unread. *)

val count_unread :
  user_id:Matrix_proto.Id.User_id.t ->
  notification:(Matrix_proto.Event.Raw_event.t -> Push_evaluator.notification) ->
  t ->
  Matrix_proto.Event.Raw_event.t list ->
  counts
(** [count_unread ~user_id ~notification t events] counts the events strictly
    after {!latest_read_in_timeline}. [notification] is normally
    {!Push_evaluator.notification_for_event} partially applied to a ruleset and
    a context. Each event contributes at most one notification and at most one
    highlight. *)

val latest_read_in_thread :
  user_id:Matrix_proto.Id.User_id.t ->
  thread_id:Matrix_proto.Id.Event_id.t ->
  t ->
  Matrix_proto.Event.Raw_event.t list ->
  int option
(** [latest_read_in_thread ~user_id ~thread_id t events] is the read position
    among the root and direct [m.thread] replies for [thread_id]. An own event
    in another thread or in the main timeline cannot advance this position. *)

val count_unread_in_thread :
  user_id:Matrix_proto.Id.User_id.t ->
  thread_id:Matrix_proto.Id.Event_id.t ->
  notification:(Matrix_proto.Event.Raw_event.t -> Push_evaluator.notification) ->
  t ->
  Matrix_proto.Event.Raw_event.t list ->
  counts
(** [count_unread_in_thread ~user_id ~thread_id ~notification t events] counts
    only events belonging to [thread_id]. *)
