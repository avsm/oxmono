(** receipts — read receipts and the fully-read marker.

    A receipt says how far the user has read. A public one is broadcast to the
    room, a private one is visible only to the sender, and the fully-read marker
    is private and says where to put the "new messages" line. All three name an
    event, and all three are monotonic in the server's view of the timeline, so
    pointing at an older event than the one already recorded is accepted and has
    no effect.

    {!Read_state} holds the receipts a sync delivers and derives unread counts
    from them.

    @see <https://spec.matrix.org/v1.11/client-server-api/#receipts> Receipts
    @see <https://spec.matrix.org/v1.11/client-server-api/#fully-read-markers>
      Fully Read Markers *)

(** The kinds of receipt the specification defines. *)
type receipt_type =
  | Read  (** [m.read], which the whole room sees. *)
  | Read_private  (** [m.read.private], which only the sender sees. *)
  | Fully_read  (** [m.fully_read], which moves the marker instead. *)

val receipt_type_to_string : receipt_type -> string
(** [receipt_type_to_string t] is the wire form of [t]. *)

val send_receipt :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?receipt_type:receipt_type ->
  ?thread_id:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [send_receipt client ~room_id ~event_id ?thread_id ()] records the caller as
    having read up to [event_id]. A supplied [thread_id] scopes an [m.read] or
    [m.read.private] receipt to that validated thread root and is encoded in the
    request body as [{"thread_id":"..."}]. The default/main-timeline request
    body remains [{}]. [Fully_read] rejects [thread_id], since its marker is
    necessarily unthreaded.

    ([POST /_matrix/client/v3/rooms/{roomId}/receipt/{receiptType}/{eventId}],
    Matrix 1.0). [receipt_type] defaults to {!Read}. After the receipt succeeds,
    this also clears the room's [m.marked_unread] account-data flag. If that
    account-data update fails, its error is returned. *)

val set_read_marker :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?fully_read:Matrix_proto.Id.Event_id.t ->
  ?read:Matrix_proto.Id.Event_id.t ->
  ?read_private:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [set_read_marker client ~room_id ~fully_read ()] moves any supplied read
    positions in one request
    ([POST /_matrix/client/v3/rooms/{roomId}/read_markers], Matrix 1.0).
    [fully_read] is the private fully-read marker; [read] and [read_private] are
    public and private receipts. Each defaults to absent, but a call with all
    three absent is rejected locally. After the request succeeds, this also
    clears the room's [m.marked_unread] account-data flag. If that account-data
    update fails, its error is returned. *)
