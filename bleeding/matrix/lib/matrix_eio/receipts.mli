(** receipts — read receipts and the fully-read marker, raising instead of
    returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Receipts} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces. *)

(** Who a receipt is visible to. *)
type receipt_type = Matrix_client.Receipts.receipt_type =
  | Read  (** Public, and seen by everyone in the room. *)
  | Read_private  (** Private to the user who sent it. *)
  | Fully_read  (** The user's own marker of where reading stopped. *)

val send_receipt :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?receipt_type:receipt_type ->
  ?thread_id:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [send_receipt c ~room_id ~event_id ?thread_id ()] is
    {!Matrix_client.Receipts.send_receipt} with the result unwrapped. *)

val set_read_marker :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?fully_read:Matrix_proto.Id.Event_id.t ->
  ?read:Matrix_proto.Id.Event_id.t ->
  ?read_private:Matrix_proto.Id.Event_id.t ->
  unit ->
  unit
(** [set_read_marker c ~room_id ~fully_read ()] is
    {!Matrix_client.Receipts.set_read_marker} with the result unwrapped. *)
