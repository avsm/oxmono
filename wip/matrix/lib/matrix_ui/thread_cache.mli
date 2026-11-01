(** Durable, room-scoped thread event projections.

    Event bodies deliberately live in {!Event_cache}; this cache owns the
    per-thread identity, bounded ordered event-ID projection, pagination cursors
    and read/unread projection. A thread retains its root and at most 255
    replies; body retention remains subject to the shared event cache's
    room-wide bound. *)

type pagination_token = Matrix_client.Paginator.pagination_token =
  | Not_started
  | Has_more of string
  | Hit_end

type pagination = { backward : pagination_token; forward : pagination_token }

type snapshot = {
  room_id : Matrix_proto.Id.Room_id.t;
  root_id : Matrix_proto.Id.Event_id.t;
  root : Matrix_proto.Event.Raw_event.t option;
  replies : Matrix_proto.Event.Raw_event.t list;
  events : Matrix_proto.Event.Raw_event.t list;
  pagination : pagination;
  receipts : Matrix_client.Read_state.t;
  unread : Matrix_client.Read_state.counts;
}

type t

val create :
  event_cache:Event_cache.t -> ?store:Matrix_client.Store.t -> unit -> t

val snapshot :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  snapshot option

val ingest :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  events:Matrix_proto.Event.Raw_event.t list ->
  unit
(** Ingests roots and [m.thread] replies. Unknown roots are retained until the
    root arrives; all identified events are registered in {!Event_cache}. *)

val ingest_thread :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  events:Matrix_proto.Event.Raw_event.t list ->
  unit
(** Ingests a known root and its bundled or relation replies, including a
    bundled reply whose envelope omits [m.relates_to]. *)

val set_pagination :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  backward:pagination_token ->
  forward:pagination_token ->
  unit

val set_receipts :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  Matrix_client.Read_state.t ->
  unit

val set_room_receipts :
  t -> room_id:Matrix_proto.Id.Room_id.t -> Matrix_client.Read_state.t -> unit

val set_unread :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  Matrix_client.Read_state.counts ->
  unit

val subscribe_events :
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  t ->
  (snapshot -> unit) ->
  unit ->
  unit

val subscribe_receipts :
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  t ->
  (snapshot -> unit) ->
  unit ->
  unit

val subscribe_unread :
  room_id:Matrix_proto.Id.Room_id.t ->
  root_id:Matrix_proto.Id.Event_id.t ->
  t ->
  (snapshot -> unit) ->
  unit ->
  unit

val forget_room : t -> Matrix_proto.Id.Room_id.t -> unit
val close : t -> unit
