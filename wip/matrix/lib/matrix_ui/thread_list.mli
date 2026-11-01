(** A server-backed, duplicate-safe list of thread roots.

    Each page is fetched through the Matrix client [/rooms/{roomId}/threads]
    paginator. Roots are merged into {!Thread_info}; replies remain owned by the
    shared {!Event_cache} and are not fetched by this service. *)

type state = Matrix_client.Thread_paginator.state =
  | Start
  | Loading
  | Next of string
  | End
  | Failed of Matrix_client.Error.t

type t

val create :
  client:Matrix_client.Client.t ->
  thread_info:Thread_info.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?event_cache:Event_cache.t ->
  ?thread_cache:Thread_cache.t ->
  unit ->
  t
(** [create] owns pagination state for one room and merges every page root into
    [thread_info]. When [event_cache] is supplied, identified roots and valid
    bundled latest replies are also retained as detached external events in the
    shared cache. The view closes and clears itself when the room is forgotten.
*)

val set_filter : t -> Matrix_client.Relations.thread_filter -> unit
(** [set_filter t filter] resets the admitted root view and begins fetching with
    [filter]. *)

val reset : t -> unit
(** [reset t] resets pagination while retaining the current filter. *)

val close : t -> unit
(** [close t] detaches sync updates and clears the rich list. Idempotent. *)

val state : t -> state

val continuation : t -> string option
(** The token for the next backwards page, if one is available. *)

val roots : t -> Matrix_proto.Event.Raw_event.t list
(** Successfully loaded roots in server order, with duplicate event IDs removed.
    A repeated root can still update its bundled summary in [infos]. *)

val loaded_pages : t -> int
val is_at_last_page : t -> bool

val subscribe : t -> (state -> unit) -> unit -> unit
(** Subscribe to deterministic pagination state transitions. *)

val next_page : t -> ?limit:int -> unit -> (unit, Matrix_client.Error.t) result
(** Fetch one backwards-in-history page. A loading or terminal call is a
    successful no-op, as is a call after [close]. A failed call retains its
    continuation token. *)

val infos : t -> Thread_info.info Observable.List.t
(** Rich summaries for roots admitted by this list, updated by page ingestion
    and sync refresh. *)

val snapshot : t -> Thread_info.info array
