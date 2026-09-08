(** A short-lived, in-memory timeline centred on one event.

    The view owns no cache or store entries. It is a projection over
    {!Matrix_client.Paginator}: the paginator owns the request state and event
    de-duplication, while this module owns the order in which those events are
    presented to a UI. *)

type state = Initial | Fetching_target | Idle | Paginating | Closed

type thread_mode = Matrix_client.Paginator.thread_mode =
  | Automatic
  | Force
      (** How the focused view chooses its pagination source. [Automatic] uses
          thread relations only for a threaded reply; [Force] focuses the
          event's thread, or treats it as the root when it has no thread
          relation. *)

type page = Matrix_client.Paginator.page = {
  events : Matrix_proto.Event.Raw_event.t list;
  hit_end : bool;
}

type start_result = Matrix_client.Paginator.start_result = {
  events : Matrix_proto.Event.Raw_event.t list;
  has_previous : bool;
  has_next : bool;
}

type error =
  | Event_not_found of Matrix_proto.Id.Event_id.t
  | Invalid_state of { expected : state; actual : state }
  | Client_error of Matrix_client.Error.t

val pp_error : Format.formatter -> error -> unit

type t
(** The type of one event-focused view. *)

val create :
  client:Matrix_client.Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?event_cache:Event_cache.t ->
  ?thread_cache:Thread_cache.t ->
  ?limit:int ->
  ?thread_mode:thread_mode ->
  unit ->
  t
(** [create ... ?limit ?thread_mode ()] makes an unstarted view. [limit] is the
    total number of context events requested by {!start}; it defaults to 10.
    [thread_mode] defaults to {!Automatic}. When [event_cache] is supplied,
    identified events returned by the initial context and later pagination are
    registered as detached events in that shared cache. They remain available to
    cache-only consumers such as {!Pinned_events}, but do not acquire a timeline
    position. Forgetting the room through that cache closes the view and clears
    its visible events. *)

val events : t -> Matrix_proto.Event.Raw_event.t Observable.List.t
(** [events t] is the observable list in chronological order. *)

val snapshot : t -> Matrix_proto.Event.Raw_event.t array
(** [snapshot t] is a copy of the currently visible events. *)

val start : t -> unit -> (start_result, error) result
(** [start t ()] fetches the target and its context and publishes the result in
    chronological order. It is valid in {!Initial}; after {!reset}, it may be
    started again. A failed request leaves the view empty. After {!close}, it is
    a typed no-op returning an empty result. In {!Automatic} mode, an unthreaded
    target uses room pagination while hiding threaded replies; threaded targets
    use the thread's [/relations] pagination. {!Force} mode always uses
    [/relations], treating an unthreaded target as the root. *)

val paginate_backward : t -> ?limit:int -> unit -> (page, error) result
(** Fetch one page toward the beginning. The server's reverse-chronological page
    is reversed before it is prepended. A failed request leaves the visible list
    unchanged. Negative [limit] values raise [Invalid_argument]. After {!close},
    this is a successful empty, terminal page. *)

val paginate_forward : t -> ?limit:int -> unit -> (page, error) result
(** Fetch one page toward the live edge and append it in server order. A failed
    request leaves the visible list unchanged. Negative [limit] values raise
    [Invalid_argument]. After {!close}, this is a successful empty, terminal
    page. *)

val reset : t -> (unit, error) result
(** Reset the paginator and clear the observable list. A reset rejected by an
    in-flight paginator leaves the list unchanged. Reset is a successful no-op
    after {!close}. *)

val state : t -> state
(** The wrapper state, including {!Closed}. *)

val subscribe : t -> (state -> unit) -> unit -> unit
(** Subscribe to wrapper state transitions. The callback is called immediately
    with the current state; the returned function is idempotent. *)

val close : t -> unit
(** Close the view, clear its observable list, and make later operations typed
    no-ops. Idempotent. *)
