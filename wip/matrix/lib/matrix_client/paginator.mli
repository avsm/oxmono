(** Stateful room or thread pagination around one event.

    A paginator starts with [/context], then walks independently toward the
    beginning or end with [/messages], or with recursive [/relations] requests
    when the selected focus is a thread. It deliberately does not write into an
    event cache: event-focused and pinned-event views can decide where the
    returned events belong, while {!Matrix_ui.Room_timeline} keeps owning its
    chunked room cache.

    The state guard mirrors [matrix-sdk/src/paginators/room.rs] at the pinned
    Rust parity revision. A failed request restores the preceding idle state and
    leaves both tokens unchanged. *)

type state =
  | Initial
  | Fetching_target
  | Idle
  | Paginating  (** The externally visible state of a paginator. *)

type pagination_token =
  | Not_started
  | Has_more of string
  | Hit_end
      (** One edge's progress. [Not_started] asks [/messages] to begin at that
          direction's default edge, [Has_more] carries the next token, and
          [Hit_end] suppresses further requests. *)

type tokens = { previous : pagination_token; next : pagination_token }
(** The independent backward and forward tokens. *)

type thread_mode =
  | Automatic
  | Force
      (** [Automatic] uses thread relations only when the focused event is a
          threaded reply. [Force] treats the focused event as a thread root
          unless it carries a thread relation. *)

type error =
  | Event_not_found of Matrix_proto.Id.Event_id.t
  | Invalid_state of { expected : state; actual : state }
  | Client_error of Error.t
      (** A target-specific failure, an invalid state transition, or the
          underlying Matrix client error. *)

val pp_error : Format.formatter -> error -> unit

type start_result = {
  events : Matrix_proto.Event.Raw_event.t list;
      (** Context in chronological order: events before the target, the target,
          then events after it. Envelopes without an event ID are omitted. In
          thread mode this is restricted to the selected root and its replies.
      *)
  has_previous : bool;
  has_next : bool;
}
(** The result of the initial [/context] request. *)

type page = {
  events : Matrix_proto.Event.Raw_event.t list;
      (** The server's order: newest-first backwards and oldest-first forwards.
          Envelopes without an event ID are omitted. *)
  hit_end : bool;
}
(** One directional [/messages] or [/relations] page. *)

type t
(** A mutable paginator. Calls may be made from multiple fibers provided the
    supplied [Client.t] is safe there; a re-entrant or overlapping transition is
    rejected rather than queued. *)

val create :
  client:Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?thread_mode:thread_mode ->
  unit ->
  t
(** [create ~client ~room_id ?thread_mode ()] is a fresh paginator in
    {!Initial}. *)

val state : t -> state
val tokens : t -> tokens

val thread_root : t -> Matrix_proto.Id.Event_id.t option
(** [thread_root t] is the actual root used for relation pagination. *)

val subscribe : t -> (state -> unit) -> unit -> unit
(** [subscribe t f] immediately calls [f] with the current state and again after
    each transition. The returned function unsubscribes idempotently. *)

val start_from :
  t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?limit:int ->
  unit ->
  (start_result, error) result
(** [start_from t ~event_id ()] fetches the target and its context. It is valid
    only in {!Initial}; [limit] is the total surrounding-event limit and
    defaults to 10. A Matrix [M_NOT_FOUND] is {!Event_not_found}.

    The server returns the events before the target in reverse chronological
    order, so this function reverses that portion before joining the three
    parts. Missing or null edge tokens become {!Hit_end}.

    Raises [Invalid_argument] when [limit] is negative. *)

val paginate_backward : t -> ?limit:int -> unit -> (page, error) result
(** Walk toward the beginning. Valid only in {!Idle}; defaults to 30 events.
    Once the beginning is known, returns an empty page with [hit_end = true]
    without making another request. *)

val paginate_forward : t -> ?limit:int -> unit -> (page, error) result
(** Walk toward the live edge, otherwise as {!paginate_backward}. *)

val reset : t -> (unit, error) result
(** [reset t] forgets both tokens and returns to {!Initial}. It is rejected
    while a request is in progress. *)
