(** A small paged list of thread-root events.

    This is backed by the Matrix v1 [/rooms/{roomId}/threads] endpoint. It
    deliberately returns raw events: bundled thread summaries are not part of
    the protocol type currently exposed by [matrix-chat.proto]. *)

type state = Start | Loading | Next of string | End | Failed of Error.t
type t

val create :
  ?on_root:(Matrix_proto.Event.Raw_event.t -> unit) ->
  client:Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  unit ->
  t
(** [on_root] is called for each page event that has an event ID, including a
    repeated ID. This lets an aggregate merge a later, richer bundled summary
    while the paginator still exposes each root only once. It must not perform a
    re-entrant pagination call. *)

val set_filter : t -> Relations.thread_filter -> unit
(** [set_filter t filter] clears all roots and pagination progress. It raises
    [Invalid_argument] if called from a re-entrant fetch callback. *)

val reset : t -> unit
(** [reset t] clears roots and pagination while retaining the filter. It has the
    same re-entrancy restriction as {!set_filter}. *)

val close : t -> unit
(** [close t] invalidates the paginator. An in-flight response is discarded, all
    loaded roots are cleared, and later operations are successful no-ops.
    Idempotent. *)

val state : t -> state
(** The current request/token state. *)

val roots : t -> Matrix_proto.Event.Raw_event.t list
(** Successfully loaded roots in server/page order. A malformed entry without an
    event ID is omitted, and an event ID repeated by a later page occurs only
    once. *)

val loaded_pages : t -> int
val is_at_last_page : t -> bool

val subscribe : t -> (state -> unit) -> unit -> unit
(** [subscribe t f] reports the current state immediately and every distinct
    transition afterward. The returned function unsubscribes idempotently. *)

val next_page : t -> ?limit:int -> unit -> (unit, Error.t) result
(** [next_page t] fetches and appends one page. Event IDs already returned by
    earlier pages are omitted. A failed request keeps the token, roots and page
    count so the next call retries it. At the end—or re-entrantly while another
    page is loading—it succeeds without making another request, matching the
    pinned Rust [ThreadListService::paginate] guard.

    Raises [Invalid_argument] if [limit] is not positive. *)
