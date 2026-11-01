(** sliding_sync_state — a room list accumulated from sliding sync responses.

    A sliding sync response carries deltas, so what is known about a room is the
    fold of every response since the session began. {!apply} performs that fold
    and the accessors read the result.

    The fold keeps enough to render and order a room list, including a bounded
    raw timeline, and can persist a complete versioned snapshot. It does not
    evaluate push rules or decrypt events. It also takes the server's [name] and
    [is_dm] at face value rather than recomputing them from heroes, member
    counts and the [m.direct] account data, which is what {!Base_client} does.
*)

type t
(** The type for accumulated sliding sync state. *)

type room
(** The type for what is known about one room. *)

type profile = (string * Jsont.json) list
(** A deterministic profile field map. Fields are sorted by name. *)

(** {1 The fold} *)

val empty : t
(** [empty] is the state before any response, with nothing known. *)

val load : Store.t -> (t, Error.t) result
(** [load store] restores the complete accumulated state from the sliding-sync
    slot. A missing slot is [Ok empty]. Unsupported or malformed snapshots are
    returned as {!Error.Json_error}. *)

val load_opt : Store.t -> (t option, Error.t) result
(** [load_opt store] distinguishes a missing snapshot from a persisted {!empty}
    snapshot. This matters after an expired sliding-sync position has
    deliberately been cleared. *)

val discard : Store.t -> unit
(** [discard store] stages removal of the private legacy sliding-sync slot. It
    does not flush, so a caller can combine the removal with writes to the
    common {!Store} projection in one transaction. *)

val save : Store.t -> t -> (unit, Error.t) result
(** [save store state] stores and flushes the complete sliding-sync snapshot,
    including its own accumulated profiles. If setting or flushing fails, the
    sliding-sync slot is restored to its previous in-memory value. This does not
    mirror profiles into the common {!Store} profile snapshot; that snapshot is
    owned by {!Matrix_eio.Sync_service} when a sliding loop is bridged to an
    active base client. *)

val apply :
  ?advance_to_device:bool -> t -> Matrix_proto.Sliding_sync.Response.t -> t
(** [apply ~advance_to_device t r] folds [r] into [t]. An absent member leaves
    the previous value alone, since MSC4186 sends deltas. The exceptions are the
    avatar, where an explicit JSON null clears it, and the invite state, whose
    absence means the room is no longer an invite. [advance_to_device] defaults
    to [true]; loops set it to [false] when the request did not enable that
    extension, so an unsolicited cursor cannot later skip messages. *)

val pos : t -> string option
(** [pos t] is the [pos] of the last response applied, and [None] before the
    first. *)

val to_device_since : t -> string option
(** [to_device_since t] is the last [extensions.to_device.next_batch] seen, and
    [None] if the extension has never answered. *)

val lists : t -> (string * int) list
(** [lists t] is each named list's total room count, sorted by name. *)

val profiles : t -> (Matrix_proto.Id.User_id.t * profile) list
(** [profiles t] is every accumulated global profile, sorted by user ID. *)

val find_profile : t -> Matrix_proto.Id.User_id.t -> profile option
(** [find_profile t user_id] returns the user's accumulated profile fields, or
    [None] after a drop or before the first update. *)

val find_profile_field :
  t -> Matrix_proto.Id.User_id.t -> string -> Jsont.json option
(** [find_profile_field t user_id field] reads one profile field. *)

val rooms_by_recency : t -> room list
(** [rooms_by_recency t] is every known room, most recently bumped first. Rooms
    sort by descending [bump_stamp], those without one last, ties broken by room
    id so the order is total. This is the ordering a sliding sync room list is
    meant to be rendered in. *)

val find_room : t -> Matrix_proto.Id.Room_id.t -> room option
(** [find_room t room_id] is what is known about [room_id], and [None] if no
    response has mentioned it. *)

val timeline_capacity : int
(** [timeline_capacity] is [100], how many timeline events {!apply} retains per
    room. *)

(** {1 Rooms} *)

val room_id : room -> Matrix_proto.Id.Room_id.t
(** [room_id r] is the room [r] describes. *)

val name : room -> string option
(** [name r] is the name the server computed, and [None] if it sent none. *)

val avatar_url : room -> string option
(** [avatar_url r] is the room's [mxc://] avatar, and [None] if it has none. *)

val is_dm : room -> bool option
(** [is_dm r] is whether the server calls the room a direct message, and [None]
    if it did not say. *)

val is_invite : room -> bool
(** [is_invite r] is [true] when the last response carried invite state, so the
    invitation is still unaccepted. *)

val highlight_count : room -> int
(** [highlight_count r] is the server's count of unread highlights. *)

val notification_count : room -> int
(** [notification_count r] is the server's count of unread notifications. *)

val timeline : room -> Matrix_proto.Event.Raw_event.t list
(** [timeline r] is the most recent events, oldest first, at most
    {!timeline_capacity} of them. A response marked limited marks a gap, so
    {!apply} clears what came before it. *)

val required_state :
  room ->
  (Matrix_proto.Sliding_sync.Required_state.t * Matrix_proto.Event.Raw_event.t)
  list
(** [required_state r] is the room's current state, one entry per key, each
    holding the latest event seen for it. *)

val find_state :
  room ->
  Matrix_proto.Sliding_sync.Required_state.t ->
  Matrix_proto.Event.Raw_event.t option
(** [find_state r k] is the latest state event under [k]. *)

val prev_batch : room -> string option
(** [prev_batch r] is the token for the events before {!timeline}, and [None] if
    no response gave one. *)

val joined_count : room -> int option
(** [joined_count r] is how many members have joined, and [None] if the server
    did not say. *)

val invited_count : room -> int option
(** [invited_count r] is how many members are invited, and [None] if the server
    did not say. *)

val bump_stamp : room -> int option
(** [bump_stamp r] is the recency stamp {!rooms_by_recency} sorts on, and [None]
    if the server did not send one. *)

val heroes : room -> Matrix_proto.Sliding_sync.Response.hero list
(** [heroes r] is the members the server offered for naming the room. *)
