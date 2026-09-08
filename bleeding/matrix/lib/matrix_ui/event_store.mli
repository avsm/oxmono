(** event_store — persistent storage behind the UI event cache.

    A store holds a room as an ordered list of chunks rather than as a flat list
    of events. A chunk either holds events or stands for a hole in the history,
    carrying the token that fills it, so a client which fell out of the server's
    sync window keeps its persisted history {e behind} the hole rather than
    throwing it away.

    {!memory} is the backend this library ships. [matrix-chat.ui.sqlite] provides one
    over a file, and {!v} takes any other implementation of {!S}. *)

(** {1 Failures} *)

module Error : sig
  (** The type for what a store operation failed with. *)
  type t =
    | Closed  (** The store was closed before the call. *)
    | Codec of string  (** An event could not be encoded or decoded. *)
    | Backend of string  (** The backend refused the operation. *)

  val to_string : t -> string
  (** [to_string e] is a one-line message naming the failure. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] prints {!to_string} on [ppf]. *)
end

(** {1 The chunk model} *)

module Internal : sig
  (** The representation the event cache and the store backends share. It is
      exported so that a backend outside this library can implement {!S}. *)

  (** The type for the delivery state of a locally-sent event. *)
  type delivery =
    | Synced  (** The server has the event. *)
    | Sending  (** A request for it is in flight. *)
    | Queued  (** The send queue holds it and has not sent it yet. *)
    | Failed of string  (** The send was given up on, with this reason. *)

  type event = {
    stable_id : string;
        (** Identifies the event across saves and reloads. It is derived from
            the event id, or from the transaction id while the event is a local
            echo. *)
    event : Matrix_proto.Event.Raw_event.t;
    clear_event : Matrix_proto.Event.Raw_event.t option;
        (** The plaintext of an [m.room.encrypted] event, once one is known. *)
    delivery : delivery;
  }
  (** The type for stored events. *)

  type events_chunk = {
    chunk_id : int;
    prev_token : string option;  (** Back-paginates to before [events]. *)
    next_token : string option;
        (** Back-paginates to the chunk's right edge. *)
    events : event list;  (** Oldest first. Never empty in a stored room. *)
  }
  (** The type for runs of contiguous events. *)

  type gap_chunk = {
    gap_id : int;
    token : string;
        (** Back-paginates from the start of the chunk that follows the gap, so
            that filling the gap walks backwards into it. *)
  }
  (** The type for holes in a room's history. *)

  (** The type for chunks. Every chunk carries the back-pagination token of its
      {e left edge}, the token to hand [/messages?dir=b] to receive the events
      immediately before the chunk's first event. For a {!Gap} that is [token],
      and for an {!Events} chunk it is [prev_token], which is [None] only where
      the token was never learnt, at the room's beginning or on a chunk built by
      resolving a gap whose response had no [end]. *)
  type chunk = Events of events_chunk | Gap of gap_chunk

  (** Persisted shape without decoding event rows. *)
  type chunk_metadata =
    | Events_metadata of {
        chunk_id : int;
        prev_token : string option;
        next_token : string option;
        stable_ids : string list;
            (** Exact event-row identities in oldest-first order. Never empty.
            *)
      }
    | Gap_metadata of gap_chunk

  type room_metadata = {
    chunks : chunk_metadata list;  (** Oldest first. *)
    next_chunk_id : int;
    external_events : event list;
  }
  (** The complete persisted topology. Chunk IDs are unique and below
      [next_chunk_id], event stable IDs are unique across the room, gaps are
      neither adjacent nor trailing, and event metadata is never empty. *)

  val max_external_events : int
  (** The maximum number of detached events retained per room. The newest
      records are retained when a caller supplies more than this bound. *)

  type room = {
    chunks : chunk list;  (** Oldest first. *)
    next_chunk_id : int;  (** The id the room's next chunk takes. *)
    external_events : event list;
        (** Events held outside the timeline, oldest first. These are bounded by
            {!max_external_events}, require an event ID, and have [Synced]
            delivery. *)
  }
  (** The type for stored rooms. *)

  val validate_layout : room -> (unit, string) result
  (** [validate_layout room] checks chunk/counter identities and the gap
      topology. Event lists are deliberately ignored so this also validates a
      layout-only {!Layout} value. *)

  val validate_room : room -> (unit, string) result
  (** [validate_room room] additionally requires non-empty event chunks and
      unique, non-empty stable event identities. *)

  (** A cold-load result. [Tail] contains the complete layout and detached
      events, but only the newest events chunk has been decoded. *)
  type initial =
    | Full of room
    | Tail of { metadata : room_metadata; newest : events_chunk option }

  val validate_metadata : room_metadata -> (unit, string) result
  (** [validate_metadata metadata] checks the lazy layout, counters and stable
      identities without decoding event rows. *)

  val validate_initial : initial -> (unit, string) result
  (** [validate_initial initial] additionally checks that a lazy tail is absent
      exactly for an empty layout, or exactly matches its final metadata. *)

  val room_events : room -> event list
  (** [room_events room] is the chunks' events concatenated, oldest first. *)

  val room_prev_batch : room -> string option
  (** [room_prev_batch room] is the token at the room's oldest edge, which is
      the front chunk's left-edge token. *)

  val room_has_gap : room -> bool
  (** [room_has_gap room] is [true] when the room holds a gap anywhere. *)

  (** The type for one step of an incremental save. *)
  type change =
    | Layout of room
        (** Replace the chunk structure and detached-event list, and with it
            [next_chunk_id]. Events inside chunks are ignored, so only the shape
            is written, and rows belonging to chunks the layout no longer
            mentions are deleted. *)
    | Put_event of { chunk_id : int; position : int; event : event }
        (** Insert or replace one event, keyed by its [stable_id]. *)
    | Replace_events_chunk of { chunk_id : int; events : event list }
        (** Authoritatively replace one loaded chunk's rows, retaining its
            layout and all neighbouring (possibly unloaded) chunks. *)
    | Delete_event of { stable_id : string }
end

(** {1 Backends} *)

module type S = sig
  (** The operations a store backend provides. Each is called with the store's
      lock held, so an implementation is never re-entered concurrently, and each
      may run for as long as its medium takes. *)

  type t
  (** The type for backends. *)

  val load_room :
    t -> Matrix_proto.Id.Room_id.t -> (Internal.room option, Error.t) result
  (** [load_room t room_id] is the stored room, or [None] where the backend
      holds none. *)

  val save_room :
    t -> Matrix_proto.Id.Room_id.t -> Internal.room -> (unit, Error.t) result
  (** [save_room t room_id room] rewrites the room whole. *)

  val apply :
    t ->
    Matrix_proto.Id.Room_id.t ->
    Internal.change list ->
    (unit, Error.t) result
  (** [apply t room_id changes] applies a delta atomically, at a cost
      proportional to the delta rather than to the size of the room. *)

  val remove_room : t -> Matrix_proto.Id.Room_id.t -> (unit, Error.t) result
  (** [remove_room t room_id] forgets everything held for the room. *)

  val close : t -> unit
  (** [close t] releases the backend's resources. It is called once. *)
end

module type Lazy_S = sig
  (** The versioned backend extension used by {!v_lazy}. *)

  include S

  val load_room_initial :
    t -> Matrix_proto.Id.Room_id.t -> (Internal.initial option, Error.t) result
  (** [load_room_initial t room_id] returns either an eager legacy room or its
      complete ordered metadata plus exactly the final events chunk. For a
      [Tail], [newest] is [None] exactly when [chunks] is empty; otherwise it
      must match the final {!Internal.Events_metadata} entry byte-for-byte in
      identity, tokens and stable-ID order. *)

  val load_events_chunk :
    t ->
    Matrix_proto.Id.Room_id.t ->
    int ->
    (Internal.events_chunk option, Error.t) result
  (** [load_events_chunk t room_id chunk_id] decodes that events chunk, or is
      [None] when it no longer exists or is not an events chunk. The returned
      identity, tokens and stable-ID order must still match the initial
      metadata. *)
end

(** {1 Stores} *)

(** The type for whether a store may hold the plaintext of an encrypted event.
*)
type plaintext_policy =
  | Store_plaintext
  | Ciphertext_only
      (** Write the wire event only, so that a decrypted body never reaches the
          medium. *)

type t
(** The type for stores. Calls are serialized, so a store may be shared by
    several fibers. *)

val v :
  ?plaintext_policy:plaintext_policy ->
  (module S with type t = 'backend) ->
  'backend ->
  t
(** [v (module B) backend] is a store over [backend]. [plaintext_policy]
    defaults to {!Ciphertext_only}, and the backend never sees an event the
    policy excludes. *)

val v_lazy :
  ?plaintext_policy:plaintext_policy ->
  (module Lazy_S with type t = 'backend) ->
  'backend ->
  t
(** [v_lazy] is [v] over a backend which supports layout and per-chunk loads.
    Its ordinary {!load_room} operation remains available and is eager. *)

val memory : ?plaintext_policy:plaintext_policy -> unit -> t
(** [memory ()] is a store that keeps its rooms in this process and loses them
    with it. It implements the lazy initial/tail and per-chunk path.
    [plaintext_policy] defaults to {!Ciphertext_only}. *)

val plaintext_policy : t -> plaintext_policy
(** [plaintext_policy t] is the policy [t] was built with. *)

val load_room :
  t -> Matrix_proto.Id.Room_id.t -> (Internal.room option, Error.t) result
(** [load_room t room_id] is the stored room, or [None] where the store holds
    none. Events the {!val-plaintext_policy} excludes come back without their
    plaintext whatever the medium holds. *)

val load_room_initial :
  t -> Matrix_proto.Id.Room_id.t -> (Internal.initial option, Error.t) result
(** [load_room_initial] performs a lazy-capable initial load. Backends supplied
    through {!v} return {!Internal.Full}; backends supplied through {!v_lazy}
    choose their representation. *)

val load_events_chunk :
  t ->
  Matrix_proto.Id.Room_id.t ->
  int ->
  (Internal.events_chunk option, Error.t) result
(** [load_events_chunk t room_id chunk_id] decodes one persisted events chunk.
*)

val save_room :
  t -> Matrix_proto.Id.Room_id.t -> Internal.room -> (unit, Error.t) result
(** [save_room t room_id room] rewrites the room whole. {!apply} is what writes
    a delta. *)

val apply :
  t ->
  Matrix_proto.Id.Room_id.t ->
  Internal.change list ->
  (unit, Error.t) result
(** [apply t room_id changes] applies a delta in one transaction. An empty
    [changes] writes nothing. *)

val remove_room : t -> Matrix_proto.Id.Room_id.t -> (unit, Error.t) result
(** [remove_room t room_id] forgets everything the store holds for the room. *)

val close : t -> unit
(** [close t] closes the backend. Every later call is {!Error.Closed}.
    Idempotent. *)
