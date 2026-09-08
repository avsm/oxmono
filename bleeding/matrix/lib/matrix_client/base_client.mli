(** base_client — room state maintained across syncs.

    {!apply} folds a [/sync] response into a {!type-state} and reports what
    moved in a {!changes} record. {!persist} copies the parts worth keeping into
    a {!Store}. Nothing here performs I/O, decrypts or uploads keys.
    [Matrix_eio.Sync_service] drives the pair in a long-poll fiber, and the E2EE
    payloads are passed through in {!changes} for {!Encryption} to pick up. *)

(** {1 Room summaries}

    Re-exported from {!Store}, which owns them because it persists them. *)

type membership = Store.membership = Joined | Invited | Left | Knocked

type room_info = Store.room_info
(** {!Store.room_info}. The fields {!apply} maintains that {!Store} only
    describes are [display_name], computed here by {!compute_display_name}, and
    [marked_unread], folded from the room's account data on every response.
    [marked_unread] honours both the stable [m.marked_unread] type and the
    unstable [com.famedly.marked_unread] one, and is cleared when the user's
    read receipt or read marker moves in a response that does not itself carry
    the flag. Once the stable type has been observed, later unstable events are
    ignored. Clearing it does not tell the server. *)

val display_name : room_info -> string
(** [display_name info] is [Store.display_name_to_string info.display_name]. *)

(** {1 State} *)

type state
(** The client's picture of the world. Immutable, since {!apply} returns a new
    one. *)

type profile_change = {
  changed_user_id : Matrix_proto.Id.User_id.t;
  previous_profile : Store.profile option;
  current_profile : Store.profile option;
}
(** One effective MSC4262 global-profile change. [previous_profile] and
    [current_profile] distinguish a missing user from a present profile with no
    fields. Changes are reported in user-id order. *)

val create :
  user_id:Matrix_proto.Id.User_id.t ->
  ?display_name:string ->
  ?ruleset:Matrix_proto.Push.Ruleset.t ->
  ?plaintext_policy:Store.plaintext_policy ->
  unit ->
  state
(** [create ~user_id ()] is an empty state.

    [display_name] is the fallback own display name used by the
    [contains_display_name] push condition until a room's own [m.room.member]
    state is known, and defaults to the user id's localpart. Once known, the
    room-specific membership display name wins. [ruleset] defaults to
    {!Matrix_proto.Push.default_ruleset}[ ~user_id]. A valid [m.push_rules]
    global account-data event in a sync response becomes active before that
    response's notifications are evaluated; a valid stored event is restored by
    {!of_store}. Pass the server's own explicitly with {!with_ruleset} when
    bootstrapping without account data. [plaintext_policy] defaults to
    {!Store.Ciphertext_only}; {!Store.Store_plaintext} is an explicit opt-in
    that may retain decrypted event contents at rest. *)

val of_store :
  Store.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?display_name:string ->
  ?ruleset:Matrix_proto.Push.Ruleset.t ->
  unit ->
  state
(** [of_store store ~user_id ()] resumes from what [store] holds, namely its
    sync token, room summaries, account data, receipts and global profiles. The
    next {!apply} is therefore an incremental sync rather than an initial one.
    The state inherits the store's {!type:Store.plaintext_policy}; that caller
    configuration is not stored in [base_state.json]. *)

val persist : Store.t -> state -> unit
(** [persist store state] copies the state's sync token, room summaries, global
    account data, receipts and complete global-profile snapshot into [store]. It
    does not write to disk, which is {!Store.flush}. It raises
    [Invalid_argument] if the state and store have different
    {!type:Store.plaintext_policy} settings, preventing a secure store from
    being bypassed by persisting a plaintext-configured state.

    The profile snapshot is authoritative: when this store is also used by a
    sliding-sync driver, callers must resume this state with {!of_store} and
    serialize persistence, rather than persist two independently stale
    snapshots. *)

val migrate_legacy_sliding_state :
  Store.t -> state -> (state * bool, Error.t) result
(** [migrate_legacy_sliding_state store state] consumes the private legacy
    [sliding_sync_state] slot, folds it into the common state, and flushes the
    slot removal and common projection as one transaction. An existing common
    sliding cursor wins over the legacy snapshot. The result's boolean is [true]
    when a legacy slot was consumed. On a codec or persistence failure, the
    store is restored to its previous in-memory snapshot and [Error _] is
    returned; an unexpected exception is re-raised after the same rollback. *)

val with_ruleset : state -> Matrix_proto.Push.Ruleset.t -> state
(** [with_ruleset state ruleset] is [state] evaluating push rules with
    [ruleset]. *)

val with_push_rules : state -> Matrix_proto.Push.Ruleset.t -> state
(** [with_push_rules state ruleset] installs [ruleset] as the active rules and
    records the corresponding valid [m.push_rules] global account-data event.
    This is the state-side transition for an endpoint bootstrap; unlike
    {!with_ruleset}, it is durable when {!persist} is subsequently used. *)

val with_display_name : state -> string -> state
(** [with_display_name state name] changes the fallback own display name used
    for rooms whose own [m.room.member] state is not known yet. *)

val user_id : state -> Matrix_proto.Id.User_id.t
(** The user the state belongs to. *)

val next_batch : state -> string option
(** The sync token the next request should resume from, or [None] before the
    first response. *)

val sliding_pos : state -> string option
(** The independent MSC4186 position, or [None] before a sliding response. *)

val sliding_to_device_since : state -> string option
(** The independent MSC4186 to-device cursor, advanced only for responses to
    requests which enabled the to-device extension. *)

val sliding_lists : state -> (string * int) list
(** The latest count for each named sliding-sync list, sorted by name. *)

val reset_sliding_session : state -> state
(** [reset_sliding_session state] drops the sliding position, to-device cursor
    and list counts without discarding durable rooms, receipts, profiles,
    account data, or the classic sync token. Use it after [M_UNKNOWN_POS]. *)

val ruleset : state -> Matrix_proto.Push.Ruleset.t
(** The push rules {!apply} evaluates. *)

val rooms : state -> room_info list
(** Every known room. Rooms with a [recency_stamp] precede rooms without one;
    differing stamps are ordered newest first. Equal or absent stamps fall back
    to [last_active_ts], most recently active first. *)

val find_room : state -> Matrix_proto.Id.Room_id.t -> room_info option
(** [find_room state room_id] is what is known about [room_id]. *)

val forget_room : state -> Matrix_proto.Id.Room_id.t -> state
(** [forget_room state room_id] removes the room and all room-scoped derived
    projections from the pure state. Global account data, including [m.direct],
    is left untouched. *)

val remove_direct_room : state -> Matrix_proto.Id.Room_id.t -> state
(** [remove_direct_room state room_id] removes [room_id] from every entry in the
    local [m.direct] account-data projection, preserving all unrelated account
    data. It is used after the corresponding remote update succeeds. *)

val inviter :
  state -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t option
(** [inviter state room_id] is the authenticated sender of the own user's
    current stripped [m.room.member] invite, or [None] when the room is not
    currently invited or the invite state is absent or malformed. *)

val rooms_with : state -> membership -> room_info list
(** [rooms_with state m] is every known room whose membership is [m], in the
    order {!val-rooms} gives. *)

val find_account_data : state -> string -> Jsont.json option
(** [find_account_data state event_type] is the content of that global
    account-data event. *)

val all_account_data : state -> (string * Jsont.json) list
(** Every global account-data event, as type and content. *)

val profiles : state -> (Matrix_proto.Id.User_id.t * Store.profile) list
(** [profiles state] is the complete MSC4262 global-profile snapshot, ordered by
    user id. Each profile's arbitrary JSON fields are ordered by name. *)

val find_profile : state -> Matrix_proto.Id.User_id.t -> Store.profile option
(** [find_profile state user_id] is that user's accumulated global profile.
    [Some []] means a profile is present but has no fields. *)

val find_profile_field :
  state -> Matrix_proto.Id.User_id.t -> string -> Jsont.json option
(** [find_profile_field state user_id field] is the opaque JSON value currently
    stored for [field]. *)

val apply_profile_updates :
  state ->
  Matrix_proto.Sliding_sync.Response.profiles ->
  state * profile_change list
(** [apply_profile_updates state updates] folds an MSC4262 profile delta into
    [state]. An [Updated] map patches only the named fields, JSON null deletes a
    field, and [Dropped] removes the whole user. Unknown fields and values are
    retained. Only effective before/after changes are reported; the result and
    changes are deterministic even if a caller constructs duplicate entries.

    This pure fold is shared by sliding-sync drivers. Classic {!apply} has no
    profile extension and therefore reports no profile changes. Updating room
    heroes or other room-derived profile consumers is outside this core fold. *)

val receipts : state -> Matrix_proto.Id.Room_id.t -> Read_state.t
(** [receipts state room_id] is the read positions known for [room_id], and
    {!Read_state.empty} for a room with none. *)

val with_local_unread_counts :
  state -> room_id:Matrix_proto.Id.Room_id.t -> Read_state.counts -> state
(** [with_local_unread_counts state ~room_id counts] updates only the local
    unread counters. Unknown rooms and equal counters return [state] itself. *)

val presence : state -> Jsont.json list
(** [presence state] is the [m.presence] events of the last response, as they
    arrived. Empty when that response carried none. *)

val members :
  state -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t list
(** [members state room_id] is everybody this state has seen join or be invited
    to [room_id], which is what {!Encryption.encrypt_room_event}'s [~members]
    wants.

    It is built from the [m.room.member] events the responses carried and is
    restored from {!Store.room_info.state_events} after a restart. A sync that
    lazy-loads members still gives a partial list; consult
    {!Store.room_info.members_complete} before treating it as exhaustive. A
    limited timeline does not make already-complete state partial: only the
    request's state coverage determines completeness. Nothing here calls
    [/joined_members]. *)

val replace_members :
  state -> Matrix_proto.Id.Room_id.t -> Rooms.member list -> state
(** [replace_members state room_id members] installs the authoritative current
    membership snapshot returned by [/rooms/{roomId}/members], marks a joined
    room's member state complete, and retains normalized member state events so
    the result survives persistence. An unknown room is left unchanged. *)

val human_members :
  state -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t list
(** [human_members state room_id] is {!members} without the service users named
    by [m.room.member_hints] or the legacy [io.element.functional_members] state
    event. The unfiltered set remains the one to use for encryption. *)

val service_members : room_info -> Matrix_proto.Id.User_id.t list
(** [service_members room] is the room's service-user hint. The stable
    [m.room.member_hints] event takes precedence over the legacy Element event.
*)

val human_member_count : room_info -> int
(** [human_member_count room] is the joined-plus-invited summary count with
    service users whose active member state is cached subtracted. *)

val state_events : state -> Matrix_proto.Id.Room_id.t -> Store.state_event list
(** [state_events state room_id] is the durable current-state projection for the
    room, including unknown event types. *)

val find_state_event :
  state ->
  Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  ?state_key:string ->
  unit ->
  Store.state_event option
(** [find_state_event state room_id ~event_type ~state_key ()] looks up one
    event in the durable projection. [state_key] defaults to [""]. *)

val retention : room_info -> Matrix_proto.Event.Room_retention_content.t option
(** [retention room] decodes the stable or MSC1763 retention state event in
    [room]. It survives store reload because it is derived from the durable
    state-event projection. *)

val push_context :
  state -> Matrix_proto.Id.Room_id.t -> Push_evaluator.Context.t
(** [push_context state room_id] is what {!Push_evaluator.evaluate} needs for
    [room_id], namely the user's id and room-specific membership display name,
    the room's joined-member count, and its power levels as last seen. The
    state's fallback display name is used only if own membership is unknown. *)

(** {1 Applying a response} *)

type state_coverage = {
  state_complete : bool;
  members_complete : bool;
  encryption_state_complete : bool;
}
(** What the request that produced a response guaranteed. This is separate from
    the response because a named server-side filter cannot be inspected from its
    JSON. *)

val unknown_state_coverage : state_coverage
(** Makes no completeness claim. State deltas are still cached and previously
    established completeness is retained. *)

val complete_state_coverage : state_coverage
(** Says the request was unfiltered and covered all state, members and the
    meaningful absence or presence of [m.room.encryption]. *)

type decrypted = {
  encrypted : Matrix_proto.Event.Raw_event.t;
      (** The [m.room.encrypted] event as the server sent it. It also appears,
          in place, in {!room_change.timeline}. *)
  plaintext : Matrix_proto.Event.Raw_event.t;
      (** The same envelope, carrying the type and content Megolm recovered.
          This is the event to show, and the one the push rules were evaluated
          against. *)
  info : Encryption.decrypted_event;
      (** Which session decrypted it, whose keys signed it, and how far the
          sending device is trusted. *)
}
(** A timeline event that arrived encrypted and came back out. *)

type room_change = {
  changed_room_id : Matrix_proto.Id.Room_id.t;
  info : room_info;  (** The room after the response. *)
  previous : room_info option;  (** As it was, or [None] for a new room. *)
  timeline : Matrix_proto.Event.Raw_event.t list;
      (** New timeline events, oldest first, exactly as they arrived. An
          [m.room.encrypted] event stays encrypted here. *)
  decrypted : decrypted list;
      (** The timeline's [m.room.encrypted] events that {!apply}'s [decrypt]
          opened, in timeline order. Empty when no [decrypt] was given. *)
  undecrypted :
    (Matrix_proto.Event.Raw_event.t * Encryption.decrypt_error) list;
      (** The ones it could not. An {!Encryption.Unknown_session} here is the
          normal "the key has not arrived yet" case, and is worth retrying once
          {!Encryption.outcome.new_sessions} mentions the session. *)
  state_events : Matrix_proto.Event.Raw_event.t list;
      (** State delta outside the timeline. *)
  ephemeral : Jsont.json list;  (** Typing and receipt events. *)
  room_account_data : (string * Jsont.json) list;
  limited : bool;
      (** The server dropped events, so any timeline this caller holds has a gap
          and should be refilled from [info.prev_batch]. *)
  unread : Read_state.counts;
      (** What this response added to the room's local counts. *)
}
(** What one response moved in one room. *)

type changes = {
  batch : string;  (** The response's [next_batch]. *)
  room_changes : room_change list;
  profile_changes : profile_change list;
      (** Empty for classic [/sync], which has no MSC4262 extension. *)
  global_account_data : (string * Jsont.json) list;
  to_device : Jsont.json list;
  device_lists : Matrix_proto.Sync.Device_lists.t option;
  one_time_keys_count : (string * int) list;
  unused_fallback_key_types : string list option;
  presence_events : Jsont.json list;
}
(** Everything the response moved, for a caller that reacts rather than
    re-reads. The E2EE fields are passed through untouched, since this module
    neither decrypts nor uploads keys. *)

val apply :
  ?decrypt:
    (Matrix_proto.Id.Room_id.t ->
    Matrix_proto.Event.Raw_event.t ->
    (Encryption.decrypted_event, Encryption.decrypt_error) result) ->
  ?coverage:state_coverage ->
  state ->
  Matrix_proto.Sync.Response.t ->
  state * changes
(** [apply state response] is [state] with [response] folded in, and what that
    moved. [coverage] defaults to {!unknown_state_coverage}; pass
    {!complete_state_coverage} for a response to an unfiltered classic [/sync].
    A limited timeline creates a history gap but does not reduce state or member
    completeness already established by the request or an earlier response.

    [decrypt] is called for every [m.room.encrypted] timeline event before
    anything reads an event's type or content, so the local unread and highlight
    counts of an encrypted room are computed from the plaintext. Both its
    outcomes are reported in {!room_change.decrypted} and
    {!room_change.undecrypted}. {!room_change.timeline} always carries the
    server event. [room_info.latest_event] carries the plaintext only when the
    state was created with {!Store.Store_plaintext}; the default
    {!Store.Ciphertext_only} keeps the ciphertext representation, and no
    plaintext reaches the {!Store}. Without [decrypt], every encrypted event
    stays encrypted.

    [decrypt] is normally {!Encryption.decrypt_room_event} partially applied to
    a machine that has already seen this response, so run
    {!Encryption.process_sync} first or the room keys this response delivered
    will not be in the machine yet.

    Unread and highlight counts are computed locally with {!Push_evaluator} over
    the new timeline events and kept beside the server's, which the server
    cannot get right for an encrypted room. *)

val apply_sliding :
  ?decrypt:
    (Matrix_proto.Id.Room_id.t ->
    Matrix_proto.Event.Raw_event.t ->
    (Encryption.decrypted_event, Encryption.decrypt_error) result) ->
  ?to_device_enabled:bool ->
  state ->
  Matrix_proto.Sliding_sync.Response.t ->
  state * changes
(** [apply_sliding state response] folds an MSC4186 response into the same room
    and account state as {!apply}, while advancing a separate sliding position.
    Room state is derived only from [required_state], never state events in the
    timeline. Invite/knock membership, the response avatar tri-state, heroes,
    counts, room extensions, receipts and typing are folded for the union of
    response rooms and extension-only known rooms. Unknown extension-only rooms
    are not invented.

    The server-provided room [name] and [is_dm] fields are deliberately ignored:
    names are derived from current room state/heroes and directness from global
    [m.direct]. [num_live], when present, restricts local unread evaluation to
    the final live suffix without hiding historical timeline events from the
    caller. Global profiles are applied exactly once and reported through
    {!changes.profile_changes} without rewriting room membership/hero display.

    [to_device_enabled] defaults to [true]. When false, an unsolicited to-device
    cursor is exposed in [changes] but cannot advance the persisted cursor. As
    with {!apply}, process the response through
    {!Encryption.process_sliding_sync} before supplying [decrypt]. *)

(** Callbacks run once a response has been applied.

    An extension point for a layer this module does not know about, such as an
    encryption service that needs the to-device traffic or a UI that wants every
    timeline event, so that it can be attached to the driver rather than wired
    in here. *)
module Hooks : sig
  type t
  (** The type for a set of registered callbacks. *)

  val create : unit -> t
  (** [create ()] is an empty set. *)

  val on_response :
    t -> (state -> Matrix_proto.Sync.Response.t -> changes -> unit) -> unit
  (** [on_response t f] registers [f] to run once per applied response, with the
      state {!apply} produced. Callbacks run in registration order. *)

  val on_sliding_response :
    t ->
    (state -> Matrix_proto.Sliding_sync.Response.t -> changes -> unit) ->
    unit
  (** [on_sliding_response t f] is the MSC4186 counterpart of {!on_response}. *)

  val on_room_event :
    t ->
    (Matrix_proto.Id.Room_id.t -> Matrix_proto.Event.Raw_event.t -> unit) ->
    unit
  (** [on_room_event t f] registers [f] to run for every new timeline event of
      every room, in timeline order. *)

  val run : t -> state -> Matrix_proto.Sync.Response.t -> changes -> unit
  (** [run t state response changes] runs the registered callbacks, the room
      event ones first. The Eio driver calls it, and a caller driving {!apply}
      itself should too. An exception from a callback propagates. *)

  val run_sliding :
    t -> state -> Matrix_proto.Sliding_sync.Response.t -> changes -> unit
  (** [run_sliding] runs room-event callbacks first and then the registered
      sliding-response callbacks. *)
end

(** {1 Room naming}

    Exposed so the algorithm can be run without a sync response, for a room
    preview or a space summary. *)

val compute_display_name :
  user_id:Matrix_proto.Id.User_id.t -> room_info -> Store.display_name
(** [compute_display_name ~user_id info] is the specification's algorithm,
    namely a non-empty [m.room.name], else the canonical alias, else a name
    built from the heroes and the member counts.

    In the hero branch [user_id] is dropped from the heroes and the remaining
    display names are sorted and joined with [", "]. Then no heroes but more
    than one member gives ["N people"]; as many heroes as there are other
    members gives just the names; fewer gives ["A, B, and N others"], where N is
    [joined + invited - heroes] and does not subtract [user_id]; and one member
    or none gives {!Store.Empty_was} or {!Store.Empty}.

    An invited room's member counts describe a room the user cannot see and are
    not meaningful, so the count is taken as the number of heroes plus one.

    @see <https://spec.matrix.org/v1.11/client-server-api/#calculating-the-display-name-for-a-room>
      Calculating the display name for a room *)
