(** store — the base client's persisted state.

    Room summaries, global account data, receipts, global user profiles and the
    sync token. Holding these across a restart is what lets a client resume an
    incremental [/sync] instead of asking the server for everything again.
    {!Base_client} computes the values and this module keeps them.

    {!memory} and {!on_disk} share one {!t}, so a caller picks a backend once.
    Every setter writes to memory and marks the store dirty, and only {!flush}
    touches the filesystem. An event cache, cryptographic state and media are
    out of scope. *)

(** {1 Room summaries} *)

(** Which of the four [/sync] room maps a room last appeared in. *)
type membership = Joined | Invited | Left | Knocked

val membership_to_string : membership -> string
(** [membership_to_string m] is the wire form of [m]. *)

val membership_of_string : string -> (membership, [> `Msg of string ]) result
(** [membership_of_string s] is the membership [s] names. A ban arrives in
    [rooms.leave] and so reads back as {!Left}. *)

type hero = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
      (** Resolved from the room's [m.room.member] events when one has been
          seen. *)
  avatar_url : string option;
      (** Resolved from the room's [m.room.member] events when one has been
          seen. *)
}
(** A member the server nominated to name the room with. *)

(** The outcome of the specification's room-name algorithm, kept structured so a
    UI can tell a real name from a computed one.

    @see <https://spec.matrix.org/v1.11/client-server-api/#calculating-the-display-name-for-a-room>
      Calculating the display name for a room *)
type display_name =
  | Named of string  (** From [m.room.name]. *)
  | Aliased of string  (** From [m.room.canonical_alias]. *)
  | Calculated of string  (** From the heroes and the member counts. *)
  | Empty_was of string
      (** Everyone has left, and [string] is the name the heroes gave the room
          while they were in it. *)
  | Empty  (** Everyone has left and there are no heroes. *)

val display_name_to_string : display_name -> string
(** [display_name_to_string d] is the rendered form of [d]. {!Empty_was}[ s] is
    ["Empty Room (was " ^ s ^ ")"], {!Empty} is ["Empty Room"], and the rest are
    their payload. *)

type state_completeness =
  | No_state
  | Partial
  | Complete
      (** How much of a room's state the last synchronization source promised.
          [Complete] does not imply that every member profile was returned when
          lazy loading was enabled; {!room_info.members_complete} records that
          separately. *)

type marked_unread_source =
  | Stable
  | Unstable
      (** Which account-data spelling last supplied {!room_info.marked_unread}.
          Existing stores default to [Unstable]; once the stable spelling has
          been observed, later unstable events are ignored. *)

type state_event = {
  event_type : Matrix_proto.Event.Event_type.t;
  state_key : string;
  content : Jsont.json;
  sender : Matrix_proto.Id.User_id.t option;
  event_id : Matrix_proto.Id.Event_id.t option;
  origin_server_ts : Matrix_proto.Event.Timestamp.t option;
}
(** A current state event retained by type and state key. Full [/sync] events
    keep their sender, event id and timestamp; stripped invite/knock events keep
    whichever of those fields they carried. Unknown event types and their
    content are deliberately retained. *)

type room_info = {
  room_id : Matrix_proto.Id.Room_id.t;
  membership : membership;
  name : string option;  (** The [m.room.name], verbatim. *)
  canonical_alias : Matrix_proto.Id.Room_alias.t option;
  topic : string option;
  avatar_url : string option;
  encryption : Jsont.json option;
      (** The [m.room.encryption] content, if the room is encrypted. *)
  heroes : hero list;
  joined_member_count : int;  (** The summary's [m.joined_member_count]. *)
  invited_member_count : int;  (** The summary's [m.invited_member_count]. *)
  is_dm : bool;
      (** Derived from global [m.direct] for joined and left rooms, and from the
          user's own stripped membership event for invited rooms. It is always
          [false] for knocked rooms. *)
  display_name : display_name;
      (** Computed by {!Base_client.compute_display_name}. *)
  notification_count : int;  (** Server-side [unread_notifications]. *)
  highlight_count : int;  (** Server-side [unread_notifications]. *)
  local_unread_count : int;  (** {!Read_state.counts.unread}. *)
  local_notification_count : int;  (** {!Read_state.counts.notifications}. *)
  local_highlight_count : int;  (** {!Read_state.counts.highlights}. *)
  marked_unread : bool;
      (** The room's [m.marked_unread] account data, the "mark as unread" a user
          sets by hand, which no message count reflects. *)
  marked_unread_source : marked_unread_source;
      (** The spelling that last supplied [marked_unread]. *)
  latest_event : Matrix_proto.Event.Raw_event.t option;
      (** The most recent timeline event worth showing in a room list. *)
  prev_batch : string option;  (** Back-pagination token from the last sync. *)
  tags : (string * float option) list;
      (** From the room's [m.tag] account data, as tag name and its [order]. *)
  last_active_ts : int64;
      (** The greatest [origin_server_ts] seen in this room's timeline. *)
  recency_stamp : int option;
      (** Sliding-sync ordering metadata, distinct from {!last_active_ts}. *)
  state_events : state_event list;
      (** The latest event under every state [type, state_key] pair seen for the
          room, including unknown event types and complete member profiles. *)
  state_completeness : state_completeness;
  members_complete : bool;
      (** Whether the member state is known not to be lazy-loaded or gappy. *)
  encryption_state_complete : bool;
      (** Whether the synchronization source explicitly covered the room's
          encryption state, including the meaningful absence of an encryption
          event. *)
}
(** What this library maintains about a room.

    The record stays open because a room list reads most of it, but only one
    module writes each field. {!Base_client.apply} owns everything except the
    three [local_] counts, which it copies from what {!Read_state.count_unread}
    returned, and {!field-display_name}, which
    {!Base_client.compute_display_name} derives from [name], [canonical_alias],
    [heroes] and the two member counts. A caller that stores a hand-built record
    through {!set_room} is telling the next {!Base_client.apply} what the room
    was, so the fields must agree with each other. *)

val empty_room_info :
  room_id:Matrix_proto.Id.Room_id.t -> membership:membership -> room_info
(** [empty_room_info ~room_id ~membership] is a room just heard of, with no
    state, no counts and an {!Empty} name. *)

val room_info_jsont : room_info Jsont.t
(** Reads and writes a room summary as {!flush} stores it. *)

val find_state_event :
  room_info ->
  event_type:Matrix_proto.Event.Event_type.t ->
  ?state_key:string ->
  unit ->
  state_event option
(** [find_state_event room ~event_type ~state_key ()] is the cached current
    state event under that pair. [state_key] defaults to the empty key. *)

val state_events_of_type :
  room_info -> Matrix_proto.Event.Event_type.t -> state_event list
(** [state_events_of_type room event_type] is every cached event of that type,
    in deterministic state-key order. *)

(** {1 The store} *)

(** Whether decrypted event bodies may be retained in this store. The secure
    default is {!Ciphertext_only}; {!Store_plaintext} is an explicit opt-in
    which may expose message contents at rest. *)
type plaintext_policy = Store_plaintext | Ciphertext_only

type profile = (string * Jsont.json) list
(** An MSC4262 global user profile. Field names are deliberately open and field
    values remain raw JSON, so additions do not require a library release. The
    store returns fields in name order. *)

type t
(** The type for stores. *)

type snapshot
(** An opaque in-memory savepoint for one store handle. *)

val memory : ?plaintext_policy:plaintext_policy -> unit -> t
(** [memory ()] is a new empty in-memory store. {!flush} is a no-op on it.
    [plaintext_policy] defaults to {!Ciphertext_only}; {!Store_plaintext} is an
    explicit opt-in to retaining decrypted bodies at rest. *)

val on_disk : dir:Eio.Fs.dir_ty Eio.Path.t -> t
(** [on_disk ~dir] is a store backed by [dir/base_state.json], loading it if it
    exists. It uses the secure {!Ciphertext_only} policy. [dir] is normally
    {!Profile_store.dir}, so the base client's state sits beside the session and
    device files of the same profile.

    A file that does not parse, or whose format version is newer than this SDK
    understands, is logged on the [matrix.store] source and treated as absent,
    so a format change costs a full initial sync rather than silently
    misinterpreting state. The file is written 0600, since room names and the
    latest event of every room are as sensitive as the messages themselves.

    Raises [Eio.Io] if the file is there but cannot be read. A later {!flush}
    fails with {!Error.Policy_denied} if another handle changes the file before
    this store flushes, leaving this store dirty and leaving the newer file
    untouched. *)

val on_disk_with_policy :
  dir:Eio.Fs.dir_ty Eio.Path.t -> plaintext_policy:plaintext_policy -> t
(** [on_disk_with_policy ~dir] is {!on_disk} with an explicit policy. Use
    {!Store_plaintext} only when retaining decrypted message contents at rest is
    acceptable. *)

val plaintext_policy : t -> plaintext_policy
(** [plaintext_policy t] is the policy selected when [t] was opened. It is
    caller configuration and is not persisted in [base_state.json]. *)

val dir : t -> Eio.Fs.dir_ty Eio.Path.t option
(** The directory an {!on_disk} store writes to, and [None] for a {!memory} one.
*)

val snapshot : t -> snapshot
(** [snapshot t] captures every mutable value in [t], including its dirty and
    optimistic-concurrency state. It performs no I/O. *)

val restore : t -> snapshot -> unit
(** [restore t snapshot] rolls [t] back to a savepoint created from that same
    handle. It performs no I/O and raises [Invalid_argument] for a snapshot from
    another store. This is intended for an enclosing failed persistence
    transaction; never restore after a successful {!flush}. *)

(** {2 Sync token} *)

val next_batch : t -> string option
(** The last sync token stored, or [None] before the first response. *)

val set_next_batch : t -> string -> unit
(** [set_next_batch t token] records [token] as where the next [/sync] resumes.
*)

(** {2 Sliding-sync session} *)

val sliding_pos : t -> string option
(** The persisted sliding-sync position, if one exists. *)

val set_sliding_pos : t -> string option -> unit
(** Replace the persisted sliding-sync position. *)

val sliding_to_device_since : t -> string option
(** The persisted sliding-sync to-device token, if one exists. *)

val set_sliding_to_device_since : t -> string option -> unit
(** Replace the persisted sliding-sync to-device token. *)

val sliding_lists : t -> (string * int) list
(** The persisted sliding-sync list counts, normalized by list name. *)

val replace_sliding_session :
  t ->
  pos:string option ->
  to_device_since:string option ->
  lists:(string * int) list ->
  unit
(** Atomically replace all sliding-sync session metadata. Duplicate list names
    take their last supplied count and the result is sorted by list name. *)

(** {2 Rooms} *)

val rooms : t -> room_info list
(** Every room known to the store, in no particular order. *)

val find_room : t -> Matrix_proto.Id.Room_id.t -> room_info option
(** [find_room t room_id] is the summary of [room_id]. *)

val set_room : t -> room_info -> unit
(** [set_room t info] replaces the summary of [info.room_id]. *)

val remove_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [remove_room t room_id] forgets [room_id]. *)

(** {2 Global account data} *)

val find_account_data : t -> string -> Jsont.json option
(** [find_account_data t event_type] is the content of that account-data event.
*)

val set_account_data : t -> string -> Jsont.json -> unit
(** [set_account_data t event_type content] replaces that account-data event. *)

val remove_account_data : t -> string -> unit
(** [remove_account_data t event_type] removes that account-data event. *)

val all_account_data : t -> (string * Jsont.json) list
(** Every account-data event, as type and content. *)

(** {2 Global profiles} *)

val profiles : t -> (Matrix_proto.Id.User_id.t * profile) list
(** [profiles t] is the complete MSC4262 global-profile snapshot, ordered by
    user id. Each profile's fields are ordered by name. *)

val replace_profiles : t -> (Matrix_proto.Id.User_id.t * profile) list -> unit
(** [replace_profiles t profiles] replaces the complete global-profile snapshot.
    Replacing rather than patching here ensures a dropped user is not
    resurrected by a later persistence pass. Duplicate users and duplicate
    fields take their last supplied value; subsequent reads are deterministic.
*)

(** {2 Receipts} *)

val receipts : t -> Matrix_proto.Id.Room_id.t -> Read_state.t option
(** [receipts t room_id] is the read positions stored for [room_id]. *)

val set_receipts : t -> Matrix_proto.Id.Room_id.t -> Read_state.t -> unit
(** [set_receipts t room_id r] replaces the read positions of [room_id]. *)

val remove_receipts : t -> Matrix_proto.Id.Room_id.t -> unit
(** [remove_receipts t room_id] forgets the read positions of [room_id]. *)

val all_receipts : t -> (Matrix_proto.Id.Room_id.t * Read_state.t) list
(** Every room's read positions, in no particular order. *)

(** Slots another module keeps in this file.

    A module that wants to persist beside the base client's own state declares
    one slot with its codec and reaches its value through it. The name must be
    unique across the library, and the codec must read back what an earlier
    version of it wrote. {!Send_queue} keeps its unsent requests this way. *)
module Slot : sig
  type 'a key
  (** The type for slots holding a value of type ['a]. *)

  val v : name:string -> 'a Jsont.t -> 'a key
  (** [v ~name codec] is the slot stored under [name] and read by [codec]. *)

  val find : t -> 'a key -> ('a option, Error.t) result
  (** [find t k] is the value in [k], and [Ok None] when nothing has been
      stored. A value that does not decode is {!Error.Json_error}. *)

  val set : t -> 'a key -> 'a -> (unit, Error.t) result
  (** [set t k v] stores [v] in [k]. A value that does not encode is
      {!Error.Json_error} and nothing is stored. *)

  val remove : t -> 'a key -> unit
  (** [remove t k] empties [k]. *)
end

(** {2 Persistence} *)

val dirty : t -> bool
(** [dirty t] is [true] when there are changes {!flush} has not written. *)

val flush : t -> (unit, Error.t) result
(** [flush t] writes the store out if it is dirty and on disk. It is [Ok ()] for
    an in-memory store, and for one that is not dirty.

    The file is written with a unique 0600 same-directory temporary file, synced
    before atomic rename. Eio has no portable directory-fsync operation, so
    persistence of the renamed directory entry after power loss is not
    guaranteed.

    Raises [Eio.Io] if the file cannot be written. *)

val clear : t -> unit
(** [clear t] forgets everything it holds. The next {!flush} overwrites the file
    with an empty one; it does not delete it. *)
