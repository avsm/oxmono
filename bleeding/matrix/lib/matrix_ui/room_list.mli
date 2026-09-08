(** room_list — a reactive room-list projection with Unicode-aware filtering.

    A list is built over an {!Event_cache} and a {!Matrix_client.Base_client}
    state, and republished by {!refresh} after every sync. {!all_rooms} is every
    room the state knows, and {!val-rooms} is that list under the {!Filter.t} in
    force, in the order {!type-order} names. *)

(** {1 Rooms} *)

(** The type for the bucket of a sectioned room list a room falls in, computed
    from membership, tags and [is_dm] in that order, so that a favourite direct
    message is filed under {!Favourites} rather than {!People}. An Element-style
    list builds its sections by running one {!Filter.t} per section. *)
type section =
  | Invites
  | Favourites
  | People
  | Rooms
  | Low_priority
  | Historical  (** Left rooms, and knocked ones, which have no section. *)

(** The type for the order a room list is shown in. *)
type order =
  | Activity
      (** Rooms whose preview is an unsent local echo first, then by the latest
          event's timestamp, newest first, then, only once neither room has a
          latest event at all, by the recency stamp, then by name. A room with a
          latest event always sorts above one without, which is what keeps the
          comparison from switching scales mid-sort. *)
  | Name
      (** Display name ascending, then {!Activity} as the tie-break. Bytewise,
          neither collated nor case-folded. *)

type room = {
  id : Matrix_proto.Id.Room_id.t;
  name : string;  (** {!Matrix_client.Base_client.display_name}. *)
  avatar_url : string option;
  topic : string option;
  membership : Matrix_client.Base_client.membership;
  section : section;  (** Derived. *)
  is_dm : bool;
  is_space : bool;
      (** Whether durable [m.room.create] content names this room as an
          [m.space]. Missing or malformed create content means [false]. *)
  latest : string option;
      (** The preview, derived. It is the body of the newest event
          {!Presentation.is_preview_worthy} admits, which is the plaintext where
          the cache holds a decryption. An edited message previews as its edit,
          and {!latest_sender} and {!latest_timestamp} then become the edit's.
          It is [None] for a room with nothing to preview yet. *)
  latest_sender : Matrix_proto.Id.User_id.t option;  (** Derived. *)
  latest_timestamp : Matrix_proto.Event.Timestamp.t option;
      (** The preview event's timestamp. Derived. *)
  latest_is_unsent : bool;
      (** The preview is a local echo the server has not acknowledged. Such
          rooms sort first under {!Activity}. Derived. *)
  last_active : Matrix_proto.Event.Timestamp.t;
      (** The greatest [origin_server_ts] the room has seen, which is
          {!Matrix_client.Store.room_info.last_active_ts}. A room with no
          {!latest_timestamp} has this to show instead. *)
  notification_count : int;
      (** The server's count, or the locally evaluated one for an encrypted
          room, whichever is larger. Derived. *)
  highlight_count : int;  (** Unread mentions, counted as above. Derived. *)
  unread_messages : int;
      (** Unread messages counted locally against the own read receipt. *)
  marked_unread : bool;
      (** The room's [m.marked_unread] account data, which a user set by hand.
          {!Matrix_client.Store.room_info.marked_unread} says when it clears. *)
  encrypted : bool;
  tags : (string * float option) list;
}
(** The type for the rooms a list holds. The fields marked derived are computed
    from the sync state and the event cache together. The rest are copies of the
    {!Matrix_client.Base_client.room_info} the sync state holds. *)

val unread : room -> bool
(** [unread room] is [true] when the room has unread notifications or messages,
    or a user marked it unread by hand. *)

(** {1 Filters} *)

module Filter : sig
  (** The type for which rooms a list shows. A filter is data rather than a
      closure, so that it can be compared, stored and printed. *)

  (** The type for which count an {!Unread} filter reads. *)
  type unread =
    | Mentions  (** {!room.highlight_count} *)
    | Notifications  (** {!room.notification_count} *)
    | Messages  (** {!room.unread_messages} *)

  (** The type for filters. *)
  type t =
    | Everything  (** Matches every room; the identity of {!All}. *)
    | Nothing  (** Matches no room. *)
    | All of t list  (** Every one holds. *)
    | Any of t list  (** At least one holds. *)
    | Not of t
    | Membership of Matrix_client.Base_client.membership
    | Non_left  (** Joined, invited or knocked. *)
    | Dm  (** [Not Dm] is every room that is not a direct message. *)
    | Favourite  (** Tagged [m.favourite]. *)
    | Low_priority  (** Tagged [m.lowpriority]. *)
    | Unread of unread
        (** The count is above zero, or the room is marked unread by hand. *)
    | Name of string  (** {!Matching.contains} against the display name. *)
    | Fuzzy of string
        (** {!Matching.fuzzy_score} against the display name, so the pattern's
            characters must appear in order but need not be adjacent and ["mtx"]
            matches ["matrix"]. {!val-score} exposes the number, so that a
            toolkit can rank on it. *)
    | Search of string  (** {!Name}, widened to match the room id too. *)
    | Room_ids of Matrix_proto.Id.Room_id.t list
    | In_section of section
    | Space  (** Rooms whose [m.room.create] [type] is ["m.space"]. *)
    | Deduplicate_versions
        (** Hide rooms superseded by a known room tombstone. A joined room is
            hidden only when its successor is joined or left (banned rooms are
            represented as {!Matrix_client.Base_client.Left}); an invited or
            knocked successor keeps the joined room visible. Any non-joined room
            is hidden whenever its successor is known. Unknown or malformed
            tombstones never hide a room. This needs the complete room list, so
            standalone {!matches} conservatively returns [true]; filtering a
            {!t} through {!set_filter} applies the contextual rule, including
            when this constructor is nested in {!All}, {!Any} or {!Not}. *)

  val matches : t -> room -> bool
  (** [matches filter room] is whether the room passes the filter. *)

  val score : t -> room -> int option
  (** [score filter room] is [None] exactly when [matches filter room] is
      [false], and otherwise how well the room matches. It is the sum of the
      {!Fuzzy} scores under an {!All}, the best of them under an {!Any}, and [0]
      where no fuzzy clause is involved. A toolkit that wants "best match first"
      sorts on this, and {!compare_rooms} does not. Scores are comparable only
      between rooms judged by the same filter. *)

  val everything : t -> bool
  (** [everything filter] is whether the filter is known to admit every room,
      for a caller that wants to show "no filter" or skip work of its own. It is
      conservative, so [All []] and {!Everything} are [true] while [Not Nothing]
      is [false]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same filter. Two filters
      that admit the same rooms by different routes are not equal. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf filter] prints the filter as a parenthesised expression. *)
end

(** {1 Sorting} *)

val compare_rooms : order -> room -> room -> int
(** [compare_rooms order] is the comparator the list sorts by. It orders by
    section first, then by the composition {!type-order} names, then by room id,
    so that the order is total and a rerun reproduces it. *)

(** {1 The list} *)

type t
(** The type for room lists. *)

val create : Event_cache.t -> Matrix_client.Base_client.state -> t
(** [create cache state] is a list over the cache and the sync state. The own
    user, which decides whether a membership event may preview a room, is
    {!Matrix_client.Base_client.user_id} of the state rather than an argument of
    its own, so that a caller cannot pass one the sync service disagrees with.
    {!Runtime} is the supported way to build one. *)

val all_rooms : t -> room Observable.List.t
(** [all_rooms t] is every room the sync state knows, unfiltered and in the
    order the state gives them. *)

val rooms : t -> room Observable.List.t
(** [rooms t] is {!all_rooms} under the {!val-filter} in force, in the order
    {!val-sort} names. *)

val find : t -> Matrix_proto.Id.Room_id.t -> room option
(** [find t room_id] is the record for one room, looked up in {!all_rooms}
    rather than in {!val-rooms}. A caller that holds a room id, an invite it is
    about to accept or a room a timeline is open on, is asking about that room
    and not about whether the filter in force admits it. [None] means the sync
    state knows no such room. *)

val filter : t -> Filter.t
(** [filter t] is the filter in force, {!Filter.Non_left} until {!set_filter}
    says otherwise. *)

val set_filter : t -> Filter.t -> unit
(** [set_filter t filter] republishes {!val-rooms} under [filter]. Setting the
    filter it already has does nothing. *)

val sort : t -> order
(** [sort t] is the order in force, {!Activity} until {!set_sort} says
    otherwise. *)

val set_sort : t -> order -> unit
(** [set_sort t order] republishes {!val-rooms} in [order]. Setting the order it
    already has does nothing. *)

val refresh : t -> Matrix_client.Base_client.state -> unit
(** [refresh t state] recomputes both lists against [state]. The list is
    pull-driven, because a room's record is a projection of the sync state and
    of the event cache together and only the caller that applied the sync knows
    both have settled. {!Runtime} calls it after every sync and after every
    send-queue change. *)
