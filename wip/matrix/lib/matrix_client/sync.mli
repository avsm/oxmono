(** sync — the [/sync] endpoint and the filters it takes.

    A sync is a long poll. The server holds the request open until something
    happens or [timeout] expires, then answers with everything since the [since]
    token. Feeding each response's [next_batch] back as the next [since] is what
    makes the stream continuous. The loop that does so lives in
    [Matrix_eio.Sync].

    @see <https://spec.matrix.org/v1.11/client-server-api/#syncing> Syncing *)

(** {1 One sync} *)

type params = {
  filter : string option;
      (** A filter id from {!Filter.create}, or a filter inline as JSON. *)
  since : string option;
      (** The previous response's [next_batch]. Absent means an initial sync,
          which returns the full state of every room. *)
  full_state : bool;  (** Return all state even when [since] is given. *)
  set_presence : [ `Online | `Offline | `Unavailable ] option;
      (** Presence to publish for the duration of the call. Absent uses the
          client-owned {!Client.val-sync_presence} default, and [`Offline] keeps
          the call from marking the user online. The effective [`Online] value
          is omitted from the query, as required by the Matrix wire default;
          [`Offline] and [`Unavailable] are serialized explicitly. *)
  timeout : int;
      (** Milliseconds the server may hold the request open. [0] returns at
          once. *)
}
(** What one request asks for. *)

val default_params : params
(** No filter, no [since], no [full_state], no explicit [set_presence], and a
    [timeout] of 30000 milliseconds. {!sync_once} resolves the absent presence
    from {!Client.val-sync_presence}; an effective [`Online] is omitted on the
    wire. *)

val sync_once :
  Client.t ->
  ?params:params ->
  unit ->
  (Matrix_proto.Sync.Response.t, Error.t) result
(** [sync_once t ()] performs one [GET /_matrix/client/v3/sync] and is what the
    server sent. [params] defaults to {!default_params}. Pass the response's
    [next_batch] as the next call's {!params.since}. *)

(** Filters, which cut down what a sync returns.

    A filter is uploaded once with {!Filter.create} and named by the id that
    call yields, which the server keeps, so an id outlives the process that made
    it.

    @see <https://spec.matrix.org/v1.11/client-server-api/#filtering> Filtering
*)
module Filter : sig
  type event = {
    limit : int option;  (** Maximum events to return. *)
    not_senders : string list;  (** Excluded senders, winning over [senders]. *)
    not_types : string list;  (** Excluded event types, winning over [types]. *)
    senders : string list;  (** Empty means every sender. *)
    types : string list;  (** Empty means every type. Globs are allowed. *)
  }
  (** A filter over presence and account data. *)

  type room_event = {
    limit : int option;
    not_senders : string list;
    not_types : string list;
    senders : string list;
    types : string list;
    lazy_load_members : bool;
        (** Return only the [m.room.member] events of the senders appearing in
            the timeline, rather than every member of the room. *)
    include_redundant_members : bool;
        (** Under lazy loading, resend a member event the client has already
            been given. *)
    not_rooms : string list;  (** Excluded rooms, winning over [rooms]. *)
    rooms : string list;  (** Empty means every room. *)
    contains_url : bool option;
        (** Keep only events whose content has, or has not, a [url]. *)
  }
  (** A filter over the state, timeline, ephemeral and account-data sections of
      a room. *)

  type room = {
    not_rooms : string list;
    rooms : string list;
    ephemeral : room_event option;
    include_leave : bool;  (** Include rooms the user has left. *)
    state : room_event option;
    timeline : room_event option;
    account_data : room_event option;
  }
  (** Which rooms a sync covers, and what it returns of each. *)

  type t = {
    event_fields : string list;
        (** Dot-separated paths to keep in each event. Empty keeps all of them.
        *)
    event_format : [ `Client | `Federation ];
        (** [`Client] strips the federation-only members from each event. *)
    presence : event option;
    account_data : event option;
    room : room option;
  }
  (** A complete filter, as uploaded to the homeserver. *)

  val default_event : event
  (** No limit and no restriction. *)

  val default_room_event : room_event
  (** No restriction, with [lazy_load_members] on. *)

  val default_room : room
  (** No restriction, and rooms the user has left are excluded. *)

  val default : t
  (** Every field, in the client event format, with no section filtered. *)

  val jsont : t Jsont.t
  (** Reads and writes a filter as the homeserver stores it. Every member is
      optional on the wire and decodes to the {!default} value. *)

  val room_event_jsont : room_event Jsont.t
  (** Reads and writes a {!room_event}, the [RoomEventFilter] shape that also
      appears outside a complete filter. *)

  val create : Client.t -> filter:t -> (string, Error.t) result
  (** [create t ~filter] is [POST /_matrix/client/v3/user/{userId}/filter] and
      is the filter id to pass as {!params.filter}.

      Fails with {!Error.No_session} when the client carries none. *)

  val get : Client.t -> filter_id:string -> (t, Error.t) result
  (** [get t ~filter_id] is
      [GET /_matrix/client/v3/user/{userId}/filter/{filterId}], the filter as
      the server stored it.

      Fails with {!Error.No_session} when the client carries none. *)
end
