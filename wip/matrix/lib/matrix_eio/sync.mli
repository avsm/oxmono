(** sync — the [/sync] endpoint, as a fiber.

    {!val-sync} is one long poll. {!sync_forever} and the stream variants below
    fork a fiber on the switch they are given and loop, feeding each response's
    [next_batch] back as the next [since]; releasing the switch ends the loop.

    {!type-action} and {!type-callbacks} are the loop vocabulary of this
    library. {!Sliding_sync} drives its own loop with the same pair, and
    {!Sync_service} the loop that also maintains room state. *)

(** {1 One sync} *)

type params = Matrix_client.Sync.params = {
  filter : string option;
      (** A filter id from {!Filter.create}, or a filter inline as JSON. *)
  since : string option;
      (** The previous response's [next_batch]. Absent means an initial sync,
          which returns the full state of every room. *)
  full_state : bool;  (** Return all state even when [since] is given. *)
  set_presence : [ `Online | `Offline | `Unavailable ] option;
      (** Presence to publish for the duration of the call. Absent leaves it as
          it is, and [`Offline] keeps the call from marking the user online. *)
  timeout : int;
      (** Milliseconds the server may hold the request open. [0] returns at
          once. *)
}
(** What one request asks for. *)

val default_params : params
(** [default_params] has no filter, no [since], no [full_state], no
    [set_presence], and a [timeout] of 30000 milliseconds. *)

type response = Matrix_proto.Sync.Response.t
(** What one response carries. *)

val sync : Client.t -> ?params:params -> unit -> response
(** [sync client ()] performs one [GET /_matrix/client/v3/sync] and is what the
    server sent. The call blocks for up to [params.timeout]. [params] defaults
    to {!default_params}.

    Raises [Eio.Io] with [Error.E e] on a transport or protocol failure. *)

(** {1 The loop vocabulary} *)

(** What a callback asks a loop to do next. *)
type action =
  | Continue  (** Send the next request immediately. *)
  | Stop  (** Leave the loop. *)
  | Retry_after of float  (** Sleep this many seconds, then continue. *)

type 'a callbacks = {
  on_response : 'a -> action;
  on_error : Error.err -> action;
}
(** What a loop calls for each response and each failure. *)

val default_on_error : Error.err -> action
(** [default_on_error e] is [Retry_after 5.0], whatever [e] is. *)

val callbacks :
  ?on_error:(Error.err -> action) ->
  on_response:('a -> action) ->
  unit ->
  'a callbacks
(** [callbacks ~on_response ()] is the pair a loop takes. [on_error] defaults to
    {!default_on_error}. *)

(** {1 Looping} *)

val sync_forever :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  ?initial_since:string ->
  ?params:params ->
  callbacks:response callbacks ->
  unit ->
  unit
(** [sync_forever ~sw ~clock client ~callbacks ()] forks a fiber on [sw] that
    syncs until a callback returns {!Stop}, or [sw] is released. It returns as
    soon as the fiber is forked.

    [initial_since] resumes from a stored token; without it the first request is
    an initial sync. [params] defaults to {!default_params}, and its [since] is
    replaced on every iteration. The token advances only after [on_response]
    returns, so a handler that raises leaves it where it was and the next
    request asks for the same batch again. Nothing is retried on its own, so
    backoff is [on_error]'s business, through {!Retry_after}.

    An exception out of a callback leaves the loop and fails [sw]. *)

val sync_to_stream :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  stream:response Eio.Stream.t ->
  ?initial_since:string ->
  ?params:params ->
  ?on_error:(Error.err -> action) ->
  unit ->
  unit
(** [sync_to_stream ~sw ~clock client ~stream ()] is {!sync_forever} with a
    handler that adds each response to [stream], so that syncing and processing
    run in separate fibers. The loop blocks while the stream is full, so a
    consumer that stops reading stops the syncing. *)

val create_sync_stream :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  ?capacity:int ->
  ?initial_since:string ->
  ?params:params ->
  unit ->
  response Eio.Stream.t
(** [create_sync_stream ~sw ~clock client ()] is a stream of responses, with
    {!sync_to_stream} already running behind it. [capacity] is the stream's
    buffer and defaults to 10. *)

val iter :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  ?initial_since:string ->
  ?params:params ->
  (response -> unit) ->
  unit
(** [iter ~sw ~clock client f] is {!sync_forever} calling [f] on each response
    and always continuing, so only releasing [sw] stops it. *)

(** {1 Filters}

    A filter is uploaded once with {!create_filter} and named by the id that
    call yields, which the server keeps, so an id outlives the process that made
    it. *)

module Filter = Matrix_client.Sync.Filter
(** The filter shapes and their codecs. See {!Matrix_client.Sync.Filter}. *)

type filter = Filter.t
(** A complete filter, as uploaded to the homeserver. *)

val default_filter : filter
(** [default_filter] keeps every field, in the client event format, with no
    section filtered. *)

val default_room_filter : Filter.room
(** [default_room_filter] has no restriction, and excludes rooms the user has
    left. *)

val default_event_filter : Filter.event
(** [default_event_filter] has no limit and no restriction. *)

val default_room_event_filter : Filter.room_event
(** [default_room_event_filter] has no restriction, with [lazy_load_members] on.
*)

val create_filter : Client.t -> filter:filter -> string
(** [create_filter client ~filter] uploads [filter] and is the id to pass as
    [params.filter].

    Raises [Eio.Io] with [Error.E Error.Not_logged_in] when [client] has no
    session, and with [Error.E e] if the upload fails. *)

val get_filter : Client.t -> filter_id:string -> filter
(** [get_filter client ~filter_id] is the filter as the server stored it.

    Raises [Eio.Io] with [Error.E Error.Not_logged_in] when [client] has no
    session, and with [Error.E e] if the filter is unknown. *)
