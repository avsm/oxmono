(** Authorized requests and optional silent observation of enabled rooms. *)

type event = { room : string; sender : string; id : string; body : string }

type complete =
  Openrouter.Message.t list ->
  Openrouter.Tool.t list ->
  string option * Openrouter.Tool.call list

type t

val create :
  config:Config.t ->
  store:Store.t ->
  self:string ->
  plugins:Plugin.t list ->
  complete:complete ->
  now:(unit -> float) ->
  t
(** [create ~config ~store ~self ~plugins ~complete ~now] builds an assistant.
    Only plugins named by the profile are enabled. Names use lowercase ASCII
    letters, digits and underscores, up to 64 characters. Command names such as
    [allow], [deny] and [reset] are reserved. [now] is retained for source
    compatibility and is unused. The model callback owns its transport and
    deadline. A turn permits at most six tool calls. Empty answers and failed
    terminal requests get one tool-free synthesis retry, then a visible fallback
    without replaying tools. Turn directives extend the opening system message
    while preserving the tool transcript. Older conversation context is
    compacted through additional tool-free completion calls near the profile
    limits. Summaries persist with source metadata and are scoped to their
    original room and sender. Failed compaction falls back to bounded history.
*)

val handle :
  t ->
  ?mentioned:bool ->
  ?direct:bool ->
  ?on_accept:(unit -> unit) ->
  send:(string -> unit) ->
  event ->
  unit
(** [handle t ~send event] handles commands and exact account mentions from an
    allowed sender in an enabled room. [mentioned] records an authenticated
    [m.mentions] entry for this bot. [direct] permits prefix-free messages in a
    confirmed two-person DM without enabling it as a group room. Both default to
    false. It serializes requests and suppresses duplicates without a cooldown.
    [on_accept] runs after authorization and claiming a fresh addressed event,
    before command or answer work. Implicit addressing is judged before
    acceptance. It defaults to doing nothing. Context advances only after [send]
    confirms delivery. Errors and cancellation propagate. Retries of claimed
    events are intentionally skipped. The adapter must supply authentic Matrix
    sender IDs, ignore notices, use an edit's new content and its own event ID,
    strip reply fallbacks, check DM membership, and provide a bounded delivery
    operation. *)

val with_feeds : t -> Feeds.t -> t
(** [with_feeds t feeds] adds feed tools and scheduled polling before serving
    requests. The tools remain restricted to the admin and allowed friends. *)

val with_room_observation : t -> t
(** [with_room_observation t] enables a separate tool-free model call for every
    nonempty group message in an enabled room, including unapproved senders.
    Bounded observations persist in SQLite and inform later authorized replies
    in that room. The same call judges informal addressing and follow-ups using
    room context and the sender's recent exchanges with Crow. A positive
    decision enters the authorized model path, never the local command parser.
    Malformed or failed observations default to silence for implicit addressing.
    Explicit commands, mentions and DMs do not depend on that judgment.
    Observation grants no authority or tool access. *)

val with_locations : t -> Locations.t -> t
(** [with_locations t locations] adds initialized OwnTracks capabilities for the
    admin and allowed friends, including scheduled model actions. *)

val with_matrix : t -> Matrix_rooms.t -> t
(** [with_matrix t matrix] adds read-only Matrix room inspection for the admin
    and allowed friends, including scheduled model actions. *)

val person_line : Store.person -> string

val fire : t -> send:(string -> unit) -> Store.reminder -> run_id:int -> string
(** [fire t ~send job ~run_id] runs a claimed reminder through the model with
    its linked memory and original source. Current authority and cancellation
    are rechecked before tools and delivery. The adapter checks room access and
    supplies a bounded send operation. *)

val with_calendars : t -> Calendars.t -> t
val with_caldav : t -> Caldav_tools.t -> t
val with_emails : t -> Emails.t -> t
