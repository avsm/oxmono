(** Message authorization precedes context access, model calls and plugins. *)

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
    [allow], [deny] and [reset] are reserved. [now] is monotonic seconds. The
    model callback owns its transport and deadline. *)

val handle :
  t -> ?mentioned:bool -> ?direct:bool -> send:(string -> unit) -> event -> unit
(** [handle t ~send event] handles commands and exact account mentions from an
    allowed sender in an enabled room. [mentioned] records an authenticated
    [m.mentions] entry for this bot. [direct] permits prefix-free messages in a
    confirmed two-person DM without enabling it as a group room. Both default to
    false. It serializes requests, suppresses duplicates and applies a
    ten-second interval to model and plugin calls per sender and room. Context
    advances only after [send] confirms delivery. Errors and cancellation
    propagate. Retries of claimed events are intentionally skipped. The adapter
    must supply authentic Matrix sender IDs, ignore notices and edits, strip
    reply fallbacks, check DM membership, and provide a bounded delivery
    operation. *)

val with_feeds : t -> Feeds.t -> t
(** [with_feeds t feeds] adds feed tools and scheduled polling before serving
    requests. The tools remain restricted to the admin and allowed friends. *)

val with_locations : t -> Locations.t -> t
(** [with_locations t locations] adds initialized OwnTracks capabilities for the
    admin and allowed friends, including scheduled model actions. *)

val person_line : Store.person -> string

val fire : t -> send:(string -> unit) -> Store.reminder -> run_id:int -> string
(** [fire t ~send job ~run_id] runs a claimed reminder through the model with
    its linked memory and original source. Current authority and cancellation
    are rechecked before tools and delivery. The adapter checks room access and
    supplies a bounded send operation. *)
