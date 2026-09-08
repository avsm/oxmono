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

val handle : t -> send:(string -> unit) -> event -> unit
(** [handle t ~send event] handles an explicit [!crow] command from an allowed
    sender in an enabled room. It serializes requests, suppresses duplicates and
    applies a ten-second interval to model and plugin calls per sender and room.
    Context advances only after [send] confirms delivery. Errors and
    cancellation propagate. Retries of claimed events are intentionally skipped.
    The adapter must supply authentic Matrix sender IDs, ignore notices and
    edits, and provide a bounded delivery operation. *)

val person_line : Store.person -> string
