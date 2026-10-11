open Bonsai_term

val app :
  default_name:string ->
  execute:(Siftbot_setup.request -> Siftbot_setup.response Effect.t) ->
  select:(string -> unit Effect.t) ->
  exit:(unit -> unit Effect.t) ->
  dimensions:Dimensions.t Bonsai.t ->
  local_ Bonsai.graph ->
  view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
(** [app ~default_name ~execute ~select ~exit ~dimensions graph] renders the
    profile picker and masked credential form. [select] opens a saved profile.
    [execute] must use the dedicated setup channel, never the review worker. *)
