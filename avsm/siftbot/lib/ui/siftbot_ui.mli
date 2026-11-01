open Bonsai_term
(** Bonsai review workspace. Backend effects cross the process protocol. *)

module Settings = Settings

val app :
  ?profile_name:string ->
  ?configure:(unit -> unit Effect.t) ->
  execute:(Siftbot_model.request -> Siftbot_model.response Effect.t) ->
  exit:(unit -> unit Effect.t) ->
  dimensions:Dimensions.t Bonsai.t ->
  local_ Bonsai.graph ->
  view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
