open Bonsai_term

val app :
  poll:(unit -> (string, string) Result.t Effect.t) ->
  interval:float ->
  exit:(unit -> unit Effect.t) ->
  dimensions:Dimensions.t Bonsai.t ->
  local_ Bonsai.graph ->
  view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
(** [app ~poll ~interval ...] polls snapshot JSON without overlapping requests.
    Space pauses, Tab changes views, / edits the filter and q exits. Connection
    gauges expire after five seconds without a heartbeat. *)
