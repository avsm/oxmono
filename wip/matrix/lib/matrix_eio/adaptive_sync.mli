(** Adaptive sync — opt-in native sliding-sync discovery with classic fallback.

    The adaptive loop owns no state of its own. The caller's {!Sync_service.t}
    is passed to either transport, so the classic and sliding projections share
    one persisted {!type:Matrix_client.Base_client.state}. A homeserver which
    advertises MSC4186 but rejects the endpoint is switched to classic sync
    exactly once. *)

type mode =
  | Discovering
  | Sliding
  | Classic  (** The transport currently selected by {!run}. *)

type response =
  | Sliding of Matrix_proto.Sliding_sync.Response.t
  | Classic of Matrix_proto.Sync.Response.t
      (** The raw response delivered to {!run}'s callback. *)

val run :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  service:Sync_service.t ->
  ?request:Sliding_sync.Request.t ->
  ?controller:Sliding_sync.Controller.t ->
  ?initial_pos:string ->
  ?timeout_ms:int ->
  ?txn_id:bool ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  ?thread_subscription_store:Matrix_client.Store.t ->
  ?classic_params:Matrix_client.Sync.params ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  ?on_mode:(mode -> unit) ->
  ?on_response:(response -> Sync.action) ->
  ?on_error:(Error.err -> Sync.action) ->
  on_change:(Sync_service.state -> Sync_service.changes -> unit) ->
  unit ->
  unit
(** [run ~sw ~clock client ~service ~on_change ()] forks an adaptive loop.

    It first publishes {!Discovering} and probes the homeserver's versions. An
    advertised native endpoint publishes {!Sliding}; a server without the
    feature publishes {!Classic} and starts {!Sync_service.run}. If a native
    poll later returns the normalized unsupported 404 or [M_UNRECOGNIZED], the
    native loop is stopped and classic sync is started once, without invoking
    [on_error]. Other errors are passed to [on_error]. Returning {!Sync.Stop}
    from [on_response] or [on_error] stops the adaptive loop and never triggers
    fallback.

    [controller], when supplied, is the mutable sliding request driver. If it is
    omitted, a controller is made from [request], which defaults to
    {!Sliding_sync.Request.v}. [request] is ignored when [controller] is
    supplied. [classic_params] configures classic [/sync]; [set_presence], when
    supplied, overrides its presence field as well as the sliding poll. Sliding
    timing, transaction ids and thread-subscription persistence apply to the
    native phase. Both phases use the same [service], crypto options,
    [on_change] callback and switch cancellation. If [on_error] is omitted,
    retry delays follow the classic service policy, starting at 500 ms and
    doubling to a 60 s ceiling; a successful response resets that delay. *)
