val run :
  process_mgr:_ Eio_unix.Process.mgr ->
  ?env:Eio.Process.Env.t ->
  ?token:string ->
  ?retry_failed:bool ->
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  string list ->
  unit
(** [run ~process_mgr args] invokes [hf args], falling back to [uvx hf args]
    when [hf] is absent. [retry_failed], false by default, also retries an
    explicit nonzero exit. Signals and cancellation never trigger retries.
    [token] overrides HF_TOKEN in the child environment only. [env] defaults to
    the process manager's environment. Tokens are never placed in arguments. *)
