(** Continuous mailbox observation with durable reconciliation.

    IDLE is only a wakeup hint. Each wakeup (or bounded renewal/poll) runs a
    complete SQLite-staged scan before reporting a publication. Use one
    supervisor per mailbox with a bounded connection pool above it. *)

type retry_error =
  | Connect_failed of Imap_eio.Error.t
  | Connect_timed_out
  | Scan_failed of Engine.error
  | Scan_timed_out
  | Idle_failed of Imap_eio.Error.t

type error =
  | Invalid_configuration of string
  | Fatal_scan of Engine.error

val run :
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  connect:(sw:Eio.Switch.t -> (Imap_eio.Client.t, Imap_eio.Error.t) result) ->
  store:Imap_store.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  next_stage_id:(unit -> string) ->
  on_publish:(Imap_store.staged_receipt -> unit) ->
  ?on_retry:(retry_error -> unit) ->
  ?poll_seconds:float -> ?retry_seconds:float -> ?max_retry_seconds:float ->
  ?connect_timeout_seconds:float -> ?scan_timeout_seconds:float ->
  ?idle_renew_seconds:float ->
  unit -> (unit, error) result
(** Scans immediately, then uses IDLE when offered or polls otherwise.
    Before entering IDLE, reselects and compares UIDVALIDITY, UIDNEXT and
    HIGHESTMODSEQ with the published cursor to close the scan-to-watch race.
    An absent MODSEQ may delay a flag-only change until the next renewal/poll.
    Repeated connection/scan failures use exponential retry from
    [retry_seconds] (default 5s), capped by [max_retry_seconds] (default 300s).
    A successful publication and wait reset the delay. Connection setup has a
    30-second default deadline; a complete scan has a 3600-second default
    deadline. Timeouts reconnect after backoff and a cancelled staged scan
    discards its provisional SQLite rows. Invalid scope, limits
    and mirror consistency errors return [Fatal_scan]. The caller stops the
    loop by cancelling its fiber. Stage IDs must be globally unique. *)
