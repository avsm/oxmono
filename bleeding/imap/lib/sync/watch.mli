(** Continuous mailbox observation with durable reconciliation.

    IDLE is only a wakeup hint. A scan runs when the selected mailbox state
    differs from the published cursor, and at each renewal or poll. Every
    scan is complete and SQLite-staged before its publication is reported.
    Use one supervisor per mailbox with a bounded connection pool above it. *)

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
(** [run ~clock ~connect ~store ~scope ~mailbox ~next_stage_id ~on_publish ()]
    scans [mailbox] immediately, reports each publication to [on_publish], and
    then waits for a change before scanning again. It returns only an error,
    and the caller stops it by cancelling its fiber.

    When the scanning connection offers IDLE, a second connection selects the
    mailbox and compares UIDVALIDITY, UIDNEXT and HIGHESTMODSEQ with the
    published cursor. A difference starts a scan. Otherwise it enters IDLE,
    and after any untagged response it selects the mailbox and compares
    again, so a keepalive alone starts no scan. After [idle_renew_seconds]
    (default 1500) it scans regardless. An absent MODSEQ can delay a
    flag-only change until that renewal. Without IDLE it scans every
    [poll_seconds] (default 60).

    A failed connection, scan or IDLE is reported to [on_retry] and retried
    after an exponential delay from [retry_seconds] (default 5) capped at
    [max_retry_seconds] (default 300). A successful publication and wait
    reset the delay. Connecting has a deadline of [connect_timeout_seconds]
    (default 30) and a complete scan one of [scan_timeout_seconds] (default
    3600). A timed-out scan discards its provisional SQLite rows. Invalid
    scope, limit and mirror consistency errors return [Fatal_scan].
    Exceptions raised by [on_publish] and [on_retry] propagate. Stage IDs from
    [next_stage_id] must be globally unique.

    Returns [Invalid_configuration] unless every interval is finite and
    positive, [idle_renew_seconds] is at most 1740 as RFC 2177 requires, and
    [max_retry_seconds] is at least [retry_seconds]. *)
