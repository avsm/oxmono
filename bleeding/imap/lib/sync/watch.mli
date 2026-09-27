(** Continuous observation of one mailbox with durable scans.

    IDLE is only a wakeup hint. A scan runs when the selected mailbox state
    differs from the published cursor, and at each renewal or poll. Every
    scan is complete and staged in SQLite before its publication is
    reported. Run one supervisor per mailbox, with a bounded connection
    pool above it. *)

type retry_error =
  | Connect_failed of Error.t
      (** [Connect_failed e] is a failure of the caller's [connect],
          including a context {!Ctx.v} refused. *)
  | Connect_timed_out
      (** [Connect_timed_out] is a [connect] that exceeded its deadline. *)
  | Scan_failed of Error.t
      (** [Scan_failed e] is a failed {!Engine.scan_once}. *)
  | Scan_timed_out
      (** [Scan_timed_out] is a connection and scan that exceeded their
          deadline. *)
  | Idle_failed of Imap_eio.Error.t
      (** [Idle_failed e] is a failed SELECT or IDLE on the waiting
          connection, including a server that does not offer IDLE there. *)
(** The type for the failures {!run} retries. *)

type error =
  | Invalid_configuration of string
      (** [Invalid_configuration why] is an interval {!run} rejects. *)
  | Fatal_scan of Error.t
      (** [Fatal_scan e] is a connect or scan error that a retry cannot
          fix, which is [Invalid_scope], [Limit], [Mirror] or
          [Uidvalidity_changed]. *)
(** The type for the errors that stop {!run}. *)

val run :
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  connect:(sw:Eio.Switch.t -> (Ctx.t, Error.t) result) ->
  next_stage_id:(unit -> string) ->
  on_publish:(Imap_store.staged_receipt -> unit) ->
  ?on_retry:(retry_error -> unit) ->
  ?poll_seconds:float -> ?retry_seconds:float -> ?max_retry_seconds:float ->
  ?connect_timeout_seconds:float -> ?scan_timeout_seconds:float ->
  ?idle_renew_seconds:float ->
  unit -> (unit, error) result
(** [run ~clock ~connect ~next_stage_id ~on_publish ()] scans the mailbox of
    the context [connect] returns at once, reports each publication to
    [on_publish], and then waits for a change before it scans again. Stage
    IDs come from [next_stage_id] and must be globally unique. [connect]
    runs for every scan and every wait, under a switch that ends with that
    scan or wait. [run] returns only an error, and the caller stops it by
    cancelling its fiber. Timing uses [clock].

    When the scanning connection offers IDLE, the wait selects the mailbox
    on a new connection and compares UIDVALIDITY, UIDNEXT and HIGHESTMODSEQ
    with the published cursor. A difference starts a scan. Otherwise the
    wait enters IDLE, and after any untagged response it selects the mailbox
    and compares again, so a keepalive alone starts no scan. After
    [idle_renew_seconds] it scans regardless, and a mailbox without a
    CONDSTORE anchor can delay a flag-only change until then.
    [idle_renew_seconds] defaults to 1500. Without IDLE the wait sleeps for
    [poll_seconds], which defaults to 60.

    A failed or timed-out connection, scan or wait is reported to
    [on_retry], which defaults to ignoring it, and retried after a delay
    that starts at [retry_seconds] and doubles up to [max_retry_seconds].
    They default to 5 and 300. A successful publication and wait reset the
    delay. Each [connect] has a deadline of [connect_timeout_seconds],
    default 30, and each connection and scan together one of
    [scan_timeout_seconds], default 3600. A timed-out scan discards its
    staged rows. A connect or scan failure of a scan that no retry can fix
    returns [Fatal_scan]. Exceptions raised by [on_publish] and [on_retry]
    propagate.

    The result is [Invalid_configuration] unless every interval is finite
    and positive, [idle_renew_seconds] is at most 1740 as RFC 2177 requires,
    and [max_retry_seconds] is at least [retry_seconds]. *)
