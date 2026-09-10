type t = private {
  history_days : int;
  history_limit : int;
  history_megabytes : int;
  receipt_days : int;
  receipt_limit : int;
  inbox_limit : int;
  inbox_megabytes : int;
  replay_hours : int;
  reconcile_seconds : int;
  maintenance_seconds : int;
}
(** Storage and observer policy for a spindle process. *)

val v :
  ?history_days:int ->
  ?history_limit:int ->
  ?history_megabytes:int ->
  ?receipt_days:int ->
  ?receipt_limit:int ->
  ?inbox_limit:int ->
  ?inbox_megabytes:int ->
  ?replay_hours:int ->
  ?reconcile_seconds:int ->
  ?maintenance_seconds:int ->
  unit ->
  t
(** [v ()] retains completed pipelines for 30 days, at most 1000 pipelines or
    1024 MiB. Receipts last seven days with a 100000 entry cap. The inbox admits
    at most 10000 events or 64 MiB. The replay window is 24 hours, ref checks
    run every 300 seconds and maintenance every 60 seconds. Limits are positive.
    Receipt retention must cover the replay window and exceed the reconciliation
    interval. Active jobs, pending events and unexpired JWT nonces are never
    removed to satisfy these limits. *)

val default : t
