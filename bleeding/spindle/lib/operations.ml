(* SPDX-License-Identifier: ISC *)
type t = {
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

let v ?(history_days = 30) ?(history_limit = 1000) ?(history_megabytes = 1024)
    ?(receipt_days = 7) ?(receipt_limit = 100000) ?(inbox_limit = 10000)
    ?(inbox_megabytes = 64) ?(replay_hours = 24) ?(reconcile_seconds = 300)
    ?(maintenance_seconds = 60) () =
  List.iter
    (fun (name, value) ->
      if value < 1 || value > 10000000 then
        invalid_arg (name ^ " must be 1..10000000"))
    [
      ("history_days", history_days);
      ("history_limit", history_limit);
      ("history_megabytes", history_megabytes);
      ("receipt_days", receipt_days);
      ("receipt_limit", receipt_limit);
      ("inbox_limit", inbox_limit);
      ("inbox_megabytes", inbox_megabytes);
      ("replay_hours", replay_hours);
      ("reconcile_seconds", reconcile_seconds);
      ("maintenance_seconds", maintenance_seconds);
    ];
  if receipt_days * 24 < replay_hours then
    invalid_arg "receipt retention must cover the replay window";
  if reconcile_seconds >= receipt_days * 86400 then
    invalid_arg "reconciliation must run before ref checkpoints expire";
  {
    history_days;
    history_limit;
    history_megabytes;
    receipt_days;
    receipt_limit;
    inbox_limit;
    inbox_megabytes;
    replay_hours;
    reconcile_seconds;
    maintenance_seconds;
  }

let default = v ()
