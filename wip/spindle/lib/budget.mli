(** Token-bucket budgets for work that anonymous requests can cause. *)

type t
(** A budget shared by every client, and a smaller one for each client. *)

val v :
  now:(unit -> float) ->
  rate:float ->
  burst:float ->
  client_rate:float ->
  client_burst:float ->
  unit ->
  t
(** [v ~now ~rate ~burst ~client_rate ~client_burst ()] allows [rate] spends
    a second in all, with bursts of [burst], and [client_rate] a second for
    each client, with bursts of [client_burst]. [now] is a monotonic clock in
    seconds. At most 4096 clients are tracked at once. *)

val spend : t -> client:string option -> bool
(** [spend t ~client] takes one token from [client]'s budget, if [client] is
    named, and from the shared budget. It is [false], taking nothing, when
    either is empty. *)
