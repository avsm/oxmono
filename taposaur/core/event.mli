(** Versioned event envelopes. Outcomes retain their original JSON shape. *)

type log = {
  step : string;
  status : int option;
  elapsed : float option;
  detail : string option;
}
type error = { message : string; source : string option }

type payload =
  | Log of log
  | Error of error
  | Outcome of Jsont.json
  | Summary of Jsont.json

type t = {
  v : int;
  seq : int;
  at : string;
  cmd : string;
  subject : string option;
  payload : payload;
}

val jsont : t Jsont.t @@ portable
val of_string : string -> (t, string) result
val to_string : t -> (string, string) result
(** [of_string s] requires exactly one object payload. Sequence numbers
    are positive and restart per run. Consumers must retain file order. *)
val kind : t -> string
(** [kind t] is [log], [error], [outcome] or [summary]. *)
