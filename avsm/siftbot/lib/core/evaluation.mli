(** Chronological fixture evaluation with explicit denominators. *)

type split = {
  training : Siftbot_model.message list;
  testing : Siftbot_model.message list;
  excluded_duplicates : int;
}

val split : cutoff:string -> Siftbot_model.message list -> split
(** [split ~cutoff messages] removes duplicate deliveries before splitting and
    excludes later replies from training. *)

type report = {
  reviewed : int;
  predicted : int;
  correct : int;
  abstained : int;
  false_spam : int;
  missed_relevant : int;
}

val score :
  threshold:float ->
  truth:Siftbot_model.assignment list ->
  Siftbot_model.assignment list ->
  report
(** [score ~threshold ~truth predictions] compares only explicit admin labels.
    Counts include abstentions and never convert predictions into truth. *)

val format : report -> string

val freeze_baseline : Store.t -> cutoff:string -> string * string list
(** [freeze_baseline db ~cutoff] freezes area definitions, confirmed examples,
    input source revisions, baseline version and per-dimension threshold
    reports. Only messages before [cutoff] form the training corpus. Returns the
    run ID and reports. This evaluates the lexical/header baseline. *)
