(** Resumable interval import, with an explicit filtered snapshot. *)

type remote = {
  service : string;
  account : string;
  username : string;
  batch_size : int;
  state : unit -> string;
  query :
    since:string -> until:string -> position:int -> Jmap_eio.Mail.query_page;
  read : string list -> Jmap.Proto.Email.t Jmap.Proto.Method.get_response;
  thread : string -> position:int -> Jmap_eio.Mail.thread_page;
  changes : string -> Jmap.Proto.Method.changes_response;
}

val remote : service:string -> body_limit:int -> Jmap_eio.Mail.reader -> remote

val start : Store.t -> remote -> since:string -> until:string -> unit
(** [start db remote ~since ~until] persists a fixed interval and state anchor.
    An existing job retains its original interval. *)

val step : Store.t -> remote -> bool
(** [step db remote] commits one bounded import, hydration or catch-up unit.
    [true] means the initial import is complete. Repeated calls after completion
    follow changes. Expired states initiate reconciliation without discarding
    published records. *)

val interval : Store.t -> remote -> (string * string) option

val message :
  service:string ->
  account:string ->
  since:string ->
  until:string ->
  Jmap.Proto.Email.t ->
  Siftbot_model.message
(** [message ~service ~account ~since ~until email] preserves source headers,
    original body values and truncation while versioning classifier inputs. *)

val evaluation_cutoff : Store.t -> string
(** [evaluation_cutoff db] is three calendar months before the latest persisted
    import endpoint. Resume and monitoring do not move the boundary. *)

type phase = Listing | Hydrating | Catching_up | Reconciling | Idle

type progress = {
  phase : phase;
  listed : int;
  total : int option;
  pending_threads : int;
}

type outcome = Complete | Paused

val progress : Store.t -> remote -> progress
(** [progress db remote] describes the committed import checkpoint and pending
    thread context. [total] is the server's latest listing estimate, if known. *)

val sync :
  ?max_steps:int ->
  ?on_progress:(progress -> unit) ->
  check:(unit -> unit) ->
  Store.t ->
  remote ->
  outcome
(** [sync ~check db remote] runs a started import through listing, thread
    context and catch-up. It resumes committed work and has no default step
    limit. [check] permits cancellation between steps. [max_steps] explicitly
    bounds one invocation. [on_progress] runs initially and after each commit. *)
