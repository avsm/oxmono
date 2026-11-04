(** Following repositories' pipelines as they run.

    A watcher polls each repository's spindle with
    [org.tangled.ci.queryPipelines], as Tangled's web pages do, and reports
    pipelines as they start, as their workflows change and as they finish.
    Spindles publish no stream of pipeline changes, so a change shows up at the
    next poll. *)

type workflow = {
  name : string;
  status : string;
      (** [blocked], [pending], [running], [success], [failed], [timeout] or
          [cancelled]. *)
  error : string option;
  started_at : string option;
  finished_at : string option;
}

type pipeline = {
  repo : Tangled_api.repository;
  spindle : string;  (** The origin of the spindle that runs it. *)
  id : string;
  commit : string;
  trigger : Jsont.json;  (** The trigger event, as the spindle gives it. *)
  created_at : string option;
  workflows : workflow list;  (** In the order the spindle lists them. *)
}

type change = {
  workflow : workflow;
  previous : string option;  (** Its status before, if it was seen before. *)
}

type event =
  | Started of pipeline  (** A pipeline was seen for the first time. *)
  | Changed of pipeline * change list
      (** Workflows were added or changed status. *)
  | Finished of pipeline  (** Every workflow has ended. *)

val finished : pipeline -> bool
(** [finished p] holds when [p] has workflows and each has ended. *)

val status : pipeline -> string
(** [status p] is [p]'s status as Tangled shows it: [running] if any workflow
    runs, then [pending], [blocked], [failed], [timeout], [cancelled], and
    otherwise [success]. A pipeline without workflows is [pending]. *)

val trigger_kind : pipeline -> string
(** [trigger_kind p] is [push], [pull_request], [manual], [schedule] or the
    trigger's [$type]. *)

val trigger_ref : pipeline -> string option
(** [trigger_ref p] is the ref of a push or manual run, or the source branch of
    a pull request. *)

val pull : pipeline -> string option
(** [pull p] is the AT-URI of the pull request that started [p], if any. *)

val web_url : ?appview:string -> ?handle:string -> pipeline -> string
(** [web_url ~appview ~handle p] is [p]'s page on [appview],
    [https://tangled.org] by default. The repository's owner is named by
    [handle], or else by DID, which Tangled's pages also accept. *)

type t

val v :
  ?interval:float -> ?recent:int -> clock:_ Eio.Time.clock -> Tangled_api.t -> t
(** [v ~interval ~recent ~clock api] is a watcher that polls every [interval]
    seconds, 15 by default, looking at each repository's [recent] newest
    pipelines, 10 by default. *)

val run :
  t ->
  repos:(unit -> Tangled_api.repository list) ->
  ?resume:(string * string) list ->
  ?on_error:(Tangled_api.repository -> exn -> unit) ->
  (event -> unit) ->
  'a
(** [run t ~repos ~resume ~on_error f] calls [f] with each event, forever.
    [repos] is asked for the repositories to watch before every poll.

    Pipelines that have already finished when they are first seen are not
    reported, apart from those in [resume], given as
    [(repository DID, pipeline ID)], which a caller was following before a
    restart. Each of those is fetched once and reported as [Started], then as
    [Finished] if it ended meanwhile. A repository whose poll fails is passed to
    [on_error], which ignores it by default, and is polled again next time.
    Cancellation stops [run]. *)

val poll :
  t ->
  repos:Tangled_api.repository list ->
  ?on_error:(Tangled_api.repository -> exn -> unit) ->
  (event -> unit) ->
  unit
(** [poll t ~repos ~on_error f] polls once, as each round of {!run} does. *)
