(** Sending repositories to the spindle that tests their kind of project.

    A spindle built with this module does not assume what language its
    repositories are in. Every event starts a [detect] workflow, which has
    {{:https://github.com/git-pkgs/brief} brief} report on the commit and starts
    the workflows of each {!route} that accepts the report, such as an OCaml CI
    for a repository with opam files. The route's executor then runs those
    workflows, and every workflow they spawn, as if it were the spindle's own.
    brief runs wherever the {!scan} given runs it, such as a container on a
    build cluster, and never on the spindle's host. *)

type route = {
  name : string;  (** The route's name, as the [detect] log gives it. *)
  accepts : Brief.t -> bool;
      (** [accepts report] is whether the route tests the project [report]
          describes. *)
  jobs : Spindle.Job.t list;
      (** The workflows started for an accepted repository, with their steps and
          environment. Their [accepts] is not consulted, since the [detect]
          workflow has already selected the event. *)
  executor : Spindle.Executor.t;
      (** Runs the route's workflows and those they spawn. *)
}

type scan = Spindle.Executor.context -> Spindle.Executor.request -> string
(** [scan ctx r] is the JSON that brief reports for the commit of [r], run where
    the scan sees fit. It is called within the [detect] step, and may log to
    [ctx] and wait as queued. It raises when it gets no report. *)

val jobs : Spindle.Job.t list
(** [jobs] is the [detect] workflow, run for every event {!Spindle.Job.routine}
    selects. *)

val route_var : string
(** [route_var] is [SPINDLE_ROUTE], the environment variable that names the
    route of every workflow a route starts, so that workflows spawned across a
    restart still reach it. *)

val executor : scan:scan -> route list -> Spindle.Executor.t
(** [executor ~scan routes] runs the [detect] workflow and hands every other
    workflow to the route named by {!route_var} in its environment. [detect]
    gets brief's report from [scan], logs its summary, with the whole report in
    a folded [report] step, and starts the jobs of each route that accepts it.
    It fails when [scan] does, and succeeds when no route accepts the project,
    which it says. A repository's own workflows are refused. *)
