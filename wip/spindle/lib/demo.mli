(** Executors that run nothing, for trying a spindle out. *)

val solve : ?compilers:string list -> _ Eio.Time.clock -> Executor.t
(** [solve clock] simulates a planning spindle. A workflow it did not spawn is
    described as {!print} logs it, then given a [solve] step that pretends to
    solve the repository's dependencies for each of [compilers], which default
    to [ocaml-4.14], [ocaml-5.5] and [oxcaml-5.2minus39]. It spawns a workflow
    for each compiler, which logs made-up dependency installation, build and
    test output with short random waits and succeeds. Nothing is run. *)

val print : ?delay:float -> _ Eio.Time.clock -> Executor.t
(** [print ~delay clock] logs the request, with each secret's value replaced by
    a placeholder, and logs each step without running it. It also prints the
    request to standard output. It then sleeps [delay] seconds, 2 by default,
    and succeeds. *)
