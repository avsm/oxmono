(** Executor configuration.

    Decoupled from the producer (the d10ir library doesn't care how plans are
    produced). Designed to be the public knob surface consumers like [oi] expose
    to their CLI. *)

type t = {
  build_parallelism : int;
      (** Maximum number of nodes whose script runs concurrently. Each slot may
          spawn a recursive tree of subprocesses, so this is really an upper
          bound on concurrent build phases, not on total subprocess count. *)
  keep_staging : bool;
      (** If [true], leave staging directories in place after a build (success
          or failure) for debugging. Default [false]. *)
  log_dir : string option;
      (** Directory for per-node build logs. If [None], logs go to a subdir of
          the d10 cache root. *)
  inject_env : (string * string) list;
      (** Extra environment variables injected into every node's script
          environment, in addition to whatever the node carries. The d10ir
          library is opam-agnostic — defaults are empty. Consumers like [oi] add
          their own (e.g. [OCAMLFIND_LDCONF=ignore] for opam build hardening).
      *)
}

val default : t
(** Defaults: [build_parallelism = min(domain_count, 8)],
    [keep_staging = false], [log_dir = None], [inject_env = []]. *)

val with_env_overrides : t -> t
(** [with_env_overrides t] applies environment-variable overrides:
    - [OI_BUILD_PARALLELISM] → [build_parallelism].
    - [OI_KEEP_STAGING] (any non-empty value) → [keep_staging = true]. *)

val pp : t Fmt.t
(** [pp] renders the parallelism, [keep_staging], and [log_dir] knobs on one
    line. [inject_env] is omitted to keep the line short. *)
