(** Configuration shared by direct plan and single-node execution. *)

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
  inherit_path : bool;
      (** Append the host PATH to recipe PATH entries. Default: true. *)
  inject_env : (string * string) list;
      (** Extra environment variables injected into every node's script
          environment, overriding entries with the same key. Defaults to empty.
          Producers can supply [OCAMLFIND_LDCONF=ignore] where needed. *)
}

val default : t
(** [default] uses [build_parallelism = max(1, min(domain_count, 8))],
    [keep_staging = false], [log_dir = None], [inherit_path = true],
    [inject_env = []]. [OI_DOMAINS] overrides the detected domain count. *)

val with_env_overrides : t -> t
(** [with_env_overrides t] applies environment-variable overrides:
    - [OI_BUILD_PARALLELISM] → [build_parallelism].
    - [OI_KEEP_STAGING] (any non-empty value) → [keep_staging = true]. *)

val pp : t Fmt.t
(** [pp] renders the parallelism, [keep_staging], and [log_dir] knobs on one
    line. [inject_env] is omitted to keep the line short. *)
