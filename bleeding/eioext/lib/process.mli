(** Bounded subprocess operations for native Unix Eio backends.

    Each call owns a process group. Remaining descendants are terminated on
    leader exit, stream-consumer failure or cancellation. Descendants that leave
    the group are outside this guarantee. These operations are intended for
    finite commands, not daemon launchers. *)

type result = {
  status : Eio.Process.exit_status;
  stdout : string;
  stderr : string;
}

exception Output_limit of int
(** [Output_limit limit] reports a captured stream exceeding [limit] bytes. *)

val executable :
  process_mgr:_ Eio_unix.Process.mgr ->
  ?env:Eio.Process.Env.t ->
  string ->
  string
(** [executable ~process_mgr name] resolves [name] using the child environment's
    PATH without running it. An explicit path is returned unchanged. Errors use
    [Eio.Process.Executable_not_found] and [Eio.Process.Permission_denied]. *)

val capture :
  process_mgr:_ Eio_unix.Process.mgr ->
  ?env:Eio.Process.Env.t ->
  ?input:string ->
  ?max_stdout:int ->
  ?max_stderr:int ->
  string list ->
  result
(** [capture ~process_mgr args] runs [args] with closed stdin and captures at
    most 16 MiB of stdout and 64 KiB of stderr by default. [input] supplies
    stdin. Both streams are drained concurrently. Invalid bounds raise
    [Invalid_argument] before starting a child. Bounds must be nonnegative and
    smaller than [max_int]. *)

val with_stdout :
  process_mgr:_ Eio_unix.Process.mgr ->
  ?env:Eio.Process.Env.t ->
  ?input:string ->
  ?max_stderr:int ->
  string list ->
  (Eio.Flow.source_ty Eio.Resource.t -> 'a) ->
  Eio.Process.exit_status * 'a * string
(** [with_stdout ~process_mgr args read] passes stdout to [read] and captures
    bounded stderr. [read] must consume stdout to EOF or raise. Stdin is closed
    unless [input] is supplied. The source is valid only during [read]. *)

val run :
  process_mgr:_ Eio_unix.Process.mgr ->
  ?env:Eio.Process.Env.t ->
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  string list ->
  unit
(** [run ~process_mgr args] runs [args], inheriting standard streams unless an
    output sink is supplied. A nonzero status raises [Eio.Process.Child_error].
    Cleanup preserves the original exception. *)
