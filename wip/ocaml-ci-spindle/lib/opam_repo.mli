(** Local clones of opam repositories, refreshed from their remotes. *)

type t

val v :
  _ Eio_unix.Process.mgr ->
  _ Eio.Time.Mono.t ->
  dir:string ->
  name:string ->
  string ->
  t
(** [v process clock ~dir ~name url] is the repository [url], cloned into
    [dir/name] when first refreshed. *)

val name : t -> string
val url : t -> string

val with_snapshot :
  ?max_age:float -> t -> (commit:string -> packages:string -> 'a) -> 'a
(** [with_snapshot t f] is [f ~commit ~packages] for the clone's newest commit
    and its [packages] directory, which do not change until [f] returns. The
    clone is first fetched unless it was fetched less than [max_age] seconds
    ago, 600 by default, and the first call clones. A fetch may take ten
    minutes. A failed fetch keeps the previous commit, or raises when there is
    none. Calls on [t] run one at a time. *)
