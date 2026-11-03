(** Local clones of opam repositories, refreshed from their remotes. *)

type t

val v :
  _ Eio_unix.Process.mgr ->
  _ Eio.Time.clock ->
  dir:string ->
  name:string ->
  string ->
  t
(** [v process clock ~dir ~name url] is the repository [url], cloned into
    [dir/name] when first refreshed. *)

val name : t -> string
val url : t -> string

val refresh : ?max_age:float -> t -> unit
(** [refresh t] fetches the newest commit of the default branch unless it was
    fetched less than [max_age] seconds ago, 600 by default. The first refresh
    clones. Concurrent refreshes wait for one fetch. *)

val commit : t -> string
(** [commit t] is the commit last fetched. It raises [Failure] before the first
    {!refresh}. *)

val packages : t -> string
(** [packages t] is the clone's [packages] directory. *)
