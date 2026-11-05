(** Local clones of opam repositories, refreshed from their remotes. *)

type t

val v :
  _ Eio_unix.Process.mgr ->
  _ Eio.Time.Mono.t ->
  dir:string ->
  name:string ->
  ?branch:string ->
  string ->
  t
(** [v process clock ~dir ~name ~branch url] is the repository [url] at
    [branch], its default branch by default, cloned into [dir/name] when first
    refreshed. Git never asks for credentials. *)

val name : t -> string
val url : t -> string
val branch : t -> string option

val with_snapshot :
  ?max_age:float -> t -> (commit:string -> packages:string -> 'a) -> 'a
(** [with_snapshot t f] is [f ~commit ~packages] for the clone's newest commit
    and its [packages] directory, which do not change until [f] returns. The
    clone is first fetched unless it was fetched less than [max_age] seconds
    ago, 600 by default, and the first call clones. A fetch may take ten
    minutes. A failed fetch keeps the previous commit, or raises when there is
    none. Calls on [t] run one at a time. *)

(** {1 Remotes named by projects} *)

type remotes
(** The opam repositories that projects name, each cloned once. *)

val remotes :
  _ Eio_unix.Process.mgr -> _ Eio.Time.Mono.t -> dir:string -> remotes
(** [remotes process clock ~dir] is an empty set of remotes, cloned under
    [dir]. *)

val remote : remotes -> url:string -> ?branch:string -> unit -> t option
(** [remote r ~url ~branch ()] is the repository [url] at [branch], made on
    first use and kept, or [None] when [r] already holds 64 others. Git
    reaches it only over https. *)
