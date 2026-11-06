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

val default_hosts : string list
(** [default_hosts] is [github.com], [gitlab.com], [codeberg.org],
    [bitbucket.org] and [git.sr.ht]. *)

val remotes :
  ?hosts:string list ->
  _ Eio_unix.Process.mgr ->
  _ Eio.Time.Mono.t ->
  dir:string ->
  remotes
(** [remotes ~hosts process clock ~dir] is an empty set of remotes, cloned
    under [dir], on the [hosts] that {!allowed} accepts, {!default_hosts} by
    default.

    The build workers fetch a remote again themselves, outside the checks
    that {!remote} makes, so a host that a project could point at a private
    address, for example by its DNS answering differently there, would let
    the project reach the cluster's network. [hosts] should therefore name
    only public forges that serve no one's private addresses. *)

val hosts : remotes -> string list
(** [hosts r] is the hosts that [r] accepts, in lower case. *)

val allowed : remotes -> string -> bool
(** [allowed r host] holds when [host] is one of [hosts r], ignoring case.
    Subdomains are not accepted. *)

val remote :
  remotes -> url:string -> host:string -> ?branch:string -> unit -> t option
(** [remote r ~url ~host ~branch ()] is the repository [url], whose host is
    [host], at [branch], made on first use and kept, or [None] when [r]
    already holds 64 others.

    Each fetch first resolves [host] and refuses it unless every address is
    public, and git then reaches only that address, over https, with no
    redirects and none of the host's git configuration. The clone holds only
    the [packages] directory, and makes no symbolic links. *)

val forget : remotes -> t -> unit
(** [forget r t] drops [t] from [r] if it was never fetched, so that a remote
    that fails does not hold one of the 64 places. *)
