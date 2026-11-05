(** Finding an opam project. *)

type t = { opam_files : (string * string) list }
(** The project's opam files at the root of the repository, with their text. *)

val find : Spindle.Executor.files -> t option
(** [find files] is the project in [files], or [None] when it has no opam files
    at its root. Files whose names are not opam package names, such as
    [a;b.opam], are ignored. *)

val packages : t -> (string * string) list
(** [packages t] is each package's name and opam file text. *)

type remote = { url : string; host : string; branch : string option }
(** An opam repository a project names: an [https] address, its host, and
    maybe a branch. *)

val remote_of_string : string -> (remote, string) result
(** [remote_of_string s] is the remote that [s] names, as
    [[git+]https://HOST/PATH[#BRANCH]], or why it is not used: another
    protocol, credentials or a port in the address, a host that is not a
    domain name or is one that only a local network resolves, or a branch
    name that git refuses or that a shell would read specially. The host's
    addresses are not checked here, but when {!Opam_repo} fetches it. *)

val remotes : t -> remote list * (string * string) list
(** [remotes t] is the union of the remotes that the [x-opam-repositories]
    fields of [t]'s opam files name, in order of first appearance and at most
    four, and each entry not used, with the reason. *)
