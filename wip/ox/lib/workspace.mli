type t = { root : string; projects : string list; prepared : Runner.prepared }
(** Working-tree builds with repository dependencies supplied by day10. *)

val prepare :
  ?binary:string ->
  Support.proc ->
  clock:D10.Config.clk ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  Runner.config ->
  roots:string list ->
  action:Runner.action ->
  dry_run:bool ->
  t
(** [prepare proc ~clock ~fs ~sys config ~roots ~action ~dry_run] discovers opam
    packages in the Git checkout and builds dependencies absent from the
    workspace. A root toolchain lock pins the compiler and default repositories.
    Local prerequisites of repository packages are also built. Empty [roots]
    selects projects beneath the working directory. With [binary], empty [roots]
    instead selects its unique local package owner across the checkout. *)

val execute : t -> test:bool -> profile:string -> jobs:int -> 'a
(** [execute workspace ~test ~profile ~jobs] runs scoped Dune aliases from the
    Git root using the prepared environment. Working files remain editable. *)

val run : t -> profile:string -> jobs:int -> binary:string -> string list -> 'a
(** [run workspace ~profile ~jobs ~binary args] incrementally builds and runs a
    Dune binary, preserving the caller's working directory and arguments. *)
