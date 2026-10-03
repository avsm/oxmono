type config = {
  cache : string;
  data : string;
  compiler : string;
  repositories : string list;
  overlays : string list;
  from : string option;
  revision : string;
  refresh : bool;
  jobs : int;
  cache_tag : string;
}
(** Build complete environments at stable paths in a per-user cache. *)

type prepared

val default_compiler : unit -> string
(** [default_compiler ()] reads the selected opam switch without modifying it. *)

val prepare :
  Support.proc ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  config ->
  target:string ->
  with_packages:string list ->
  dry_run:bool ->
  prepared
(** [prepare proc ~clock ~fs config ~target ~with_packages ~dry_run] resolves
    and installs under a process lock. Completion is recorded after the binary
    and full opam export exist. A dry run copies compiler artifacts but executes
    no package actions. *)

val exec : prepared -> string list -> 'a
(** [exec prepared args] replaces the process with the selected binary in its
    opam environment, preserving arguments and the working directory. *)
