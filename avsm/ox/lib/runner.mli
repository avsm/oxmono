(** Resolve and build opam recipes as local day10 package layers. *)

type config = {
  cache : string;
  data : string;
  toolchain : string;
  repositories : string list;
  overlays : string list;
  from : string option;
  revision : string option;
  refresh : bool;
  jobs : int;
  cache_tag : string;
}

type prepared

val prepare :
  Support.proc ->
  clock:D10.Config.clk ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  config ->
  target:string ->
  with_packages:string list ->
  dry_run:bool ->
  prepared
(** [prepare proc ~clock ~fs ~sys config ~target ~with_packages ~dry_run]
    resolves packages in-process and builds their day10 layers. No opam CLI or
    switch is used. The toolchain uses the same build path as other packages. *)

val exec : prepared -> string list -> 'a
(** [exec prepared args] directly executes the binary with its layer
    environment. *)
