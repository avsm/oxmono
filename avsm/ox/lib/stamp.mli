type package = {
  name : string;
  project : string;
  opam_path : string;
  base : string;
  version : string;
  source_hash : string;
  opam : OpamFile.OPAM.t;
  binaries : string list;
}
(** Export committed Dune projects through ordinary opam metadata. *)

type snapshot = {
  root : string;
  commit : string;
  source : string;
  packages : package list;
}

val inspect :
  Support.proc ->
  repo:string ->
  revision:string ->
  source:string option ->
  snapshot
(** [inspect proc ~repo ~revision ~source] reads committed package metadata.
    Empty opam placeholders are reported and skipped. Duplicate names fail. *)

val stamped_opam : snapshot -> package -> string
(** [stamped_opam snapshot package] pins source and internal dependencies to the
    snapshot, retaining the package's opam build and install commands. *)

val export :
  Support.proc ->
  repo:string ->
  revision:string ->
  source:string option ->
  output:string ->
  snapshot
(** [export proc ~repo ~revision ~source ~output] atomically creates a new opam
    repository. [output] must not exist. *)
