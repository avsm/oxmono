val valid_tag : string -> bool
(** [valid_tag name] checks the calendar date and positive epoch of a release
    tag. *)

val plan :
  Support.proc ->
  repo:string ->
  revision:string ->
  packages:string list ->
  owner:string option ->
  tag:string ->
  base_url:string ->
  guard_repository:string option ->
  previous:string option ->
  OpamJson.t
(** [plan proc ...] resolves a committed release request and records its
    resolved package versions, changed and reused projects, and hashes of its
    inputs. With [previous], temporary trial archives are rebuilt to compare
    checksums. Plans do not publish archives. *)

val export :
  Support.proc ->
  repo:string ->
  revision:string ->
  packages:string list ->
  owner:string option ->
  tag:string ->
  base_url:string ->
  guard_repository:string option ->
  previous:string option ->
  output:string ->
  unit
(** [export proc ... ~output] writes deterministic project archives, an opam
    overlay and a checksum manifest from a committed revision. [output] must not
    exist. [tag] is an existing YYYYMMDD-EPOCH Git tag. An explicit [revision]
    must resolve to the same commit. HEAD selects the tag's commit.
    [guard_repository] must be [Some path] and supplies the pinned upstream
    guard metadata. [previous] retains identical archives, package versions and
    URLs. Dependency metadata changes can require new versions of dependents. If
    every selected archive matches, no output directory is created. Source
    package entries remain immutable. No release-wide policy dependency is
    added. The caller must allocate a fresh tag before changing published bytes. *)

val verify : Support.proc -> directory:string -> unit
(** [verify proc ~directory] validates the manifest, archives and metadata
    checksums without installing packages or contacting a server. *)
