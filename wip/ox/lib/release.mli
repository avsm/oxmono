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
  OpamJson.t
(** [plan proc ...] resolves a committed release request and records its package
    closure and guard input hash without writing archives. *)

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
    guard metadata. [previous] optionally carries an earlier overlay forward.
    The caller must allocate a fresh version before changing published bytes. *)

val verify : Support.proc -> directory:string -> unit
(** [verify proc ~directory] validates the manifest, archives and metadata
    checksums without installing packages or contacting a server. *)
