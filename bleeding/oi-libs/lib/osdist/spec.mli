(** Package metadata flowing through [oi dist bundle → pkg → repo].

    Built once in [oi dist bundle] from one of:
    - {!of_local_project}: project mode — derive from cwd's [*.opam] and
      [dune-project]. The package name comes from the "root" opam package: the
      one in the cwd's [*.opam] set that no other local [*.opam] depends on. If
      more than one such candidate exists, the caller must pass [--pkg-name].
    - {!of_target_name}: a [TARGET] cmdline argument — name is the TARGET; the
      caller fills in [maintainer]/[homepage]/[license] from cmdline overrides
      or the defaults baked here.

    Serialised next to the bundle tarball as [<pkg>-<ver>.osdist.json] and read
    back by [oi dist pkg]. *)

type t = {
  package : string;
  version : string;
  epoch : int option;
  maintainer : string;
  homepage : string;
  license : string;
  prefix : string;  (** Install prefix, e.g. ["/usr"]. *)
  synopsis : string;
  description : string;
  binaries : string list;
      (** Executables produced by the build, used for the [debian/install] and
          [%files] lists. May be left empty; both family templates fall back to
          a recursive glob of [bin/]. *)
  depexts : (string * string list) list;
      (** Per-{!Dockerfile_opam.Distro.tag_of_distro} overlay depexts, computed
          at bundle time and consumed at pkg time. Keyed by the upstream distro
          tag (e.g. ["ubuntu-26.04"], ["fedora-44"]); empty for the
          [alpine-static] target (it builds inside the bundled musl chain). *)
}

val codec : t Jsont.t
(** JSON codec used for the sidecar file. *)

val write_sidecar : path:string -> t -> unit
(** [write_sidecar ~path t] serialises [t] to [path] (atomic via
    [<path>.tmp.<pid> + rename]). The conventional [path] is
    [<bundle-dir>/<pkg>-<ver>.osdist.json]. *)

val read_sidecar : path:string -> (t, string) result

val sidecar_path : bundle_path:string -> string
(** [sidecar_path ~bundle_path] is the conventional sidecar location for a
    bundle tarball ([bundle_path] with the [.tar.gz] suffix replaced by
    [.osdist.json]). *)

(** {1 Construction from external metadata} *)

val of_target_name : name:string -> version:string -> t
(** Construct a {!t} with conservative defaults for a TARGET-mode bundle. The
    caller will overlay any cmdliner overrides on top. *)

val of_opam_file :
  name:string -> version:string -> path:string -> (t, string) result
(** [of_opam_file ~name ~version ~path] parses [path] as an opam file and
    returns a {!t} with [maintainer], [homepage], [license], [synopsis] and
    [description] taken from it. Useful in TARGET mode to enrich the spec with
    the resolved package's opam metadata. *)

val override :
  ?package:string ->
  ?epoch:int ->
  ?maintainer:string ->
  ?homepage:string ->
  ?license:string ->
  ?prefix:string ->
  t ->
  t
(** [override ?package ?epoch ?maintainer ?homepage ?license ?prefix t] applies
    optional cmdliner overrides to [t]. Each [None] argument leaves the field
    untouched. *)

(** {1 Project mode — derive from *.opam DAG}

    Reads every [*.opam] in [cwd] via {!OpamFile.OPAM}. The "root" package is
    the unique [*.opam] in the cwd whose name appears in no other local
    [*.opam]'s [depends:] formula — i.e. the top of the transitive-deps DAG.

    On ambiguity (more than one root), the caller is expected to pass
    [--pkg-name] and re-enter via {!of_target_name} (or in the future, a more
    direct [select]).

    The version is read from [dune-project]'s [(version …)] stanza, falling back
    to the opam [version:] field, then to ["0.0.0"]. *)

type derive_error =
  | No_opam_files
  | Multiple_roots of string list
  | Cycle of string list

val pp_derive_error : Format.formatter -> derive_error -> unit
(** [pp_derive_error ppf e] renders [e] as a human-readable diagnostic. *)

val of_local_project : cwd:string -> (t, derive_error) result
(** [of_local_project ~cwd] reads every [*.opam] in [cwd] and returns a {!t}
    derived from the "root" package (the one no other local opam depends on).
    Errors with {!Multiple_roots} when the project has more than one such
    package (caller must disambiguate via [--pkg-name]). *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] renders [t] as [<package> <version>] for diagnostics. *)
