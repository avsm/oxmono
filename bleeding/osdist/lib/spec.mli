(** Package metadata and JSON sidecars.

    Construct metadata from a package name, an opam file or a local project.
    Sidecars store this metadata alongside source archives for later packaging.
    Constructors leave [binaries] and [depexts] empty. *)

type t = {
  package : string;  (** Native package name. *)
  version : string;  (** Upstream version, without a distribution revision. *)
  epoch : int option;  (** Version epoch. Only positive values are emitted. *)
  maintainer : string;  (** Maintainer in [Name <email>] form. *)
  homepage : string;  (** Project URL, or the empty string if unknown. *)
  license : string;  (** License name or expression. *)
  prefix : string;  (** Installation prefix, usually ["/usr"]. *)
  synopsis : string;  (** One-line summary. *)
  description : string;  (** Longer description, possibly multiline. *)
  binaries : string list;
      (** Executable names recorded in the sidecar. Packaging generators use the
          build's installed files rather than this list. *)
  depexts : (string * string list) list;
      (** System dependencies keyed by [Dockerfile_opam.Distro.tag_of_distro],
          for example ["ubuntu-26.04"]. Callers select the appropriate list and
          pass it to generators as [overlay_depexts]. *)
}

(** {1:construction Constructing metadata} *)

val of_target_name : name:string -> version:string -> t
(** [of_target_name ~name ~version] is metadata with the given package name and
    version, an ISC license, a [/usr] prefix and generated descriptions. The
    homepage is empty and the epoch is absent. The maintainer uses [DEBEMAIL]
    with [DEBFULLNAME], defaulting to [Maintainer] for the name. Without
    [DEBEMAIL], it is [Maintainer <maintainer@example.org>]. *)

val of_opam_file :
  name:string -> version:string -> path:string -> (t, string) result
(** [of_opam_file ~name ~version ~path] is metadata read from the opam file at
    [path], using the supplied package name and version. It takes the first
    maintainer and homepage, joins licenses with [" AND "], and copies the
    synopsis and description. An absent description falls back to the synopsis.
    An absent maintainer uses the defaults of {!of_target_name}. An absent
    license defaults to ISC. The prefix is [/usr] and the epoch is absent. File
    and parse errors are returned as [Error]. *)

val override :
  ?package:string ->
  ?epoch:int ->
  ?maintainer:string ->
  ?homepage:string ->
  ?license:string ->
  ?prefix:string ->
  t ->
  t
(** [override ?package ?epoch ?maintainer ?homepage ?license ?prefix t] is [t]
    with the supplied fields replaced. Omitted arguments preserve the existing
    fields. To clear an epoch, use a record update. *)

(** {1:projects Local projects} *)

type derive_error =
  | No_opam_files
      (** The directory is missing or contains no [*.opam] files. *)
  | Multiple_roots of string list
      (** More than one package has no local dependents. Lists the candidates. *)
  | Cycle of string list
      (** Every package has a local dependent. Lists all local package names. *)

val of_local_project : cwd:string -> (t, derive_error) result
(** [of_local_project ~cwd] is metadata for the unique package whose name
    appears in no local dependency formula. It reads [*.opam] files directly in
    [cwd], using their basenames as package names. Dependency filters are not
    evaluated. The version comes from [dune-project], then the package's opam
    [version] field, then ["0.0.0"]. Other fields follow {!of_opam_file}.

    Root selection errors are returned as [Error]. A missing directory returns
    [Error No_opam_files]. Other directory access and opam file errors raise
    exceptions. Use {!of_opam_file} to select a package explicitly when several
    roots exist. *)

val pp_derive_error : Format.formatter -> derive_error -> unit
(** [pp_derive_error ppf e] prints a diagnostic for [e] on [ppf]. *)

(** {1:sidecars JSON sidecars} *)

val codec : t Jsont.t
(** [codec] is the JSON codec for package metadata. Absent [binaries] and
    [depexts] fields decode as empty lists. Empty lists are omitted on output. *)

val sidecar_path : bundle_path:string -> string
(** [sidecar_path ~bundle_path] is [bundle_path] with its [.tar.gz] suffix
    replaced by [.osdist.json]. If no such suffix exists, [.osdist.json] is
    appended. *)

val write_sidecar : path:string -> t -> unit
(** [write_sidecar ~path t] writes [t] as indented JSON, atomically replacing
    [path]. The parent directory must exist. Encoding and file errors raise
    exceptions. *)

val read_sidecar : path:string -> (t, string) result
(** [read_sidecar ~path] is the metadata decoded from [path]. Missing files,
    read errors and invalid JSON are returned as [Error]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [<package> <version>] on [ppf]. *)
