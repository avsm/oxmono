(** Repository configuration and installation instructions.

    These functions return text for an APT and DNF repository. Callers create
    and sign the repository indexes, export the public key and publish the
    files. The generated URLs use this layout beneath [baseurl]:

    {v
    apt/                 APT repository and public key
    rpm/<target-tag>/    RPM repository and public key
    bin/                 Static archives with "latest" in place of the version
    install.sh           Generated installer
    v} *)

type config = {
  baseurl : string;  (** Public repository URL, without a trailing slash. *)
  origin : string;  (** APT origin and DNF repository display name. *)
  label : string;  (** APT label. *)
  description : string;  (** One-line APT repository description. *)
  gpg_key_id : string;  (** Signing key identifier for [reprepro]. *)
  pubkey_filename : string;
      (** Exported ASCII-armored public key basename, such as ["hello.asc"].
          Place a copy in [apt/] and each [rpm/<target-tag>/] directory. *)
}

val apt_distributions : config -> deb_targets:Target.t list -> string
(** [apt_distributions cfg ~deb_targets] is [conf/distributions] for [reprepro].
    It emits one stanza per distinct codename, sorted by name. Targets without a
    codename are omitted. Each stanza uses component [main], architecture
    [amd64] and signing key [cfg.gpg_key_id]. *)

val dnf_repo_file : config -> Target.t -> pkg:string -> string
(** [dnf_repo_file cfg t ~pkg] is a DNF repository file for [t], with section
    name [pkg]. It uses [rpm/<target-tag>/] beneath [cfg.baseurl] and enables
    signature checking for both packages and repository metadata. *)

val install_sh : config -> Spec.t -> targets:Target.t list -> string
(** [install_sh cfg s ~targets] is a POSIX shell installer for [s.package]. It
    detects Debian and Ubuntu by codename and Fedora 43 or 44 by version,
    selecting a native repository when the matching target is present. With a
    static target present, other hosts fall back to the static archive.

    The script accepts [--repo-url URL], [--static] and [--prefix DIR].
    [--static] forces a static download even if [targets] has no static entry.
    Static executables are installed in [DIR/bin], defaulting to
    [$HOME/.local/bin]. Native installation uses system package directories and
    invokes [sudo] when needed and available. *)

val install_md : config -> Spec.t -> targets:Target.t list -> string
(** [install_md cfg s ~targets] is Markdown installation documentation with
    commands for the packaging families present in [targets]. *)
