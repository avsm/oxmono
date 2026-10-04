(** RPM packaging generators for distributions using DNF.

    These functions generate files for targets in the {!Target.Rpm} family. The
    build context must contain [Dockerfile], [<package>.spec] and the source
    archive [<package>-<version>.tar.gz]. The archive must provide
    [build.sh build JOBS] and [build.sh install PREFIX DESTDIR]. *)

val spec :
  Spec.t -> Target.t -> overlay_depexts:string list -> date_rpm:string -> string
(** [spec s t ~overlay_depexts ~date_rpm] is the RPM specfile for [s] on [t]. It
    calls the source bundle's [build.sh] and packages all installed files and
    symlinks. System dependencies are added to [BuildRequires] and, unless their
    names begin with [-], to [Requires]. The release defaults to [1]. Supply the
    changelog date in RPM form, such as [Sun Oct 04 2026]. *)

val dockerfile :
  Spec.t -> Target.t -> overlay_depexts:string list -> Dockerfile.t
(** [dockerfile s t ~overlay_depexts] is a Dockerfile using [t.base_image].
    Building the image installs the toolchain and system dependencies and stages
    the archive and specfile. Running it invokes [rpmbuild] as an unprivileged
    user and copies the binary packages to [/artefacts]. *)

val filename : Spec.t -> Target.t -> string
(** [filename s t] is the binary package filename, without the epoch.
    Distribution suffixes are included for Fedora, CentOS, Oracle Linux and RHEL
    targets. For example, [hello-1.0.0-1.fc44.x86_64.rpm]. *)
