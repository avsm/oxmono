(** Debian and Ubuntu packaging generators.

    These functions generate files for targets in the {!Target.Deb} family.
    Write them into the build context alongside [<package>-<version>.tar.gz]:

    {v
    Dockerfile
    debian/control
    debian/rules
    debian/changelog
    debian/copyright
    debian/source/format
    v}

    The source archive must provide [build.sh build JOBS] and
    [build.sh install PREFIX DESTDIR]. *)

val control : Spec.t -> Target.t -> overlay_depexts:string list -> string
(** [control s t ~overlay_depexts] is [debian/control] for [s] on [t]. The
    supplied system dependencies are added to [Build-Depends]. *)

val rules : Spec.t -> Target.t -> string
(** [rules s t] is [debian/rules], using debhelper to call the source bundle's
    [build.sh] for building and installing under [s.prefix]. Write this file
    with executable permissions. *)

val changelog : Spec.t -> Target.t -> date_rfc2822:string -> string
(** [changelog s t ~date_rfc2822] is a [debian/changelog] entry with the package
    epoch, version and Debian revision. The revision defaults to [1] and the
    codename to [unstable]. Supply the date in RFC 2822 form, such as
    [Sun, 04 Oct 2026 12:00:00 +0000]. *)

val copyright : Spec.t -> string
(** [copyright s] is a minimal machine-readable [debian/copyright] file. *)

val source_format : string
(** [source_format] is ["3.0 (quilt)\n"], for [debian/source/format]. *)

val dockerfile :
  Spec.t -> Target.t -> overlay_depexts:string list -> Dockerfile.t
(** [dockerfile s t ~overlay_depexts] is a Dockerfile using [t.base_image].
    Building the image installs the toolchain and system dependencies and stages
    the source archive and [debian/] directory. Running it invokes
    [dpkg-buildpackage -b -uc -us] and copies the binary packages to
    [/artefacts]. *)

val filename : Spec.t -> Target.t -> string
(** [filename s t] is the binary package filename, without the epoch. For
    example, [hello_1.0.0-1~deb13_amd64.deb]. *)
