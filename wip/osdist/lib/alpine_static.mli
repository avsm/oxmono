(** Static binary archives built on Alpine Linux.

    Use a target in the {!Target.Static} family with an Alpine image that
    supplies an OCaml toolchain and an [opam] user, such as
    {!Target.alpine_static}. The build context must contain
    [<package>-<version>.tar.gz]. Its [build.sh] must support
    [build.sh build JOBS] and [build.sh install PREFIX DESTDIR], and honour
    [OI_STATIC=1] by linking executables statically. *)

val dockerfile :
  ?overlay_depexts:string list -> Spec.t -> Target.t -> Dockerfile.t
(** [dockerfile ?overlay_depexts s t] is a Dockerfile using [t.base_image].
    Building the image installs the toolchain dependencies and unpacks the
    sources. Running it builds with [OI_STATIC=1], installs with prefix [/usr]
    under [/dist], and archives the contents of [/dist/usr/bin]. The tarball and
    a [.sha256] checksum file are written to [/artefacts]. [overlay_depexts]
    defaults to [[]] and adds Alpine system packages. *)

val tarball_filename : Spec.t -> Target.t -> string
(** [tarball_filename s t] is [<package>-<version>-linux-<arch>-static.tar.gz]. *)

val build_sh : Spec.t -> Target.t -> string
(** [build_sh s t] is a host-side shell script to write alongside the
    Dockerfile. It runs [docker build], then [docker run] with the script's
    directory mounted at [/artefacts]. The resulting archive and checksum are
    written there. The script makes that directory world-writable so the
    container's unprivileged user can write its output. *)
