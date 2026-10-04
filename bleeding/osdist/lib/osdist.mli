(** Native Linux packaging for OCaml projects.

    Osdist generates Debian and RPM packaging files, Dockerfiles for building
    packages and static binaries, and repository configuration. Callers write
    the generated files, run the containers and publish the resulting packages.

    {1:usage Generating packaging files}

    Create package metadata with {!Spec}, choose a {!Target}, then use the
    generator for its packaging family. For example, to generate a Debian
    control file:

    {[
      let spec =
        Osdist.Spec.of_target_name ~name:"hello" ~version:"1.0.0"
        |> Osdist.Spec.override ~maintainer:"Example <dev@example.org>"
      in
      Osdist.Deb.control spec Osdist.Target.debian_13
        ~overlay_depexts:[ "libgmp-dev" ]
    ]}

    Generators return strings or [Dockerfile.t] values. Render a Dockerfile with
    [Dockerfile.string_of_t]. The [overlay_depexts] arguments contain system
    package names for the selected distribution. Callers resolve these
    dependencies and pass them explicitly.

    {1:bundles Source bundles and build containers}

    Each build context must contain a source archive named
    [<package>-<version>.tar.gz], with a top-level directory of the same name
    without [.tar.gz]. That directory must contain an executable [build.sh]
    supporting:

    {v
    ./build.sh build JOBS
    ./build.sh install PREFIX DESTDIR
    v}

    Installation must place files beneath [DESTDIR], retaining the [PREFIX]
    path. Debian builds also require the generated [debian/] directory in the
    build context. RPM builds require [<package>.spec].

    The generated Dockerfiles install build dependencies and stage sources
    during [docker build]. Running each image builds the project and writes its
    packages to [/artefacts]. Bind-mount an output directory there to collect
    them. Static builds require the project to honour [OI_STATIC=1] and install
    its executables under [/usr/bin].

    {1:api Modules} *)

module Spec = Spec
(** Package metadata, opam file readers and JSON sidecars. *)

module Target = Target
(** Distribution targets, build images and package naming conventions. *)

module Deb = Deb
(** Debian and Ubuntu package generation. *)

module Rpm = Rpm
(** RPM package generation for distributions using DNF. *)

module Alpine_static = Alpine_static
(** Static binary archives built on Alpine Linux. *)

module Repo_index = Repo_index
(** APT and DNF repository configuration and installation instructions. *)
