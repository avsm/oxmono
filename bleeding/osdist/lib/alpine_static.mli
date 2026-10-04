(** Static-musl binary build on Alpine.

    Produces a {!Dockerfile.t} whose final stage is [scratch] carrying just the
    statically-linked binaries from the bundle. The [build] stage runs the
    bundle's own [make] with [OI_STATIC=1] in the environment (the project's
    [bin/dune] picks that up and appends [-cclib -static] to the release
    profile).

    The packaging entry point ([Osdist_cmd.Pkg]) drives
    [docker buildx build --output type=local,dest=…] to extract the binaries,
    then tars them into {!tarball_filename}. *)

val dockerfile :
  ?overlay_depexts:string list -> Spec.t -> Target.t -> Dockerfile.t
(** [dockerfile ?overlay_depexts s t] is the static-musl multi-stage
    [Dockerfile.t] for [s] on [t]: an [oi-builder] alpine+ocaml stage that runs
    the bundle's [build.sh] with [OI_STATIC=1], then a [scratch] final stage
    carrying just [/bin]. [overlay_depexts] are extra alpine packages (evaluated
    from the closure's [depexts:] filters against [os-distribution = "alpine"])
    to install on top of the bootstrap toolchain. *)

val tarball_filename : Spec.t -> Target.t -> string
(** [tarball_filename s t] is the basename of the static-binary tarball:
    [<pkg>-<ver>-linux-<arch>-static.tar.gz]. *)

val build_sh : Spec.t -> Target.t -> string
(** [build_sh s t] is a host-side helper script that runs
    [docker buildx build --output type=local], tars the result into
    {!tarball_filename}, and writes a sha256 sidecar. Written next to the
    Dockerfile; invoked by the top-level [build.sh] driver. *)
