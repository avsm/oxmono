(** Debian/Ubuntu packaging generators.

    Pure string / {!Dockerfile.t} generators — no IO, no shell-out. The caller
    (see [Osdist_cmd.Pkg]) is responsible for writing the strings to disk and
    for actually invoking [docker build].

    A single target's output directory looks like:
    {v
    out/<target>/
      Dockerfile          -- {!dockerfile}
      debian/control      -- {!control}
      debian/rules        -- {!rules}     (executable, 0755)
      debian/changelog    -- {!changelog}
      debian/copyright    -- {!copyright}
      debian/source/format -- {!source_format}
    v} *)

val control : Spec.t -> Target.t -> overlay_depexts:string list -> string
(** [control s t ~overlay_depexts] is the [debian/control] body for [s] on [t],
    with [overlay_depexts] folded into [Build-Depends:]. *)

val rules : Spec.t -> Target.t -> string
(** [rules s t] is the [debian/rules] body: a debhelper-driven Makefile whose
    [override_dh_auto_build] / [override_dh_auto_install] hooks delegate to the
    bundle's [build.sh]. *)

val changelog : Spec.t -> Target.t -> date_rfc2822:string -> string
(** [changelog s t ~date_rfc2822] is the [debian/changelog] entry, encoding the
    epoch, [s.version], [t.debrev], and [t.codename]. *)

val copyright : Spec.t -> string
(** [copyright s] is a minimal Machine-Readable [debian/copyright]. *)

val source_format : string
(** [source_format] is the body of [debian/source/format] — pinned to
    [3.0 (quilt)]. *)

val dockerfile :
  Spec.t -> Target.t -> overlay_depexts:string list -> Dockerfile.t
(** [dockerfile s t ~overlay_depexts] is the multi-stage build's [Dockerfile.t]:
    a [build] stage that installs the toolchain + overlay depexts and runs
    [dpkg-buildpackage -b], then a [scratch] final stage carrying just the
    produced [.deb]. *)

val filename : Spec.t -> Target.t -> string
(** [filename s t] is the conventional binary [.deb] filename for [s] on [t],
    e.g. [oi_0.13.5-1~resolute1_amd64.deb]. *)
