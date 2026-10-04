module DF = Dockerfile

let ( @@ ) = Dockerfile.( @@ )

(* -- Debian arch tag derived from the target arch ----------------------- *)

let arch_of (t : Target.t) =
  match t.arch with "x86_64" -> "amd64" | "aarch64" -> "arm64" | a -> a

(* -- Epoch prefix: "1:" if [epoch = Some n] (n >= 1), else "" ----------- *)

let epoch_prefix (s : Spec.t) =
  match s.epoch with Some n when n > 0 -> Fmt.str "%d:" n | _ -> ""

(* -- Build/runtime depexts ---------------------------------------------- *)

(* Bootstrap toolchain every Debian-family build needs, before any
   project-specific [overlay_depexts] go on top. Mirrors
   [Registry_docker.build_depexts] for Apt — kept inline here so [Osdist]
   does not depend on [lib/cmd]. *)
let base_build_depexts =
  [
    "gcc";
    "g++";
    "make";
    "m4";
    "perl";
    "pkg-config";
    "patch";
    "rsync";
    "tar";
    "gzip";
    "bzip2";
    "xz-utils";
    "zstd";
    "git";
    "curl";
    "ca-certificates";
    "ocaml";
  ]

(* Tools needed to produce a .deb from a source tree. [build-essential]
   is the implicit "always assumed" build-dep that [dpkg-checkbuilddeps]
   verifies regardless of what [debian/control] lists — without it, every
   build fails with "Unmet build dependencies: build-essential:native"
   before any compilation even runs. Listed here (Dockerfile install
   only) rather than in [control]'s [Build-Depends:] since Debian Policy
   §4.2 explicitly states it should not be listed there. *)
let tooling =
  [ "build-essential"; "dpkg-dev"; "debhelper"; "devscripts"; "fakeroot" ]

let combined_install ~overlay_depexts =
  base_build_depexts @ tooling @ overlay_depexts
  |> List.sort_uniq String.compare
  |> String.concat " "

(* -- debian/control ----------------------------------------------------- *)

let control (s : Spec.t) (t : Target.t) ~overlay_depexts =
  let bd =
    List.sort_uniq String.compare
      ("debhelper-compat (= 13)"
       :: List.filter (fun x -> x <> "ocaml") base_build_depexts
      @ overlay_depexts)
    |> String.concat ",\n "
  in
  let codename = Option.value ~default:"unknown" t.codename in
  let homepage_line =
    if s.homepage = "" then "" else Fmt.str "Homepage: %s\n" s.homepage
  in
  Fmt.str
    "Source: %s\n\
     Section: devel\n\
     Priority: optional\n\
     Maintainer: %s\n\
     Build-Depends:\n\
    \ %s\n\
     Standards-Version: 4.7.0\n\
     %sRules-Requires-Root: no\n\n\
     Package: %s\n\
     Architecture: any\n\
     Depends: ${shlibs:Depends}, ${misc:Depends}\n\
     Description: %s\n\
     %s\n"
    s.package s.maintainer bd homepage_line s.package
    (if s.synopsis = "" then "Packaged by osdist" else s.synopsis)
    (if s.description = "" then " Built for " ^ codename ^ "."
     else " " ^ String.concat "\n " (String.split_on_char '\n' s.description))

(* -- debian/rules ------------------------------------------------------- *)

(* The bundle ships a top-level [build.sh] that exposes [build] and
   [install <prefix> <destdir>] entry points. [rules] delegates to it so
   the deb / rpm paths share the same build invocation.

   NB: [@] is escaped as [@@] throughout because [Fmt.str] is
   [Format.asprintf] under the hood, which treats a bare [@] as the
   start of a formatting directive (e.g. [@.], [@,]) and silently
   swallows it. An unescaped [$@] in [\tdh $@] becomes [$] in the
   emitted file, which dh then rejects with "Unknown sequence $". *)
let rules (s : Spec.t) (_t : Target.t) =
  Fmt.str
    "#!/usr/bin/make -f\n\
     export DEB_BUILD_OPTIONS = nostrip nocheck\n\
     NJOBS := $(shell nproc 2>/dev/null || echo 2)\n\
     %%:\n\
     \tdh $@@\n\
     override_dh_auto_configure:\n\
     override_dh_auto_build:\n\
     \t./build.sh build $(NJOBS)\n\
     override_dh_auto_test:\n\
     override_dh_auto_install:\n\
     \t./build.sh install %s $(CURDIR)/debian/%s\n"
    s.prefix s.package

(* -- debian/changelog --------------------------------------------------- *)

let changelog (s : Spec.t) (t : Target.t) ~date_rfc2822 =
  let codename = Option.value ~default:"unstable" t.codename in
  let debrev = Option.value ~default:"1" t.debrev in
  Fmt.str
    "%s (%s%s-%s) %s; urgency=medium\n\n\
    \  * Packaged by osdist from the oi dist bundle.\n\n\
     %s -- %s  %s\n"
    s.package (epoch_prefix s) s.version debrev codename "" s.maintainer
    date_rfc2822

(* -- debian/copyright (minimal, MachineReadable-style) ------------------ *)

let copyright (s : Spec.t) =
  Fmt.str
    "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n\
     Upstream-Name: %s\n\
     Source: %s\n\n\
     Files: *\n\
     Copyright: %s\n\
     License: %s\n"
    s.package
    (if s.homepage = "" then "(unknown)" else s.homepage)
    s.maintainer s.license

let source_format = "3.0 (quilt)\n"

(* -- The .deb filename convention --------------------------------------- *)

let filename (s : Spec.t) (t : Target.t) =
  let debrev = Option.value ~default:"1" t.debrev in
  Fmt.str "%s_%s-%s_%s.deb" s.package s.version debrev (arch_of t)

(* -- Dockerfile --------------------------------------------------------- *)

(* Multi-stage build:
     Single-stage: install toolchain + project depexts and stage the
     .orig tarball + debian/ tree at image-build time; the actual
     [dpkg-buildpackage] runs at container-run time so the user can
     [docker compose up --build] with [/artefacts] bind-mounted to
     ./artefacts/<tag>/ and collect the .deb directly. *)
let dockerfile (s : Spec.t) (t : Target.t) ~overlay_depexts =
  let pkgs = combined_install ~overlay_depexts in
  let orig = Fmt.str "%s_%s.orig.tar.gz" s.package s.version in
  let srcdir = Fmt.str "%s-%s" s.package s.version in
  DF.comment "Generated by `oi dist pkg` — build %s (.deb)" t.tag
  @@ DF.parser_directive (`Syntax "docker/dockerfile:1.6")
  @@ DF.from t.base_image
  @@ DF.env [ ("DEBIAN_FRONTEND", "noninteractive") ]
  @@ DF.run
       "apt-get update && apt-get install -y --no-install-recommends %s && rm \
        -rf /var/lib/apt/lists/*"
       pkgs
  @@ DF.workdir "/work"
  (* Bundle tarball is the upstream .orig: drop it in with the canonical
     "<name>_<ver>.orig.tar.gz" name expected by dpkg-source. *)
  @@ DF.copy
       ~src:[ Fmt.str "%s-%s.tar.gz" s.package s.version ]
       ~dst:(Fmt.str "/work/%s" orig) ()
  @@ DF.run "tar -xzf %s" orig
  @@ DF.copy ~src:[ "debian" ] ~dst:(Fmt.str "/work/%s/debian" srcdir) ()
  @@ DF.run "chmod 0755 /work/%s/debian/rules" srcdir
  (* Mountpoint for the host-side bind-mount; [0777] so writes from
     any container UID (root here, but [opam] for alpine-static) land
     successfully even when the host dir was created with a different
     owner. *)
  @@ DF.run "mkdir -p /artefacts && chmod 0777 /artefacts"
  @@ DF.workdir "/work/%s" srcdir
  @@ DF.cmd_exec
       [
         "sh";
         "-c";
         "set -eu; dpkg-buildpackage -b -uc -us && cp /work/*.deb /artefacts/";
       ]
