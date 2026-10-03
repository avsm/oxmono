module Distro = Dockerfile_opam.Distro

type family = Deb | Rpm | Static

type t = {
  tag : string;
  family : family;
  distro : Distro.t;
  base_image : string;
  codename : string option;
  debrev : string option;
  rpmrel : string option;
  arch : string;
}

let string_of_family = function
  | Deb -> "deb"
  | Rpm -> "rpm"
  | Static -> "static"

(* Project convention: alpine's apk feeds a static-musl build rather
   than a native .apk artefact, so [`Apk -> Static]. Apt and Yum get
   the natural [Deb] / [Rpm] mapping; other package managers aren't
   in scope for [oi dist]. *)
let family_of_distro d =
  match Distro.package_manager d with
  | `Apt -> Deb
  | `Yum -> Rpm
  | `Apk -> Static
  | `Zypper -> Fmt.failwith "osdist: unsupported package manager zypper"
  | `Pacman -> Fmt.failwith "osdist: unsupported package manager pacman"
  | `Cygwin -> Fmt.failwith "osdist: unsupported package manager cygwin"
  | `Windows -> Fmt.failwith "osdist: unsupported package manager windows"

(* Docker image reference for a distro's official base image. Uses
   [base_distro_tag] which returns ("ubuntu", "noble"), ("debian", "13"),
   ("fedora", "44"), ("alpine", "3.22"), etc. — directly pullable on
   Docker Hub. *)
let base_image_of_distro d =
  let repo, tag = Distro.base_distro_tag d in
  Fmt.str "%s:%s" repo tag

(* Build a [t] for a normal (deb/rpm) target. The [tag], [family], and
   [base_image] all derive from [distro]; only the per-distro packaging
   policy bits ([codename] / [debrev] for deb, [rpmrel] for rpm) need
   passing in. *)
let make ?codename ?debrev ?rpmrel ?(arch = "x86_64") ?tag ?base_image distro =
  let tag = Option.value tag ~default:(Distro.tag_of_distro distro) in
  let family = family_of_distro distro in
  let base_image =
    Option.value base_image ~default:(base_image_of_distro distro)
  in
  { tag; family; distro; base_image; codename; debrev; rpmrel; arch }

let ubuntu_24_04 = make ~codename:"noble" ~debrev:"1~noble1" (`Ubuntu `V24_04)

let ubuntu_26_04 =
  make ~codename:"resolute" ~debrev:"1~resolute1" (`Ubuntu `V26_04)

let debian_13 = make ~codename:"trixie" ~debrev:"1~deb13" (`Debian `V13)
let fedora_44 = make ~rpmrel:"1" (`Fedora `V44)

(* The static target is the one place that diverges from the auto-derived
   defaults: the [tag] is the project-specific ["alpine-static"] (rather
   than ["alpine-3.22"], to keep the per-target output dir distinct from
   a hypothetical native-alpine target), and the [base_image] is
   [ocaml/opam:alpine-3.22-ocaml-5.4] (the [Registry_docker.oi_builder_stage]
   base) rather than bare [alpine:3.22] — we need the OCaml toolchain
   already installed inside the build stage. The [`Alpine `V3_22] distro
   ties this to the same alpine version so the bundle sidecar's depexts
   are evaluated against the right [os-distribution]. *)
let alpine_static =
  make ~tag:"alpine-static" ~base_image:"ocaml/opam:alpine-3.22-ocaml-5.4"
    (`Alpine `V3_22)

(* Order matters for the generated [build.sh]: glibc distros first
   (highest likelihood of producing artefacts), then [alpine-static]
   last because musl is the strictest target and tends to surface
   upstream porting issues that don't affect glibc builds. *)
let default_targets =
  [ ubuntu_24_04; ubuntu_26_04; debian_13; fedora_44; alpine_static ]

let of_tag s = List.find_opt (fun t -> t.tag = s) default_targets

let parse_list s =
  let toks =
    String.split_on_char ',' s |> List.map String.trim
    |> List.filter (fun x -> x <> "")
  in
  let resolved = List.filter_map of_tag toks in
  if resolved = [] then
    Fmt.failwith "osdist: no known targets in %S (known: %s)" s
      (String.concat ", " (List.map (fun t -> t.tag) default_targets));
  resolved

let pp ppf t =
  Fmt.pf ppf "%s [%s, %s]" t.tag (string_of_family t.family) t.base_image
