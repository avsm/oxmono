(** Distribution targets and package naming metadata. *)

type family =
  | Deb  (** Debian packages. *)
  | Rpm  (** RPM packages. *)
  | Static  (** Archives of static binaries built on Alpine. *)

type t = {
  tag : string;  (** Target identifier, such as ["ubuntu-26.04"]. *)
  family : family;  (** Packaging format. *)
  distro : Dockerfile_opam.Distro.t;
      (** Distribution used to select the package manager and dependencies. *)
  base_image : string;  (** Docker image used for the build. *)
  codename : string option;  (** Debian or Ubuntu release name. *)
  debrev : string option;
      (** Debian revision, defaulting to ["1"] on output. *)
  rpmrel : string option;  (** RPM release, defaulting to ["1"] on output. *)
  arch : string;
      (** Architecture name, such as ["x86_64"] or ["aarch64"]. Debian filenames
          translate these to ["amd64"] and ["arm64"]. This field does not
          configure cross-compilation or Docker's platform. *)
}

val make :
  ?codename:string ->
  ?debrev:string ->
  ?rpmrel:string ->
  ?arch:string ->
  ?tag:string ->
  ?base_image:string ->
  Dockerfile_opam.Distro.t ->
  t
(** [make distro] is a target for [distro]. Its family follows the package
    manager: Apt selects {!constructor-Deb}, Yum selects {!constructor-Rpm}, and
    Apk selects {!constructor-Static}. The tag and base image come from
    [Dockerfile_opam.Distro] unless overridden. The architecture defaults to
    ["x86_64"]. The codename and package revisions default to [None].

    Raises [Failure] if the distribution uses another package manager. *)

(** {1:presets Predefined targets}

    All predefined targets use ["x86_64"]. *)

val ubuntu_24_04 : t
(** [ubuntu_24_04] is Ubuntu 24.04 with image [ubuntu:noble], codename [noble]
    and Debian revision [1~noble1]. *)

val ubuntu_26_04 : t
(** [ubuntu_26_04] is Ubuntu 26.04 with image [ubuntu:resolute], codename
    [resolute] and Debian revision [1~resolute1]. *)

val debian_13 : t
(** [debian_13] is Debian 13 with image [debian:13], codename [trixie] and
    Debian revision [1~deb13]. *)

val fedora_44 : t
(** [fedora_44] is Fedora 44 with image [fedora:44] and RPM release [1]. *)

val alpine_static : t
(** [alpine_static] is the static musl target with tag [alpine-static] and image
    [ocaml/opam:alpine-3.22-ocaml-5.4]. The image supplies stock OCaml 5.4. The
    distribution is Alpine 3.22. *)

val default_targets : t list
(** [default_targets] is the list of predefined targets in the order above. *)

(** {1:lookup Lookup and display} *)

val of_tag : string -> t option
(** [of_tag tag] is the predefined target with exactly this tag, if any.
    Matching is case-sensitive. *)

val parse_list : string -> t list
(** [parse_list s] is the list of predefined targets named by comma-separated
    tags in [s]. Whitespace is trimmed and empty or unknown tags are dropped.
    Order and duplicates are preserved.

    Raises [Failure] if no known targets remain. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [<tag> [<family>, <base_image>]] on [ppf]. *)

val string_of_family : family -> string
(** [string_of_family f] is ["deb"], ["rpm"] or ["static"] for [f]. *)
