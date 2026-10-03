(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Code releases, tracked per repository.

    A release is a version published from a repository on a forge. It may also
    have reached package registries. Releases are side data rather than
    entries: they have no page and no slug, and a repository points at the
    project it belongs to instead. *)

@@ portable

(** {1 Types} *)

type forge =
  | Github  (** A repository on github.com, under any organisation. *)
  | Tangled  (** A repository on tangled, held as atproto records. *)
(** Where the code of a repository lives. *)

type registry = {
  name : string;
      (** The registry as ecosyste.ms names it, such as [pypi.org] or
          [opam.ocaml.org]. *)
  package : string;  (** The name of the package on that registry. *)
  url : string;  (** The registry's page for this version. *)
}
(** A package registry that carries a release. *)

type release = {
  version : string;  (** The version with no leading [v]. *)
  tag : string option;  (** The forge's tag, where it differs from [version]. *)
  date : Ptime.date;  (** When the forge published the release. *)
  summary : string;  (** One line describing the release. *)
  url : string;  (** The release page on the forge. *)
  registries : registry list;  (** The registries that carry this version. *)
}
(** One release made on a forge. *)

type t = {
  repo : string;
      (** [org/name] on GitHub and [handle/name] on tangled. It is the key the
          file is keyed on. *)
  forge : forge;
  project : string option;  (** Slug of the project this repository serves. *)
  releases : release list;  (** Newest first. *)
}
(** A repository and the releases registered from it. *)

type ts = t list

(** {1 Accessors} *)

val repo : t -> string
val forge : t -> forge
val project : t -> string option
val releases : t -> release list

val forge_to_string : forge -> string
(** [forge_to_string f] is the token [f] is written as in the file. *)

val forge_of_string : string -> forge option
(** [forge_of_string s] is the forge [s] names, or [None]. *)

val encode_segment : string -> string
(** [encode_segment s] is [s] as one URL path segment. Letters, digits and
    [-._~] stay and every other byte is percent-encoded. *)

val metadata_url : registry -> release -> string
(** [metadata_url reg r] is the ecosyste.ms page for version [r] of [reg]'s
    package. It is derived and never stored. *)

val add_registries : release -> registry list -> release
(** [add_registries r regs] is [r] with each registry of [regs] that [r] does
    not already have by name. A registry [r] has is kept as it was. *)

(** {1 Ordering} *)

val compare_release : release -> release -> int
(** [compare_release a b] orders newest first. *)

val compare : t -> t -> int
(** [compare a b] orders by the date of the most recent release, newest first,
    and by repository name where neither has one. *)

val latest : t -> release option
(** [latest t] is the most recent release of [t], or [None]. *)

(** {1 Files} *)

val of_yaml : Yamlrw.value -> t
(** [of_yaml v] is the repository [v] describes. A version may be written
    without quotes. The forge defaults to GitHub.

    @raise Failure if a required field is missing or malformed. *)

val to_yaml : t -> Yamlrw.value

val load_file : string -> ts @@ nonportable
(** [load_file path] is the repositories in [path], or the empty list if the
    file does not exist.

    @raise Failure if the file is not valid releases data. *)

val save_file : string -> ts -> unit @@ nonportable
(** [save_file path ts] writes [ts] to [path], newest first. The file is
    replaced whole, so a write that fails leaves it as it was.

    @raise Sys_error if the file cannot be written. *)

val merge : ts -> ts -> ts
(** [merge existing incoming] is [existing] updated with [incoming], matching on
    repository. An incoming release replaces the release of the same version,
    and the other releases are kept. An incoming record with no project keeps
    the existing one. A repository only in [existing] is kept. *)
