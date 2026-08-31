(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Code releases, tracked per repository.

    A release is a dated version of a repository, seen either where the code
    is developed or on a registry that carries it. Releases are side data
    rather than entries: they have no page and no slug, and a repository
    points at the project it belongs to instead. *)

@@ portable

(** {1 Types} *)

type forge =
  | Github  (** A repository on github.com, under any organisation. *)
  | Tangled  (** A repository on tangled, held as atproto records. *)
(** Where the code of a repository lives. *)

type source =
  | Forge  (** Released on the repository's own forge. *)
  | Registry of string
      (** Carried by a package registry, named as ecosyste.ms names it, so
          [pypi.org] and [npmjs.org] as well as repackagings such as
          [nixpkgs-24.11] and [guix]. *)
(** Where a particular version was seen. A repository's own releases and the
    registries that carry it are the same kind of fact, dated separately,
    because a registry version rarely lands on the day the tag is cut. *)

type release = {
  source : source;
  version : string;
      (** The version as its source names it, with no leading [v]. *)
  tag : string option;
      (** The tag the forge cut, where it differs from the version. *)
  date : Ptime.date;
  name : string option;  (** The title of the release, where it has one. *)
  url : string option;
}
(** One dated version. *)

type t = {
  repo : string;
      (** [org/name] on GitHub, [handle/name] on tangled. Unique across
          forges in practice, and the key this file is keyed on. *)
  forge : forge;
  project : string option;  (** Slug of the project this repository serves. *)
  synced_at : Ptime.date option;  (** When the sync last read this
                                      repository. *)
  releases : release list;  (** Newest first. *)
}
(** A tracked repository and everything released from it. *)

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

val source_to_string : source -> string
(** [source_to_string s] is the token [s] is written as in the file, which is
    ["forge"] for a repository's own release and the registry name
    otherwise. *)

val source_of_string : string -> source
(** [source_of_string s] is the source [s] names. Anything other than
    ["forge"] is a registry of that name, since the set of registries is
    whatever the upstream index reports and is not worth enumerating here. *)

val is_own : release -> bool
(** [is_own r] is whether [r] was cut on the repository's own forge rather
    than observed on a registry. *)

(** {1 Ordering} *)

val compare_release : release -> release -> int
(** [compare_release a b] orders newest first. *)

val compare : t -> t -> int
(** [compare a b] orders by the date of the most recent release, newest
    first, and by repository name where neither has one. *)

val latest : t -> release option
(** [latest t] is the most recent release of [t], or [None]. *)

(** {1 Files} *)

val of_yaml : Yamlrw.value -> t
(** [of_yaml v] is the repository [v] describes.

    @raise Failure if a required field is missing or malformed. *)

val to_yaml : t -> Yamlrw.value

val load_file : string -> ts @@ nonportable
(** [load_file path] is the repositories in [path], or the empty list if the
    file does not exist. *)

val save_file : string -> ts -> unit @@ nonportable
(** [save_file path ts] writes [ts] to [path], newest first. *)

val merge : ts -> ts -> ts
(** [merge existing incoming] is [existing] updated with [incoming], matching
    on repository. A repository in [incoming] replaces its counterpart, and
    one only in [existing] is kept, so a sync that covers part of the file
    does not drop the rest. *)
