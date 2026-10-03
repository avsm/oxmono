(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The package registries that carry a release, found through ecosyste.ms. *)

val repository_url : Bushel.Release.forge -> string -> string
(** [repository_url forge repo] is the repository URL in the form ecosyste.ms
    indexes it by. Tangled repositories are indexed as
    [git+https://tangled.org/handle/name]. *)

val summary_of_description : string -> string
(** [summary_of_description d] is the first sentence of [d] on one line, cut to
    at most 120 characters and ending in [...] if it was cut. *)

val preferred_packages : string -> string list
(** [preferred_packages repo] is the package names to prefer for [repo]: the
    repository's name, and that name without a leading [ocaml-]. *)

val attach :
  ?prefer:string list ->
  allowed:string list ->
  packages:(string * string) list ->
  carries:(registry:string -> package:string -> string option) ->
  unit ->
  Bushel.Release.registry list
(** [attach ~allowed ~packages ~carries ()] is a registry entry for each
    registry of [allowed], in that order, that has a package in [packages] for
    which [carries ~registry ~package] gives the release page of the version.
    [packages] is the [(registry, package)] pairs ecosyste.ms reports for a
    repository. A registry with several packages uses the first of [prefer]
    that is one of them and the first package otherwise. A registry that is not
    in [allowed], such as a repackaging by nixpkgs, is never attached. *)

val pick_description :
  ?prefer:string list ->
  allowed:string list ->
  attached:Bushel.Release.registry list ->
  found:(string * string * string option) list ->
  unit ->
  string option
(** [pick_description ~allowed ~attached ~found ()] is the description to
    summarise a release with. [found] is the [(registry, package, description)]
    triples ecosyste.ms reports for a repository. The description of an attached
    package is used first. Otherwise it is the first description in an allowed
    registry, taking a package named in [prefer] before another. A registry that
    is not in [allowed] is never used. *)

val lookup :
  Ecosystems.t ->
  allowed:string list ->
  forge:Bushel.Release.forge ->
  repo:string ->
  version:string ->
  (Bushel.Release.registry list * string option, string) result
(** [lookup eco ~allowed ~forge ~repo ~version] is the registries that carry
    [version] of [repo], and a description of the package from
    {!pick_description}, which a release no registry carries yet still has. A
    registry that answers that it has no such version is left out. *)
