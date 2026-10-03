(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Releases as the forges report them.

    These functions read the JSON of the GitHub API and of atproto and do no
    I/O. The date of a release is the forge's own publication date. *)

type candidate = {
  repo : string;  (** [org/name] on GitHub, [handle/name] on tangled. *)
  forge : Bushel.Release.forge;
  tag : string;  (** The tag. On tangled it is the version. *)
  version : string;  (** The tag with a leading [v] removed. *)
  date : Ptime.date;  (** When the forge published the release. *)
  title : string option;  (** The release title, where there is one. *)
  url : string;  (** The release page. *)
  author : string option;
      (** The GitHub login that published it. Tangled artifacts have none. *)
  prerelease : bool;
}
(** A release a forge has, whether or not it is registered. *)

val version_of_tag : string -> string
(** [version_of_tag tag] is [tag] without a leading [v] that is followed by a
    digit. [vim-9] is unchanged. *)

val github_releases : repo:string -> string -> (candidate list, string) result
(** [github_releases ~repo json] is the releases in the body [json] of
    [GET /repos/{repo}/releases]. Drafts are left out. *)

val github_release : repo:string -> string -> (candidate, string) result
(** [github_release ~repo json] is the release in the body [json] of
    [GET /repos/{repo}/releases/tags/{tag}]. *)

val github_events : string -> (string * string) list
(** [github_events json] is the [(repo, tag)] of each published release in the
    body [json] of [GET /users/{login}/events/public]. *)

val tangled_version : repo_name:string -> string -> string option
(** [tangled_version ~repo_name name] is the version of the artifact called
    [name], which has the shape [repo_name-version.tbz]. It is [None] if [name]
    has another shape. *)

val tangled_artifacts : repo:string -> string -> (candidate list, string) result
(** [tangled_artifacts ~repo json] is the releases of [repo] in the body [json]
    of [com.atproto.repo.listRecords] for [sh.tangled.repo.artifact]. An
    artifact belongs to [repo] if its name has the repository's name as a
    prefix. Artifacts of one version are one release. *)

val unregistered :
  author:string ->
  registered:Bushel.Release.ts ->
  candidate list ->
  candidate list
(** [unregistered ~author ~registered candidates] is the candidates that
    [author] published and that are not in [registered]. Prereleases are left
    out. A candidate with no author is kept, because tangled artifacts live in
    the author's own repository. *)
