(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Finding and registering releases on GitHub and tangled. *)

val token_of_env : string option -> string option
(** [token_of_env v] is the token in the value [v] of [GITHUB_TOKEN], or [None]
    if [v] is absent or empty. *)

val token : unit -> string option
(** [token ()] is the GitHub token in [GITHUB_TOKEN], or [None] if it is unset
    or empty. *)

val build :
  Bushel_forge.candidate ->
  registries:Bushel.Release.registry list ->
  description:string option ->
  summary:string option ->
  Bushel.Release.release
(** [build c ~registries ~description ~summary] is the release to register for
    [c]. Its date and URL are the forge's. Its summary is [summary] if that is
    not blank, else the first sentence of [description], else the title of [c]
    unless that is only the version, else the repository's name and the version.
    The tag is kept only if it differs from the version. *)

val github_release :
  http:Bushel_http.t ->
  token:string option ->
  repo:string ->
  tag:string ->
  (Bushel_forge.candidate, string) result
(** [github_release ~http ~token ~repo ~tag] is the release of [repo] at
    [tag]. *)

val github_releases :
  http:Bushel_http.t ->
  token:string option ->
  repo:string ->
  (Bushel_forge.candidate list, string) result
(** [github_releases ~http ~token ~repo] is the 100 most recent releases of
    [repo]. *)

val github_events :
  http:Bushel_http.t ->
  token:string option ->
  user:string ->
  ((string * string) list, string) result
(** [github_events ~http ~token ~user] is the [(repo, tag)] of the releases
    [user] published recently. GitHub keeps about a month of events. *)

val tangled_artifacts :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  http:Bushel_http.t ->
  repo:string ->
  (Bushel_forge.candidate list, string) result
(** [tangled_artifacts ~sw ~env ~http ~repo] is the releases of the Tangled
    repository [repo], written [handle/name]. It resolves the handle to its DID
    and data server and reads the artifacts attached to the repository. A
    network failure is an [Error]. *)
