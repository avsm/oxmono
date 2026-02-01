(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** High-level Tangled API operations.

    This module provides operations for interacting with Tangled's AT Protocol
    collections and knot servers. It wraps the XRPC client with Tangled-specific
    functionality.

    {2 Overview}

    Tangled uses two types of servers:
    - {b PDS} (Personal Data Server): Stores AT Protocol records like
      [sh.tangled.repo] in the user's repository
    - {b Knot}: Distributed git hosting servers that store the actual git data

    Most operations require authentication. Use {!login} to authenticate and
    {!resume} to restore a saved session.

    {2 Usage}

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let api = Tangled_api.create ~sw ~env ~pds:"https://bsky.social" () in

      (* Login *)
      Tangled_api.login api ~identifier:"alice.bsky.social"
        ~password:"app-password";

      (* List repos *)
      let repos = Tangled_api.list_repos api () in
      List.iter (fun (rkey, repo) -> Fmt.pr "%s: %s@." rkey repo.name) repos;

      (* Create repo *)
      Tangled_api.create_repo api ~name:"my-project" ~knot:"knot.tangled.sh"
        ~description:"My new project" ();

      (* Clone *)
      Tangled_api.clone api ~repo:"alice/my-project" ~dir:"./my-project" ()
    ]} *)

(** {1 API Client} *)

type t = Xrpc_auth.Client.t
(** Tangled API client (uses shared xrpc_auth client). *)

val create :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; net : _ Eio.Net.t
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  app_name:string ->
  ?profile:string ->
  pds:string ->
  unit ->
  t
(** [create ~sw ~env ~app_name ?profile ~pds ()] creates a Tangled API client.

    @param sw Eio switch for resource management
    @param env Eio environment capabilities
    @param app_name Application name for session storage
    @param pds Base URL of the PDS (e.g., ["https://bsky.social"]) *)

(** {1 Authentication} *)

val login : t -> identifier:string -> password:string -> unit
(** [login api ~identifier ~password] authenticates with the PDS.

    Stores the session for subsequent requests and saves it to disk.

    @param identifier Handle or DID (e.g., ["alice.bsky.social"])
    @param password Account password or app password

    @raise Eio.Io with {!Xrpc.Error.E} on authentication failure *)

val resume : t -> session:Xrpc_auth.Session.t -> unit
(** [resume api ~session] resumes from a saved session.

    Refreshes the access token if needed.

    @raise Eio.Io with {!Xrpc.Error.E} if tokens are expired *)

val logout : t -> unit
(** [logout api] logs out and clears the session from disk. *)

val get_session : t -> Xrpc_auth.Session.t option
(** [get_session api] returns the current session, if authenticated. *)

val is_logged_in : t -> bool
(** [is_logged_in api] returns [true] if there's an active session. *)

(** {1 Identity} *)

val resolve_handle : t -> string -> string
(** [resolve_handle api handle] resolves a handle to a DID.

    @raise Eio.Io with {!Xrpc.Error.E} if handle not found *)

val get_did : t -> string
(** [get_did api] returns the DID of the authenticated user.

    @raise Failure if not logged in *)

(** {1 Repository Operations} *)

val list_repos :
  t ->
  ?did:string ->
  unit ->
  (string * Atp_lexicon_tangled.Sh.Tangled.Repo.main) list
(** [list_repos api ?did ()] lists repositories for a user.

    @param did DID to list repos for (default: logged-in user)
    @return List of [(rkey, repo)] pairs *)

val get_repo :
  t ->
  did:string ->
  rkey:string ->
  Atp_lexicon_tangled.Sh.Tangled.Repo.main option
(** [get_repo api ~did ~rkey] fetches a single repository record. *)

val create_repo :
  t ->
  name:string ->
  knot:string ->
  ?description:string ->
  ?default_branch:string ->
  unit ->
  string
(** [create_repo api ~name ~knot ?description ?default_branch ()] creates a new
    repository.

    This performs two operations: 1. Creates [sh.tangled.repo] record on the PDS
    2. Initializes bare git repo on the knot server

    @param name Repository name (e.g., ["my-project"])
    @param knot Knot hostname (e.g., ["knot.tangled.sh"])
    @param description Optional description
    @param default_branch Default branch name (default: ["main"])
    @return The rkey of the created repository

    @raise Eio.Io with {!Xrpc.Error.E} on failure *)

val delete_repo : t -> name:string -> knot:string -> unit
(** [delete_repo api ~name ~knot] deletes a repository.

    This performs two operations: 1. Deletes the [sh.tangled.repo] record from
    the PDS 2. Deletes the git repo from the knot server

    @raise Eio.Io with {!Xrpc.Error.E} on failure *)

val get_repo_info :
  t -> knot:string -> did:string -> name:string -> Tangled_types.repo_info
(** [get_repo_info api ~knot ~did ~name] gets repository info from a knot.

    Returns the default branch and language statistics.

    @raise Eio.Io with {!Xrpc.Error.E} on failure *)

(** {1 Git Operations} *)

val clone : t -> repo:string -> ?dir:string -> unit -> unit
(** [clone api ~repo ?dir ()] clones a repository.

    @param repo Repository identifier: ["user/repo"] or AT URI
    @param dir Target directory (default: repo name)

    This shells out to [git clone]. Requires [git] in PATH.

    @raise Failure if git command fails *)

val git_url : t -> knot:string -> did:string -> name:string -> string
(** [git_url api ~knot ~did ~name] constructs the git clone URL.

    Returns [https://{knot}/{did}/{name}.git]. *)

(** {1 Pipeline Operations} *)

val get_spindle_for_repo :
  t -> did:string -> repo_name:string -> (string * string) option
(** [get_spindle_for_repo api ~did ~repo_name] looks up the spindle configured
    for a repository.

    @param did DID of the repo owner
    @param repo_name Repository name
    @return
      [Some (spindle_host, spindle_did)] if a spindle is configured, [None]
      otherwise *)

val list_pipelines :
  t ->
  spindle:string ->
  unit ->
  (string * Atp_lexicon_tangled.Sh.Tangled.Pipeline.main) list
(** [list_pipelines api ~spindle ()] lists all CI pipelines from a spindle.

    Pipelines are stored by the spindle (CI runner), not by the user's PDS.

    @param spindle Spindle hostname (e.g., ["spindle.tangled.sh"])
    @return List of [(rkey, pipeline)] pairs *)

val list_pipelines_for_repo :
  t ->
  spindle:string ->
  did:string ->
  repo_name:string ->
  unit ->
  (string * Atp_lexicon_tangled.Sh.Tangled.Pipeline.main) list
(** [list_pipelines_for_repo api ~spindle ~did ~repo_name ()] lists pipelines
    for a specific repository.

    @param spindle Spindle hostname to query
    @param did DID of the repo owner
    @param repo_name Repository name to filter by
    @return List of [(rkey, pipeline)] pairs for the given repo *)

val list_pipeline_statuses :
  t ->
  spindle:string ->
  pipeline_rkey:string ->
  unit ->
  Atp_lexicon_tangled.Sh.Tangled.Pipeline.Status.main list
(** [list_pipeline_statuses api ~spindle ~pipeline_rkey ()] lists status updates
    for a specific pipeline.

    @param spindle Spindle hostname where the pipeline is stored
    @param pipeline_rkey The rkey of the pipeline to get statuses for
    @return List of status updates, most recent first *)

val get_pipeline_summary :
  t ->
  spindle:string ->
  pipeline_rkey:string ->
  unit ->
  (string * Atp_lexicon_tangled.Sh.Tangled.Pipeline.Status.main) list
(** [get_pipeline_summary api ~spindle ~pipeline_rkey ()] gets the latest status
    for each workflow in a pipeline.

    @param spindle Spindle hostname where the pipeline is stored
    @param pipeline_rkey The rkey of the pipeline
    @return List of [(workflow_name, latest_status)] pairs *)

(** {1 Knot Operations} *)

val get_knot_version :
  t -> knot:string -> Atp_lexicon_tangled.Sh.Tangled.Knot.Version.output
(** [get_knot_version api ~knot] gets the version of a knot server.

    @param knot Knot hostname (e.g., ["knot.tangled.sh"]) *)

val list_knot_keys :
  t ->
  knot:string ->
  ?limit:int ->
  ?cursor:string ->
  unit ->
  Atp_lexicon_tangled.Sh.Tangled.Knot.ListKeys.output
(** [list_knot_keys api ~knot ?limit ?cursor ()] lists public keys stored on a
    knot server.

    @param knot Knot hostname
    @param limit Maximum number of keys to return
    @param cursor Pagination cursor *)

(** {1 Profile Operations} *)

val get_profile :
  t -> did:string -> Atp_lexicon_tangled.Sh.Tangled.Actor.Profile.main option
(** [get_profile api ~did] gets a user's Tangled profile.

    @param did DID of the user
    @return [Some profile] if found, [None] otherwise *)

(** {1 Public Keys} *)

val list_public_keys :
  t ->
  ?did:string ->
  unit ->
  (string * Atp_lexicon_tangled.Sh.Tangled.PublicKey.main) list
(** [list_public_keys api ?did ()] lists a user's SSH public keys.

    @param did DID to list keys for (default: logged-in user)
    @return List of [(rkey, public_key)] pairs *)

(** {1 Stars} *)

val list_stars :
  t ->
  ?did:string ->
  unit ->
  (string * Atp_lexicon_tangled.Sh.Tangled.Feed.Star.main) list
(** [list_stars api ?did ()] lists a user's starred repositories.

    @param did DID to list stars for (default: logged-in user)
    @return List of [(rkey, star)] pairs *)
