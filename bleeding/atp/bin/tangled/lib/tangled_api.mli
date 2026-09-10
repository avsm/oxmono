type t = Xrpc_auth.Client.t
(** Tangled API client (uses shared xrpc_auth client). *)

val create :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  app_name:string ->
  ?profile:string ->
  pds:string ->
  ?http:_ Fetch.t ->
  unit ->
  t
(** [create ~sw ~env ~app_name ?profile ~pds ?http ()] creates a Tangled API
    client.

    @param sw Eio switch for resource management
    @param env Eio environment capabilities
    @param app_name Application name for session storage
    @param pds Base URL of the PDS (e.g., ["https://bsky.social"])
    @param http Optional shared HTTP client reused by every request *)

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

val get_client : t -> Xrpc.Client.t
val service_url : string -> string
val service_host : string -> string
val service_did : string -> string
val public_client : t -> service:string -> Xrpc.Client.t

val service_client :
  t ->
  service:string ->
  ?audience:string ->
  nsid:string ->
  unit ->
  Xrpc.Client.t
(** [service_client t ~service ~nsid ()] obtains a fresh PDS-issued token
    restricted to [nsid]. An explicit audience supports local test gateways. *)

val encode : 'a Jsont.t -> 'a -> Jsont.json
val decode : 'a Jsont.t -> Jsont.json -> 'a
val now : unit -> string
val rkey_of_uri : string -> string

module Atproto = Atp_lexicon_atproto.Com.Atproto
module Lex = Atp_lexicon_tangled.Sh.Tangled

val list_records :
  t -> did:string -> collection:string -> Atproto.Repo.ListRecords.record list
(** [list_records t ~did ~collection] reads every page, rejecting repeated
    cursors. Records are read from the configured PDS. *)

val get_record :
  t ->
  did:string ->
  collection:string ->
  rkey:string ->
  Atproto.Repo.GetRecord.output option

val create_record :
  t ->
  collection:string ->
  ?rkey:string ->
  Jsont.json ->
  Atproto.Repo.CreateRecord.output

val put_record :
  t ->
  collection:string ->
  rkey:string ->
  swap_record:string option ->
  Jsont.json ->
  Atproto.Repo.PutRecord.output

val delete_record :
  t -> collection:string -> rkey:string -> ?swap_record:string -> unit -> unit

val list_repos : t -> ?did:string -> unit -> (string * Lex.Repo.main) list

val list_public_keys :
  t -> ?did:string -> unit -> (string * Lex.PublicKey.main) list

val list_stars : t -> ?did:string -> unit -> (string * Lex.Feed.Star.main) list
val get_repo : t -> did:string -> rkey:string -> Lex.Repo.main option
val get_profile : t -> did:string -> Lex.Actor.Profile.main option

type repository = {
  owner : string;
  rkey : string;
  record : Lex.Repo.main;
  service : string;
}

val repo_did : repository -> string

val resolve_repo : t -> ?knot:string -> string -> repository
(** [resolve_repo t repo] resolves owner/name or a record URI. A repository DID
    requires [knot] for authoritative ownership lookup. *)

val create_repo :
  t ->
  name:string ->
  knot:string ->
  ?audience:string ->
  ?description:string ->
  ?default_branch:string ->
  ?source:repository ->
  unit ->
  repository

val delete_repo : t -> ?audience:string -> repository -> unit
(** [delete_repo t repository] removes the PDS record using a CID comparison
    before requesting knot teardown. Failures preserve the knot error. *)

val git_url : repository -> string
val clone : t -> repo:string -> ?knot:string -> ?dir:string -> unit -> unit

val query_pipelines :
  t ->
  spindle:string ->
  repo:string ->
  ?commits:string list ->
  ?kinds:string list ->
  ?limit:int ->
  ?cursor:string ->
  unit ->
  Lex.Ci.QueryPipelines.output

val get_pipeline :
  t -> spindle:string -> pipeline:string -> Lex.Ci.GetPipeline.output

val trigger_pipeline :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  trigger:Jsont.json ->
  ?workflows:string list ->
  unit ->
  Lex.Ci.TriggerPipeline.output

val cancel_pipeline :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  pipeline:string ->
  ?workflows:string list ->
  unit ->
  unit
