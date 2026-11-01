(** Tangled clients for PDS records, knots and spindles.

    A repository has its own DID, hosted by a knot. The knot holds the
    repository's [org.tangled.repo.manifest] record, which names the owner and
    the repository's name. The owner's PDS holds an
    [org.tangled.repo.declaration] record, keyed by the lowercased name, that
    names the repository DID back. A repository is resolved only while both
    agree. *)

type t
(** A Tangled client with an optional PDS session. *)

val default_plc : string
(** [default_plc] is [https://plc.directory]. *)

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
  ?plc:string ->
  pds:string ->
  ?http:_ Fetch.t ->
  unit ->
  t
(** [create ~sw ~env ~app_name ?profile ?plc ~pds ?http ()] is a client for
    the PDS [pds]. [plc] is the PLC directory that resolves [did:plc]
    identifiers, {!default_plc} by default. [http] is shared by every request.
*)

(** {1 Authentication} *)

val login : t -> identifier:string -> password:string -> unit
(** [login t ~identifier ~password] starts a PDS session and saves it. *)

val login_oauth :
  t ->
  net:_ Eio.Net.t ->
  ?identifier:string ->
  ?redirect:Xrpc_auth.Oauth.redirect ->
  ?scope:string ->
  open_url:(string -> unit) ->
  unit ->
  unit
(** [login_oauth t ~net ?identifier ?redirect ?scope ~open_url ()] signs in through the
    PDS's OAuth server and saves the session, as {!Xrpc_auth.Oauth.authorize}
    describes. [t]'s PDS must host the account. *)

val service_methods : string list
(** [service_methods] are the methods this module asks the PDS to sign
    service tokens for. An OAuth session needs an [rpc] scope for each. *)

val resume : t -> session:Xrpc_auth.Session.t -> unit
(** [resume t ~session] resumes a saved session, refreshing it if needed. *)

val logout : t -> unit
val get_session : t -> Xrpc_auth.Session.t option
val is_logged_in : t -> bool

val get_did : t -> string
(** [get_did t] is the DID of the session.

    @raise Failure if there is no session. *)

val plc : t -> string

(** {1 Identity and services} *)

val resolve_handle : t -> string -> string
(** [resolve_handle t handle] is the DID of [handle]. A DID is returned as is.
*)

val get_client : t -> Xrpc.Client.t
(** [get_client t] is the session's PDS client, or an anonymous one. *)

val service_url : string -> string
(** [service_url s] is the HTTP(S) origin of a hostname or origin [s]. *)

val service_host : string -> string
val service_did : string -> string
(** [service_did s] is the [did:web] of the service at [s]. *)

val public_client : t -> service:string -> Xrpc.Client.t

val service_client :
  t ->
  service:string ->
  ?audience:string ->
  nsid:string ->
  unit ->
  Xrpc.Client.t
(** [service_client t ~service ?audience ~nsid ()] is a client for [service]
    carrying a fresh PDS-issued token bound to [nsid], valid for 60 seconds.
    The token's audience is [audience], or the [did:web] of [service]. Knots
    accept each token once, so obtain one per request. *)

val handle_of_did : t -> string -> string option
(** [handle_of_did t did] is the handle that the DID document of [did] lists,
    if that handle resolves back to [did]. It is [None] when either lookup
    fails or they disagree. *)

val pds_of : t -> string -> string
(** [pds_of t did] is the PDS named in the DID document of [did]. *)

val encode : 'a Jsont.t -> 'a -> Jsont.json
val decode : 'a Jsont.t -> Jsont.json -> 'a
val now : unit -> string
val rkey_of_uri : string -> string

module Atproto = Atp_lexicon_atproto.Com.Atproto
module Lex = Atp_lexicon_tangled.Org.Tangled
module Legacy = Atp_lexicon_tangled.Sh.Tangled

(** {1 Records} *)

val list_records :
  t ->
  ?service:string ->
  did:string ->
  collection:string ->
  unit ->
  Atproto.Repo.ListRecords.record list
(** [list_records t ?service ~did ~collection ()] reads every page of a
    collection, rejecting repeated cursors. Records are read from [service], or
    else from the configured PDS. *)

val get_record :
  t ->
  ?service:string ->
  did:string ->
  collection:string ->
  rkey:string ->
  unit ->
  Atproto.Repo.GetRecord.output option
(** [get_record] reads one record from the server {!list_records} would use. *)

val create_record :
  t ->
  collection:string ->
  ?rkey:string ->
  Jsont.json ->
  Atproto.Repo.CreateRecord.output
(** [create_record t ~collection ?rkey record] writes to the session's PDS,
    setting [record]'s [$type] to [collection]. *)

val put_record :
  t ->
  collection:string ->
  rkey:string ->
  swap_record:string option ->
  Jsont.json ->
  Atproto.Repo.PutRecord.output

val delete_record :
  t -> collection:string -> rkey:string -> ?swap_record:string -> unit -> unit

val list_declarations :
  t -> ?did:string -> unit -> (string * Lex.Repo.Declaration.main) list
(** [list_declarations t ?did ()] is the repositories [did] declares, as
    [(slug, declaration)]. This and the other typed readers below read another
    account's records from the PDS its DID document names. *)

val list_ssh_keys :
  t -> ?did:string -> unit -> (string * Lex.Key.SshKey.main) list

val list_stars : t -> ?did:string -> unit -> (string * Lex.Feed.Star.main) list
val get_profile : t -> did:string -> Lex.Actor.Profile.main option

(** {1 Repositories} *)

type repository = {
  did : string;  (** The repository DID. *)
  owner : string;  (** The owner's DID. *)
  name : string;
  knot : string;  (** The origin of the knot hosting the repository. *)
  manifest : Lex.Repo.Manifest.main;
  manifest_value : Jsont.json;
  manifest_cid : string option;
}

val resolve_repo : t -> ?knot:string -> string -> repository
(** [resolve_repo t ?knot spec] resolves [owner/name], a bare [name] of the
    session's account, a declaration AT-URI or a repository DID. The knot is
    the one in the repository's DID document unless [knot] overrides it.

    @raise Failure
      if the owner's declaration and the knot's manifest disagree. *)

val create_repo :
  t ->
  name:string ->
  knot:string ->
  ?audience:string ->
  ?description:string ->
  ?default_branch:string ->
  ?spindle:string ->
  ?source:repository ->
  ?import:string ->
  unit ->
  repository
(** [create_repo t ~name ~knot ()] asks [knot] to mint a repository, then
    declares it on the session's PDS. [source] makes it a fork. [import] is the
    clone URL of a repository for the knot to import, which it does after
    returning. If the declaration fails the error names the minted DID, so that
    the declaration can be written by hand. *)

val delete_repo : t -> ?audience:string -> repository -> unit
(** [delete_repo t repository] removes the owner's declaration, then asks the
    knot to delete the repository. *)

val git_url : repository -> string
(** [git_url r] is the HTTPS clone URL by repository DID. *)

val clone : t -> repo:string -> ?knot:string -> ?dir:string -> unit -> unit

val set_spindle : t -> repository -> string option -> unit
(** [set_spindle t r spindle] assigns the spindle at hostname [spindle] to [r],
    or removes the assignment, by rewriting the manifest on the knot. *)

val collaborators : t -> repository -> string list
(** [collaborators t r] is the DIDs granted collaboration on [r]. *)

val add_collaborator : t -> repository -> string -> unit
val remove_collaborator : t -> repository -> string -> unit

val repo_create_record :
  t ->
  repository ->
  collection:string ->
  ?rkey:string ->
  Jsont.json ->
  Atproto.Repo.CreateRecord.output
(** [repo_create_record t r ~collection record] writes [record] into the
    repository account of [r] through its knot. The knot mints the key of
    tickets, comments and reactions, and refuses one given by the caller. The
    session then attests the record in its own account, as the author. A failed
    attestation is logged as a warning, since the record exists. *)

(** {1 Knots} *)

val knot_members : t -> knot:string -> string list

val add_knot_member : t -> knot:string -> ?audience:string -> string -> unit
(** [add_knot_member t ~knot did] grants [did] membership of [knot]. The
    session must be a knot admin. *)

val remove_knot_member : t -> knot:string -> ?audience:string -> string -> unit

(** {1 Issues and pull requests}

    Both are [org.tangled.track.ticket] records. A pull request has a target
    branch. *)

val tickets : t -> repository -> (string * Lex.Track.Ticket.main) list
val ticket : t -> repository -> string -> Lex.Track.Ticket.main option

val open_ticket :
  t ->
  repository ->
  title:string ->
  ?body:string ->
  ?target_branch:string ->
  ?labels:string list ->
  unit ->
  Atproto.Repo.CreateRecord.output
(** [open_ticket t r ~title ()] opens a ticket in [r]. A [target_branch] makes
    it a pull request. [labels] are the keys of labels defined in [r]. *)

val set_ticket_state : t -> repository -> rkey:string -> string -> unit
(** [set_ticket_state t r ~rkey state] sets a ticket to [open], [closed] or
    [completed]. Only its author or a maintainer may. *)

val submit_patch :
  t ->
  repository ->
  ticket:string ->
  base:string ->
  head:string ->
  ?source:string ->
  ?branch:string ->
  unit ->
  Lex.Review.SubmitPatch.output
(** [submit_patch t r ~ticket ~base ~head ()] adds a revision to pull request
    [ticket]. The knot fetches [head] from repository [source], [r] by
    default, optionally from [branch]. *)

(** {1 Artifacts} *)

val list_artifacts :
  t -> repository -> (string * Lex.Repo.Artifact.main) list
(** [list_artifacts t r] is the release artifacts in the repository account of
    [r], as [(rkey, artifact)]. Records that do not decode are left out. *)

val list_legacy_artifacts :
  t -> did:string -> repo:string -> (string * Legacy.Repo.Artifact.main) list
(** [list_legacy_artifacts t ~did ~repo] is the [sh.tangled.repo.artifact]
    records that [did] attached to its legacy repository called [repo]. A
    repository is named by the [name] of its [sh.tangled.repo] record, or by
    its record key. An artifact is left out if its repository record cannot be
    read or decoded, or if it points at a repository of another account. *)

val artifact_version : string -> string option
(** [artifact_version name] is the version in the file name [name] of a release
    artifact, which is written [package-version.tbz]. The version starts at the
    first dash that is followed by a digit. It is [None] if [name] is not a
    known archive or has no version. *)

(** {1 Spindles} *)

val spindle_url : t -> string -> string
(** [spindle_url t s] is the origin of spindle [s], a DID or hostname. A DID is
    resolved to its [#tangled_spindle] service. *)

val spindle_of : t -> ?spindle:string -> repository -> string
(** [spindle_of t ?spindle r] is the origin of [spindle], or of the spindle in
    the manifest of [r]. *)

val query_pipelines :
  t ->
  spindle:string ->
  repo:string ->
  ?commits:string list ->
  ?triggers:string list ->
  ?limit:int ->
  ?cursor:string ->
  unit ->
  Lex.Ci.QueryPipelines.output
(** [triggers] are trigger NSIDs such as [org.tangled.event.push]. *)

val get_pipeline :
  t -> spindle:string -> repo:string -> pipeline:string -> Lex.Ci.Defs.pipeline

val workflow_runs : Lex.Ci.Defs.pipeline -> Lex.Ci.Defs.workflow_run list

val trigger_pipeline :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  commit:string ->
  ?ref_:string ->
  ?workflows:string list ->
  ?inputs:(string * string) list ->
  unit ->
  Lex.Event.NotifyEvent.ci_output
(** [trigger_pipeline] sends a manual trigger through
    [org.tangled.event.notifyEvent]. The session must be able to push to
    [repo]. *)

val cancel_pipeline :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  pipeline:string ->
  ?workflows:string list ->
  unit ->
  unit

val approve_pipeline :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  pipeline:string ->
  unit ->
  unit

val retry_pipeline :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  pipeline:string ->
  ?workflows:string list ->
  unit ->
  string
(** [retry_pipeline] is the ID of the new pipeline. *)

val workflow_definitions :
  t ->
  spindle:string ->
  repo:string ->
  ?ref_:string ->
  unit ->
  Lex.Ci.ListWorkflowDefinitions.output

val allow_list : t -> spindle:string -> ?audience:string -> unit -> Jsont.json

val allow_list_change :
  t -> spindle:string -> ?audience:string -> add:bool -> string -> unit
(** [allow_list_change t ~spindle ~add did] admits [did] to the spindle, or
    withdraws it. The session must be a spindle admin. *)

(** {1 Git}

    Reads of a repository's Git data, served by its knot. *)

val branches :
  t ->
  repository ->
  ?limit:int ->
  ?cursor:string ->
  unit ->
  Lex.Git.ListBranches.output

val branch : t -> repository -> string -> Lex.Git.GetBranch.output option
(** [branch t r name] is the branch [name] of [r], or [None] if [r] has no such
    branch. *)

val default_branch : t -> repository -> string option

val tags :
  t ->
  repository ->
  ?limit:int ->
  ?cursor:string ->
  unit ->
  Lex.Git.ListTags.output

val commits :
  t ->
  repository ->
  ref_:string ->
  ?path:string ->
  ?limit:int ->
  ?cursor:string ->
  unit ->
  Lex.Git.ListCommits.output
(** [commits t r ~ref_ ()] is the history from [ref_], newest first. The cursor
    of the next page is the last commit of this one. *)

val tree :
  t ->
  repository ->
  ?ref_:string ->
  ?path:string ->
  unit ->
  Lex.Git.GetTree.output

val blob : t -> repository -> ?ref_:string -> path:string -> unit -> string
(** [blob t r ~path ()] is the content of the file [path], at [ref_] or the
    default branch. *)

val diff :
  t ->
  repository ->
  base:string ->
  head:string ->
  ?paths:string list ->
  unit ->
  Lex.Git.GetDiff.output
(** [diff t r ~base ~head ()] is the change from [base] to [head]. Each file
    names its blobs and the novel lines of each side, not their content. *)

val delete_branch : t -> repository -> string -> unit
val set_default_branch : t -> repository -> string -> unit

val import_status : t -> repository -> Lex.Repo.GetImportStatus.output
(** [import_status t r] is how far the knot got importing [r] from its source.
    Its status is absent if [r] was not imported. *)

(** {1 Record history} *)

val record_versions :
  t -> repository -> uri:string -> Lex.Repo.ListRecordVersions.version list
(** [record_versions t r ~uri] is every version of the record [uri] that the
    knot of [r] holds, oldest first. *)

(** {1 Comments} *)

type comment = {
  rkey : string;
  author : string option;  (** The DID the knot names as its writer. *)
  comment : Lex.Feed.Comment.main;
}

val comments : t -> repository -> ticket:string -> comment list
(** [comments t r ~ticket] is the comments on ticket [ticket], oldest first. *)

val add_comment :
  t -> repository -> ticket:string -> string -> Atproto.Repo.CreateRecord.output
(** [add_comment t r ~ticket body] comments [body], in Markdown, on the current
    version of ticket [ticket]. *)

val ticket_uri : repository -> string -> string

(** {1 Ticket edits and revisions} *)

val ticket_author : t -> repository -> string -> string option

val edit_ticket :
  t ->
  repository ->
  rkey:string ->
  ?title:string ->
  ?body:string ->
  ?assign:string list ->
  ?unassign:string list ->
  ?label:string list ->
  ?unlabel:string list ->
  unit ->
  unit
(** [edit_ticket t r ~rkey ()] changes the fields given and keeps the rest. An
    empty [body] removes it. [assign] and [unassign] are DIDs. [label] and
    [unlabel] are label keys. *)

val ticket_labels : Lex.Track.Ticket.main -> string list
(** [ticket_labels ticket] is the keys of the labels on [ticket]. *)

(** {1 Labels}

    A label is defined once per repository, by its owner, and applied to
    tickets by key. *)

type label = { key : string; definition : Lex.Label.Definition.main }

val labels : t -> repository -> label list
(** [labels t r] is the labels defined in [r]. *)

val label_key : string -> string
(** [label_key name] is the key a label called [name] is stored under: its
    letters and digits in lower case, with other runs replaced by a hyphen.
    Raises [Invalid_argument] if [name] has no letter or digit. *)

val create_label :
  t -> repository -> name:string -> ?color:string -> unit -> string
(** [create_label t r ~name ?color ()] defines a label and is its key. [color]
    is a hex colour such as [#e11d48]. Only the owner of [r] may. The key of a
    deleted label cannot be used again. *)

val edit_label :
  t -> repository -> key:string -> ?name:string -> ?color:string -> unit -> unit
(** [edit_label t r ~key ()] renames or recolours a label. Tickets keep it. *)

val delete_label : t -> repository -> key:string -> unit
(** [delete_label t r ~key] deletes a label. *)

val patches : t -> repository -> ticket:string -> Lex.Review.Patch.main list
(** [patches t r ~ticket] is the revisions of pull request [ticket], oldest
    first. *)

val oid_of : Jsont.json -> string
(** [oid_of v] is the object ID in an [org.tangled.git.oid] value. *)

(** {1 Releases}

    A release artifact is a file attached to a commit, usually that of a tag.
    Its bytes are a blob in the repository account. *)

val upload_artifact :
  t ->
  repository ->
  oid:string ->
  name:string ->
  ?content_type:string ->
  string ->
  Atproto.Repo.CreateRecord.output
(** [upload_artifact t r ~oid ~name data] attaches [data] to commit [oid] as the
    file [name].

    @raise Invalid_argument if [data] exceeds 50 MiB. *)

val download_artifact : t -> repository -> Lex.Repo.Artifact.main -> string
val delete_artifact : t -> repository -> rkey:string -> unit

(** {1 Service administration}

    Knots and spindles admit members through invite codes. Each call needs the
    session to be an admin of [service], except [redeem_invite]. *)

val invite_code :
  t ->
  service:string ->
  ?audience:string ->
  ?uses:int ->
  ?for_account:string ->
  unit ->
  string
(** [invite_code t ~service ()] is a new code that admits [uses] accounts, 1 by
    default, or only [for_account]. *)

val redeem_invite : t -> service:string -> ?audience:string -> string -> unit
(** [redeem_invite t ~service code] makes the session a member of [service]. *)

val knot_bans : t -> knot:string -> string list
val ban_from_knot : t -> knot:string -> ?audience:string -> string -> unit
val unban_from_knot : t -> knot:string -> ?audience:string -> string -> unit

(** {1 Spindle settings}

    Secrets and webhooks belong to a repository on a spindle. The session must
    be able to push to [repo]. *)

val secrets :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  unit ->
  Lex.Secret.ListSecrets.secret list

val add_secret :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  key:string ->
  string ->
  unit

val remove_secret :
  t -> spindle:string -> ?audience:string -> repo:string -> string -> unit

val webhooks :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  unit ->
  Lex.Webhook.Defs.webhook list

val create_webhook :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  url:string ->
  events:string list ->
  ?secret:string ->
  unit ->
  string
(** [create_webhook] is the ID of the new, active webhook. *)

val update_webhook :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  webhook:string ->
  ?url:string ->
  ?events:string list ->
  ?secret:string ->
  ?active:bool ->
  unit ->
  unit

val delete_webhook :
  t -> spindle:string -> ?audience:string -> repo:string -> string -> unit

val webhook_deliveries :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  webhook:string ->
  ?limit:int ->
  unit ->
  Lex.Webhook.Defs.delivery list

val redeliver_webhook :
  t ->
  spindle:string ->
  ?audience:string ->
  repo:string ->
  webhook:string ->
  string ->
  unit

val workflow_definition :
  t -> spindle:string -> repo:string -> string -> Lex.Ci.Defs.workflow

(** {1 Spindle moderation}

    Each call needs the session to be an admin of [spindle]. These methods are
    under [org.tangled.temp], so they may change. *)

val spindle_bans : t -> spindle:string -> ?audience:string -> unit -> Jsont.json

val spindle_ban :
  t -> spindle:string -> ?audience:string -> ban:bool -> string -> unit
(** [spindle_ban t ~spindle ~ban did] bans the repository or owner [did], wiping
    its state on the spindle, or lifts the ban. *)

val quotas : t -> spindle:string -> ?audience:string -> unit -> Jsont.json

val quota_usage :
  t -> spindle:string -> ?audience:string -> ?did:string -> unit -> Jsont.json

val set_quota :
  t ->
  spindle:string ->
  ?audience:string ->
  did:string ->
  resource:string ->
  int option ->
  unit
(** [set_quota t ~spindle ~did ~resource limit] overrides the quota of [did] for
    [resource], such as [workflows] or [memory_mib]. [None] is unlimited. *)

val unset_quota :
  t ->
  spindle:string ->
  ?audience:string ->
  did:string ->
  resource:string ->
  unit ->
  unit
