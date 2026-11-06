(** Repositories this spindle serves, and who may use it.

    A repository is served when the [org.tangled.repo.manifest] on its knot
    names this spindle, the owner's [org.tangled.repo.declaration] names the
    repository back, and the owner is admitted. *)

type repo = {
  did : string;
  owner : string;
  name : string;
  knot : string;  (** Knot origin. Empty for a static repository. *)
  source : string;  (** Git URL or local path to clone. *)
  created : string;  (** When the manifest was created, if known. *)
}

type admission =
  | Open  (** Any owner may use the spindle. *)
  | Closed  (** Only admins and allow-listed owners may. *)

type t

val v :
  store:Store.t ->
  settings:Settings.t ->
  network:Network.t ->
  owner:string ->
  ?admins:string list ->
  hostname:string ->
  admission:admission ->
  static:repo option ->
  ttl:float ->
  now:(unit -> float) ->
  unit ->
  t
(** [v ~store ~network ~owner ~hostname ~admission ~static ~ttl ~now ()] is a
    catalog for the spindle at [hostname]. [owner] and [admins] administer it.
    [static] is always served. Acceptance is cached for [ttl] seconds. *)

val slug : string -> string
(** [slug name] is the record key of a repository called [name]: [name] in
    lowercase. *)

val self : t -> string
(** [self t] is the spindle's [did:web]. *)

val web_did : string -> string
(** [web_did host] is the [did:web] of [host], encoding a port as [%3A]. *)

val admin : t -> string -> bool
(** [admin t did] holds for the owner and the admins. A ban is checked apart, by
    {!admitted} and by the methods that authenticate callers. *)

val admitted : t -> string -> bool
(** [admitted t owner] holds when [owner]'s repositories may run here. A ban
    overrides admins, open admission and the allow-list. *)

val on_rename : t -> (repo -> old_name:string -> unit) -> unit
(** [on_rename t f] calls [f] when a served repository's name changes. *)

val knot_origin : t -> string -> string
(** [knot_origin t did] is the origin of the knot that repository [did]'s DID
    document names. *)

val allowed : t -> string list
(** [allowed t] is the allow-listed owners. *)

val allow : t -> string -> unit
(** [allow t did] admits owner [did] and drops cached acceptance. *)

val disallow : t -> string -> unit
(** [disallow t did] withdraws [did]'s admission and drops cached acceptance. *)

val managed : t -> string -> repo option
(** [managed t did] is the served repository [did], or [None]. It reads the knot
    and the owner's PDS unless a cached answer is fresh. A banned repository or
    owner is not served. Network failures raise. *)

val forget : t -> string -> unit
(** [forget t did] drops the cached acceptance of [did]. *)

val cached : t -> string -> bool
(** [cached t id] holds when {!managed} would answer for [id] without asking
    the network. *)

val known : t -> string -> bool
(** [known t id] holds for the static repository and for repositories this
    spindle has accepted before. It reads only local state. *)

val ref_contains : t -> repo -> ref_:string -> commit:string -> bool
(** [ref_contains t repo ~ref_ ~commit] holds when [commit] is among the 100
    newest commits of [ref_] that [repo]'s knot lists, as deep as the reference
    spindle looks. A static repository's refs are not checked. *)

val names_self : t -> Jsont.json -> bool
(** [names_self t manifest] holds when the [org.tangled.repo.manifest] record
    [manifest] names this spindle. *)

val deleted : t -> string -> bool
(** [deleted t id] holds when the knot of repository [id] reports it deleted
    through [com.atproto.sync.getRepoStatus]. *)

val current : t -> repo -> bool
(** [current t repo] checks that [repo] is still served by the same owner and
    knot. *)

val knot_did : t -> string -> string
(** [knot_did t did] is the [did:web] of the knot that the repository DID
    document names. *)

val locate : t -> string -> string
(** [locate t did] is the clone URL of any repository, served or not. *)

val authorized : t -> repo -> string -> bool
(** [authorized t repo actor] holds for the owner and for collaborators the knot
    currently records. It asks the knot on every call. *)

val permitted : t -> repo -> string -> bool
(** [permitted t repo actor] holds when [actor] is the DID of [repo], which has
    a knot, or when [authorized t repo actor] holds. Tangled signs a
    repository's settings and pipeline requests with the repository's key, held
    by its knot. A static repository has no knot, so its DID is not accepted. *)

val pull_labels : t -> repo -> pull:string -> string list
(** [pull_labels t repo ~pull] is the keys of the labels on the pull request
    [pull], an AT-URI of a ticket of [repo], that its owner or a collaborator
    added. A pull request's author may label it too, so a label counts only when
    the version that last added it was made by someone [authorized] for [repo].
    It asks the knot for every version of the ticket and checks each one's
    content id. It is [[]] when anything fails. *)

val network : t -> Network.t
(** [network t] is the network [t] reads through. *)

type manifest = {
  name : string;
  owner : string;  (** The DID the manifest names as owner, unchecked. *)
  spindle : string option;  (** The spindle it names, if any. *)
}

val manifest : t -> string -> manifest option
(** [manifest t did] is what the knot's manifest of the repository [did] says,
    whether or not this spindle serves it. It asks the knot each time. It raises
    as {!managed} does. *)

val default_branch : t -> repo -> string option
(** [default_branch t repo] is the branch the knot's [org.tangled.git.refs]
    record names. *)

val default_head : t -> repo -> (string * string) option
(** [default_head t repo] is the default branch and the object ID at its head,
    from the knot's [org.tangled.git.refs] record, or [None] when the record,
    the branch or a valid object ID for it is missing. *)
