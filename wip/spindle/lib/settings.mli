(** Per-repository and per-account settings: secrets, bans, invite codes and
    quotas. Every value is kept in the spindle's SQLite store. *)

type t

type defaults = { repo : (string * int) list; user : (string * int) list }
(** Default limits by resource, for repositories and for their owners. A missing
    or zero limit is unlimited. *)

val v : store:Store.t -> now:(unit -> float) -> defaults:defaults -> t
(** [v ~store ~now ~defaults] keeps settings in [store], with [now] the wall
    clock in seconds, for expiry and calendar months. *)

val note_owner : t -> repo:string -> owner:string -> unit
(** [note_owner t ~repo ~owner] records whose repository [repo] is, so that
    banning the owner reaches it. *)

val owner_of : t -> string -> string option
(** [owner_of t repo] is the owner {!note_owner} recorded for [repo]. *)

val repos_of : t -> string -> string list
(** [repos_of t owner] is the repositories recorded as [owner]'s. *)

(** {1 Secrets} *)

val secrets : t -> string -> (string * string) list
(** [secrets t repo] is each secret's key and value. *)

val list_secrets : t -> string -> Jsont.json
(** [list_secrets t repo] is the [org.tangled.secret.listSecrets] answer, which
    names the secrets without their values. *)

val add_secret : t -> repo:string -> by:string -> string -> string -> unit
(** [add_secret t ~repo ~by key value] adds a secret. An existing key answers
    [SecretAlreadyExists]. *)

val remove_secret : t -> repo:string -> string -> unit
(** [remove_secret t ~repo key] removes a secret. An absent key answers
    [SecretNotFound]. *)

val check_secret : key:string -> value:string option -> unit
(** [check_secret ~key ~value] applies the reference spindle's limits: a key is
    a shell identifier of at most 50 characters and a value 1 to 4096 bytes. *)

(** {1 Bans} *)

val banned : t -> string -> bool
(** [banned t did] holds when [did] is banned. *)

val bans : t -> Jsont.json
(** [bans t] is the [org.tangled.temp.spindle.moderation] list of bans. *)

val ban : t -> string -> unit
(** [ban t did] bans [did], which may then not use the spindle. *)

val unban : t -> string -> unit
(** [unban t did] lifts a ban. An absent ban answers [BanNotFound]. *)

(** {1 Invite codes} *)

val random_bytes : int -> string
(** [random_bytes n] is [n] bytes from the system's random source. *)

val invite :
  t -> by:string -> uses:int -> for_account:string option -> Jsont.json
(** [invite t ~by ~uses ~for_account] is a new code, valid for two days, that
    [uses] accounts may redeem, or only [for_account]. *)

val redeem : t -> string -> actor:string -> unit
(** [redeem t code ~actor] spends one use of [code] for [actor]. An unknown,
    spent, expired or misaddressed code answers [InviteNotFound]. *)

(** {1 Quotas} *)

type scope = Repo | User

val limit : t -> scope -> string -> string -> int option
(** [limit t scope did resource] is the limit for [did] in [scope]: its
    override, else the default. [None] is unlimited. *)

val set_limit : t -> string -> string -> int option -> unit
(** [set_limit t did resource limit] overrides a limit. [None] is unlimited. *)

val unset_limit : t -> string -> string -> unit
(** [unset_limit t did resource] removes an override, so the default applies. *)

val limits : t -> Jsont.json
(** [limits t] lists every override, with an unlimited one as [-1]. *)

val add_minutes : t -> string list -> int -> unit
(** [add_minutes t dids minutes] charges CI minutes this month to each DID. *)

val minutes : t -> string -> int
(** [minutes t did] is the CI minutes [did] has used this calendar month. *)

val wipe : t -> string -> unit
(** [wipe t repo] removes [repo]'s secrets, quota overrides and minutes. *)
