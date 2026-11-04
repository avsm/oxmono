(** Per-repository and per-account settings: secrets, bans, invite codes and
    quotas. Every value is kept in the spindle's SQLite store. *)

type t

type defaults = { repo : (string * int) list; user : (string * int) list }
(** Default limits by resource, for repositories and for their owners. A missing
    or zero limit is unlimited. *)

val v : store:Store.t -> now:(unit -> float) -> defaults:defaults -> t

val note_owner : t -> repo:string -> owner:string -> unit
(** [note_owner t ~repo ~owner] records whose repository [repo] is, so that
    banning the owner reaches it. *)

val owner_of : t -> string -> string option
val repos_of : t -> string -> string list

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

val check_secret : key:string -> value:string option -> unit
(** [check_secret ~key ~value] applies the reference spindle's limits: a key is
    a shell identifier of at most 50 characters and a value 1 to 4096 bytes. *)

(** {1 Bans} *)

val banned : t -> string -> bool
val bans : t -> Jsont.json
val ban : t -> string -> unit

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
val limits : t -> Jsont.json

val add_minutes : t -> string list -> int -> unit
(** [add_minutes t dids minutes] charges CI minutes this month to each DID. *)

val minutes : t -> string -> int
(** [minutes t did] is the CI minutes [did] has used this calendar month. *)

val wipe : t -> string -> unit
(** [wipe t repo] removes [repo]'s secrets, quota overrides and minutes. *)
