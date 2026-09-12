(** HTTPS CardDAV discovery, conservative duplicate detection and seeding.
    Credentials and the underlying narrowed Fetch capability are private. *)

type t

type card = {
  uid : string;
  href : string;
  etag : string option;
  data : string;
  props : Mapping.property list;
}

val default_server : string
val limit : int

val make :
  fetch:_ Fetch.t ->
  root:string ->
  username:string ->
  password:string ->
  readonly:bool ->
  t
(** [readonly] permits only GET, HEAD, OPTIONS, PROPFIND and REPORT, including
    at the Fetch capability boundary. All requests and redirects are confined to
    the configured HTTPS origin and port. *)

val is_readonly : t -> bool
val password : string -> string
val validate : t -> string -> unit

val request :
  t ->
  ?body:string ->
  ?headers:(string * string) list ->
  string ->
  string ->
  int * Http.Header.t * string

val responses : string -> string -> (string * Httpz_dav.element list) list
val discover : ?collection:string -> t -> Common.value

val fetch_all : t -> Common.value -> card list
(** A failed, truncated, ambiguous or incomplete listing raises [Common.Error].
*)

val normalize_name : string -> string

val assert_contact : string -> string -> Common.value -> unit
(** Require complete source reconstruction and retention of every emitted
    property, including display fallbacks. Extra remote properties survive. *)

val plan :
  string -> Common.value -> Common.value -> card list -> Common.value list

val strong_etag : Http.Header.t -> string

val seed_one : t -> string -> Common.value -> string -> Common.value
(** Conditional creation followed by verified GET. Never overwrites. *)

val inspect :
  ?collection:string ->
  apply:bool ->
  dav:t ->
  bundle:string ->
  username:string ->
  report:string ->
  unit ->
  Common.value
(** Save a full snapshot and plan. With [apply], create each unambiguous new
    contact and verify it before starting the next. Reports are new private
    directories; failures remain recorded for reconciliation. *)
