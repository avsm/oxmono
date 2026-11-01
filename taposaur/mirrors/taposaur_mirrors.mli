(** Configured, bounded exact-DOI queries with retained evidence and audit. *)
type read
type write
type 'a store
type provider = Glutton of string | Datacite of string | Datacite_clickhouse of string
type client
val client : ?audit:Taposaur_audit.session -> ?timeout:float ->
  clock:_ Eio.Time.clock -> providers:provider list -> _ Fetch.t -> client
val open_read : sw:Eio.Switch.t -> _ Eio.Path.t -> read store
val open_write : ?audit:Taposaur_audit.session -> sw:Eio.Switch.t ->
  _ Eio.Path.t -> write store
val find : 'a store -> Taposaur.Doi.t -> Taposaur.Publisher_evidence.t option
val record : 'a store -> sha256:string -> string option
val lookup : client -> write store -> Taposaur.Doi.t ->
  Taposaur.Publisher_evidence.t option
(** Return retained evidence or try configured providers in order. Exact DOI,
    nonblank publisher and successful parsing are required. Failed/mismatched
    responses are retained for diagnosis, but never selected. Errors do not
    become cached permanent misses. Bodies are bounded to 2 MiB. *)
val import_csv : write store -> path:string -> string -> unit
(** Retain a CSV as fallback evidence. Requires doi/publisher columns. Conflicting
    publisher values for a DOI are excluded. Does not select rows until lookup. *)
val fallback : write store -> Taposaur.Doi.t -> Taposaur.Publisher_evidence.t option
(** Select unambiguous imported CSV evidence only when no mirror result exists. *)
val decode : provider:string -> endpoint:string -> doi:Taposaur.Doi.t ->
  string -> (Taposaur.Publisher_evidence.t, string) result
(** Pure exact-DOI validation of Crossref-shaped JSON, used by all providers. *)
val default_path : unit -> string
val open_configured : sw:Eio.Switch.t -> fs:_ Eio.Path.t -> read store option
(** TAPOSAUR_MIRRORS_DB, or taposaur-mirrors.db when present. An explicitly
    configured missing database is an error. No network requests. *)
