(** Exact-DOI abstract lookups with retained responses and audit.

    OpenAlex is the provider. Single-work reads are free of its credit
    budget, but a list query filtered on up to 100 DOIs costs one credit
    and returns them all in one round trip, so {!lookup_many} batches that
    way and reports when the daily budget or rate limit refuses it. *)
type read
type write
type 'a store
val open_read : sw:Eio.Switch.t -> _ Eio.Path.t -> read store
val open_write : ?audit:Taposaur_audit.session -> sw:Eio.Switch.t ->
  _ Eio.Path.t -> write store
val find : 'a store -> Taposaur.Doi.t -> Taposaur.Abstract_evidence.t option
(** [find store doi] is accepted evidence, if any. *)
val attempted : 'a store -> Taposaur.Doi.t -> bool
(** [attempted store doi] is whether any lookup of [doi] was recorded,
    accepted or not. *)
val record : 'a store -> sha256:string -> string option
(** [record store ~sha256] is the retained response body. *)
val count : 'a store -> int
val iter : 'a store -> (Taposaur.Abstract_evidence.t -> unit) -> unit
(** [iter store f] visits accepted evidence in DOI order. *)

type client
val client : ?audit:Taposaur_audit.session -> ?timeout:float ->
  ?api_key:string -> ?endpoint:string ->
  clock:_ Eio.Time.clock -> _ Fetch.t -> client
(** [client ~clock http] queries [endpoint] (default
    [https://api.openalex.org]) with a deadline per request (default 30 s)
    and an 8 MiB response limit. [api_key] is sent as the [api_key] query
    parameter; the caller keeps it out of logs. Pace [http] with
    {!Fetch.with_limits}: OpenAlex allows 10 requests a second without a
    key and 30 with one. *)
type refusal = Rate_limited | Budget | Http of int | Transport of string
(** Why a lookup returned nothing: HTTP 429, a 402 or 403 budget
    refusal, another status, or a transport or decoding failure. *)
val refusal_name : refusal -> string
val batches : Taposaur.Doi.t list -> Taposaur.Doi.t list list
(** [batches dois] splits a list, order kept, into lookups of at most 100
    DOIs whose filter stays under OpenAlex's request URL limit. *)
val lookup_many : client -> write store -> Taposaur.Doi.t list ->
  ((Taposaur.Doi.t * Taposaur.Abstract_evidence.t) list, refusal) result
(** [lookup_many client store dois] fetches up to 100 DOIs that have no
    accepted evidence in one filtered list query. The response body is
    retained once; each DOI gets an attempt row naming the decision:
    [accepted], [not_found] (absent from the results), [missing_abstract],
    or a failure. Accepted evidence is stored and returned. A refusal
    stores nothing for rate limits, budgets, or another HTTP status on a
    multi-DOI batch, so those DOIs can be retried, singly if need be; a
    single DOI's HTTP failure is recorded against it. Raises
    [Invalid_argument] above 100 DOIs or past the URL limit. *)
val lookup : client -> write store -> Taposaur.Doi.t ->
  (Taposaur.Abstract_evidence.t option, refusal) result
(** [lookup client store doi] is retained evidence or one single-work
    read, which OpenAlex does not charge against the credit budget. A 404
    is recorded as [not_found] and returned as [Ok None]. *)
val decode : provider:string -> endpoint:string -> doi:Taposaur.Doi.t ->
  string -> (Taposaur.Abstract_evidence.t, string) result
(** Pure validation of one OpenAlex work: the DOI must match and the
    inverted index must reconstruct to a nonblank abstract. *)
val reconstruct : int list Jsont.String_map.t -> string option
(** [reconstruct index] rebuilds abstract text from an OpenAlex inverted
    index, or [None] when it is empty or absurdly long. *)
val default_path : unit -> string
val open_configured : sw:Eio.Switch.t -> fs:_ Eio.Path.t -> read store option
(** TAPOSAUR_ABSTRACTS_DB, or taposaur-abstracts.db when present. An
    explicitly configured missing database is an error. No network
    requests. *)
