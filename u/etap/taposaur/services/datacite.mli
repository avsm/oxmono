(** A local {{:https://datacite.org/}DataCite} metadata lookup.

    DataCite registers the DOIs Crossref does not, which is most of
    what a repository or a data archive mints. The service here is the
    site's own index of the
    {{:https://support.datacite.org/docs/datacite-public-data-file}public
    data file}, not [api.datacite.org], so it is self-hosted and no
    credential is involved: {!client} mints its capability from a
    [~server] base URL rather than from a key. A trailing ["/"] on it
    is ignored. The base URL includes the mount point, as in
    [http://localhost:8090/api/datacite], and the operations build
    [/lookup] and [/stats] under it.

    A lookup answers a Crossref-shaped subset of the record, so
    {!Glutton.Record.jsont} reads it unchanged: [DOI], [type],
    [publisher], [title], [container-title], [issued] and [URL], with a
    member absent rather than null when unknown. [type] is DataCite's
    [resourceTypeGeneral] mapped to Crossref's vocabulary when the
    record was ingested, so a preprint reads as ["posted-content"] and
    a report as ["report"]. Matching is case-insensitive, and a miss is
    a 404 carrying a JSON [message], returned as data per
    {!Fetch.fetch}. *)

type t
(** The authority to call one DataCite lookup service, minted by
    {!client} with the server address inside it. It is abstract, so
    {!client} is the only way to make one and another service's
    capability cannot stand in for it. *)

val client : server:string -> _ Fetch.t -> t
(** [client ~server t] is [t] bound to the DataCite lookup rooted at
    [server], whose trailing ["/"] is ignored. The operations only ever
    build URLs under [server], but [t] itself is not restricted to that
    origin, so narrow it with {!Fetch.restrict} before minting if that
    matters. Every wrapper goes on [t], as the result is not a client.

    @raise Invalid_argument if [server] is not an http or https URL, or
      carries a query. *)


val server : t -> string
(** [server t] is the base URL [t] was minted for, without its
    trailing ["/"]. *)

val lookup_url : server:string -> doi:Taposaur.Doi.t -> string
(** [lookup_url ~server ~doi] is the lookup query for [doi] on the
    service rooted at [server]. The identifier travels as a query
    parameter, so its ["/"] is not escaped. *)

val stats_url : server:string -> string
(** [stats_url ~server] is the service report endpoint. *)

val by_doi : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [by_doi ~sw t doi] fetches [doi]'s record as JSON. A DOI the store
    does not hold is a 404, whatever its case. Read the answer with
    {!Glutton.Record.jsont}, which is the same codec the biblio server's
    answers go through. *)

val stats : sw:Eio.Switch.t -> t -> Fetch.response
(** [stats ~sw t] fetches the service's count of the records it holds
    and how they break down by [type], which is how to tell an empty or
    half-loaded instance from a complete one. *)
