(** A {{:https://github.com/kermitt2/biblio-glutton}biblio-glutton}
    bibliographical lookup and matching service.

    The service is self-hosted and no credential is involved, so
    {!client} mints its capability from a [~server] base URL rather
    than from a key. A trailing ["/"] on it is ignored. The lookups
    target [/service/lookup], which answers the complete Crossref
    record for a work along with its [pmid] and [pmcid]
    cross-identifiers. The open-access resolutions target
    [/service/oa], and {!data} targets [/service/data]. Identifiers
    travel as query parameters.

    A miss is a 404 carrying a JSON [message], returned as data per
    {!Fetch.fetch}. A request that names nothing to look up is a 400.
    Matching is conservative, so a candidate that fails the service's
    post-validation is reported as a miss rather than guessed at. *)

type t
(** The authority to call one biblio-glutton server, minted by
    {!client} with the server address inside it. It is abstract, so
    {!client} is the only way to make one and another service's
    capability cannot stand in for it. *)

val client : server:string -> _ Fetch.t -> t
(** [client ~server t] is [t] bound to the biblio-glutton server rooted
    at [server], whose trailing ["/"] is ignored. The operations only
    ever build URLs under [server], but [t] itself is not restricted to
    that origin, so narrow it with {!Fetch.restrict} before minting if
    that matters. Every wrapper goes on [t], as the result is not a
    client.

    @raise Invalid_argument if [server] is not an http or https URL, or
      carries a query. *)


val server : t -> string
(** [server t] is the base URL [t] was minted for, without its
    trailing ["/"]. *)

val lookup_url :
  server:string ->
  ?doi:Taposaur.Doi.t ->
  ?pmcid:string ->
  ?pmid:string ->
  ?pii:string ->
  ?istexid:string ->
  ?halid:string ->
  ?first_author:string ->
  ?atitle:string ->
  ?jtitle:string ->
  ?volume:string ->
  ?first_page:string ->
  ?year:string ->
  ?biblio:string ->
  ?parse_reference:bool ->
  unit ->
  string
(** [lookup_url ~server ()] is the lookup query on the server rooted at
    [server] built from whichever arguments are given, each
    percent-encoded. It combines parameters the operations below do not,
    for a client of your own with the authority to reach [server].

    The service tries the identifiers in the order [doi], [halid],
    [pmid], [pmcid], [pii], [istexid], and stops at the first one
    given.
    It does not fall through to the next on a miss. [pmcid] is
    normalised to the ["PMC"]-prefixed form the store is keyed by, so
    ["PMC4221854"] and ["4221854"] are the same request. With no
    identifier the metadata arguments ([first_author], [atitle],
    [jtitle], [volume], [first_page], [year]) or a raw [biblio]
    reference string drive the match instead. [first_author], [atitle]
    and [year] also serve as post-validation constraints on an
    identifier lookup. [parse_reference] (default [true] at the server)
    sends [biblio] through GROBID before matching. *)

val oa_url :
  server:string ->
  ?doi:Taposaur.Doi.t ->
  ?pmcid:string ->
  ?pmid:string ->
  ?pii:string ->
  unit ->
  string
(** [oa_url ~server ()] is the open-access resolution query for the
    identifier given. It needs an Unpaywall snapshot loaded into the
    server: without one every identifier answers a 404. *)

val data_url : server:string -> string
(** [data_url ~server] is the service report endpoint. *)

val by_doi :
  sw:Eio.Switch.t ->
  ?first_author:string ->
  ?atitle:string ->
  ?year:string ->
  t ->
  Taposaur.Doi.t ->
  Fetch.response
(** [by_doi ~sw t doi] fetches [doi]'s Crossref record as JSON. Any of
    [first_author], [atitle] and [year] given must agree with the record
    found, or the answer is a 404. *)

val by_pmid :
  sw:Eio.Switch.t ->
  ?first_author:string ->
  ?atitle:string ->
  ?year:string ->
  t ->
  string ->
  Fetch.response
(** [by_pmid ~sw t pmid] fetches the record for the PubMed identifier
    [pmid], as {!by_doi} does. *)

val by_pmcid :
  sw:Eio.Switch.t ->
  ?first_author:string ->
  ?atitle:string ->
  ?year:string ->
  t ->
  string ->
  Fetch.response
(** [by_pmcid ~sw t pmcid] fetches the record for the PubMed Central
    identifier [pmcid], with or without its ["PMC"] prefix, as {!by_doi}
    does. *)

val by_pii :
  sw:Eio.Switch.t ->
  ?first_author:string ->
  ?atitle:string ->
  ?year:string ->
  t ->
  string ->
  Fetch.response
(** [by_pii ~sw t pii] fetches the record for the Elsevier publisher
    item identifier [pii], as {!by_doi} does. Give it in the compact
    form its URLs carry, ["S0378112719300000"] rather than
    ["S0378-1127(19)30000-0"].

    This resolves through the server's ISTEX store, which is loaded
    separately from the Crossref one: without it every PII answers a
    404, even for a work the server holds under its DOI. {!data}
    reports the store's size as [istex_pii2ids]. *)

val by_metadata :
  sw:Eio.Switch.t ->
  ?first_author:string ->
  ?atitle:string ->
  ?jtitle:string ->
  ?volume:string ->
  ?first_page:string ->
  ?year:string ->
  t ->
  unit ->
  Fetch.response
(** [by_metadata ~sw t ()] matches a work from the fields given. Give
    every field to hand, as a candidate with less signal is likelier to
    fail post-validation. Nothing at all is a 400. *)

val by_reference :
  sw:Eio.Switch.t ->
  ?parse_reference:bool ->
  ?first_author:string ->
  ?atitle:string ->
  ?jtitle:string ->
  ?year:string ->
  t ->
  string ->
  Fetch.response
(** [by_reference ~sw t biblio] matches the raw reference string
    [biblio], which the server sends through GROBID first unless
    [parse_reference] is [false]. That parse is the slow step, so use
    {!by_metadata} when the fields are already separated. Any of the
    other arguments given constrain the result. *)

val oa_by_doi : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [oa_by_doi ~sw t doi] fetches [doi]'s open-access link as JSON. See
    {!oa_url} on the snapshot this needs. *)

val oa_by_pmid : sw:Eio.Switch.t -> t -> string -> Fetch.response
(** [oa_by_pmid ~sw t pmid] is {!oa_by_doi} for a PubMed identifier. *)

val oa_by_pmcid : sw:Eio.Switch.t -> t -> string -> Fetch.response
(** [oa_by_pmcid ~sw t pmcid] is {!oa_by_doi} for a PubMed Central
    identifier, with or without its ["PMC"] prefix. *)

val data : sw:Eio.Switch.t -> t -> Fetch.response
(** [data ~sw t] fetches the server's report of how many records each of
    its stores holds, which is how to tell an empty or half-loaded
    instance from a complete one. *)

(** {1 Reading the answers} *)

(** The fields of a lookup answer a caller routes on. *)
module Record : sig
  type t = {
    doi : string option;  (** The record's [DOI] member, unvalidated. *)
    title : string option;
        (** The first entry of [title], which Crossref writes as an
            array. *)
    journal : string option;  (** The first entry of [container-title]. *)
    year : int option;
        (** The year of [published], or of [published-print] or
            [published-online] in a record without it. *)
    publisher : string option;  (** The house, as prose: ["Elsevier BV"]. *)
    pmid : string option;  (** The cross-identifier the service adds. *)
    pmcid : string option;
    oa : string option;
    kind : string option;
        (** Crossref's word for what the record is: ["journal-article"]
            for a paper, ["journal-issue"] and ["journal"] for the
            things a journal list carries among its papers. *)
        (** The [oaLink] the service adds from its Unpaywall store, for
            an instance that has one loaded. *)
  }
  (** The typed view of a lookup answer. The service answers the work's
      Crossref record itself, so every field is a member of that record
      rather than of a wrapper around it, and everything not named here
      is ignored. *)

  val jsont : t Jsont.t
  (** [jsont] reads a lookup answer. Every member is optional, so a
      sparse record decodes to a [t] of [None]s rather than failing. A
      title or journal written as a bare string rather than as an array
      is read as the one entry it is, a cross-identifier written as a
      number as the identifier it spells, and a date part that is null
      as no year.

      Encoding writes the year back as [published] and the title and
      journal as one-entry arrays, which is the shape the service
      answers. *)
end

val oa_link_jsont : string option Jsont.t
(** [oa_link_jsont] reads the [oaLink] of an open-access answer, and is
    [None] for an answer that names none. *)
