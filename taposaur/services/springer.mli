(** The Springer Nature full-text API.

    Authentication is an [api_key] query parameter, bound by {!client}
    so that a URL a caller builds never carries it. Queries batch up to
    25 DOIs and answer JATS XML. On a 429 the service expects a
    half-hour pause: retry with {!retry} and keep to one request in
    flight ([Fetch.with_limits ~max_concurrent:1]). *)

type t
(** The authority to call the Springer Nature API, minted by {!client}.
    It is abstract, so {!client} is the only way to make one and another
    service's capability cannot stand in for it. *)

val origin : string
(** [origin] is ["https://api.springernature.com"], the URL
    prefix every request goes under. *)

val url : dois:Taposaur.Doi.t list -> string
(** [url ~dois] is the batch query for [dois].
    @raise Invalid_argument if [dois] is empty or longer than 25. *)

val client : api_key:string -> _ Fetch.t -> t
(** [client ~api_key t] is [t] restricted to {!origin} and to the safe
    methods, with [api_key] bound as a query parameter on every request.
    Every wrapper goes on [t], as the result is not a client. *)


val articles : sw:Eio.Switch.t -> t -> Taposaur.Doi.t list -> Fetch.response

val pdf_url : doi:Taposaur.Doi.t -> string
(** [pdf_url ~doi] is where the publisher serves [doi]'s PDF by network
    entitlement, with no API key: nature.com for a 10.1038 name,
    link.springer.com/content/pdf otherwise. *)

val pdf_client : _ Fetch.t -> t
(** [pdf_client t] is a client for {!pdf}, confined to the two PDF
    origins and carrying no credential — the entitlement is the
    network's. *)

val pdf : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [pdf ~sw t doi] requests {!pdf_url}. *)
(** [articles ~sw t dois] fetches the batch response for [dois]. A
    requested DOI absent from the body was not found. *)

val retry : Fetch.Retry.config
(** [retry] is a configuration for {!Fetch.with_retry} that waits the
    service's half hour on a 429 or 500, three times at most. *)
