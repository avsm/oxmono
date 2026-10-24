(** The Wiley Text and Data Mining API.

    Authentication is a [Wiley-TDM-Client-Token] header drawn from a
    pool of tokens. Article requests redirect to a separate content
    host, so {!client} scopes the token to {!origin} without restricting
    the client to it. Pace requests 2-4 seconds apart
    ([Fetch.with_limits ~min_interval:3.0]). *)

type t
(** The authority to call the Wiley TDM API, minted by {!client}. It is
    abstract, so {!client} is the only way to make one and another
    service's capability cannot stand in for it. *)

val origin : string
(** [origin] is ["https://api.wiley.com"], the URL prefix the API
    answers on. *)

val url : doi:Taposaur.Doi.t -> string
(** [url ~doi] is the TDM article endpoint for [doi]. *)

val client : tokens:string list -> _ Fetch.t -> t
(** [client ~tokens t] is [t] restricted to the safe methods, with a
    token from [tokens] attached to each request bound for {!origin},
    rotating through the pool per request. Every wrapper goes on [t], as
    the result is not a client.
    @raise Invalid_argument if [tokens] is empty. *)


val pdf : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [pdf ~sw t doi] fetches [doi]'s PDF, following the redirect to the
    content host. *)
