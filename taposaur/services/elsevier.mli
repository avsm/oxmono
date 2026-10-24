(** The Elsevier ScienceDirect article API.

    Authentication is an [X-ELS-APIKey] header, with an optional
    [X-ELS-Insttoken] institution token. Responses are served directly
    from {!origin}. Pace bulk use with
    [Fetch.with_limits ~max_concurrent:5]. *)

type t
(** The authority to call ScienceDirect, minted by {!client}. It is
    abstract, so {!client} is the only way to make one and another
    service's capability cannot stand in for it. *)

val origin : string
(** [origin] is ["https://api.elsevier.com"], the URL prefix every
    request goes under. *)

val url : doi:Taposaur.Doi.t -> string
(** [url ~doi] is the article endpoint for [doi]. *)

val client :
  api_key:(unit -> string) ->
  ?inst_token:(unit -> string) ->
  _ Fetch.t -> t
(** [client ~api_key t] is [t] restricted to {!origin} and to the safe
    methods, with [api_key ()], and [inst_token ()] when given, attached
    to every request. Every wrapper goes on [t], as the result is not a
    client. *)


val article : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [article ~sw t doi] fetches [doi]'s full text as XML. 4xx and 5xx
    statuses are returned as data, per {!Fetch.fetch}. *)
