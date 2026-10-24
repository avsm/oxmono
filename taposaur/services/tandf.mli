(** Taylor & Francis PDF access.

    There is no API and no authentication: PDFs are fetched from the
    public site. Pace requests 2-4 seconds apart
    ([Fetch.with_limits ~min_interval:3.0]). *)

type t
(** The authority to call the Taylor & Francis site, minted by
    {!client}. It is abstract, so {!client} is the only way to make one
    and another service's capability cannot stand in for it. *)

val origin : string
(** [origin] is ["https://www.tandfonline.com"], the URL prefix every
    request goes under. *)

val url : doi:Taposaur.Doi.t -> string
(** [url ~doi] is the PDF location for [doi]. *)

val client : _ Fetch.t -> t
(** [client t] is [t] restricted to {!origin} and to the safe methods.
    There is no credential to attach. Every wrapper goes on [t], as the
    result is not a client. *)

val pdf : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [pdf ~sw t doi] fetches [doi]'s PDF. The site may answer a
    challenge page instead. Check the response's content type. *)
