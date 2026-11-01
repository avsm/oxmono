(** PLOS full text ({{:https://journals.plos.org/}journals.plos.org}).

    There is no API and no authentication: every article is served from
    the public site, as JATS XML and as a PDF. Pace requests a second
    or two apart ([Fetch.with_limits ~min_interval:2.0]).

    An annotation DOI, [10.1371/annotation/] and a UUID, names a
    correction rather than an article and is served elsewhere. Nothing
    here covers one. *)

type t
(** The authority to call the PLOS site, minted by {!client}. It is
    abstract, so {!client} is the only way to make one and another
    service's capability cannot stand in for it. *)

val origin : string
(** [origin] is ["https://journals.plos.org"], the URL prefix every
    request starts at. *)

val xml_url : doi:Taposaur.Doi.t -> string
(** [xml_url ~doi] is the JATS XML location for [doi]. One path serves
    every PLOS journal, so this needs to know nothing about which of
    them published [doi]. *)

val pdf_url : doi:Taposaur.Doi.t -> string
(** [pdf_url ~doi] is {!xml_url}'s location for the PDF. *)

val client : _ Fetch.t -> t
(** [client t] is [t] restricted to the safe methods. There is no
    credential to attach, and no origin restriction either: a file
    request is answered with a redirect to the signed storage URL the
    bytes are at, which is another origin. Every wrapper goes on [t],
    as the result is not a client. *)

val article : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [article ~sw t doi] fetches [doi]'s full text as JATS XML. A DOI
    the site does not hold is a 404 whose body is a web page. *)

val pdf : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [pdf ~sw t doi] fetches [doi]'s PDF, which is the same article
    typeset. *)
