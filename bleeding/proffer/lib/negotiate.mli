(** Choosing a response variant from the request's Accept header. *)

type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]
(** A media type this library can negotiate. [`Other] carries a full type such
    as ["image/png"]. *)

val of_accept : string option -> media list @@ portable
(** [of_accept accept] is the media types [accept] asks for, most preferred
    first, with q-values honoured and a missing q taken as 1. It is [[]] when
    [accept] is absent or empty. A type this library does not name becomes
    [`Other]. *)

val v :
  (media * 'env Route.handler) list @ portable -> 'env Route.handler @ portable
  @@ portable
(** [v variants] is a handler that answers with the variant the client most
    prefers, which is the first media type its Accept header ranks that
    [variants] offers. The client's order decides, not the order [variants] are
    listed in. The first entry of [variants] is the fallback, taken when the
    client accepts none of them or sends no Accept header. The chosen response
    gains [Vary: Accept], since it depends on that header. An empty [variants]
    leaves nothing to answer with and gives a 404. [variants] is taken at
    [portable] because the handler it yields captures it, and a route stores
    that handler in a portable closure. *)
