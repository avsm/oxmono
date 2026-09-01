(** This module provides simple response selection from the Accept request
    field. Base media types are matched after parameters other than [q] are
    discarded; a range such as [text/*] or [*/*] matches any type under it. See
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-12.5.1}RFC 9110 section
     12.5.1} for the field syntax and preference semantics. *)

type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]
(** A [media] is a media type this library can negotiate. [`Other] carries a
    full type such as ["image/png"]. Other media types are matched without
    regard to ASCII case. *)

val of_accept : string option @ local -> media list @@ portable
(** [of_accept accept] is the base media types in [accept], most preferred
    first, ordered by q-value with a missing q taken as 1. Media parameters
    other than [q] are discarded. Zero-quality ranges are dropped, as is any
    member whose q is not a qvalue, and equally ranked ranges keep their wire
    order. It is [[]] when [accept] is absent or empty. A type or wildcard this
    library does not name becomes [`Other]. *)

val v :
  (media * 'env Route.handler) list @ portable -> 'env Route.handler @ portable
  @@ portable
(** [v variants] is a handler that invokes the first match in the client's
    preference order, comparing [`Other] strings without regard to ASCII case.
    The first variant is served when the client sends no Accept field; when it
    sends one that no variant satisfies, the response is 406 Not Acceptable
    listing the available types. The response gains [Vary: Accept]. An empty
    list responds with 404 Not Found. [variants] is taken at [portable] because
    the handler it yields captures it, and a route stores that handler in a
    portable closure. *)

val select :
  'a Httpz.Media.t list -> Req.t @ local -> 'a Httpz.Media.t @@ portable
(** [select codecs req] is the client's preferred codec, or the first codec
    when no accepted range matches. It raises [Invalid_argument] for an empty
    list. *)

val select_opt :
  'a Httpz.Media.t list -> Req.t @ local -> 'a Httpz.Media.t option @@ portable
(** [select_opt codecs req] is {!select} but [None] when the client stated
    what it accepts and no codec falls within it. See
    {!Proffer.Negotiate.select_opt}. *)

val encode :
  ?status:Status.t ->
  ?etag:Etag.t @ local ->
  ?cache:Cache_control.t @ local ->
  ?headers:Headers.t @ local ->
  Resp.respond @ local ->
  Req.t @ local ->
  'a Httpz.Media.t list ->
  'a ->
  unit
  @@ portable
(** [encode respond req codecs value] responds with [value] under the selected
    codec and [Vary: Accept], or with 406 Not Acceptable. See
    {!Proffer.Negotiate.encode}. *)
