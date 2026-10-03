val pages :
  ?per_page:int -> (page:string -> per_page:string -> 'a list) -> 'a Seq.t
(** [pages ?per_page f] is the items of every page of [f], in order.
    [f ~page ~per_page] fetches one page. Pages are numbered from 1. The
    sequence is lazy and ends at the first empty page, so a server that
    returns fewer items than [per_page] loses none. Errors from [f] are raised
    when the sequence is forced, and traversing it twice repeats every request.
    [per_page] defaults to 100. The API caps it at 1000. The arguments are
    strings because the generated operations take them as strings.
    @raise Invalid_argument if [per_page] is less than 1. *)

val default_base_url : string
(** [default_base_url] is ["https://packages.ecosyste.ms/api/v1"]. *)

val create :
  ?session:_ Fetch.t ->
  ?max_response_bytes:int ->
  ?user_agent:string ->
  ?base_url:string ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Ecosystems.t
(** [create ?session ?max_response_bytes ?user_agent ?base_url ~sw env] is a
    client for the public API.

    Without [session], requests are paced per origin, and a 429, 500, 502, 503
    or 504 response is retried up to three times, honouring Retry-After. No
    cookies are kept. [user_agent] defaults to ["ocaml-ecosystems"] and applies
    only to this default session.

    With [session], that client is used as it stands and [user_agent] is
    ignored. [max_response_bytes] bounds a response body and defaults to 16 MiB.
    [base_url] defaults to {!default_base_url}.

    The [Ecosystems.PackageWithRegistry.bulk_lookup_packages] body is a raw
    [Jsont.json] object, because the generator leaves the spec's inline
    request schema opaque. *)
