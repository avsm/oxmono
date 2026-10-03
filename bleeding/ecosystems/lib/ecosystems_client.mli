val pages :
  ?per_page:int -> (page:string -> per_page:string -> 'a list) -> 'a Seq.t
(** [pages ?per_page f] is the items of every page of [f], in order.
    [f ~page ~per_page] fetches one page. Pages are numbered from 1. The
    sequence is lazy and stops after a page with fewer than [per_page] items.
    [per_page] defaults to 100. The arguments are strings because the
    generated operations take them as strings. *)

val default_base_url : string
(** [default_base_url] is ["https://packages.ecosyste.ms/api/v1"]. *)

val create :
  ?user_agent:string ->
  ?base_url:string ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Ecosystems.t
(** [create ?user_agent ?base_url ~sw env] is a client for the public API.
    [user_agent] defaults to ["ocaml-ecosystems"]. [base_url] defaults to
    {!default_base_url}. *)
