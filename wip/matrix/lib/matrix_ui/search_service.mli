(** Reactive pagination over the homeserver's room-event search endpoint.

    The pinned Rust SDK's default UI search service is backed by its local event
    index. OCaml Matrix does not have that index, so this deliberately presents
    the typed [POST /search] service instead. Results retain the server's
    ordering and later pages are appended. *)

type state =
  | Idle of { end_reached : bool }
      (** No request is running. [end_reached] means the last successful page
          omitted its next token. *)
  | Loading  (** A search request is currently running. *)

type t
(** The mutable state for one query. Since endpoint calls are synchronous,
    another fiber which enters while the fetch implementation has yielded is
    rejected instead of racing the active request. *)

val create : client:Matrix_client.Client.t -> unit -> t
(** [create ~client ()] is an empty service. *)

val search :
  t ->
  criteria:Matrix_client.Search.criteria ->
  (unit, Matrix_client.Error.t) result
(** [search t ~criteria] clears the old query and immediately loads its first
    page. A failure leaves an empty, retryable query and is exposed through
    {!last_error}.

    Returns a local {!Matrix_client.Error.Json_error} if another request is
    already running. *)

val next_page : t -> (unit, Matrix_client.Error.t) result
(** [next_page t] appends the next server page. A failed request preserves the
    current token and all successful results, so the next call retries exactly
    that page. At the end it succeeds without making a request. Before {!search}
    and during another request it returns a local
    {!Matrix_client.Error.Json_error}. *)

val results : t -> Matrix_client.Search.hit Observable.List.t
(** Every successful hit so far, in server and page order. *)

val state : t -> state Observable.Value.t
(** The observable request state. *)

val last_error : t -> Matrix_client.Error.t option Observable.Value.t
(** The last request error. It is cleared when a request starts. *)

val loaded_pages : t -> int
(** The number of successful requests for the current query, including an empty
    terminal page. Failed requests do not increment it. *)
