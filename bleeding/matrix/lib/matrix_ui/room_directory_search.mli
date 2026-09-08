(** Reactive, paged search of the public room directory.

    This is the stateful facade corresponding to
    [matrix-sdk/src/room_directory_search.rs] in the pinned Rust SDK. Results
    arrive in server order and successful later pages are appended. Starting a
    new search clears the preceding query and results. *)

type state =
  | Start  (** No search has been started. *)
  | Loading  (** A request owns the service. *)
  | Next of string  (** A page succeeded and [string] is its next token. *)
  | End  (** The last page succeeded. *)
  | Failed of Matrix_client.Error.t
      (** The last request failed. Its token and successful results are kept so
          {!next_page} retries the same page. *)

type t
(** One mutable directory query. Calls from concurrent fibers are serialized, as
    Rust's mutable receiver requires of its callers. *)

val create : client:Matrix_client.Client.t -> unit -> t
(** [create ~client ()] is an empty service in {!Start}. *)

val search :
  t ->
  ?filter:string ->
  batch_size:int ->
  ?via_server:string ->
  unit ->
  (unit, Matrix_client.Error.t) result
(** [search t ~batch_size ()] clears [t] and immediately requests its first
    page. [filter] is matched by the homeserver against room names, topics and
    aliases. [via_server] searches that server's public directory instead of the
    client's homeserver.

    A failed first page leaves an empty, retryable search in {!Failed}.

    Raises [Invalid_argument] when [batch_size] is not positive. *)

val next_page : t -> (unit, Matrix_client.Error.t) result
(** [next_page t] appends the page named by the last successful response. A
    failure preserves the token and existing results, so another call retries
    it. At {!End} this succeeds without making a request. Before {!search} it
    returns a local {!Matrix_client.Error.Json_error}. *)

val results : t -> Matrix_client.Directory.room_summary Observable.List.t
(** All successful results so far, in server/page order. *)

val state : t -> state Observable.Value.t
(** The current request/progress state. *)

val loaded_pages : t -> int
(** The number of result-sized pages, calculated as the pinned Rust SDK does:
    [ceil (result count / batch size)]. It is zero before a search and after an
    empty first page. *)

val is_at_last_page : t -> bool
(** Whether the last successful response omitted its next token. *)
