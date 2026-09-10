(** Crowthebot mail capabilities backed by typed JMAP methods. *)

type reader
type writer
type identity = { account : string; username : string }

val connect_read_only :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  token:string ->
  ?account_id:string ->
  ?max_body:int ->
  string ->
  reader
(** [connect_read_only ~sw ~fetch ~clock ~token ?account_id ?max_body url]
    connects a bearer token to one mail account. The HTTP capability permits
    only session discovery, Email/query, Email/get, Thread/get and Mailbox/get.
    Endpoints remain on the configured HTTPS origin. Responses default to a 32
    MiB limit and each HTTP exchange has a 20-second deadline. *)

val connect_read_write :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  token:string ->
  ?account_id:string ->
  ?max_body:int ->
  string ->
  writer
(** [connect_read_write ~sw ~fetch ~clock ~token ?account_id ?max_body url] adds
    only Email/set updates of mailboxIds children. Creating, destroying,
    submitting and editing message contents remain unavailable. *)

val identity : reader -> identity
val writer_identity : writer -> identity

type query_page = {
  query : Jmap.Proto.Method.query_response;
  next_position : int64 option;
}

type thread_page = {
  thread_id : Jmap.Proto.Id.t;
  position : int;
  total : int;
  email_ids : Jmap.Proto.Id.t list;
  emails : Jmap.Proto.Email.t Jmap.Proto.Method.get_response option;
  next_position : int option;
}

val query :
  reader ->
  ?filter:Jmap.Proto.Email.filter ->
  ?sort:Jmap.Proto.Filter.comparator list ->
  position:int ->
  limit:int ->
  collapse_threads:bool ->
  unit ->
  query_page
(** [query reader ?filter ?sort ~position ~limit ~collapse_threads ()] runs one
    Email/query page, with a limit of 1 to 50. Filters and comparators use the
    typed JMAP codecs. The result includes IDs and the server's query state. *)

val read :
  reader ->
  id:Jmap.Proto.Id.t ->
  Jmap.Proto.Email.t Jmap.Proto.Method.get_response
(** [read reader ~id] fetches one email including text and HTML body values.
    Reads do not change keywords or mark messages as seen. *)

val thread :
  reader -> id:Jmap.Proto.Id.t -> position:int -> limit:int -> thread_page
(** [thread reader ~id ~position ~limit] reads up to ten emails in a thread.
    [next_position] continues the ordered thread listing. *)

val mailboxes : reader -> Jmap.Proto.Mailbox.t Jmap.Proto.Method.get_response
(** [mailboxes reader] lists mailbox IDs, names, roles and rights. *)

val update_labels :
  writer ->
  id:Jmap.Proto.Id.t ->
  add:Jmap.Proto.Id.t list ->
  remove:Jmap.Proto.Id.t list ->
  Jmap.Proto.Email.t Jmap.Proto.Method.set_response
(** [update_labels writer ~id ~add ~remove] patches the membership of one email.
    It reads the current Email state and uses ifInState for the update, leaving
    other labels intact. Conflicts are reported without retrying the mutation.
    No resulting empty mailbox set is allowed. *)

val error : exn -> string
(** [error exn] classifies errors without server text or email contents. *)

module Http : sig
  val create :
    url:string ->
    writable:bool ->
    _ Fetch.t ->
    Fetch.plain * (account:string -> api_url:string -> unit)
  (** [create ~url ~writable fetch] restricts HTTP requests for a mail
      connection. The returned binding function pins the discovered account and
      API URL. Intended for adapters that supply their own session lifecycle. *)
end
