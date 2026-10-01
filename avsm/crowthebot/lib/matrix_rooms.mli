(** Room metadata from the live Matrix synchronization state, and posting to a
    joined room or an existing DM on a requester's behalf. *)

type t

type sender = Matrix_proto.Id.Room_id.t -> string -> (string, string) result
(** Sends Markdown text to a joined room as a notice and returns the event ID,
    or why delivery failed. *)

val create :
  store:Store.t ->
  self:string ->
  state:(unit -> Matrix_client.Base_client.state option) ->
  ?send:(unit -> sender option) ->
  unit ->
  t
(** [create ~store ~self ~state ?send ()] retains an authorization store, Crow's
    own Matrix ID and a read-only state callback. Without [send], or while it
    returns [None], [matrix_send] fails. *)

val names : string list
val is_tool : string -> bool
val tools : Agentkit.Agent.Tool.t list
val system_prompt : string

val invoke :
  t ->
  actor:string ->
  room:string ->
  string ->
  string ->
  (string, string) result
(** [invoke t ~actor ~room name arguments] runs a Matrix tool for the admin or
    an allowed friend. [room] is the default room for detail queries. Results
    contain at most five rooms or members and an explicit continuation cursor.
    No message bodies or arbitrary room state are returned.

    [matrix_send] posts to a joined room that [actor] is a member of, or to an
    existing DM whose complete membership is exactly Crow and the recipient. The
    recipient must be the admin or an approved friend. Each message ends with a
    line naming [actor]. *)
