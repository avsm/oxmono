(** Room metadata, paginated message history and posting to a joined room or
    an existing DM on a requester's behalf. *)

type t

type sender = Matrix_proto.Id.Room_id.t -> string -> (string, string) result
(** Sends Markdown text to a joined room as a notice and returns the event ID,
    or why delivery failed. *)

val create :
  store:Store.t ->
  self:string ->
  state:(unit -> Matrix_client.Base_client.state option) ->
  ?send:(unit -> sender option) ->
  ?speak:(unit -> sender option) ->
  ?client:Matrix_eio.Client.t -> ?encryption:Matrix_eio.Encryption.t ->
  unit ->
  t
(** [create ~store ~self ~state ?send ?speak ()] retains an authorization
    store, Crow's own Matrix ID and a read-only state callback. Without [send],
    or while it returns [None], [matrix_send] fails. [speak] turns text into a
    voice note and sends it. Without it [matrix_voice_note] is not offered.
    [client] enables the read-only
    [matrix_history] tool. [encryption] decrypts history using available keys. *)

val room_label : Matrix_client.Base_client.room_info -> string
(** [room_label room] is the canonical alias, nonblank room name, or room ID,
    in that order. *)

val names : string list
val is_tool : string -> bool
val tools : t -> Agentkit.Agent.Tool.t list
(** [tools t] omits [matrix_voice_note] when [t] cannot speak and [matrix_history]
    when no Matrix client was supplied. *)
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
    Metadata queries return no bodies or arbitrary room state. [matrix_history]
    reads up to five events per page after checking current membership remotely.
    It returns excerpts, timestamps, relations and an opaque backwards cursor.
    Missing keys and truncated bodies are explicit. Pages fit the tool's context
    budget and do not enter Crow's incoming-message handlers or send read receipts.

    [matrix_send] posts to a joined room that [actor] is a member of, or to an
    existing DM whose complete membership is exactly Crow and the recipient. The
    recipient must be the admin or an approved friend.

    [matrix_voice_note] follows the same rules and defaults to [room]. *)
