(** Read-only room metadata from the live Matrix synchronization state. *)

type t

val create :
  store:Store.t -> state:(unit -> Matrix_client.Base_client.state option) -> t
(** [create ~store ~state] retains an authorization store and a read-only state
    callback. No filesystem, network or message-sending capability is needed. *)

val names : string list
val is_tool : string -> bool
val tools : Openrouter.Tool.t list
val system_prompt : string

val invoke :
  t ->
  actor:string ->
  room:string ->
  string ->
  string ->
  (string, string) result
(** [invoke t ~actor ~room name arguments] queries joined rooms for the admin or
    an allowed friend. [room] is the default room for detail queries. Results
    contain at most five rooms or members and an explicit continuation cursor.
    No message bodies or arbitrary room state are returned. *)
