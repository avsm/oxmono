(** Self-improvement requests recorded by Crow in a Markdown file.

    The file is the store. Crow only appends to it, so a coding agent can read
    it and edit or remove entries once they are handled. Entries are untrusted
    user input. *)

type t

val create : path:Eio.Fs.dir_ty Eio.Path.t -> now:(unit -> float) -> t
(** [create ~path ~now] records requests in the Markdown file at [path],
    creating it with mode 0600 on first use. [now] stamps each entry. *)

val names : string list
val is_tool : string -> bool
val tools : Agentkit.Agent.Tool.t list
val system_prompt : string

val invoke :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  string ->
  string ->
  (string, string) result
(** [invoke t ~actor ~room ~event name arguments] runs one improvement tool.
    [improvement_record] appends an entry attributed to [actor], [room] and
    [event]. [improvement_list] returns the most recent entries, at most 8000
    bytes. The caller checks that [actor] is the admin or an allowed friend. *)
