type t

val create :
  tools:Bushel_agent.t -> allowed:(actor:string -> room:string -> bool) -> t

val available : t -> actor:string -> room:string -> bool

val invoke :
  t ->
  actor:string ->
  room:string ->
  check:(unit -> unit) ->
  string ->
  string ->
  (string, string) result
(** [invoke t ~actor ~room ~check name arguments] rechecks access before and
    after retrieval and embedding inference. The caller owns audit logging. *)

val sources : fs:Eio.Fs.dir_ty Eio.Path.t -> string * string
(** [sources ~fs] discovers Bushel's configured data directory and Sortal's
    native store using their library configuration and XDG overrides. *)

val authorized : Store.t -> actor:string -> room:string -> bool
(** [authorized store ~actor ~room] uses Crow's existing allowed users,
    enabled rooms and accepted direct rooms. *)

val load :
  sw:Eio.Switch.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  fetch:_ Fetch.t ->
  store:Store.t ->
  max_bytes:int ->
  unit ->
  t option
(** [load ...] discovers the existing Bushel and Sortal installation without
    opening its index. Missing source directories leave tools disabled. Each
    authorized invocation reads the published Bushel snapshot using current
    configuration. Native models are owned by [sw]. *)
