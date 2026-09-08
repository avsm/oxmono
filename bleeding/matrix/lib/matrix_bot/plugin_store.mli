(** plugin_store — small persistent values, keyed by plugin.

    A plugin keeps counters, cursors and preferences here rather than in a file
    of its own. Values are JSON, named by a plugin, a key and optionally a room,
    and a bot with a profile keeps them across restarts.

    The file is one JSON object,
    [{ "<plugin>": { "<room id or \"*\">": { "<key>": <value> } } }], with ["*"]
    holding the values stored without a room. Every write replaces it whole
    through a unique 0600 temporary name, file sync and an atomic rename, so a
    crash loses at most the write in progress and never leaves half a file. The
    file is synced before rename, but Eio has no portable directory-fsync
    operation, so persistence of the renamed directory entry after power loss is
    not guaranteed. The store is not shared between processes. *)

(** The type for what a store operation failed with. *)
type error =
  | Codec of string  (** A value could not be encoded or decoded. *)
  | Backend of string  (** The file could not be written. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints [e] on one line, without a trailing newline. *)

val error_to_string : error -> string
(** [error_to_string e] is {!pp_error} into a string. *)

type t
(** The type for stores. *)

val memory : unit -> t
(** [memory ()] is a store that forgets on exit, for tests and for a bot with no
    profile. *)

val open_file : Eio.Fs.dir_ty Eio.Path.t -> t
(** [open_file path] is the store held in [path], loaded if the file exists and
    created on the first write. A file that does not parse is renamed aside and
    reported on {!Logging.src}, and the store starts empty. Missing files are
    treated as empty; other filesystem failures, and cancellation, propagate as
    exceptions. *)

val find :
  t ->
  ?room:Matrix_proto.Id.Room_id.t ->
  plugin:string ->
  key:string ->
  'a Jsont.t ->
  ('a option, error) result
(** [find t ~plugin ~key codec] is the value stored under [plugin], [room] and
    [key], decoded with [codec]. It is [Ok None] when nothing is stored there
    and {!Codec} when what is stored no longer decodes. Without [room] the value
    stored outside any room is read. *)

val set :
  t ->
  ?room:Matrix_proto.Id.Room_id.t ->
  plugin:string ->
  key:string ->
  'a Jsont.t ->
  'a ->
  (unit, error) result
(** [set t ~plugin ~key codec value] stores [value] under [plugin], [room] and
    [key], and writes the file. Without [room] the value is stored outside any
    room, where a plugin's own [room] lookups will not see it. *)

val update :
  t ->
  ?room:Matrix_proto.Id.Room_id.t ->
  plugin:string ->
  key:string ->
  'a Jsont.t ->
  ('a option -> 'a) ->
  ('a, error) result
(** [update t ~plugin ~key codec f] stores and is [f] of the current value, with
    no other fiber reading or writing that key in between. [f] is passed [None]
    when nothing is stored and when what is stored no longer decodes. *)

val remove :
  t ->
  ?room:Matrix_proto.Id.Room_id.t ->
  plugin:string ->
  key:string ->
  unit ->
  (unit, error) result
(** [remove t ~plugin ~key ()] drops the value stored under [plugin], [room] and
    [key], and writes the file. Removing what is not there succeeds. *)

val keys :
  t -> ?room:Matrix_proto.Id.Room_id.t -> plugin:string -> unit -> string list
(** [keys t ~plugin ()] is the keys [plugin] has stored for [room], in
    increasing order. *)
