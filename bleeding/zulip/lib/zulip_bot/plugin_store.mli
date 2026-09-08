(** Persistent JSON values for bot plugins.

    Values are partitioned first by plugin and then by room scope. Omitting a
    room selects a global namespace distinct from every explicit room string,
    including [*]. Operations on one store are serialized between fibers. *)

type error =
  | Codec of string
  | Backend of string
      (** The type for store errors. [Codec message] reports conversion between
          an OCaml value and JSON. [Backend message] reports storage, file
          validation, or stored-structure failure. *)

type t
(** The type for plugin stores. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] prints a human-readable representation of [error] on
    [ppf]. *)

val error_to_string : error -> string
(** [error_to_string error] is a human-readable representation of [error]. *)

val memory : unit -> t
(** [memory ()] is a new empty in-memory store. *)

val open_file : Eio.Fs.dir_ty Eio.Path.t -> (t, error) result
(** [open_file path] is a store backed by [path]. A missing file starts an empty
    store. An existing file must be regular and grant no permissions to its
    group or other users. Files are limited to 1 MiB and 128 levels of JSON
    nesting. Every store object is limited to 4096 members. Writes replace
    [path] atomically through a synced mode-[0600] temporary file in the same
    directory and enforce the same limits. Separate processes must not write the
    same file concurrently.

    Fiber cancellation propagates. Other file, permission, validation, and
    structure failures are returned as [Backend]. *)

val find :
  t ->
  ?room:string ->
  plugin:string ->
  key:string ->
  'a Jsont.t ->
  ('a option, error) result
(** [find store ~room ~plugin ~key codec] is the value at [key] in the [room]
    namespace of [plugin], decoded with [codec]. It is [None] if the key is
    absent. [room] defaults to the global namespace. Decoding failures are
    returned as [Codec]. *)

val set :
  t ->
  ?room:string ->
  plugin:string ->
  key:string ->
  'a Jsont.t ->
  'a ->
  (unit, error) result
(** [set store ~room ~plugin ~key codec value] encodes [value] with [codec] and
    binds it to [key] in the [room] namespace of [plugin]. Any previous value is
    replaced. [room] defaults to the global namespace. The binding changes only
    if the backing store is updated successfully. Encoding failures are returned
    as [Codec]. Storage failures are returned as [Backend]. *)

val update :
  t ->
  ?room:string ->
  plugin:string ->
  key:string ->
  'a Jsont.t ->
  ('a option -> 'a) ->
  ('a, error) result
(** [update store ~room ~plugin ~key codec f] applies [f] to the current decoded
    value and stores the result encoded with [codec]. [f] receives [None] if the
    key is absent. [room] defaults to the global namespace. The read, callback,
    and write are serialized as one operation. The binding changes only if the
    backing store is updated successfully. [f] runs while the store's write lock
    is held and must not call another operation on the same store, which would
    deadlock. Exceptions from [f] propagate. Conversion failures are returned as
    [Codec]. Storage failures are returned as [Backend]. *)

val remove :
  t ->
  ?room:string ->
  plugin:string ->
  key:string ->
  unit ->
  (unit, error) result
(** [remove store ~room ~plugin ~key ()] removes [key] from the [room] namespace
    of [plugin]. An absent key is accepted. [room] defaults to the global
    namespace. The binding changes only if the backing store is updated
    successfully. Storage failures are returned as [Backend]. *)

val keys : t -> ?room:string -> plugin:string -> unit -> string list
(** [keys store ~room ~plugin ()] is the sorted list of keys in the [room]
    namespace of [plugin]. [room] defaults to the global namespace. *)
