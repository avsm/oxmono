val read_file : ?max_bytes:int -> fs:_ Eio.Path.t -> string -> string
(** [read_file ~fs path] reads through [fs] with bounded, cancellable Eio I/O.
    The same private-file and line rules apply as in [Sekrit.read_file]. *)

val resolve :
  fs:_ Eio.Path.t ->
  backend:Sekrit.Backend.t ->
  Sekrit.Reference.t ->
  Sekrit.Secret.t
(** [resolve ~fs ~backend reference] resolves file references through [fs].
    Named backend callbacks run in the calling fiber and may perform Eio
    operations. Resolving never caches a value. *)

module Source : sig
  type t

  val v : fs:_ Eio.Path.t -> backend:Sekrit.Backend.t -> Sekrit.Reference.t -> t
  val get : t -> Sekrit.Secret.t

  val snapshot : t -> t
  (** [snapshot source] explicitly resolves and retains a value. Live sources
      re-resolve through their filesystem/backend capabilities on each get. *)
end

module Store : sig
  type t

  val open_ : sw:Eio.Switch.t -> root:_ Eio.Path.t -> key_file:_ Eio.Path.t -> t
  (** [open_ ~sw ~root ~key_file] opens a native filesystem store after
      validating and pinning its directory capabilities. It does not initialize
      a store. [t] expires with [sw]. File I/O uses Eio and crypto runs in
      worker threads. Concurrent operations on one handle serialize. Other
      handles or processes contending for the same vault receive a busy
      [Sekrit.Error]. *)

  val init : root:_ Eio.Path.t -> key_file:_ Eio.Path.t -> unit
  (** [init ~root ~key_file] initializes a store through the supplied
      capabilities. Native directories are required for durable publication and
      file locks. It refuses to replace a missing key when ciphertext already
      exists. *)

  val find : t -> string -> Sekrit.Secret.t option
  val list : t -> string list
  val put : t -> name:string -> replace:bool -> Sekrit.Secret.t -> unit
  val remove : t -> string -> unit
  val backend : t -> Sekrit.Backend.t
end

val read_reference : ?max_bytes:int -> fs:_ Eio.Path.t -> string -> string
(** [read_reference ~fs reference] reads a private file or a named reference
    from the default encrypted store, through [fs]. No implicit initialization
    occurs. The default bound is 64 KiB for either source. *)

module Sealed_file : sig
  val read : key_file:_ Eio.Path.t -> context:string -> _ Eio.Path.t -> string

  val write :
    key_file:_ Eio.Path.t -> context:string -> _ Eio.Path.t -> string -> unit
  (** [write ~key_file ~context path plaintext] encrypts application-owned data
      and durably publishes it through directory capabilities. The key must
      exist outside the data directory. The caller owns the transaction lock.
      Reads never accept plaintext or migrate data implicitly. *)
end
