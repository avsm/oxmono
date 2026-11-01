exception Error of string
(** Errors never contain secret values. *)

module Secret : sig
  type t

  val of_string : string -> t

  val with_value : t -> (string -> 'a) -> 'a
  (** [with_value secret f] exposes the value only to [f]. OCaml strings cannot
      be reliably erased. This is an API boundary, not protected memory. *)

  val pp : Format.formatter -> t -> unit
end

val read_file : ?max_bytes:int -> string -> string
(** [read_file path] reads an owned private regular file, refusing symlinks. It
    removes one trailing LF or CRLF and rejects empty or multiline values.
    Spaces are preserved. The default bound is 64 KiB. This is synchronous. *)

val single_line : string -> string
(** [single_line value] validates and removes one trailing LF or CRLF without
    trimming spaces. It never includes [value] in an error. *)

module Reference : sig
  type t

  val file : string -> t
  val named : string -> t

  val of_string : string -> t
  (** [of_string text] reads [sekrit:NAME] as a named reference and other
      strings as file paths. Relative paths must be resolved by the caller. *)

  val to_string : t -> string
  val source : t -> [ `File of string | `Named of string ]
  val pp : Format.formatter -> t -> unit
end

module Backend : sig
  type t

  val v : find:(string -> Secret.t option) -> t
  (** [v ~find] supplies a resolver without granting write or enumeration
      capabilities. A future Keychain backend can implement this interface. *)
end

val resolve : backend:Backend.t -> Reference.t -> Secret.t
(** [resolve ~backend reference] rereads the source on each call. There is no
    implicit cache. Connections that need a stable value take an explicit
    snapshot using [Source.snapshot]. *)

module Source : sig
  type t

  val v : backend:Backend.t -> Reference.t -> t
  val get : t -> Secret.t

  val snapshot : t -> t
  (** [snapshot source] resolves once and retains that value. *)
end

module Paths : sig
  val root : unit -> string

  val key_file : unit -> string
  (** Paths use XDG data/config homes, respectively. [SEKRIT_STORE_DIR] and
      [SEKRIT_KEY_FILE] override them with absolute paths. No files are created. *)
end

module Key : sig
  val create : string -> unit
  (** [create path] creates a private random 256-bit master key. It refuses
      replacement. Losing the key loses access to encrypted values. *)
end

module Sealed_file : sig
  val is_sealed : string -> bool
  val read : key_file:string -> context:string -> string -> string

  val write : key_file:string -> context:string -> string -> string -> unit
  (** [write ~key_file ~context path plaintext] atomically stores an encrypted
      authenticated envelope. [context] is authenticated and must be a stable
      logical identity, independent of the physical path. Reads reject altered
      ciphertext, a wrong key or context, and plaintext input. The key must
      already exist and be outside the containing directory. Caller owns the
      read/modify/write lock. The plaintext bound is 4 MiB. *)
end

module Envelope : sig
  val seal : key:Secret.t -> context:string -> string -> string

  val open_ : key:Secret.t -> context:string -> string -> string
  (** [open_ ~key ~context envelope] authenticates and decrypts an envelope.
      These operations perform no filesystem I/O. Keys must contain 32 raw
      bytes. [seal] obtains a fresh nonce from the operating system CSPRNG.
      Plaintext is bounded to 4 MiB. *)
end

module Store : sig
  type t

  val of_storage :
    read:(unit -> string) -> update:((string -> string) -> unit) -> t
  (** [of_storage ~read ~update] shares the vault schema with another I/O
      backend. Callbacks exchange plaintext vault snapshots. [update change]
      must atomically read, apply [change] and durably publish under its lock.
      The backend owns initialization, encryption and lifetime. *)

  val v : ?root:string -> ?key_file:string -> unit -> t
  (** [v ()] describes a store without creating it. *)

  val init : t -> unit
  (** [init store] initializes an empty store and creates its master key if
      absent. It never creates a replacement key for an existing vault. *)

  val find : t -> string -> Secret.t option
  val list : t -> string list
  val put : t -> name:string -> replace:bool -> Secret.t -> unit
  val remove : t -> string -> unit
  val backend : t -> Backend.t
end

val read_reference : ?max_bytes:int -> string -> string
(** [read_reference reference] reads a private file or [sekrit:NAME] from the
    default store. Values use the single-line rule. Applications that need
    explicit capabilities should use [resolve] instead. *)
