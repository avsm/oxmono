(** Private connection state and serialized wire exchanges. *)
type error = Error.t =
  | Closed
  | Protocol of string
  | Transport of string
  | Rejected of { tag : string; status : [ `No | `Bad ];
      code : Imap.Response.code option; text : string }
  | State of string
  | Missing_uid of int64
  | Limit of string
  | Uncertain of string

type t = {
  flow : Transport.flow;
  mutable wire : Imap.Wire.t;
  mutable queued : Imap.Wire.event list;
  mutex : Eio.Mutex.t;
  mutable closed : bool;
  mutable tag_number : int;
  mutable generation : int;
  mutable saved_search_nonce : unit ref;
  mutable selected : string option;
  mutable uidbatches_last_mailbox : string option;
  mutable readonly : bool;
  mutable capabilities : string list;
  mutable enabled : string list;
  input : Cstruct.t;
  mutable read_size : int;
  max_metadata : int;
  max_responses : int;
  max_command_metadata : int;
}

exception Failure of error

val create : ?max_metadata:int -> ?max_responses:int ->
  ?max_command_metadata:int -> Transport.flow -> t
val close : t -> unit
val check_open : t -> unit
val has : t -> string -> bool
(** [has t name] holds when the latest CAPABILITY response listed [name],
    which must be uppercase. *)
val revision_two : t -> bool
(** [revision_two t] holds when IMAP4rev2 is in effect. The server advertises
    it and either omits IMAP4rev1 or accepted ENABLE IMAP4rev2. *)
val mailbox_mode : t -> Imap.Mailbox_name.mode
val mailbox_wire : t -> string -> string
(** [mailbox_wire t name] encodes the UTF-8 mailbox [name] for the wire in
    {!mailbox_mode}. It raises [Failure (State _)] for an invalid name. *)
val read_response : ?on_literal:(string -> unit) ->
  ?on_literal_start:(int64 -> unit) -> t -> Imap.Wire.event list
(** [read_response ?on_literal ?on_literal_start t] reads one response. With
    [on_literal], the payload of each [BODY[...]] or [BINARY[...]] literal in
    a FETCH response goes to [on_literal] after [on_literal_start] receives
    its length, and is not returned. Every other literal is returned and
    counts against the metadata limit. *)
val parse : Imap.Wire.event list -> Imap.Response.t

type command_result = {
  untagged : Imap.Response.t list;
  completion : Imap.Response.t;
  partial : (int64 * int64 option) option;
}

val command_result : ?on_literal:(string -> unit) ->
  ?on_literal_start:(int64 -> unit) -> ?mutation:bool -> ?accept_partial:bool ->
  ?saved_search_criterion:string -> t -> string -> command_result
val command : ?on_literal:(string -> unit) ->
  ?on_literal_start:(int64 -> unit) -> ?mutation:bool ->
  t -> string -> Imap.Response.t list
val compress_deflate : t -> unit
type append_part = { prefix : string; length : int64; read : Cstruct.t -> int; synchronizing : bool }
val append_many : t -> append_part list -> Imap.Response.t
val append : ?synchronizing:bool -> t -> prefix:string -> length:int64 ->
  _ Eio.Flow.source -> Imap.Response.t
val idle_once : t -> Imap.Response.t list
val io_failure : exn -> bool
(** [io_failure ex] holds for [Eio.Io], [Unix.Unix_error], [End_of_file] and
    TLS alerts and failures. *)
val protect : t -> (unit -> 'a) -> ('a, error) result
(** [protect t f] is [Ok (f ())]. A [Failure e] becomes [Error e]. An
    {!io_failure} closes [t] and becomes [Error (Transport _)]. Any other
    exception closes [t] and is re-raised with its backtrace. *)
val locked : t -> (unit -> 'a) -> ('a, error) result
val authenticate_cram_md5 : t -> Auth.t -> unit
val authenticate_initial : t -> mechanism:string -> encoded:string ->
  sasl_ir:bool -> oauthbearer:bool -> unit

val logout : t -> unit
