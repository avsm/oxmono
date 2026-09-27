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
  | Unsupported of Imap.Capability.t
  | Not_enabled of Imap.Capability.t

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
  mutable capabilities : Imap.Capability.Set.t;
  mutable enabled : Imap.Capability.Set.t;
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

val has : t -> Imap.Capability.t -> bool
(** [has t c] holds when the latest CAPABILITY response listed [c], or
    {!revision_two} holds and [Imap.Capability.implied_by_rev2 c]. *)

val is_enabled : t -> Imap.Capability.t -> bool
(** [is_enabled t c] holds when an ENABLED response confirmed [c]. *)

val require : t -> Imap.Capability.t -> unit
(** [require t c] raises [Failure (Unsupported c)] unless [has t c]. *)

val require_enabled : t -> Imap.Capability.t -> unit
(** [require_enabled t c] raises [Failure (Not_enabled c)] unless
    [is_enabled t c]. *)

val revision_two : t -> bool
(** [revision_two t] holds when IMAP4rev2 is advertised and either IMAP4rev1
    is not or ENABLE IMAP4rev2 succeeded. *)

val mailbox_mode : t -> Imap.Mailbox_name.mode

val mailbox_wire : t -> string -> string
(** [mailbox_wire t name] encodes the UTF-8 [name] in {!mailbox_mode}, or
    raises [Failure (State _)] for an invalid name. *)

val read_response : ?on_literal:(string -> unit) ->
  ?on_literal_start:(int64 -> unit) -> t -> Imap.Wire.event list
(** [read_response ?on_literal ?on_literal_start t] reads one response,
    passing each FETCH [BODY[...]] or [BINARY[...]] literal to [on_literal]
    after its length to [on_literal_start] instead of returning it. *)

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

type append_part = {
  prefix : string;
  length : int64;
  read : Cstruct.t -> int;
  synchronizing : bool;
}

val append_many : t -> append_part list -> Imap.Response.t
val append : ?synchronizing:bool -> t -> prefix:string -> length:int64 ->
  _ Eio.Flow.source -> Imap.Response.t
val idle_once : t -> Imap.Response.t list

val io_failure : exn -> bool
(** [io_failure ex] holds for [Eio.Io], [Unix.Unix_error], [End_of_file] and
    TLS alerts and failures. *)

val protect : t -> (unit -> 'a) -> ('a, error) result
(** [protect t f] is [Ok (f ())], maps [Failure e] to [Error e] and an
    {!io_failure} to [Error (Transport _)], and closes [t] on any exception
    other than [Failure], re-raising one that is not an {!io_failure}. *)

val locked : t -> (unit -> 'a) -> ('a, error) result
val authenticate_cram_md5 : t -> Auth.t -> unit
val authenticate_initial : t -> mechanism:string -> encoded:string ->
  sasl_ir:bool -> oauthbearer:bool -> unit

val logout : t -> unit
