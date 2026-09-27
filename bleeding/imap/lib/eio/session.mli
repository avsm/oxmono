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
  mutable flow : Transport.flow;
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
val read_response : ?on_literal:(string -> unit) ->
  ?on_literal_start:(int64 -> unit) -> ?collect_literals:bool ->
  t -> Imap.Wire.event list
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
val protect : t -> (unit -> 'a) -> ('a, error) result
val locked : t -> (unit -> 'a) -> ('a, error) result
val authentication_rejected : tag:string -> status:[ `No | `Bad ] ->
  code:Imap.Response.code option -> exn
val authenticate_cram_md5 : t -> Auth.t -> unit
val authenticate_initial : t -> mechanism:string -> encoded:string ->
  sasl_ir:bool -> oauthbearer:bool -> unit

val logout : t -> unit
