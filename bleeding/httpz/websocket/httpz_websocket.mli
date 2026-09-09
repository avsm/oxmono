(* SPDX-License-Identifier: ISC *)

(** RFC 6455 handshakes and bounded WebSocket connections over borrowed bytes.

    This library performs no network discovery. Callers supply an HTTP upgrade
    and byte transport, with TLS, timeouts and access policy where appropriate.
    It supports version 13 without extensions or compression. *)

@@ portable

type role = Client | Server
type kind = Text | Binary

module Handshake : sig
  type headers = (string * string) list

  val request : ?protocols:string list -> nonce:string -> unit -> headers
  (** [request ~nonce ()] supplies the upgrade headers. [nonce] must contain
      16 fresh, cryptographically random bytes. The caller adds Host and sends
      an HTTP/1.1 GET. Invalid nonce lengths or protocol tokens raise
      [Invalid_argument]. No extension is offered. *)

  val accept :
    ?protocol:string -> meth:string -> http_1_1:bool -> headers ->
    (headers, string) result
  (** [accept ~meth ~http_1_1 headers] validates a request and supplies response
      headers for status 101. [headers] must preserve duplicate fields.
      [protocol], when supplied, must occur in the client's offer.
      The HTTP parser must already have checked general field syntax and
      authority. The application checks Origin and authorization before
      accepting. Extension offers are declined. *)

  val verify :
    ?protocols:string list -> key:string -> status:int -> headers ->
    (string option, string) result
  (** [verify ~key ~status headers] validates the response to {!request},
      returning the selected subprotocol. [key] is the encoded request key.
      Unexpected extensions and subprotocols are errors. The HTTP layer must
      check HTTP/1.1, preserve buffered bytes and hand over only after success.
      Redirects and authentication retries belong to the caller's policy. *)
end

module Frame : sig
  type opcode = Continuation | Text | Binary | Close | Ping | Pong
  type status = Complete | Partial | Malformed | Too_large
  type header =
    #{ fin : bool; opcode : opcode; length : int; masked : bool;
       mask : int; header_length : int }

  val parse :
    role:role -> max_payload:int -> bytes -> off:int -> len:int ->
    #(status * header)
  [@@zero_alloc]
  (** [parse ~role ~max_payload bytes ~off ~len] validates a frame header.
      [len] is a byte count. Payload bytes need not be present. Metadata is
      meaningful only on [Complete]. Invalid ranges return [Malformed].
      [role] is the local role, so servers require masked input and clients
      require unmasked input. Lengths must use the shortest encoding. *)

  val write :
    bytes -> off:int -> fin:bool -> opcode:opcode -> length:int ->
    masked:bool -> mask:int -> int
  [@@zero_alloc]
  (** [write bytes ~off ~fin ~opcode ~length ~masked ~mask] writes a header and
      returns its length. Invalid ranges and control frames raise
      [Invalid_argument]. [mask] holds four bytes in network order. *)

  val mask : bytes -> off:int -> len:int -> key:int -> offset:int -> unit
  [@@zero_alloc]
  (** [mask bytes ~off ~len ~key ~offset] XORs the range in place. [offset] is
      the byte position within the frame payload, for chunked masking. *)
end

type t
(** A connection belongs to one domain, with one receiving fiber. *)

exception Protocol_error of int * string
(** [Protocol_error (code, reason)] terminates a connection. Codes include
    1002 for framing, 1007 for UTF-8, 1009 for size bounds and 1006 for EOF
    without a close frame. No further I/O is permitted through that [t]. *)

val create :
  role:role ->
  ?max_message:int ->
  ?max_fragments:int ->
  ?random:(bytes -> off:int -> len:int -> unit) ->
  read:(bytes -> off:int -> len:int -> int) ->
  write:(bytes -> off:int -> len:int -> unit) ->
  with_write_lock:((unit -> unit) @ local -> unit) ->
  unit -> t
(** [create ~role ~read ~write ~with_write_lock ()] uses an already upgraded
    connection. [read] returns zero on EOF. [write] writes the entire slice
    before returning, without retaining or mutating it. Both must use bounded
    I/O timeouts and preserve bytes buffered during HTTP parsing.

    [with_write_lock] serializes writes, including automatic pong and close
    replies, against application sends. It must invoke its callback exactly
    once before returning. Use a fiber mutex for concurrent send/receive.
    Transport or lock exceptions invalidate the connection and propagate.

    Clients must supply [random], which fills fresh cryptographically random
    mask bytes on every frame. Servers do not use it. [max_message] defaults
    to 16 MiB and [max_fragments] to 1024. Storage grows on demand within the
    message bound. Invalid limits or missing client randomness raise
    [Invalid_argument]. *)

val receive :
  t -> f:(kind -> bytes -> off:int -> len:int -> unit) @ local -> bool
(** [receive t ~f] calls [f] with one complete validated text or binary message
    and returns [true]. The slice is borrowed until [f] returns. It must not
    be retained or mutated. Fragmented messages are assembled in the reusable
    receive buffer. Ping receives a pong and pong is ignored.

    A valid close is echoed if necessary and returns [false]. Further calls
    then return [false]. After {!close}, data is consumed without delivery
    until the peer closes. Invalid input raises {!Protocol_error}. The caller
    must close the underlying transport when finished, on error or timeout. *)

val send : t -> kind -> bytes -> off:int -> len:int -> unit
(** [send t kind bytes ~off ~len] writes one final data frame. Server output
    borrows the payload directly. Client output masks through reusable scratch
    storage without modifying the payload. Text must be UTF-8. *)

val ping : t -> bytes -> off:int -> len:int -> unit
(** [ping t bytes ~off ~len] writes a ping of at most 125 bytes. *)

val close : t -> ?code:int -> ?reason:string -> unit -> unit
(** [close t ()] starts the close handshake, defaulting to code 1000. Continue
    {!receive} under a deadline to await the peer, then close the transport.
    Repeated calls have no effect. Invalid codes, UTF-8, or a reason longer
    than 123 bytes raise [Invalid_argument]. *)
