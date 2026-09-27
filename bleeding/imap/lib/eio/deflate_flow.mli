(** Internal RFC 4978 raw DEFLATE transport. Each direction is an independent,
    continuous stream. Writes sync-flush without emitting a final block.
    Input/output buffers are 64 KiB, with bounded codec queues and windows.
    Outbound LZ77 history spans one write and restarts at the next, because
    the upstream API has no LZ77 sync-flush. Inbound history persists across
    all blocks and reads. At most 16 MiB of compressed input may pass without
    decoded output. *)
type t

type Eio.Exn.err += Deflate of string
(** [Deflate message] reports a malformed or final compressed block, an
    exhausted input budget or use of a closed flow. [read] raises it as
    [Eio.Io], and [write] raises it for a closed flow. *)

val create :
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t -> t
(** Takes ownership at an exact compressed-stream boundary. *)
val read : t -> Cstruct.t -> int
val write : t -> Cstruct.t list -> unit
val close : t -> unit
(** [close t] closes the original resource once. Later calls do nothing. A
    codec or I/O failure, or cancellation during an operation, also closes
    it. Cancellation while an operation waits for its direction does not.
    Reads and writes may run concurrently, and operations in each direction
    are serialized. [close] waits for neither direction, so an operation
    running in another fiber fails when the flow closes. *)
