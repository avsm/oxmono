(** Internal RFC 4978 raw DEFLATE transport. Each direction is an independent,
    continuous stream. Writes sync-flush without emitting a final block.
    Input/output buffers are 64 KiB, with bounded codec queues and windows.
    Outbound LZ77 history restarts per 64 KiB chunk (the upstream API has no
    LZ77 sync-flush); inbound history persists across all blocks and reads.
    At most 16 MiB of compressed input may pass without decoded output. *)
type t
val create :
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t -> t
(** Takes ownership at an exact compressed-stream boundary. *)
val read : t -> Cstruct.t -> int
val write : t -> Cstruct.t list -> unit
val close : t -> unit
(** Idempotent; closes the original resource once. Codec/I/O failures and
    cancellation close the resource. Concurrent reads and writes are allowed,
    with operations in each direction serialized independently. *)
