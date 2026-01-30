(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bytesrw adapters for Eio

    This module provides adapters to create {!Bytesrw.Bytes.Reader.t} and
    {!Bytesrw.Bytes.Writer.t} from Eio flows, mirroring the API of
    [Bytesrw_unix] for Eio's effect-based I/O.

    Unlike the Buf_read/Buf_write wrappers, these adapters read and write
    directly to the flow, allowing bytesrw to handle its own buffering. *)

(** {1 Readers} *)

val bytes_reader_of_flow :
  ?slice_length:int -> _ Eio.Flow.source -> Bytesrw.Bytes.Reader.t
(** [bytes_reader_of_flow flow] creates a reader from an Eio source flow.

    Reads directly from the flow without intermediate buffering.

    @param slice_length Maximum bytes per slice (default: 65536) *)

(** {1 Writers} *)

val bytes_writer_of_flow :
  ?slice_length:int -> _ Eio.Flow.sink -> Bytesrw.Bytes.Writer.t
(** [bytes_writer_of_flow flow] creates a writer from an Eio sink flow.

    Writes directly to the flow without intermediate buffering.

    @param slice_length Suggested slice length for upstream (default: 65536) *)
