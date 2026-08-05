(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bytesrw adapters for Eio

    This module provides adapters to create {!Bytesrw.Bytes.Reader.t} and
    {!Bytesrw.Bytes.Writer.t} from Eio flows, mirroring the API of
    {!Bytesrw_unix} for Eio's effect-based I/O. *)

open Bytesrw

(** Create a [Bytes.Reader.t] from an Eio source flow.

    Reads directly from the flow without intermediate buffering.

    @param slice_length
      Maximum bytes per slice (default: 65536, which is
      {!Bytes.Slice.unix_io_buffer_size}) *)
let bytes_reader_of_flow ?(slice_length = Bytes.Slice.unix_io_buffer_size)
    (flow : _ Eio.Flow.source) : Bytes.Reader.t =
  let buf_size = Bytes.Slice.check_length slice_length in
  let read () =
    let cstruct = Cstruct.create buf_size in
    match Eio.Flow.single_read flow cstruct with
    | 0 -> Bytes.Slice.eod
    | count ->
        let data_cs = Cstruct.sub cstruct 0 count in
        let buf = Cstruct.to_bytes data_cs in
        Bytes.Slice.make buf ~first:0 ~length:count
    | exception End_of_file -> Bytes.Slice.eod
  in
  Bytes.Reader.make ~slice_length read

(** Create a [Bytes.Writer.t] from an Eio sink flow.

    Writes directly to the flow without intermediate buffering.

    @param slice_length
      Suggested slice length for upstream (default: 65536, which is
      {!Bytes.Slice.unix_io_buffer_size}) *)
let bytes_writer_of_flow ?(slice_length = Bytes.Slice.unix_io_buffer_size)
    (flow : _ Eio.Flow.sink) : Bytes.Writer.t =
  let rec write slice =
    if Bytes.Slice.is_eod slice then ()
    else begin
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      let length = Bytes.Slice.length slice in
      let cstruct = Cstruct.of_bytes ~off:first ~len:length bytes in
      match Eio.Flow.single_write flow [ cstruct ] with
      | count when count = length -> ()
      | count -> write (Option.get (Bytes.Slice.drop count slice))
    end
  in
  Bytes.Writer.make ~slice_length write
