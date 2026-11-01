(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON responses. *)

let encode codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error _ -> {|{"error":"JSON encoding failed"}|}

(* jsont hands the writer a slice at a time. A slice is bytes with an offset
   and a length, which is exactly what the sink's byte path takes, so the
   encoded body never becomes a string. The slice length is the sink's chunk,
   and 64KB matches what the httpz backend writes through. *)
let stream codec v sink =
  let w =
    Bytesrw.Bytes.Writer.make ~slice_length:65536 (fun slice ->
        if not (Bytesrw.Bytes.Slice.is_eod slice) then
          Proffer.Body.Sink.write_sub sink
            (Bytesrw.Bytes.Slice.bytes slice)
            ~off:(Bytesrw.Bytes.Slice.first slice)
            ~len:(Bytesrw.Bytes.Slice.length slice))
  in
  match Jsont_bytesrw.encode codec v ~eod:true w with
  | Ok () -> ()
  | Error e -> failwith ("JSON encoding failed: " ^ e)
