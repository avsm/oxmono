(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Buf_read = Eio.Buf_read
module Buf_write = Eio.Buf_write

let max_line = 16 * 1024 * 1024

(* JSON text is UTF-8, and the peer's parser rejects a string that is not. The
   bytes we carry are file contents and command output, which are not always
   valid UTF-8, so an invalid sequence is replaced rather than sent. Sending it
   would turn one stray byte in the user's data into a fault that stops both
   ends. *)
let utf_8 s =
  let n = String.length s in
  let rec valid i =
    if i >= n then true
    else
      let d = String.get_utf_8_uchar s i in
      Uchar.utf_decode_is_valid d && valid (i + Uchar.utf_decode_length d)
  in
  if valid 0 then s
  else begin
    let b = Buffer.create n in
    let i = ref 0 in
    while !i < n do
      let d = String.get_utf_8_uchar s !i in
      (* An invalid decode yields [Uchar.rep], which is U+FFFD. *)
      Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d);
      i := !i + Uchar.utf_decode_length d
    done;
    Buffer.contents b
  end

let write line w text =
  Buf_write.string w (line text);
  Buf_write.char w '\n'

(* How much of an over-long line to hand back. Enough to name the message it
   claimed to be, short enough to print. *)
let bad_prefix = 200

(* A line over the reader's buffer leaves the reader part way through it, and
   there is no way back to a message boundary. The caller is told what it saw
   and is expected to stop, which is what any other fault asks of it too. *)
let over_long r =
  let seen = Buf_read.peek r in
  let len = min bad_prefix (Cstruct.length seen) in
  Printf.sprintf "%s... (line over the %d byte limit)"
    (Cstruct.to_string ~len seen)
    max_line

let read of_line r =
  if Buf_read.at_end_of_input r then `Eof
  else
    match Buf_read.line r with
    | line -> (
        match of_line line with Some msg -> `Msg msg | None -> `Bad line)
    | exception Buf_read.Buffer_limit_exceeded -> `Bad (over_long r)
