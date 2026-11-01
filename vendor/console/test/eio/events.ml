(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Reads JSON events on standard input with [Console.Task.Event.jsont], prints
    each line that does not decode or is not written back as it was, then the
    number of events read back. *)

module Event = Console.Task.Event

let () =
  let rec loop n =
    match In_channel.input_line stdin with
    | None -> n
    | Some line -> (
        match Jsont_bytesrw.decode_string Event.jsont line with
        | Ok e when String.equal (Event.to_string e) line -> loop (n + 1)
        | Ok e ->
            Fmt.pr "%s@.is written back as@.%s@." line (Event.to_string e);
            loop n
        | Error e ->
            Fmt.pr "%s@.does not decode: %s@." line e;
            loop n)
  in
  Fmt.pr "%d events read back@." (loop 0)
