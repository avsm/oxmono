(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let end_mark = "<end of the terminal's output>"

let ends_with_mark b =
  let n = Buffer.length b and m = String.length end_mark in
  n >= m && String.equal (Buffer.sub b (n - m) m) end_mark

let read_all ?(on_read = ignore) pty ended =
  let written = Buffer.create 4096 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  let rec read () =
    if not (ends_with_mark written) then
      match Eio.Flow.single_read source buf with
      | n ->
          Buffer.add_string written (Cstruct.to_string ~len:n buf);
          on_read (Buffer.contents written);
          read ()
      | exception (End_of_file | Eio.Io _) -> ()
  in
  let mark () =
    ended ();
    let tty = Eio_unix.Fd.use_exn "dup" (Eio_unix.Pty.tty pty) Unix.dup in
    Eio_unix.run_in_systhread ~label:"end mark" (fun () ->
        Fun.protect
          ~finally:(fun () -> Unix.close tty)
          (fun () ->
            ignore
              (Unix.write_substring tty end_mark 0 (String.length end_mark)
                : int)))
  in
  Eio.Fiber.both read mark;
  let n = Buffer.length written - String.length end_mark in
  if ends_with_mark written then Buffer.sub written 0 n
  else Buffer.contents written
