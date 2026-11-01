(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a program writes on a pseudo-terminal, read to its end without a
    timeout. *)

val read_all :
  ?on_read:(string -> unit) -> Eio_unix.Pty.t -> (unit -> unit) -> string
(** [read_all pty ended] is what is written on [pty] until [ended ()] returns,
    which it does once the program writing there has exited, and [on_read]
    (defaults to doing nothing) is called with what was read so far after each
    read. A mark is then written on the terminal, after all the program wrote,
    and the reader stops at it: no read is cut short and no wait depends on
    time. *)
