(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal geometry and tty detection.

    Eio has no [isatty], so that is the smallest wrapper over Unix; the window
    size is Eio's. *)

val width : Eio_unix.Fd.t option -> int
(** [width fd] is the width in columns of the terminal behind [fd]: the
    [TIOCGWINSZ] ioctl on [fd] if positive, else [$COLUMNS] if it is in
    \[[1];[4096]\], else [80]. [None] asks no ioctl. *)

val height : Eio_unix.Fd.t option -> int
(** [height fd] is like {!width} for the height in rows, from [$LINES], else
    [24]. *)

val is_tty : Eio_unix.Fd.t -> bool
(** [is_tty fd] is [true] iff [fd] is connected to a terminal. *)

val dimensions : Eio_unix.Fd.t option -> int * int
(** [dimensions fd] is [(width fd, height fd)]. *)
