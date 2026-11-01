(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let positive n = if n > 0 then Some n else None

(* No terminal has more than the 4096 columns or rows a display takes. *)
let usable n = if n > 0 && n <= 4096 then Some n else None

let window_size = function
  | None -> None
  | Some fd -> (
      try Some (Eio_unix.Pty.get_window_size fd)
      with Unix.Unix_error _ -> None)

let width_of_ioctl fd () =
  Option.bind (window_size fd) (fun w -> positive w.cols)

let dimension_of_env name usable =
  Option.bind (Sys.getenv_opt name) (fun value ->
      Option.bind (int_of_string_opt value) usable)

let first ~default sources =
  Option.value ~default (List.find_map (fun source -> source ()) sources)

(* No [tput] fallback: it reads the termcap entry the shell read, at the
   cost of a fork. *)
let width fd =
  first ~default:80
    [ width_of_ioctl fd; (fun () -> dimension_of_env "COLUMNS" usable) ]

let height_of_ioctl fd () =
  Option.bind (window_size fd) (fun w -> positive w.rows)

let height fd =
  first ~default:24
    [ height_of_ioctl fd; (fun () -> dimension_of_env "LINES" usable) ]

let is_tty fd = Eio_unix.Fd.use_exn "isatty" fd Unix.isatty
let dimensions fd = (width fd, height fd)
