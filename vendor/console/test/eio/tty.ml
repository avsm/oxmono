(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [tty.exe program args] runs [program] on a terminal of 80 columns and 24
   rows, in the environment of [tty.exe], and prints the screen it leaves,
   the lines scrolled off its top first; [tty.exe -] prints the screen that
   the bytes of its standard input leave, and [tty.exe --cast file] the one
   that the asciicast recording [file] leaves on a terminal of its size.
   Where no pseudo-terminal can be opened, such as in opam's sandbox on
   macOS, it exits with 77. *)

let rows = 24
let cols = 80

let environment () =
  let ours = [ "TERM=xterm-256color"; "LANG=en_US.UTF-8"; "NO_COLOR=" ] in
  let name b = List.hd (String.split_on_char '=' b) in
  let names = List.map name ours in
  Array.of_list
    (ours
    @ List.filter
        (fun b -> not (List.mem (name b) names))
        (Array.to_list (Unix.environment ())))

let run argv =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = Eio_unix.Pty.open_pty ~sw () in
  Eio_unix.Pty.set_window_size (Eio_unix.Pty.pty pty)
    { rows; cols; xpixel = 0; ypixel = 0 };
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:(environment ())
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:(List.hd argv) argv
  in
  Pty_reader.read_all pty (fun () ->
      ignore (Eio.Process.await child : Eio.Process.exit_status))

let screen ?(rows = rows) ?(cols = cols) bytes =
  let vte = Console_vte.v ~rows ~cols () in
  List.iter (Console_vte.feed vte) bytes;
  List.iter print_endline (Console_vte.screen ~scrollback:true vte)

let decode t line =
  match Jsont_bytesrw.decode_string t line with
  | Ok v -> v
  | Error e -> failwith e

let cast file =
  match In_channel.with_open_bin file In_channel.input_lines with
  | [] -> failwith (file ^ ": empty")
  | header :: events ->
      let { Console__.Cast.width; height; _ } =
        decode Console__.Cast.header header
      in
      screen ~rows:height ~cols:width
        (List.map (fun l -> (decode Console__.Cast.event l).data) events)

let () =
  match Array.to_list Sys.argv with
  | [ _; "-" ] -> screen [ In_channel.input_all stdin ]
  | [ _; "--cast"; file ] -> cast file
  | _ :: (_ :: _ as argv) -> (
      match run argv with
      | bytes -> screen [ bytes ]
      | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
          prerr_endline "tty.exe: no pseudo-terminal";
          exit 77)
  | _ ->
      prerr_endline "usage: tty.exe PROGRAM [ARG...]";
      exit 2
