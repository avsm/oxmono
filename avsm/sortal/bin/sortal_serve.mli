(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The [sortal serve] subcommand.

    It lives beside the executable rather than in [sortal.cmd] because it needs
    [sortal.web] and a backend, and the core library has no business depending
    on either. *)

val port_arg : int Cmdliner.Term.t
(** [port_arg] is [--port], defaulting to 8380. *)

val info : Cmdliner.Cmd.info
(** [info] describes the subcommand to cmdliner. *)

val cmd : port:int -> Xdge.t -> Eio_unix.Stdenv.base -> int
(** [cmd ~port xdg stdenv] serves the web UI on 127.0.0.1 at [port] until the
    process is interrupted, then returns an exit code.

    The listening address is loopback and not configurable: the UI has no
    authentication, so anyone who can reach the socket can read and change
    every contact.

    Writes go through the git-backed store when the data directory is a git
    repository, so each edit made in the browser lands as its own commit. *)
