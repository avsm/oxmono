(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner commands for maintaining a {!Proffer_log} file.

    A server adds [group] to its command list and gains [NAME log rotate],
    [NAME log merge], [NAME log vacuum] and [NAME log ls] over the file its
    [path] function names. Each command runs its own Eio main loop. *)

type path = Eio_unix.Stdenv.base -> Eio.Fs.dir_ty Eio.Path.t
(** A [path] names the live log file from the process environment. *)

val group :
  ?setup:unit Cmdliner.Term.t -> ?name:string -> path:path -> unit ->
  int Cmdliner.Cmd.t
(** [group ~path ()] is the [log] command group, named [name] when given.
    [setup] is evaluated before every subcommand, for logging options. *)

val commands : ?setup:unit Cmdliner.Term.t -> path:path -> unit ->
  int Cmdliner.Cmd.t list
(** [commands ~path ()] is the subcommands of {!group}, for a server that
    arranges them itself. *)
