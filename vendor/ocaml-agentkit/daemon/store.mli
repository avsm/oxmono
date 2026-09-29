(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The directory numpty owns.

    Everything a run writes lives under one root, [$XDG_STATE_HOME/numpty] by
    default.

    {v
    journal/2026-08-08.jsonl    one record per line, append only
    memory/000017.json          an immutable snapshot of the whole memory
    memory/current              the version in force
    workspace/                  the directory the agent's file tools hold
    control                     the socket a running numpty answers on
    lock                        held for the life of a run
    v}

    The schedule is not here. It is [$XDG_CONFIG_HOME/numpty/schedule.json],
    written by a person and only read by numpty, so nothing numpty does can
    rewrite the instructions it was given.

    There is no counter file. The next [seq] and [run] come from the journal and
    the memory version from [memory/current], since a counter kept beside the
    journal could disagree with it after a crash between the two writes. *)

type dir = Eio.Fs.dir_ty Eio.Path.t

(** {1 Where things are}

    These take a root rather than an open store, so a command that only reads
    the files, such as [numpty log], reaches them without taking the lock. *)

val default_root : Xdge.t -> dir
(** [default_root xdg] is [$XDG_STATE_HOME/numpty], which [--store] overrides.
*)

val schedule_file : Xdge.t -> dir
(** [schedule_file xdg] is [$XDG_CONFIG_HOME/numpty/schedule.json]. It is not
    under the store, because the daemon reads it and never writes it. *)

val journal_dir : _ Eio.Path.t -> dir
(** [journal_dir root] is where the journal segments are. *)

val memory_dir : _ Eio.Path.t -> dir
(** [memory_dir root] is where the memory versions are. *)

val workspace_dir : _ Eio.Path.t -> dir
(** [workspace_dir root] is the directory the agent's file tools are rooted at.
    There is no [--dir]. An unattended agent does not share a tree a person is
    editing. *)

val control_path : _ Eio.Path.t -> dir
(** [control_path root] is the unix socket a running numpty answers on. *)

val lock_path : _ Eio.Path.t -> dir
(** [lock_path root] is the file a run holds for its life. *)

val journalled_version : _ Eio.Path.t -> int
(** [journalled_version root] is the highest memory version any record in the
    journal names, and 0 if none does. It is what {!Agentkit.Memory.recover}
    settles the memory store against. *)

(** {1 A store open for a run} *)

exception Locked of { path : string; pid : int }
(** Raised by {!open_} where another run holds the lock, naming that run's
    process id. Two runs sharing a store would interleave journal lines and race
    the version counter, and the engine admits one model per process anyway, so
    this costs nothing that was available. *)

type t
(** A store open for one run, holding the lock, the journal and the memory. *)

val open_ : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> _ Eio.Path.t -> t
(** [open_ ~sw ~clock root] takes the lock on [root] and returns the store,
    creating [root] and its directories if they are absent.

    The lock is a POSIX record lock, so a run that crashed holds nothing and the
    next one starts. A live run does hold it, and a second one is refused with
    {!Locked} naming the first one's process id.

    It then recovers what a crash left. The journal supplies the next [seq] and
    [run]. A memory snapshot the journal never named was never in force and is
    removed, and one the journal did name is adopted, each with an
    {!Agentkit.Journal.Error} record saying which it was. A socket file left by
    a crashed run belongs to nobody, the lock being taken first, and is
    unlinked.

    It raises {!Locked}, {!Agentkit.Journal.Bad_record} for a journal whose end
    cannot be read, or whatever a write raised. The run stops on any of them,
    since a store that cannot be settled is one no account can be kept in.

    The lock is released and the journal closed when [sw] finishes, so a run
    that unwinds leaves nothing behind. *)

val root : t -> dir
(** [root t] is the directory [t] was opened on. *)

val journal : t -> Agentkit.Journal.t
(** [journal t] is the journal open for appending, at the run and sequence
    number recovery gave it. *)

val memory : t -> Agentkit.Memory.t
(** [memory t] is the memory store, settled against the journal. *)

val workspace : t -> dir
(** [workspace t] is the directory the agent's file tools are rooted at. *)

val close : t -> unit
(** [close t] closes the journal and releases the lock. Closing twice does
    nothing. *)
