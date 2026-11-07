(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** SQLite access log for Proffer servers.

    A log is a SQLite file holding one row per {!Proffer_httpz.event}. Old
    rows rotate into archive files beside it, one per calendar year, and a
    read-only connection sees the live file and every archive through one
    view. See [ACCESS_LOG_DESIGN.md] for the schema and the cascade of
    files.

    The live file and its archives share the schema. Opening a file at an
    older schema migrates it in place. *)

type t
(** A live access log and its archives. *)

val schema_version : int
(** [schema_version] is the version written to [PRAGMA user_version] and
    the [schema] key of [log_meta]. *)

val open_ : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_ ~sw path] is the log at [path], created when absent and migrated
    when it holds an older schema. The writer connection closes with [sw].
    Archives are files named [STEM-SUFFIX.db] in the same directory, where
    [STEM.db] is the file name of [path].

    @raise Failure when the file holds a newer schema than this library. *)

val record : t -> timestamp:float -> Proffer_httpz.event -> unit
(** [record t ~timestamp event] inserts [event] at Unix time [timestamp].
    The backend passes an event at local, so a caller globalizes it first.
    Inserts share one writer connection: a server serving from several
    domains must hand events to the domain that owns [t]. *)

val reader : t -> Sqlite3_eio.t
(** [reader t] is a read-only connection on which [requests] is the live
    table and [requests_all] is a view over the live table and every
    archive attached at the last {!refresh}. The first call opens the
    connection and attaches the archives present then. A file that matches
    the archive name pattern but was not written by {!rotate} is ignored.
    SQLite attaches at most ten files unless built with a higher limit,
    which yearly archives reach after a decade. *)

val refresh : t -> unit
(** [refresh t] rescans the directory, attaches archives written since the
    reader was opened and detaches archives that were removed. It is cheap
    when nothing changed, so a page can call it on each request. *)

val reader_of_path : sw:Eio.Switch.t -> _ Eio.Path.t -> Sqlite3_eio.t
(** [reader_of_path ~sw path] is a read-only connection as {!reader} gives,
    over the log at [path] and its archives, for a tool that does not
    write. It closes with [sw]. *)

type archive = {
  file : string;  (** [file] is the archive's file name. *)
  range_start : float;  (** [range_start] is its earliest timestamp. *)
  range_end : float;  (** [range_end] is its latest timestamp. *)
  rows : int;  (** [rows] is its row count. *)
}

val archives : t -> archive list
(** [archives t] describes the archives attached to the reader, oldest
    first. It refreshes first. *)

val live_rows : t -> int
(** [live_rows t] is the row count of the live table. *)

type rotation = {
  moved : int;  (** [moved] is the number of rows moved. *)
  written : string list;  (** [written] names the archives written to. *)
}

val rotate :
  t -> before:float -> ?batch:int -> ?progress:(int -> unit) -> unit ->
  rotation
(** [rotate t ~before ()] moves every row with a timestamp before [before]
    into the archive for the UTC year of its timestamp, [batch] rows per
    transaction, 20000 by default, calling [progress] with the running
    total after each. Each batch copies rows into the archive and deletes
    them from the live file in one transaction, so an interrupted rotation
    leaves every file consistent and resumes where it stopped when run
    again. A timestamp that yields no calendar year stays in the live
    file. Call it from a maintenance process: another process inserting
    into the same file waits only for the delete of one batch, whereas an
    insert through [t] itself during a batch would join its transaction. *)

val merge : t -> from:_ Eio.Path.t -> int
(** [merge t ~from] copies every row of the log or archive at [from] into
    the live table of [t] and is the number of rows copied. Row ids are
    reassigned. [from] may hold any schema version with the [requests]
    columns. *)

val vacuum : t -> unit
(** [vacuum t] rebuilds the live file so that space freed by rotation
    returns to the filesystem. Run it with no other connection open. *)
