# Access log design

A `proffer-log` library records every `Proffer_httpz.event` in a SQLite
database, rotates old rows into archive files beside it and gives analytics
one view over the live file and every archive. Arod's `arod.log` library is
the first consumer and its existing database migrates in place. Sortal and
other servers that log only to `Logs` today can adopt the same file.

## Why

Arod's `access.db` has reached 4.6GB. The server runs `ANALYZE` over it
on every start before it listens, which the operator sees as a freeze of
up to a minute, and every checkpoint, backup and whole-table analytics
query pays for the size. The rows are dominated by the request header JSON
column. The logging code is arod-specific although nothing in it is about
the site.

## Library

`proffer-log` lives in `wip/proffer/log/` as module `Proffer_log`. It
depends on `proffer`, `proffer-httpz` for the event type, `sqlite3-eio`,
`jsont` and `jsont.bytesrw`. It is optional: `proffer` and `proffer-httpz`
do not depend on it, and a server without analytics keeps logging to `Logs`
as the [3-log example](../../examples/proffer/3-log/README.md) does.

```ocaml
type t

val open_ : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_ ~sw path] is the log at [path], created when absent and
    migrated when it holds an older schema. It closes with [sw]. *)

val record : t -> timestamp:float -> Proffer_httpz.event -> unit
(** [record t ~timestamp event] inserts [event] at Unix time [timestamp].
    The caller globalizes the event first, as the backend passes it at
    local. Inserts share one writer connection, so a server that serves
    from several domains must hand events to the domain that owns [t]. *)

val reader : t -> Sqlite3_eio.t
(** [reader t] is a read-only connection with every archive attached and
    the [requests_all] view defined. Analytics query [requests] for the
    live rows or [requests_all] for the whole history. *)

val refresh : t -> unit
(** [refresh t] rescans the archive directory and reattaches, so a page can
    see an archive written after the server started. *)

type rotation = { moved : int; archives : string list }

val rotate :
  t -> before:float -> ?batch:int -> ?progress:(int -> unit) -> unit ->
  rotation
(** [rotate t ~before ()] moves rows with a timestamp before [before] into
    one archive per calendar year, [NAME-YYYY.db] beside the live file,
    [batch] rows per transaction, 20000 by default. A running server keeps
    inserting between batches. *)

val merge : t -> from:_ Eio.Path.t -> int
(** [merge t ~from] copies every row of the log or archive at [from] into
    [t] and is the number of rows copied. Row ids are reassigned. *)

val vacuum : t -> unit
(** [vacuum t] rebuilds the live file so space freed by rotation returns
    to the filesystem. Run it with the server stopped. *)

val archives : t -> (string * float * float) list
(** [archives t] lists each attached archive with the timestamp range it
    holds. *)
```

The `Proffer_log_cmd` module in a `proffer-log.cmdliner` sub-library
provides `rotate`, `merge`, `vacuum` and `ls` terms over a path, so a server
exposes them as `NAME log rotate --keep DAYS`, `NAME log merge FILE`,
`NAME log vacuum` and `NAME log ls` without repeating the glue.

## Files and schema

The live file and its archives share one schema. The `requests` table keeps
arod's columns unchanged, so every existing analytics query still runs:

```sql
CREATE TABLE requests (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp REAL NOT NULL, remote_addr TEXT NOT NULL,
  forwarded_for TEXT, forwarded_proto TEXT,
  method TEXT NOT NULL, target TEXT NOT NULL, path TEXT NOT NULL,
  host TEXT, user_agent TEXT, referer TEXT, accept TEXT,
  request_headers TEXT,
  status_code INTEGER NOT NULL, response_content_type TEXT,
  response_body_size INTEGER NOT NULL, cache_status TEXT,
  duration_us INTEGER NOT NULL
);
CREATE TABLE log_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
```

`log_meta` holds `schema` (the version, 2), `role` (`live` or `archive`),
and for an archive `range_start` and `range_end` as the timestamps it may
hold. `PRAGMA user_version` carries the same version so a shell can read it.
The indexes are arod's four: timestamp, path, status with timestamp, and
the covering metrics index.

Every open runs `PRAGMA analysis_limit=1000` before `ANALYZE`, which
samples each index instead of scanning it. On the 4.6GB file that is the
difference between nine seconds and a fraction of a second warm, and far
more cold.

Archives are named from the live file: `access.db` has `access-2025.db`,
`access-2026.db`. The year is the UTC year of the row's timestamp, so a
file holds a closed range and rotation appends to this year's archive as the
retention window slides. A monthly `--period` is a later option if yearly
files grow too large; the names then carry a month.

## Reading across files

`reader` opens the live file read-only, attaches each `NAME-*.db` in the
directory as `a_2025` and so on, and creates a temporary view:

```sql
CREATE TEMP VIEW requests_all AS
  SELECT * FROM main.requests
  UNION ALL SELECT * FROM a_2025.requests
  UNION ALL SELECT * FROM a_2026.requests;
```

SQLite pushes a `WHERE timestamp >= ?` clause into each arm of a `UNION ALL`
view, so a range query uses every file's timestamp index and skips files
whose range the planner can prove empty from the index. An all-time query
reads every file, as it reads the whole table today. The attached limit is
10 files by default, which yearly archives reach in a decade; the bundled
SQLite can raise `SQLITE_MAX_ATTACHED` when needed.

A file that matches the name pattern but carries no `log_meta` row naming
it an archive, such as a copy made by hand, is detached again and left out
of the view, and `refresh` detaches an archive whose file has gone.

An archive is read-only through the reader because the reader connection
is read-only. The writer attaches an archive only during `rotate` and
`merge`, and detaches afterwards.

## Rotation

`rotate` walks the live table in id order. Each batch is one transaction
on the writer connection:

```sql
INSERT INTO a_2025.requests (timestamp, remote_addr, ...)
  SELECT timestamp, remote_addr, ... FROM main.requests
  WHERE id > ?last AND id <= ?hi AND timestamp < ?before
    AND timestamp >= ?year_start AND timestamp < ?year_end;
DELETE FROM main.requests
  WHERE id > ?last AND id <= ?hi AND timestamp < ?before;
```

Both statements carry the same year predicate, and the batch fails if the
counts differ, so a row leaves the live file only once an archive holds
it, and a timestamp no calendar can place stays where it is. The file is
in WAL mode, so the copy, a read, never blocks the server's inserts, and
the delete holds the write lock for one batch. Arod's inserts
wait up to five seconds, which a batch of twenty thousand rows never
approaches. The archive's `range_end` is raised as rows land in it. A batch
that fails leaves both files consistent because the insert and delete
share a transaction.

Freed pages are reused by later inserts, so the live file stops growing
once rotation runs regularly. `vacuum` is a one-time step after the first
rotation of a large file and needs the file quiet.

## Merge and migration

`merge` attaches the source and runs `INSERT INTO requests (columns) SELECT
columns FROM src.requests`, omitting `id`. Nothing refers to ids across
files, so reassignment is safe. A source at an older schema is migrated
through a temporary copy first, so merge accepts arod's current file.

`open_` migrates in place:

- Version 0, arod's current file, has `requests` with the version 2
  columns and no `log_meta`. Migration adds `log_meta`, drops the two
  status and cache indexes arod already drops, sets `user_version` and
  takes no time proportional to the data.
- A later version that changes columns adds them with `ALTER TABLE` where
  SQLite allows and rebuilds the table otherwise, inside one transaction,
  and records the new version. Archives migrate when attached by the
  writer, so a rotation after an upgrade brings every file to the same
  version before writing to it.

Arod's `access.db` therefore needs no copy or conversion: the first start
of a build with `proffer-log` upgrades it, and `arod log rotate --keep 90`
moves its history into yearly archives while the server runs. A `vacuum`
during a maintenance window then shrinks the live file.

## Arod changes

- `arod.log` keeps `Arod_stats` and the analytics handlers and drops
  `Arod_log`, calling `Proffer_log` from `arod_server.ml`.
- The analytics page and `arod stats` query `requests_all`; the live
  dashboard keeps `requests`, since its windows are an hour and a day.
- The stats handler calls `refresh` when it finds an archive newer than
  its last scan, so a rotation needs no restart.
- `arod log rotate`, `merge`, `vacuum` and `ls` come from
  `Proffer_log_cmd` over the XDG data path.

## Open points

- The event type lives in `proffer-httpz`. Moving it into `proffer` as a
  backend-neutral record would let the mock backend log too and drop the
  backend dependency from `proffer-log`. That is a separate change.
- Blanking `request_headers` in archived rows would shrink archives by most
  of their size, at the cost of the two analytics panels that scan that
  column over all time. Offer it as `rotate --strip-headers` once the
  panels can state their range.
- Serving from several domains needs the event handed to the writer's
  domain; see [TODO.md](../../TODO.md).

## Tests

`wip/proffer/log/test` covers, on temporary files:

- recording events and reading them back through `reader`;
- rotation of synthetic rows spanning three years into three archives,
  with counts in each file and through `requests_all` matching, and the
  live file holding only the retained window;
- rotation while another connection inserts, with no insert lost;
- merging an archive back and the row count restored;
- opening a hand-written version 0 file, then checking `user_version`,
  `log_meta` and that every row survives;
- `refresh` picking up an archive created after `reader` was opened.
