(** ClickHouse output formats for query results.

    Controls the serialization format of query results returned by
    {!Chdb_query.execute} and related functions.  The default is
    {!CSV}.

    See the
    {{:https://clickhouse.com/docs/en/interfaces/formats}ClickHouse formats documentation}
    for detailed format descriptions. *)

type t =
  | CSV                  (** Comma-separated values *)
  | CSVWithNames         (** CSV with a header row of column names *)
  | TSV                  (** Tab-separated values (alias: TabSeparated) *)
  | TSVWithNames         (** TSV with a header row of column names *)
  | JSON                 (** Full JSON object with metadata and statistics *)
  | JSONEachRow          (** One JSON object per row, newline-delimited *)
  | JSONCompact          (** Compact JSON with arrays instead of objects *)
  | JSONCompactEachRow   (** Compact JSON, one array per row *)
  | Native               (** ClickHouse native binary format *)
  | RowBinary            (** Compact binary row format for zero-copy access *)
  | RowBinaryWithNames   (** RowBinary preceded by column names *)
  | Arrow                (** Apache Arrow IPC streaming format *)
  | Parquet              (** Apache Parquet columnar format *)
  | Pretty               (** Human-readable table with Unicode borders *)
  | PrettyCompact        (** Compact human-readable table *)
  | Vertical             (** Each column on its own line per row *)
  | Null                 (** Discard output (useful for DDL / INSERT benchmarks) *)
  | Custom of string     (** Any other ClickHouse format by name *)

val to_string : t -> string
(** [to_string fmt] returns the ClickHouse format name string
    (e.g. ["CSV"], ["JSONEachRow"]). *)

val pp : Format.formatter -> t -> unit
(** [pp ppf fmt] pretty-prints the format name. *)
