[@@@alert "-deprecated"]

(** {1 chdb -- ClickHouse Embedded Database for OCaml}

    High-performance bindings to
    {{:https://clickhouse.com/chdb}chdb} with zero-copy result
    access, unboxed statistics, and streaming queries.

    {2 Quick start}

    {[
      let () =
        Chdb.Connection.with_connection ~f:(fun conn ->
          let s = Chdb.Query.execute_string conn ~format:TSV
                    "SELECT number, number * number AS sq
                     FROM numbers(10)" in
          print_string s) ()
    ]}

    {2 Zero-copy binary access}

    {[
      Chdb.Query.with_buffer conn ~format:RowBinary
        "SELECT toUInt64(number) FROM numbers(1000)"
        ~f:(fun buf ->
          let sum = Chdb.Binary.sum_uint64 buf in
          Printf.printf "sum = %Ld\n" sum)
    ]}

    {2 Domain-safe access}

    {[
      let (Chdb.Safe.Pack conn) = Chdb.Safe.create () in
      Chdb.Safe.with_lock conn ~f:(fun password ->
        let s = Chdb.Safe.query_string conn ~password ~format:TSV
                  "SELECT 1 + 1" in
        print_string s)
    ]} *)

(** {2 Errors} *)

module Error = Chdb_error

(** {2 Output formats} *)

module Format = Chdb_format

(** {2 Query statistics} *)

module Stats = Chdb_stats

(** {2 Results} *)

module Result = Chdb_result

(** {2 Connections} *)

module Connection = Chdb_connection

(** {2 Query execution} *)

module Query = Chdb_query

(** {2 Streaming} *)

module Stream = Chdb_stream

(** {2 Binary format parsing}

    High-performance readers for ClickHouse RowBinary format
    using {!Base_bigstring} with optimized accessors. *)

module Binary = Chdb_binary

(** {2 Domain-safe connections}

    Capsule-based access control for compile-time data-race
    safety.  See {!Chdb_safe} for details. *)

module Safe = Chdb_safe
