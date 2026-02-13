[@@@alert "-deprecated"]

(** {1 chdb -- ClickHouse Embedded Database for OCaml}

    High-performance bindings to chdb with zero-copy result access,
    unboxed statistics, and streaming queries. *)

module Error = Chdb_error
module Format = Chdb_format
module Stats = Chdb_stats
module Result = Chdb_result
module Connection = Chdb_connection
module Query = Chdb_query
module Stream = Chdb_stream
module Binary = Chdb_binary
module Safe = Chdb_safe
