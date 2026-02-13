(* test_chdb.ml -- Comprehensive test suite for chdb OCaml bindings *)

open Chdb

let with_conn f = Connection.with_connection ~f ()

(* ---------- Connection tests ---------- *)

let test_connect () =
  let conn = Connection.connect () in
  Alcotest.(check bool) "connection is open" true
    (Connection.is_open conn);
  Connection.close conn;
  Alcotest.(check bool) "connection is closed" false
    (Connection.is_open conn)

let test_connect_close_idempotent () =
  let conn = Connection.connect () in
  Connection.close conn;
  Connection.close conn;
  Alcotest.(check bool) "still closed" false
    (Connection.is_open conn)

let test_with_connection () =
  let result : int = with_conn (fun conn ->
    Alcotest.(check bool) "open inside" true
      (Connection.is_open conn);
    42)
  in
  Alcotest.(check int) "with_connection returns value" 42 result

let test_with_connection_exception () =
  let raised = ref false in
  (try with_conn (fun _conn -> failwith "test exception")
   with Failure _ -> raised := true);
  Alcotest.(check bool) "exception propagated" true !raised

let test_use_after_close () =
  let conn = Connection.connect () in
  Connection.close conn;
  Alcotest.check_raises "use after close"
    (Error.E Error.Use_after_close)
    (fun () -> ignore (Query.execute conn "SELECT 1"))

(* ---------- Basic query tests ---------- *)

let test_select_one () =
  with_conn (fun conn ->
    let s = Query.execute_string conn "SELECT 1" in
    Alcotest.(check string) "select 1" "1\n" s)

let test_select_string () =
  with_conn (fun conn ->
    let s = Query.execute_string conn "SELECT 'hello'" in
    Alcotest.(check string) "select hello" "\"hello\"\n" s)

let test_select_number () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:TSV "SELECT 42" in
    Alcotest.(check string) "select 42" "42\n" s)

let test_select_multiple_rows () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:TSV
              "SELECT number FROM numbers(5)" in
    Alcotest.(check string) "numbers 0-4" "0\n1\n2\n3\n4\n" s)

let test_select_multiple_columns () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:TSV
              "SELECT 1 AS a, 'b' AS b, 3.14 AS c" in
    let has_tab = String.contains s '\t' in
    Alcotest.(check bool) "has tab separator" true has_tab)

(* ---------- Format tests ---------- *)

let test_format_csv () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:CSV "SELECT 1, 2, 3" in
    Alcotest.(check bool) "CSV has comma" true (String.contains s ','))

let test_format_csv_with_names () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:CSVWithNames
              "SELECT 1 AS a, 2 AS b" in
    Alcotest.(check bool) "has header" true
      (String.length s > 0 && s.[0] = '"'))

let test_format_json () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:JSON "SELECT 1 AS n" in
    Alcotest.(check bool) "JSON has brace" true (String.contains s '{'))

let test_format_json_each_row () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:JSONEachRow
              "SELECT number AS n FROM numbers(3)" in
    let lines =
      String.split_on_char '\n' s
      |> List.filter (fun l -> String.length l > 0)
    in
    Alcotest.(check int) "3 JSON lines" 3 (List.length lines))

let test_format_pretty () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:Pretty
              "SELECT 1 AS n" in
    Alcotest.(check bool) "Pretty non-empty" true (String.length s > 0))

let test_format_vertical () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:Vertical
              "SELECT 1 AS n, 2 AS m" in
    Alcotest.(check bool) "Vertical has colon" true
      (String.contains s ':'))

let test_format_null () =
  with_conn (fun conn ->
    Query.execute_ignore conn "SELECT number FROM numbers(1000)";
    (* Should not raise *) ())

(* ---------- Result access tests ---------- *)

let test_result_length () =
  with_conn (fun conn ->
    let result = Query.execute conn ~format:TSV "SELECT 42" in
    Alcotest.(check bool) "length > 0" true (Result.length result > 0);
    Result.destroy result)

let test_result_is_empty () =
  with_conn (fun conn ->
    let result = Query.execute conn ~format:TSV
                   "SELECT number FROM numbers(0)" in
    Alcotest.(check bool) "empty result" true (Result.is_empty result);
    Result.destroy result)

let test_result_to_string () =
  with_conn (fun conn ->
    let result = Query.execute conn ~format:TSV "SELECT 'test'" in
    let s = Result.to_string result in
    Alcotest.(check string) "to_string" "test\n" s;
    Result.destroy result)

let test_result_to_bigstring () =
  with_conn (fun conn ->
    let result = Query.execute conn ~format:TSV "SELECT 'bigstring'" in
    let buf = Result.to_bigstring result in
    let len = Bigarray.Array1.dim buf in
    Alcotest.(check bool) "bigstring length > 0" true (len > 0);
    Result.destroy result)

let test_zero_copy_bigstring () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:TSV "SELECT 'zero_copy'"
      ~f:(fun buf ->
        let len = Bigarray.Array1.dim buf in
        Alcotest.(check bool) "has data" true (len > 0)))

let test_result_stats () =
  with_conn (fun conn ->
    let result = Query.execute conn
                   "SELECT number FROM numbers(100)" in
    let stats = Result.get_stats result in
    Alcotest.(check bool) "elapsed >= 0" true (stats.elapsed >= 0.0);
    Alcotest.(check bool) "rows_read >= 0" true
      (Int64.compare stats.rows_read 0L >= 0);
    Result.destroy result)

let test_result_elapsed () =
  with_conn (fun conn ->
    let result = Query.execute conn
                   "SELECT number FROM numbers(1000)" in
    let elapsed = Result.elapsed result in
    Alcotest.(check bool) "elapsed >= 0" true (elapsed >= 0.0);
    Result.destroy result)

let test_with_result () =
  with_conn (fun conn ->
    let len = Query.with_result conn ~format:TSV "SELECT 42"
                ~f:Result.length in
    Alcotest.(check bool) "length > 0" true (len > 0))

let test_stats_pp () =
  with_conn (fun conn ->
    let result = Query.execute conn "SELECT 1" in
    let stats = Result.get_stats result in
    let s = Stdlib.Format.asprintf "%a" Stats.pp stats in
    Alcotest.(check bool) "pp has elapsed" true
      (String.length s > 0);
    Result.destroy result)

let test_stats_local () =
  with_conn (fun conn ->
    let result = Query.execute conn
                   "SELECT number FROM numbers(100)" in
    (* get_stats_local returns a locally-allocated record;
       compare against heap-allocated version to verify *)
    let heap = Result.get_stats result in
    let Stats.{ elapsed; rows_read; bytes_read;
                storage_rows_read; storage_bytes_read } =
      Result.get_stats_local result in
    (* Polymorphic = works on local values *)
    Alcotest.(check bool) "local elapsed matches" true
      (elapsed = heap.elapsed);
    Alcotest.(check bool) "local rows_read matches" true
      (rows_read = heap.rows_read);
    Alcotest.(check bool) "local bytes_read matches" true
      (bytes_read = heap.bytes_read);
    Alcotest.(check bool) "local storage_rows_read matches" true
      (storage_rows_read = heap.storage_rows_read);
    Alcotest.(check bool) "local storage_bytes_read matches" true
      (storage_bytes_read = heap.storage_bytes_read);
    Result.destroy result)

(* ---------- Error handling tests ---------- *)

let test_query_error () =
  with_conn (fun conn ->
    Alcotest.check_raises "invalid SQL raises"
      (Error.E (Error.Query_failed ""))
      (fun () ->
        try ignore (Query.execute conn "INVALID SQL BLAH")
        with Error.E (Error.Query_failed _) ->
          raise (Error.E (Error.Query_failed ""))))

let test_error_to_string () =
  let msg = Error.to_string (Connection_failed "test") in
  Alcotest.(check bool) "has message" true (String.length msg > 0)

(* ---------- Multiple query tests ---------- *)

let test_multiple_queries () =
  with_conn (fun conn ->
    for i = 1 to 10 do
      let s = Query.execute_string conn ~format:TSV
                (Printf.sprintf "SELECT %d" i) in
      Alcotest.(check string) (Printf.sprintf "query %d" i)
        (Printf.sprintf "%d\n" i) s
    done)

let test_create_and_query_table () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE test_t (id UInt32, name String) \
       ENGINE = MergeTree() ORDER BY id";
    Query.execute_ignore conn
      "INSERT INTO test_t VALUES (1, 'alice'), (2, 'bob')";
    let s = Query.execute_string conn ~format:TSV
              "SELECT name FROM test_t ORDER BY id" in
    Alcotest.(check string) "table query" "alice\nbob\n" s)

let test_aggregation () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:TSV
              "SELECT sum(number) FROM numbers(101)" in
    Alcotest.(check string) "sum 0..100 = 5050" "5050\n" s)

(* ---------- Streaming tests ---------- *)

let test_stream_basic () =
  with_conn (fun conn ->
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(100)" in
    let s = Stream.collect_string stream in
    Alcotest.(check bool) "stream non-empty" true (String.length s > 0))

let test_stream_count () =
  with_conn (fun conn ->
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(10000)" in
    let count = Stream.count stream in
    Alcotest.(check bool) "got chunks" true (count >= 1))

let test_stream_iter () =
  with_conn (fun conn ->
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(100)" in
    let total_bytes = ref 0 in
    Stream.iter stream ~f:(fun chunk ->
      total_bytes := !total_bytes + Result.length chunk);
    Alcotest.(check bool) "streamed bytes > 0" true (!total_bytes > 0))

let test_stream_fold () =
  with_conn (fun conn ->
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(100)" in
    let total = Stream.fold stream ~init:0 ~f:(fun acc chunk ->
      acc + Result.length chunk)
    in
    Alcotest.(check bool) "fold bytes > 0" true (total > 0))

let test_stream_to_seq () =
  with_conn (fun conn ->
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(50)" in
    let seq = Stream.to_seq stream in
    let count = Seq.fold_left (fun n _chunk -> n + 1) 0 seq in
    Alcotest.(check bool) "seq has chunks" true (count >= 1))

let test_stream_cancel () =
  with_conn (fun conn ->
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(1000000)" in
    let _first = Stream.fetch stream in
    Stream.cancel stream;
    let after_cancel = Stream.fetch stream in
    Alcotest.(check bool) "none after cancel" true
      (Option.is_none after_cancel))

(* ---------- Binary format tests ---------- *)

let get_int64_le (buf : Result.bigstring) pos =
  let b0 = Char.code (Bigarray.Array1.get buf pos) in
  let b1 = Char.code (Bigarray.Array1.get buf (pos+1)) in
  let b2 = Char.code (Bigarray.Array1.get buf (pos+2)) in
  let b3 = Char.code (Bigarray.Array1.get buf (pos+3)) in
  let b4 = Char.code (Bigarray.Array1.get buf (pos+4)) in
  let b5 = Char.code (Bigarray.Array1.get buf (pos+5)) in
  let b6 = Char.code (Bigarray.Array1.get buf (pos+6)) in
  let b7 = Char.code (Bigarray.Array1.get buf (pos+7)) in
  Int64.(add (of_int b0)
    (add (shift_left (of_int b1) 8)
    (add (shift_left (of_int b2) 16)
    (add (shift_left (of_int b3) 24)
    (add (shift_left (of_int b4) 32)
    (add (shift_left (of_int b5) 40)
    (add (shift_left (of_int b6) 48)
         (shift_left (of_int b7) 56))))))))

let test_row_binary_uint64 () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(42)" ~f:(fun buf ->
      Alcotest.(check bool) "8 bytes" true
        (Bigarray.Array1.dim buf = 8);
      let v = get_int64_le buf 0 in
      Alcotest.(check int64) "value is 42" 42L v))

let test_row_binary_multiple () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(10)" ~f:(fun buf ->
      Alcotest.(check int) "80 bytes" 80 (Bigarray.Array1.dim buf);
      let sum = ref 0L in
      for i = 0 to 9 do
        let v = get_int64_le buf (i * 8) in
        sum := Int64.add !sum v
      done;
      Alcotest.(check int64) "sum 0..9 = 45" 45L !sum))

let test_row_binary_float () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toFloat64(3.14)" ~f:(fun buf ->
      Alcotest.(check int) "8 bytes" 8 (Bigarray.Array1.dim buf);
      let bits = get_int64_le buf 0 in
      let f = Int64.float_of_bits bits in
      Alcotest.(check bool) "close to 3.14" true
        (Float.abs (f -. 3.14) < 0.001)))

(* ---------- Large result tests ---------- *)

let test_large_result () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:TSV
              "SELECT number FROM numbers(100000)" in
    let lines =
      String.split_on_char '\n' s
      |> List.filter (fun l -> String.length l > 0)
    in
    Alcotest.(check int) "100000 rows" 100000 (List.length lines))

let test_large_bigstring () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(10000)" ~f:(fun buf ->
      Alcotest.(check int) "80000 bytes" 80000 (Bigarray.Array1.dim buf);
      let first = get_int64_le buf 0 in
      let last = get_int64_le buf (9999 * 8) in
      Alcotest.(check int64) "first = 0" 0L first;
      Alcotest.(check int64) "last = 9999" 9999L last))

(* ---------- execute_ignore tests ---------- *)

let test_execute_ignore () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE ignore_test (x UInt32) ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO ignore_test VALUES (1), (2), (3)";
    let s = Query.execute_string conn ~format:TSV
              "SELECT sum(x) FROM ignore_test" in
    Alcotest.(check string) "sum = 6" "6\n" s)

(* ---------- Input format tests ---------- *)

let test_input_csv () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE csv_in (id UInt32, name String, val Float64) \
       ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO csv_in FORMAT CSV\n\
       1,\"Alice\",95.5\n\
       2,\"Bob\",87.3\n\
       3,\"Carol\",92.1";
    let s = Query.execute_string conn ~format:TSV
              "SELECT id, name, val FROM csv_in ORDER BY id" in
    Alcotest.(check string) "CSV insert"
      "1\tAlice\t95.5\n2\tBob\t87.3\n3\tCarol\t92.1\n" s)

let test_input_tsv () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE tsv_in (id UInt32, name String, val Float64) \
       ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO tsv_in FORMAT TSV\n\
       10\tDave\t88\n\
       11\tEve\t91.5";
    let s = Query.execute_string conn ~format:TSV
              "SELECT id, name, val FROM tsv_in ORDER BY id" in
    Alcotest.(check string) "TSV insert"
      "10\tDave\t88\n11\tEve\t91.5\n" s)

let test_input_json_each_row () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE json_in (id UInt32, name String, val Float64) \
       ENGINE = Memory";
    Query.execute_ignore conn
      {|INSERT INTO json_in FORMAT JSONEachRow
{"id": 20, "name": "Grace", "val": 99.0}
{"id": 21, "name": "Hank", "val": 85.5}|};
    let s = Query.execute_string conn ~format:TSV
              "SELECT id, name, val FROM json_in ORDER BY id" in
    Alcotest.(check string) "JSONEachRow insert"
      "20\tGrace\t99\n21\tHank\t85.5\n" s)

let test_input_json_compact_each_row () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE jcer_in (id UInt32, name String, val Float64) \
       ENGINE = Memory";
    Query.execute_ignore conn
      {|INSERT INTO jcer_in FORMAT JSONCompactEachRow
[30, "Jack", 82.0]
[31, "Kate", 94.5]|};
    let s = Query.execute_string conn ~format:TSV
              "SELECT id, name, val FROM jcer_in ORDER BY id" in
    Alcotest.(check string) "JSONCompactEachRow insert"
      "30\tJack\t82\n31\tKate\t94.5\n" s)

let test_roundtrip_json () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE rt_json (id UInt32, name String, val Float64) \
       ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO rt_json VALUES (1, 'A', 10.0), (2, 'B', 20.0)";
    let json = Query.execute_string conn ~format:JSONEachRow
                 "SELECT * FROM rt_json ORDER BY id" in
    Query.execute_ignore conn "TRUNCATE TABLE rt_json";
    Query.execute_ignore conn
      (Printf.sprintf "INSERT INTO rt_json FORMAT JSONEachRow\n%s" json);
    let s = Query.execute_string conn ~format:TSV
              "SELECT * FROM rt_json ORDER BY id" in
    Alcotest.(check string) "roundtrip JSON"
      "1\tA\t10\n2\tB\t20\n" s)

let test_roundtrip_csv () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE rt_csv (id UInt32, name String, val Float64) \
       ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO rt_csv VALUES (5, 'X', 50.0), (6, 'Y', 60.0)";
    let csv = Query.execute_string conn ~format:CSV
                "SELECT * FROM rt_csv ORDER BY id" in
    Query.execute_ignore conn "TRUNCATE TABLE rt_csv";
    Query.execute_ignore conn
      (Printf.sprintf "INSERT INTO rt_csv FORMAT CSV\n%s" csv);
    let s = Query.execute_string conn ~format:TSV
              "SELECT * FROM rt_csv ORDER BY id" in
    Alcotest.(check string) "roundtrip CSV"
      "5\tX\t50\n6\tY\t60\n" s)

let test_input_csv_types () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE csv_types ( \
         u8 UInt8, u16 UInt16, u32 UInt32, u64 UInt64, \
         i32 Int32, f32 Float32, f64 Float64, \
         s String, d Date, dt DateTime \
       ) ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO csv_types FORMAT CSV\n\
       255,65535,4294967295,18446744073709551615,\
       -42,3.14,2.718281828,\"hello\",\"2024-01-15\",\"2024-01-15 12:30:00\"";
    let s = Query.execute_string conn ~format:TSV
              "SELECT u8, u16, u32, u64, i32, s FROM csv_types" in
    Alcotest.(check string) "typed CSV"
      "255\t65535\t4294967295\t18446744073709551615\t-42\thello\n" s)

let test_input_json_nested () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE json_nested ( \
         id UInt32, tags Array(String), meta String \
       ) ENGINE = Memory";
    Query.execute_ignore conn
      {|INSERT INTO json_nested FORMAT JSONEachRow
{"id": 1, "tags": ["a","b","c"], "meta": "first"}
{"id": 2, "tags": ["x"], "meta": "second"}|};
    let s = Query.execute_string conn ~format:JSONEachRow
              "SELECT * FROM json_nested ORDER BY id" in
    Alcotest.(check bool) "has array" true (String.length s > 0);
    (* Verify we can read back the array *)
    let arr = Query.execute_string conn ~format:TSV
                "SELECT tags FROM json_nested WHERE id = 1" in
    Alcotest.(check string) "array value" "['a','b','c']\n" arr)

let test_input_csv_nullable () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE csv_null ( \
         id UInt32, name Nullable(String), score Nullable(Float64) \
       ) ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO csv_null FORMAT CSV\n\
       1,\"Alice\",95.5\n\
       2,\\N,\\N\n\
       3,\"Carol\",\\N";
    let s = Query.execute_string conn ~format:TSV
              "SELECT id, name, score FROM csv_null ORDER BY id" in
    Alcotest.(check string) "nullable CSV"
      "1\tAlice\t95.5\n2\t\\N\t\\N\n3\tCarol\t\\N\n" s)

let test_input_json_nullable () =
  with_conn (fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE json_null ( \
         id UInt32, name Nullable(String), score Nullable(Float64) \
       ) ENGINE = Memory";
    Query.execute_ignore conn
      {|INSERT INTO json_null FORMAT JSONEachRow
{"id": 1, "name": "Alice", "score": 95.5}
{"id": 2, "name": null, "score": null}
{"id": 3, "name": "Carol", "score": null}|};
    let s = Query.execute_string conn ~format:TSV
              "SELECT id, name, score FROM json_null ORDER BY id" in
    Alcotest.(check string) "nullable JSON"
      "1\tAlice\t95.5\n2\t\\N\t\\N\n3\tCarol\t\\N\n" s)

(* ---------- Binary module tests ---------- *)

let test_binary_sum_uint64 () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(100)" ~f:(fun buf ->
      let sum = Binary.sum_uint64 buf in
      Alcotest.(check int64) "sum 0..99" 4950L sum))

let test_binary_iter_uint64 () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(5)" ~f:(fun buf ->
      let values = ref [] in
      Binary.iter_uint64 buf ~f:(fun _i v ->
        values := v :: !values);
      let values = List.rev !values in
      Alcotest.(check (list int64)) "0..4"
        [0L; 1L; 2L; 3L; 4L] values))

let test_binary_fold_uint64 () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(10)" ~f:(fun buf ->
      let max_val = Binary.fold_uint64 buf ~init:0L
        ~f:(fun acc _i v -> Int64.max acc v) in
      Alcotest.(check int64) "max = 9" 9L max_val))

let test_binary_get_int64 () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(42)" ~f:(fun buf ->
      let v = Binary.get_int64_t_le buf ~pos:0 in
      Alcotest.(check int64) "42" 42L v))

let test_binary_get_uint32 () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt32(12345)" ~f:(fun buf ->
      let v = Binary.get_uint32_le buf ~pos:0 in
      Alcotest.(check int) "12345" 12345 v))

let test_binary_varint () =
  (* RowBinary encodes String as: varint_length + bytes *)
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT 'hello'" ~f:(fun buf ->
      let str_len, consumed = Binary.get_varint buf ~pos:0 in
      Alcotest.(check int) "string length" 5 str_len;
      Alcotest.(check int) "varint consumed" 1 consumed))

let test_binary_get_string () =
  with_conn (fun conn ->
    Query.with_buffer conn ~format:RowBinary
      "SELECT 'hello'" ~f:(fun buf ->
      let s, consumed = Binary.get_string buf ~pos:0 in
      Alcotest.(check string) "string" "hello" s;
      Alcotest.(check int) "total consumed" 6 consumed))

(* ---------- Or_error tests ---------- *)

let test_try_execute_ok () =
  with_conn (fun conn ->
    match Query.try_execute conn ~format:TSV "SELECT 42" with
    | Ok result ->
      let s = Result.to_string result in
      Result.destroy result;
      Alcotest.(check string) "42" "42\n" s
    | Error err ->
      Alcotest.fail (Base.Error.to_string_hum err))

let test_try_execute_error () =
  with_conn (fun conn ->
    match Query.try_execute conn "INVALID SQL QUERY" with
    | Ok result ->
      Result.destroy result;
      Alcotest.fail "expected error"
    | Error _ -> ())

let test_try_execute_string_ok () =
  with_conn (fun conn ->
    match Query.try_execute_string conn ~format:TSV "SELECT 42" with
    | Ok s -> Alcotest.(check string) "42" "42\n" s
    | Error err -> Alcotest.fail (Base.Error.to_string_hum err))

let test_try_execute_string_error () =
  with_conn (fun conn ->
    match Query.try_execute_string conn "INVALID SQL QUERY" with
    | Ok _ -> Alcotest.fail "expected error"
    | Error _ -> ())

(* ---------- Test runner ---------- *)

let () =
  Alcotest.run "chdb" [
    "connection", [
      Alcotest.test_case "connect/close" `Quick test_connect;
      Alcotest.test_case "close idempotent" `Quick
        test_connect_close_idempotent;
      Alcotest.test_case "with_connection" `Quick test_with_connection;
      Alcotest.test_case "with_connection exception" `Quick
        test_with_connection_exception;
      Alcotest.test_case "use after close" `Quick test_use_after_close;
    ];
    "basic queries", [
      Alcotest.test_case "SELECT 1" `Quick test_select_one;
      Alcotest.test_case "SELECT string" `Quick test_select_string;
      Alcotest.test_case "SELECT number" `Quick test_select_number;
      Alcotest.test_case "multiple rows" `Quick test_select_multiple_rows;
      Alcotest.test_case "multiple columns" `Quick
        test_select_multiple_columns;
    ];
    "formats", [
      Alcotest.test_case "CSV" `Quick test_format_csv;
      Alcotest.test_case "CSVWithNames" `Quick test_format_csv_with_names;
      Alcotest.test_case "JSON" `Quick test_format_json;
      Alcotest.test_case "JSONEachRow" `Quick test_format_json_each_row;
      Alcotest.test_case "Pretty" `Quick test_format_pretty;
      Alcotest.test_case "Vertical" `Quick test_format_vertical;
      Alcotest.test_case "Null" `Quick test_format_null;
    ];
    "result access", [
      Alcotest.test_case "length" `Quick test_result_length;
      Alcotest.test_case "is_empty" `Quick test_result_is_empty;
      Alcotest.test_case "to_string" `Quick test_result_to_string;
      Alcotest.test_case "to_bigstring" `Quick test_result_to_bigstring;
      Alcotest.test_case "zero-copy bigstring" `Quick
        test_zero_copy_bigstring;
      Alcotest.test_case "stats" `Quick test_result_stats;
      Alcotest.test_case "elapsed" `Quick test_result_elapsed;
      Alcotest.test_case "with_result" `Quick test_with_result;
      Alcotest.test_case "stats pp" `Quick test_stats_pp;
      Alcotest.test_case "stats local" `Quick test_stats_local;
    ];
    "error handling", [
      Alcotest.test_case "invalid SQL" `Quick test_query_error;
      Alcotest.test_case "error_to_string" `Quick test_error_to_string;
    ];
    "multiple queries", [
      Alcotest.test_case "sequential queries" `Quick test_multiple_queries;
      Alcotest.test_case "create and query table" `Quick
        test_create_and_query_table;
      Alcotest.test_case "aggregation" `Quick test_aggregation;
    ];
    "streaming", [
      Alcotest.test_case "basic stream" `Quick test_stream_basic;
      Alcotest.test_case "stream count" `Quick test_stream_count;
      Alcotest.test_case "stream iter" `Quick test_stream_iter;
      Alcotest.test_case "stream fold" `Quick test_stream_fold;
      Alcotest.test_case "stream to_seq" `Quick test_stream_to_seq;
      Alcotest.test_case "stream cancel" `Quick test_stream_cancel;
    ];
    "binary format", [
      Alcotest.test_case "RowBinary UInt64" `Quick test_row_binary_uint64;
      Alcotest.test_case "RowBinary multiple" `Quick
        test_row_binary_multiple;
      Alcotest.test_case "RowBinary float" `Quick test_row_binary_float;
    ];
    "large results", [
      Alcotest.test_case "100k rows" `Slow test_large_result;
      Alcotest.test_case "large bigstring" `Slow test_large_bigstring;
    ];
    "execute_ignore", [
      Alcotest.test_case "DDL + INSERT + SELECT" `Quick test_execute_ignore;
    ];
    "input formats", [
      Alcotest.test_case "CSV input" `Quick test_input_csv;
      Alcotest.test_case "TSV input" `Quick test_input_tsv;
      Alcotest.test_case "JSONEachRow input" `Quick test_input_json_each_row;
      Alcotest.test_case "JSONCompactEachRow input" `Quick
        test_input_json_compact_each_row;
      Alcotest.test_case "roundtrip JSON" `Quick test_roundtrip_json;
      Alcotest.test_case "roundtrip CSV" `Quick test_roundtrip_csv;
      Alcotest.test_case "CSV with all types" `Quick test_input_csv_types;
      Alcotest.test_case "JSON nested arrays" `Quick test_input_json_nested;
      Alcotest.test_case "CSV nullable" `Quick test_input_csv_nullable;
      Alcotest.test_case "JSON nullable" `Quick test_input_json_nullable;
    ];
    "binary module", [
      Alcotest.test_case "sum_uint64" `Quick test_binary_sum_uint64;
      Alcotest.test_case "iter_uint64" `Quick test_binary_iter_uint64;
      Alcotest.test_case "fold_uint64" `Quick test_binary_fold_uint64;
      Alcotest.test_case "get_int64_t_le" `Quick test_binary_get_int64;
      Alcotest.test_case "get_uint32_le" `Quick test_binary_get_uint32;
      Alcotest.test_case "get_varint" `Quick test_binary_varint;
      Alcotest.test_case "get_string" `Quick test_binary_get_string;
    ];
    "or_error", [
      Alcotest.test_case "try_execute ok" `Quick test_try_execute_ok;
      Alcotest.test_case "try_execute error" `Quick test_try_execute_error;
      Alcotest.test_case "try_execute_string ok" `Quick
        test_try_execute_string_ok;
      Alcotest.test_case "try_execute_string error" `Quick
        test_try_execute_string_error;
    ];
  ]
