open Duckdb

(* Helper: create an in-memory database with a connection *)
let with_db f =
  let db = open_database () in
  let conn = connect db in
  f conn;
  close db

let%expect_test "library version" =
  let v = library_version () in
  Printf.printf "starts with 'v': %b\n" (String.length v > 0 && v.[0] = 'v');
  [%expect {| starts with 'v': true |}]

let%expect_test "vector size" =
  let sz = vector_size () in
  Printf.printf "vector_size = %d\n" sz;
  [%expect {| vector_size = 2048 |}]

let%expect_test "open in-memory and query" =
  with_db (fun conn ->
    let r = query conn "SELECT 42 AS answer" in
    Printf.printf "cols=%d rows=%d\n" (column_count r) (row_count r);
    Printf.printf "col_name=%s\n" (column_name r 0);
    Printf.printf "value=%Ld\n" (value_int64 r 0 0));
  [%expect {|
    cols=1 rows=1
    col_name=answer
    value=42 |}]

let%expect_test "multiple rows and types" =
  with_db (fun conn ->
    let _ = query conn
      "CREATE TABLE t (id INTEGER, name VARCHAR, value DOUBLE)" in
    let _ = query conn
      "INSERT INTO t VALUES (1, 'alpha', 3.14), (2, 'beta', 2.72), (3, 'gamma', 1.41)" in
    let r = query conn "SELECT * FROM t ORDER BY id" in
    Printf.printf "cols=%d rows=%d\n" (column_count r) (row_count r);
    for row = 0 to row_count r - 1 do
      let id = value_int32 r 0 row in
      let name = value_string r ~col:1 ~row in
      let v = value_double r 2 row in
      Printf.printf "%ld\t%s\t%.2f\n" id name v
    done);
  [%expect {|
    cols=3 rows=3
    1	alpha	3.14
    2	beta	2.72
    3	gamma	1.41 |}]

let%expect_test "null handling" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE nulls (x INTEGER, y VARCHAR)" in
    let _ = query conn
      "INSERT INTO nulls VALUES (1, 'a'), (NULL, 'b'), (3, NULL)" in
    let r = query conn "SELECT * FROM nulls ORDER BY rowid" in
    for row = 0 to row_count r - 1 do
      let x_null = value_is_null r 0 row in
      let y_null = value_is_null r 1 row in
      Printf.printf "row %d: x_null=%b y_null=%b\n" row x_null y_null
    done);
  [%expect {|
    row 0: x_null=false y_null=false
    row 1: x_null=true y_null=false
    row 2: x_null=false y_null=true |}]

let%expect_test "column types" =
  with_db (fun conn ->
    let r = query conn
      "SELECT 1::INTEGER AS i, 'hello'::VARCHAR AS s, 3.14::DOUBLE AS d, \
       TRUE AS b, DATE '2024-01-15' AS dt" in
    for col = 0 to column_count r - 1 do
      Printf.printf "%s: %s\n" (column_name r col) (Type.to_string (column_type r col))
    done);
  [%expect {|
    i: INTEGER
    s: VARCHAR
    d: DOUBLE
    b: BOOLEAN
    dt: DATE |}]

let%expect_test "error handling" =
  with_db (fun conn ->
    (try
       let _ = query conn "SELECT * FROM nonexistent" in
       print_string "unreachable"
     with Error msg ->
       Printf.printf "caught error: %b\n" (String.length msg > 0));
    (* Verify the connection still works after an error *)
    let r = query conn "SELECT 1" in
    Printf.printf "after error: rows=%d\n" (row_count r));
  [%expect {|
    caught error: true
    after error: rows=1 |}]

let%expect_test "prepared statements" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE ps (id INTEGER, name VARCHAR, val DOUBLE)" in
    let stmt = prepare conn "INSERT INTO ps VALUES ($1, $2, $3)" in
    Printf.printf "param_count=%d\n" (param_count stmt);
    (* Insert first row *)
    bind_int32 stmt 1 1l;
    bind_string stmt 2 "hello";
    bind_double stmt 3 3.14;
    let _ = execute stmt in
    (* Insert second row *)
    clear_bindings stmt;
    bind_int32 stmt 1 2l;
    bind_string stmt 2 "world";
    bind_double stmt 3 2.72;
    let _ = execute stmt in
    (* Insert row with null *)
    clear_bindings stmt;
    bind_int32 stmt 1 3l;
    bind_null stmt 2;
    bind_double stmt 3 0.0;
    let _ = execute stmt in
    (* Query back *)
    let r = query conn "SELECT * FROM ps ORDER BY id" in
    for row = 0 to row_count r - 1 do
      let id = value_int32 r 0 row in
      let name = if value_is_null r 1 row then "NULL"
                 else value_string r ~col:1 ~row in
      let v = value_double r 2 row in
      Printf.printf "%ld\t%s\t%.2f\n" id name v
    done);
  [%expect {|
    param_count=3
    1	hello	3.14
    2	world	2.72
    3	NULL	0.00 |}]

let%expect_test "prepared statement with select" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE lookup (id INTEGER, name VARCHAR)" in
    let _ = query conn
      "INSERT INTO lookup VALUES (1, 'one'), (2, 'two'), (3, 'three')" in
    let stmt = prepare conn "SELECT name FROM lookup WHERE id = $1" in
    bind_int64 stmt 1 2L;
    let r = execute stmt in
    Printf.printf "name=%s\n" (value_string r ~col:0 ~row:0);
    clear_bindings stmt;
    bind_int64 stmt 1 3L;
    let r = execute stmt in
    Printf.printf "name=%s\n" (value_string r ~col:0 ~row:0));
  [%expect {|
    name=two
    name=three |}]

let%expect_test "prepared statement error" =
  with_db (fun conn ->
    (try
       let _ = prepare conn "SELECTT nonsense" in
       print_string "unreachable"
     with Error _ ->
       print_string "caught prepare error\n");
    (* Connection still usable *)
    let r = query conn "SELECT 1 AS ok" in
    Printf.printf "ok=%Ld\n" (value_int64 r 0 0));
  [%expect {|
    caught prepare error
    ok=1 |}]

let%expect_test "data chunk API" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE chunk_test (x INTEGER, y DOUBLE)" in
    (* Insert enough rows to potentially span chunks *)
    let _ = query conn
      "INSERT INTO chunk_test SELECT i, i * 1.5 FROM range(10) t(i)" in
    let r = query conn "SELECT * FROM chunk_test ORDER BY x" in
    let nchunks = Data_chunk.chunk_count r in
    Printf.printf "chunks=%d\n" nchunks;
    let total_rows = ref 0 in
    for i = 0 to nchunks - 1 do
      let chunk = Data_chunk.get_chunk r i in
      let sz = Data_chunk.size chunk in
      let ncols = Data_chunk.column_count chunk in
      Printf.printf "chunk %d: rows=%d cols=%d\n" i sz ncols;
      total_rows := !total_rows + sz
    done;
    Printf.printf "total_rows=%d\n" !total_rows);
  [%expect {|
    chunks=1
    chunk 0: rows=10 cols=2
    total_rows=10 |}]

let%expect_test "data chunk vector access" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE vec (x INTEGER, s VARCHAR)" in
    let _ = query conn
      "INSERT INTO vec VALUES (10, 'ten'), (20, 'twenty'), (30, 'thirty')" in
    let r = query conn "SELECT * FROM vec ORDER BY x" in
    let chunk = Data_chunk.get_chunk r 0 in
    let sz = Data_chunk.size chunk in
    (* Access raw data as bigarray *)
    let x_data = Vector.data chunk ~col:0 in
    Printf.printf "x_data has_data=%b (dim=%d)\n"
      (Bigarray.Array1.dim x_data > 0) (Bigarray.Array1.dim x_data);
    (* String access *)
    for row = 0 to sz - 1 do
      let s = Vector.get_string chunk ~col:1 ~row in
      Printf.printf "row %d: s=%s\n" row s
    done;
    (* Validity check *)
    Printf.printf "all valid: %b\n"
      (let ok = ref true in
       for row = 0 to sz - 1 do
         if not (Vector.is_valid chunk 0 row) then ok := false;
         if not (Vector.is_valid chunk 1 row) then ok := false
       done;
       !ok));
  [%expect {|
    x_data has_data=true (dim=48)
    row 0: s=ten
    row 1: s=twenty
    row 2: s=thirty
    all valid: true |}]

let%expect_test "data chunk with nulls" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE vnull (x INTEGER)" in
    let _ = query conn
      "INSERT INTO vnull VALUES (1), (NULL), (3), (NULL), (5)" in
    let r = query conn "SELECT * FROM vnull ORDER BY rowid" in
    let chunk = Data_chunk.get_chunk r 0 in
    let has_validity = Vector.validity chunk ~col:0 <> None in
    Printf.printf "has validity bitmap: %b\n" has_validity;
    for row = 0 to Data_chunk.size chunk - 1 do
      Printf.printf "row %d: valid=%b\n" row (Vector.is_valid chunk 0 row)
    done);
  [%expect {|
    has validity bitmap: true
    row 0: valid=true
    row 1: valid=false
    row 2: valid=true
    row 3: valid=false
    row 4: valid=true |}]

let%expect_test "large result spanning chunks" =
  with_db (fun conn ->
    let n = 5000 in
    let _ = query conn
      (Printf.sprintf
         "CREATE TABLE big AS SELECT i AS x FROM range(%d) t(i)" n) in
    let r = query conn "SELECT * FROM big ORDER BY x" in
    let nchunks = Data_chunk.chunk_count r in
    Printf.printf "chunks=%d (for %d rows)\n" nchunks n;
    let total = ref 0 in
    for i = 0 to nchunks - 1 do
      let chunk = Data_chunk.get_chunk r i in
      total := !total + Data_chunk.size chunk
    done;
    Printf.printf "total_rows=%d\n" !total;
    Printf.printf "matches: %b\n" (!total = n));
  [%expect {|
    chunks=3 (for 5000 rows)
    total_rows=5000
    matches: true |}]

let%expect_test "rows_changed" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE rc (x INTEGER)" in
    let _ = query conn "INSERT INTO rc VALUES (1), (2), (3)" in
    let r = query conn "UPDATE rc SET x = x + 10 WHERE x > 1" in
    Printf.printf "rows_changed=%d\n" (rows_changed r));
  [%expect {| rows_changed=2 |}]

let%expect_test "bind_bool and bind_int64" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE bt (b BOOLEAN, big BIGINT)" in
    let stmt = prepare conn "INSERT INTO bt VALUES ($1, $2)" in
    bind_bool stmt 1 true;
    bind_int64 stmt 2 9_999_999_999L;
    let _ = execute stmt in
    clear_bindings stmt;
    bind_bool stmt 1 false;
    bind_int64 stmt 2 (-1L);
    let _ = execute stmt in
    let r = query conn "SELECT * FROM bt ORDER BY big" in
    for row = 0 to row_count r - 1 do
      let b = value_int64 r 0 row in  (* bool as int *)
      let big = value_int64 r 1 row in
      Printf.printf "b=%Ld big=%Ld\n" b big
    done);
  [%expect {|
    b=0 big=-1
    b=1 big=9999999999 |}]

let%expect_test "blob binding" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE blobs (data BLOB)" in
    let stmt = prepare conn "INSERT INTO blobs VALUES ($1)" in
    let blob = Bytes.of_string "\x00\x01\x02\xff" in
    bind_blob stmt 1 blob;
    let _ = execute stmt in
    let r = query conn "SELECT octet_length(data) AS len FROM blobs" in
    let len = value_int64 r 0 0 in
    Printf.printf "blob length=%Ld\n" len);
  [%expect {| blob length=4 |}]
