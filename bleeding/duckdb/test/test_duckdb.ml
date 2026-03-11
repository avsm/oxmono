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

(* ── Geometry module tests ─────────────────────────────────────── *)

let%expect_test "geometry WKT roundtrip — point" =
  let g = Geometry.point 1.5 2.5 in
  let wkt = Geometry.to_wkt g in
  Printf.printf "wkt=%s\n" wkt;
  let g2 = Geometry.of_wkt wkt in
  Printf.printf "roundtrip=%s\n" (Geometry.to_wkt g2);
  Printf.printf "type=%s\n"
    (Geometry.geom_type_to_string (Geometry.geom_type g));
  [%expect {|
    wkt=POINT (1.5 2.5)
    roundtrip=POINT (1.5 2.5)
    type=POINT |}]

let%expect_test "geometry WKT roundtrip — linestring" =
  let open Geometry in
  let g = linestring [{ x = 0.0; y = 0.0 }; { x = 1.0; y = 1.0 };
                       { x = 2.0; y = 3.0 }] in
  let wkt = to_wkt g in
  Printf.printf "wkt=%s\n" wkt;
  let g2 = of_wkt wkt in
  Printf.printf "roundtrip=%s\n" (to_wkt g2);
  [%expect {|
    wkt=LINESTRING (0 0, 1 1, 2 3)
    roundtrip=LINESTRING (0 0, 1 1, 2 3) |}]

let%expect_test "geometry WKT roundtrip — polygon" =
  let open Geometry in
  let g = polygon [
    [{ x = 0.; y = 0. }; { x = 10.; y = 0. }; { x = 10.; y = 10. };
     { x = 0.; y = 10. }; { x = 0.; y = 0. }]
  ] in
  let wkt = to_wkt g in
  Printf.printf "wkt=%s\n" wkt;
  let g2 = of_wkt wkt in
  Printf.printf "roundtrip=%s\n" (to_wkt g2);
  [%expect {|
    wkt=POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))
    roundtrip=POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0)) |}]

let%expect_test "geometry WKT parse — multipoint" =
  let g = Geometry.of_wkt "MULTIPOINT (1 2, 3 4, 5 6)" in
  Printf.printf "type=%s\n"
    (Geometry.geom_type_to_string (Geometry.geom_type g));
  Printf.printf "wkt=%s\n" (Geometry.to_wkt g);
  [%expect {|
    type=MULTIPOINT
    wkt=MULTIPOINT (1 2, 3 4, 5 6) |}]

let%expect_test "geometry WKT parse — multilinestring" =
  let g = Geometry.of_wkt
    "MULTILINESTRING ((0 0, 1 1), (2 2, 3 3, 4 4))" in
  Printf.printf "wkt=%s\n" (Geometry.to_wkt g);
  [%expect {| wkt=MULTILINESTRING ((0 0, 1 1), (2 2, 3 3, 4 4)) |}]

let%expect_test "geometry WKT parse — multipolygon" =
  let g = Geometry.of_wkt
    "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), ((2 2, 3 2, 3 3, 2 2)))" in
  Printf.printf "wkt=%s\n" (Geometry.to_wkt g);
  [%expect {| wkt=MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), ((2 2, 3 2, 3 3, 2 2))) |}]

let%expect_test "geometry WKT parse — geometry collection" =
  let g = Geometry.of_wkt
    "GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (0 0, 1 1))" in
  Printf.printf "wkt=%s\n" (Geometry.to_wkt g);
  Printf.printf "type=%s\n"
    (Geometry.geom_type_to_string (Geometry.geom_type g));
  [%expect {|
    wkt=GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (0 0, 1 1))
    type=GEOMETRYCOLLECTION |}]

let%expect_test "geometry WKT error" =
  (try
     let _ = Geometry.of_wkt "BOGUS (1 2)" in
     print_string "unreachable"
   with Error msg ->
     Printf.printf "caught: %s\n" msg);
  [%expect {| caught: WKT: unknown geometry type at position 0 |}]

let%expect_test "geometry SQL literal" =
  let g = Geometry.point 42.0 (-73.5) in
  Printf.printf "sql=%s\n" (Geometry.to_sql_literal g);
  [%expect {| sql='POINT (42 -73.5)'::GEOMETRY |}]

let%expect_test "geometry with DuckDB — store and retrieve" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE geo (id INTEGER, g GEOMETRY)" in
    let _ = query conn
      "INSERT INTO geo VALUES \
       (1, 'POINT (1 2)'::GEOMETRY), \
       (2, 'LINESTRING (0 0, 1 1, 2 2)'::GEOMETRY), \
       (3, 'POLYGON ((0 0, 10 0, 10 10, 0 0))'::GEOMETRY)" in
    (* Cast back to VARCHAR to read as WKT *)
    let r = query conn
      "SELECT id, g::VARCHAR AS wkt FROM geo ORDER BY id" in
    for row = 0 to row_count r - 1 do
      let id = value_int32 r 0 row in
      let wkt = value_string r ~col:1 ~row in
      let g = Geometry.of_wkt wkt in
      Printf.printf "%ld: %s → %s\n" id
        (Geometry.geom_type_to_string (Geometry.geom_type g))
        (Geometry.to_wkt g)
    done);
  [%expect {|
    1: POINT → POINT (1 2)
    2: LINESTRING → LINESTRING (0 0, 1 1, 2 2)
    3: POLYGON → POLYGON ((0 0, 10 0, 10 10, 0 0)) |}]

let%expect_test "geometry with DuckDB — insert from OCaml" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE geo2 (g GEOMETRY)" in
    let pts = [
      Geometry.point 10.0 20.0;
      Geometry.point 30.0 40.0;
    ] in
    List.iter (fun g ->
      let sql = Printf.sprintf
        "INSERT INTO geo2 VALUES (%s)" (Geometry.to_sql_literal g) in
      let _ = query conn sql in ()) pts;
    let r = query conn "SELECT g::VARCHAR AS wkt FROM geo2" in
    for row = 0 to row_count r - 1 do
      let g = Geometry.of_result r ~col:0 ~row in
      Printf.printf "%s\n" (Geometry.to_wkt g)
    done);
  [%expect {|
    POINT (10 20)
    POINT (30 40) |}]

let%expect_test "geometry column type" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE gt (g GEOMETRY)" in
    let _ = query conn "INSERT INTO gt VALUES ('POINT (0 0)'::GEOMETRY)" in
    let r = query conn "SELECT g FROM gt" in
    Printf.printf "type=%s\n"
      (Type.to_string (column_type r 0)));
  [%expect {| type=INVALID |}]
  (* DuckDB C API doesn't expose GEOMETRY type id yet; reports as INVALID *)

let%expect_test "geometry prepared statement" =
  with_db (fun conn ->
    let _ = query conn "CREATE TABLE gp (g GEOMETRY)" in
    let stmt = prepare conn "INSERT INTO gp VALUES ($1::GEOMETRY)" in
    let g = Geometry.point 99.0 (-1.5) in
    bind_string stmt 1 (Geometry.to_wkt g);
    let _ = execute stmt in
    let r = query conn "SELECT g::VARCHAR FROM gp" in
    let g2 = Geometry.of_result r ~col:0 ~row:0 in
    Printf.printf "roundtrip=%s\n" (Geometry.to_wkt g2));
  [%expect {| roundtrip=POINT (99 -1.5) |}]
