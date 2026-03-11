let () =
  Printf.printf "DuckDB version: %s\n%!" (Duckdb.library_version ());
  let db = Duckdb.open_database () in
  let conn = Duckdb.connect db in
  (* Create a table and insert data *)
  let _ = Duckdb.query conn "CREATE TABLE test (id INTEGER, name VARCHAR, value DOUBLE)" in
  let _ = Duckdb.query conn "INSERT INTO test VALUES (1, 'hello', 3.14), (2, 'world', 2.72), (3, 'duckdb', 1.41)" in
  (* Query it back *)
  let result = Duckdb.query conn "SELECT id, name, value FROM test ORDER BY id" in
  let ncols = Duckdb.column_count result in
  let nrows = Duckdb.row_count result in
  Printf.printf "Columns: %d, Rows: %d\n%!" ncols nrows;
  (* Print column names *)
  for col = 0 to ncols - 1 do
    if col > 0 then print_char '\t';
    print_string (Duckdb.column_name result col)
  done;
  print_newline ();
  (* Print rows using typed access *)
  for row = 0 to nrows - 1 do
    let id = Duckdb.value_int64 result 0 row in
    let name = Duckdb.value_string result ~col:1 ~row in
    let value = Duckdb.value_double result 2 row in
    Printf.printf "%Ld\t%s\t%.2f\n" id name value
  done;
  (* Test error handling *)
  (try
     let _ = Duckdb.query conn "SELECT * FROM nonexistent_table" in
     assert false
   with Duckdb.Error msg ->
     Printf.printf "Caught expected error: %s\n%!" msg);
  Duckdb.close db;
  Printf.printf "Hello DuckDB from OxCaml!\n%!"
