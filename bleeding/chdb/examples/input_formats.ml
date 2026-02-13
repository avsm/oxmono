(* input_formats.ml -- Test different input format ingestion *)

open Chdb

let () =
  Connection.with_connection ~f:(fun conn ->
    Query.execute_ignore conn
      "CREATE TABLE fmt_test (id UInt32, name String, score Float64) \
       ENGINE = Memory";

    (* CSV input *)
    Printf.printf "=== CSV Input ===\n";
    Query.execute_ignore conn
      "INSERT INTO fmt_test FORMAT CSV\n\
       1,\"Alice\",95.5\n\
       2,\"Bob\",87.3\n\
       3,\"Carol\",92.1";
    print_string (Query.execute_string conn ~format:TSV
      "SELECT * FROM fmt_test ORDER BY id");

    (* TSV input *)
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Printf.printf "\n=== TSV Input ===\n";
    Query.execute_ignore conn
      "INSERT INTO fmt_test FORMAT TSV\n\
       10\tDave\t88\n\
       11\tEve\t91.5\n\
       12\tFrank\t76.2";
    print_string (Query.execute_string conn ~format:TSV
      "SELECT * FROM fmt_test ORDER BY id");

    (* JSONEachRow input *)
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Printf.printf "\n=== JSONEachRow Input ===\n";
    Query.execute_ignore conn
      {|INSERT INTO fmt_test FORMAT JSONEachRow
{"id": 20, "name": "Grace", "score": 99.0}
{"id": 21, "name": "Hank", "score": 85.5}
{"id": 22, "name": "Iris", "score": 93.7}|};
    print_string (Query.execute_string conn ~format:TSV
      "SELECT * FROM fmt_test ORDER BY id");

    (* JSONCompactEachRow input *)
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Printf.printf "\n=== JSONCompactEachRow Input ===\n";
    Query.execute_ignore conn
      {|INSERT INTO fmt_test FORMAT JSONCompactEachRow
[30, "Jack", 82.0]
[31, "Kate", 94.5]|};
    print_string (Query.execute_string conn ~format:TSV
      "SELECT * FROM fmt_test ORDER BY id");

    (* Roundtrip: query as JSONEachRow -> re-insert *)
    Printf.printf "\n=== Roundtrip (JSONEachRow) ===\n";
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Query.execute_ignore conn
      "INSERT INTO fmt_test VALUES (1, 'A', 10.0), (2, 'B', 20.0)";
    let json = Query.execute_string conn ~format:JSONEachRow
      "SELECT * FROM fmt_test ORDER BY id" in
    Printf.printf "Exported:\n%s" json;
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Query.execute_ignore conn
      (Printf.sprintf "INSERT INTO fmt_test FORMAT JSONEachRow\n%s" json);
    print_string (Query.execute_string conn ~format:TSV
      "SELECT * FROM fmt_test ORDER BY id");

    (* Roundtrip: query as CSV -> re-insert *)
    Printf.printf "\n=== Roundtrip (CSV) ===\n";
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Query.execute_ignore conn
      "INSERT INTO fmt_test VALUES (5, 'X', 50.0), (6, 'Y', 60.0)";
    let csv = Query.execute_string conn ~format:CSV
      "SELECT * FROM fmt_test ORDER BY id" in
    Printf.printf "Exported:\n%s" csv;
    Query.execute_ignore conn "TRUNCATE TABLE fmt_test";
    Query.execute_ignore conn
      (Printf.sprintf "INSERT INTO fmt_test FORMAT CSV\n%s" csv);
    print_string (Query.execute_string conn ~format:TSV
      "SELECT * FROM fmt_test ORDER BY id")
  ) ()
