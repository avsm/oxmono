(* table_ops.ml -- Showcase table creation, inserts, and queries *)

open Chdb

let () =
  Connection.with_connection ~f:(fun conn ->
    (* Create table *)
    Printf.printf "=== Create Table ===\n";
    Query.execute_ignore conn
      "CREATE TABLE users ( \
         id UInt32, \
         name String, \
         email String, \
         age UInt8 \
       ) ENGINE = MergeTree() ORDER BY id";
    Printf.printf "Table 'users' created\n";

    (* Insert data *)
    Printf.printf "\n=== Insert Data ===\n";
    Query.execute_ignore conn
      "INSERT INTO users VALUES \
       (1, 'Alice', 'alice@example.com', 30), \
       (2, 'Bob', 'bob@example.com', 25), \
       (3, 'Carol', 'carol@example.com', 35), \
       (4, 'Dave', 'dave@example.com', 28), \
       (5, 'Eve', 'eve@example.com', 32)";
    Printf.printf "5 rows inserted\n";

    (* Simple select *)
    Printf.printf "\n=== All Users ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT * FROM users ORDER BY id");

    (* Filtered query *)
    Printf.printf "\n=== Users Over 30 ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT name, age FROM users \
                     WHERE age > 30 ORDER BY age");

    (* Count *)
    Printf.printf "\n=== Count ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT count() FROM users");

    (* Temporary table with Memory engine *)
    Printf.printf "\n=== Memory Table ===\n";
    Query.execute_ignore conn
      "CREATE TABLE scores (name String, score UInt32) ENGINE = Memory";
    Query.execute_ignore conn
      "INSERT INTO scores VALUES \
       ('Alice', 95), ('Bob', 87), ('Carol', 92)";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT name, score FROM scores ORDER BY score DESC");

    (* Join across tables *)
    Printf.printf "\n=== Join ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT u.name, s.score \
                     FROM users AS u \
                     INNER JOIN scores AS s ON u.name = s.name \
                     ORDER BY s.score DESC")
  ) ()
