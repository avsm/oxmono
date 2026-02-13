(* error_handling.ml -- Showcase error handling patterns *)

open Chdb

let try_query conn query =
  match Query.execute conn query with
  | result ->
    let s = Result.to_string result in
    Result.destroy result;
    Printf.printf "OK: %s" s
  | exception Error.E err ->
    Printf.printf "ERROR: %s\n" (Error.to_string err)

let () =
  Connection.with_connection ~f:(fun conn ->
    Printf.printf "=== Valid Query ===\n";
    try_query conn "SELECT 42";

    Printf.printf "\n=== Syntax Error ===\n";
    try_query conn "SELECTT 42";

    Printf.printf "\n=== Unknown Table ===\n";
    try_query conn "SELECT * FROM nonexistent_table";

    Printf.printf "\n=== Division By Zero ===\n";
    try_query conn "SELECT 1 / 0";

    Printf.printf "\n=== Type Error ===\n";
    try_query conn "SELECT toUInt8(256)";

    Printf.printf "\n=== Use After Close ===\n";
    let conn2 = Connection.connect () in
    Connection.close conn2;
    (match Query.execute conn2 "SELECT 1" with
     | _ -> Printf.printf "Unexpected success\n"
     | exception Error.E err ->
       Printf.printf "ERROR: %s\n" (Error.to_string err))
  ) ()
