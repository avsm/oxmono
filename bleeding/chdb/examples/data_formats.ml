(* data_formats.ml -- Showcase different output formats *)

open Chdb

let query = "SELECT number AS id, \
             concat('item_', toString(number)) AS name, \
             number * 1.5 AS price \
             FROM numbers(1, 4)"

let () =
  Connection.with_connection ~f:(fun conn ->
    Printf.printf "=== CSV ===\n";
    print_string (Query.execute_string conn ~format:CSV query);

    Printf.printf "\n=== CSVWithNames ===\n";
    print_string (Query.execute_string conn ~format:CSVWithNames query);

    Printf.printf "\n=== TSV ===\n";
    print_string (Query.execute_string conn ~format:TSV query);

    Printf.printf "\n=== TSVWithNames ===\n";
    print_string (Query.execute_string conn ~format:TSVWithNames query);

    Printf.printf "\n=== JSONEachRow ===\n";
    print_string (Query.execute_string conn ~format:JSONEachRow query);

    Printf.printf "\n=== JSONCompactEachRow ===\n";
    print_string (Query.execute_string conn ~format:JSONCompactEachRow query);

    Printf.printf "\n=== Vertical ===\n";
    print_string (Query.execute_string conn ~format:Vertical query);

    Printf.printf "\n=== PrettyCompact ===\n";
    print_string (Query.execute_string conn ~format:PrettyCompact query)
  ) ()
