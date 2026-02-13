(* basic_queries.ml -- Showcase basic chdb query patterns *)

open Chdb

let () =
  Connection.with_connection ~f:(fun conn ->
    (* Simple scalar queries *)
    Printf.printf "=== Scalar Queries ===\n";
    print_string (Query.execute_string conn ~format:TSV "SELECT 1");
    print_string (Query.execute_string conn ~format:TSV "SELECT 42 + 58");
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT 'hello, chdb!'");

    (* Arithmetic and type conversions *)
    Printf.printf "\n=== Arithmetic ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT 2 * 3 * 7");
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT round(pi(), 4)");
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT pow(2, 10)");

    (* String functions *)
    Printf.printf "\n=== String Functions ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT upper('clickhouse')");
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT concat('ch', 'db')");
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT length('hello')");
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT reverse('abcde')");

    (* Number sequences *)
    Printf.printf "\n=== Number Sequences ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT number FROM numbers(5)");

    (* Multiple columns *)
    Printf.printf "\n=== Multiple Columns ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT number, number * number AS square, \
                     number * number * number AS cube \
                     FROM numbers(1, 6)")
  ) ()
