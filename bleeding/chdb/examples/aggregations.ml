(* aggregations.ml -- Showcase aggregation queries *)

open Chdb

let () =
  Connection.with_connection ~f:(fun conn ->
    Printf.printf "=== Basic Aggregations ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT \
                       count() AS cnt, \
                       sum(number) AS total, \
                       avg(number) AS average, \
                       min(number) AS minimum, \
                       max(number) AS maximum \
                     FROM numbers(1, 100)");

    Printf.printf "\n=== Group By ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT \
                       number % 3 AS bucket, \
                       count() AS cnt, \
                       sum(number) AS total \
                     FROM numbers(1, 10) \
                     GROUP BY bucket \
                     ORDER BY bucket");

    Printf.printf "\n=== Having ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT \
                       number % 5 AS grp, \
                       count() AS cnt \
                     FROM numbers(100) \
                     GROUP BY grp \
                     HAVING cnt >= 20 \
                     ORDER BY grp");

    Printf.printf "\n=== Window Functions ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT \
                       number, \
                       sum(number) OVER (ORDER BY number \
                         ROWS BETWEEN UNBOUNDED PRECEDING \
                         AND CURRENT ROW) AS running_sum \
                     FROM numbers(1, 6)");

    Printf.printf "\n=== Array Aggregation ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT \
                       number % 3 AS grp, \
                       groupArray(number) AS members \
                     FROM numbers(1, 10) \
                     GROUP BY grp \
                     ORDER BY grp");

    Printf.printf "\n=== Quantiles ===\n";
    print_string (Query.execute_string conn ~format:TSV
                    "SELECT \
                       quantile(0.5)(number) AS median, \
                       quantile(0.9)(number) AS p90, \
                       quantile(0.99)(number) AS p99 \
                     FROM numbers(1000)")
  ) ()
