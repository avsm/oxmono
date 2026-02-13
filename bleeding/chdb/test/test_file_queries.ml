(* test_file_queries.ml -- Tests querying from CSV, JSON, TSV files *)

open Chdb

let with_conn f = Connection.with_connection ~f ()

(* Data files are copied into the build dir by dune deps.
   Fallback to source tree path for dune exec. *)
let data_dir =
  let cwd = Sys.getcwd () in
  let local = Filename.concat cwd "data" in
  if Sys.file_exists local then local
  else Filename.concat cwd "bleeding/chdb/test/data"

let data path = Filename.concat data_dir path

let file_query ~format ~schema path =
  Printf.sprintf "SELECT * FROM file('%s', %s, '%s')" path format schema

let users_schema =
  "id UInt32, name String, email String, age UInt8, department String"

let events_schema =
  "ts DateTime, user_id UInt32, action String, duration_ms UInt32"

let products_schema =
  "id UInt32, name String, category String, price Float64, stock UInt32"

let orders_schema =
  "order_id UInt32, user_id UInt32, product_id UInt32, \
   quantity UInt32, date Date"

let metrics_schema =
  "ts DateTime, metric String, value Float64, host String"

(* ---------- CSV file tests ---------- *)

let test_csv_select_all () =
  with_conn (fun conn ->
    let s = Query.execute_string conn ~format:TSV
      (file_query ~format:"CSV" ~schema:users_schema (data "users.csv")) in
    let lines = String.split_on_char '\n' s
      |> List.filter (fun l -> String.length l > 0) in
    Alcotest.(check int) "10 users" 10 (List.length lines))

let test_csv_filter () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT name, age FROM file('%s', CSV, '%s') \
       WHERE department = 'engineering' ORDER BY age"
      (data "users.csv") users_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "engineering users"
      "Alice\t30\nEve\t32\nCarol\t35\nHank\t38\n" s)

let test_csv_aggregation () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT department, count() AS cnt, avg(age) AS avg_age \
       FROM file('%s', CSV, '%s') \
       GROUP BY department ORDER BY department"
      (data "users.csv") users_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "dept aggregation"
      "engineering\t4\t33.75\nmarketing\t3\t33\nsales\t3\t32\n" s)

let test_csv_with_names () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT name, department FROM file('%s', CSVWithNames) \
       WHERE age > 35 ORDER BY name"
      (data "users_header.csv") in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "CSVWithNames filter"
      "Frank\tmarketing\nHank\tengineering\nJack\tsales\n" s)

(* ---------- JSON file tests ---------- *)

let test_json_select_all () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT count() FROM file('%s', JSONEachRow, '%s')"
      (data "events.jsonl") events_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "15 events" "15\n" s)

let test_json_filter () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT user_id, action FROM file('%s', JSONEachRow, '%s') \
       WHERE action = 'purchase' ORDER BY user_id"
      (data "events.jsonl") events_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "purchases"
      "1\tpurchase\n3\tpurchase\n4\tpurchase\n" s)

let test_json_aggregation () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT action, count() AS cnt, \
              avg(duration_ms) AS avg_dur \
       FROM file('%s', JSONEachRow, '%s') \
       GROUP BY action ORDER BY action"
      (data "events.jsonl") events_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "event aggregation"
      "login\t4\t0\nlogout\t4\t0\npage_view\t4\t1875\npurchase\t3\t4533.333333333333\n"
      s;
    (* Also verify as JSON output *)
    let j = Query.execute_string conn ~format:JSONEachRow
      (Printf.sprintf
        "SELECT action, count() AS cnt \
         FROM file('%s', JSONEachRow, '%s') \
         GROUP BY action ORDER BY action"
        (data "events.jsonl") events_schema) in
    Alcotest.(check bool) "JSON output has action" true
      (String.length j > 0 && j.[0] = '{'))

let test_json_time_range () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT count() FROM file('%s', JSONEachRow, '%s') \
       WHERE ts >= '2024-01-15 09:30:00'"
      (data "events.jsonl") events_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "events after 09:30" "8\n" s)

(* ---------- TSV file tests ---------- *)

let test_tsv_select_all () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT count() FROM file('%s', TSV, '%s')"
      (data "products.tsv") products_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "10 products" "10\n" s)

let test_tsv_filter () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT name, price FROM file('%s', TSV, '%s') \
       WHERE category = 'electronics' ORDER BY price"
      (data "products.tsv") products_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "electronics"
      "Widget\t29.99\nGadget\t49.99\nMechanism\t59.99\nWhatchamacallit\t99.99\n"
      s)

let test_tsv_aggregation () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT category, count() AS cnt, sum(stock) AS total_stock \
       FROM file('%s', TSV, '%s') \
       GROUP BY category ORDER BY category"
      (data "products.tsv") products_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "category agg"
      "electronics\t4\t315\nhome\t3\t950\noffice\t3\t520\n" s)

(* ---------- Cross-file joins ---------- *)

let test_join_csv_json () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT u.name, count() AS order_count \
       FROM file('%s', CSV, '%s') AS u \
       INNER JOIN file('%s', JSONEachRow, '%s') AS o \
         ON u.id = o.user_id \
       GROUP BY u.name ORDER BY order_count DESC, u.name"
      (data "users.csv") users_schema
      (data "orders.jsonl") orders_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "user order counts"
      "Alice\t3\nBob\t3\nCarol\t2\nDave\t2\nEve\t2\n" s)

let test_join_json_tsv () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT p.name AS product, \
              sum(o.quantity) AS total_qty, \
              sum(o.quantity * p.price) AS revenue \
       FROM file('%s', JSONEachRow, '%s') AS o \
       INNER JOIN file('%s', TSV, '%s') AS p \
         ON o.product_id = p.id \
       GROUP BY p.name \
       ORDER BY revenue DESC \
       LIMIT 3"
      (data "orders.jsonl") orders_schema
      (data "products.tsv") products_schema in
    let s = Query.execute_string conn ~format:TSV q in
    let lines = String.split_on_char '\n' s
      |> List.filter (fun l -> String.length l > 0) in
    Alcotest.(check int) "top 3 products" 3 (List.length lines))

let test_three_way_join () =
  with_conn (fun conn ->
    (* Users who made purchases and what they bought *)
    let q = Printf.sprintf
      "SELECT u.name, p.name AS product, o.quantity \
       FROM file('%s', JSONEachRow, '%s') AS o \
       INNER JOIN file('%s', CSV, '%s') AS u ON o.user_id = u.id \
       INNER JOIN file('%s', TSV, '%s') AS p ON o.product_id = p.id \
       WHERE u.department = 'engineering' \
       ORDER BY u.name, p.name"
      (data "orders.jsonl") orders_schema
      (data "users.csv") users_schema
      (data "products.tsv") products_schema in
    let s = Query.execute_string conn ~format:TSV q in
    let lines = String.split_on_char '\n' s
      |> List.filter (fun l -> String.length l > 0) in
    Alcotest.(check bool) "has results" true (List.length lines > 0);
    (* Alice is in engineering and has orders *)
    Alcotest.(check bool) "has Alice" true
      (List.exists (fun l ->
        String.length l > 5 && String.sub l 0 5 = "Alice") lines))

(* ---------- Metrics (time-series CSV) ---------- *)

let test_metrics_by_host () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT host, metric, avg(value) AS avg_val \
       FROM file('%s', CSV, '%s') \
       GROUP BY host, metric \
       ORDER BY host, metric"
      (data "metrics.csv") metrics_schema in
    let s = Query.execute_string conn ~format:TSV q in
    let lines = String.split_on_char '\n' s
      |> List.filter (fun l -> String.length l > 0) in
    Alcotest.(check int) "4 host-metric combos" 4 (List.length lines))

let test_metrics_peak () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT host, max(value) AS peak_cpu \
       FROM file('%s', CSV, '%s') \
       WHERE metric = 'cpu' \
       GROUP BY host ORDER BY host"
      (data "metrics.csv") metrics_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "peak cpu"
      "server-1\t78.9\nserver-2\t45.6\n" s)

let test_metrics_hourly () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT toHour(ts) AS hour, \
              avg(value) AS avg_cpu \
       FROM file('%s', CSV, '%s') \
       WHERE metric = 'cpu' \
       GROUP BY hour ORDER BY hour"
      (data "metrics.csv") metrics_schema in
    let s = Query.execute_string conn ~format:TSV q in
    Alcotest.(check string) "hourly cpu"
      "0\t40.3\n1\t48.5\n2\t62.25\n3\t35.8\n" s)

(* ---------- Output format roundtrips on file data ---------- *)

let test_file_to_json_output () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT name, price FROM file('%s', TSV, '%s') \
       WHERE category = 'home' ORDER BY name"
      (data "products.tsv") products_schema in
    let s = Query.execute_string conn ~format:JSONEachRow q in
    Alcotest.(check string) "JSON output"
      {|{"name":"Apparatus","price":24.99}
{"name":"Doohickey","price":9.99}
{"name":"Thingamajig","price":19.99}
|} s)

let test_file_to_csv_output () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT id, name FROM file('%s', CSV, '%s') \
       WHERE department = 'sales' ORDER BY id"
      (data "users.csv") users_schema in
    let s = Query.execute_string conn ~format:CSV q in
    Alcotest.(check string) "CSV output"
      "4,\"Dave\"\n7,\"Grace\"\n10,\"Jack\"\n" s)

(* ---------- Binary access on file data ---------- *)

let test_file_row_binary () =
  with_conn (fun conn ->
    let q = Printf.sprintf
      "SELECT toUInt64(stock) FROM file('%s', TSV, '%s') \
       ORDER BY id"
      (data "products.tsv") products_schema in
    Query.with_buffer conn ~format:RowBinary q ~f:(fun buf ->
      let sum = Binary.sum_uint64 buf in
      (* 150+75+500+200+30+300+100+250+60+120 = 1785 *)
      Alcotest.(check int64) "stock sum" 1785L sum))

(* ---------- Test runner ---------- *)

let () =
  Alcotest.run "chdb-file-queries" [
    "csv files", [
      Alcotest.test_case "select all" `Quick test_csv_select_all;
      Alcotest.test_case "filter" `Quick test_csv_filter;
      Alcotest.test_case "aggregation" `Quick test_csv_aggregation;
      Alcotest.test_case "CSVWithNames" `Quick test_csv_with_names;
    ];
    "json files", [
      Alcotest.test_case "select all" `Quick test_json_select_all;
      Alcotest.test_case "filter" `Quick test_json_filter;
      Alcotest.test_case "aggregation" `Quick test_json_aggregation;
      Alcotest.test_case "time range" `Quick test_json_time_range;
    ];
    "tsv files", [
      Alcotest.test_case "select all" `Quick test_tsv_select_all;
      Alcotest.test_case "filter" `Quick test_tsv_filter;
      Alcotest.test_case "aggregation" `Quick test_tsv_aggregation;
    ];
    "cross-file joins", [
      Alcotest.test_case "CSV + JSON join" `Quick test_join_csv_json;
      Alcotest.test_case "JSON + TSV join" `Quick test_join_json_tsv;
      Alcotest.test_case "three-way join" `Quick test_three_way_join;
    ];
    "time-series metrics", [
      Alcotest.test_case "by host" `Quick test_metrics_by_host;
      Alcotest.test_case "peak cpu" `Quick test_metrics_peak;
      Alcotest.test_case "hourly aggregation" `Quick test_metrics_hourly;
    ];
    "output formats", [
      Alcotest.test_case "file to JSON" `Quick test_file_to_json_output;
      Alcotest.test_case "file to CSV" `Quick test_file_to_csv_output;
    ];
    "binary access", [
      Alcotest.test_case "file to RowBinary" `Quick test_file_row_binary;
    ];
  ]
