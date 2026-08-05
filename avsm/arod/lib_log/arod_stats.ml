(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Webstats analysis of the arod_log SQLite access log database.
   All queries are read-only and operate on the existing requests table. *)

type time_range =
  | All
  | Last_hours of int
  | Since of float  (* Unix timestamp *)

let time_clause = function
  | All -> ""
  | Last_hours h ->
    Printf.sprintf " AND timestamp >= (strftime('%%s','now') - %d)" (h * 3600)
  | Since ts ->
    Printf.sprintf " AND timestamp >= %f" ts

(* Helper: run a query that returns a single integer *)
let query_int db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, v = Sqlite3_eio.fold db stmt ~init:0 ~f:(fun _acc row ->
    match row.(0) with
    | Sqlite3.Data.INT i -> Int64.to_int i
    | Sqlite3.Data.FLOAT f -> Float.to_int f
    | _ -> 0
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  v

(* Helper: run a query that returns a single float *)
let query_float db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, v = Sqlite3_eio.fold db stmt ~init:0.0 ~f:(fun _acc row ->
    match row.(0) with
    | Sqlite3.Data.FLOAT f -> f
    | Sqlite3.Data.INT i -> Int64.to_float i
    | _ -> 0.0
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  v

(* Helper: run a query returning (string * int) rows *)
let query_string_int db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let s = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let n = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    (s, n) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

(* Helper: run a query returning (string * float) rows *)
let query_string_float db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let s = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let f = match row.(1) with
      | Sqlite3.Data.FLOAT f -> f
      | Sqlite3.Data.INT i -> Int64.to_float i
      | _ -> 0.0
    in
    (s, f) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

(* Helper: query returning (string * int * float) rows *)
let query_string_int_float db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let s = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let n = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let f = match row.(2) with
      | Sqlite3.Data.FLOAT f -> f
      | Sqlite3.Data.INT i -> Int64.to_float i
      | _ -> 0.0
    in
    (s, n, f) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

(** {1 Statistics queries} *)

let total_requests db range =
  query_int db
    (Printf.sprintf "SELECT COUNT(*) FROM requests WHERE 1=1%s"
       (time_clause range))

let date_range db =
  let stmt = Sqlite3_eio.prepare db
    {|SELECT MIN(timestamp), MAX(timestamp) FROM requests|} in
  let _rc, (lo, hi) = Sqlite3_eio.fold db stmt ~init:(0.0, 0.0)
    ~f:(fun _acc row ->
      let lo = match row.(0) with Sqlite3.Data.FLOAT f -> f | _ -> 0.0 in
      let hi = match row.(1) with Sqlite3.Data.FLOAT f -> f | _ -> 0.0 in
      (lo, hi)
    ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  (lo, hi)

let status_code_breakdown db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT
           CASE
             WHEN status_code >= 200 AND status_code < 300 THEN '2xx'
             WHEN status_code >= 300 AND status_code < 400 THEN '3xx'
             WHEN status_code >= 400 AND status_code < 500 THEN '4xx'
             WHEN status_code >= 500 THEN '5xx'
             ELSE 'other'
           END AS bucket,
           COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY bucket ORDER BY bucket|}
       (time_clause range))

let top_paths db range n =
  query_string_int db
    (Printf.sprintf
       {|SELECT path, COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY path ORDER BY cnt DESC LIMIT %d|}
       (time_clause range) n)

let top_404s db range n =
  query_string_int db
    (Printf.sprintf
       {|SELECT path, COUNT(*) AS cnt
         FROM requests WHERE status_code = 404%s
         GROUP BY path ORDER BY cnt DESC LIMIT %d|}
       (time_clause range) n)

let latency_overview db range =
  let stmt = Sqlite3_eio.prepare db
    (Printf.sprintf
       {|SELECT
           AVG(duration_us),
           MIN(duration_us),
           MAX(duration_us)
         FROM requests WHERE 1=1%s|}
       (time_clause range)) in
  let _rc, (avg, mn, mx) = Sqlite3_eio.fold db stmt ~init:(0.0, 0, 0)
    ~f:(fun _acc row ->
      let avg = match row.(0) with
        | Sqlite3.Data.FLOAT f -> f | Sqlite3.Data.INT i -> Int64.to_float i | _ -> 0.0 in
      let mn = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
      let mx = match row.(2) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
      (avg, mn, mx)
    ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  (avg, mn, mx)

let latency_percentiles db range =
  let stmt = Sqlite3_eio.prepare db
    (Printf.sprintf
       {|SELECT duration_us FROM requests WHERE 1=1%s
         ORDER BY duration_us ASC|}
       (time_clause range)) in
  let _rc, durations = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let d = match row.(0) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    d :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  let arr = Array.of_list (List.rev durations) in
  let len = Array.length arr in
  if len = 0 then (0, 0, 0, 0)
  else
    let pct p = arr.(min (len - 1) (int_of_float (float_of_int len *. p))) in
    (pct 0.50, pct 0.90, pct 0.95, pct 0.99)

let latency_by_path db range n =
  query_string_int_float db
    (Printf.sprintf
       {|SELECT path, COUNT(*) AS cnt, AVG(duration_us) AS avg_us
         FROM requests WHERE 1=1%s
         GROUP BY path HAVING cnt >= 2
         ORDER BY avg_us DESC LIMIT %d|}
       (time_clause range) n)

let cache_stats db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT
           COALESCE(cache_status, 'none') AS cs,
           COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY cs ORDER BY cnt DESC|}
       (time_clause range))

let bandwidth db range =
  query_float db
    (Printf.sprintf
       "SELECT COALESCE(SUM(response_body_size), 0) FROM requests WHERE 1=1%s"
       (time_clause range))

let top_user_agents db range n =
  query_string_int db
    (Printf.sprintf
       {|SELECT COALESCE(user_agent, '(none)') AS ua, COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY ua ORDER BY cnt DESC LIMIT %d|}
       (time_clause range) n)

let top_referers db range n =
  query_string_int db
    (Printf.sprintf
       {|SELECT COALESCE(referer, '(direct)') AS ref, COUNT(*) AS cnt
         FROM requests WHERE referer IS NOT NULL AND referer != ''%s
         GROUP BY ref ORDER BY cnt DESC LIMIT %d|}
       (time_clause range) n)

let requests_per_hour db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT strftime('%%Y-%%m-%%d %%H:00', timestamp, 'unixepoch') AS hour,
              COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY hour ORDER BY hour DESC LIMIT 48|}
       (time_clause range))

let top_remote_addrs db range n =
  query_string_int db
    (Printf.sprintf
       {|SELECT remote_addr, COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY remote_addr ORDER BY cnt DESC LIMIT %d|}
       (time_clause range) n)

let content_type_breakdown db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT COALESCE(response_content_type, '(none)') AS ct, COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY ct ORDER BY cnt DESC|}
       (time_clause range))

let method_breakdown db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT method, COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY method ORDER BY cnt DESC|}
       (time_clause range))

let slowest_requests db range n =
  let stmt = Sqlite3_eio.prepare db
    (Printf.sprintf
       {|SELECT method, path, status_code, duration_us,
                strftime('%%Y-%%m-%%d %%H:%%M:%%S', timestamp, 'unixepoch')
         FROM requests WHERE 1=1%s
         ORDER BY duration_us DESC LIMIT %d|}
       (time_clause range) n) in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let meth = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let path = match row.(1) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let status = match row.(2) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let dur = match row.(3) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let ts = match row.(4) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    (meth, path, status, dur, ts) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

(** {1 Pretty-printing} *)

let human_bytes b =
  if b >= 1_073_741_824.0 then Printf.sprintf "%.1f GB" (b /. 1_073_741_824.0)
  else if b >= 1_048_576.0 then Printf.sprintf "%.1f MB" (b /. 1_048_576.0)
  else if b >= 1024.0 then Printf.sprintf "%.1f KB" (b /. 1024.0)
  else Printf.sprintf "%.0f B" b

let human_duration_us us =
  if us >= 1_000_000 then Printf.sprintf "%.2fs" (float_of_int us /. 1_000_000.0)
  else if us >= 1000 then Printf.sprintf "%.1fms" (float_of_int us /. 1000.0)
  else Printf.sprintf "%dus" us

let format_timestamp ts =
  match Ptime.of_float_s ts with
  | Some t -> Ptime.to_rfc3339 ~frac_s:0 t
  | None -> Printf.sprintf "%.0f" ts

let bar_chart max_val n width =
  if max_val = 0 then ""
  else
    let len = max 1 (n * width / max_val) in
    String.make len '#'

let print_table header rows =
  (* Compute column widths *)
  let ncols = List.length header in
  let widths = Array.make ncols 0 in
  List.iteri (fun i h -> widths.(i) <- String.length h) header;
  List.iter (fun row ->
    List.iteri (fun i cell ->
      if i < ncols then
        widths.(i) <- max widths.(i) (String.length cell)
    ) row
  ) rows;
  (* Print header *)
  List.iteri (fun i h ->
    if i > 0 then Printf.printf "  ";
    Printf.printf "%-*s" widths.(i) h
  ) header;
  Printf.printf "\n";
  (* Separator *)
  Array.iter (fun w ->
    Printf.printf "%s  " (String.make w '-')
  ) widths;
  Printf.printf "\n";
  (* Rows *)
  List.iter (fun row ->
    List.iteri (fun i cell ->
      if i > 0 then Printf.printf "  ";
      if i < ncols then Printf.printf "%-*s" widths.(i) cell
    ) row;
    Printf.printf "\n"
  ) rows

(** {1 Report} *)

let report ~sw path range =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path in
  let total = total_requests db range in
  if total = 0 then
    Printf.printf "No requests logged yet.\n"
  else begin
    (* Overview *)
    let first_ts, last_ts = date_range db in
    Printf.printf "=== Arod Access Log Statistics ===\n\n";
    Printf.printf "Period: %s to %s\n" (format_timestamp first_ts) (format_timestamp last_ts);
    Printf.printf "Total requests: %d\n" total;
    let span = last_ts -. first_ts in
    if span > 0.0 then
      Printf.printf "Avg rate: %.1f req/hour\n"
        (float_of_int total /. span *. 3600.0);
    Printf.printf "\n";

    (* Status codes *)
    Printf.printf "--- Status Codes ---\n";
    let statuses = status_code_breakdown db range in
    let max_s = List.fold_left (fun m (_, n) -> max m n) 0 statuses in
    List.iter (fun (bucket, cnt) ->
      let pct = float_of_int cnt /. float_of_int total *. 100.0 in
      Printf.printf "  %s  %6d (%5.1f%%)  %s\n"
        bucket cnt pct (bar_chart max_s cnt 30)
    ) statuses;
    Printf.printf "\n";

    (* HTTP methods *)
    Printf.printf "--- HTTP Methods ---\n";
    let methods = method_breakdown db range in
    List.iter (fun (m, cnt) ->
      Printf.printf "  %-6s  %d\n" m cnt
    ) methods;
    Printf.printf "\n";

    (* Latency overview *)
    let avg_us, min_us, max_us = latency_overview db range in
    let p50, p90, p95, p99 = latency_percentiles db range in
    Printf.printf "--- Latency ---\n";
    Printf.printf "  avg    %s\n" (human_duration_us (Float.to_int avg_us));
    Printf.printf "  min    %s\n" (human_duration_us min_us);
    Printf.printf "  max    %s\n" (human_duration_us max_us);
    Printf.printf "  p50    %s\n" (human_duration_us p50);
    Printf.printf "  p90    %s\n" (human_duration_us p90);
    Printf.printf "  p95    %s\n" (human_duration_us p95);
    Printf.printf "  p99    %s\n" (human_duration_us p99);
    Printf.printf "\n";

    (* Cache *)
    Printf.printf "--- Cache ---\n";
    let cache = cache_stats db range in
    let cache_total = List.fold_left (fun acc (_, n) -> acc + n) 0 cache in
    List.iter (fun (status, cnt) ->
      let pct = float_of_int cnt /. float_of_int cache_total *. 100.0 in
      Printf.printf "  %-6s  %6d (%5.1f%%)\n" status cnt pct
    ) cache;
    let hits = List.fold_left (fun acc (s, n) -> if s = "hit" then acc + n else acc) 0 cache in
    let cacheable = List.fold_left (fun acc (s, n) ->
      if s = "hit" || s = "miss" then acc + n else acc) 0 cache in
    if cacheable > 0 then
      Printf.printf "  Hit rate: %.1f%% (of cacheable)\n"
        (float_of_int hits /. float_of_int cacheable *. 100.0);
    Printf.printf "\n";

    (* Bandwidth *)
    let bw = bandwidth db range in
    Printf.printf "--- Bandwidth ---\n";
    Printf.printf "  Total: %s\n" (human_bytes bw);
    if total > 0 then
      Printf.printf "  Avg/request: %s\n" (human_bytes (bw /. float_of_int total));
    Printf.printf "\n";

    (* Top paths *)
    Printf.printf "--- Top Paths (by hits) ---\n";
    let paths = top_paths db range 15 in
    print_table ["Path"; "Hits"; "%%"]
      (List.map (fun (p, cnt) ->
        [p; string_of_int cnt;
         Printf.sprintf "%.1f" (float_of_int cnt /. float_of_int total *. 100.0)]
      ) paths);
    Printf.printf "\n";

    (* Slowest paths *)
    Printf.printf "--- Slowest Paths (avg latency, 2+ hits) ---\n";
    let slow_paths = latency_by_path db range 10 in
    print_table ["Path"; "Hits"; "Avg Latency"]
      (List.map (fun (p, cnt, avg) ->
        [p; string_of_int cnt; human_duration_us (Float.to_int avg)]
      ) slow_paths);
    Printf.printf "\n";

    (* Slowest individual requests *)
    Printf.printf "--- Slowest Requests ---\n";
    let slow_reqs = slowest_requests db range 10 in
    print_table ["Method"; "Path"; "Status"; "Duration"; "Time"]
      (List.map (fun (m, p, s, d, ts) ->
        [m; p; string_of_int s; human_duration_us d; ts]
      ) slow_reqs);
    Printf.printf "\n";

    (* 404s *)
    let notfounds = top_404s db range 10 in
    if notfounds <> [] then begin
      Printf.printf "--- Top 404s ---\n";
      print_table ["Path"; "Count"]
        (List.map (fun (p, cnt) -> [p; string_of_int cnt]) notfounds);
      Printf.printf "\n"
    end;

    (* Content types *)
    Printf.printf "--- Response Content Types ---\n";
    List.iter (fun (ct, cnt) ->
      Printf.printf "  %-40s  %d\n" ct cnt
    ) (content_type_breakdown db range);
    Printf.printf "\n";

    (* Top user agents *)
    Printf.printf "--- Top User Agents ---\n";
    let uas = top_user_agents db range 10 in
    List.iter (fun (ua, cnt) ->
      let short = if String.length ua > 72 then String.sub ua 0 72 ^ "..." else ua in
      Printf.printf "  %6d  %s\n" cnt short
    ) uas;
    Printf.printf "\n";

    (* Top referers *)
    let refs = top_referers db range 10 in
    if refs <> [] then begin
      Printf.printf "--- Top Referers ---\n";
      List.iter (fun (r, cnt) ->
        Printf.printf "  %6d  %s\n" cnt r
      ) refs;
      Printf.printf "\n"
    end;

    (* Top IPs *)
    Printf.printf "--- Top Remote Addresses ---\n";
    let addrs = top_remote_addrs db range 10 in
    List.iter (fun (addr, cnt) ->
      Printf.printf "  %6d  %s\n" cnt addr
    ) addrs;
    Printf.printf "\n";

    (* Traffic by hour *)
    Printf.printf "--- Requests per Hour (last 48h) ---\n";
    let hourly = requests_per_hour db range in
    let max_h = List.fold_left (fun m (_, n) -> max m n) 0 hourly in
    List.iter (fun (hour, cnt) ->
      Printf.printf "  %s  %5d  %s\n" hour cnt (bar_chart max_h cnt 40)
    ) (List.rev hourly)
  end
