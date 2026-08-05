(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Hidden stats dashboard at /action — not linked from any public page. *)

open Htmlit

(** {1 Time Range} *)

type time_range = Last_days of int | All

let time_clause = function
  | All -> ""
  | Last_days d ->
    Printf.sprintf " AND timestamp >= (strftime('%%s','now') - %d)" (d * 86400)

let range_of_string = function
  | "30d" -> Last_days 30
  | "6m" -> Last_days 180
  | "all" -> All
  | _ -> Last_days 7

let range_to_string = function
  | Last_days 7 -> "7d"
  | Last_days 30 -> "30d"
  | Last_days 180 -> "6m"
  | All -> "all"
  | Last_days d -> string_of_int d ^ "d"

let range_label = function
  | Last_days 7 -> "7 Days"
  | Last_days 30 -> "30 Days"
  | Last_days 180 -> "6 Months"
  | All -> "All Time"
  | Last_days d -> string_of_int d ^ " Days"

let all_ranges = [Last_days 7; Last_days 30; Last_days 180; All]

(** {1 UA Classification} *)

let ua_categories = [
  "Feed readers", [
    "Feedly"; "Feedbin"; "NetNewsWire"; "Newsboat"; "FreshRSS"; "Miniflux";
    "Tiny Tiny RSS"; "Inoreader"; "NewsBlur"; "Blogtrottr"; "FrostySoftStort";
    "BuobeBot"; "BuobeFeedDiscovery";
  ];
  "AI crawlers", [
    "ChatGPT-User"; "ClaudeBot"; "Aranet-SearchBot"; "Amazonbot";
    "meta-externalagent"; "GPTBot"; "Bytespider"; "Applebot-Extended";
    "PerplexityBot"; "Cohere-AI"; "anthropic-ai"; "Google-Extended";
  ];
  "SEO crawlers", [
    "AhrefsBot"; "AhrefsSiteAudit"; "SemrushBot"; "Barkrowler";
    "MJ12bot"; "DotBot";
  ];
  "Search engines", [
    "Googlebot"; "GoogleOther"; "bingbot"; "YandexBot"; "Baiduspider";
    "DuckDuckBot";
  ];
  "Link previews", [
    "Slackbot"; "matrix-hookshot"; "Twitterbot"; "WhatsApp"; "TelegramBot";
    "LinkedInBot"; "Discordbot";
  ];
  "Security scanners", [
    "CensysInspect"; "l9explore"; "NetScope"; "fhms-its-research-scanner";
    "Shadowserver"; "security research scanner";
  ];
  "Programmatic", [
    "curl/"; "python-requests"; "python-httpx"; "Go-http-client";
    "ocaml-cohttp"; "Java/"; "Ruby"; "PHP/";
  ];
]

let ua_classify_sql =
  let cases = List.map (fun (category, patterns) ->
    let conditions = List.map (fun pat ->
      Printf.sprintf "user_agent LIKE '%%%s%%'" pat
    ) patterns in
    Printf.sprintf "WHEN %s THEN '%s'"
      (String.concat " OR " conditions) category
  ) ua_categories in
  Printf.sprintf
    {|CASE
        %s
        WHEN user_agent IS NULL OR user_agent = '' THEN 'Unknown'
        ELSE 'Browsers'
      END|}
    (String.concat "\n        " cases)

(** {1 Referrer Domain Classification} *)

let referrer_categories = [
  "Search", ["google.com"; "bing.com"; "kagi.com"; "duckduckgo.com";
             "search.yahoo.com"; "ecosia.org"; "baidu.com"; "yandex."];
  "Social", ["lobste.rs"; "linkedin.com"; "t.co"; "reddit.com"; "bsky.app";
             "mastodon."; "twitter.com"; "x.com"; "threads.net"];
  "Aggregators", ["news.ycombinator.com"; "pckt.blog"; "blogtrottr.com";
                   "hacker-news.firebaseio.com"];
  "Self", ["anil.recoil.org"];
]

let classify_referrer ref_url =
  (* Extract domain: split on '/', get host, strip port, lowercase *)
  let domain = match String.split_on_char '/' ref_url with
    | _ :: "" :: host :: _ ->
      (match String.split_on_char ':' host with h :: _ -> h | [] -> host)
    | _ -> ref_url
  in
  let domain = String.lowercase_ascii domain in
  match List.find_opt (fun (_, domains) ->
    List.exists (fun d ->
      String.ends_with ~suffix:d domain || String.equal d domain
    ) domains
  ) referrer_categories with
  | Some (cat, _) -> cat
  | None -> "Other"

let classify_referrers refs =
  let by_cat = Hashtbl.create 8 in
  List.iter (fun (ref_url, cnt) ->
    let cat = classify_referrer ref_url in
    let total, items = try Hashtbl.find by_cat cat with Not_found -> (0, []) in
    Hashtbl.replace by_cat cat (total + cnt, (ref_url, cnt) :: items)
  ) refs;
  let cats = Hashtbl.fold (fun cat (total, items) acc ->
    let sorted_items = List.sort (fun (_, a) (_, b) -> compare b a) items in
    (cat, total, sorted_items) :: acc
  ) by_cat [] in
  List.sort (fun (_, a, _) (_, b, _) -> compare b a) cats

(** {1 SQL Query Helpers} *)

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

let query_string_int db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let s = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let n = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    (s, n) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

(** {1 Data Queries} *)

let total_requests db range =
  query_int db
    (Printf.sprintf "SELECT COUNT(*) FROM requests WHERE 1=1%s" (time_clause range))

let avg_response_time db range =
  query_float db
    (Printf.sprintf "SELECT COALESCE(AVG(duration_us), 0) FROM requests WHERE 1=1%s"
       (time_clause range))

let cache_hit_rate db range =
  let tc = time_clause range in
  let hits = query_int db
    (Printf.sprintf "SELECT COUNT(*) FROM requests WHERE cache_status = 'hit'%s" tc) in
  let cacheable = query_int db
    (Printf.sprintf "SELECT COUNT(*) FROM requests WHERE cache_status IN ('hit', 'miss')%s" tc) in
  if cacheable > 0 then float_of_int hits /. float_of_int cacheable *. 100.0
  else 0.0

let error_rate db range =
  let tc = time_clause range in
  let errors = query_int db
    (Printf.sprintf "SELECT COUNT(*) FROM requests WHERE status_code >= 400%s" tc) in
  let total = query_int db
    (Printf.sprintf "SELECT COUNT(*) FROM requests WHERE 1=1%s" tc) in
  if total > 0 then float_of_int errors /. float_of_int total *. 100.0
  else 0.0

let total_bandwidth db range =
  query_float db
    (Printf.sprintf "SELECT COALESCE(SUM(response_body_size), 0) FROM requests WHERE 1=1%s"
       (time_clause range))

let status_breakdown db range =
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
        FROM requests
        WHERE 1=1%s
        GROUP BY bucket ORDER BY bucket|}
      (time_clause range))

let content_type_breakdown db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT COALESCE(response_content_type, '(none)') AS ct, COUNT(*) AS cnt
         FROM requests WHERE 1=1%s
         GROUP BY ct ORDER BY cnt DESC|}
       (time_clause range))

let traffic_over_time db range =
  let bucket_expr, limit =
    match range with
    | Last_days d when d <= 7 ->
      (* Hourly buckets *)
      "strftime('%Y-%m-%d %H:00', timestamp, 'unixepoch')", d * 24
    | Last_days d when d <= 30 ->
      (* 6-hour blocks *)
      ("strftime('%Y-%m-%d ', timestamp, 'unixepoch') || printf('%02d:00', "
       ^ "(CAST(strftime('%H', timestamp, 'unixepoch') AS INTEGER) / 6) * 6)"),
      d * 4
    | _ ->
      (* Daily buckets *)
      "strftime('%Y-%m-%d', timestamp, 'unixepoch')", 366
  in
  query_string_int db
    (Printf.sprintf
       {|SELECT %s AS bucket, COUNT(*) AS cnt
         FROM requests
         WHERE 1=1%s
         GROUP BY bucket ORDER BY bucket ASC
         LIMIT %d|}
       bucket_expr (time_clause range) limit)

let latency_percentiles db range =
  let sql = Printf.sprintf
    {|SELECT duration_us FROM requests WHERE 1=1%s ORDER BY duration_us ASC|}
    (time_clause range) in
  let stmt = Sqlite3_eio.prepare db sql in
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

let latency_histogram db range =
  query_string_int db
    (Printf.sprintf
      {|SELECT
          CASE
            WHEN duration_us < 100 THEN '<0.1ms'
            WHEN duration_us < 500 THEN '0.1-0.5ms'
            WHEN duration_us < 1000 THEN '0.5-1ms'
            WHEN duration_us < 5000 THEN '1-5ms'
            WHEN duration_us < 10000 THEN '5-10ms'
            WHEN duration_us < 50000 THEN '10-50ms'
            WHEN duration_us < 100000 THEN '50-100ms'
            WHEN duration_us < 500000 THEN '100-500ms'
            WHEN duration_us < 1000000 THEN '0.5-1s'
            ELSE '>1s'
          END AS bucket,
          COUNT(*) AS cnt
        FROM requests
        WHERE 1=1%s
        GROUP BY bucket
        ORDER BY MIN(duration_us) ASC|}
      (time_clause range))

let all_referers db range =
  query_string_int db
    (Printf.sprintf
       {|SELECT COALESCE(referer, '(direct)') AS ref, COUNT(*) AS cnt
         FROM requests WHERE referer IS NOT NULL AND referer <> ''%s
         GROUP BY ref ORDER BY cnt DESC LIMIT 100|}
       (time_clause range))

let recent_requests db =
  let stmt = Sqlite3_eio.prepare db
    {|SELECT
        strftime('%Y-%m-%d %H:%M:%S', timestamp, 'unixepoch') AS ts,
        method, path, status_code, duration_us, cache_status,
        response_body_size
      FROM requests
      ORDER BY timestamp DESC LIMIT 50|} in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let ts = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let meth = match row.(1) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let path = match row.(2) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let status = match row.(3) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let dur = match row.(4) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let cache = match row.(5) with Sqlite3.Data.TEXT s -> s | Sqlite3.Data.NULL -> "" | _ -> "" in
    let size = match row.(6) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    (ts, meth, path, status, dur, cache, size) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

let cache_breakdown db range =
  query_string_int db
    (Printf.sprintf
      {|SELECT COALESCE(cache_status, 'none') AS cs, COUNT(*) AS cnt
        FROM requests WHERE 1=1%s GROUP BY cs ORDER BY cnt DESC|}
      (time_clause range))

let cache_rate_over_time db range =
  let bucket_expr, limit =
    match range with
    | Last_days d when d <= 7 ->
      "strftime('%Y-%m-%d %H:00', timestamp, 'unixepoch')", d * 24
    | Last_days d when d <= 30 ->
      ("strftime('%Y-%m-%d ', timestamp, 'unixepoch') || printf('%02d:00', "
       ^ "(CAST(strftime('%H', timestamp, 'unixepoch') AS INTEGER) / 6) * 6)"),
      d * 4
    | _ ->
      "strftime('%Y-%m-%d', timestamp, 'unixepoch')", 366
  in
  let sql = Printf.sprintf
    {|SELECT %s AS bucket,
             SUM(CASE WHEN cache_status = 'hit' THEN 1 ELSE 0 END) * 100.0 /
               NULLIF(SUM(CASE WHEN cache_status IN ('hit','miss') THEN 1 ELSE 0 END), 0) AS rate
      FROM requests
      WHERE 1=1%s
      GROUP BY bucket ORDER BY bucket ASC
      LIMIT %d|}
    bucket_expr (time_clause range) limit in
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let hour = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let rate = match row.(1) with
      | Sqlite3.Data.FLOAT f -> f
      | Sqlite3.Data.NULL -> 0.0
      | _ -> 0.0 in
    (hour, rate) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

(** {1 String Helpers} *)

let contains_substr s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0

(** {1 Attack Probe Detection} *)

let attack_path_patterns = [
  "/wp-"; "/wp-login"; "/wp-admin"; "/xmlrpc.php";
  "/.env"; "/.git/"; "/config.json"; "/.aws"; "/.ssh";
  "/actuator/"; "/geoserver/"; "/solr/"; "/struts";
  "/cgi-bin/"; "/boaform/"; "/SDK/"; "/admin/";
  "/phpmyadmin"; "/myadmin";
  "..%2F"; "..%252F"; "../";
  "/backup/"; "/bin/"; "/bins/"; "/LIFE";
  "/login"; "/console"; "/debug"; "/_next/server";
]

let is_connect_probe path =
  (try let _ = String.index path ':' in
       not (String.starts_with ~prefix:"/" path)
   with Not_found -> false)
  || String.starts_with ~prefix:"http://" path

let is_random_hash path =
  String.length path > 10 &&
  String.get path 0 = '/' &&
  let s = String.sub path 1 (String.length path - 1) in
  String.for_all (fun c ->
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
  ) s

let is_attack_probe path =
  is_connect_probe path ||
  is_random_hash path ||
  List.exists (fun pat -> contains_substr path pat) attack_path_patterns

let recent_errors_with_referrer db =
  let range = Last_days 7 in
  let stmt = Sqlite3_eio.prepare db
    (Printf.sprintf
       {|SELECT path, COUNT(*) AS cnt,
                (SELECT referer FROM requests r2
                 WHERE r2.path = requests.path AND r2.referer IS NOT NULL
                   AND r2.referer <> '' AND r2.status_code >= 400
                 ORDER BY r2.timestamp DESC LIMIT 1) AS top_ref
         FROM requests WHERE status_code >= 400%s
         GROUP BY path ORDER BY cnt DESC LIMIT 200|}
       (time_clause range)) in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let path = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let cnt = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let referrer = match row.(2) with
      | Sqlite3.Data.TEXT s when s <> "" -> Some s
      | _ -> None in
    (path, cnt, referrer) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

let feed_paths = ["/news.xml"; "/perma.xml"; "/perma.json"; "/feed.json"; "/notes/atom.xml"]

let feed_overview db range =
  let paths_sql = String.concat "," (List.map (Printf.sprintf "'%s'") feed_paths) in
  let stmt = Sqlite3_eio.prepare db
    (Printf.sprintf
       {|SELECT path, COUNT(*) AS cnt, COUNT(DISTINCT user_agent) AS unique_uas
         FROM requests WHERE path IN (%s)%s
         GROUP BY path ORDER BY cnt DESC|}
       paths_sql (time_clause range)) in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let path = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let cnt = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let uas = match row.(2) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    (path, cnt, uas) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

let feed_top_readers db range =
  let paths_sql = String.concat "," (List.map (Printf.sprintf "'%s'") feed_paths) in
  query_string_int db
    (Printf.sprintf
       {|SELECT COALESCE(user_agent, '(none)') AS ua, COUNT(*) AS cnt
         FROM requests WHERE path IN (%s)%s
         GROUP BY ua ORDER BY cnt DESC LIMIT 15|}
       paths_sql (time_clause range))

let parse_subscriber_count ua =
  let parts = String.split_on_char ' ' ua in
  let rec find = function
    | n :: s :: _ when String.starts_with ~prefix:"subscriber" s ->
      (try Some (int_of_string n) with _ -> None)
    | _ :: rest -> find rest
    | [] -> None
  in
  find parts

let simplify_feed_reader ua =
  let known = [
    "Feedly", "Feedly"; "Feedbin", "Feedbin"; "NetNewsWire", "NetNewsWire";
    "Newsboat", "Newsboat"; "FreshRSS", "FreshRSS"; "Miniflux", "Miniflux";
    "Tiny Tiny RSS", "Tiny Tiny RSS"; "Inoreader", "Inoreader";
    "NewsBlur", "NewsBlur"; "Blogtrottr", "Blogtrottr";
    "FrostySoftStort", "FrostySoftStort"; "matrix-hookshot", "Matrix";
    "Slackbot", "Slack"; "python-httpx", "python-httpx";
  ] in
  match List.find_opt (fun (pat, _) -> contains_substr ua pat) known with
  | Some (_, name) -> name
  | None ->
    if String.length ua > 50 then String.sub ua 0 50 ^ "..." else ua

let top_content_by_type db range content_type n =
  let prefix = "/" ^ content_type ^ "/%" in
  let exact = "/" ^ content_type in
  let sql = Printf.sprintf
    {|SELECT path,
             COUNT(*) AS cnt,
             COUNT(DISTINCT referer) AS unique_refs,
             COALESCE(SUM(response_body_size), 0) AS bw,
             AVG(duration_us) AS avg_us
      FROM requests
      WHERE path LIKE '%s' AND path != '%s'
        AND status_code = 200%s
      GROUP BY path ORDER BY cnt DESC LIMIT %d|}
    prefix exact (time_clause range) n in
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, rows = Sqlite3_eio.fold db stmt ~init:[] ~f:(fun acc row ->
    let path = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let cnt = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let unique_refs = match row.(2) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let bw = match row.(3) with
      | Sqlite3.Data.FLOAT f -> f
      | Sqlite3.Data.INT i -> Int64.to_float i
      | _ -> 0.0 in
    let avg_us = match row.(4) with
      | Sqlite3.Data.FLOAT f -> f
      | Sqlite3.Data.INT i -> Int64.to_float i
      | _ -> 0.0 in
    (path, cnt, unique_refs, bw, avg_us) :: acc
  ) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  List.rev rows

let traffic_classification db range =
  let sql = Printf.sprintf
    {|SELECT %s AS category,
             COUNT(*) AS cnt,
             COALESCE(SUM(response_body_size), 0) AS bw
      FROM requests WHERE 1=1%s
      GROUP BY category ORDER BY cnt DESC|}
    ua_classify_sql (time_clause range) in
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

(** {1 Formatting Helpers} *)

let human_bytes b =
  if b >= 1_073_741_824.0 then Printf.sprintf "%.1f GB" (b /. 1_073_741_824.0)
  else if b >= 1_048_576.0 then Printf.sprintf "%.1f MB" (b /. 1_048_576.0)
  else if b >= 1024.0 then Printf.sprintf "%.1f KB" (b /. 1024.0)
  else Printf.sprintf "%.0f B" b

let human_duration_us us =
  if us >= 1_000_000 then Printf.sprintf "%.2fs" (float_of_int us /. 1_000_000.0)
  else if us >= 1000 then Printf.sprintf "%.1fms" (float_of_int us /. 1000.0)
  else Printf.sprintf "%dus" us

let format_number n =
  let s = string_of_int n in
  let len = String.length s in
  if len <= 3 then s
  else
    let buf = Buffer.create (len + len / 3) in
    let rem = len mod 3 in
    for i = 0 to len - 1 do
      if i > 0 && (i - rem) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf s.[i]
    done;
    Buffer.contents buf

(** {1 SVG Chart Rendering} *)

(** Render a bar chart as SVG. *)
let svg_bar_chart ~width ~height ~bars ~color () =
  let n = List.length bars in
  if n = 0 then El.void
  else
    let max_val = List.fold_left (fun m (_, v) -> max m v) 0 bars in
    let max_val = max max_val 1 in
    let bar_w = float_of_int width /. float_of_int n in
    let gap = max 1.0 (bar_w *. 0.1) in
    let actual_w = bar_w -. gap in
    let bars_els = List.mapi (fun i (label, value) ->
      let h = float_of_int value /. float_of_int max_val *. (float_of_int height -. 20.0) in
      let h = max h 1.0 in
      let x = float_of_int i *. bar_w +. (gap /. 2.0) in
      let y = float_of_int height -. h -. 16.0 in
      El.splice [
        El.v "rect" ~at:[
          At.v "x" (Printf.sprintf "%.1f" x);
          At.v "y" (Printf.sprintf "%.1f" y);
          At.v "width" (Printf.sprintf "%.1f" actual_w);
          At.v "height" (Printf.sprintf "%.1f" h);
          At.v "fill" color;
          At.v "rx" "2";
        ] [
          El.v "title" ~at:[] [El.txt (Printf.sprintf "%s: %s" label (format_number value))]
        ];
        (* Label every Nth bar to avoid overlap *)
        (if n <= 24 || i mod (max 1 (n / 12)) = 0 then
           let short_label =
             if String.length label > 5 then
               (* Extract just the hour part: "YYYY-MM-DD HH:00" -> "HH" *)
               let parts = String.split_on_char ' ' label in
               match parts with
               | [_; time] -> (try String.sub time 0 2 with _ -> label)
               | _ -> label
             else label
           in
           El.v "text" ~at:[
             At.v "x" (Printf.sprintf "%.1f" (x +. actual_w /. 2.0));
             At.v "y" (Printf.sprintf "%d" height);
             At.v "text-anchor" "middle";
             At.v "font-size" "9";
             At.v "fill" "currentColor";
             At.v "opacity" "0.6";
           ] [El.txt short_label]
         else El.void)
      ]
    ) bars in
    El.v "svg" ~at:[
      At.v "viewBox" (Printf.sprintf "0 0 %d %d" width height);
      At.v "width" "100%";
      At.v "height" (string_of_int height);
      At.class' "stats-chart";
      At.v "preserveAspectRatio" "none";
    ] bars_els

(** Render a horizontal bar chart for status codes. *)
let svg_horizontal_bars ~width ~height ~bars () =
  let n = List.length bars in
  if n = 0 then El.void
  else
    let max_val = List.fold_left (fun m (_, v, _) -> max m v) 0 bars in
    let max_val = max max_val 1 in
    let bar_h = float_of_int height /. float_of_int n in
    let gap = 4.0 in
    let actual_h = bar_h -. gap in
    let label_w = 80.0 in
    let chart_w = float_of_int width -. label_w -. 10.0 in
    let bars_els = List.mapi (fun i (label, value, color) ->
      let w = float_of_int value /. float_of_int max_val *. chart_w in
      let w = max w 2.0 in
      let y = float_of_int i *. bar_h +. (gap /. 2.0) in
      El.splice [
        El.v "text" ~at:[
          At.v "x" "0";
          At.v "y" (Printf.sprintf "%.1f" (y +. actual_h /. 2.0 +. 4.0));
          At.v "font-size" "12";
          At.v "fill" "currentColor";
          At.v "font-weight" "600";
        ] [El.txt label];
        El.v "rect" ~at:[
          At.v "x" (Printf.sprintf "%.0f" label_w);
          At.v "y" (Printf.sprintf "%.1f" y);
          At.v "width" (Printf.sprintf "%.1f" w);
          At.v "height" (Printf.sprintf "%.1f" actual_h);
          At.v "fill" color;
          At.v "rx" "3";
        ] [];
        El.v "text" ~at:[
          At.v "x" (Printf.sprintf "%.0f" (label_w +. w +. 6.0));
          At.v "y" (Printf.sprintf "%.1f" (y +. actual_h /. 2.0 +. 4.0));
          At.v "font-size" "11";
          At.v "fill" "currentColor";
          At.v "opacity" "0.7";
        ] [El.txt (format_number value)];
      ]
    ) bars in
    El.v "svg" ~at:[
      At.v "viewBox" (Printf.sprintf "0 0 %d %d" width height);
      At.v "width" "100%";
      At.v "height" (string_of_int height);
      At.class' "stats-chart";
    ] bars_els

(** Render a latency histogram as SVG. *)
let svg_latency_histogram ~width ~height ~bars () =
  svg_bar_chart ~width ~height ~bars ~color:"#6366f1" ()

(** Render cache rate over time as SVG line/area chart. *)
let svg_cache_rate_chart ~width ~height ~data () =
  let n = List.length data in
  if n = 0 then El.void
  else
    let padding_top = 10.0 in
    let padding_bottom = 20.0 in
    let chart_h = float_of_int height -. padding_top -. padding_bottom in
    let dx = float_of_int width /. float_of_int (max 1 (n - 1)) in
    let points = List.mapi (fun i (_label, rate) ->
      let x = float_of_int i *. dx in
      let y = padding_top +. chart_h -. (rate /. 100.0 *. chart_h) in
      (x, y)
    ) data in
    let line_str = String.concat " " (List.mapi (fun i (x, y) ->
      Printf.sprintf "%s%.1f,%.1f" (if i = 0 then "M" else "L") x y
    ) points) in
    (* Area fill *)
    let area_str =
      line_str
      ^ Printf.sprintf " L%.1f,%.1f L0,%.1f Z"
          (float_of_int width) (padding_top +. chart_h) (padding_top +. chart_h)
    in
    El.v "svg" ~at:[
      At.v "viewBox" (Printf.sprintf "0 0 %d %d" width height);
      At.v "width" "100%";
      At.v "height" (string_of_int height);
      At.class' "stats-chart";
    ] [
      (* Grid lines at 25%, 50%, 75%, 100% *)
      El.splice (List.map (fun pct ->
        let y = padding_top +. chart_h -. (pct /. 100.0 *. chart_h) in
        El.splice [
          El.v "line" ~at:[
            At.v "x1" "0"; At.v "y1" (Printf.sprintf "%.1f" y);
            At.v "x2" (string_of_int width); At.v "y2" (Printf.sprintf "%.1f" y);
            At.v "stroke" "currentColor"; At.v "stroke-opacity" "0.1";
            At.v "stroke-dasharray" "4,4";
          ] [];
          El.v "text" ~at:[
            At.v "x" (string_of_int (width - 2));
            At.v "y" (Printf.sprintf "%.1f" (y -. 2.0));
            At.v "text-anchor" "end";
            At.v "font-size" "9"; At.v "fill" "currentColor"; At.v "opacity" "0.4";
          ] [El.txt (Printf.sprintf "%.0f%%" pct)];
        ]
      ) [25.0; 50.0; 75.0; 100.0]);
      (* Area *)
      El.v "path" ~at:[
        At.v "d" area_str;
        At.v "fill" "#22c55e"; At.v "fill-opacity" "0.15";
      ] [];
      (* Line *)
      El.v "path" ~at:[
        At.v "d" line_str;
        At.v "fill" "none"; At.v "stroke" "#22c55e"; At.v "stroke-width" "2";
      ] [];
    ]

(** {1 Dashboard CSS} *)

let dashboard_css = {|
  .stats-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
    gap: 1rem;
    margin-bottom: 2rem;
  }
  .stats-card {
    background: var(--color-surface, #f8f8f6);
    border: 1px solid var(--color-border, #e5e5e0);
    border-radius: 0.75rem;
    padding: 1.25rem;
    transition: box-shadow 0.2s;
  }
  .stats-card:hover {
    box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  }
  .stats-card-label {
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    opacity: 0.6;
    margin-bottom: 0.25rem;
  }
  .stats-card-value {
    font-size: 1.75rem;
    font-weight: 700;
    font-variant-numeric: tabular-nums;
    line-height: 1.2;
  }
  .stats-card-sub {
    font-size: 0.75rem;
    opacity: 0.5;
    margin-top: 0.125rem;
  }
  .stats-section {
    margin-bottom: 2.5rem;
  }
  .stats-section h2 {
    font-size: 1rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    opacity: 0.7;
    margin-bottom: 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 2px solid var(--color-border, #e5e5e0);
  }
  .stats-chart {
    display: block;
    color: var(--color-text, #1a1a1a);
  }
  .stats-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 0.85rem;
    font-variant-numeric: tabular-nums;
  }
  .stats-table th {
    text-align: left;
    font-size: 0.7rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    opacity: 0.5;
    padding: 0.5rem 0.75rem;
    border-bottom: 2px solid var(--color-border, #e5e5e0);
  }
  .stats-table td {
    padding: 0.4rem 0.75rem;
    border-bottom: 1px solid var(--color-border, #e5e5e0);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 400px;
  }
  .stats-table tr:hover td {
    background: var(--color-surface, #f8f8f6);
  }
  .stats-table .num {
    text-align: right;
    font-variant-numeric: tabular-nums;
  }
  .stats-table .path-cell {
    max-width: 300px;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .active-col {
    background: var(--color-surface-alt, #eeeeea);
  }
  .metric-label {
    font-weight: 700;
  }
  .status-2xx { color: #22c55e; }
  .status-3xx { color: #3b82f6; }
  .status-4xx { color: #f59e0b; }
  .status-5xx { color: #ef4444; }
  .status-badge {
    display: inline-block;
    padding: 0.125rem 0.5rem;
    border-radius: 9999px;
    font-size: 0.75rem;
    font-weight: 600;
  }
  .badge-2xx { background: #dcfce7; color: #166534; }
  .badge-3xx { background: #dbeafe; color: #1e40af; }
  .badge-4xx { background: #fef3c7; color: #92400e; }
  .badge-5xx { background: #fee2e2; color: #991b1b; }
  .chart-container {
    background: var(--color-surface, #f8f8f6);
    border: 1px solid var(--color-border, #e5e5e0);
    border-radius: 0.75rem;
    padding: 1rem;
    overflow-x: auto;
  }
  .live-dot {
    display: inline-block;
    width: 8px;
    height: 8px;
    background: #22c55e;
    border-radius: 50%;
    animation: pulse 2s infinite;
  }
  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.4; }
  }
  .stats-two-col {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 2rem;
  }
  @media (max-width: 768px) {
    .stats-two-col {
      grid-template-columns: 1fr;
    }
  }
  @media (prefers-color-scheme: dark) {
    .badge-2xx { background: #166534; color: #dcfce7; }
    .badge-3xx { background: #1e40af; color: #dbeafe; }
    .badge-4xx { background: #92400e; color: #fef3c7; }
    .badge-5xx { background: #991b1b; color: #fee2e2; }
  }
  .tabs {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
  }
  .tab {
    padding: 0.5rem 1rem;
    border-radius: 0.5rem;
    font-size: 0.85rem;
    font-weight: 600;
    text-decoration: none;
    color: inherit;
    background: var(--color-surface, #f8f8f6);
    border: 1px solid var(--color-border, #e5e5e0);
    transition: all 0.15s;
  }
  .tab:hover {
    background: var(--color-surface-alt, #eeeeea);
  }
  .tab-active {
    background: var(--color-text, #1a1a1a);
    color: var(--color-bg, #fffffc);
    border-color: transparent;
  }
|}

(** {1 Dashboard JS} *)

let dashboard_js = {|
(function() {
  function td(text, cls, title) {
    var c = document.createElement('td');
    if (cls) c.className = cls;
    if (title) c.title = title;
    c.textContent = text;
    return c;
  }
  function refreshLive() {
    fetch('/action/api/recent')
      .then(r => r.json())
      .then(data => {
        const tbody = document.getElementById('live-tbody');
        if (!tbody || !data.requests) return;
        tbody.innerHTML = '';
        data.requests.forEach(r => {
          const tr = document.createElement('tr');
          const statusCls = r.status < 300 ? 'badge-2xx' :
                           r.status < 400 ? 'badge-3xx' :
                           r.status < 500 ? 'badge-4xx' : 'badge-5xx';
          tr.appendChild(td(r.timestamp));
          var mtd = td(r.method); mtd.innerHTML = '<strong>' + mtd.textContent + '</strong>';
          tr.appendChild(mtd);
          tr.appendChild(td(r.path, 'path-cell', r.path));
          var std = document.createElement('td');
          var span = document.createElement('span');
          span.className = 'status-badge ' + statusCls;
          span.textContent = r.status;
          std.appendChild(span);
          tr.appendChild(std);
          tr.appendChild(td(r.duration, 'num'));
          tr.appendChild(td(r.cache || '-'));
          tr.appendChild(td(r.size, 'num'));
          tbody.appendChild(tr);
        });
      })
      .catch(() => {});
  }
  setInterval(refreshLive, 5000);
})();
|}

(** {1 HTML Rendering} *)

let status_color = function
  | "2xx" -> "#22c55e"
  | "3xx" -> "#3b82f6"
  | "4xx" -> "#f59e0b"
  | "5xx" -> "#ef4444"
  | _ -> "#9ca3af"

let status_badge_class status =
  if status < 300 then "badge-2xx"
  else if status < 400 then "badge-3xx"
  else if status < 500 then "badge-4xx"
  else "badge-5xx"

let time_tabs range =
  let tab r =
    let active = range_to_string range = range_to_string r in
    El.a ~at:[
      At.href (Printf.sprintf "/action?range=%s" (range_to_string r));
      At.class' (if active then "tab tab-active" else "tab");
    ] [El.txt (range_label r)]
  in
  El.div ~at:[At.class' "tabs"] (List.map tab all_ranges)

let overview_comparison db active_range =
  let metrics = List.map (fun range ->
    let total = total_requests db range in
    let avg_lat = avg_response_time db range in
    let cache_rate = cache_hit_rate db range in
    let err_rate = error_rate db range in
    let bw = total_bandwidth db range in
    (range, total, avg_lat, cache_rate, err_rate, bw)
  ) all_ranges in
  let col_at range =
    if range_to_string range = range_to_string active_range
    then [At.class' "num active-col"]
    else [At.class' "num"]
  in
  let header =
    El.v "tr" ~at:[] (
      El.v "th" ~at:[] [El.txt "Metric"] ::
      List.map (fun (range, _, _, _, _, _) ->
        El.v "th" ~at:(col_at range) [El.txt (range_label range)]
      ) metrics
    )
  in
  let metric_row label f =
    El.v "tr" ~at:[] (
      El.v "td" ~at:[At.class' "metric-label"] [El.txt label] ::
      List.map (fun (range, _, _, _, _, _ as m) ->
        El.v "td" ~at:(col_at range) [El.txt (f m)]
      ) metrics
    )
  in
  let rows = [
    metric_row "Requests" (fun (_, total, _, _, _, _) -> format_number total);
    metric_row "Avg Latency" (fun (_, _, avg_lat, _, _, _) ->
      human_duration_us (Float.to_int avg_lat));
    metric_row "Cache Rate" (fun (_, _, _, cache_rate, _, _) ->
      Printf.sprintf "%.1f%%" cache_rate);
    metric_row "Error Rate" (fun (_, _, _, _, err_rate, _) ->
      Printf.sprintf "%.1f%%" err_rate);
    metric_row "Bandwidth" (fun (_, _, _, _, _, bw) -> human_bytes bw);
  ] in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Overview"];
    El.div ~at:[At.class' "chart-container"] [
      El.table ~at:[At.class' "stats-table"] [
        El.v "thead" ~at:[] [header];
        El.v "tbody" ~at:[] rows
      ]
    ]
  ]

let traffic_section db range =
  let data = traffic_over_time db range in
  let bars = List.map (fun (h, c) -> (h, c)) data in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt (Printf.sprintf "Traffic Over Time (%s)" (range_label range))];
    El.div ~at:[At.class' "chart-container"] [
      svg_bar_chart ~width:800 ~height:200 ~bars ~color:"#6366f1" ()
    ]
  ]

let status_section db range =
  let statuses = status_breakdown db range in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 statuses in
  let bars = List.map (fun (bucket, cnt) ->
    (bucket, cnt, status_color bucket)
  ) statuses in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Response Status Distribution"];
    El.div ~at:[At.class' "chart-container"] [
      svg_horizontal_bars ~width:500 ~height:(max 80 (List.length bars * 40)) ~bars ()
    ];
    El.div ~at:[At.class' "mt-3 text-sm opacity-60"] [
      El.txt (Printf.sprintf "Total: %s requests" (format_number total))
    ]
  ]

let simplify_ct ct =
  match String.split_on_char ';' ct with
  | short :: _ -> String.trim short
  | [] -> ct

let content_type_section db range =
  let raw = content_type_breakdown db range in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 raw in
  (* Merge rows after simplifying MIME types *)
  let merged = Hashtbl.create 16 in
  List.iter (fun (ct, cnt) ->
    let key = simplify_ct ct in
    let prev = try Hashtbl.find merged key with Not_found -> 0 in
    Hashtbl.replace merged key (prev + cnt)
  ) raw;
  let items = Hashtbl.fold (fun k v acc -> (k, v) :: acc) merged [] in
  let items = List.sort (fun (_, a) (_, b) -> compare b a) items in
  let rows = List.map (fun (ct, cnt) ->
    let pct = if total > 0 then float_of_int cnt /. float_of_int total *. 100.0 else 0.0 in
    El.v "tr" ~at:[] [
      El.v "td" ~at:[] [El.txt ct];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
      El.v "td" ~at:[At.class' "num"] [El.txt (Printf.sprintf "%.1f%%" pct)];
    ]
  ) items in
  El.div ~at:[At.class' "chart-container"] [
    El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"]
      [El.txt "Response Content Types"];
    El.table ~at:[At.class' "stats-table"] [
      El.v "thead" ~at:[] [
        El.v "tr" ~at:[] [
          El.v "th" ~at:[] [El.txt "Type"];
          El.v "th" ~at:[At.class' "num"] [El.txt "Count"];
          El.v "th" ~at:[At.class' "num"] [El.txt "%"];
        ]
      ];
      El.v "tbody" ~at:[] rows
    ]
  ]

let latency_section db range =
  let p50, p90, p95, p99 = latency_percentiles db range in
  let hist = latency_histogram db range in
  let bars = List.map (fun (bucket, cnt) -> (bucket, cnt)) hist in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Latency Distribution"];
    El.div ~at:[At.class' "stats-grid"; At.style "margin-bottom: 1rem"] [
      El.div ~at:[At.class' "stats-card"] [
        El.div ~at:[At.class' "stats-card-label"] [El.txt "p50"];
        El.div ~at:[At.class' "stats-card-value"] [El.txt (human_duration_us p50)]];
      El.div ~at:[At.class' "stats-card"] [
        El.div ~at:[At.class' "stats-card-label"] [El.txt "p90"];
        El.div ~at:[At.class' "stats-card-value"] [El.txt (human_duration_us p90)]];
      El.div ~at:[At.class' "stats-card"] [
        El.div ~at:[At.class' "stats-card-label"] [El.txt "p95"];
        El.div ~at:[At.class' "stats-card-value"] [El.txt (human_duration_us p95)]];
      El.div ~at:[At.class' "stats-card"] [
        El.div ~at:[At.class' "stats-card-label"] [El.txt "p99"];
        El.div ~at:[At.class' "stats-card-value"] [El.txt (human_duration_us p99)]];
    ];
    El.div ~at:[At.class' "chart-container"] [
      svg_latency_histogram ~width:600 ~height:180 ~bars ()
    ]
  ]

let referrer_section db range =
  let refs = all_referers db range in
  let cats = classify_referrers refs in
  let total_referred = List.fold_left (fun acc (_, t, _) -> acc + t) 0 cats in
  let truncate_url url n =
    if String.length url > n then String.sub url 0 n ^ "..." else url
  in
  let rows = List.concat_map (fun (cat, total, items) ->
    let pct = if total_referred > 0
      then float_of_int total /. float_of_int total_referred *. 100.0
      else 0.0 in
    let header_row = El.v "tr" ~at:[] [
      El.v "td" ~at:[] [El.strong [El.txt cat]];
      El.v "td" ~at:[At.class' "num"] [El.strong [El.txt (format_number total)]];
      El.v "td" ~at:[At.class' "num"] [El.strong [El.txt (Printf.sprintf "%.1f%%" pct)]];
    ] in
    if cat = "Self" then [header_row]
    else
      let top_items = List.filteri (fun i _ -> i < 3) items in
      let sub_rows = List.map (fun (ref_url, cnt) ->
        let item_pct = if total_referred > 0
          then float_of_int cnt /. float_of_int total_referred *. 100.0
          else 0.0 in
        El.v "tr" ~at:[] [
          El.v "td" ~at:[At.style "padding-left:1.5rem"; At.v "title" ref_url]
            [El.span ~at:[At.style "opacity:0.7"]
              [El.txt (truncate_url ref_url 60)]];
          El.v "td" ~at:[At.class' "num"; At.style "opacity:0.7"]
            [El.txt (format_number cnt)];
          El.v "td" ~at:[At.class' "num"; At.style "opacity:0.7"]
            [El.txt (Printf.sprintf "%.1f%%" item_pct)];
        ]
      ) top_items in
      header_row :: sub_rows
  ) cats in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Referrer Analysis"];
    El.div ~at:[At.class' "chart-container"] [
      El.table ~at:[At.class' "stats-table"] [
        El.v "thead" ~at:[] [
          El.v "tr" ~at:[] [
            El.v "th" ~at:[] [El.txt "Source"];
            El.v "th" ~at:[At.class' "num"] [El.txt "Visits"];
            El.v "th" ~at:[At.class' "num"] [El.txt "%"];
          ]
        ];
        El.v "tbody" ~at:[] rows
      ]
    ]
  ]

let errors_section db =
  let all_errors = recent_errors_with_referrer db in
  let attacks, legit = List.partition (fun (path, _, _) -> is_attack_probe path) all_errors in
  let attack_total = List.fold_left (fun acc (_, cnt, _) -> acc + cnt) 0 attacks in
  let legit_rows =
    let top = List.filteri (fun i _ -> i < 20) legit in
    List.map (fun (path, cnt, referrer) ->
      let ref_text = match referrer with Some r -> r | None -> "-" in
      El.v "tr" ~at:[] [
        El.v "td" ~at:[At.class' "path-cell"; At.v "title" path] [El.txt path];
        El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
        El.v "td" ~at:[At.class' "path-cell"; At.v "title" ref_text]
          [El.txt (if String.length ref_text > 60
                   then String.sub ref_text 0 60 ^ "..."
                   else ref_text)];
      ]
    ) top
  in
  let attack_rows =
    let top = List.filteri (fun i _ -> i < 10) attacks in
    List.map (fun (path, cnt, _) ->
      El.v "tr" ~at:[] [
        El.v "td" ~at:[At.class' "path-cell"; At.v "title" path] [El.txt path];
        El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
      ]
    ) top
  in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Errors (Last 7 Days)"];
    (* Legitimate 404s *)
    El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"]
      [El.txt "Legitimate 404s"];
    (if legit_rows = [] then
       El.p ~at:[At.class' "text-sm opacity-50"] [El.txt "None"]
     else
       El.div ~at:[At.class' "chart-container"] [
         El.table ~at:[At.class' "stats-table"] [
           El.v "thead" ~at:[] [
             El.v "tr" ~at:[] [
               El.v "th" ~at:[] [El.txt "Path"];
               El.v "th" ~at:[At.class' "num"] [El.txt "Count"];
               El.v "th" ~at:[] [El.txt "Referrer"];
             ]
           ];
           El.v "tbody" ~at:[] legit_rows
         ]
       ]);
    (* Attack probes *)
    El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"; At.style "margin-top: 1rem"]
      [El.txt (Printf.sprintf "Attack Probes (%s total)" (format_number attack_total))];
    (if attack_rows = [] then
       El.p ~at:[At.class' "text-sm opacity-50"] [El.txt "None"]
     else
       El.div ~at:[At.class' "chart-container"] [
         El.table ~at:[At.class' "stats-table"] [
           El.v "thead" ~at:[] [
             El.v "tr" ~at:[] [
               El.v "th" ~at:[] [El.txt "Path"];
               El.v "th" ~at:[At.class' "num"] [El.txt "Count"];
             ]
           ];
           El.v "tbody" ~at:[] attack_rows
         ]
       ]);
  ]

let live_activity_section db =
  let recent = recent_requests db in
  let rows = List.map (fun (ts, meth, path, status, dur, cache, size) ->
    let badge_cls = status_badge_class status in
    El.v "tr" ~at:[] [
      El.v "td" ~at:[] [El.txt ts];
      El.v "td" ~at:[] [El.strong [El.txt meth]];
      El.v "td" ~at:[At.class' "path-cell"; At.v "title" path] [El.txt path];
      El.v "td" ~at:[] [
        El.span ~at:[At.class' ("status-badge " ^ badge_cls)]
          [El.txt (string_of_int status)]];
      El.v "td" ~at:[At.class' "num"] [El.txt (human_duration_us dur)];
      El.v "td" ~at:[] [El.txt (if cache = "" then "-" else cache)];
      El.v "td" ~at:[At.class' "num"] [El.txt (human_bytes (float_of_int size))];
    ]
  ) recent in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [
      El.span ~at:[At.class' "live-dot"; At.style "margin-right: 0.5rem"] [];
      El.txt "Live Activity"
    ];
    El.div ~at:[At.class' "chart-container"; At.style "overflow-x: auto"] [
      El.table ~at:[At.class' "stats-table"] [
        El.v "thead" ~at:[] [
          El.v "tr" ~at:[] [
            El.v "th" ~at:[] [El.txt "Time"];
            El.v "th" ~at:[] [El.txt "Method"];
            El.v "th" ~at:[] [El.txt "Path"];
            El.v "th" ~at:[] [El.txt "Status"];
            El.v "th" ~at:[At.class' "num"] [El.txt "Latency"];
            El.v "th" ~at:[] [El.txt "Cache"];
            El.v "th" ~at:[At.class' "num"] [El.txt "Size"];
          ]
        ];
        El.v "tbody" ~at:[At.id "live-tbody"] rows
      ]
    ]
  ]

let category_color = function
  | "Browsers" -> "#3b82f6"
  | "Feed readers" -> "#22c55e"
  | "AI crawlers" -> "#f59e0b"
  | "SEO crawlers" -> "#ef4444"
  | "Search engines" -> "#8b5cf6"
  | "Link previews" -> "#06b6d4"
  | "Security scanners" -> "#dc2626"
  | "Programmatic" -> "#6b7280"
  | _ -> "#9ca3af"

let traffic_classification_section db range =
  let data = traffic_classification db range in
  let total = List.fold_left (fun acc (_, n, _) -> acc + n) 0 data in
  let bars = List.map (fun (cat, cnt, _) ->
    (cat, cnt, category_color cat)
  ) data in
  let rows = List.map (fun (cat, cnt, bw) ->
    let pct = if total > 0 then float_of_int cnt /. float_of_int total *. 100.0 else 0.0 in
    let color = category_color cat in
    El.v "tr" ~at:[] [
      El.v "td" ~at:[] [
        El.span ~at:[At.style (Printf.sprintf
          "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:0.5rem" color)] [];
        El.txt cat;
      ];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
      El.v "td" ~at:[At.class' "num"] [El.txt (Printf.sprintf "%.1f%%" pct)];
      El.v "td" ~at:[At.class' "num"] [El.txt (human_bytes bw)];
    ]
  ) data in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Traffic Classification"];
    El.div ~at:[At.class' "chart-container"] [
      svg_horizontal_bars ~width:500 ~height:(max 80 (List.length bars * 40)) ~bars ()
    ];
    El.div ~at:[At.class' "chart-container"; At.style "margin-top: 1rem"] [
      El.table ~at:[At.class' "stats-table"] [
        El.v "thead" ~at:[] [
          El.v "tr" ~at:[] [
            El.v "th" ~at:[] [El.txt "Category"];
            El.v "th" ~at:[At.class' "num"] [El.txt "Requests"];
            El.v "th" ~at:[At.class' "num"] [El.txt "%"];
            El.v "th" ~at:[At.class' "num"] [El.txt "Bandwidth"];
          ]
        ];
        El.v "tbody" ~at:[] rows
      ]
    ]
  ]

let cache_section db range =
  let breakdown = cache_breakdown db range in
  let rate_data = cache_rate_over_time db range in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 breakdown in
  let breakdown_items = List.map (fun (status, cnt) ->
    let pct = if total > 0 then float_of_int cnt /. float_of_int total *. 100.0 else 0.0 in
    El.div ~at:[At.class' "flex justify-between items-center py-1"] [
      El.span ~at:[At.class' "font-medium"] [El.txt status];
      El.span ~at:[At.class' "text-sm opacity-70 tabular-nums"]
        [El.txt (Printf.sprintf "%s (%.1f%%)" (format_number cnt) pct)];
    ]
  ) breakdown in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Cache Performance"];
    El.div ~at:[At.class' "stats-two-col"] [
      El.div ~at:[At.class' "chart-container"] [
        El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"] [El.txt "Hit/Miss Breakdown"];
        El.div ~at:[] breakdown_items;
      ];
      El.div ~at:[At.class' "chart-container"] [
        El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"]
          [El.txt (Printf.sprintf "Hit Rate Over Time (%s)" (range_label range))];
        svg_cache_rate_chart ~width:400 ~height:150 ~data:rate_data ();
      ];
    ]
  ]

let feed_section db range =
  let overview = feed_overview db range in
  let readers = feed_top_readers db range in
  let total_subs = List.fold_left (fun acc (ua, _) ->
    match parse_subscriber_count ua with
    | Some n -> acc + n
    | None -> acc
  ) 0 readers in
  let overview_rows = List.map (fun (path, cnt, uas) ->
    El.v "tr" ~at:[] [
      El.v "td" ~at:[At.class' "path-cell"] [El.txt path];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number uas)];
    ]
  ) overview in
  let reader_rows = List.map (fun (ua, cnt) ->
    let subs = match parse_subscriber_count ua with
      | Some n -> format_number n
      | None -> "-"
    in
    El.v "tr" ~at:[] [
      El.v "td" ~at:[At.class' "path-cell"; At.v "title" ua]
        [El.txt (simplify_feed_reader ua)];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
      El.v "td" ~at:[At.class' "num"] [El.txt subs];
    ]
  ) readers in
  let header_text =
    if total_subs > 0 then
      Printf.sprintf "Feed Subscribers (~%s est.)" (format_number total_subs)
    else
      "Feed Subscribers"
  in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt header_text];
    El.div ~at:[At.class' "stats-two-col"] [
      El.div ~at:[At.class' "chart-container"] [
        El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"] [El.txt "Feed Endpoints"];
        El.table ~at:[At.class' "stats-table"] [
          El.v "thead" ~at:[] [
            El.v "tr" ~at:[] [
              El.v "th" ~at:[] [El.txt "Feed"];
              El.v "th" ~at:[At.class' "num"] [El.txt "Polls"];
              El.v "th" ~at:[At.class' "num"] [El.txt "Unique UAs"];
            ]
          ];
          El.v "tbody" ~at:[] overview_rows
        ]
      ];
      El.div ~at:[At.class' "chart-container"] [
        El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"] [El.txt "Top Readers"];
        El.table ~at:[At.class' "stats-table"] [
          El.v "thead" ~at:[] [
            El.v "tr" ~at:[] [
              El.v "th" ~at:[] [El.txt "Reader"];
              El.v "th" ~at:[At.class' "num"] [El.txt "Polls"];
              El.v "th" ~at:[At.class' "num"] [El.txt "Subs"];
            ]
          ];
          El.v "tbody" ~at:[] reader_rows
        ]
      ];
    ]
  ]

let slug_of_path path =
  match String.split_on_char '/' path with
  | "" :: _ :: slug :: _ -> slug
  | _ -> path

let content_table ~title data =
  let rows = List.map (fun (path, cnt, unique_refs, bw, avg_us) ->
    El.v "tr" ~at:[] [
      El.v "td" ~at:[At.class' "path-cell"; At.v "title" path]
        [El.txt (slug_of_path path)];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number cnt)];
      El.v "td" ~at:[At.class' "num"] [El.txt (format_number unique_refs)];
      El.v "td" ~at:[At.class' "num"] [El.txt (human_bytes bw)];
      El.v "td" ~at:[At.class' "num"] [El.txt (human_duration_us (Float.to_int avg_us))];
    ]
  ) data in
  El.div ~at:[At.class' "chart-container"] [
    El.h3 ~at:[At.class' "text-sm font-semibold mb-2 opacity-70"] [El.txt title];
    El.table ~at:[At.class' "stats-table"] [
      El.v "thead" ~at:[] [
        El.v "tr" ~at:[] [
          El.v "th" ~at:[] [El.txt "Slug"];
          El.v "th" ~at:[At.class' "num"] [El.txt "Hits"];
          El.v "th" ~at:[At.class' "num"] [El.txt "Referrers"];
          El.v "th" ~at:[At.class' "num"] [El.txt "Bandwidth"];
          El.v "th" ~at:[At.class' "num"] [El.txt "Avg Latency"];
        ]
      ];
      El.v "tbody" ~at:[] rows
    ]
  ]

let popular_content_section db range =
  let notes = top_content_by_type db range "notes" 15 in
  let papers = top_content_by_type db range "papers" 10 in
  El.div ~at:[At.class' "stats-section"] [
    El.h2 ~at:[] [El.txt "Popular Content"];
    content_table ~title:"Top Notes" notes;
    El.div ~at:[At.style "margin-top:1rem"] [
      content_table ~title:"Top Papers" papers;
    ];
  ]

(** {1 Full Dashboard Page} *)

let render_dashboard db range =
  let content = El.div ~at:[At.style "max-width: 1000px; margin: 0 auto; padding: 1rem 1rem 3rem"] [
    El.style [El.unsafe_raw dashboard_css];
    El.h1 ~at:[At.class' "text-2xl font-bold mb-1"] [El.txt "Arod Analytics"];
    El.p ~at:[At.class' "text-sm opacity-50 mb-4"] [
      El.txt (Printf.sprintf "Showing: %s" (range_label range))];
    time_tabs range;
    overview_comparison db range;
    traffic_classification_section db range;
    traffic_section db range;
    feed_section db range;
    popular_content_section db range;
    El.div ~at:[At.class' "stats-two-col"] [
      status_section db range;
      content_type_section db range;
    ];
    latency_section db range;
    referrer_section db range;
    cache_section db range;
    errors_section db;
    live_activity_section db;
    El.script [El.unsafe_raw dashboard_js];
  ] in
  (* Minimal standalone page -- no nav, no footer, no sitemap link *)
  let head_el = El.head [
    El.meta ~at:[At.charset "utf-8"] ();
    El.meta ~at:[At.name "viewport"; At.content "width=device-width, initial-scale=1.0"] ();
    El.meta ~at:[At.name "robots"; At.content "noindex, nofollow"] ();
    El.title [El.txt "Arod Analytics"];
    (* Tailwind for utility classes *)
    El.script ~at:[At.src "https://cdn.tailwindcss.com"] [];
    El.script [El.unsafe_raw {|
      tailwind.config = {
        darkMode: 'media',
        theme: {
          extend: {
            colors: {
              bg: 'var(--color-bg, #fffffc)',
              text: 'var(--color-text, #1a1a1a)',
              surface: 'var(--color-surface, #f8f8f6)',
              border: 'var(--color-border, #e5e5e0)',
            }
          }
        }
      }
    |}];
    El.style [El.unsafe_raw {|
      :root {
        --color-bg: #fffffc;
        --color-text: #1a1a1a;
        --color-surface: #f8f8f6;
        --color-surface-alt: #eeeeea;
        --color-border: #e5e5e0;
      }
      @media (prefers-color-scheme: dark) {
        :root {
          --color-bg: #1a1a1f;
          --color-text: #e5e5e0;
          --color-surface: #252529;
          --color-surface-alt: #2d2d33;
          --color-border: #3a3a42;
        }
      }
      body {
        font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
                     "Helvetica Neue", Arial, sans-serif;
        background: var(--color-bg);
        color: var(--color-text);
      }
    |}];
  ] in
  let body_el = El.body ~at:[At.class' "bg-bg text-text"] [content] in
  El.to_string ~doctype:true (El.html ~at:[At.lang "en"] [head_el; body_el])

(** {1 JSON API Responses} *)

let overview_json db range =
  let total = total_requests db range in
  let avg_lat = avg_response_time db range in
  let cache_rate = cache_hit_rate db range in
  let err_rate = error_rate db range in
  let bw = total_bandwidth db range in
  Ezjsonm.to_string (`O [
    ("total", `String (format_number total));
    ("avg_latency", `String (human_duration_us (Float.to_int avg_lat)));
    ("cache_rate", `String (Printf.sprintf "%.1f%%" cache_rate));
    ("error_rate", `String (Printf.sprintf "%.1f%%" err_rate));
    ("bandwidth", `String (human_bytes bw));
  ])

let traffic_json db range =
  let data = traffic_over_time db range in
  let items = List.map (fun (bucket, cnt) ->
    `O [("bucket", `String bucket); ("count", `Float (float_of_int cnt))]
  ) data in
  Ezjsonm.to_string (`O [("traffic", `A items)])

let recent_json db =
  let recent = recent_requests db in
  let items = List.map (fun (ts, meth, path, status, dur, cache, size) ->
    `O [
      ("timestamp", `String ts);
      ("method", `String meth);
      ("path", `String path);
      ("status", `Float (float_of_int status));
      ("duration", `String (human_duration_us dur));
      ("cache", `String (if cache = "" then "" else cache));
      ("size", `String (human_bytes (float_of_int size)));
    ]
  ) recent in
  Ezjsonm.to_string (`O [("requests", `A items)])
