(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Live terminal dashboard of a running server, drawn with console.

   The dashboard runs its own display rather than [Console_eio.run], whose
   display redraws ten times a second whatever happens. Here a frame is drawn
   when a request, a connection or an error arrives, bursts coalesced into at
   most ten frames a second, and once a second for a minute after the last
   request so that the rate windows settle to zero. An idle server then
   draws nothing and computes nothing. *)

open Console

(* {1 Counters} *)

type sample = {
  at : float; (* Unix time of the response. *)
  meth : string;
  path : string;
  status : int;
  duration_us : int;
  body_size : int;
  cache : string option;
  remote : string;
  agent : string; (* The user agent family. *)
}

type path_stat = {
  mutable count : int;
  mutable total_us : int;
  mutable out : int;
  mutable failures : int;
}

(* One counter per time key, in a ring. A slot whose key is stale is zeroed
   before it is reused, so a quiet period reads as zero without a sweep. *)
type ring = {
  keys : int array;
  counts : int array;
  bytes : int array;
  us : int array;
}

type history = {
  fetched : float; (* Unix time of the query. *)
  day : int;
  hour : int;
  day_bytes : float;
  day_avg_us : float;
  day_status : (string * int) list;
  day_paths : (string * int) list;
}

type t = {
  now : unit -> float; (* Monotonic seconds. *)
  wall : unit -> float;
  site : string;
  listen : string;
  content : (string * int) list;
  search_entries : unit -> int option;
  cache_stats : unit -> int * int;
  max_connections : int;
  started : float;
  started_wall : float;
  mutable total : int;
  mutable out : int;
  classes : int array; (* 1xx to 5xx, then other. *)
  by_status : (int, int ref) Hashtbl.t;
  by_path : (string, path_stat) Hashtbl.t;
  by_method : (string, int ref) Hashtbl.t;
  by_type : (string, int ref) Hashtbl.t;
  by_agent : (string, int ref) Hashtbl.t;
  visitors : (string, float) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
  recent : sample option array;
  mutable recent_head : int;
  mutable recent_len : int;
  seconds : ring;
  minutes : ring;
  buckets : int array; (* Latency buckets, [bucket_edges] wide. *)
  mutable slowest : sample list;
  mutable connections : int;
  mutable peak_connections : int;
  mutable accepted : int;
  mutable cpu : float; (* CPU seconds per second over the last sample. *)
  mutable cpu_sample : float * float; (* Monotonic time and CPU seconds. *)
  mutable alloc_rate : float; (* Bytes per second over the last sample. *)
  mutable alloc_sample : float * float; (* Monotonic time and minor words. *)
  mutable gc : Gc.stat;
  mutable history : history option;
  mutable history_error : string option;
  mutable history_fetching : bool;
  mutable errors : (float * string) list; (* Newest first, at most five. *)
  mutable last_request : float; (* Monotonic, [neg_infinity] before one. *)
  mutable last_request_wall : float option;
  changed : Eio.Condition.t;
  mutable pending : bool; (* A change waits to be drawn. *)
}

let recent_capacity = 512

(* A crawler walking random URLs or a flood of addresses would otherwise
   grow these tables without bound. Past the cap, new keys are not added,
   while the totals still count every request. *)
let paths_kept = 20_000
let visitors_kept = 100_000
let seconds_kept = 120
let minutes_kept = 180
let bucket_edges = [| 1_000; 10_000; 100_000; 1_000_000 |]
let bucket_labels = [| "<1ms"; "<10ms"; "<100ms"; "<1s"; ">1s" |]

let ring n =
  {
    keys = Array.make n (-1);
    counts = Array.make n 0;
    bytes = Array.make n 0;
    us = Array.make n 0;
  }

let ring_add r key ~bytes ~us =
  let n = Array.length r.keys in
  let slot = key mod n in
  if r.keys.(slot) <> key then begin
    r.keys.(slot) <- key;
    r.counts.(slot) <- 0;
    r.bytes.(slot) <- 0;
    r.us.(slot) <- 0
  end;
  r.counts.(slot) <- r.counts.(slot) + 1;
  r.bytes.(slot) <- r.bytes.(slot) + bytes;
  r.us.(slot) <- r.us.(slot) + us

(* [ring_sum r ~until ~width f] sums [f r slot] over the [width] keys ending
   at [until], the stale slots counting as zero. *)
let ring_sum r ~until ~width f =
  let n = Array.length r.keys in
  let acc = ref 0 in
  for key = until - width + 1 to until do
    if key >= 0 then begin
      let slot = key mod n in
      if r.keys.(slot) = key then acc := !acc + f r slot
    end
  done;
  !acc

let ring_series r ~until ~width =
  let n = Array.length r.keys in
  List.init width (fun i ->
      let key = until - width + 1 + i in
      if key < 0 then 0.
      else
        let slot = key mod n in
        if r.keys.(slot) = key then float_of_int r.counts.(slot) else 0.)

let mono_seconds clock =
  Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) /. 1e9

let cpu_seconds () =
  let t = Unix.times () in
  t.Unix.tms_utime +. t.Unix.tms_stime

let create ~clock ~wall ~site ~listen ~content ~search_entries ~cache_stats
    ~max_connections () =
  let now () = mono_seconds clock in
  let started = now () in
  {
    now;
    wall;
    site;
    listen;
    content;
    search_entries;
    cache_stats;
    max_connections;
    started;
    started_wall = wall ();
    total = 0;
    out = 0;
    classes = Array.make 6 0;
    by_status = Hashtbl.create 16;
    by_path = Hashtbl.create 256;
    by_method = Hashtbl.create 8;
    by_type = Hashtbl.create 16;
    by_agent = Hashtbl.create 8;
    visitors = Hashtbl.create 256;
    hits = 0;
    misses = 0;
    recent = Array.make recent_capacity None;
    recent_head = 0;
    recent_len = 0;
    seconds = ring seconds_kept;
    minutes = ring minutes_kept;
    buckets = Array.make (Array.length bucket_edges + 1) 0;
    slowest = [];
    connections = 0;
    peak_connections = 0;
    accepted = 0;
    cpu = 0.;
    cpu_sample = (started, cpu_seconds ());
    alloc_rate = 0.;
    alloc_sample = (started, (Gc.quick_stat ()).Gc.minor_words);
    gc = Gc.quick_stat ();
    history = None;
    history_error = None;
    history_fetching = false;
    errors = [];
    last_request = neg_infinity;
    last_request_wall = None;
    changed = Eio.Condition.create ();
    pending = false;
  }

(* A change wakes the redraw loop. [pending] keeps it when no fiber waits. *)
let wake t =
  t.pending <- true;
  Eio.Condition.broadcast t.changed

(* {1 Recording} *)

let header headers name =
  List.find_map
    (fun (k, v) -> if String.lowercase_ascii k = name then Some v else None)
    headers

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

let agent_family = function
  | None -> "none"
  | Some ua ->
    let ua_l = String.lowercase_ascii ua in
    if contains ua_l "bot" || contains ua_l "crawl" || contains ua_l "spider"
       || contains ua_l "slurp" || contains ua_l "archive"
    then "bots"
    else if contains ua_l "feed" || contains ua_l "rss"
            || contains ua_l "reader" || contains ua_l "inoreader"
            || contains ua_l "newsblur"
    then "feeds"
    else if contains ua_l "curl" || contains ua_l "wget"
            || contains ua_l "python" || contains ua_l "go-http"
            || contains ua_l "okhttp" || contains ua_l "libwww"
            || contains ua_l "java/"
    then "scripts"
    else if contains ua_l "mastodon" || contains ua_l "activitypub"
            || contains ua_l "pleroma" || contains ua_l "misskey"
    then "fediverse"
    else if contains ua_l "mozilla" || contains ua_l "safari"
    then "browsers"
    else "other"

(* The visitor is the first forwarded address when a proxy adds one, and
   otherwise the peer without its port. *)
let visitor headers remote =
  match header headers "x-forwarded-for" with
  | Some xff -> (
    match String.split_on_char ',' xff with
    | first :: _ -> String.trim first
    | [] -> xff)
  | None -> (
    match String.rindex_opt remote ':' with
    | Some i -> String.sub remote 0 i
    | None -> remote)

let class_index status =
  if status >= 100 && status < 600 then (status / 100) - 1 else 5

let bucket_index us =
  let rec go i =
    if i >= Array.length bucket_edges then i
    else if us < bucket_edges.(i) then i
    else go (i + 1)
  in
  go 0

let bump tbl key =
  match Hashtbl.find_opt tbl key with
  | Some r -> incr r
  | None -> Hashtbl.replace tbl key (ref 1)

let major_type = function
  | None -> "none"
  | Some ct -> (
    match String.index_opt ct ';' with
    | Some i -> String.trim (String.sub ct 0 i)
    | None -> ct)

let slowest_kept = 8

let insert_slowest s l =
  let rec go n = function
    | [] -> if n < slowest_kept then [ s ] else []
    | x :: rest when s.duration_us > x.duration_us ->
      s :: List.filteri (fun i _ -> n + 1 + i < slowest_kept) (x :: rest)
    | x :: rest -> x :: go (n + 1) rest
  in
  go 0 l

let record t (event : Proffer_httpz.event) =
  let status = Proffer.Status.code event.status in
  let agent = agent_family (header event.request_headers "user-agent") in
  let remote = visitor event.request_headers event.remote_addr in
  let s =
    {
      at = t.wall ();
      meth = Proffer.Method.to_string event.meth;
      path = event.path;
      status;
      duration_us = event.duration_us;
      body_size = event.body_size;
      cache = event.cache_status;
      remote;
      agent;
    }
  in
  t.total <- t.total + 1;
  t.out <- t.out + s.body_size;
  let ci = class_index status in
  t.classes.(ci) <- t.classes.(ci) + 1;
  bump t.by_status status;
  bump t.by_method s.meth;
  bump t.by_type (major_type event.response_content_type);
  bump t.by_agent agent;
  if Hashtbl.mem t.visitors remote || Hashtbl.length t.visitors < visitors_kept
  then Hashtbl.replace t.visitors remote s.at;
  (match s.cache with
   | Some v when String.lowercase_ascii v = "hit" -> t.hits <- t.hits + 1
   | Some _ -> t.misses <- t.misses + 1
   | None -> ());
  (match Hashtbl.find_opt t.by_path s.path with
   | Some p ->
     p.count <- p.count + 1;
     p.total_us <- p.total_us + s.duration_us;
     p.out <- p.out + s.body_size;
     if status >= 400 then p.failures <- p.failures + 1
   | None when Hashtbl.length t.by_path < paths_kept ->
     Hashtbl.replace t.by_path s.path
       {
         count = 1;
         total_us = s.duration_us;
         out = s.body_size;
         failures = (if status >= 400 then 1 else 0);
       }
   | None -> ());
  t.recent.(t.recent_head) <- Some s;
  t.recent_head <- (t.recent_head + 1) mod recent_capacity;
  t.recent_len <- min recent_capacity (t.recent_len + 1);
  let elapsed = t.now () -. t.started in
  let key = int_of_float elapsed in
  ring_add t.seconds key ~bytes:s.body_size ~us:s.duration_us;
  ring_add t.minutes (key / 60) ~bytes:s.body_size ~us:s.duration_us;
  let bi = bucket_index s.duration_us in
  t.buckets.(bi) <- t.buckets.(bi) + 1;
  t.slowest <- insert_slowest s t.slowest;
  t.last_request <- t.now ();
  t.last_request_wall <- Some s.at;
  wake t

let connection t delta =
  t.connections <- max 0 (t.connections + delta);
  if delta > 0 then begin
    t.accepted <- t.accepted + 1;
    t.peak_connections <- max t.peak_connections t.connections
  end;
  wake t

let error t exn =
  let line = (t.wall (), Printexc.to_string exn) in
  t.errors <- line :: List.filteri (fun i _ -> i < 4) t.errors;
  wake t

(* {1 Sampling} *)

let sample t =
  let now = t.now () in
  let cpu = cpu_seconds () in
  let at, last = t.cpu_sample in
  if now > at then t.cpu <- (cpu -. last) /. (now -. at);
  t.cpu_sample <- (now, cpu);
  let gc = Gc.quick_stat () in
  let at, words = t.alloc_sample in
  if now > at then
    t.alloc_rate <-
      (gc.Gc.minor_words -. words) *. float_of_int (Sys.word_size / 8)
      /. (now -. at);
  t.alloc_sample <- (now, gc.Gc.minor_words);
  t.gc <- gc

let refresh_history t db =
  try
    let range = Arod_stats.Last_hours 24 in
    let day = Arod_stats.total_requests db range in
    let hour = Arod_stats.total_requests db (Arod_stats.Last_hours 1) in
    let day_bytes = Arod_stats.bandwidth db range in
    let day_avg_us, _, _ = Arod_stats.latency_overview db range in
    let day_status = Arod_stats.status_code_breakdown db range in
    let day_paths = Arod_stats.top_paths db range 6 in
    t.history <-
      Some
        {
          fetched = t.wall ();
          day;
          hour;
          day_bytes;
          day_avg_us;
          day_status;
          day_paths;
        };
    t.history_error <- None
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> t.history_error <- Some (Printexc.to_string exn)

(* The access log is summarised again at most once a minute, and only when
   a frame is drawn, in a fiber of its own since the query runs in a system
   thread. Its completion draws a frame. *)
let history_ttl = 60.

let maybe_refresh_history ~sw t = function
  | None -> ()
  | Some db ->
    let stale =
      match t.history with
      | None -> true
      | Some h -> t.wall () -. h.fetched >= history_ttl
    in
    if stale && not t.history_fetching then begin
      t.history_fetching <- true;
      Eio.Fiber.fork ~sw (fun () ->
          Fun.protect
            ~finally:(fun () -> t.history_fetching <- false)
            (fun () -> refresh_history t db);
          wake t)
    end

(* {1 Figures} *)

let with_commas n =
  let s = string_of_int (abs n) in
  let len = String.length s in
  let b = Buffer.create (len + (len / 3)) in
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ',';
      Buffer.add_char b c)
    s;
  (if n < 0 then "-" else "") ^ Buffer.contents b

let bytes_figure v =
  if v >= 1e12 then Fmt.str "%.1fT" (v /. 1e12)
  else if v >= 1e9 then Fmt.str "%.2fG" (v /. 1e9)
  else if v >= 1e6 then Fmt.str "%.1fM" (v /. 1e6)
  else if v >= 1e3 then Fmt.str "%.0fK" (v /. 1e3)
  else Fmt.str "%.0fB" v

let latency_figure us =
  if us >= 1_000_000 then Fmt.str "%.2fs" (float_of_int us /. 1e6)
  else if us >= 10_000 then Fmt.str "%.0fms" (float_of_int us /. 1e3)
  else if us >= 1_000 then Fmt.str "%.1fms" (float_of_int us /. 1e3)
  else Fmt.str "%dus" us

let rate_figure r =
  if r >= 100. then Fmt.str "%.0f" r
  else if r >= 10. then Fmt.str "%.1f" r
  else Fmt.str "%.2f" r

let duration_figure secs =
  let s = int_of_float secs in
  let d = s / 86400 and h = s / 3600 mod 24 and m = s / 60 mod 60 in
  let sec = s mod 60 in
  if d > 0 then Fmt.str "%dd%02dh%02dm" d h m
  else if h > 0 then Fmt.str "%dh%02dm%02ds" h m sec
  else if m > 0 then Fmt.str "%dm%02ds" m sec
  else Fmt.str "%ds" sec

let clock_figure wall =
  let tm = Unix.localtime wall in
  Fmt.str "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let percent num den =
  if den = 0 then "-"
  else Fmt.str "%.0f%%" (100. *. float_of_int num /. float_of_int den)

(* {1 Derived figures} *)

let recent_list t =
  List.init t.recent_len (fun i ->
      let idx = (t.recent_head - 1 - i + recent_capacity) mod recent_capacity in
      match t.recent.(idx) with Some s -> s | None -> assert false)

let percentiles t =
  let n = t.recent_len in
  if n = 0 then (0, 0, 0, 0)
  else begin
    let arr = Array.make n 0 in
    for i = 0 to n - 1 do
      match t.recent.(i) with
      | Some s -> arr.(i) <- s.duration_us
      | None -> ()
    done;
    Array.sort compare arr;
    let pct p = arr.(min (n - 1) (int_of_float (float_of_int n *. p))) in
    (pct 0.5, pct 0.9, pct 0.99, arr.(n - 1))
  end

let elapsed_key t = int_of_float (t.now () -. t.started)

let window t width =
  let until = elapsed_key t - 1 in
  let n = ring_sum t.seconds ~until ~width (fun r s -> r.counts.(s)) in
  let b = ring_sum t.seconds ~until ~width (fun r s -> r.bytes.(s)) in
  let us = ring_sum t.seconds ~until ~width (fun r s -> r.us.(s)) in
  (n, b, us)

let top_paths t n =
  Hashtbl.fold (fun path p acc -> (path, p) :: acc) t.by_path []
  |> List.sort (fun (_, a) (_, b) -> compare b.count a.count)
  |> List.filteri (fun i _ -> i < n)

let sorted_counts tbl =
  Hashtbl.fold (fun k r acc -> (k, !r) :: acc) tbl []
  |> List.sort (fun (_, a) (_, b) -> compare b a)

(* {1 Views} *)

let txt = View.text
let muted s = View.role `Muted (txt s)
let bold s = View.styled Style.bold (txt s)
let strong v = View.styled Style.bold v

let status_role status =
  if status < 300 then `Succeeded
  else if status < 400 then `Accent
  else if status < 500 then `Warning
  else `Failed

let latency_role us =
  if us < 10_000 then `Succeeded
  else if us < 100_000 then `Accent
  else if us < 1_000_000 then `Warning
  else `Failed

let class_role = function
  | 1 -> `Succeeded
  | 2 -> `Accent
  | 3 -> `Warning
  | 4 -> `Failed
  | _ -> `Muted

let role_colour theme = function
  | `Succeeded -> Theme.succeeded theme
  | `Warning -> Theme.warning theme
  | `Failed -> Theme.failed theme
  | `Accent -> Theme.accent theme
  | `Muted -> (
    match Theme.muted theme with Some c -> c | None -> Theme.accent theme)

(* A panel of lines of a muted label and its figures, the figures against
   the right edge. *)
let facts ?(gap = 1) title rows =
  let n = List.fold_left (fun n (_, c) -> max n (List.length c)) 0 rows in
  let line (label, cells) =
    let pad = List.init (n - List.length cells) (fun _ -> View.empty) in
    (muted label :: pad) @ cells
  in
  Panel.view ~title:(txt title)
    (Table.view ~border:Border.none ~gap
       (Table.column View.empty
        :: List.init n (fun _ -> Table.column ~align:`Right View.empty))
       (List.map line rows))

(* The headline reads nothing that moves on its own, so that an idle
   server shows the same frame until a request arrives. *)
let headline t =
  let open View in
  let hits, misses = t.cache_stats () in
  let search =
    match t.search_entries () with
    | Some n -> role `Succeeded (txt (Fmt.str "index %s" (with_commas n)))
    | None -> role `Warning (txt "indexing")
  in
  let last =
    match t.last_request_wall with
    | Some at -> role `Accent (txt (clock_figure at))
    | None -> muted "none yet"
  in
  hcat ~gap:3
    [
      strong (gradient (txt "AROD"));
      strong (txt t.site);
      role `Accent (txt t.listen);
      hcat ~gap:1 [ muted "started"; txt (clock_figure t.started_wall) ];
      hcat ~gap:1 [ muted "last"; last ];
      hcat ~gap:1
        [
          muted "conns";
          txt (Fmt.str "%d/%d" t.connections t.max_connections);
        ];
      hcat ~gap:1 [ muted "cache"; txt (percent hits (hits + misses)) ];
      search;
      hcat ~gap:1 [ muted "ocaml"; txt Sys.ocaml_version ];
    ]

let content_line t =
  let open View in
  let parts =
    List.map
      (fun (kind, n) -> hcat ~gap:1 [ txt (with_commas n); muted kind ])
      t.content
  in
  hcat ~gap:1 (List.concat_map (fun p -> [ p; muted "·" ]) parts |> fun l ->
      List.filteri (fun i _ -> i < List.length l - 1) l)

let traffic t =
  let n1, _, _ = window t 1 in
  let n10, b10, us10 = window t 10 in
  let n60, b60, us60 = window t 60 in
  let series = ring_series t.seconds ~until:(elapsed_key t) ~width:60 in
  let avg10 = if n10 = 0 then 0 else us10 / n10 in
  let avg60 = if n60 = 0 then 0 else us60 / n60 in
  let rate n w = txt (rate_figure (float_of_int n /. float_of_int w)) in
  let open View in
  Panel.view ~title:(txt "Traffic")
    (vcat
       [
         Table.view ~border:Border.none ~gap:1
           [
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
           ]
           [
             [ muted "req/s 1s"; strong (rate n1 1); muted "10s"; rate n10 10 ];
             [
               muted "60s";
               rate n60 60;
               muted "peak";
               txt (Fmt.str "%.0f/s" (List.fold_left max 0. series));
             ];
             [
               muted "out/s 10s";
               txt (bytes_figure (float_of_int b10 /. 10.));
               muted "60s";
               txt (bytes_figure (float_of_int b60 /. 60.));
             ];
             [
               muted "avg 10s";
               role (latency_role avg10) (txt (latency_figure avg10));
               muted "60s";
               role (latency_role avg60) (txt (latency_figure avg60));
             ];
           ];
         Table.view ~border:Border.none ~gap:1
           [
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
           ]
           [
             [
               muted "total";
               strong (txt (with_commas t.total));
               muted "sent";
               txt (bytes_figure (float_of_int t.out));
             ];
             [
               muted "visitors";
               txt (with_commas (Hashtbl.length t.visitors));
               muted "accepted";
               txt (with_commas t.accepted);
             ];
             [
               muted "open";
               txt (string_of_int t.connections);
               muted "peak open";
               txt (string_of_int t.peak_connections);
             ];
           ];
       ])

(* Requests per second over the last minute and per minute over the last
   half hour, side by side. *)
let load_line t =
  let seconds = ring_series t.seconds ~until:(elapsed_key t) ~width:60 in
  let minutes =
    ring_series t.minutes ~until:(elapsed_key t / 60) ~width:30
  in
  let open View in
  hcat ~gap:3
    [
      hcat ~gap:1 [ muted "req/s 60s"; Bar.sparkline ~min:0. seconds ];
      hcat ~gap:1 [ muted "req/min 30m"; Bar.sparkline ~min:0. minutes ];
    ]

let latency t =
  let p50, p90, p99, mx = t |> percentiles in
  let figure us = View.role (latency_role us) (txt (latency_figure us)) in
  let chart =
    Bar.chart ~height:3
      (Array.to_list
         (Array.mapi
            (fun i n -> (bucket_labels.(i), float_of_int n))
            t.buckets))
  in
  let open View in
  Panel.view ~title:(txt "Latency")
    ~subtitle:(muted (Fmt.str "last %d" t.recent_len))
    (vcat
       [
         Table.view ~border:Border.none ~gap:1
           [
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
           ]
           [
             [ muted "p50"; figure p50; muted "p90"; figure p90 ];
             [ muted "p99"; figure p99; muted "max"; figure mx ];
           ];
         chart;
       ])

let statuses t =
  let total = max 1 t.total in
  let rows =
    List.filter_map
      (fun i ->
        let n = t.classes.(i) in
        if n = 0 && i <> 1 then None
        else
          let label = if i = 5 then "other" else Fmt.str "%dxx" (i + 1) in
          let role = class_role i in
          let frac = float_of_int n /. float_of_int total in
          Some
            [
              View.role role (txt label);
              txt (with_commas n);
              View.fit (fun room ->
                  Bar.horizontal ~width:10
                    ~style:(Style.fg (role_colour (Room.theme room) role))
                    frac);
              muted (percent n t.total);
            ])
      [ 0; 1; 2; 3; 4; 5 ]
  in
  let codes =
    Hashtbl.fold (fun c r acc -> (c, !r) :: acc) t.by_status []
    |> List.filter (fun (c, _) -> c >= 400)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.filteri (fun i _ -> i < 4)
  in
  let methods = sorted_counts t.by_method |> List.filteri (fun i _ -> i < 4) in
  let open View in
  let summary items =
    hcat ~gap:1
      (List.concat_map
         (fun (k, n) -> [ muted k; txt (with_commas n) ])
         items)
  in
  Panel.view ~title:(txt "Status")
    (vcat
       [
         Table.view ~border:Border.none ~gap:1
           [
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
           ]
           rows;
         hcat ~gap:1
           [
             muted "cache";
             role `Succeeded
               (txt (Fmt.str "%s hit" (percent t.hits (t.hits + t.misses))));
             muted (Fmt.str "%s miss" (with_commas t.misses));
           ];
         (if codes = [] then muted "no errors"
          else summary (List.map (fun (c, n) -> (string_of_int c, n)) codes));
         summary methods;
       ])

let process t =
  let gc = t.gc in
  let word = float_of_int (Sys.word_size / 8) in
  let heap = float_of_int gc.Gc.heap_words *. word in
  let top = float_of_int gc.Gc.top_heap_words *. word in
  let cpu_role =
    if t.cpu > 0.8 then `Failed
    else if t.cpu > 0.4 then `Warning
    else `Succeeded
  in
  facts "Process"
    [
      ("cpu", [ View.role cpu_role (txt (Fmt.str "%.0f%%" (100. *. t.cpu))) ]);
      ("heap", [ txt (bytes_figure heap) ]);
      ("peak heap", [ txt (bytes_figure top) ]);
      ("alloc/s", [ txt (bytes_figure t.alloc_rate) ]);
      ("minor gc", [ txt (with_commas gc.Gc.minor_collections) ]);
      ("major gc", [ txt (with_commas gc.Gc.major_collections) ]);
      ("pid", [ txt (string_of_int (Unix.getpid ())) ]);
    ]

let recent_rows t n =
  List.filteri (fun i _ -> i < n) (recent_list t)
  |> List.map (fun s ->
         let cache =
           match s.cache with
           | Some v when String.lowercase_ascii v = "hit" ->
             View.role `Succeeded (txt v)
           | Some v -> muted v
           | None -> muted "-"
         in
         [
           muted (clock_figure s.at);
           txt s.meth;
           View.role (status_role s.status) (txt (string_of_int s.status));
           View.role (latency_role s.duration_us)
             (txt (latency_figure s.duration_us));
           txt (bytes_figure (float_of_int s.body_size));
           cache;
           (if s.status >= 400 then
              View.role (status_role s.status) (txt s.path)
            else txt s.path);
           muted s.remote;
           muted s.agent;
         ])

let recent t n =
  let rows = recent_rows t n in
  let rows = if rows = [] then [ [ muted "waiting for requests" ] ] else rows in
  Panel.view ~title:(txt "Recent requests") ~expand:true
    (Table.view ~border:Border.none ~gap:1
       [
         Table.column ~width:8 ~shrinkable:false (txt "time");
         Table.column ~width:4 ~overflow:`Truncate (txt "meth");
         Table.column ~width:3 ~shrinkable:false (txt "st");
         Table.column ~width:7 ~align:`Right ~shrinkable:false (txt "took");
         Table.column ~width:6 ~align:`Right (txt "size");
         Table.column ~width:5 ~overflow:`Truncate (txt "cache");
         Table.column ~min_width:12 ~max_width:70 ~overflow:`Truncate
           (txt "path");
         Table.column ~width:15 ~overflow:`Truncate (txt "visitor");
         Table.column ~width:9 ~overflow:`Truncate (txt "agent");
       ]
       rows)

let paths t =
  let rows =
    List.map
      (fun (path, p) ->
        let avg = p.total_us / max 1 p.count in
        [
          txt path;
          txt (with_commas p.count);
          View.role (latency_role avg) (txt (latency_figure avg));
          txt (bytes_figure (float_of_int p.out));
          (if p.failures = 0 then muted "-"
           else View.role `Failed (txt (string_of_int p.failures)));
        ])
      (top_paths t 8)
  in
  let rows = if rows = [] then [ [ muted "none yet" ] ] else rows in
  Panel.view ~title:(txt "Top paths")
    (Table.view ~border:Border.none ~gap:1
       [
         Table.column ~max_width:32 ~overflow:`Truncate (txt "path");
         Table.column ~align:`Right (txt "hits");
         Table.column ~align:`Right (txt "avg");
         Table.column ~align:`Right (txt "sent");
         Table.column ~align:`Right (txt "err");
       ]
       rows)

let slowest t =
  let rows =
    List.map
      (fun s ->
        [
          View.role (latency_role s.duration_us)
            (txt (latency_figure s.duration_us));
          View.role (status_role s.status) (txt (string_of_int s.status));
          txt s.path;
          muted (clock_figure s.at);
        ])
      t.slowest
  in
  let rows = if rows = [] then [ [ muted "none yet" ] ] else rows in
  Panel.view ~title:(txt "Slowest")
    (Table.view ~border:Border.none ~gap:1
       [
         Table.column ~align:`Right (txt "time");
         Table.column (txt "st");
         Table.column ~max_width:28 ~overflow:`Truncate (txt "path");
         Table.column (txt "at");
       ]
       rows)

let agents t =
  let items = sorted_counts t.by_agent in
  let total = max 1 t.total in
  let rows =
    List.map
      (fun (family, n) ->
        [
          txt family;
          txt (with_commas n);
          Bar.horizontal ~width:8 (float_of_int n /. float_of_int total);
          muted (percent n t.total);
        ])
      items
  in
  let types = sorted_counts t.by_type |> List.filteri (fun i _ -> i < 5) in
  let rows = if rows = [] then [ [ muted "none yet" ] ] else rows in
  let open View in
  Panel.view ~title:(txt "Agents")
    (vcat
       [
         Table.view ~border:Border.none ~gap:1
           [
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
             Table.column View.empty;
             Table.column ~align:`Right View.empty;
           ]
           rows;
         (if types = [] then View.empty
          else
            vcat
              (muted "content types"
               :: List.map
                    (fun (ty, n) ->
                      hcat ~gap:1 [ txt (with_commas n); muted ty ])
                    types));
       ])

let history t =
  let body =
    match (t.history, t.history_error) with
    | None, None -> muted "reading access log"
    | None, Some e -> View.role `Failed (txt e)
    | Some h, _ ->
      let statuses =
        View.hcat ~gap:1
          (List.concat_map
             (fun (bucket, n) ->
               let role =
                 match bucket with
                 | "2xx" -> `Succeeded
                 | "3xx" -> `Accent
                 | "4xx" -> `Warning
                 | "5xx" -> `Failed
                 | _ -> `Muted
               in
               [ View.role role (txt bucket); txt (with_commas n) ])
             h.day_status)
      in
      let avg = int_of_float h.day_avg_us in
      View.vcat
        [
          Table.view ~border:Border.none ~gap:1
            [
              Table.column View.empty;
              Table.column ~align:`Right View.empty;
              Table.column View.empty;
              Table.column ~align:`Right View.empty;
            ]
            [
              [
                muted "24h";
                strong (txt (with_commas h.day));
                muted "1h";
                txt (with_commas h.hour);
              ];
              [
                muted "sent";
                txt (bytes_figure h.day_bytes);
                muted "avg";
                View.role (latency_role avg) (txt (latency_figure avg));
              ];
            ];
          statuses;
          Table.view ~border:Border.none ~gap:1
            [
              Table.column ~max_width:26 ~overflow:`Truncate View.empty;
              Table.column ~align:`Right View.empty;
            ]
            (List.map
               (fun (path, n) -> [ txt path; txt (with_commas n) ])
               h.day_paths);
          muted (Fmt.str "as of %s" (clock_figure h.fetched));
        ]
  in
  Panel.view ~title:(txt "Last 24h") body

let errors t =
  match t.errors with
  | [] -> View.empty
  | lines ->
    View.vcat
      (List.map
         (fun (at, msg) ->
           View.hcat ~gap:1
             [ muted (clock_figure at); View.role `Failed (txt msg) ])
         (List.filteri (fun i _ -> i < 3) lines))

(* The panels of a row, as many as fit from the left. *)
let rec fitting room = function
  | [] -> View.empty
  | views -> (
    let v = View.hcat ~gap:1 views in
    match Room.width room with
    | Some w when View.width v > w ->
      fitting room (List.filteri (fun i _ -> i < List.length views - 1) views)
    | _ -> v)

(* The three head lines and the recent requests always show. The top
   panels, the error lines and the bottom panels follow, each kept only
   when it leaves the recent requests at least [min_rows], and the recent
   requests take the lines left. Without a height, as in the last frame,
   everything shows. *)
let min_rows = 3
let max_rows = 25

let view t =
  View.fit (fun room ->
      let height = Option.value (Room.height room) ~default:max_int in
      let head = 3 and frame = 3 in
      let top_v =
        fitting room [ traffic t; latency t; statuses t; process t ]
      in
      let bottom_v =
        fitting room [ paths t; slowest t; agents t; history t ]
      in
      let errors_v = errors t in
      let keep used v =
        if used + View.height v + frame + min_rows <= height then
          (used + View.height v, v)
        else (used, View.empty)
      in
      let used, top_v = keep head top_v in
      let used, errors_v = keep used errors_v in
      let used, bottom_v = keep used bottom_v in
      let rows =
        if height = max_int then max_rows
        else max min_rows (min max_rows (height - used - frame))
      in
      View.vcat
        [
          headline t;
          content_line t;
          load_line t;
          top_v;
          recent t rows;
          bottom_v;
          errors_v;
        ])

(* {1 Running} *)

(* Logs messages print above the region: a mark and a colour per level, and
   a source other than the default named first. *)
let log_line src level msg =
  let mark, level_role =
    match level with
    | Logs.Error -> ("\xe2\x9c\x97 ", `Failed)
    | Logs.Warning -> ("! ", `Warning)
    | Logs.Debug -> ("\xc2\xb7 ", `Muted)
    | Logs.Info | Logs.App -> ("", `Accent)
  in
  let source =
    if Logs.Src.equal src Logs.default then View.empty
    else muted (Fmt.str "[%s] " (Logs.Src.name src))
  in
  View.(role level_role (txt mark) ++ source ++ txt msg)

let reporter display =
  let report src level ~over k msgf =
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    Format.kasprintf
      (fun msg ->
        Display.print display (log_line src level msg);
        over ();
        k ())
      fmt
  in
  { Logs.report }

(* Rates settle for this long after the last request, one frame a second. *)
let settle = 60.

let redraw_loop ~sw ~clock ?reader t display =
  let rec loop () =
    if not t.pending then begin
      let settling = t.now () -. t.last_request < settle in
      if settling then
        Eio.Fiber.first
          (fun () -> Eio.Condition.await_no_mutex t.changed)
          (fun () -> Eio.Time.Mono.sleep clock 1.)
      else Eio.Condition.await_no_mutex t.changed
    end;
    (* A burst of requests makes one frame. *)
    Eio.Time.Mono.sleep clock 0.1;
    t.pending <- false;
    sample t;
    maybe_refresh_history ~sw t reader;
    Display.redraw display;
    loop ()
  in
  loop ()

let run ~sw ~clock ?reader t f =
  let ppf = Fmt.stderr in
  (* Frames are drawn on change alone, so a theme that moves would jump. *)
  Theme.set ppf (Theme.still (Theme.get ppf));
  let display =
    Display.v ?size:(Console_eio.size ppf) ~now:t.now ppf (view t)
  in
  let previous = Logs.reporter () in
  let close () =
    Logs.set_reporter previous;
    Display.close display
  in
  (* An ending signal writes a byte to a pipe, which a fiber reads before
     closing the display and exiting. The handler must be portable, so it
     captures only the pipe's descriptor. Exiting also closes the display,
     for an exit from elsewhere in the program. *)
  at_exit close;
  let readable, writable = Unix.pipe ~cloexec:true () in
  let handler =
    Sys.Signal_handle
      (fun _ ->
        try ignore (Unix.single_write_substring writable "!" 0 1 : int)
        with Unix.Unix_error _ -> ())
  in
  let ending = [ Sys.sigint; Sys.sigterm ] in
  let restore =
    List.map
      (fun signal ->
        let previous = Sys.Safe.signal signal handler in
        fun () -> Sys.Safe.set_signal signal previous)
      ending
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio_unix.await_readable readable;
      close ();
      exit 130);
  Logs.set_reporter (reporter display);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Display.redraw display;
      redraw_loop ~sw ~clock ?reader t display);
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun restore -> restore ()) restore;
      Unix.close writable;
      Unix.close readable;
      close ())
    f
