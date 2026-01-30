(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Stress test framework for conpool

    Spawns variable number of echo servers on random ports, then exercises
    the connection pool with multiple parallel client fibers.
    Collects detailed event traces for visualization.
*)

(** Configuration for the stress test *)
type config = {
  name : string;           (** Test name for identification *)
  num_servers : int;       (** Number of echo servers to spawn *)
  num_clients : int;       (** Number of client connections per server *)
  messages_per_client : int;  (** Number of messages each client sends *)
  max_parallel_clients : int; (** Maximum concurrent client fibers *)
  message_size : int;      (** Size of each message in bytes *)
  pool_size : int;         (** Max connections per endpoint *)
}

let default_config = {
  name = "default";
  num_servers = 3;
  num_clients = 10;
  messages_per_client = 5;
  max_parallel_clients = 20;
  message_size = 64;
  pool_size = 5;
}

(** Test presets for different scenarios *)
let presets = [
  (* High connection reuse - few connections, many messages *)
  { name = "high_reuse";
    num_servers = 2;
    num_clients = 20;
    messages_per_client = 50;
    max_parallel_clients = 10;
    message_size = 32;
    pool_size = 3;
  };
  (* Many endpoints - test endpoint scaling *)
  { name = "many_endpoints";
    num_servers = 10;
    num_clients = 10;
    messages_per_client = 10;
    max_parallel_clients = 50;
    message_size = 64;
    pool_size = 5;
  };
  (* High concurrency - stress parallel connections *)
  { name = "high_concurrency";
    num_servers = 3;
    num_clients = 100;
    messages_per_client = 5;
    max_parallel_clients = 100;
    message_size = 64;
    pool_size = 20;
  };
  (* Large messages - test throughput *)
  { name = "large_messages";
    num_servers = 3;
    num_clients = 20;
    messages_per_client = 20;
    max_parallel_clients = 30;
    message_size = 1024;
    pool_size = 10;
  };
  (* Constrained pool - force queuing *)
  { name = "constrained_pool";
    num_servers = 2;
    num_clients = 50;
    messages_per_client = 10;
    max_parallel_clients = 50;
    message_size = 64;
    pool_size = 2;
  };
  (* Burst traffic - many clients, few messages each *)
  { name = "burst_traffic";
    num_servers = 5;
    num_clients = 200;
    messages_per_client = 2;
    max_parallel_clients = 100;
    message_size = 32;
    pool_size = 15;
  };
]

(** Extended stress test - 100x messages, 10x clients/servers *)
let extended_preset = {
  name = "extended_stress";
  num_servers = 30;
  num_clients = 1000;
  messages_per_client = 100;
  max_parallel_clients = 500;
  message_size = 128;
  pool_size = 50;
}

(** Statistics collected during test *)
type latency_stats = {
  mutable count : int;
  mutable total : float;
  mutable min : float;
  mutable max : float;
  mutable latencies : (float * float) list; (* (timestamp, latency) pairs *)
}

let create_latency_stats () = {
  count = 0;
  total = 0.0;
  min = Float.infinity;
  max = 0.0;
  latencies = [];
}

let update_latency stats latency timestamp =
  stats.count <- stats.count + 1;
  stats.total <- stats.total +. latency;
  stats.min <- min stats.min latency;
  stats.max <- max stats.max latency;
  stats.latencies <- (timestamp, latency) :: stats.latencies

(** Generate a random message of given size *)
let generate_message size =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let len = String.length chars in
  String.init size (fun _ -> chars.[Random.int len])

(** Echo server handler - echoes back everything it receives *)
let handle_echo_client flow _addr =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | n ->
        let data = Cstruct.sub buf 0 n in
        Eio.Flow.write flow [data];
        loop ()
    | exception End_of_file -> ()
  in
  loop ()

(** Start an echo server on a random port, returns the port number *)
let start_echo_server ~sw net =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let listening_socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  let actual_addr = Eio.Net.listening_addr listening_socket in
  let port = match actual_addr with
    | `Tcp (_, port) -> port
    | _ -> failwith "Expected TCP address"
  in

  Eio.Fiber.fork_daemon ~sw (fun () ->
    try
      while true do
        Eio.Net.accept_fork ~sw listening_socket
          ~on_error:(fun _ -> ())
          handle_echo_client
      done;
      `Stop_daemon
    with Eio.Cancel.Cancelled _ ->
      `Stop_daemon
  );

  port

(** Client test: connect via pool, send message, verify echo *)
let run_client_test ~clock ~test_start_time pool endpoint message latency_stats errors =
  let msg_len = String.length message in
  let start_time = Eio.Time.now clock in

  try
    Conpool.with_connection pool endpoint (fun conn ->
      (* Send message *)
      Eio.Flow.copy_string message conn.Conpool.flow;
      Eio.Flow.copy_string "\n" conn.Conpool.flow;

      (* Read echo response *)
      let response = Eio.Buf_read.of_flow conn.Conpool.flow ~max_size:(msg_len + 1) in
      let echoed = Eio.Buf_read.line response in

      let end_time = Eio.Time.now clock in
      let latency = (end_time -. start_time) *. 1000.0 in  (* Convert to ms *)
      let relative_time = (end_time -. test_start_time) *. 1000.0 in (* ms since test start *)

      if String.equal echoed message then begin
        update_latency latency_stats latency relative_time
      end else begin
        incr errors
      end
    )
  with _ex ->
    incr errors

(** Run a single client that sends multiple messages *)
let run_client ~clock ~test_start_time pool endpoints (cfg : config) latency_stats errors client_id =
  for _ = 1 to cfg.messages_per_client do
    let endpoint_idx = Random.int (Array.length endpoints) in
    let endpoint = endpoints.(endpoint_idx) in
    let message = Printf.sprintf "c%d-%s" client_id (generate_message cfg.message_size) in
    run_client_test ~clock ~test_start_time pool endpoint message latency_stats errors
  done

(** Pool statistics aggregated from all endpoints *)
type pool_stats = {
  total_created : int;
  total_reused : int;
  total_closed : int;
  active : int;
  idle : int;
  pool_errors : int;
}

(** Test result type *)
type test_result = {
  test_name : string;
  num_servers : int;
  num_clients : int;
  messages_per_client : int;
  pool_size : int;
  duration : float;
  total_messages : int;
  total_errors : int;
  throughput : float;
  avg_latency : float;
  min_latency : float;
  max_latency : float;
  latency_data : (float * float) list; (* (timestamp, latency) pairs for visualization *)
  pool_stats : pool_stats;
}

(** Main stress test runner - returns a test result *)
let run_stress_test ~env (cfg : config) : test_result =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  let latency_stats = create_latency_stats () in
  let errors = ref 0 in
  let ports = ref [||] in

  let result : test_result option ref = ref None in

  begin
    try
      Eio.Switch.run @@ fun sw ->
        (* Start echo servers *)
        ports := Array.init cfg.num_servers (fun _ ->
          start_echo_server ~sw net
        );

        Eio.Time.sleep clock 0.05;

        let endpoints = Array.map (fun port ->
          Conpool.Endpoint.make ~host:"127.0.0.1" ~port
        ) !ports in

        (* Create connection pool *)
        let pool_config = Conpool.Config.make
          ~max_connections_per_endpoint:cfg.pool_size
          ~max_idle_time:30.0
          ~max_connection_lifetime:120.0
          ~connect_timeout:5.0
          ~connect_retry_count:3
          ()
        in

        let pool = Conpool.create_basic ~sw ~net ~clock ~config:pool_config () in

        (* Record start time *)
        let start_time = Eio.Time.now clock in

        (* Run clients in parallel *)
        let total_clients = cfg.num_servers * cfg.num_clients in
        let client_ids = List.init total_clients (fun i -> i) in
        Eio.Fiber.List.iter ~max_fibers:cfg.max_parallel_clients
          (fun client_id ->
            run_client ~clock ~test_start_time:start_time pool endpoints cfg latency_stats errors client_id)
          client_ids;

        let end_time = Eio.Time.now clock in
        let duration = end_time -. start_time in

        (* Collect pool statistics from all endpoints *)
        let all_stats = Conpool.all_stats pool in
        let pool_stats = List.fold_left (fun acc (_, stats) ->
          {
            total_created = acc.total_created + Conpool.Stats.total_created stats;
            total_reused = acc.total_reused + Conpool.Stats.total_reused stats;
            total_closed = acc.total_closed + Conpool.Stats.total_closed stats;
            active = acc.active + Conpool.Stats.active stats;
            idle = acc.idle + Conpool.Stats.idle stats;
            pool_errors = acc.pool_errors + Conpool.Stats.errors stats;
          }
        ) { total_created = 0; total_reused = 0; total_closed = 0; active = 0; idle = 0; pool_errors = 0 } all_stats in

        (* Build result *)
        let r : test_result = {
          test_name = cfg.name;
          num_servers = cfg.num_servers;
          num_clients = cfg.num_clients;
          messages_per_client = cfg.messages_per_client;
          pool_size = cfg.pool_size;
          duration;
          total_messages = latency_stats.count;
          total_errors = !errors;
          throughput = float_of_int latency_stats.count /. duration;
          avg_latency = if latency_stats.count > 0
            then latency_stats.total /. float_of_int latency_stats.count
            else 0.0;
          min_latency = if latency_stats.count > 0 then latency_stats.min else 0.0;
          max_latency = latency_stats.max;
          latency_data = List.rev latency_stats.latencies;
          pool_stats;
        } in
        result := Some r;

        Eio.Switch.fail sw Exit
    with Exit -> ()
  end;

  match !result with
  | Some r -> r
  | None -> failwith "Test failed to produce result"

(** Convert result to JSON string *)
let result_to_json result =
  Printf.sprintf {|{
  "test_name": "%s",
  "num_servers": %d,
  "num_clients": %d,
  "messages_per_client": %d,
  "duration": %.3f,
  "total_messages": %d,
  "total_errors": %d,
  "throughput": %.2f,
  "avg_latency": %.2f,
  "min_latency": %.2f,
  "max_latency": %.2f
}|}
    result.test_name
    result.num_servers
    result.num_clients
    result.messages_per_client
    result.duration
    result.total_messages
    result.total_errors
    result.throughput
    result.avg_latency
    result.min_latency
    result.max_latency

(** Escape strings for JavaScript *)
let js_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | _ -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Calculate histogram buckets for latency data *)
let calculate_histogram latencies num_buckets =
  if List.length latencies = 0 then ([], []) else
  let latency_values = List.map snd latencies in
  let min_lat = List.fold_left min Float.infinity latency_values in
  let max_lat = List.fold_left max 0.0 latency_values in
  let bucket_width = (max_lat -. min_lat) /. float_of_int num_buckets in

  let buckets = Array.make num_buckets 0 in
  List.iter (fun lat ->
    let bucket_idx = min (num_buckets - 1) (int_of_float ((lat -. min_lat) /. bucket_width)) in
    buckets.(bucket_idx) <- buckets.(bucket_idx) + 1
  ) latency_values;

  let bucket_labels = List.init num_buckets (fun i ->
    let start = min_lat +. (float_of_int i *. bucket_width) in
    Printf.sprintf "%.2f" start
  ) in
  let bucket_counts = Array.to_list buckets in
  (bucket_labels, bucket_counts)

(** Generate HTML report from test results *)
let generate_html_report results =
  let timestamp = Unix.time () |> Unix.gmtime in
  let date_str = Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
    (timestamp.Unix.tm_year + 1900)
    (timestamp.Unix.tm_mon + 1)
    timestamp.Unix.tm_mday
    timestamp.Unix.tm_hour
    timestamp.Unix.tm_min
    timestamp.Unix.tm_sec
  in

  (* Calculate summary statistics *)
  let total_messages = List.fold_left (fun acc r -> acc + r.total_messages) 0 results in
  let total_errors = List.fold_left (fun acc r -> acc + r.total_errors) 0 results in
  let total_duration = List.fold_left (fun acc r -> acc +. r.duration) 0.0 results in

  (* Generate JavaScript arrays for comparison charts *)
  let test_names = String.concat ", " (List.map (fun r -> Printf.sprintf "\"%s\"" (js_escape r.test_name)) results) in
  let throughputs = String.concat ", " (List.map (fun r -> Printf.sprintf "%.2f" r.throughput) results) in
  let avg_latencies = String.concat ", " (List.map (fun r -> Printf.sprintf "%.2f" r.avg_latency) results) in
  let error_rates = String.concat ", " (List.map (fun r ->
    if r.total_messages > 0 then
      Printf.sprintf "%.2f" (float_of_int r.total_errors /. float_of_int r.total_messages *. 100.0)
    else "0.0"
  ) results) in

  (* Generate per-test detailed sections with histograms and timelines *)
  let test_details = String.concat "\n" (List.mapi (fun idx r ->
    let (hist_labels, hist_counts) = calculate_histogram r.latency_data 20 in
    let hist_labels_str = String.concat ", " (List.map (fun s -> Printf.sprintf "\"%s\"" s) hist_labels) in
    let hist_counts_str = String.concat ", " (List.map string_of_int hist_counts) in

    (* Sample data points for timeline (take every Nth point if too many) *)
    let max_points = 500 in
    let sample_rate = max 1 ((List.length r.latency_data) / max_points) in
    let sampled_data = List.filteri (fun i _ -> i mod sample_rate = 0) r.latency_data in
    let timeline_data = String.concat ", " (List.map (fun (t, l) ->
      Printf.sprintf "{x: %.2f, y: %.3f}" t l
    ) sampled_data) in

    Printf.sprintf {|
      <div class="test-detail">
        <h3>%s</h3>
        <div class="compact-grid">
          <div class="compact-metric"><span class="label">Servers:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Clients:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Msgs/Client:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Pool Size:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Total Msgs:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Duration:</span> <span class="value">%.2fs</span></div>
          <div class="compact-metric highlight"><span class="label">Throughput:</span> <span class="value">%.0f/s</span></div>
          <div class="compact-metric highlight"><span class="label">Avg Lat:</span> <span class="value">%.2fms</span></div>
          <div class="compact-metric"><span class="label">Min Lat:</span> <span class="value">%.2fms</span></div>
          <div class="compact-metric"><span class="label">Max Lat:</span> <span class="value">%.2fms</span></div>
          <div class="compact-metric %s"><span class="label">Errors:</span> <span class="value">%d</span></div>
        </div>
        <div class="compact-grid" style="margin-top: 0.5rem;">
          <div class="compact-metric"><span class="label">Conns Created:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Conns Reused:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Conns Closed:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Active:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Idle:</span> <span class="value">%d</span></div>
          <div class="compact-metric"><span class="label">Reuse Rate:</span> <span class="value">%.1f%%%%</span></div>
        </div>
        <div class="chart-row">
          <div class="chart-half">
            <h4>Latency Distribution</h4>
            <canvas id="hist_%d"></canvas>
          </div>
          <div class="chart-half">
            <h4>Latency Timeline</h4>
            <canvas id="timeline_%d"></canvas>
          </div>
        </div>
      </div>
      <script>
        new Chart(document.getElementById('hist_%d'), {
          type: 'bar',
          data: {
            labels: [%s],
            datasets: [{
              label: 'Count',
              data: [%s],
              backgroundColor: 'rgba(102, 126, 234, 0.6)',
              borderColor: 'rgba(102, 126, 234, 1)',
              borderWidth: 1
            }]
          },
          options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: { legend: { display: false } },
            scales: {
              x: { title: { display: true, text: 'Latency (ms)' } },
              y: { beginAtZero: true, title: { display: true, text: 'Count' } }
            }
          }
        });

        new Chart(document.getElementById('timeline_%d'), {
          type: 'scatter',
          data: {
            datasets: [{
              label: 'Latency',
              data: [%s],
              backgroundColor: 'rgba(118, 75, 162, 0.5)',
              borderColor: 'rgba(118, 75, 162, 0.8)',
              pointRadius: 2
            }]
          },
          options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: { legend: { display: false } },
            scales: {
              x: { title: { display: true, text: 'Time (ms)' } },
              y: { beginAtZero: true, title: { display: true, text: 'Latency (ms)' } }
            }
          }
        });
      </script>|}
      (js_escape r.test_name)
      r.num_servers
      r.num_clients
      r.messages_per_client
      r.pool_size
      r.total_messages
      r.duration
      r.throughput
      r.avg_latency
      r.min_latency
      r.max_latency
      (if r.total_errors > 0 then "error" else "")
      r.total_errors
      r.pool_stats.total_created
      r.pool_stats.total_reused
      r.pool_stats.total_closed
      r.pool_stats.active
      r.pool_stats.idle
      (if r.pool_stats.total_created > 0 then
        (float_of_int r.pool_stats.total_reused /. float_of_int r.pool_stats.total_created *. 100.0)
      else 0.0)
      idx idx idx
      hist_labels_str
      hist_counts_str
      idx
      timeline_data
  ) results) in

  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Connection Pool Stress Test Results</title>
  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: #f5f5f5;
      padding: 1rem;
      color: #333;
      font-size: 14px;
    }
    .container { max-width: 1600px; margin: 0 auto; }
    h1 {
      color: #667eea;
      text-align: center;
      margin-bottom: 0.3rem;
      font-size: 1.8rem;
    }
    .subtitle {
      text-align: center;
      margin-bottom: 1rem;
      font-size: 0.9rem;
      color: #666;
    }
    .summary {
      background: white;
      border-radius: 6px;
      padding: 1rem;
      margin-bottom: 1rem;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .summary h2 {
      color: #667eea;
      margin-bottom: 0.8rem;
      font-size: 1.2rem;
    }
    .summary-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
      gap: 0.8rem;
    }
    .summary-metric {
      text-align: center;
      padding: 0.8rem;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      border-radius: 4px;
      color: white;
    }
    .summary-metric-label {
      font-size: 0.75rem;
      opacity: 0.9;
      margin-bottom: 0.3rem;
    }
    .summary-metric-value {
      font-size: 1.4rem;
      font-weight: bold;
    }
    .comparison {
      background: white;
      border-radius: 6px;
      padding: 1rem;
      margin-bottom: 1rem;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .comparison h2 {
      color: #667eea;
      margin-bottom: 0.8rem;
      font-size: 1.2rem;
    }
    .comparison-charts {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 1rem;
    }
    .comparison-chart {
      height: 200px;
      position: relative;
    }
    .test-detail {
      background: white;
      border-radius: 6px;
      padding: 1rem;
      margin-bottom: 1rem;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      border-left: 3px solid #667eea;
    }
    .test-detail h3 {
      color: #764ba2;
      margin-bottom: 0.6rem;
      font-size: 1.1rem;
    }
    .test-detail h4 {
      color: #666;
      margin-bottom: 0.4rem;
      font-size: 0.9rem;
      font-weight: 500;
    }
    .compact-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
      gap: 0.4rem;
      margin-bottom: 0.8rem;
      font-size: 0.85rem;
    }
    .compact-metric {
      background: #f8f9fa;
      padding: 0.4rem 0.6rem;
      border-radius: 3px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    .compact-metric .label {
      color: #666;
      font-weight: 500;
    }
    .compact-metric .value {
      color: #333;
      font-weight: 600;
    }
    .compact-metric.highlight {
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      color: white;
    }
    .compact-metric.highlight .label,
    .compact-metric.highlight .value {
      color: white;
    }
    .compact-metric.error {
      background: #fee;
      border: 1px solid #fcc;
    }
    .chart-row {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1rem;
    }
    .chart-half {
      position: relative;
      height: 220px;
    }
    @media (max-width: 1200px) {
      .comparison-charts { grid-template-columns: 1fr; }
      .chart-row { grid-template-columns: 1fr; }
    }
    @media (max-width: 768px) {
      .compact-grid { grid-template-columns: repeat(2, 1fr); }
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>Connection Pool Stress Test Results</h1>
    <div class="subtitle">%s</div>

    <div class="summary">
      <h2>Summary</h2>
      <div class="summary-grid">
        <div class="summary-metric">
          <div class="summary-metric-label">Tests</div>
          <div class="summary-metric-value">%d</div>
        </div>
        <div class="summary-metric">
          <div class="summary-metric-label">Messages</div>
          <div class="summary-metric-value">%s</div>
        </div>
        <div class="summary-metric">
          <div class="summary-metric-label">Errors</div>
          <div class="summary-metric-value">%d</div>
        </div>
        <div class="summary-metric">
          <div class="summary-metric-label">Duration</div>
          <div class="summary-metric-value">%.1fs</div>
        </div>
      </div>
    </div>

    <div class="comparison">
      <h2>Comparison</h2>
      <div class="comparison-charts">
        <div class="comparison-chart"><canvas id="cmpThroughput"></canvas></div>
        <div class="comparison-chart"><canvas id="cmpLatency"></canvas></div>
        <div class="comparison-chart"><canvas id="cmpErrors"></canvas></div>
      </div>
    </div>

%s
  </div>

  <script>
    const testNames = [%s];
    const throughputs = [%s];
    const avgLatencies = [%s];
    const errorRates = [%s];

    const cc = {
      primary: 'rgba(102, 126, 234, 0.8)',
      secondary: 'rgba(118, 75, 162, 0.8)',
      danger: 'rgba(220, 53, 69, 0.8)',
    };

    new Chart(document.getElementById('cmpThroughput'), {
      type: 'bar',
      data: {
        labels: testNames,
        datasets: [{
          label: 'msg/s',
          data: throughputs,
          backgroundColor: cc.primary,
          borderColor: cc.primary,
          borderWidth: 1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          title: { display: true, text: 'Throughput (msg/s)' }
        },
        scales: { y: { beginAtZero: true } }
      }
    });

    new Chart(document.getElementById('cmpLatency'), {
      type: 'bar',
      data: {
        labels: testNames,
        datasets: [{
          label: 'ms',
          data: avgLatencies,
          backgroundColor: cc.secondary,
          borderColor: cc.secondary,
          borderWidth: 1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          title: { display: true, text: 'Avg Latency (ms)' }
        },
        scales: { y: { beginAtZero: true } }
      }
    });

    new Chart(document.getElementById('cmpErrors'), {
      type: 'bar',
      data: {
        labels: testNames,
        datasets: [{
          label: '%%',
          data: errorRates,
          backgroundColor: cc.danger,
          borderColor: cc.danger,
          borderWidth: 1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          title: { display: true, text: 'Error Rate (%%)' }
        },
        scales: { y: { beginAtZero: true } }
      }
    });
  </script>
</body>
</html>|}
    date_str
    (List.length results)
    (if total_messages >= 1000 then
      Printf.sprintf "%d,%03d" (total_messages / 1000) (total_messages mod 1000)
    else
      string_of_int total_messages)
    total_errors
    total_duration
    test_details
    test_names
    throughputs
    avg_latencies
    error_rates

(** Run all preset tests and return results *)
let run_all_presets ~env =
  List.map (fun config ->
    Printf.eprintf "Running test: %s\n%!" config.name;
    run_stress_test ~env config
  ) presets

(** Parse command line arguments *)
type mode =
  | Single of config
  | AllPresets
  | Extended
  | ListPresets

let parse_args () =
  let mode = ref (Single default_config) in
  let name = ref default_config.name in
  let num_servers = ref default_config.num_servers in
  let num_clients = ref default_config.num_clients in
  let messages_per_client = ref default_config.messages_per_client in
  let max_parallel = ref default_config.max_parallel_clients in
  let message_size = ref default_config.message_size in
  let pool_size = ref default_config.pool_size in
  let output_file = ref "stress_test_results.json" in

  let specs = [
    ("--all", Arg.Unit (fun () -> mode := AllPresets),
      "Run all preset test configurations");
    ("--extended", Arg.Unit (fun () -> mode := Extended),
      "Run extended stress test (30 servers, 1000 clients, 100 msgs each = 3M messages)");
    ("--list", Arg.Unit (fun () -> mode := ListPresets),
      "List available presets");
    ("--preset", Arg.String (fun p ->
      match List.find_opt (fun c -> c.name = p) presets with
      | Some c -> mode := Single c
      | None -> failwith (Printf.sprintf "Unknown preset: %s" p)),
      "Use a named preset configuration");
    ("-n", Arg.Set_string name, "Test name");
    ("-s", Arg.Set_int num_servers, Printf.sprintf "Number of servers (default: %d)" default_config.num_servers);
    ("-c", Arg.Set_int num_clients, Printf.sprintf "Clients per server (default: %d)" default_config.num_clients);
    ("-m", Arg.Set_int messages_per_client, Printf.sprintf "Messages per client (default: %d)" default_config.messages_per_client);
    ("-p", Arg.Set_int max_parallel, Printf.sprintf "Max parallel clients (default: %d)" default_config.max_parallel_clients);
    ("-b", Arg.Set_int message_size, Printf.sprintf "Message size (default: %d)" default_config.message_size);
    ("-P", Arg.Set_int pool_size, Printf.sprintf "Pool size per endpoint (default: %d)" default_config.pool_size);
    ("-o", Arg.Set_string output_file, "Output JSON file (default: stress_test_results.json)");
  ] in

  let usage = "Usage: stress_test [options]\n\nOptions:" in
  Arg.parse specs (fun _ -> ()) usage;

  let config = {
    name = !name;
    num_servers = !num_servers;
    num_clients = !num_clients;
    messages_per_client = !messages_per_client;
    max_parallel_clients = !max_parallel;
    message_size = !message_size;
    pool_size = !pool_size;
  } in

  (!mode, config, !output_file)

let () =
  Random.self_init ();
  let (mode, custom_config, output_file) = parse_args () in

  match mode with
  | ListPresets ->
      Printf.printf "Available presets:\n";
      List.iter (fun c ->
        Printf.printf "  %s: %d servers, %d clients, %d msgs/client, pool=%d\n"
          c.name c.num_servers c.num_clients c.messages_per_client c.pool_size
      ) presets

  | Single config ->
      let config = if config.name = "default" then custom_config else config in
      Eio_main.run @@ fun env ->
        let result = run_stress_test ~env config in
        let results = [result] in

        (* Write JSON *)
        let json = Printf.sprintf "[%s]" (result_to_json result) in
        let oc = open_out output_file in
        output_string oc json;
        close_out oc;
        Printf.printf "Results written to %s\n" output_file;

        (* Write HTML *)
        let html_file =
          if Filename.check_suffix output_file ".json" then
            Filename.chop_suffix output_file ".json" ^ ".html"
          else
            output_file ^ ".html"
        in
        let html = generate_html_report results in
        let oc_html = open_out html_file in
        output_string oc_html html;
        close_out oc_html;
        Printf.printf "HTML report written to %s\n" html_file;

        Printf.printf "Test: %s - %d messages, %.2f msg/s, %.2fms avg latency, %d errors\n"
          result.test_name result.total_messages result.throughput result.avg_latency result.total_errors

  | AllPresets ->
      Eio_main.run @@ fun env ->
        let results = run_all_presets ~env in

        (* Write JSON *)
        let json = "[" ^ String.concat ",\n" (List.map result_to_json results) ^ "]" in
        let oc = open_out output_file in
        output_string oc json;
        close_out oc;
        Printf.printf "Results written to %s\n" output_file;

        (* Write HTML *)
        let html_file =
          if Filename.check_suffix output_file ".json" then
            Filename.chop_suffix output_file ".json" ^ ".html"
          else
            output_file ^ ".html"
        in
        let html = generate_html_report results in
        let oc_html = open_out html_file in
        output_string oc_html html;
        close_out oc_html;
        Printf.printf "HTML report written to %s\n" html_file;

        List.iter (fun r ->
          Printf.printf "  %s: %d messages, %.2f msg/s, %.2fms avg latency, %d errors\n"
            r.test_name r.total_messages r.throughput r.avg_latency r.total_errors
        ) results

  | Extended ->
      Printf.printf "Running extended stress test: %d servers, %d clients/server, %d msgs/client\n"
        extended_preset.num_servers extended_preset.num_clients extended_preset.messages_per_client;
      Printf.printf "Total messages: %d\n%!"
        (extended_preset.num_servers * extended_preset.num_clients * extended_preset.messages_per_client);
      Eio_main.run @@ fun env ->
        let result = run_stress_test ~env extended_preset in
        let results = [result] in

        (* Write JSON *)
        let json = Printf.sprintf "[%s]" (result_to_json result) in
        let oc = open_out output_file in
        output_string oc json;
        close_out oc;
        Printf.printf "Results written to %s\n" output_file;

        (* Write HTML *)
        let html_file =
          if Filename.check_suffix output_file ".json" then
            Filename.chop_suffix output_file ".json" ^ ".html"
          else
            output_file ^ ".html"
        in
        let html = generate_html_report results in
        let oc_html = open_out html_file in
        output_string oc_html html;
        close_out oc_html;
        Printf.printf "HTML report written to %s\n" html_file;

        Printf.printf "Test: %s - %d messages, %.2f msg/s, %.2fms avg latency, %d errors\n"
          result.test_name result.total_messages result.throughput result.avg_latency result.total_errors
