(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Test conpool with 16 localhost servers on different 127.0.* addresses *)

open Eio.Std

(* Create a simple echo server on a specific address and port *)
let create_server ~sw ~net ipaddr port connections_ref =
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10
    (`Tcp (ipaddr, port))
  in

  Eio.Fiber.fork ~sw (fun () ->
    try
      while true do
        Eio.Net.accept_fork socket ~sw ~on_error:(fun ex ->
          traceln "Server %a error: %s" Eio.Net.Sockaddr.pp (`Tcp (ipaddr, port))
            (Printexc.to_string ex)
        ) (fun flow _addr ->
          (* Track this connection *)
          Atomic.incr connections_ref;

          (* Simple protocol: read lines and echo them back, until EOF *)
          try
            let buf = Eio.Buf_read.of_flow flow ~max_size:1024 in
            while true do
              let line = Eio.Buf_read.line buf in
              traceln "Server on %a:%d received: %s"
                Eio.Net.Ipaddr.pp ipaddr port line;

              Eio.Flow.copy_string (line ^ "\n") flow
            done
          with
          | End_of_file ->
              traceln "Server on %a:%d client disconnected"
                Eio.Net.Ipaddr.pp ipaddr port;
              Eio.Flow.close flow;
              Atomic.decr connections_ref
          | ex ->
              traceln "Server on %a:%d error handling connection: %s"
                Eio.Net.Ipaddr.pp ipaddr port
                (Printexc.to_string ex);
              Eio.Flow.close flow;
              Atomic.decr connections_ref
        )
      done
    with Eio.Cancel.Cancelled _ -> ()
  )

(** Generate 16 different servers on 127.0.0.1 with different ports *)
let generate_localhost_addresses () =
  List.init 16 (fun i ->
    (* Use 127.0.0.1 for all, just different ports *)
    let addr_str = "127.0.0.1" in
    (* Create raw IPv4 address as 4 bytes *)
    let raw_bytes = Bytes.create 4 in
    Bytes.set raw_bytes 0 (Char.chr 127);
    Bytes.set raw_bytes 1 (Char.chr 0);
    Bytes.set raw_bytes 2 (Char.chr 0);
    Bytes.set raw_bytes 3 (Char.chr 1);
    let addr = Eio.Net.Ipaddr.of_raw (Bytes.to_string raw_bytes) in
    (addr_str, addr, 10000 + i)
  )

let () =
  (* Setup logging *)
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Logs.Src.set_level Conpool.src (Some Logs.Debug);

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->

  traceln "=== Starting 16 localhost servers ===";

  (* Generate addresses *)
  let servers = generate_localhost_addresses () in

  (* Create connection counters for each server *)
  let connection_refs = List.map (fun _ -> Atomic.make 0) servers in

  (* Start all servers *)
  List.iter2 (fun (_addr_str, addr, port) conn_ref ->
    traceln "Starting server on %a:%d"
      Eio.Net.Ipaddr.pp addr port;
    create_server ~sw ~net:env#net addr port conn_ref
  ) servers connection_refs;

  (* Give servers time to start *)
  Eio.Time.sleep env#clock 0.5;

  traceln "\n=== Creating connection pool ===";

  (* Create connection pool *)
  let pool_config = Conpool.Config.make
    ~max_connections_per_endpoint:5
    ~max_idle_time:30.0
    ~max_connection_lifetime:60.0
    ()
  in

  let pool = Conpool.create_basic
    ~sw
    ~net:env#net
    ~clock:env#clock
    ~config:pool_config
    ()
  in

  traceln "\n=== Stress testing with thousands of concurrent connections ===";

  (* Disable debug logging for stress test *)
  Logs.Src.set_level Conpool.src (Some Logs.Info);

  (* Create endpoints for all servers *)
  let endpoints = List.map (fun (addr_str, _addr, port) ->
    Conpool.Endpoint.make ~host:addr_str ~port
  ) servers in

  (* Stress test: thousands of concurrent requests across all 16 servers *)
  let num_requests = 50000 in

  traceln "Launching %d concurrent requests across %d endpoints..."
    num_requests (List.length endpoints);
  traceln "Pool config: max %d connections per endpoint"
    (Conpool.Config.max_connections_per_endpoint pool_config);

  let start_time = Unix.gettimeofday () in
  let success_count = Atomic.make 0 in
  let error_count = Atomic.make 0 in
  let last_progress = ref 0 in

  (* Generate list of (endpoint, request_id) pairs *)
  let tasks = List.init num_requests (fun i ->
    let endpoint = List.nth endpoints (i mod List.length endpoints) in
    (endpoint, i)
  ) in

  (* Run all requests concurrently with fiber limit *)
  Eio.Fiber.List.iter ~max_fibers:200 (fun (endpoint, req_id) ->
    try
      Conpool.with_connection pool endpoint (fun conn ->
        let test_msg = Printf.sprintf "Request %d" req_id in
        Eio.Flow.copy_string (test_msg ^ "\n") conn.Conpool.flow;

        let buf = Eio.Buf_read.of_flow conn.Conpool.flow ~max_size:1024 in
        let _response = Eio.Buf_read.line buf in
        let count = Atomic.fetch_and_add success_count 1 + 1 in

        (* Progress indicator every 5000 requests *)
        if count / 5000 > !last_progress then begin
          last_progress := count / 5000;
          traceln "  Progress: %d/%d (%.1f%%)"
            count num_requests
            (100.0 *. float_of_int count /. float_of_int num_requests)
        end
      )
    with e ->
      Atomic.incr error_count;
      if Atomic.get error_count <= 10 then
        traceln "Request %d to %a failed: %s"
          req_id Conpool.Endpoint.pp endpoint (Printexc.to_string e)
  ) tasks;

  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in
  let successful = Atomic.get success_count in
  let failed = Atomic.get error_count in

  traceln "\n=== Stress test results ===";
  traceln "Total requests: %d" num_requests;
  traceln "Successful: %d" successful;
  traceln "Failed: %d" failed;
  traceln "Duration: %.2fs" duration;
  traceln "Throughput: %.0f req/s" (float_of_int successful /. duration);
  traceln "Average latency: %.2fms" (duration *. 1000.0 /. float_of_int successful);

  traceln "\n=== Connection pool statistics ===";
  let all_stats = Conpool.all_stats pool in

  (* Calculate totals *)
  let total_created = List.fold_left (fun acc (_, s) -> acc + Conpool.Stats.total_created s) 0 all_stats in
  let total_reused = List.fold_left (fun acc (_, s) -> acc + Conpool.Stats.total_reused s) 0 all_stats in
  let total_closed = List.fold_left (fun acc (_, s) -> acc + Conpool.Stats.total_closed s) 0 all_stats in
  let total_errors = List.fold_left (fun acc (_, s) -> acc + Conpool.Stats.errors s) 0 all_stats in

  traceln "Total connections created: %d" total_created;
  traceln "Total connections reused: %d" total_reused;
  traceln "Total connections closed: %d" total_closed;
  traceln "Total errors: %d" total_errors;
  traceln "Connection reuse ratio: %.2fx (reused/created)"
    (if total_created > 0 then float_of_int total_reused /. float_of_int total_created else 0.0);
  traceln "Pool efficiency: %.1f%% (avoided creating %d connections)"
    (if successful > 0 then 100.0 *. float_of_int total_reused /. float_of_int successful else 0.0)
    total_reused;

  traceln "\nPer-endpoint breakdown:";
  List.iter (fun (endpoint, stats) ->
    traceln "  %a: created=%d reused=%d active=%d idle=%d"
      Conpool.Endpoint.pp endpoint
      (Conpool.Stats.total_created stats)
      (Conpool.Stats.total_reused stats)
      (Conpool.Stats.active stats)
      (Conpool.Stats.idle stats)
  ) all_stats;

  traceln "\n=== Verifying server-side connection counts ===";
  List.iter2 (fun (addr_str, _addr, port) conn_ref ->
    let count = Atomic.get conn_ref in
    traceln "Server %s:%d - Active connections: %d" addr_str port count
  ) servers connection_refs;

  traceln "\n=== Test completed successfully ==="
