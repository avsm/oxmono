open Cmdliner
module C = Perma_proxy.Cache
module L = Perma_proxy.Logger

let parse_map s =
  match String.index_opt s '=' with
  | None -> invalid_arg "Mappings use PREFIX=UPSTREAM"
  | Some i ->
      Perma_proxy.mapping ~prefix:(String.sub s 0 i)
        ~upstream:(String.sub s (i + 1) (String.length s - i - 1))

let protect f =
  try
    Eio_main.run f;
    0
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
      prerr_endline (Printexc.to_string exn);
      1

let directory env path = Eio.Path.(Eio.Stdenv.fs env / path)

let owned env dir ?client ?limits ?upstreams f =
  C.with_open ~dir
    ~clock:(Eio.Stdenv.mono_clock env)
    ~wall_clock:(Eio.Stdenv.clock env)
    ~random:(Eio.Stdenv.secure_random env)
    ?client ?limits ?upstreams f

let run port cache_dir verbose maps max_object reserve downloads timeout
    max_entries allow_query roots =
  protect (fun env ->
      let mappings = List.map parse_map maps in
      let limits : C.limits =
        {
          max_object_bytes = max_object;
          reserve_bytes = reserve;
          downloads;
          timeout = Duration.of_sec timeout;
          max_entries;
        }
      in
      let client =
        Fetch_httpz.v ~https:Httpz_tls.system ~decode:false
          ~max_response:max_object
          ~clock:(Eio.Stdenv.mono_clock env)
          (Eio.Stdenv.net env) ()
      in
      owned env (directory env cache_dir)
        ~client:(client :> Fetch.plain)
        ~limits
        ~upstreams:(List.map Perma_proxy.upstream mappings @ roots)
        (fun cache ->
          let proxy = Perma_proxy.create ~cache ~allow_query mappings in
          let on_event (event : Proffer_httpz.event @ local) =
            let event = Proffer_httpz.globalize_event event in
            let meth = Httpz.Method.to_string event.meth in
            let status = Httpz.Res.status_code event.status in
            let range =
              List.exists
                (fun (name, _) -> String.lowercase_ascii name = "range")
                event.request_headers
            in
            (try
               L.request (C.logger cache) ~meth ~path:event.path
                 ~object_key:(Perma_proxy.object_key proxy event.target)
                 ~range ~status ~cache:event.cache_status ~bytes:event.body_size
                 ~duration_us:event.duration_us
             with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
                Printf.eprintf "Proxy telemetry write failed: %s\n%!"
                  (Printexc.to_string exn));
            if verbose then
              Printf.printf "%s %s -> %d (%s)\n%!" meth event.path status
                (Option.value event.cache_status ~default:"-")
          in
          let connections = ref 0 in
          let session =
            Printf.sprintf "%d:%.6f" (Unix.getpid ())
              (Eio.Time.now (Eio.Stdenv.clock env))
          in
          let heartbeat running =
            let queued, active = C.activity cache in
            try
              L.heartbeat (C.logger cache) ~session ~running
                ~connections:(if running then !connections else 0)
                ~queued:(if running then queued else 0)
                ~transfers:(if running then active else [])
            with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
                Printf.eprintf "Proxy heartbeat failed: %s\n%!"
                  (Printexc.to_string exn)
          in
          Fun.protect
            ~finally:(fun () -> Eio.Cancel.protect (fun () -> heartbeat false))
            (fun () ->
              Eio.Switch.run (fun sw ->
                  Eio.Fiber.fork_daemon ~sw (fun () ->
                      while true do
                        heartbeat true;
                        Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 1.
                      done;
                      `Stop_daemon);
                  Proffer_httpz.run ~sw env ~port ~env:proxy ~on_event
                    ~on_connection:(fun delta ->
                      connections := !connections + delta)
                    ~on_error:(fun exn ->
                      prerr_endline (Printexc.to_string exn))
                    Perma_proxy.site))))

let gc cache_dir =
  protect (fun env ->
      owned env (directory env cache_dir) (fun cache ->
          let result = C.gc cache in
          Printf.printf "Removed %d blobs (%Ld bytes) and %d temporary files.\n"
            result.blobs result.bytes result.temporary))

let forget cache_dir url =
  protect (fun env ->
      owned env (directory env cache_dir) (fun cache ->
          C.forget cache ~url;
          print_endline
            "URL removed from the index. Run gc to reclaim unreferenced blobs."))

let stats cache_dir =
  protect (fun env ->
      Eio.Switch.run (fun sw ->
          let dir = directory env cache_dir in
          Printf.printf "{\"cache\":%s,\"telemetry\":%s}\n" (C.summary ~sw dir)
            (L.summary ~sw dir)))

let snapshot cache_dir =
  protect (fun env ->
      Eio.Switch.run (fun sw ->
          print_endline
            (L.snapshot ~sw ~clock:(Eio.Stdenv.clock env)
               ~mono_clock:(Eio.Stdenv.mono_clock env)
               (directory env cache_dir))))

let export cache_dir table after limit =
  protect (fun env ->
      Eio.Switch.run (fun sw ->
          print_endline
            (L.export ~sw (directory env cache_dir) ~table ~after ~limit)))

let port =
  Arg.(
    value & opt int 9999
    & info [ "port"; "p" ] ~docv:"PORT" ~doc:"Listen on loopback at this port.")

let cache =
  Arg.(
    value & opt string "./perma-cache"
    & info [ "cache-dir" ] ~docv:"DIR"
        ~doc:
          "Private cache directory. Serving and maintenance require exclusive \
           ownership.")

let verbose =
  Arg.(value & flag & info [ "verbose"; "v" ] ~doc:"Print completed requests.")

let maps =
  Arg.(
    value
    & opt_all string
        [
          "/tessera=https://data.source.coop/tessera/tessera/zarr/v1.1-dclimate";
        ]
    & info [ "map"; "m" ] ~docv:"PREFIX=URL"
        ~doc:"Map a local prefix to an immutable upstream store.")

let max_object =
  Arg.(
    value
    & opt int C.default_limits.max_object_bytes
    & info [ "max-object-bytes" ] ~doc:"Maximum whole-object size, in bytes.")

let reserve =
  Arg.(
    value
    & opt int64 C.default_limits.reserve_bytes
    & info [ "reserve-bytes" ]
        ~doc:"Minimum free disk space after reserved downloads, in bytes.")

let downloads =
  Arg.(
    value
    & opt int C.default_limits.downloads
    & info [ "downloads" ] ~doc:"Maximum concurrent upstream downloads.")

let timeout =
  Arg.(
    value & opt int 1800
    & info [ "download-timeout" ]
        ~doc:"Maximum seconds per active upstream download.")

let max_entries =
  Arg.(
    value
    & opt int C.default_limits.max_entries
    & info [ "max-entries" ]
        ~doc:"Maximum number of cached URLs, including missing objects.")

let allow_query =
  Arg.(
    value & flag
    & info [ "allow-query" ]
        ~doc:
          "Allow query strings as distinct cache keys. Use only with immutable \
           public URLs.")

let roots =
  Arg.(
    value & opt_all string []
    & info [ "allow-upstream" ] ~docv:"URL"
        ~doc:
          "Additional approved redirect root. Redirects must remain within a \
           mapped or approved root.")

let serve =
  Term.(
    const run $ port $ cache $ verbose $ maps $ max_object $ reserve $ downloads
    $ timeout $ max_entries $ allow_query $ roots)

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let () =
  let url =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"UPSTREAM-URL")
  in
  let table =
    Arg.(
      value
      & opt
          (enum [ ("requests", `Requests); ("downloads", `Downloads) ])
          `Requests
      & info [ "kind" ] ~doc:"Event table to export.")
  in
  let after =
    Arg.(
      value & opt int64 0L & info [ "after" ] ~doc:"Export rows after this ID.")
  in
  let limit =
    Arg.(value & opt int 1000 & info [ "limit" ] ~doc:"Page size (1 to 10000).")
  in
  exit
    (Cmd.eval'
       (Cmd.group ~default:serve
          (Cmd.info "zarr-perma-proxy"
             ~doc:"Cache whole HTTP objects and serve local byte ranges.")
          [
            command "serve" "Serve the proxy." serve;
            command "gc"
              "Collect unreferenced blobs while the proxy is stopped."
              Term.(const gc $ cache);
            command "forget"
              "Forget an upstream URL while the proxy is stopped."
              Term.(const forget $ cache $ url);
            command "stats"
              "Read cache totals and hourly telemetry as JSON, including while \
               serving."
              Term.(const stats $ cache);
            command "snapshot" "Read live gauges and recent telemetry as JSON."
              Term.(const snapshot $ cache);
            command "export" "Export a page of raw telemetry as JSON."
              Term.(const export $ cache $ table $ after $ limit);
          ]))
