open Cmdliner

(* Copy a local_ string to a global string.
   Needed because httpz_eio's request_info fields are stack-allocated. *)
let globalize (local_ s : string) : string =
  let len = String.length s in
  let dst = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst i (String.unsafe_get s i)
  done;
  Bytes.unsafe_to_string dst

let run port cache_dir verbose maps_strs =
  let maps = List.filter_map Perma_cache.parse_map maps_strs in
  if maps = [] then begin
    Printf.eprintf "Error: no valid --map arguments provided\n%!";
    exit 1
  end;
  Printf.printf "httpz-perma-proxy listening on port %d\n%!" port;
  Printf.printf "Cache directory: %s\n%!" cache_dir;
  List.iter (fun (m : Perma_cache.url_map) ->
    Printf.printf "  %s -> %s (host: %s)\n%!" m.prefix m.upstream m.upstream_host)
    maps;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  (try Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / cache_dir)
   with Eio.Io _ -> ());
  let session = Requests.create ~sw env in
  (* Build catch-all route *)
  let open Httpz_server.Route in
  let routes = of_list [
    get_h tail (Httpz.Header_name.Range +> h0)
      (fun segments (range_header, ()) ctx respond ->
        let path = "/" ^ String.concat "/" segments in
        let is_head = Httpz_server.Route.is_head ctx in
        let net = Eio.Stdenv.net env in
        let r = Perma_cache.handle_request
          ~sw ~net ~fs ~cache_dir ~session ~maps ~verbose
          ~path ~is_head ~range_header in
        let _: unit = (respond ~status:r.status) ~headers:r.resp_headers r.body in
        ());
    (* OPTIONS for CORS preflight *)
    route Httpz.Method.Options tail h0
      (fun _segments () _ctx respond ->
        let r = Perma_cache.cors_preflight_response in
        let _: unit = (respond ~status:r.status) ~headers:r.resp_headers r.body in
        ())
  ] in
  let on_request (local_ info : Httpz_eio.request_info) =
    let meth = Httpz.Method.to_string info.meth in
    let path = globalize info.path in
    let status = Httpz.Res.status_to_string info.status in
    let duration = info.duration_us in
    Printf.printf "%s %s -> %s (%dus)\n%!" meth path status duration
  in
  let on_error exn =
    Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string exn)
  in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  Eio.Net.run_server socket
    ~on_error:(fun exn ->
      Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn))
    (fun flow addr ->
      Httpz_eio.handle_client ~routes ~on_request ~on_error flow addr)

let port_t =
  Arg.(value & opt int 9999 & info ["port"; "p"] ~doc:"Listen port (default: 9999)")

let cache_dir_t =
  Arg.(value & opt string "./perma-cache" & info ["cache-dir"] ~doc:"Cache directory (default: ./perma-cache)")

let verbose_t =
  Arg.(value & flag & info ["verbose"; "v"] ~doc:"Verbose logging")

let map_t =
  Arg.(non_empty & opt_all string [] & info ["map"; "m"]
         ~doc:"URL mapping PREFIX=UPSTREAM (repeatable, at least one required)")

let cmd =
  let doc = "Permanent caching HTTP proxy for Vite/browser tile access" in
  let info = Cmd.info "httpz-perma-proxy" ~doc in
  Cmd.v info Term.(const run $ port_t $ cache_dir_t $ verbose_t $ map_t)

let () = exit (Cmd.eval cmd)
