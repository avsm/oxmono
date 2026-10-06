(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let parse_map s =
  match String.index_opt s '=' with
  | None -> invalid_arg "Mappings use PREFIX=UPSTREAM"
  | Some i ->
      Perma_proxy.mapping ~prefix:(String.sub s 0 i)
        ~upstream:(String.sub s (i + 1) (String.length s - i - 1))

let run port cache_dir verbose maps =
  try
    let mappings = List.map parse_map maps in
    Eio_main.run @@ fun env ->
    let dir = Eio.Path.(Eio.Stdenv.fs env / cache_dir) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
    Eio.Switch.run @@ fun sw ->
    let lock = Eio.Path.open_out ~sw ~create:(`If_missing 0o600)
        Eio.Path.(dir / ".lock") in
    let fd = match Eio_unix.Resource.fd_opt lock with
      | Some fd -> fd
      | None -> failwith "Cache directory locking requires an OS file descriptor" in
    (* Eio has no file-lock API. Keep the nonblocking POSIX call within
       the lifetime of the Eio-owned descriptor. *)
    Eio_unix.Fd.use_exn "cache directory lock" fd (fun fd ->
        try Unix.lockf fd Unix.F_TLOCK 0 with
        | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
            failwith "Cache directory is already in use by another proxy"
        | Unix.Unix_error (code, fn, arg) -> raise (Eio_unix.Err.v code fn arg));
    let client = Fetch_httpz.v ~https:Httpz_tls.system ~decode:false
        ~max_response:max_int ~clock:(Eio.Stdenv.mono_clock env) (Eio.Stdenv.net env) () in
    let cache = Perma_proxy.Cache.create ~dir ~client:(client :> Fetch.plain)
        ~random:(Eio.Stdenv.secure_random env) in
    let proxy = Perma_proxy.create ~cache mappings in
    let on_event (event : Proffer_httpz.event @ local) =
      if verbose then Printf.printf "%s %s -> %d (%s)\n%!"
          (Httpz.Method.to_string event.meth) (Proffer.Req.globalize event.path)
          (Httpz.Res.status_code event.status)
          (match event.cache_status with
           | None -> "-" | Some s -> Proffer.Req.globalize s) in
    Proffer_httpz.run ~sw env ~port ~env:proxy ~on_event
      ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
      Perma_proxy.site;
    0
  with
  | Invalid_argument s | Failure s -> prerr_endline s; 1
  | Eio.Io _ | Unix.Unix_error _ as e -> prerr_endline (Printexc.to_string e); 1

let port = Arg.(value & opt int 9999 & info ["port"; "p"] ~docv:"PORT"
    ~doc:"Listen on loopback at this port.")
let cache = Arg.(value & opt string "./perma-cache" & info ["cache-dir"]
    ~docv:"DIR" ~doc:"Persistent cache directory, owned by one server process.")
let verbose = Arg.(value & flag & info ["verbose"; "v"] ~doc:"Log requests.")
let maps = Arg.(value & opt_all string
    ["/tessera=https://data.source.coop/tessera/tessera/zarr/v1.1-dclimate"]
    & info ["map"; "m"] ~docv:"PREFIX=URL"
        ~doc:"Upstream mappings. Defaults to the v1.1-dclimate Tessera Zarr store.")
let () = exit (Cmd.eval' (Cmd.v
    (Cmd.info "zarr-perma-proxy" ~doc:"Permanently cache HTTP objects and Zarr shard ranges.")
    Term.(const run $ port $ cache $ verbose $ maps)))
