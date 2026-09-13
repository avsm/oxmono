(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
let bounded_input () =
  let b = Buffer.create 4096 and chunk = Bytes.create 4096 in
  let rec loop () =
    let n = input stdin chunk 0 4096 in
    if n > 0 then (
      if Buffer.length b + n > Termanil_protocol.max_bytes then
        failwith "Worker input exceeds 64 MiB";
      Buffer.add_subbytes b chunk 0 n;
      loop ())
  in
  loop ();
  Buffer.contents b

let run path init check demo demo_dir =
  try
    if check then (
      print_endline "termanil/v4";
      Ok ())
    else if init then (
      print_endline (Termanil_backend.Config.init ?path ());
      Ok ())
    else
      let result =
        match Termanil_protocol.parse_request (bounded_input ()) with
        | Error s -> Error s
        | Ok request ->
            let config =
              if demo || Option.is_some demo_dir then
                Termanil_backend.Config.demo ?root:demo_dir ()
              else Termanil_backend.Config.load ?path ()
            in
            Termanil_backend.execute config request
      in
      print_string (Termanil_protocol.response result);
      Ok ()
  with
  | Dooit.Common.Error s | Failure s ->
      if init then Error s
      else (
        print_string (Termanil_protocol.response (Error s));
        Ok ())
  | _ ->
      if init then Error "Cannot create termanil config"
      else (
        print_string
          (Termanil_protocol.response
             (Error "Cannot load termanil configuration"));
        Ok ())

let () =
  let open Cmdliner in
  let config =
    Arg.(value & opt (some string) None & info [ "config" ] ~docv:"FILE")
  in
  let init = Arg.(value & flag & info [ "init-config" ]) in
  let check = Arg.(value & flag & info [ "check" ]) in
  let demo = Arg.(value & flag & info [ "demo" ]) in
  let demo_dir =
    Arg.(value & opt (some string) None & info [ "demo-dir" ] ~docv:"DIR")
  in
  let cmd =
    Cmd.v
      (Cmd.info "termanil-worker" ~doc:"Native backend for termanil")
      Term.(
        term_result
          (const (fun p i c d r ->
               Result.map_error (fun s -> `Msg s) (run p i c d r))
          $ config $ init $ check $ demo $ demo_dir))
  in
  exit (Cmd.eval cmd)
