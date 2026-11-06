(* SPDX-License-Identifier: ISC *)

let with_dir env prefix f =
  let name = Filename.temp_file prefix "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect
    ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
    (fun () -> f name Eio.Path.(env#fs / name))

let has s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0
