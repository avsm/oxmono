(* Scan and fold a Maildir of 100,000 messages in a temporary directory.
   One message in seven is in cur with the S flag, the rest in new. *)

let count = 100_000
let body = "From: bench@example.test\r\nSubject: bench\r\n\r\nBody\r\n"

let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let message root i =
  let name = Printf.sprintf "im-%032x" i in
  let path =
    if i mod 7 = 0 then Filename.concat (Filename.concat root "cur")
        (name ^ ":2,S")
    else Filename.concat (Filename.concat root "new") name in
  Out_channel.with_open_bin path (fun oc -> output_string oc body)

let () =
  let root = Filename.temp_file "imap-bench-maildir-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) @@ fun () ->
  Eio_main.run @@ fun env ->
  let local = function
    | Ok x -> x
    | Error e -> failwith (Format.asprintf "%a" Maildir.pp_error e) in
  let m = local (Maildir.open_dir Eio.Path.(Eio.Stdenv.fs env / root)) in
  for i = 0 to count - 1 do message root i done;
  let scanned = Bench_measure.run "Maildir.scan 100000 messages" (fun () ->
    List.length (local (Maildir.scan m))) in
  let folded = Bench_measure.run "Maildir.fold 100000 messages" (fun () ->
    local (Maildir.fold m ~init:0 ~f:(fun n _ -> n + 1))) in
  if scanned <> count || folded <> count then failwith "occurrence count"
