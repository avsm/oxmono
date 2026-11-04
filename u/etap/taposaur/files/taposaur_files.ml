let ( / ) = Eio.Path.( / )
let fd file = match Eio_unix.Resource.fd_opt file with
  | Some fd -> fd | None -> failwith "Durable writes require Unix files"
let sync_dir dir = Eio.Path.with_open_in dir (fun file ->
  Eio_unix.Fd.use_exn "sync directory" (fd file) (fun fd ->
    Eio_unix.run_in_systhread (fun () -> Unix.fsync fd)))
let exists path = Eio.Path.kind ~follow:false path <> `Not_found
let load ?(max_bytes = 16 * 1024 * 1024) path =
  Eio.Path.with_open_in ~follow:false path (fun file ->
    Eio.Buf_read.(take_all (of_flow ~max_size:max_bytes file)))
let mode_of dir name default =
  match Eio.Path.stat ~follow:false (dir / name) with
  | { kind = `Regular_file; perm; _ } -> perm
  | _ -> default
  | exception Eio.Io _ -> default
let atomic ?(perm = 0o600) dir name value =
  let tmp = dir / (name ^ ".tmp") in
  Eio.Path.with_open_out ~follow:false ~create:(`Or_truncate perm) tmp
    (fun file -> Eio.Flow.copy_string value file; Eio.File.sync file);
  Eio.Path.rename tmp (dir / name);
  sync_dir dir
let digest file =
  let ctx = ref Digestif.SHA256.empty and bytes = ref 0 in
  let buffer = Cstruct.create 65536 in
  (try while true do
    let n = Eio.Flow.single_read file buffer in
    ctx := Digestif.SHA256.feed_string !ctx (Cstruct.to_string ~len:n buffer);
    bytes := !bytes + n
  done with End_of_file -> ());
  Digestif.SHA256.(to_hex (get !ctx)), !bytes
let digest_path path = Eio.Path.with_open_in ~follow:false path digest
