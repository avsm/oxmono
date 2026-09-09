let filename = "crowthebot.json"

let directory env profile =
  Matrix_client.Profile_store.validate_profile_name profile;
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let dir = Matrix_client.Profile_store.(dir (create ~xdg ~profile)) in
  let stat = Unix.lstat (Eio.Path.native_exn dir) in
  if
    stat.Unix.st_kind <> Unix.S_DIR
    || stat.st_uid <> Unix.getuid ()
    || stat.st_perm land 0o077 <> 0
  then invalid_arg "profile directory must be owned by you with mode 0700";
  dir

let with_lock dir f =
  let path = Eio.Path.native_exn Eio.Path.(dir / ".crowthebot.lock") in
  let fd =
    Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600
  in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  (try Unix.lockf fd Unix.F_TLOCK 0
   with Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
     failwith "this profile is already in use by crowthebot");
  f ()

let private_file path =
  let stat = Unix.lstat path in
  if
    stat.Unix.st_kind <> Unix.S_REG
    || stat.st_uid <> Unix.getuid ()
    || stat.st_perm land 0o077 <> 0
  then invalid_arg "secret and state files must be owned by you with mode 0600"

let read_secret path =
  private_file path;
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let size = in_channel_length ic in
  if size > 16384 then invalid_arg "secret file is too large";
  let value = really_input_string ic size in
  let value =
    if String.ends_with ~suffix:"\n" value then
      String.sub value 0 (String.length value - 1)
    else value
  in
  let value =
    if String.ends_with ~suffix:"\r" value then
      String.sub value 0 (String.length value - 1)
    else value
  in
  if String.contains value '\n' || String.contains value '\r' then
    invalid_arg "secret file must contain one line";
  if value = "" then invalid_arg "secret file is empty";
  value

let load dir =
  let path = Eio.Path.(dir / filename) in
  private_file (Eio.Path.native_exn path);
  if (Unix.stat (Eio.Path.native_exn path)).st_size > 65536 then
    invalid_arg "configuration is too large";
  match Jsont_bytesrw.decode_string Config.jsont (Eio.Path.load path) with
  | Error _ -> failwith "invalid crowthebot.json configuration"
  | Ok config ->
      let config = Config.upgrade config in
      Config.validate config;
      config

let database ~sw dir ~admin =
  let path = Eio.Path.(dir / "crowthebot.sqlite3") in
  let native = Eio.Path.native_exn path in
  if not (Sys.file_exists native) then
    Eio.Path.save ~create:(`Exclusive 0o600) path "";
  private_file native;
  Store.create
    (Sqlite3_eio.open_path ~sw ~uri:false ~busy_timeout:5000 path)
    ~admin

let init ~sw dir config =
  Config.validate config;
  let encoded =
    match
      Jsont_bytesrw.encode_string ~format:Jsont.Indent Config.jsont config
    with
    | Ok s -> s
    | Error _ -> failwith "cannot encode configuration"
  in
  let path = Eio.Path.(dir / filename) in
  if Sys.file_exists (Eio.Path.native_exn path) then
    invalid_arg
      "profile already initialized. Edit crowthebot.json to configure it";
  ignore (database ~sw dir ~admin:config.admin);
  Eio.Path.save ~create:(`Exclusive 0o600) path (encoded ^ "\n")
