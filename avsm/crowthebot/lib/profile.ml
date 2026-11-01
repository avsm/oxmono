let filename = "crowthebot.toml"
let legacy_filename = "crowthebot.json"

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
  try
    Eio_unix.run_in_systhread (fun () ->
        Sekrit.read_reference ~max_bytes:16384 path)
  with Sekrit.Error message -> invalid_arg message

let load dir =
  let path = Eio.Path.(dir / filename) in
  let decode path =
    private_file (Eio.Path.native_exn path);
    if (Unix.stat (Eio.Path.native_exn path)).st_size > 65536 then
      invalid_arg "configuration is too large";
    match Config.of_toml_string (Eio.Path.load path) with
    | Error message -> failwith ("invalid crowthebot.toml configuration: " ^ message)
    | Ok config -> config
  in
  let config =
    if Sys.file_exists (Eio.Path.native_exn path) then decode path
    else
      let legacy = Eio.Path.(dir / legacy_filename) in
      private_file (Eio.Path.native_exn legacy);
      match Jsont_bytesrw.decode_string Config.jsont (Eio.Path.load legacy) with
      | Error _ -> failwith "invalid legacy crowthebot.json configuration"
      | Ok config -> config
  in
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
  let encoded = Tomlt_bytesrw.encode_string Config.tomlt config in
  let path = Eio.Path.(dir / filename) in
  if Sys.file_exists (Eio.Path.native_exn path) then
    invalid_arg
      "profile already initialized. Edit crowthebot.toml to configure it";
  ignore (database ~sw dir ~admin:config.admin);
  Eio.Path.save ~create:(`Exclusive 0o600) path (encoded ^ "\n")
