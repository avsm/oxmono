let check name condition = if not condition then failwith name
let ok = function Ok value -> value | Error message -> failwith message
let error = function Error _ -> true | Ok _ -> false

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let settings : Bushel_config.http_embeddings =
  {
    url = "https://embeddings.example/v1/embeddings";
    model = "test-embedding";
    dimensions = 768;
    api_key_file = None;
  }

let validate_http settings =
  Bushel_config.validate_embeddings (Bushel_config.Http settings)

let configure_http ?file settings =
  Bushel_config.configure_embeddings ?file (Bushel_config.Http settings)

let http = function
  | Some (Bushel_config.Http settings) -> settings
  | _ -> failwith "expected HTTP configuration"

let mlx = function
  | Some (Bushel_config.Mlx settings) -> settings
  | _ -> failwith "expected MLX configuration"

let native : Bushel_config.mlx_embeddings =
  {
    model_dir = "models/qwen-embedding";
    dimensions = 1024;
    max_tokens = 8192;
    query_instruction = Bushel_config.default_query_instruction;
  }

let save path text =
  Out_channel.with_open_bin path (fun out -> output_string out text)

let read path = In_channel.with_open_bin path In_channel.input_all

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Sys.readdir path
    |> Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path)
  else Unix.unlink path

let rec ordered = function
  | Tomlt.Toml.Table fields ->
      Tomlt.Toml.Table
        (List.map (fun (key, value) -> (key, ordered value)) fields
        |> List.sort (fun (a, _) (b, _) -> String.compare a b))
  | Tomlt.Toml.Array values -> Tomlt.Toml.Array (List.map ordered values)
  | value -> value

let without_embeddings text =
  match Tomlt_bytesrw.of_string text with
  | Ok (Tomlt.Toml.Table fields) ->
      ordered (Tomlt.Toml.Table (List.remove_assoc "embeddings" fields))
  | _ -> failwith "invalid test document"

let test_validation () =
  check "invalid named secret is a configuration error"
    (error (validate_http { settings with api_key_file = Some "sekrit:../invalid" }));
  check "missing section defaults to None"
    ((ok (Bushel_config.of_string "")).embeddings = None);
  check "default has no embeddings"
    ((Bushel_config.default ()).embeddings = None);
  check "explicit HTTP backend accepted"
    (http
       (ok
          (Bushel_config.of_string
             "[embeddings]\n\
              backend='http'\n\
              url='https://example.org/'\n\
              model='test'\n\
              dimensions=1\n"))
         .embeddings
    = {
        settings with
        url = "https://example.org/";
        model = "test";
        dimensions = 1;
      });
  List.iter
    (fun url -> ignore (ok (validate_http { settings with url })))
    [
      "http://127.0.0.1:11434/v1/embeddings";
      "https://embeddings.example/v1/embeddings";
      "https://[::1]:8443/embeddings";
      "HTTPS://EMBEDDINGS.EXAMPLE/v1/embeddings";
    ];
  List.iter
    (fun url ->
      check "bad endpoint rejected"
        (error (validate_http { settings with url })))
    [
      "";
      "/v1/embeddings";
      "ftp://embeddings.example/";
      "https:///embeddings";
      "https://";
      "https:embeddings.example";
      "https://user:do-not-print-this@embeddings.example/";
      "https://embeddings.example/?key=do-not-print-this";
      "https://embeddings.example/?";
      "https://embeddings.example/#";
      "https://embeddings.example/#do-not-print-this";
      "https://embeddings.example:abc/";
      "https://embeddings.example:99999/";
      "https://embeddings.example/%zz";
      "https://embeddings.example/a b";
      "https://embeddings.example/\n";
    ];
  List.iter
    (fun dimensions ->
      check "bad dimensions rejected"
        (error (validate_http { settings with dimensions })))
    [ min_int; -1; 0; 4097; max_int ];
  List.iter
    (fun dimensions -> ignore (ok (validate_http { settings with dimensions })))
    [ 1; 4096 ];
  check "blank model rejected"
    (error (validate_http { settings with model = " \t" }));
  check "HTTP key path rejected"
    (error
       (validate_http
          {
            settings with
            url = "http://localhost:8080/";
            api_key_file = Some "/missing/key";
          }));
  List.iter
    (fun document ->
      check "partial or invalid decoded config rejected"
        (error (Bushel_config.of_string document)))
    [
      "[embeddings]\n";
      "[embeddings]\nurl='https://example.org/'\n";
      "[embeddings]\nurl='https://example.org/'\nmodel='test'\n";
      "[embeddings]\nurl='https://example.org/'\nmodel='test'\ndimensions=0\n";
      "[embeddings]\n\
       url='http://localhost/'\n\
       model='test'\n\
       dimensions=1\n\
       api_key_file='missing'\n";
      "[embeddings]\nurl='https://example.org/?'\nmodel='test'\ndimensions=1\n";
      "[embeddings]\n\
       url='https://example.org/'\n\
       model='test'\n\
       dimensions=1\n\
       api_key_file=''\n";
    ]

let test_native_validation () =
  let minimal =
    "[embeddings]\n\
     backend='mlx'\n\
     model_dir='models/qwen-embedding'\n\
     dimensions=1024\n"
  in
  check "MLX defaults and relative model path"
    (mlx (ok (Bushel_config.of_string minimal)).embeddings = native);
  List.iter
    (fun document ->
      check "invalid MLX settings rejected"
        (error (Bushel_config.of_string document)))
    [
      "[embeddings]\nbackend='mlx'\n";
      "[embeddings]\nbackend='mlx'\nmodel_dir='model'\n";
      "[embeddings]\nbackend='mlx'\ndimensions=32\n";
      "[embeddings]\n\
       backend='unknown'\n\
       url='https://example.org/'\n\
       model='test'\n\
       dimensions=32\n";
      "[embeddings]\nmodel_dir='model'\ndimensions=32\n";
      minimal ^ "max_tokens='8192'\n";
      minimal ^ "query_instruction=42\n";
    ];
  let validate e = Bushel_config.validate_embeddings (Bushel_config.Mlx e) in
  List.iter
    (fun dimensions ->
      check "MLX dimensions rejected"
        (error (validate { native with dimensions })))
    [ min_int; 0; 31; 4097; max_int ];
  List.iter
    (fun dimensions -> ignore (ok (validate { native with dimensions })))
    [ 32; 4096 ];
  List.iter
    (fun max_tokens ->
      check "MLX token limit rejected"
        (error (validate { native with max_tokens })))
    [ min_int; 0; 31; 32769; max_int ];
  List.iter
    (fun max_tokens -> ignore (ok (validate { native with max_tokens })))
    [ 32; 32768 ];
  List.iter
    (fun model_dir ->
      check "MLX model path rejected"
        (error (validate { native with model_dir })))
    [ ""; " "; "a\000b" ];
  List.iter
    (fun query_instruction ->
      check "invalid query instruction rejected"
        (error (validate { native with query_instruction })))
    [ "a\000b"; String.make 8193 'a' ];
  ignore
    (ok (validate { native with query_instruction = String.make 8192 'a' }));
  check "decoded overlong instruction rejected"
    (error
       (Bushel_config.of_string
          (minimal ^ "query_instruction='" ^ String.make 8193 'a' ^ "'\n")));
  check "explicit empty instruction retained"
    ((mlx
        (ok (Bushel_config.of_string (minimal ^ "query_instruction=''\n")))
          .embeddings)
       .query_instruction = "");
  check "MLX validation also runs on decoded settings"
    (error (Bushel_config.of_string (minimal ^ "max_tokens=32769\n")))

let test_native_files dir =
  let file = Filename.concat dir "mlx.toml" in
  save file
    "# keep\n\
     custom='value'\n\
     [embeddings]\n\
     url='https://example.org/'\n\
     model='old'\n\
     dimensions=1\n\
     api_key_file='secret-path'\n\
     custom_setting='keep'\n";
  ignore
    (ok (Bushel_config.configure_embeddings ~file (Bushel_config.Mlx native)));
  let content = read file in
  let expected =
    { native with model_dir = Filename.concat (Sys.getcwd ()) native.model_dir }
  in
  check "configure anchors model directory"
    (mlx (ok (Bushel_config.of_string content)).embeddings = expected);
  check "MLX file round trip"
    (mlx (ok (Bushel_config.load_file file)).embeddings = expected);
  check "switch to MLX preserves unrelated and unknown values"
    (contains content "# keep"
    && contains content "custom='value'"
    && contains content "custom_setting = \"keep\"");
  check "switch to MLX clears HTTP fields"
    ((not (contains content "api_key_file"))
    && (not (contains content "url ="))
    && (not (contains content "model ="))
    && not (contains content "secret-path"));
  ignore (ok (configure_http ~file settings));
  let content = read file in
  check "switch to HTTP clears native fields"
    ((not (contains content "backend"))
    && (not (contains content "model_dir"))
    && (not (contains content "max_tokens"))
    && not (contains content "query_instruction"));
  check "HTTP remains backward compatible after switch"
    (http (ok (Bushel_config.load_file file)).embeddings = settings);
  let handwritten =
    "[embeddings]\nbackend='mlx'\nmodel_dir='models/local'\ndimensions=32\n"
  in
  save file handwritten;
  check "loaded model path resolves beside config"
    ((mlx (ok (Bushel_config.load_file file)).embeddings).model_dir
    = Filename.concat dir "models/local");
  let cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir cwd)
    (fun () ->
      Sys.chdir dir;
      check "relative config also resolves model path absolutely"
        ((mlx (ok (Bushel_config.load_file "mlx.toml")).embeddings).model_dir
        = Filename.concat (Sys.getcwd ()) "models/local"));
  let tilde = { native with model_dir = "~/models/local" } in
  ignore
    (ok (Bushel_config.configure_embeddings ~file (Bushel_config.Mlx tilde)));
  check "model home path expands"
    ((mlx (ok (Bushel_config.load_file file)).embeddings).model_dir
    = Bushel_config.expand_path tilde.model_dir)

let test_files dir =
  let file = Filename.concat dir "new/nested/config.toml" in
  check "new path returned" (ok (configure_http ~file settings) = file);
  check "new config round trips"
    ((ok (Bushel_config.load_file file)).embeddings
   = Some (Bushel_config.Http settings));
  check "new file is private" ((Unix.stat file).Unix.st_perm land 0o777 = 0o600);
  check "new directories are private"
    ((Unix.stat (Filename.dirname file)).Unix.st_perm land 0o777 = 0o700);
  let key = Filename.concat dir "key" in
  save key "do-not-print-this-secret-value\n";
  let with_key = { settings with api_key_file = Some key } in
  ignore (ok (configure_http ~file with_key));
  let content = read file in
  check "only the key path is stored" (contains content key);
  check "key contents never stored"
    (not (contains content "do-not-print-this-secret-value"));
  check "key configuration round trips"
    ((ok (Bushel_config.load_file file)).embeddings
   = Some (Bushel_config.Http with_key));
  let tilde = { settings with api_key_file = Some "~/.missing-bushel-key" } in
  ignore (ok (configure_http ~file tilde));
  check "absent key is allowed and expanded"
    ((http (ok (Bushel_config.load_file file)).embeddings).api_key_file
    = Some (Bushel_config.expand_path "~/.missing-bushel-key"));
  let relative_key = "relative-bushel-key" in
  ignore
    (ok
       (configure_http ~file { settings with api_key_file = Some relative_key }));
  let expected_key = Filename.concat (Sys.getcwd ()) relative_key in
  check "configure anchors relative key to invocation directory"
    ((http (ok (Bushel_config.of_string (read file))).embeddings).api_key_file
   = Some expected_key);
  let handwritten =
    "[data]\n\
     local_dir='./data'\n\
     [embeddings]\n\
     url='https://example.org/'\n\
     model='test'\n\
     dimensions=1\n\
     api_key_file='keys/token'\n"
  in
  save file handwritten;
  check "of_string leaves relative key unanchored"
    ((http (ok (Bushel_config.of_string handwritten)).embeddings).api_key_file
   = Some "keys/token");
  let loaded = ok (Bushel_config.load_file file) in
  check "load_file anchors key to config directory"
    ((http loaded.embeddings).api_key_file
    = Some (Filename.concat (Filename.dirname file) "keys/token"));
  check "data directory semantics unchanged" (loaded.data_dir = "./data");
  let cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir cwd)
    (fun () ->
      Sys.chdir dir;
      let loaded = ok (Bushel_config.load_file "new/nested/config.toml") in
      check "relative config path also yields absolute key path"
        ((http loaded.embeddings).api_key_file
        = Some (Filename.concat (Sys.getcwd ()) "new/nested/keys/token")));
  let original =
    {|# Keep this comment and spelling.
title = "custom title"
note = """
[embeddings]
url = "this is a string, not a section"
"""

[data]
local_dir = "/custom/data"

[unknown]
values = [1, "two", true]
metadata = { child = { enabled = true }, other = 3 }

[embeddings]
url = "https://old.example/embeddings"
model = "old"
dimensions = 12
api_key_file = "/old/key"
custom_setting = "keep it"

# Keep the sync comment too.
[sync]
remote = "ssh://custom.example/data"
branch = "custom-branch"
|}
  in
  save file original;
  Unix.chmod file 0o640;
  ignore (ok (configure_http ~file settings));
  let content = read file in
  check "unrelated and unknown values preserved"
    (Tomlt.Toml.equal
       (without_embeddings original)
       (without_embeddings content));
  check "unrelated comments preserved"
    (contains content "# Keep this comment and spelling."
    && contains content "# Keep the sync comment too.");
  check "unknown embedding values preserved"
    (contains content "custom_setting = \"keep it\"");
  check "old key path removed" (not (contains content "/old/key"));
  check "existing file permissions preserved"
    ((Unix.stat file).Unix.st_perm land 0o777 = 0o640);
  check "configured section has new values"
    ((ok (Bushel_config.load_file file)).embeddings
   = Some (Bushel_config.Http settings));
  let unusual =
    {|title = "keep"
embeddings = {url="https://old.example/", model="old", dimensions=1}
[other]
value = 17
|}
  in
  save file unusual;
  ignore (ok (configure_http ~file settings));
  check "inline section preserves other values"
    (Tomlt.Toml.equal
       (without_embeddings unusual)
       (without_embeddings (read file)));
  save file "[broken\n";
  check "invalid existing TOML refused" (error (configure_http ~file settings));
  check "invalid file untouched" (read file = "[broken\n");
  check "directory destination refused"
    (error (configure_http ~file:dir settings));
  let blocker = Filename.concat dir "parent-is-a-file" in
  save blocker "unchanged";
  check "write failure is returned"
    (error
       (configure_http ~file:(Filename.concat blocker "config.toml") settings));
  check "write failure keeps existing data" (read blocker = "unchanged");
  check "no temporary files remain"
    (not
       (Array.exists
          (String.starts_with ~prefix:".bushel-config-")
          (Sys.readdir (Filename.dirname file))))

let hold_config_lock file =
  let fd = Unix.openfile (file ^ ".lock") [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      output_char stdout 'r';
      flush stdout;
      try ignore (input_char stdin) with End_of_file -> ())

let test_config_lock dir =
  let file = Filename.concat dir "locked.toml" in
  save file "custom=42\n";
  let ready_read, ready_write = Unix.pipe ~cloexec:true () in
  let release_read, release_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process Sys.executable_name
      [| Sys.executable_name; "--hold-config-lock"; file |]
      release_read ready_write Unix.stderr
  in
  Unix.close ready_write;
  Unix.close release_read;
  let rec wait () =
    try snd (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close ready_read;
      Unix.close release_write;
      (try Unix.kill pid Sys.sigkill
       with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      ignore (wait ()))
    (fun () ->
      let readable, _, _ = Unix.select [ ready_read ] [] [] 10. in
      check "config lock child starts" (readable <> []);
      let signal = Bytes.create 1 in
      check "config lock child acquired lock"
        (Unix.read ready_read signal 0 1 = 1 && Bytes.get signal 0 = 'r');
      check "overlapping config update reports contention"
        (configure_http ~file settings
        = Error "Another process is updating this configuration");
      check "overlapping config update leaves file unchanged"
        (read file = "custom=42\n"));
  ignore (ok (Bushel_config.configure_image_embeddings ~file native));
  ignore (ok (configure_http ~file settings));
  let config = ok (Bushel_config.load_file file) in
  check "updates succeed after lock release and preserve both backends"
    (config.embeddings = Some (Bushel_config.Http settings)
    && Option.is_some config.image_embeddings)

let run () =
  test_validation ();
  test_native_validation ();
  let dir = Filename.temp_dir "bushel-embeddings-config" "" in
  Fun.protect
    ~finally:(fun () -> remove dir)
    (fun () ->
      test_files dir;
      test_native_files dir;
      test_config_lock dir;
      let file = Filename.concat dir "named-secret.toml" in
      let reference = "sekrit:embedding/main" in
      ignore
        (ok
           (configure_http ~file
              { settings with api_key_file = Some reference }));
      let loaded = http (ok (Bushel_config.load_file file)).embeddings in
      check "named secret reference survives path resolution"
        (loaded.api_key_file = Some reference));
  print_endline
    "Bushel embeddings configuration, preservation and validation passed"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--hold-config-lock"; file ] -> hold_config_lock file
  | _ -> run ()
