module M = Apple_mlx_embeddings.Model

let check name condition = if not condition then failwith name

let contains text part =
  let rec loop offset =
    offset + String.length part <= String.length text
    && (String.sub text offset (String.length part) = part || loop (offset + 1))
  in
  loop 0

let rec mkdir path =
  if not (Sys.file_exists path) then (
    mkdir (Filename.dirname path);
    Unix.mkdir path 0o700)

let rec remove path =
  match Unix.lstat path with
  | stat when stat.st_kind = Unix.S_DIR ->
      Array.iter
        (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let sparse path size =
  mkdir (Filename.dirname path);
  let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.LargeFile.ftruncate fd size)

let snapshot cache (model : M.t) =
  Filename.concat cache
    ("models--"
    ^ String.concat "--" (String.split_on_char '/' model.repo)
    ^ "/snapshots/" ^ model.revision)

let populate cache (model : M.t) =
  List.iter
    (fun (name, size) ->
      sparse (Filename.concat (snapshot cache model) name) size)
    model.files

let option name args =
  let rec find = function
    | key :: value :: _ when key = name -> value
    | _ :: rest -> find rest
    | [] -> failwith ("Missing fake downloader argument: " ^ name)
  in
  find args

let wait_forever () =
  while true do
    Unix.sleep 3600
  done

let fake backend =
  let args = Array.to_list Sys.argv in
  let log = Sys.getenv "APPLE_MLX_MODEL_TEST_LOG" in
  let oc = open_out_gen [ Open_creat; Open_append; Open_binary ] 0o600 log in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> Marshal.to_channel oc args []);
  Printf.printf "fake-%s-stdout\n%!" backend;
  Printf.eprintf "fake-%s-stderr\n%!" backend;
  let mode = Sys.getenv "APPLE_MLX_MODEL_TEST_MODE" in
  if
    mode = "both-fail"
    || (mode = "hf-fail" || mode = "uvx-child" || mode = "uvx-child-exit")
       && backend = "hf"
  then exit 17;
  if mode = "wait" then wait_forever ();
  if mode = "uvx-child" || mode = "uvx-child-exit" then (
    let self = Sys.executable_name in
    let pid =
      Unix.create_process self [| self; "--wait-child" |] Unix.stdin Unix.stdout
        Unix.stderr
    in
    Out_channel.with_open_bin (Sys.getenv "APPLE_MLX_MODEL_TEST_CHILD_PID")
      (fun oc -> Printf.fprintf oc "%d\n" pid);
    if mode = "uvx-child-exit" then exit 0 else wait_forever ());
  if mode = "incomplete" then exit 0;
  let rec repository = function
    | "download" :: repo :: files -> (repo, files)
    | _ :: rest -> repository rest
    | [] -> failwith "Fake downloader received no download command"
  in
  let repo, rest = repository args in
  let rec filenames = function
    | name :: _ when String.starts_with ~prefix:"--" name -> []
    | name :: rest -> name :: filenames rest
    | [] -> []
  in
  let model = List.find (fun (m : M.t) -> m.repo = repo) M.all in
  check "fake downloader revision is pinned"
    (option "--revision" args = model.revision);
  let cache = option "--cache-dir" args in
  List.iter
    (fun name ->
      let path = Filename.concat (snapshot cache model) name in
      let regular_pointer =
        match Unix.lstat path with
        | stat -> stat.st_kind = Unix.S_REG
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
      in
      (* HF forced fetches replace blobs but retain regular snapshot copies.
         A later ordinary fetch repairs the pointer after it has been removed. *)
      if List.mem "--force-download" args && regular_pointer then ()
      else sparse path (List.assoc name model.files))
    (filenames rest)

let calls path =
  if not (Sys.file_exists path) then []
  else
    In_channel.with_open_bin path (fun ic ->
        let rec read acc =
          match (Marshal.from_channel ic : string list) with
          | args -> read (args :: acc)
          | exception End_of_file -> List.rev acc
        in
        read [])

let executable args = Filename.basename (List.hd args)

let expected_args ?files ~backend ~cache ~force (model : M.t) =
  [ backend ]
  @ (if backend = "uvx" then [ "hf" ] else [])
  @ [ "download"; model.repo ]
  @ Option.value ~default:(List.map fst model.files) files
  @ [
      "--revision"; model.revision; "--cache-dir"; cache; "--repo-type"; "model";
    ]
  @ if force then [ "--force-download" ] else []

let cache_paths root =
  let variables =
    [
      "HF_HUB_CACHE";
      "HUGGINGFACE_HUB_CACHE";
      "HF_HOME";
      "XDG_CACHE_HOME";
      "HOME";
    ]
  in
  List.iter (fun name -> Unix.putenv name "") variables;
  let home = Filename.concat root "home" in
  Unix.putenv "HOME" home;
  check "HOME cache fallback"
    (M.cache_dir () = Filename.concat home ".cache/huggingface/hub");
  let xdg = Filename.concat root "xdg" in
  Unix.putenv "XDG_CACHE_HOME" xdg;
  check "XDG cache precedes HOME"
    (M.cache_dir () = Filename.concat xdg "huggingface/hub");
  let hf_home = Filename.concat root "hf-home" in
  Unix.putenv "HF_HOME" hf_home;
  check "HF_HOME precedes XDG" (M.cache_dir () = Filename.concat hf_home "hub");
  let legacy = Filename.concat root "legacy" in
  Unix.putenv "HUGGINGFACE_HUB_CACHE" legacy;
  check "legacy hub cache precedes HF_HOME" (M.cache_dir () = legacy);
  let current = Filename.concat root "current" in
  Unix.putenv "HF_HUB_CACHE" current;
  check "HF_HUB_CACHE has highest priority" (M.cache_dir () = current);
  Unix.putenv "HF_HUB_CACHE" "";
  check "empty overrides are ignored" (M.cache_dir () = legacy);
  Unix.putenv "HF_HUB_CACHE" "relative-cache";
  check "relative environment paths resolve against cwd"
    (M.cache_dir () = Filename.concat (Sys.getcwd ()) "relative-cache");
  Unix.putenv "HF_HUB_CACHE" "~/hub";
  check "tilde prefix expands HOME" (M.cache_dir () = Filename.concat home "hub");
  Unix.putenv "HF_HUB_CACHE" "~";
  check "bare tilde expands HOME" (M.cache_dir () = home)

exception Cancel_download

let run () =
  let root = Filename.temp_file "mlx-model-test" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let saved =
    List.map
      (fun name -> (name, Sys.getenv_opt name))
      [
        "PATH";
        "HOME";
        "XDG_CACHE_HOME";
        "HF_HOME";
        "HF_HUB_CACHE";
        "HUGGINGFACE_HUB_CACHE";
        "APPLE_MLX_MODEL_TEST_LOG";
        "APPLE_MLX_MODEL_TEST_MODE";
        "APPLE_MLX_MODEL_TEST_CHILD_PID";
      ]
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, value) -> Unix.putenv name (Option.value ~default:"" value))
        saved;
      remove root)
    (fun () ->
      check "catalog has its default" (List.mem M.default M.all);
      List.iter
        (fun (model : M.t) ->
          check "catalog lookup by name" (M.find model.name = Some model);
          check "catalog lookup by repository" (M.find model.repo = Some model);
          check "catalog pins a full commit"
            (String.length model.revision = 40
            && String.for_all
                 (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                 model.revision);
          check "catalog records positive file lengths"
            (model.files <> []
            && List.for_all (fun (_, n) -> n > 0L) model.files))
        M.all;
      check "unknown model is not guessed" (M.find "unknown-model" = None);
      cache_paths root;
      let model = List.hd M.all in
      let cache = Filename.concat root "hub" in
      let path = snapshot cache model in
      check "snapshot path matches Hugging Face layout"
        (M.dir ~cache_dir:cache model = path);
      check "explicit relative cache resolves against cwd"
        (M.dir ~cache_dir:"relative-cache" model
        = snapshot (Filename.concat (Sys.getcwd ()) "relative-cache") model);
      check "missing cache is incomplete"
        (not (M.present ~cache_dir:cache model));
      populate cache model;
      check "sparse files with exact lengths are complete"
        (M.present ~cache_dir:cache model);
      let filename, size = List.hd model.files in
      let file = Filename.concat path filename in
      let blob = Filename.concat root "blob" in
      Unix.rename file blob;
      Unix.symlink blob file;
      check "regular blob symlinks count as complete"
        (M.present ~cache_dir:cache model);
      Sys.remove blob;
      check "dangling blob symlink is incomplete"
        (not (M.present ~cache_dir:cache model));
      Sys.remove file;
      Unix.mkdir file 0o700;
      check "directory is not a complete model file"
        (not (M.present ~cache_dir:cache model));
      Unix.rmdir file;
      sparse file size;
      let bin = Filename.concat root "bin" in
      Unix.mkdir bin 0o700;
      let log = Filename.concat root "calls" in
      Unix.putenv "PATH" bin;
      Unix.putenv "APPLE_MLX_MODEL_TEST_LOG" log;
      Unix.putenv "APPLE_MLX_MODEL_TEST_MODE" "complete";
      let self = Unix.realpath Sys.executable_name in
      let hf = Filename.concat bin "hf" and uvx = Filename.concat bin "uvx" in
      let install path = Unix.symlink self path in
      Eio_main.run (fun env ->
          let proc = Eio.Stdenv.process_mgr env in
          let clock = Eio.Stdenv.clock env in
          let output = Buffer.create 256 in
          let stderr = Eio.Flow.buffer_sink output in
          let ensure () = M.ensure ~proc ~stderr ~cache_dir:cache model in
          check "complete cache needs no downloader executable"
            (ensure () = path && calls log = [] && Buffer.length output = 0);
          let reset mode =
            remove log;
            Buffer.clear output;
            Unix.putenv "APPLE_MLX_MODEL_TEST_MODE" mode
          in
          install hf;
          install uvx;
          remove path;
          reset "complete";
          check "cold download requests every pinned file" (ensure () = path);
          check "cold download arguments"
            (calls log
            = [ expected_args ~backend:"hf" ~cache ~force:false model ]);
          Sys.remove file;
          Unix.mkdir file 0o700;
          reset "complete";
          check "invalid directory refuses download without removing it"
            (try
               ignore (ensure ());
               false
             with Invalid_argument _ | Failure _ ->
               (Unix.lstat file).st_kind = Unix.S_DIR && calls log = []);
          Unix.rmdir file;
          sparse file size;
          let shard_name, _ =
            List.find
              (fun (name, _) -> String.ends_with ~suffix:".safetensors" name)
              model.files
          in
          let shard = Filename.concat path shard_name in
          Sys.remove shard;
          reset "complete";
          check "missing weight file resumes download" (ensure () = path);
          check "hf requests only missing weights without forced redownload"
            (calls log
            = [
                expected_args ~files:[ shard_name ] ~backend:"hf" ~cache
                  ~force:false model;
              ]);
          check "both downloader output streams use the supplied sink"
            (contains (Buffer.contents output) "fake-hf-stdout"
            && contains (Buffer.contents output) "fake-hf-stderr");
          sparse file (Int64.pred size);
          check "wrong length is incomplete"
            (not (M.present ~cache_dir:cache model));
          reset "repair";
          ignore (ensure ());
          check "corrupt cache forces repair"
            (calls log
             = [
                 expected_args ~files:[ filename ] ~backend:"hf" ~cache
                   ~force:true model;
                 expected_args ~files:[ filename ] ~backend:"hf" ~cache
                   ~force:false model;
               ]
            && M.present ~cache_dir:cache model);
          sparse file (Int64.pred size);
          reset "both-fail";
          check "failed forced fetch preserves the invalid snapshot marker"
            (try
               ignore (ensure ());
               false
             with Failure _ ->
               (Unix.LargeFile.stat file).st_size = Int64.pred size);
          check "failed repair tries forced fetch through both tools"
            (calls log
            = [
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:true model;
                expected_args ~files:[ filename ] ~backend:"uvx" ~cache
                  ~force:true model;
              ]);
          reset "repair";
          ignore (ensure ());
          check "interrupted repair retries force before relinking"
            (calls log
            = [
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:true model;
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:false model;
              ]);
          Unix.rename file blob;
          sparse blob (Int64.pred size);
          Unix.symlink blob file;
          reset "repair";
          ignore (ensure ());
          check "forced blob repair retains a valid snapshot symlink"
            (calls log
             = [
                 expected_args ~files:[ filename ] ~backend:"hf" ~cache
                   ~force:true model;
               ]
            && (Unix.lstat file).st_kind = Unix.S_LNK
            && (Unix.LargeFile.stat blob).st_size = size);
          Sys.remove file;
          Unix.rename blob file;
          sparse file (Int64.pred size);
          Sys.remove shard;
          reset "repair";
          ignore (ensure ());
          check "invalid and missing files use separate download modes"
            (calls log
            = [
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:true model;
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:false model;
                expected_args ~files:[ shard_name ] ~backend:"hf" ~cache
                  ~force:false model;
              ]);
          Sys.remove file;
          Unix.symlink (Filename.concat root "missing-blob") file;
          reset "incomplete";
          check "successful but incomplete download reports failure"
            (try
               ignore (ensure ());
               false
             with Failure message -> contains message "incomplete");
          check "dangling cache link does not force or invoke fallback"
            (calls log
            = [
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:false model;
              ]);
          Sys.remove file;
          reset "hf-fail";
          check "hf failure falls back to uvx" (ensure () = path);
          check "fallback preserves the pinned invocation"
            (calls log
            = [
                expected_args ~files:[ filename ] ~backend:"hf" ~cache
                  ~force:false model;
                expected_args ~files:[ filename ] ~backend:"uvx" ~cache
                  ~force:false model;
              ]);
          Sys.remove file;
          Sys.remove hf;
          reset "complete";
          check "absent hf falls back to uvx" (ensure () = path);
          check "only uvx ran when hf is absent"
            (calls log
            = [
                expected_args ~files:[ filename ] ~backend:"uvx" ~cache
                  ~force:false model;
              ]);
          install hf;
          Sys.remove file;
          reset "both-fail";
          check "two failures provide actionable installation guidance"
            (try
               ignore (ensure ());
               false
             with Failure message -> contains message "Install hf or uv");
          check "failed download attempts exactly the two tools"
            (List.map executable (calls log) = [ "hf"; "uvx" ]);
          reset "wait";
          check "active download cancellation propagates"
            (try
               Eio.Fiber.first
                 (fun () -> ignore (ensure ()))
                 (fun () ->
                   Eio.Time.with_timeout_exn clock 10. (fun () ->
                       while calls log = [] do
                         Eio.Time.sleep clock 0.01
                       done);
                   raise Cancel_download);
               false
             with Cancel_download -> true);
          check "cancelled hf does not invoke uvx"
            (List.map executable (calls log) = [ "hf" ]);
          reset "complete";
          check "download recovers after cancellation" (ensure () = path);
          Sys.remove file;
          reset "uvx-child";
          let pid_file = Filename.concat root "child-pid" in
          Unix.putenv "APPLE_MLX_MODEL_TEST_CHILD_PID" pid_file;
          let child = ref None in
          let read_pid () =
            if Sys.file_exists pid_file then
              try
                child :=
                  Some
                    (In_channel.with_open_bin pid_file In_channel.input_all
                    |> String.trim |> int_of_string)
              with Failure _ -> ()
          in
          Fun.protect
            ~finally:(fun () ->
              read_pid ();
              Option.iter
                (fun pid ->
                  try Unix.kill pid Sys.sigkill
                  with Unix.Unix_error (Unix.ESRCH, _, _) -> ())
                !child)
            (fun () ->
              check "uvx descendant cancellation propagates"
                (try
                   Eio.Fiber.first
                     (fun () -> ignore (ensure ()))
                     (fun () ->
                       Eio.Time.with_timeout_exn clock 10. (fun () ->
                           while !child = None do
                             read_pid ();
                             Eio.Time.sleep clock 0.01
                           done);
                       raise Cancel_download);
                   false
                 with Cancel_download -> true);
              check "fallback launched exactly hf and uvx"
                (List.map executable (calls log) = [ "hf"; "uvx" ]);
              let pid = Option.get !child in
              let alive () =
                try
                  Unix.kill pid 0;
                  true
                with Unix.Unix_error (Unix.ESRCH, _, _) -> false
              in
              check "cancelled uvx leaves no running descendant"
                (try
                   Eio.Time.with_timeout_exn clock 5. (fun () ->
                       while alive () do
                         Eio.Time.sleep clock 0.01
                       done);
                   true
                 with Eio.Time.Timeout -> false));
          reset "complete";
          check "download recovers after descendant cancellation"
            (ensure () = path);
          Sys.remove file;
          remove pid_file;
          child := None;
          reset "uvx-child-exit";
          Fun.protect
            ~finally:(fun () ->
              read_pid ();
              Option.iter
                (fun pid ->
                  try Unix.kill pid Sys.sigkill
                  with Unix.Unix_error (Unix.ESRCH, _, _) -> ())
                !child)
            (fun () ->
              check "exited uvx leader cannot leave output drain blocked"
                (try
                   Eio.Time.with_timeout_exn clock 10. (fun () ->
                       ignore (ensure ()));
                   false
                 with Failure message -> contains message "incomplete");
              read_pid ();
              let pid = Option.get !child in
              let alive () =
                try
                  Unix.kill pid 0;
                  true
                with Unix.Unix_error (Unix.ESRCH, _, _) -> false
              in
              check "exited uvx leader leaves no descendant"
                (try
                   Eio.Time.with_timeout_exn clock 5. (fun () ->
                       while alive () do
                         Eio.Time.sleep clock 0.01
                       done);
                   true
                 with Eio.Time.Timeout -> false))));
  print_endline
    "model catalog, cache repair, downloader fallback and cancellation passed"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--wait-child" then
    wait_forever ()
  else
    match Filename.basename Sys.argv.(0) with
    | "hf" -> fake "hf"
    | "uvx" -> fake "uvx"
    | _ -> run ()
