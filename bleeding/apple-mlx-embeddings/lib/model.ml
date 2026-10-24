type t = {
  name : string;
  repo : string;
  revision : string;
  dimensions : int;
  files : (string * int64) list;
}

let checkpoint name size revision dimensions weights index config tokenizer
    tokenizer_config added_tokens =
  {
    name;
    repo = "mlx-community/Qwen3-Embedding-" ^ size ^ "-4bit-DWQ";
    revision;
    dimensions;
    files =
      [
        ("config.json", config);
        ("model.safetensors", weights);
        ("model.safetensors.index.json", index);
        ("tokenizer.json", tokenizer);
        ("tokenizer_config.json", tokenizer_config);
        ("special_tokens_map.json", 613L);
        ("added_tokens.json", added_tokens);
        ("merges.txt", 1671853L);
        ("vocab.json", 2776833L);
        ("generation_config.json", 117L);
      ];
  }

(* Revisions and lengths come from the Hub's versioned file manifests. Keep
   them together so a changed upstream branch cannot change an index identity. *)
let small =
  checkpoint "qwen3-0.6b" "0.6B" "6c3ae70858513f1a78e9cdca3cae330d9075cd2a" 1024
    335296756L 49770L 937L 11423705L 5404L 707L

let default =
  checkpoint "qwen3-4b" "4B" "b5d88f1fe49b50d2ac01b4692ca2d387f14f9c72" 2560
    2262632177L 63964L 937L 11422947L 4686L 605L

let large =
  checkpoint "qwen3-8b" "8B" "885642d6b98742ea03b77a1673579c92ca961efd" 4096
    4257149761L 63964L 938L 11422947L 4686L 605L

let all = [ small; default; large ]
let find name = List.find_opt (fun m -> m.name = name || m.repo = name) all

let vision_default =
  {
    name = "qwen3-vl-2b";
    repo = "mlx-community/Qwen3-VL-Embedding-2B-4bit";
    revision = "c713a965783416b9467b419f2d14ca6445847455";
    dimensions = 2048;
    files =
      [
        ("config.json", 1949L);
        ("model.safetensors", 1782040155L);
        ("model.safetensors.index.json", 82780L);
        ("tokenizer.json", 11423701L);
        ("tokenizer_config.json", 348L);
        ("special_tokens_map.json", 613L);
        ("added_tokens.json", 707L);
        ("vocab.json", 2776833L);
        ("chat_template.jinja", 5519L);
        ("preprocessor_config.json", 783L);
        ("video_preprocessor_config.json", 817L);
      ];
  }

let vision_all = [ vision_default ]

let find_vision name =
  List.find_opt (fun m -> m.name = name || m.repo = name) vision_all

let getenv name =
  match Sys.getenv_opt name with Some "" -> None | value -> value

let home () =
  match getenv "HOME" with
  | Some path -> path
  | None -> invalid_arg "Set HF_HUB_CACHE or HOME for embedding model downloads"

let absolute path =
  if path = "" || String.contains path '\000' then
    invalid_arg "Invalid embedding model cache directory";
  let path =
    if path = "~" then home ()
    else if String.starts_with ~prefix:"~/" path then
      Filename.concat (home ()) (String.sub path 2 (String.length path - 2))
    else path
  in
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let cache_dir () =
  let path =
    match getenv "HF_HUB_CACHE" with
    | Some path -> path
    | None -> (
        match getenv "HUGGINGFACE_HUB_CACHE" with
        | Some path -> path
        | None -> (
            match getenv "HF_HOME" with
            | Some path -> Filename.concat path "hub"
            | None ->
                let cache =
                  match getenv "XDG_CACHE_HOME" with
                  | Some path -> path
                  | None -> Filename.concat (home ()) ".cache"
                in
                Filename.concat cache "huggingface/hub"))
  in
  absolute path

let root = function None -> cache_dir () | Some path -> absolute path

let dir ?cache_dir model =
  let repo =
    "models--" ^ String.concat "--" (String.split_on_char '/' model.repo)
  in
  Filename.concat (root cache_dir)
    (Filename.concat repo (Filename.concat "snapshots" model.revision))

let file_state path expected =
  match Unix.LargeFile.stat path with
  | stat ->
      if
        stat.Unix.LargeFile.st_kind = Unix.S_REG
        && stat.Unix.LargeFile.st_size = expected
      then `Complete
      else `Invalid
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> `Missing

let present ?cache_dir model =
  let path = dir ?cache_dir model in
  List.for_all
    (fun (name, size) ->
      file_state (Filename.concat path name) size = `Complete)
    model.files

let run ~proc ~stderr args =
  Eio.Switch.run (fun sw ->
      let source, sink = Eio_unix.pipe sw in
      let output = Eio_unix.Resource.fd sink in
      let child =
        Eio.Cancel.protect (fun () ->
            Eio_unix.Process.spawn_unix ~sw proc ~pgid:0
              ~fds:
                [
                  (0, Eio_unix.Fd.stdin, `Blocking);
                  (1, output, `Blocking);
                  (2, output, `Blocking);
                ]
              args)
      in
      Fun.protect
        ~finally:(fun () ->
          (* uvx waits for a separate hf process. Killing only uvx leaves
             that process downloading after its caller has cancelled. Also
             close any inherited pipes if the wrapper exits before its child. *)
          try Unix.kill (-Eio.Process.pid child) Sys.sigkill
          with Unix.Unix_error (Unix.ESRCH, _, _) -> ())
        (fun () ->
          Eio.Flow.close sink;
          Eio.Fiber.fork ~sw (fun () -> Eio.Flow.copy source stderr);
          let status = Eio.Process.await child in
          match status with
          | `Exited 0 -> ()
          | status -> raise (Eio.Process.err (Eio.Process.Child_error status))))

let invalid_file path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_REG | Unix.S_LNK; _ } -> ()
  | _ ->
      failwith
        ("Embedding model cache contains a non-file entry: " ^ path
       ^ ". Move it out of the cache and retry.")
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let ensure ~proc ~stderr ?cache_dir model =
  Eio.Fiber.check ();
  let cache_dir = root cache_dir in
  let path = dir ~cache_dir model in
  let missing, invalid =
    List.fold_right
      (fun (name, size) (missing, invalid) ->
        match file_state (Filename.concat path name) size with
        | `Complete -> (missing, invalid)
        | `Missing -> (name :: missing, invalid)
        | `Invalid -> (missing, name :: invalid))
      model.files ([], [])
  in
  List.iter (fun name -> invalid_file (Filename.concat path name)) invalid;
  let download ~force files =
    let args =
      [ "download"; model.repo ] @ files
      @ [
          "--revision";
          model.revision;
          "--cache-dir";
          cache_dir;
          "--repo-type";
          "model";
        ]
      @ if force then [ "--force-download" ] else []
    in
    let invoke command = run ~proc ~stderr (command @ args) in
    try invoke [ "hf" ]
    with Eio.Io (Eio.Process.E _, _) -> (
      Eio.Fiber.check ();
      Eio.Flow.copy_string "hf unavailable or failed; retrying with uvx hf\n"
        stderr;
      try invoke [ "uvx"; "hf" ]
      with Eio.Io (Eio.Process.E _, _) ->
        Eio.Fiber.check ();
        failwith
          "Embedding model download failed. Install hf or uv, check network \
           access and the downloader errors, then retry.")
  in
  if missing <> [] || invalid <> [] then (
    Eio.Flow.copy_string
      (Printf.sprintf "Downloading %s (%s) into %s\n" model.name model.repo path)
      stderr;
    if invalid <> [] then (
      download ~force:true invalid;
      (* HF force-download replaces blobs, but leaves existing regular snapshot
         files unchanged. Keep invalid entries until the download succeeds so
         cancellation cannot lose the need to repair a corrupt cached blob. *)
      let relink =
        List.filter
          (fun name ->
            file_state (Filename.concat path name) (List.assoc name model.files)
            <> `Complete)
          invalid
      in
      List.iter
        (fun name ->
          let file = Filename.concat path name in
          try Unix.unlink file with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
        relink;
      if relink <> [] then download ~force:false relink);
    if missing <> [] then download ~force:false missing;
    Eio.Fiber.check ();
    if not (present ~cache_dir model) then
      failwith
        "Embedding model download is incomplete or has incorrect file sizes. \
         Check the downloader errors and retry.");
  path
