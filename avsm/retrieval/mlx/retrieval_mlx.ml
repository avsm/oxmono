module E = Retrieval.Embedding
module J = Retrieval.Json
module Native = Apple_mlx_embeddings
module SHA = Digestif.SHA256

type stamp = {
  name : string;
  device : int;
  inode : int;
  size : int;
  modified : float;
  changed : float;
}

let relevant name =
  List.exists
    (fun suffix -> String.ends_with ~suffix name)
    [ ".safetensors"; ".json"; ".model"; ".txt"; ".tiktoken"; ".jinja" ]

let manifest root =
  let rec walk ancestors relative acc =
    let path = Filename.concat root relative in
    let stat = Unix.stat path in
    match stat.st_kind with
    | Unix.S_DIR ->
        let identity = (stat.st_dev, stat.st_ino) in
        if List.mem identity ancestors then
          invalid_arg "Cycle in embedding model directory";
        Sys.readdir path
        |> Array.fold_left
             (fun acc name ->
               if String.starts_with ~prefix:"." name then acc
               else
                 walk (identity :: ancestors)
                   (if relative = "" then name
                    else Filename.concat relative name)
                   acc)
             acc
    | Unix.S_REG when relevant relative ->
        {
          name = relative;
          device = stat.st_dev;
          inode = stat.st_ino;
          size = stat.st_size;
          modified = stat.st_mtime;
          changed = stat.st_ctime;
        }
        :: acc
    | _ when relevant relative ->
        invalid_arg "Embedding model files must be regular files"
    | _ -> acc
  in
  let files = walk [] "" [] in
  if
    (not (List.exists (fun f -> f.name = "config.json") files))
    || not
         (List.exists
            (fun f -> String.ends_with ~suffix:".safetensors" f.name)
            files)
  then
    invalid_arg "Embedding model requires config.json and safetensors weights";
  List.sort (fun a b -> String.compare a.name b.name) files

let digest_file path =
  let fd =
    Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK ] 0
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      if (Unix.fstat fd).st_kind <> Unix.S_REG then
        invalid_arg "Embedding model files must be regular files";
      let bytes = Bytes.create 65536 in
      let rec loop digest =
        match Unix.read fd bytes 0 (Bytes.length bytes) with
        | 0 -> SHA.(to_hex (get digest))
        | n -> loop (SHA.feed_bytes digest ~len:n bytes)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop digest
      in
      loop SHA.empty)

let revision_lock = Mutex.create ()
let revisions = ref []

let revision root =
  Eio_unix.run_in_systhread (fun () ->
      Mutex.lock revision_lock;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock revision_lock)
        (fun () ->
          let before = manifest root in
          match
            List.find_opt
              (fun (path, previous, _) -> path = root && previous = before)
              !revisions
          with
          | Some (_, _, digest) -> digest
          | None ->
              let digest =
                before
                |> List.map (fun f ->
                       J.encode (Jsont.list Jsont.string)
                         [ f.name; digest_file (Filename.concat root f.name) ])
                |> J.encode (Jsont.list Jsont.string)
                |> E.hash
              in
              if manifest root <> before then
                failwith "Embedding model files changed while hashing";
              revisions :=
                (root, before, digest)
                ::
                (match
                   List.filter (fun (path, _, _) -> path <> root) !revisions
                 with
                | first :: _ -> [ first ]
                | [] -> []);
              digest))

let model_lock = Eio.Mutex.create ()
let loaded : (string * Native.t) option ref = ref None

let clear () =
  Eio.Cancel.protect (fun () ->
      match !loaded with
      | None -> ()
      | Some (_, model) ->
          loaded := None;
          Native.close model)

let release () =
  Eio.Cancel.protect (fun () -> Eio.Mutex.use_ro model_lock clear)

let create ~model_dir ~dimensions ~max_tokens ~query_instruction ~check =
  if
    String.length query_instruction > 8192
    || String.contains query_instruction '\000'
  then
    invalid_arg "MLX query instruction must be at most 8192 bytes without NUL";
  if dimensions < 32 || dimensions > 4096 then
    invalid_arg "MLX embedding dimensions must be between 32 and 4096";
  if max_tokens < 32 || max_tokens > 32768 then
    invalid_arg "MLX embedding token limit must be between 32 and 32768";
  check ();
  let model_dir = Unix.realpath model_dir in
  let content = revision model_dir in
  check ();
  let identity =
    J.encode (Jsont.list Jsont.string)
      [
        "qwen3-embedding/v1";
        Native.bridge_version;
        content;
        string_of_int dimensions;
        string_of_int max_tokens;
        query_instruction;
      ]
    |> E.hash
  in
  let unchanged () =
    if revision model_dir <> content then
      failwith "Embedding model changed. Recreate the provider before indexing"
  in
  let embed ~task ~check inputs =
    if
      inputs = []
      || List.length inputs > 4
      || List.exists (fun text -> String.length text > 8192) inputs
    then invalid_arg "MLX embedding batch exceeds limits";
    check ();
    let inputs =
      List.map
        (fun text ->
          match task with
          | `Document -> text
          | `Query when query_instruction = "" -> text
          | `Query -> "Instruct: " ^ query_instruction ^ "\nQuery:" ^ text)
        inputs
      |> Array.of_list
    in
    Eio.Mutex.use_ro model_lock (fun () ->
        check ();
        unchanged ();
        check ();
        let model =
          match !loaded with
          | Some (previous, model) when previous = identity -> model
          | _ ->
              clear ();
              Eio.Cancel.protect (fun () ->
                  let model = Native.load ~model_dir ~max_tokens in
                  match unchanged () with
                  | () ->
                      loaded := Some (identity, model);
                      model
                  | exception exn ->
                      let bt = Printexc.get_raw_backtrace () in
                      Native.close model;
                      Printexc.raise_with_backtrace exn bt)
        in
        check ();
        let vectors = Native.embed model ~dimensions inputs in
        check ();
        if
          Bigarray.Array2.dim1 vectors <> Array.length inputs
          || Bigarray.Array2.dim2 vectors <> dimensions
        then invalid_arg "MLX returned incorrect embedding dimensions";
        List.init (Array.length inputs) (fun row ->
            List.init dimensions (fun column -> vectors.{row, column})))
  in
  E.{ model = "mlx-qwen3:" ^ identity; dimensions; max_batch = 4; embed }

module Vision = struct
  module Vision_native = Apple_mlx_embeddings.Vision

  let model_lock = Eio.Mutex.create ()
  let loaded : (string * Vision_native.t) option ref = ref None

  let clear () =
    Eio.Cancel.protect (fun () ->
        match !loaded with
        | None -> ()
        | Some (_, model) ->
            loaded := None;
            Vision_native.close model)

  let release () =
    Eio.Cancel.protect (fun () -> Eio.Mutex.use_ro model_lock clear)

  let create ~model_dir ~dimensions ~max_tokens ~query_instruction ~check =
    if
      String.length query_instruction > 8192
      || String.contains query_instruction '\000'
    then
      invalid_arg "MLX query instruction must be at most 8192 bytes without NUL";
    if dimensions < 64 || dimensions > 4096 then
      invalid_arg "MLX embedding dimensions must be between 64 and 4096";
    if max_tokens < 32 || max_tokens > 32768 then
      invalid_arg "MLX embedding token limit must be between 32 and 32768";
    check ();
    let model_dir = Unix.realpath model_dir in
    let content = revision model_dir in
    check ();
    let identity =
      J.encode (Jsont.list Jsont.string)
        [
          "qwen3-vl-embedding/v1";
          Vision_native.bridge_version;
          content;
          string_of_int dimensions;
          string_of_int max_tokens;
          query_instruction;
        ]
      |> E.hash
    in
    let unchanged () =
      if revision model_dir <> content then
        failwith
          "Embedding model changed. Recreate the provider before indexing"
    in
    let embed ~task ~check inputs =
      if
        inputs = []
        || List.length inputs > 2
        || List.exists (fun text -> String.length text > 8192) inputs
      then invalid_arg "MLX embedding batch exceeds limits";
      check ();
      let paths =
        match task with
        | `Query -> []
        | `Document ->
            List.map
              (fun input ->
                match J.decode (Jsont.list Jsont.string) input with
                | [ path; digest ] -> (path, digest)
                | _ ->
                    invalid_arg "Image input must contain a path and pixel hash")
              inputs
      in
      let verify_pixels () =
        if paths <> [] then
          Eio_unix.run_in_systhread (fun () ->
              List.iter
                (fun (path, expected) ->
                  if digest_file path <> expected then
                    failwith
                      "Image changed during inference. Refresh the image corpus")
                paths)
      in
      let inputs =
        Array.of_list
          (match task with `Query -> inputs | `Document -> List.map fst paths)
      in
      Eio.Mutex.use_ro model_lock (fun () ->
          check ();
          unchanged ();
          verify_pixels ();
          check ();
          let model =
            match !loaded with
            | Some (previous, model) when previous = identity -> model
            | _ ->
                clear ();
                Eio.Cancel.protect (fun () ->
                    let model = Vision_native.load ~model_dir ~max_tokens in
                    match unchanged () with
                    | () ->
                        loaded := Some (identity, model);
                        model
                    | exception exn ->
                        let bt = Printexc.get_raw_backtrace () in
                        Vision_native.close model;
                        Printexc.raise_with_backtrace exn bt)
          in
          check ();
          verify_pixels ();
          check ();
          let vectors =
            match task with
            | `Query ->
                Vision_native.embed_text model ~instruction:query_instruction
                  ~dimensions inputs
            | `Document ->
                Vision_native.embed_images model ~instruction:query_instruction
                  ~dimensions inputs
          in
          verify_pixels ();
          check ();
          if
            Bigarray.Array2.dim1 vectors <> Array.length inputs
            || Bigarray.Array2.dim2 vectors <> dimensions
          then invalid_arg "MLX returned incorrect embedding dimensions";
          List.init (Array.length inputs) (fun row ->
              List.init dimensions (fun column -> vectors.{row, column})))
    in
    E.{ model = "mlx-qwen3-vl:" ^ identity; dimensions; max_batch = 2; embed }
end
