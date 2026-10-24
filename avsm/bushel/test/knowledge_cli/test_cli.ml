module Model = Apple_mlx_embeddings.Model

let check name condition = if not condition then failwith name
let ok = function Ok value -> value | Error message -> failwith message
let read path = In_channel.with_open_bin path In_channel.input_all

let save path text =
  Out_channel.with_open_bin path (fun out -> output_string out text)

let rec mkdir path =
  if not (Sys.file_exists path) then (
    mkdir (Filename.dirname path);
    Unix.mkdir path 0o700)

let populate ~cache_dir model =
  let dir = Model.dir ~cache_dir model in
  List.iter
    (fun (name, size) ->
      let path = Filename.concat dir name in
      mkdir (Filename.dirname path);
      let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () -> Unix.LargeFile.ftruncate fd size))
    model.Model.files

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Sys.readdir path
    |> Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path)
  else Unix.unlink path

let () =
  let binary = Sys.argv.(1) in
  let dir = Filename.temp_dir "bushel-configure-cli" "" in
  Fun.protect
    ~finally:(fun () -> remove dir)
    (fun () ->
      let file = Filename.concat dir "config.toml" in
      let output = Filename.concat dir "stdout" in
      let errors = Filename.concat dir "stderr" in
      let cache_dir = Filename.concat dir "hub-cache" in
      let downloader_bin = Filename.concat dir "bin" in
      let download_log = Filename.concat dir "downloads" in
      mkdir downloader_bin;
      List.iter
        (fun name ->
          let path = Filename.concat downloader_bin name in
          save path
            "#!/bin/sh\n\
             printf '%s\\n' invoked >> \"$BUSHEL_TEST_DOWNLOAD_LOG\"\n\
             echo downloader-progress\n\
             exit 9\n";
          Unix.chmod path 0o700)
        [ "hf"; "uvx" ];
      let environment =
        let replaced = [ "PATH"; "HF_HUB_CACHE"; "BUSHEL_TEST_DOWNLOAD_LOG" ] in
        Array.to_list (Unix.environment ())
        |> List.filter (fun value ->
               not
                 (List.exists
                    (fun name -> String.starts_with ~prefix:(name ^ "=") value)
                    replaced))
        |> fun previous ->
        Array.of_list
          (("PATH=" ^ downloader_bin)
          :: ("HF_HUB_CACHE=" ^ cache_dir)
          :: ("BUSHEL_TEST_DOWNLOAD_LOG=" ^ download_log)
          :: previous)
      in
      let command ?(json = true) args =
        let stdout =
          Unix.openfile output
            [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
            0o600
        in
        let stderr =
          Unix.openfile errors
            [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
            0o600
        in
        let pid =
          Fun.protect
            ~finally:(fun () ->
              Unix.close stdout;
              Unix.close stderr)
            (fun () ->
              Unix.create_process_env binary
                (Array.of_list
                   ((binary :: "knowledge" :: args)
                   @ if json then [ "--json" ] else []))
                environment Unix.stdin stdout stderr)
        in
        let _, status = Unix.waitpid [] pid in
        (status, read output)
      in
      let run args = command ("configure" :: "--config" :: file :: args) in
      let successful args =
        let status, output = run args in
        check
          ("CLI configure failed: " ^ output ^ read errors)
          (status = Unix.WEXITED 0);
        check "success JSON" (contains output "\"configured\":true")
      in
      successful
        [
          "--url";
          "https://example.org/embeddings";
          "--model";
          "legacy";
          "--dimensions";
          "768";
        ];
      check "legacy HTTP CLI works"
        ((ok (Bushel_config.load_file file)).embeddings
        = Some
            (Bushel_config.Http
               {
                 url = "https://example.org/embeddings";
                 model = "legacy";
                 dimensions = 768;
                 api_key_file = None;
               }));
      successful
        [
          "--backend";
          "mlx";
          "--model-dir";
          "local-model";
          "--dimensions";
          "1024";
        ];
      let native =
        match (ok (Bushel_config.load_file file)).embeddings with
        | Some (Bushel_config.Mlx native) -> native
        | _ -> failwith "MLX config absent"
      in
      check "MLX CLI defaults and model path"
        (native.model_dir = Filename.concat (Sys.getcwd ()) "local-model"
        && native.dimensions = 1024 && native.max_tokens = 8192
        && native.query_instruction = Bushel_config.default_query_instruction);
      successful
        [
          "--backend";
          "mlx";
          "--model-dir";
          "local-model";
          "--dimensions";
          "32";
          "--max-tokens";
          "128";
          "--query-instruction";
          "";
        ];
      check "MLX CLI explicit limits and instruction"
        ((ok (Bushel_config.load_file file)).embeddings
        = Some
            (Bushel_config.Mlx
               {
                 native with
                 dimensions = 32;
                 max_tokens = 128;
                 query_instruction = "";
               }));
      let original = read file in
      List.iter
        (fun args ->
          let status, output = run args in
          check "invalid CLI arguments fail" (status <> Unix.WEXITED 0);
          check "error JSON" (contains output "\"error\"");
          check "invalid CLI leaves config untouched" (read file = original);
          check "error does not expose URL credentials"
            (not (contains output "secret-value")))
        [
          [ "--backend"; "mlx"; "--model-dir"; "model" ];
          [ "--backend"; "mlx"; "--model"; "unknown-model" ];
          [
            "--backend"; "mlx"; "--model"; "qwen3-0.6b"; "--dimensions"; "1025";
          ];
          [
            "--backend";
            "mlx";
            "--model";
            "qwen3-4b";
            "--model-dir";
            "model";
            "--dimensions";
            "32";
          ];
          [ "--backend"; "mlx"; "--max-tokens"; "0" ];
          [ "--backend"; "mlx"; "--dimensions"; "31" ];
          [ "--url"; "https://example.org/"; "--model"; "http" ];
          [ "--backend"; "mlx"; "--model-dir"; "model"; "--dimensions"; "31" ];
          [
            "--backend";
            "mlx";
            "--model-dir";
            "model";
            "--dimensions";
            "32";
            "--max-tokens";
            "32769";
          ];
          [
            "--backend";
            "mlx";
            "--model-dir";
            "model";
            "--dimensions";
            "32";
            "--api-key-file";
            "secret-value";
          ];
          [
            "--url";
            "https://example.org/";
            "--model";
            "http";
            "--dimensions";
            "32";
            "--model-dir";
            "model";
          ];
          [ "--dimensions"; "32" ];
          [
            "--url";
            "https://secret-value@example.org/";
            "--model";
            "http";
            "--dimensions";
            "32";
          ];
        ];
      check "validation and explicit local paths never invoke downloader"
        (not (Sys.file_exists download_log));
      let status, catalogue = command [ "models" ] in
      check "models lists uncached catalogue without downloading"
        (status = Unix.WEXITED 0
        && (not (Sys.file_exists download_log))
        && contains catalogue "\"default\":\"qwen3-4b\""
        && contains catalogue "\"size_bytes\":"
        && contains catalogue "\"cached\":false");
      List.iter
        (fun (model : Model.t) ->
          check "model listed" (contains catalogue model.name))
        Model.all;
      let status, output = run [ "--backend"; "mlx" ] in
      check "download failure reported" (status <> Unix.WEXITED 0);
      check "download failure leaves configuration untouched"
        (read file = original);
      check "download was attempted" (Sys.file_exists download_log);
      check "download progress stays off JSON stdout"
        (not (contains output "downloader-progress"));
      Unix.unlink download_log;
      List.iter (populate ~cache_dir) Model.all;
      let native () =
        match (ok (Bushel_config.load_file file)).embeddings with
        | Some (Bushel_config.Mlx settings) -> settings
        | _ -> failwith "MLX catalogue configuration missing"
      in
      successful [ "--backend"; "mlx" ];
      check "MLX defaults to cached 4B full width"
        ((native ()).dimensions = Model.default.dimensions
        && (native ()).model_dir = Model.dir ~cache_dir Model.default);
      let small = Option.get (Model.find "qwen3-0.6b") in
      successful [ "--backend"; "mlx"; "--model"; small.name ];
      check "named model defaults full width"
        ((native ()).dimensions = small.dimensions
        && (native ()).model_dir = Model.dir ~cache_dir small);
      successful
        [ "--backend"; "mlx"; "--model"; small.repo; "--dimensions"; "256" ];
      check "full repository and reduced dimensions accepted"
        ((native ()).dimensions = 256
        && (native ()).model_dir = Model.dir ~cache_dir small);
      let status, catalogue = command [ "models" ] in
      check "models reports populated cache"
        (status = Unix.WEXITED 0
        && contains catalogue "\"cached\":true"
        && not (contains catalogue "\"cached\":false"));
      check "cached models need no downloader"
        (not (Sys.file_exists download_log));
      let status, human = command ~json:false [ "models" ] in
      check "models defaults to readable stdout"
        (status = Unix.WEXITED 0 && contains human "Model"
        && contains human "Dimensions"
        && contains human Model.default.name
        && contains human "bushel knowledge configure"
        && (not (contains human "\"models\":"))
        && read errors = "");
      let status, human =
        command ~json:false
          [
            "configure";
            "--config";
            file;
            "--backend";
            "mlx";
            "--model-dir";
            "local-text-model";
            "--dimensions";
            "256";
          ]
      in
      check "configure defaults to readable stdout"
        (status = Unix.WEXITED 0
        && contains human "Embedding provider configured."
        && (not (contains human "\"configured\":"))
        && read errors = "");
      let unchanged = read file in
      let status, human =
        command ~json:false
          [
            "configure";
            "--config";
            file;
            "--backend";
            "mlx";
            "--model";
            "unknown-model";
          ]
      in
      check "human errors go to stderr with nonzero status"
        (status <> Unix.WEXITED 0 && human = ""
        && contains (read errors) "Error: Unknown MLX model"
        && (not (contains (read errors) "\"error\":"))
        && read file = unchanged);
      let status, catalogue = command [ "images"; "models" ] in
      check "image JSON catalogue has distinct default"
        (status = Unix.WEXITED 0
        && contains catalogue
             ("\"default\":\"" ^ Model.vision_default.name ^ "\"")
        && not (contains catalogue ("\"name\":\"" ^ Model.default.name ^ "\""))
        );
      List.iter
        (fun (model : Model.t) ->
          check "vision model listed" (contains catalogue model.name))
        Model.vision_all;
      let status, human = command ~json:false [ "images"; "models" ] in
      check "image catalogue defaults to readable stdout"
        (status = Unix.WEXITED 0
        && contains human Model.vision_default.name
        && contains human "default"
        && contains human "bushel knowledge images configure"
        && (not (contains human "\"models\":"))
        && read errors = "");
      let image_run ?(json = true) args =
        command ~json ("images" :: "configure" :: "--config" :: file :: args)
      in
      let text_settings = (ok (Bushel_config.load_file file)).embeddings in
      let status, human =
        image_run ~json:false
          [ "--model-dir"; "local-vision-model"; "--dimensions"; "64" ]
      in
      check "local image configure accepts minimum dimensions without download"
        (status = Unix.WEXITED 0
        && contains human "Image embedding provider configured."
        && not (Sys.file_exists download_log));
      let configured = ok (Bushel_config.load_file file) in
      check "image configure preserves text provider"
        (configured.embeddings = text_settings);
      let image_settings = Option.get configured.image_embeddings in
      check "local image settings are independent and anchored"
        (image_settings.dimensions = 64
        && image_settings.model_dir
           = Filename.concat (Sys.getcwd ()) "local-vision-model"
        && image_settings.query_instruction
           = Bushel_config.default_image_query_instruction);
      let unchanged = read file in
      List.iter
        (fun args ->
          let status, output = image_run args in
          check "image validation fails without changing config"
            (status <> Unix.WEXITED 0
            && contains output "\"error\":"
            && read file = unchanged))
        [
          [ "--model-dir"; "local-vision-model"; "--dimensions"; "63" ];
          [ "--model"; Model.default.name ];
          [ "--model"; Model.default.repo ];
          [
            "--model";
            Model.vision_default.name;
            "--dimensions";
            string_of_int (Model.vision_default.dimensions + 1);
          ];
          [
            "--backend";
            "http";
            "--url";
            "https://example.org/";
            "--model";
            "http";
            "--dimensions";
            "64";
          ];
        ];
      List.iter
        (fun model ->
          let status, output = run [ "--backend"; "mlx"; "--model"; model ] in
          check "vision model rejected by text catalogue"
            (status <> Unix.WEXITED 0
            && contains output "\"error\":"
            && read file = unchanged))
        [ Model.vision_default.name; Model.vision_default.repo ];
      check "invalid model families never download"
        (not (Sys.file_exists download_log));
      populate ~cache_dir Model.vision_default;
      let status, output = image_run [] in
      check "image default config keeps JSON compatibility"
        (status = Unix.WEXITED 0 && contains output "\"configured\":true");
      let configured = ok (Bushel_config.load_file file) in
      let image_settings = Option.get configured.image_embeddings in
      check "image default uses its cached model and full width"
        (image_settings.dimensions = Model.vision_default.dimensions
        && image_settings.model_dir = Model.dir ~cache_dir Model.vision_default
        && configured.embeddings = text_settings
        && not (Sys.file_exists download_log)));
  print_endline
    "Human/JSON knowledge CLI and separate text/image catalogues passed"
