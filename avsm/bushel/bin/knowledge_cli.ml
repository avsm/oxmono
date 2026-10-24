(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner
module Store = Bushel_knowledge_store
module Model = Apple_mlx_embeddings.Model

let object_json fields =
  Jsont.Json.object'
    (List.map
       (fun (key, value) -> Jsont.Json.mem (Jsont.Json.name key) value)
       fields)

let json fields =
  let value = object_json fields in
  match Jsont_bytesrw.encode_string Jsont.json value with
  | Ok text -> text
  | Error _ -> failwith "Could not encode knowledge command result"

let error message = json [ ("error", Jsont.Json.string message) ]

let protect ~json_output ~command run =
  Console_eio.setup ();
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ());
  let started = Unix.gettimeofday () in
  let emit result code =
    if json_output then print_endline result
    else
      Knowledge_output.print
        ~elapsed:(max 0. (Unix.gettimeofday () -. started))
        ~command result;
    code
  in
  try
    let result, code = run () in
    emit result code
  with
  | Eio.Cancel.Cancelled _ | Sys.Break ->
      emit (error "Knowledge operation cancelled") 130
  | Eio.Time.Timeout -> emit (error "Knowledge operation timed out") 1
  | Invalid_argument message | Failure message | Sys_error message ->
      emit (error message) 1
  | Unix.Unix_error (code, operation, path) ->
      emit
        (error
           (Printf.sprintf "%s: %s: %s" operation path (Unix.error_message code)))
        1
  | _ ->
      emit
        (error "Knowledge operation failed. Check configuration and sources.")
        1

let json_output =
  Arg.(value & flag & info [ "json" ] ~doc:"Write machine-readable JSON.")

let config_file =
  Arg.(
    value
    & opt (some string) None
    & info [ "c"; "config" ] ~docv:"FILE"
        ~doc:"Bushel TOML configuration (default: the XDG Bushel config).")

let optional name doc =
  Arg.(value & opt (some string) None & info [ name ] ~docv:"VALUE" ~doc)

let offset =
  Arg.(
    value & opt int 0
    & info [ "offset" ] ~docv:"N"
        ~doc:"Page offset returned by the previous result's next_offset.")

let positional name doc =
  Arg.(required & pos 0 (some string) None & info [] ~docv:name ~doc)

let with_store_env env config action =
  Eio.Switch.run (fun sw ->
      let fetch = Fetch_curl.v ~sw ~timeout:(Duration.of_sec 60) () in
      Store.with_store ?config_file:config ~fs:(Eio.Stdenv.fs env)
        ~clock:(Eio.Stdenv.clock env) ~fetch ~check:Eio.Fiber.check action)

let with_store config action =
  Eio_main.run (fun env -> with_store_env env config action)

let invoke ~json_output ~command config name fields =
  protect ~json_output ~command (fun () ->
      let arguments = json fields in
      with_store config (fun store ->
          let agent = Bushel_agent.of_store store ~max_bytes:65536 in
          match
            Bushel_agent.invoke agent ~check:Eio.Fiber.check name arguments
          with
          | Ok value -> (value, 0)
          | Error message -> (error message, 1)))

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let configure_command ~images =
  let output_command = if images then "images-configure" else "configure" in
  let dimensions =
    Arg.(
      value
      & opt (some int) None
      & info [ "dimensions" ] ~docv:"N"
          ~doc:
            "Vector dimensions. Defaults to the catalogue model's full width. \
             Required for HTTP and --model-dir.")
  in
  let backend =
    Arg.(
      value
      & opt
          (enum [ ("http", `Http); ("mlx", `Mlx) ])
          (if images then `Mlx else `Http)
      & info [ "backend" ] ~docv:"BACKEND"
          ~doc:
            (if images then "Embedding backend: local mlx."
             else "Embedding backend: http (default) or local mlx."))
  in
  let max_tokens =
    Arg.(
      value
      & opt (some int) None
      & info [ "max-tokens" ] ~docv:"N"
          ~doc:"MLX token limit (32 to 32768, default 8192).")
  in
  let need name = function
    | Some value -> value
    | None -> invalid_arg ("Missing --" ^ name ^ " for this backend")
  in
  let run json_output config backend url model dimensions api_key_file model_dir
      max_tokens query_instruction =
    protect ~json_output ~command:output_command (fun () ->
        let settings, download =
          match backend with
          | `Http ->
              if images then
                invalid_arg "Image embeddings require the mlx backend";
              if
                model_dir <> None || max_tokens <> None
                || query_instruction <> None
              then
                invalid_arg
                  "--model-dir, --max-tokens and --query-instruction require \
                   mlx";
              ( Bushel_config.Http
                  {
                    url = need "url" url;
                    model = need "model" model;
                    dimensions = need "dimensions" dimensions;
                    api_key_file;
                  },
                None )
          | `Mlx ->
              if url <> None || api_key_file <> None then
                invalid_arg "--url and --api-key-file require http";
              let model_dir, dimensions, download =
                match (model_dir, model) with
                | Some _, Some _ ->
                    invalid_arg "--model and --model-dir are mutually exclusive"
                | Some path, None -> (path, need "dimensions" dimensions, None)
                | None, name ->
                    let selected =
                      match name with
                      | None ->
                          if images then Model.vision_default else Model.default
                      | Some name -> (
                          match
                            (if images then Model.find_vision else Model.find)
                              name
                          with
                          | Some selected -> selected
                          | None ->
                              invalid_arg
                                ("Unknown MLX model. Run bushel knowledge "
                                ^ if images then "images models" else "models"))
                    in
                    let dimensions =
                      Option.value dimensions ~default:selected.dimensions
                    in
                    if dimensions > selected.dimensions then
                      invalid_arg "Dimensions exceed the selected model's width";
                    (Model.dir selected, dimensions, Some selected)
              in
              ( Bushel_config.Mlx
                  {
                    model_dir;
                    dimensions;
                    max_tokens = Option.value max_tokens ~default:8192;
                    query_instruction =
                      Option.value query_instruction
                        ~default:
                          (if images then
                             Bushel_config.default_image_query_instruction
                           else Bushel_config.default_query_instruction);
                  },
                download )
        in
        (match
           match (images, settings) with
           | true, Bushel_config.Mlx native ->
               Bushel_config.validate_image_embeddings native
           | _ -> Bushel_config.validate_embeddings settings
         with
        | Ok () -> ()
        | Error message -> invalid_arg message);
        let saved =
          Eio_main.run (fun env ->
              let settings =
                match (settings, download) with
                | Bushel_config.Mlx settings, Some selected ->
                    let model_dir =
                      Model.ensure
                        ~proc:(Eio.Stdenv.process_mgr env)
                        ~stderr:(Eio.Stdenv.stderr env) selected
                    in
                    Bushel_config.Mlx { settings with model_dir }
                | settings, _ -> settings
              in
              Eio_unix.run_in_systhread ~label:"bushel.configure" (fun () ->
                  match (images, settings) with
                  | true, Bushel_config.Mlx native ->
                      Bushel_config.configure_image_embeddings ?file:config
                        native
                  | _ ->
                      Bushel_config.configure_embeddings ?file:config settings))
        in
        match saved with
        | Ok path ->
            ( json
                [
                  ("configured", Jsont.Json.bool true);
                  ("config_file", Jsont.Json.string path);
                ],
              0 )
        | Error message -> (error message, 1))
  in
  command "configure" "Configure the shared embedding provider."
    Term.(
      const run $ json_output $ config_file $ backend
      $ optional "url" "HTTP embedding API endpoint. Credentials require HTTPS."
      $ optional "model"
          (if images then "MLX catalogue name (default: qwen3-vl-2b)."
           else
             "HTTP model identifier or MLX catalogue name (default: qwen3-4b).")
      $ dimensions
      $ optional "api-key-file"
          "Path to the HTTP API key file. The key is never stored in TOML."
      $ optional "model-dir"
          "Existing local MLX directory. Bypasses downloads and requires \
           dimensions."
      $ max_tokens
      $ optional "query-instruction" "MLX search query instruction.")

let configure_cmd = configure_command ~images:false

let models_command ~images =
  let catalog, default =
    if images then (Model.vision_all, Model.vision_default)
    else (Model.all, Model.default)
  in
  let describe (model : Model.t) =
    let size =
      List.fold_left
        (fun total (_, size) -> Int64.add total size)
        0L model.files
    in
    object_json
      [
        ("name", Jsont.Json.string model.name);
        ("repo", Jsont.Json.string model.repo);
        ("revision", Jsont.Json.string model.revision);
        ("dimensions", Jsont.Json.int model.dimensions);
        ("size_bytes", Jsont.Json.number (Int64.to_float size));
        ("cached", Jsont.Json.bool (Model.present model));
        ("model_dir", Jsont.Json.string (Model.dir model));
        ("default", Jsont.Json.bool (model.name = default.name));
      ]
  in
  command "models" "List local embedding models and their cache status."
    Term.(
      const (fun json_output ->
          protect ~json_output
            ~command:(if images then "images-models" else "models")
            (fun () ->
              ( json
                  [
                    ("default", Jsont.Json.string default.name);
                    ("models", Jsont.Json.list (List.map describe catalog));
                  ],
                0 )))
      $ json_output)

let models_cmd = models_command ~images:false

let status_cmd =
  command "status" "Refresh local sources and report shared index coverage."
    Term.(
      const (fun json_output config ->
          protect ~json_output ~command:"status" (fun () ->
              (with_store config Store.status, 0)))
      $ json_output $ config_file)

let with_progress_interrupt ~clock run =
  let interrupted = Atomic.make false in
  let previous =
    Sys.Safe.signal Sys.sigint
      (Sys.Signal_handle (fun _ -> Atomic.set interrupted true))
  in
  let rec watch () =
    Eio.Time.sleep clock 0.05;
    (if Atomic.exchange interrupted false then
       match previous with
       | Sys.Signal_ignore -> ()
       | Sys.Signal_default -> raise Sys.Break
       | Sys.Signal_handle handler -> handler Sys.sigint);
    watch ()
  in
  Fun.protect
    ~finally:(fun () -> Sys.Safe.set_signal Sys.sigint previous)
    (fun () -> Eio.Fiber.first run watch)

let index_progress ~noun ~clock ~is_tty run =
  let module D = Console.Display in
  let dimensions () =
    try
      let size = Eio_unix.Pty.get_window_size Eio_unix.Fd.stderr in
      if size.cols > 0 && size.rows > 0 then (min 96 size.cols, size.rows)
      else Console_eio.dimensions ()
    with Unix.Unix_error _ -> Console_eio.dimensions ()
  in
  let ctx =
    D.ctx ~ppf:Fmt.stderr
      ~now:(fun () -> Eio.Time.now clock)
      ~wait:(Eio.Time.sleep clock) ~dimensions ~is_tty ()
  in
  let bar =
    D.Line.(
      v
        [
          spinner;
          text " ";
          label;
          text " ";
          bar 18;
          text " ";
          count;
          text " ";
          percent;
          text " ";
          elapsed;
        ])
  in
  with_progress_interrupt ~clock (fun () ->
      Console_eio.run_on ~ctx ~mode:`Activity ~bar (fun display ->
          let task =
            D.task ~transient:true display
              (if noun = "images" then "Reading image library"
               else "Reading knowledge sources")
          in
          D.tick display;
          let initial = ref None in
          let coverage = ref None in
          let on_progress ~indexed ~total =
            if !initial = None then initial := Some indexed;
            coverage := Some (indexed, total);
            D.set_label task ("Indexing " ^ noun);
            D.set_count task ~cur:indexed ~total
          in
          match run ~on_progress () with
          | result ->
              let message =
                match !coverage with
                | Some (indexed, total) when indexed < total ->
                    Printf.sprintf "Batch limit: %d %s remaining"
                      (total - indexed) noun
                | Some (indexed, _) when !initial = Some indexed ->
                    Printf.sprintf "Up to date: %d %s" indexed noun
                | Some (indexed, _) ->
                    Printf.sprintf "Indexing complete: %d %s" indexed noun
                | None -> "Indexing finished"
              in
              D.succeed ~message task;
              result
          | exception exn ->
              let backtrace = Printexc.get_raw_backtrace () in
              (match exn with
              | Eio.Cancel.Cancelled _ | Sys.Break ->
                  D.cancel ~message:"Indexing cancelled" task
              | _ -> D.fail ~message:"Indexing failed" task);
              Printexc.raise_with_backtrace exn backtrace))

let index_command ~images =
  let bounded_batches =
    let parse value =
      match int_of_string_opt value with
      | Some n when n >= 1 && n <= 10000 -> Ok n
      | _ -> Error (`Msg "max-batches must be between 1 and 10000")
    in
    Arg.conv (parse, Format.pp_print_int)
  in
  let max_batches =
    Arg.(
      value
      & opt (some bounded_batches) None
      & info [ "max-batches" ] ~docv:"N"
          ~doc:
            "Stop after N batches (1 to 10000). By default, finish all pending \
             passages.")
  in
  let no_progress =
    Arg.(
      value & flag
      & info [ "no-progress" ] ~doc:"Disable the progress display on stderr.")
  in
  command "index" "Index changed passages with the shared embedding provider."
    Term.(
      const (fun json_output config max_batches no_progress ->
          protect ~json_output
            ~command:(if images then "images-index" else "index")
            (fun () ->
              let output =
                Eio_main.run (fun env ->
                    let run ?on_progress () =
                      with_store_env env config (fun store ->
                          match max_batches with
                          | None ->
                              (if images then Store.index_images_all
                               else Store.index_all)
                                ?on_progress store ~check:Eio.Fiber.check
                          | Some max_batches ->
                              (if images then Store.index_images
                               else Store.index)
                                ?on_progress store ~check:Eio.Fiber.check
                                ~max_batches)
                    in
                    let is_tty = Unix.isatty Unix.stderr in
                    if
                      (not no_progress) && is_tty
                      && Sys.getenv_opt "TERM" <> Some "dumb"
                    then
                      index_progress
                        ~noun:(if images then "images" else "passages")
                        ~clock:(Eio.Stdenv.clock env) ~is_tty
                        (fun ~on_progress () -> run ~on_progress ())
                    else run ())
              in
              (output, 0)))
      $ json_output $ config_file $ max_batches $ no_progress)

let index_cmd = index_command ~images:false

let search_cmd =
  let mode =
    let modes =
      Arg.enum
        [
          ("lexical", "lexical"); ("semantic", "semantic"); ("hybrid", "hybrid");
        ]
    in
    Arg.(
      value & opt modes "lexical"
      & info [ "mode" ] ~docv:"MODE"
          ~doc:
            "Search mode: lexical (default), semantic or hybrid. Plain lexical \
             queries require every term. Hybrid combines keywords and \
             embedding similarity.")
  in
  let run json_output config query mode kind tag contact project since until
      offset =
    let optional_fields =
      List.filter_map
        (fun (name, value) ->
          Option.map (fun value -> (name, Jsont.Json.string value)) value)
        [
          ("kind", kind);
          ("tag", tag);
          ("contact", contact);
          ("project", project);
          ("since", since);
          ("until", until);
        ]
    in
    invoke ~json_output ~command:"search" config "knowledge_search"
      ([
         ("query", Jsont.Json.string query);
         ("mode", Jsont.Json.string mode);
         ("offset", Jsont.Json.int offset);
       ]
      @ optional_fields)
  in
  command "search" "Search the shared Bushel and Sortal corpus."
    Term.(
      const run $ json_output $ config_file
      $ positional "QUERY" "Search query."
      $ mode
      $ optional "kind" "Source kind filter."
      $ optional "tag" "Tag filter."
      $ optional "contact" "Sortal contact handle filter."
      $ optional "project" "Project filter."
      $ optional "since" "Inclusive lower date bound (YYYY-MM-DD)."
      $ optional "until" "Inclusive upper date bound (YYYY-MM-DD)."
      $ offset)

let get_cmd =
  command "get" "Read a source by its stable ID, in bounded UTF-8 pages."
    Term.(
      const (fun json_output config id offset ->
          invoke ~json_output ~command:"get" config "knowledge_get"
            [ ("id", Jsont.Json.string id); ("offset", Jsont.Json.int offset) ])
      $ json_output $ config_file
      $ positional "ID" "Source ID returned by search."
      $ offset)

let contacts_cmd =
  command "contacts" "Resolve a name, handle, email or account to contacts."
    Term.(
      const (fun json_output config query offset ->
          invoke ~json_output ~command:"contacts" config "contacts_resolve"
            [
              ("query", Jsont.Json.string query);
              ("offset", Jsont.Json.int offset);
            ])
      $ json_output $ config_file
      $ positional "QUERY" "Contact name, handle or account."
      $ offset)

let images_cmd =
  let status =
    command "status"
      "Show image search coverage and the configured image model."
      Term.(
        const (fun json_output config ->
            protect ~json_output ~command:"images-status" (fun () ->
                (with_store config Store.image_status, 0)))
        $ json_output $ config_file)
  in
  let search =
    let mode =
      Arg.(
        value
        & opt
            (enum
               [
                 ("lexical", "lexical");
                 ("semantic", "semantic");
                 ("hybrid", "hybrid");
               ])
            "semantic"
        & info [ "mode" ] ~docv:"MODE"
            ~doc:"Image search mode: semantic (default), lexical or hybrid.")
    in
    command "search" "Find images by visual content or their metadata."
      Term.(
        const (fun json_output config query mode offset ->
            protect ~json_output ~command:"images-search" (fun () ->
                ( with_store config (fun store ->
                      Store.images_search store ~check:Eio.Fiber.check ~query
                        ~mode ~offset),
                  0 )))
        $ json_output $ config_file
        $ positional "QUERY" "Describe the image to find."
        $ mode $ offset)
  in
  let get =
    command "get" "Show an image's source file and metadata."
      Term.(
        const (fun json_output config id offset ->
            protect ~json_output ~command:"images-get" (fun () ->
                ( with_store config (fun store ->
                      Store.images_get store ~check:Eio.Fiber.check ~id ~offset),
                  0 )))
        $ json_output $ config_file
        $ positional "ID" "Image ID returned by search."
        $ offset)
  in
  Cmd.group
    (Cmd.info "images" ~doc:"Configure, index and search Bushel images.")
    [
      configure_command ~images:true;
      models_command ~images:true;
      status;
      index_command ~images:true;
      search;
      get;
    ]

let cmd =
  Cmd.group
    (Cmd.info "knowledge"
       ~doc:"Configure, index and query shared Bushel and Sortal knowledge."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Commands print readable results. Use --json for machine-readable \
              output. Settings are read from Bushel configuration on every \
              invocation. Sources are local Bushel files and the native Sortal \
              store and cached feeds. No sync is performed. Search and get \
              results are bounded to 65536 bytes. Use next_offset to request \
              another page.";
         ])
    [
      images_cmd;
      configure_cmd;
      models_cmd;
      index_cmd;
      status_cmd;
      search_cmd;
      get_cmd;
      contacts_cmd;
    ]
