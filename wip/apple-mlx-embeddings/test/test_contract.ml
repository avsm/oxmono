module M = Apple_mlx_embeddings

let invalid f =
  match f () with
  | _ -> failwith "Expected invalid arguments"
  | exception Invalid_argument _ -> ()

let invalid_model () =
  let dir = Filename.temp_dir "mlx-invalid-model" "" in
  let config = Filename.concat dir "config.json" in
  let index = Filename.concat dir "model.safetensors.index.json" in
  let write path text =
    Out_channel.with_open_bin path (fun out -> output_string out text)
  in
  let rejected expected =
    match M.load ~model_dir:dir ~max_tokens:512 with
    | model ->
        M.close model;
        failwith "Invalid model unexpectedly loaded"
    | exception Failure message -> assert (message = expected)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> if Sys.file_exists path then Sys.remove path)
        [ config; index ];
      Unix.rmdir dir)
    (fun () ->
      write config
        {|{"model_type":"qwen3","hidden_size":1024,"max_position_embeddings":-1}|};
      rejected "Invalid model context length";
      write config {|{"model_type":"qwen3","hidden_size":1024}|};
      write index {|{"weight_map":{"weight":"../outside.safetensors"}}|};
      rejected "Weight shards must stay inside the model directory")

let () =
  Eio_main.run @@ fun _ ->
  invalid (fun () -> M.load ~model_dir:"" ~max_tokens:512);
  invalid (fun () -> M.load ~model_dir:"a\000b" ~max_tokens:512);
  invalid (fun () -> M.load ~model_dir:"missing" ~max_tokens:0);
  invalid (fun () -> M.load ~model_dir:"missing" ~max_tokens:32769);
  (match
     M.load ~model_dir:"/nonexistent/apple-mlx-embeddings" ~max_tokens:512
   with
  | model ->
      M.close model;
      failwith "Missing model unexpectedly loaded"
  | exception Failure _ -> ());
  (if M.available () then invalid_model ()
   else
     match M.memory_stats () with
     | _ -> failwith "Unavailable backend returned memory statistics"
     | exception Failure _ -> ());
  Gc.full_major ();
  print_endline "MLX argument and load-failure checks passed"
