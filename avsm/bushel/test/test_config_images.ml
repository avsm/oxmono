let check name value = if not value then failwith name
let ok = function Ok value -> value | Error message -> failwith message

let write path value =
  Out_channel.with_open_bin path (fun oc -> output_string oc value)

let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  let file = Filename.temp_file "bushel-image-config" ".toml" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove file;
      if Sys.file_exists (file ^ ".lock") then Sys.remove (file ^ ".lock"))
    (fun () ->
      let image : Bushel_config.mlx_embeddings =
        {
          model_dir = "local/vision";
          dimensions = 2048;
          max_tokens = 8192;
          query_instruction = Bushel_config.default_image_query_instruction;
        }
      in
      let source =
        "# retained\n\
         custom=42\n\
         [embeddings]\n\
         url='https://example.org/'\n\
         model='text'\n\
         dimensions=2\n\
         [images]\n\
         images_output_dir='pixels'\n"
      in
      write file source;
      let text = (ok (Bushel_config.load_file file)).embeddings in
      ignore (ok (Bushel_config.configure_image_embeddings ~file image));
      let config = ok (Bushel_config.load_file file) in
      check "image configure preserves text model" (config.embeddings = text);
      check "image configure anchors model path"
        (config.image_embeddings
        = Some
            {
              image with
              model_dir = Filename.concat (Sys.getcwd ()) image.model_dir;
            });
      check "image configure preserves other settings"
        (config.images_output_dir = "pixels"
        && String.starts_with ~prefix:"# retained" (read file));
      let bytes = read file in
      check "invalid vision dimensions rejected atomically"
        (Result.is_error
           (Bushel_config.configure_image_embeddings ~file
              { image with dimensions = 32 })
        && read file = bytes);
      ignore
        (ok
           (Bushel_config.configure_embeddings ~file
              (Bushel_config.Mlx
                 { image with model_dir = "text-model"; dimensions = 32 })));
      check "text configure preserves image settings"
        ((ok (Bushel_config.load_file file)).image_embeddings
       = config.image_embeddings);
      write file
        "[image_embeddings]\n\
         backend='mlx'\n\
         model_dir='relative/model'\n\
         dimensions=64\n";
      let parsed =
        Option.get (ok (Bushel_config.load_file file)).image_embeddings
      in
      check "image model paths resolve beside config"
        (parsed.model_dir
        = Filename.concat (Filename.dirname file) "relative/model");
      check "image instruction defaults independently"
        (parsed.query_instruction
         = Bushel_config.default_image_query_instruction
        && parsed.query_instruction <> Bushel_config.default_query_instruction);
      check "decoded vision dimensions are validated"
        (Result.is_error
           (Bushel_config.of_string
              "[image_embeddings]\n\
               backend='mlx'\n\
               model_dir='model'\n\
               dimensions=32\n")));
  print_endline "image configuration isolation and validation passed"
