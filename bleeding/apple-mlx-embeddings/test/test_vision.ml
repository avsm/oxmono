module V = Apple_mlx_embeddings.Vision

let instruction = "Represent the user's input."
let texts = [| "A red square."; "A blue rectangle. 北京 🐫" |]

let norm vectors row =
  let value = ref 0. in
  for column = 0 to Bigarray.Array2.dim2 vectors - 1 do
    value := !value +. (vectors.{row, column} ** 2.)
  done;
  sqrt !value

let check vectors =
  for row = 0 to Bigarray.Array2.dim1 vectors - 1 do
    for column = 0 to Bigarray.Array2.dim2 vectors - 1 do
      assert (Float.is_finite vectors.{row, column})
    done;
    assert (abs_float (norm vectors row -. 1.) < 1e-5)
  done

let same a row b other =
  for column = 0 to Bigarray.Array2.dim2 a - 1 do
    assert (abs_float (a.{row, column} -. b.{other, column}) < 1e-5)
  done

let fails f =
  match f () with
  | _ -> failwith "Expected invalid vision input to fail"
  | exception (Failure _ | Invalid_argument _) -> ()

let json out vectors =
  output_char out '[';
  for row = 0 to Bigarray.Array2.dim1 vectors - 1 do
    if row <> 0 then output_char out ',';
    output_char out '[';
    for column = 0 to Bigarray.Array2.dim2 vectors - 1 do
      if column <> 0 then output_char out ',';
      Printf.fprintf out "%.9g" vectors.{row, column}
    done;
    output_char out ']'
  done;
  output_char out ']'

let () =
  if Array.length Sys.argv < 3 || Sys.argv.(1) = "" then
    failwith "Usage: test_vision MODEL_DIR FIXTURES_DIR [OUTPUT_JSON]";
  Eio_main.run @@ fun env ->
  let images =
    Array.map (Filename.concat Sys.argv.(2)) [| "red.png"; "blue.png" |]
  in
  let pattern = Filename.concat Sys.argv.(2) "pattern.png" in
  let model = V.load ~model_dir:Sys.argv.(1) ~max_tokens:512 in
  assert (
    (Apple_mlx_embeddings.memory_stats ()).cache_limit_bytes
    <= 256 * 1024 * 1024);
  Fun.protect
    ~finally:(fun () -> V.close model)
    (fun () ->
      let text = V.embed_text model ~instruction ~dimensions:2048 texts in
      let image = V.embed_images model ~instruction ~dimensions:2048 images in
      let resized =
        V.embed_images model ~instruction ~dimensions:2048 [| pattern |]
      in
      let long = String.concat " " (List.init 2000 (fun _ -> "compiler")) in
      let truncated =
        V.embed_text model ~instruction ~dimensions:256
          [| long; long ^ " apples" |]
      in
      same truncated 0 truncated 1;
      List.iter check [ text; image; resized; truncated ];
      let formats =
        V.embed_images model ~instruction ~dimensions:2048
          (Array.map (Filename.concat Sys.argv.(2)) [| "red.webp"; "red.jpg" |])
      in
      check formats;
      same image 0 formats 0;
      if Array.length Sys.argv > 3 then
        Out_channel.with_open_bin Sys.argv.(3) (fun out ->
            output_string out "{\"texts\":";
            json out text;
            output_string out ",\"images\":";
            json out image;
            output_string out ",\"resized\":";
            json out resized;
            output_string out ",\"truncated\":";
            json out truncated;
            output_string out "}\n");
      Array.iteri
        (fun row path ->
          same image row
            (V.embed_images model ~instruction ~dimensions:2048 [| path |])
            0)
        images;
      let reduced = V.embed_images model ~instruction ~dimensions:256 images in
      check reduced;
      for row = 0 to 1 do
        let squared = ref 0. in
        for column = 0 to 255 do
          squared := !squared +. (image.{row, column} ** 2.)
        done;
        for column = 0 to 255 do
          assert (
            abs_float
              (reduced.{row, column} -. (image.{row, column} /. sqrt !squared))
            < 1e-5)
        done
      done;
      same text 0
        (V.embed_text model ~instruction:" Represent the user's input "
           ~dimensions:2048
           [| texts.(0) |])
        0;
      fails (fun () ->
          V.embed_images model ~instruction ~dimensions:2048
            [| "/nonexistent/vision.png" |]);
      fails (fun () ->
          V.embed_images model ~instruction ~dimensions:2048 [| Sys.argv.(2) |]);
      fails (fun () -> V.embed_text model ~instruction ~dimensions:2048 [||]);
      fails (fun () -> V.embed_text model ~instruction ~dimensions:63 texts);
      fails (fun () -> V.embed_text model ~instruction ~dimensions:32 texts);
      fails (fun () -> V.embed_text model ~instruction ~dimensions:4096 texts);
      fails (fun () ->
          V.embed_text model ~instruction ~dimensions:2048 [| "\255" |]);
      fails (fun () ->
          V.embed_text model ~instruction ~dimensions:2048 [| "<|image_pad|>" |]);
      let left, right =
        Eio.Fiber.pair
          (fun () ->
            V.embed_text model ~instruction ~dimensions:256 [| texts.(0) |])
          (fun () ->
            Gc.full_major ();
            V.embed_text model ~instruction ~dimensions:256 [| texts.(0) |])
      in
      same left 0 right 0;
      assert (
        try
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.001 (fun () ->
              ignore (V.embed_images model ~instruction ~dimensions:256 images));
          false
        with Eio.Time.Timeout -> true);
      check (V.embed_text model ~instruction ~dimensions:256 [| texts.(0) |]));
  V.close model;
  fails (fun () -> V.embed_text model ~instruction ~dimensions:256 texts);
  Gc.full_major ();
  print_endline
    "Vision GPU embeddings, resizing, dimensions and lifecycle passed"
