module M = Apple_mlx_embeddings

let texts =
  [|
    "Instruct: Given a web search query, retrieve relevant passages that \
     answer the query\n\
     Query:What is the capital of China?";
    "The capital of China is Beijing.";
    "Gravity attracts massive objects to each other.";
    "北京是中国的首都。";
    "A compiler translates source code into executable instructions. 🐫";
  |]

let dot a row b other =
  let total = ref 0. in
  for column = 0 to Bigarray.Array2.dim2 a - 1 do
    total := !total +. (a.{row, column} *. b.{other, column})
  done;
  !total

let expect_invalid f =
  match f () with
  | _ -> failwith "Expected invalid arguments"
  | exception Invalid_argument _ -> ()

let check_vectors vectors =
  for row = 0 to Bigarray.Array2.dim1 vectors - 1 do
    for column = 0 to Bigarray.Array2.dim2 vectors - 1 do
      assert (Float.is_finite vectors.{row, column})
    done;
    assert (abs_float (dot vectors row vectors row -. 1.) < 1e-5)
  done

let save vectors path =
  let out = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out out)
    (fun () ->
      output_string out "[";
      for row = 0 to Bigarray.Array2.dim1 vectors - 1 do
        if row > 0 then output_char out ',';
        output_char out '[';
        for column = 0 to Bigarray.Array2.dim2 vectors - 1 do
          if column > 0 then output_char out ',';
          Printf.fprintf out "%.9g" vectors.{row, column}
        done;
        output_char out ']'
      done;
      output_string out "]\n")

let () =
  if Array.length Sys.argv < 2 || Sys.argv.(1) = "" then
    failwith
      "Set APPLE_MLX_TEST_MODEL to a local Qwen3-Embedding-0.6B directory";
  Eio_main.run @@ fun env ->
  assert (M.available ());
  if Sys.getenv_opt "APPLE_MLX_TEST_CACHE_RECOVERY" = Some "1" then (
    let cache = Sys.getenv "XDG_CACHE_HOME" in
    let blocked = Filename.concat cache "apple-mlx-embeddings" in
    if Sys.file_exists blocked then
      failwith "Cache recovery test requires an empty XDG_CACHE_HOME";
    Out_channel.with_open_bin blocked (fun out -> output_string out "blocked");
    Fun.protect
      ~finally:(fun () -> Sys.remove blocked)
      (fun () ->
        match M.load ~model_dir:Sys.argv.(1) ~max_tokens:512 with
        | model ->
            M.close model;
            failwith "Blocked shader cache unexpectedly loaded"
        | exception Failure _ -> ()));
  let model = M.load ~model_dir:Sys.argv.(1) ~max_tokens:512 in
  Fun.protect
    ~finally:(fun () -> M.close model)
    (fun () ->
      let vectors = M.embed model ~dimensions:1024 texts in
      if Array.length Sys.argv > 2 then save vectors Sys.argv.(2);
      check_vectors vectors;
      Array.iteri
        (fun row text ->
          let single = M.embed model ~dimensions:1024 [| text |] in
          let similarity = dot vectors row single 0 in
          Printf.printf "batch/single row %d cosine %.9f\n%!" row similarity;
          (* Quantized BF16 kernels also vary between batch shapes in the
             reference implementation. Keep the cosine error below 0.001. *)
          assert (similarity > 0.999))
        texts;
      assert (dot vectors 0 vectors 1 > dot vectors 0 vectors 2);
      let reduced = M.embed model ~dimensions:256 texts in
      check_vectors reduced;
      for row = 0 to Array.length texts - 1 do
        let squared = ref 0. in
        for column = 0 to 255 do
          squared := !squared +. (vectors.{row, column} ** 2.)
        done;
        let scale = sqrt !squared in
        for column = 0 to 255 do
          assert (
            abs_float (reduced.{row, column} -. (vectors.{row, column} /. scale))
            < 1e-5)
        done
      done;
      expect_invalid (fun () -> M.embed model ~dimensions:1024 [||]);
      expect_invalid (fun () -> M.embed model ~dimensions:31 texts);
      expect_invalid (fun () -> M.embed model ~dimensions:1024 [| "a\000b" |]);
      expect_invalid (fun () ->
          M.embed model ~dimensions:1024 [| String.make 65537 'x' |]);
      (match M.embed model ~dimensions:1024 [| "\255" |] with
      | _ -> failwith "Invalid UTF-8 unexpectedly accepted"
      | exception Failure _ -> ());
      let first, second =
        Eio.Fiber.pair
          (fun () -> M.embed model ~dimensions:1024 [| texts.(1) |])
          (fun () ->
            Gc.full_major ();
            M.embed model ~dimensions:1024 [| texts.(1) |])
      in
      assert (dot first 0 second 0 > 0.99999);
      let long = String.concat " " (List.init 2000 (fun _ -> "compiler")) in
      let truncated =
        M.embed model ~dimensions:256 [| long; long ^ " apples" |]
      in
      assert (dot truncated 0 truncated 1 > 0.99999);
      let cancelled =
        try
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.001 (fun () ->
              ignore (M.embed model ~dimensions:256 [| long |]));
          false
        with Eio.Time.Timeout -> true
      in
      assert cancelled;
      check_vectors (M.embed model ~dimensions:256 [| texts.(1) |]));
  M.close model;
  expect_invalid (fun () -> M.embed model ~dimensions:1024 [| "closed" |]);
  Gc.full_major ();
  print_endline
    "MLX GPU inference, padding, dimensions, Unicode and lifecycle checks \
     passed"
