let check name value = if not value then failwith name
let noop () = ()

let write path value =
  Out_channel.with_open_bin path (fun oc -> output_string oc value)

let cancelled_release (provider : Retrieval.Embedding.provider) release =
  let held, mark_held = Eio.Promise.create () in
  let started, mark_started = Eio.Promise.create () in
  let checks = ref 0 in
  let hold_lock () =
    incr checks;
    if !checks = 2 then (
      Eio.Promise.resolve mark_held ();
      Eio.Promise.await started;
      raise Exit)
  in
  ignore
    (Eio.Fiber.pair
       (fun () ->
         match provider.embed ~task:`Query ~check:hold_lock [ "question" ] with
         | _ -> failwith "Lock test unexpectedly reached native inference"
         | exception Exit -> ())
       (fun () ->
         Eio.Promise.await held;
         Eio.Cancel.sub (fun context ->
             Eio.Cancel.cancel context Exit;
             Eio.Promise.resolve mark_started ();
             (* The provider lock is held until this call suspends. Cleanup
                must wait for it despite the cancelled caller. *)
             release ())))

let () =
  let root = Filename.temp_file "retrieval-mlx-identity" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let file name = Filename.concat root name in
  Fun.protect
    ~finally:(fun () ->
      Array.iter (fun name -> Sys.remove (file name)) (Sys.readdir root);
      Unix.rmdir root)
    (fun () ->
      (* Invalid model payloads prove that identity/status construction never
         attempts to load the native model or allocate GPU weights. *)
      write (file "config.json") "{}";
      write (file "model.safetensors") "fixture weights";
      write (file "tokenizer.json") "tokenizer one";
      Eio_main.run (fun _ ->
          Eio.Switch.run @@ fun sw ->
          let runtime = Retrieval_mlx.Runtime.create ~sw in
          let make ?(dimensions = 32) ?(max_tokens = 128)
              ?(query_instruction = "Find relevant passages.") () =
            Retrieval_mlx.create ~runtime ~model_dir:root ~dimensions
              ~max_tokens ~query_instruction ~check:noop
          in
          List.iter
            (fun query_instruction ->
              check "invalid instructions rejected before filesystem access"
                (try
                   ignore
                     (Retrieval_mlx.create ~runtime ~model_dir:(file "missing")
                        ~dimensions:32 ~max_tokens:128 ~query_instruction
                        ~check:noop);
                   false
                 with Invalid_argument _ -> true))
            [ String.make 8193 'x'; "invalid\000instruction" ];
          let closed =
            Eio.Switch.run (fun scope ->
                let runtime = Retrieval_mlx.Runtime.create ~sw:scope in
                Retrieval_mlx.create ~runtime ~model_dir:root ~dimensions:32
                  ~max_tokens:128 ~query_instruction:"" ~check:noop)
          in
          check "providers cannot outlive their application switch"
            (try
               ignore (closed.embed ~task:`Query ~check:noop [ "query" ]);
               false
             with Invalid_argument message ->
               message = "Embedding runtime is closed");
          let first = make () in
          check "identical local contents reuse their identity"
            (first.model = (make ()).model);
          check "dimensions participate in identity"
            (first.model <> (make ~dimensions:64 ()).model);
          check "token limits participate in identity"
            (first.model <> (make ~max_tokens:256 ()).model);
          check "query instructions participate in identity"
            (first.model <> (make ~query_instruction:"" ()).model);
          check "native batch bound" (first.max_batch = 4);
          List.iter
            (fun inputs ->
              check "invalid batch rejected before model loading"
                (try
                   ignore (first.embed ~task:`Document ~check:noop inputs);
                   false
                 with Invalid_argument _ -> true))
            [ []; List.init 5 (fun _ -> "x"); [ String.make 8193 'x' ] ];
          let tokenizer = file "tokenizer.json" in
          let stat = Unix.stat tokenizer in
          write tokenizer "tokenizer two";
          Unix.utimes tokenizer stat.st_atime stat.st_mtime;
          let changed = make () in
          check "same-size tokenizer edit with preserved mtime changes identity"
            (changed.model <> first.model);
          check "stale providers refuse inference before model loading"
            (try
               ignore (first.embed ~task:`Query ~check:noop [ "question" ]);
               false
             with Failure _ -> true);
          write (file "model.safetensors") "different weights";
          check "weight contents participate in identity"
            ((make ()).model <> changed.model);
          let current = make () in
          check "authority checked before native inference"
            (try
               ignore
                 (current.embed ~task:`Query
                    ~check:(fun () -> raise Exit)
                    [ "question" ]);
               false
             with Exit -> true);
          let vision ?(dimensions = 64) ?(max_tokens = 128)
              ?(query_instruction = "Represent the user's input.") () =
            Retrieval_mlx.Vision.create ~runtime ~model_dir:root ~dimensions
              ~max_tokens ~query_instruction ~check:noop
          in
          let image = vision () in
          check "vision and text identities never collide"
            (image.model <> (make ~dimensions:64 ()).model);
          check "vision model remains lazy and reusable"
            (image.model = (vision ()).model && image.max_batch = 2);
          cancelled_release (make ()) (fun () ->
              Retrieval_mlx.Runtime.release runtime);
          cancelled_release image (fun () ->
              Retrieval_mlx.Runtime.release runtime);
          check "vision settings participate in identity"
            (image.model <> (vision ~max_tokens:256 ()).model
            && image.model <> (vision ~dimensions:128 ()).model
            && image.model <> (vision ~query_instruction:"other" ()).model);
          check "vision minimum dimensions validated before inference"
            (try
               ignore (vision ~dimensions:32 ());
               false
             with Invalid_argument _ -> true);
          check "malformed image input rejected before native model load"
            (try
               ignore (image.embed ~task:`Document ~check:noop [ "[]" ]);
               false
             with Invalid_argument _ -> true);
          let pixel = file "fixture.webp" in
          write pixel "pixel fixture";
          let stale_input =
            Retrieval.Json.encode (Jsont.list Jsont.string)
              [ pixel; Retrieval.Embedding.hash "old pixels" ]
          in
          check "stale pixel revision rejected before native model load"
            (try
               ignore (image.embed ~task:`Document ~check:noop [ stale_input ]);
               false
             with Failure _ -> true);
          write (file "preprocessor_config.json") "changed image processing";
          check "image preprocessing changes model identity"
            (image.model <> (vision ()).model);
          Retrieval_mlx.Runtime.release runtime));
  print_endline "MLX lazy identity, revision and task boundary tests passed"
