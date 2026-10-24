module M = Apple_mlx_embeddings

let mib = 1024 * 1024

let () =
  if Array.length Sys.argv <> 2 || Sys.argv.(1) = "" then
    failwith "Usage: test_memory MODEL_DIR";
  Eio_main.run @@ fun _ ->
  let model = M.load ~model_dir:Sys.argv.(1) ~max_tokens:512 in
  Fun.protect
    ~finally:(fun () -> M.close model)
    (fun () ->
      let anchor = [| "A compiler translates programs." |] in
      let expected = M.embed model ~dimensions:64 anchor in
      let baseline = M.memory_stats () in
      assert (baseline.cache_limit_bytes <= 256 * mib);
      let count =
        match Sys.getenv_opt "APPLE_MLX_MEMORY_TEST_BATCHES" with
        | None -> 64
        | Some value -> int_of_string value
      in
      if count < 64 || count > 10000 then
        invalid_arg "Memory test batches must be between 64 and 10000";
      for batch = 0 to count - 1 do
        let width = 32 + (batch * 73 mod 480) in
        let input = String.concat " " (List.init width (fun _ -> "compiler")) in
        ignore (M.embed model ~dimensions:64 (Array.make 4 input));
        let memory = M.memory_stats () in
        assert (memory.cache_limit_bytes <= 256 * mib);
        (* One released buffer can cross the cache threshold until the next
           allocation. This workload's largest temporary is below 256 MiB. *)
        assert (memory.cache_bytes <= 512 * mib);
        (* Live tensors must return to the resident model, not grow per batch.
           Allow allocator rounding and delayed command completion. *)
        assert (memory.active_bytes <= baseline.active_bytes + (128 * mib));
        if batch mod 16 = 0 || batch = count - 1 then
          Printf.printf "batch=%d active=%d cache=%d peak=%d limit=%d\n%!" batch
            memory.active_bytes memory.cache_bytes memory.peak_bytes
            memory.cache_limit_bytes
      done;
      let actual = M.embed model ~dimensions:64 anchor in
      for i = 0 to 63 do
        assert (abs_float (actual.{0, i} -. expected.{0, i}) < 1e-6)
      done);
  print_endline "Repeated MLX batches retain bounded free and active memory"
