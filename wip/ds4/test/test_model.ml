module Model = Ds4_cli.Model

let () =
  let dir = Filename.temp_file "ds4-model-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat dir f)) (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () ->
      let touch name =
        let path = Filename.concat dir name in
        Out_channel.with_open_bin path (fun _ -> ());
        path
      in
      let env _ = None in
      let older = touch "DeepSeek-V4.1-Flash-Q4.gguf" in
      assert (Model.resolve ~env ~dir None = Ok older);
      let latest = touch "DeepSeek-V4.1-Flash-1017-IQ2_XXS-Q2_K-imatrix.gguf" in
      assert (Model.resolve ~env ~dir None = Ok latest);
      List.iter
        (fun alias -> assert (Model.resolve ~env ~dir (Some alias) = Ok latest))
        [ "ds41-q2-1017"; "ds41-q2"; "v41-q2"; "ds41f-q2" ];
      assert (Model.resolve ~env ~dir (Some "ds41-q4") = Ok older);
      assert (Model.resolve ~env:(fun _ -> Some older) ~dir None = Ok older);
      Sys.remove latest;
      assert (Result.is_error (Model.resolve ~env ~dir (Some "ds41-q2")));
      assert (Model.resolve ~env ~dir None = Ok older))
