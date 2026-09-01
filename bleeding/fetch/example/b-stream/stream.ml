let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->

  let path = Eio.Path.(Eio.Stdenv.fs env / Filename.get_temp_dir_name () / "fetch-tutorial.bin") in
  let response = Fetch.get ~sw client (base ^ "/big") in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path (fun file ->
    Eio.Flow.copy (Fetch.body response) file;
    Printf.printf "Saved %s bytes to %s\n"
      (Optint.Int63.to_string (Eio.File.size file)) (snd path));

  let source = Eio.Flow.string_source (String.make 8192 'u') in
  let response =
    Fetch.post ~sw client (base ^ "/upload") ~body:(Fetch.stream ~length:8192L source)
  in
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env)
