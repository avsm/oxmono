(* A deterministic ImageMagick substitute: fixtures contain width and height.
   The integration test still exercises Eio process spawning and file I/O. *)
let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let args = Array.to_list Sys.argv in
  let read path =
    Scanf.sscanf
      (Eio.Path.load Eio.Path.(fs / path))
      "%d %d"
      (fun w h -> (w, h))
  in
  match List.tl args with
  | [ "-ping"; "-format"; "%w %h"; path ] ->
      assert (String.ends_with ~suffix:"[0]" path);
      let w, h = read (String.sub path 0 (String.length path - 3)) in
      Printf.printf "%d %d" w h
  | [
   src;
   "-auto-orient";
   "-thumbnail";
   width;
   "-quality";
   "100";
   "-gravity";
   "center";
   "-extent";
   extent;
   dst;
  ] ->
      assert (width = extent);
      let w, h = read src in
      let target = Scanf.sscanf width "%dx" Fun.id in
      Eio.Path.save ~create:(`Or_truncate 0o644)
        Eio.Path.(fs / dst)
        (Printf.sprintf "%d %d\n" target (h * target / w))
  | _ -> failwith "unexpected ImageMagick arguments"
