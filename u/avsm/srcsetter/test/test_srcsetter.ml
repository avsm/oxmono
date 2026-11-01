open Eio.Std

let check name condition = if not condition then failwith name
let save path data = Eio.Path.save ~create:(`Or_truncate 0o644) path data

let rec remove path =
  match Eio.Path.kind ~follow:false path with
  | `Directory ->
      List.iter
        (fun name -> remove Eio.Path.(path / name))
        (Eio.Path.read_dir path);
      Eio.Path.rmdir path
  | _ -> Eio.Path.unlink path

let test_workers () =
  let started = ref false in
  let incremental =
    Seq.cons 0 (fun () ->
        check "sequence iteration remains incremental" !started;
        Seq.Nil)
  in
  Srcsetter_cmd.iter_seq_p ~max_fibers:1 (fun _ -> started := true) incremental;
  let active = ref 0 and peak = ref 0 and seen = ref [] in
  Srcsetter_cmd.iter_seq_p ~max_fibers:2
    (fun n ->
      incr active;
      peak := max !peak !active;
      Eio.Fiber.yield ();
      seen := n :: !seen;
      decr active)
    (List.to_seq [ 0; 1; 2; 3; 4 ]);
  check "bounded workers" (!peak = 2 && !active = 0);
  check "every item processed" (List.sort compare !seen = [ 0; 1; 2; 3; 4 ]);
  let forced = ref false in
  let seq () =
    forced := true;
    Seq.Nil
  in
  check "invalid limit fails before enumerating"
    (match Srcsetter_cmd.iter_seq_p ~max_fibers:0 ignore seq with
    | () -> false
    | exception Invalid_argument _ -> not !forced);
  let exception Worker_failed in
  let pending, _ = Promise.create () in
  check "worker failures cancel and join siblings"
    (match
       Srcsetter_cmd.iter_seq_p ~max_fibers:2
         (function
           | 0 ->
               incr active;
               Fun.protect
                 ~finally:(fun () -> decr active)
                 (fun () -> Promise.await pending)
           | _ -> raise Worker_failed)
         (List.to_seq [ 0; 1 ])
     with
    | () -> false
    | exception Worker_failed -> !active = 0)

let test_pipeline env root =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let src_dir = Eio.Path.(root / "Input Images") in
  let dst_dir = Eio.Path.(root / "Output Images") in
  let filter name = String.lowercase_ascii (Filename.extension name) = ".jpg" in
  (* Construct before creating the source to check that traversal stays lazy. *)
  let files = Srcsetter_cmd.file_seq ~filter src_dir in
  Eio.Path.mkdirs ~perm:0o755 Eio.Path.(src_dir / "Album Space");
  save Eio.Path.(src_dir / "Album Space/Photo.JPG") "800 600\n";
  let gif = "64 32\n" ^ String.make (1024 * 1024) 'g' in
  save Eio.Path.(src_dir / "Motion.GIF") gif;
  save Eio.Path.(src_dir / "ignored.txt") "not an image";
  Eio.Path.symlink ~link_to:"Album Space/Photo.JPG"
    Eio.Path.(src_dir / "linked.jpg");
  Eio.Path.symlink ~link_to:"." Eio.Path.(src_dir / "loop");
  let found = List.of_seq files in
  check "filtered traversal skips symlinks" (List.length found = 1);
  check "traversed path remains usable"
    (Eio.Path.load (List.hd found) = "800 600\n");
  let run ?(max_fibers = 2) src_dir dst_dir =
    Srcsetter_cmd.run ~proc_mgr ~src_dir ~dst_dir ~max_fibers
      ~img_widths:[ 100; 400; 1600 ] ()
  in
  let entries = run src_dir dst_dir in
  check "source-relative origins in discovery order"
    (List.map Srcsetter.origin entries
    = [ "Album Space/Photo.JPG"; "Motion.GIF" ]);
  let photo = List.hd entries in
  check "nested output filename"
    (Srcsetter.name photo = "album space/photo.webp");
  check "base dimensions" (Srcsetter.dims photo = (800, 600));
  check "responsive variants"
    (Srcsetter.MS.bindings (Srcsetter.variants photo)
    = [
        ("album space/photo.100.webp", (100, 75));
        ("album space/photo.400.webp", (400, 300));
      ]);
  let animation = List.nth entries 1 in
  check "GIF metadata"
    (Srcsetter.dims animation = (64, 32)
    && Srcsetter.MS.cardinal (Srcsetter.variants animation) = 0);
  check "GIF copied byte for byte"
    (Eio.Path.load Eio.Path.(dst_dir / "motion.gif") = gif);
  let index = Eio.Path.load Eio.Path.(dst_dir / "index.json") in
  check "JSON round trip" (Srcsetter.list_of_json index = Ok entries);
  let variant = Eio.Path.(dst_dir / "album space/photo.100.webp") in
  save variant "";
  check "invalid output repaired" (run src_dir dst_dir = entries);
  check "repaired dimensions" (Eio.Path.load variant = "100 75\n");
  check "stable index across worker limits"
    (run ~max_fibers:1 src_dir dst_dir = entries
    && Eio.Path.load Eio.Path.(dst_dir / "index.json") = index);
  (* Opened directory capabilities have their own native roots. Origins must
     still describe paths relative to the supplied source, including '.'. *)
  Eio.Path.with_subtree src_dir (fun source ->
      Eio.Path.with_subtree dst_dir (fun destination ->
          check "opened directory capabilities"
            (run Eio.Path.(source / ".") destination = entries)));
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Process.parse_out proc_mgr Eio.Buf_read.line [ "pwd"; "-P" ] in
  check "absolute source and destination paths"
    (run Eio.Path.(fs / cwd / native_exn src_dir) Eio.Path.(fs / cwd / native_exn dst_dir)
    = entries);
  let empty = Eio.Path.(root / "Empty") in
  Eio.Path.mkdir ~perm:0o755 empty;
  let empty_out = Eio.Path.(root / "New Empty Output") in
  check "empty source creates its index" (run empty empty_out = []);
  check "empty index"
    (Srcsetter.list_of_json (Eio.Path.load Eio.Path.(empty_out / "index.json"))
    = Ok []);
  check "invalid run limit"
    (match run ~max_fibers:0 src_dir dst_dir with
    | _ -> false
    | exception Invalid_argument _ -> true)

let () =
  Eio_main.run @@ fun env ->
  test_workers ();
  let root = Eio.Path.(Eio.Stdenv.cwd env / "test-work") in
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () -> test_pipeline env root);
  print_endline "srcsetter: Eio paths, workers, copies and pipeline passed"
