let check message value = if not value then failwith message

let refused f =
  match f () with
  | _ -> failwith "operation should have been refused"
  | exception (Eioext.Path.Error _ | Eio.Io _) -> ()

let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let main env =
  let base = Filename.temp_file "eioext-path-" "" in
  Unix.unlink base;
  Unix.mkdir base 0o700;
  Fun.protect
    ~finally:(fun () -> remove base)
    (fun () ->
      let fs = Eio.Stdenv.fs env in
      let native name = Filename.concat base name in
      let path name = Eio.Path.(fs / base / name) in
      Eioext.Native.create_private (native "value") "before";
      check "native bounded read"
        (Eioext.Native.read_private ~max_bytes:6 (native "value") = "before");
      check "Eio bounded read"
        (Eioext.Path.read_private ~max_bytes:6 (path "value") = "before");
      refused (fun () -> Eioext.Path.read_private ~max_bytes:5 (path "value"));
      Unix.chmod (native "value") 0o644;
      refused (fun () -> Eioext.Path.read_private ~max_bytes:6 (path "value"));
      Unix.chmod (native "value") 0o600;
      Unix.symlink "value" (native "alias");
      Unix.mkfifo (native "fifo") 0o600;
      refused (fun () -> Eioext.Path.read_private ~max_bytes:6 (path "alias"));
      refused (fun () -> Eioext.Path.read_private ~max_bytes:6 (path "fifo"));
      refused (fun () -> Eioext.Path.with_lock (path "fifo") ignore);
      Eioext.Native.create_private
        (native (Printf.sprintf ".eioext-%d-0" (Unix.getpid ())))
        "stale";
      Eioext.Path.atomic_write (path "value") "published";
      check "atomic replacement" (Eio.Path.load (path "value") = "published");
      let cancelled =
        try
          Eio.Cancel.sub (fun context ->
              Eioext.Path.atomic_write
                ~check:(fun () -> Eio.Cancel.cancel context Exit)
                (path "value") "must not publish");
          false
        with Eio.Cancel.Cancelled _ -> true
      in
      check "prepublication cancellation propagates" cancelled;
      check "prepublication cancellation preserves target"
        (Eio.Path.load (path "value") = "published");
      check "cancelled temporary cleaned up"
        (List.length
           (List.filter
              (String.starts_with ~prefix:".eioext-")
              (Eio.Path.read_dir Eio.Path.(fs / base)))
        = 1);
      let busy f =
        match f () with
        | _ -> failwith "expected lock contention"
        | exception Eioext.Path.Busy _ -> ()
      in
      Eioext.Native.with_lock ~path:(native "lock") (fun () ->
          busy (fun () -> Eioext.Path.with_lock (path "lock") ignore));
      Eioext.Path.with_lock (path "lock") (fun () ->
          busy (fun () -> Eioext.Native.with_lock ~path:(native "lock") ignore));
      Eioext.Path.with_lock (path "lock") (fun () ->
          match
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.01 (fun () ->
                Eioext.Path.with_lock ~wait:Eio.Fiber.yield (path "lock") ignore)
          with
          | () -> failwith "lock wait should be cancelled"
          | exception Eio.Time.Timeout -> ());
      Eioext.Path.with_lock (path "lock") ignore;
      let executable =
        if Filename.is_relative Sys.executable_name then
          Filename.concat (Sys.getcwd ()) Sys.executable_name
        else Sys.executable_name
      in
      let probe () =
        Eioext.Process.capture
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          [ executable; "--probe-lock"; native "lock" ]
      in
      Eioext.Path.with_lock (path "lock") (fun () ->
          check "cross-process lock held" ((probe ()).stdout = "busy\n");
          Unix.link (native "lock") (native "hardlink");
          refused (fun () -> Eioext.Path.with_lock (path "hardlink") ignore);
          refused (fun () ->
              Eioext.Native.with_lock ~path:(native "hardlink") ignore);
          Unix.unlink (native "hardlink");
          check "refused alias did not release process lock"
            ((probe ()).stdout = "busy\n"));
      check "cross-process lock released" ((probe ()).stdout = "free\n");
      let counter = ref 0 in
      Eio.Fiber.List.iter
        (fun _ ->
          Eioext.Path.with_lock ~wait:Eio.Fiber.yield (path "lock") (fun () ->
              let value = !counter in
              Eio.Fiber.yield ();
              counter := value + 1))
        (List.init 32 Fun.id);
      check "waiting fibers serialize" (!counter = 32));
  print_endline "Eioext filesystem publication, bounds and locks passed"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--probe-lock"; path ] -> (
      try Eioext.Native.with_lock ~path (fun () -> print_endline "free")
      with Eioext.Native.Busy _ -> print_endline "busy")
  | _ -> Eio_main.run main
