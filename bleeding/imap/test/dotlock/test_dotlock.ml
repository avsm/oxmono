let check predicate message = if not predicate then failwith message
let exists path = try ignore (Unix.lstat path); true with
  Unix.Unix_error (Unix.ENOENT,_,_) -> false
let run () =
  let root=Filename.temp_file "imap-dotlock-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let path=Filename.concat root "dovecot-uidlist.lock" in
  let moved=Filename.concat root "old-lock" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun p -> if exists p then Unix.unlink p) [path;moved];
    Unix.rmdir root) (fun () ->
    let stale=Dotlock.with_lock path (fun refresh ->
      check (exists path) "lock not created";
      Unix.utimes path 1.0 1.0;
      let contents=In_channel.with_open_bin path In_channel.input_all in
      Eio.Fiber.pair refresh refresh |> ignore;
      check ((Unix.stat path).Unix.st_mtime>1.0) "refresh did not advance mtime";
      check (In_channel.with_open_bin path In_channel.input_all=contents)
        "concurrent refresh altered lock owner";
      (try Dotlock.with_lock path (fun _ -> failwith "nested lock admitted")
       with Dotlock.Busy _ -> ());
      refresh) in
    check (not (exists path)) "normal return leaked lock";
    (match stale () with
     | exception Failure _ -> ()
     | _ -> failwith "escaped refresh remained valid");
    (try Dotlock.with_lock path (fun _ -> raise Exit) with Exit -> ());
    check (not (exists path)) "exception leaked lock";
    let entered,release=Eio.Promise.create () in
    Eio.Fiber.first
      (fun () -> Dotlock.with_lock path (fun _ ->
        Eio.Promise.resolve release ();
        Eio.Fiber.await_cancel ()))
      (fun () -> Eio.Promise.await entered);
    check (not (exists path)) "cancellation leaked lock";
    Dotlock.with_lock path (fun refresh -> refresh ());
    let replace () =
      Unix.rename path moved;
      Out_channel.with_open_bin path (fun out -> output_string out "replacement\n") in
    (match Dotlock.with_lock path (fun refresh -> replace (); refresh ()) with
     | exception Failure _ -> ()
     | _ -> failwith "replacement lock accepted");
    check (In_channel.with_open_bin path In_channel.input_all="replacement\n")
      "cleanup removed replacement";
    Unix.unlink path;
    Unix.unlink moved;
    (match Dotlock.with_lock path (fun _ -> replace ()) with
     | exception Failure _ -> ()
     | _ -> failwith "replacement at callback return accepted");
    check (exists path) "return cleanup removed replacement";
    Unix.unlink path;
    Unix.unlink moved;
    (try Dotlock.with_lock path (fun _ -> replace (); raise Exit) with Exit -> ());
    check (exists path) "exception cleanup removed replacement")
let () = Eio_main.run (fun _ -> run ())
