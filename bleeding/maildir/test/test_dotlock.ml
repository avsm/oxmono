module Dotlock = Maildir.Dotlock

let check predicate message = if not predicate then failwith message
let exists path = try ignore (Unix.lstat path); true with
  Unix.Unix_error (Unix.ENOENT,_,_) -> false
let lost label f =
  match f () with
  | exception Dotlock.Lost _ -> ()
  | _ -> failwith (label ^ " accepted")
let run fs =
  let root=Filename.temp_file "maildir-dotlock-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let path=Filename.concat root "dovecot-uidlist.lock" in
  let lock=Eio.Path.(fs / path) in
  let moved=Filename.concat root "old-lock" in
  Fun.protect ~finally:(fun () ->
    Unix.chmod root 0o700;
    List.iter (fun p -> if exists p then Unix.unlink p) [path;moved];
    Unix.rmdir root) (fun () ->
    let stale=Dotlock.with_lock lock (fun refresh ->
      check (exists path) "lock not created";
      Unix.utimes path 1.0 1.0;
      let contents=In_channel.with_open_bin path In_channel.input_all in
      Eio.Fiber.pair refresh refresh |> ignore;
      check ((Unix.stat path).Unix.st_mtime>1.0)
        "refresh did not advance mtime";
      check (In_channel.with_open_bin path In_channel.input_all=contents)
        "concurrent refresh altered lock owner";
      (try Dotlock.with_lock lock (fun _ -> failwith "nested lock admitted")
       with Dotlock.Busy _ -> ());
      refresh) in
    check (not (exists path)) "normal return leaked lock";
    lost "escaped refresh" stale;
    (try Dotlock.with_lock lock (fun _ -> raise Exit) with Exit -> ());
    check (not (exists path)) "exception leaked lock";
    let entered,release=Eio.Promise.create () in
    Eio.Fiber.first
      (fun () -> Dotlock.with_lock lock (fun _ ->
        Eio.Promise.resolve release ();
        Eio.Fiber.await_cancel ()))
      (fun () -> Eio.Promise.await entered);
    check (not (exists path)) "cancellation leaked lock";
    Dotlock.with_lock lock (fun refresh -> refresh ());
    let replace () =
      Unix.rename path moved;
      Out_channel.with_open_bin path (fun out ->
        output_string out "replacement\n") in
    lost "replacement lock" (fun () ->
      Dotlock.with_lock lock (fun refresh -> replace (); refresh ()));
    check (In_channel.with_open_bin path In_channel.input_all="replacement\n")
      "cleanup removed replacement";
    Unix.unlink path;
    Unix.unlink moved;
    lost "replacement at callback return" (fun () ->
      Dotlock.with_lock lock (fun _ -> replace ()));
    check (exists path) "return cleanup removed replacement";
    Unix.unlink path;
    Unix.unlink moved;
    (try Dotlock.with_lock lock (fun _ -> replace (); raise Exit)
     with Exit -> ());
    check (exists path) "exception cleanup removed replacement";
    Unix.unlink path;
    Unix.unlink moved;
    lost "deleted lock refresh" (fun () ->
      Dotlock.with_lock lock (fun refresh -> Unix.unlink path; refresh ()));
    lost "deleted lock at callback return" (fun () ->
      Dotlock.with_lock lock (fun _ -> Unix.unlink path));
    check (not (exists path)) "deleted lock recreated";
    Dotlock.with_lock lock (fun _ ->
      Unix.link path moved;
      Unix.utimes moved 1.0 1.0);
    check ((Unix.stat moved).Unix.st_mtime=1.0)
      "callback return rewrote the lock";
    check (not (exists path)) "checked return leaked lock";
    Unix.unlink moved;
    if Unix.geteuid ()<>0 then (
      (try Dotlock.with_lock lock (fun _ -> Unix.chmod root 0o000; raise Exit)
       with Exit -> ());
      Unix.chmod root 0o700;
      check (exists path) "unverifiable lock was removed";
      Unix.unlink path))
let () = Eio_main.run (fun env -> run (Eio.Stdenv.fs env))
