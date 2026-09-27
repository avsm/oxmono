exception Busy of string
let fail message = failwith ("Imap_maildir: " ^ message)

let with_lock native f =
  let content=Printf.sprintf "%d %s\n" (Unix.getpid ()) (Unix.gethostname ()) in
  let refresh_mutex=Eio.Mutex.create () in
  let acquired=ref None in
  let owned_fd=ref None in
  let check () =
    match !acquired with
    | None -> fail "metadata lock is not held"
    | Some expected ->
        let actual=Unix.lstat native in
        if actual.Unix.st_dev<>expected.Unix.st_dev ||
           actual.Unix.st_ino<>expected.Unix.st_ino then
          fail "Dovecot metadata lock was replaced" in
  let touch () = Eio.Mutex.use_ro refresh_mutex (fun () ->
    Eio.Cancel.protect (fun () -> Eio_unix.run_in_systhread (fun () ->
      check ();
      let fd=Option.get !owned_fd in
      ignore (Unix.lseek fd 0 Unix.SEEK_SET);
      let rec write () =
        match Unix.write_substring fd content 0 1 with
        | 1 -> ()
        | _ -> fail "short lock refresh"
        | exception Unix.Unix_error (Unix.EINTR,_,_) -> write () in
      write ();
      check ()))) in
  Fun.protect ~finally:(fun () -> Eio.Cancel.protect (fun () ->
    let expected= !acquired in
    acquired:=None;
    Eio_unix.run_in_systhread (fun () ->
      Fun.protect ~finally:(fun () ->
        Option.iter Unix.close !owned_fd;
        owned_fd:=None) (fun () ->
        match expected with
        | None -> ()
        | Some expected ->
            match Unix.lstat native with
            | actual when actual.Unix.st_dev=expected.Unix.st_dev &&
                          actual.Unix.st_ino=expected.Unix.st_ino -> Unix.unlink native
            | _ -> ()
            | exception Unix.Unix_error (Unix.ENOENT,_,_) -> ())))) (fun () ->
    let fd=Eio.Cancel.protect (fun () -> Eio_unix.run_in_systhread (fun () ->
      let open_lock () = Unix.openfile native
        [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL;Unix.O_CLOEXEC] 0o600 in
      let fd=try open_lock () with
        | Unix.Unix_error (Unix.EEXIST,_,_) ->
            raise (Busy native) in
      owned_fd:=Some fd;
      acquired:=Some (Unix.fstat fd);
      fd)) in
    Eio_unix.run_in_systhread (fun () ->
      let rec write offset = if offset<String.length content then
        match Unix.write_substring fd content offset (String.length content-offset) with
        | 0 -> fail "short lock write"
        | n -> write (offset+n)
        | exception Unix.Unix_error (Unix.EINTR,_,_) -> write offset in
      write 0);
    let result=f touch in
    touch ();
    result)
