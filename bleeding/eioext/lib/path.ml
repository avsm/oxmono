exception Error = Native.Error
exception Busy = Native.Busy

let with_open_private path f =
  let fail message = raise (Native.Error message) in
  let validate (stat : Eio.File.Stat.t) =
    if stat.kind <> `Regular_file then fail "Not a regular file";
    if stat.uid <> Int64.of_int (Unix.getuid ()) || stat.perm land 0o077 <> 0
    then fail "File must be owned and private (mode 0600)"
  in
  let before = Eio.Path.stat ~follow:false path in
  validate before;
  Eio.Path.with_open_in ~follow:false path (fun file ->
      let opened = Eio.File.stat file in
      validate opened;
      if opened.dev <> before.dev || opened.ino <> before.ino then
        fail "File changed while opening";
      f file)

let read_private ~max_bytes path =
  if max_bytes < 0 || max_bytes = max_int then
    invalid_arg "Native: invalid read bound";
  let fail message = raise (Native.Error message) in
  with_open_private path (fun file ->
      let chunk = Cstruct.create (min (max_bytes + 1) 4096) in
      let buffer = Buffer.create 256 in
      let rec loop () =
        let remaining = max_bytes - Buffer.length buffer in
        let chunk =
          Cstruct.sub chunk 0 (min (Cstruct.length chunk) (remaining + 1))
        in
        match Eio.Flow.single_read file chunk with
        | n when n > remaining -> fail "File exceeds size limit"
        | n ->
            Buffer.add_string buffer (Cstruct.to_string ~len:n chunk);
            loop ()
        | exception End_of_file -> Buffer.contents buffer
      in
      loop ())

let fail message = raise (Native.Error message)

let private_stat kind (stat : Eio.File.Stat.t) =
  if
    stat.kind <> kind
    || stat.uid <> Int64.of_int (Unix.getuid ())
    || stat.perm land 0o077 <> 0
  then fail "Storage must be owned and private"

let require_private_dir path =
  private_stat `Directory (Eio.Path.stat ~follow:false path)

let with_parent path f =
  match Eio.Path.split path with
  | None -> invalid_arg "Native: path must name a file"
  | Some (parent, name) -> Eio.Path.with_open_dir parent (fun dir -> f dir name)

let native_fd resource =
  match Eio_unix.Resource.fd_opt resource with
  | Some fd -> fd
  | None ->
      invalid_arg "Native: native filesystem required for durability and locks"

let sync_dir dir =
  Eio.Path.with_open_in ~follow:false dir (fun file ->
      let fd = native_fd file in
      Eio_unix.run_in_systhread (fun () ->
          Eio_unix.Fd.use_exn "fsync directory" fd Unix.fsync))

let create_private path data =
  with_parent path (fun dir name ->
      require_private_dir dir;
      Eio.Fiber.check ();
      Eio.Cancel.protect (fun () ->
          let path = Eio.Path.(dir / name) in
          let created = ref false in
          let complete = ref false in
          Fun.protect
            ~finally:(fun () ->
              if !created && not !complete then Eio.Path.unlink path)
            (fun () ->
              Eio.Path.with_open_out ~follow:false ~create:(`Exclusive 0o600)
                path (fun file ->
                  created := true;
                  Eio.Flow.copy_string data file;
                  Eio.File.sync file;
                  complete := true);
              sync_dir dir)))

let sequence = Atomic.make 0

let atomic_write ?(check = fun () -> ()) path data =
  with_parent path (fun dir name ->
      require_private_dir dir;
      let rec attempt remaining =
        let tmp =
          Eio.Path.(
            dir
            / Printf.sprintf ".eioext-%d-%d" (Unix.getpid ())
                (Atomic.fetch_and_add sequence 1))
        in
        let created = ref false in
        try
          Fun.protect
            ~finally:(fun () ->
              Eio.Cancel.protect (fun () ->
                  if !created then
                    try Eio.Path.unlink tmp
                    with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()))
            (fun () ->
              Eio.Path.with_open_out ~follow:false ~create:(`Exclusive 0o600)
                tmp (fun file ->
                  created := true;
                  Eio.Flow.copy_string data file;
                  Eio.File.sync file);
              check ();
              Eio.Fiber.check ();
              Eio.Cancel.protect (fun () ->
                  Eio.Path.rename tmp Eio.Path.(dir / name);
                  sync_dir dir))
        with
        | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _)
        when (not !created) && remaining > 0
        ->
          attempt (remaining - 1)
      in
      attempt 100)

let with_lock ?wait path f =
  with_parent path (fun dir name ->
      let stat = Eio.Path.stat ~follow:false dir in
      Lock.with_lock ?wait ~device:stat.dev ~inode:stat.ino ~name (fun () ->
          let path = Eio.Path.(dir / name) in
          let validate stat =
            private_stat `Regular_file stat;
            if stat.Eio.File.Stat.nlink <> 1L then
              fail "Lock file must not have hard links"
          in
          let before =
            match Eio.Path.stat ~follow:false path with
            | stat ->
                validate stat;
                Some stat
            | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
          in
          Eio.Path.with_open_out ~follow:false ~create:(`If_missing 0o600) path
            (fun file ->
              let opened = Eio.File.stat file in
              validate opened;
              let same (a : Eio.File.Stat.t) (b : Eio.File.Stat.t) =
                a.dev = b.dev && a.ino = b.ino
              in
              let current = Eio.Path.stat ~follow:false path in
              validate current;
              if
                not
                  (same current opened
                  && Option.fold ~none:true
                       ~some:(fun stat -> same stat opened)
                       before)
              then fail "Lock changed while opening";
              let fd = native_fd file in
              let lock command =
                Eio_unix.Fd.use_exn "lockf" fd (fun fd ->
                    Unix.lockf fd command 0)
              in
              let rec acquire () =
                Eio.Fiber.check ();
                match lock Unix.F_TLOCK with
                | () -> ()
                | exception Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _)
                  ->
                    (match wait with
                    | Some wait -> wait ()
                    | None -> raise (Native.Busy name));
                    acquire ()
                | exception Unix.Unix_error (Unix.EINTR, _, _) -> acquire ()
              in
              acquire ();
              Fun.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () -> lock Unix.F_ULOCK))
                f)))
