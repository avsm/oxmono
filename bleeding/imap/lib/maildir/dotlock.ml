exception Busy of string
exception Lost of string

let same (a : Eio.File.Stat.t) (b : Eio.File.Stat.t) =
  a.dev = b.dev && a.ino = b.ino

let with_lock path f =
  let name = Option.value (Eio.Path.native path) ~default:(snd path) in
  (* Eio has no getpid or gethostname. *)
  let content =
    Printf.sprintf "%d %s\n" (Unix.getpid ()) (Unix.gethostname ()) in
  let mutex = Eio.Mutex.create () in
  let held = ref None in
  let check () =
    match !held with
    | None -> raise (Lost name)
    | Some (_, expected) ->
        match Eio.Path.stat ~follow:false path with
        | actual when same actual expected -> ()
        | _ | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
            raise (Lost name) in
  let refresh () = Eio.Mutex.use_ro mutex (fun () ->
    Eio.Cancel.protect (fun () ->
      check ();
      let file, _ = Option.get !held in
      Eio.File.pwrite_all file ~file_offset:Optint.Int63.zero
        [Cstruct.of_string ~len:1 content];
      check ())) in
  let release () = Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro mutex (fun () ->
      match !held with
      | None -> ()
      | Some (_, expected) ->
          held := None;
          match Eio.Path.stat ~follow:false path with
          | actual when same actual expected ->
              Eio.Path.unlink ~missing_ok:true path
          | _ | exception Eio.Io _ -> ())) in
  Eio.Switch.run ~name:"dotlock" @@ fun sw ->
  Eio.Cancel.protect (fun () ->
    let file =
      try Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) path with
      | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> raise (Busy name) in
    match Eio.File.stat file with
    | stat -> held := Some (file, stat)
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        (try Eio.Path.unlink ~missing_ok:true path with Eio.Io _ -> ());
        Printexc.raise_with_backtrace exn bt);
  match
    let file, _ = Option.get !held in
    Eio.File.pwrite_all file ~file_offset:Optint.Int63.zero
      [Cstruct.of_string content];
    let result = f refresh in
    check ();
    result
  with
  | result ->
      (try release () with Eio.Io _ as exn ->
        Eio.Exn.reraise_with_context exn (Printexc.get_raw_backtrace ())
          "releasing lock %s" name);
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try release () with Eio.Io _ -> ());
      Printexc.raise_with_backtrace exn bt
