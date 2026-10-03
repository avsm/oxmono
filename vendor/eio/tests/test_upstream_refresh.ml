(* Exercise the upstream additions through the OxCaml interfaces and both
   Linux backends, independently of the stock-compiler MDX transcripts. *)

let connect : _ @ portable = fun ~sw net addr ->
  Eio.Net.connect ~sw net addr
    ~bind_to:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    ~options:Eio.Net.Sockopt.[
      (SO_KEEPALIVE, false);
      (SO_KEEPALIVE, true);
    ]

let expect_symlink fn =
  match fn () with
  | _ -> failwith "opened a symlink with follow:false"
  | exception Eio.Io (Eio.Fs.E Eio.Fs.Symlink, _) -> ()

let exercise_files env =
  let name = Filename.temp_file "eio-release-" ".txt" in
  let link = name ^ ".link" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists link then Unix.unlink link;
      Unix.unlink name)
    (fun () ->
      let fs = Eio.Stdenv.fs env in
      let path = Eio.Path.(fs / name) in
      let symlink = Eio.Path.(fs / link) in
      Eio.Path.save ~follow:false ~create:(`Or_truncate 0o600) path "hello";
      Eio.Path.symlink ~link_to:name symlink;
      assert (Eio.Path.load symlink = "hello");
      expect_symlink (fun () -> Eio.Path.load ~follow:false symlink);
      expect_symlink (fun () ->
        Eio.Path.save ~follow:false ~create:(`Or_truncate 0o600) symlink "bad");
      assert (Eio.Path.load ~follow:false path = "hello");
      let fd = Unix.openfile name [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
      Eio.Switch.run (fun sw ->
        let file = Eio_unix.File.import_rw ~sw ~close_unix:true fd in
        Eio.Flow.copy_string "world" file);
      (match Unix.fstat fd with
       | _ -> failwith "import_rw did not close its owned descriptor"
       | exception Unix.Unix_error (Unix.EBADF, _, _) -> ());
      assert (Eio.Path.load path = "world");
      let fd = Unix.openfile name [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Eio.Switch.run (fun sw ->
          let file = Eio_unix.File.import_ro ~sw ~close_unix:false fd in
          let data = Cstruct.create 5 in
          Eio.Flow.read_exact file data;
          assert (Cstruct.to_string data = "world"));
        assert ((Unix.fstat fd).Unix.st_size = 5)))

let exercise env =
  exercise_files env;
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~backlog:1
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let addr = Eio.Net.listening_addr server in
  let client = connect ~sw net addr in
  let peer, _ = Eio.Net.accept ~sw server in
  assert (Eio.Net.getsockopt client Eio.Net.Sockopt.SO_KEEPALIVE);
  Eio.Flow.copy_string "ok" client;
  let received = Cstruct.create 2 in
  Eio.Flow.read_exact peer received;
  assert (Cstruct.to_string received = "ok");
  (* The listener already owns this address, so binding an outbound socket to
     it must fail. This catches backends silently ignoring [bind_to]. *)
  (match Eio.Net.connect ~sw net addr ~bind_to:addr with
   | _ -> failwith "connect ignored the occupied bind address"
   | exception Eio.Io _ -> ());
  let module Env = Eio.Process.Env in
  let original = Env.of_array [| "MESSAGE=old"; "MESSAGE=duplicate"; "REMOVE=yes" |] in
  let updated = Env.override ["MESSAGE", Some "new"; "REMOVE", None] original in
  assert (Env.to_array updated = [| "MESSAGE=new" |]);
  assert (Env.get_opt "MESSAGE" original = Some "old");
  assert (Env.get_opt "" original = None);
  assert (Env.get_opt "INVALID=NAME" original = None);
  let mgr = Eio.Stdenv.process_mgr env in
  let snapshot = Eio.Process.environment mgr in
  assert (Env.get_opt "PATH" snapshot = Eio.Process.getenv_opt mgr "PATH");
  assert (Eio.Process.getenv_opt mgr "PATH" = Sys.getenv_opt "PATH");
  let output = Eio.Process.parse_out (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.take_all ~env:updated
      ["/bin/sh"; "-c"; "printf '%s:%s' \"$MESSAGE\" \"${REMOVE-unset}\""] in
  assert (output = "new:unset")

let () =
  Eio_linux.run exercise;
  Eio_posix.run exercise;
  print_endline "Eio upstream refresh: Linux and POSIX checks passed"
