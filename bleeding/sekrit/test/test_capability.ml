let check message condition = if not condition then failwith message

let refuses f =
  match f () with
  | _ -> failwith "expected capability refusal"
  | exception (Eio.Io _ | Sekrit.Error _ | Eioext.Native.Error _) -> ()

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let () =
  let base = Filename.temp_file "sekrit-capability-" "" in
  Unix.unlink base;
  Unix.mkdir base 0o700;
  Fun.protect
    ~finally:(fun () -> remove base)
    (fun () ->
      let allowed = Filename.concat base "allowed" in
      let outside = Filename.concat base "outside" in
      Unix.mkdir allowed 0o700;
      Unix.mkdir outside 0o700;
      let root = Filename.concat allowed "vault" in
      let keys = Filename.concat allowed "keys" in
      let key_file = Filename.concat keys "master.key" in
      let native = Sekrit.Store.v ~root ~key_file () in
      Sekrit.Store.init native;
      let outside_secret = Filename.concat outside "secret" in
      Eioext.Native.create_private outside_secret "outside";
      Eio_main.run (fun env ->
          Eio.Switch.run (fun sw ->
              let fs = Eio.Path.open_subtree ~sw Eio.Path.(env#fs / allowed) in
              refuses (fun () -> Sekrit_eio.read_file ~fs "../outside/secret");
              refuses (fun () ->
                  Sekrit_eio.Store.open_ ~sw
                    ~root:Eio.Path.(fs / "vault")
                    ~key_file:Eio.Path.(fs / "../outside/secret"));
              refuses (fun () ->
                  Sekrit_eio.Store.init
                    ~root:Eio.Path.(fs / "../outside/new")
                    ~key_file:Eio.Path.(fs / "keys/master.key"));
              check "init cannot create outside the capability"
                (not (Sys.file_exists (Filename.concat outside "new")));
              Unix.symlink "vault" (Filename.concat allowed "alias");
              refuses (fun () ->
                  Sekrit_eio.Store.open_ ~sw
                    ~root:Eio.Path.(fs / "alias")
                    ~key_file:Eio.Path.(fs / "keys/master.key"));
              Unix.symlink "master.key" (Filename.concat keys "alias");
              let alias =
                Sekrit_eio.Store.open_ ~sw
                  ~root:Eio.Path.(fs / "vault")
                  ~key_file:Eio.Path.(fs / "keys/alias")
              in
              refuses (fun () -> Sekrit_eio.Store.list alias);
              let pinned =
                Sekrit_eio.Store.open_ ~sw
                  ~root:Eio.Path.(fs / "vault")
                  ~key_file:Eio.Path.(fs / "keys/master.key")
              in
              let moved_root = Filename.concat allowed "vault-moved" in
              let moved_keys = Filename.concat allowed "keys-moved" in
              Unix.rename root moved_root;
              Unix.rename keys moved_keys;
              let replacement = Sekrit.Store.v ~root ~key_file () in
              Sekrit.Store.init replacement;
              Sekrit_eio.Store.put pinned ~name:"pinned" ~replace:false
                (Sekrit.Secret.of_string "kept in opened directories");
              check "replacement vault is untouched"
                (Sekrit.Store.list replacement = []);
              let original =
                Sekrit.Store.v ~root:moved_root
                  ~key_file:(Filename.concat moved_keys "master.key")
                  ()
              in
              check "opened directory capabilities survive rename"
                (Sekrit.Store.list original = [ "pinned" ]))));
  print_endline
    "Sekrit directory confinement, symlinks and pinned handles passed"
