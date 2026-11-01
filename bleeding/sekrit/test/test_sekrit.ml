let check message value = if not value then failwith message
let value secret = Sekrit.Secret.with_value secret Fun.id

let refused f =
  match f () with
  | _ -> false
  | exception (Sekrit.Error _ | Eioext.Native.Error _ | Invalid_argument _) -> true

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let base = Filename.temp_file "sekrit-test-" "" in
  Unix.unlink base;
  Unix.mkdir base 0o700;
  Fun.protect
    ~finally:(fun () -> remove base)
    (fun () ->
      let root = Filename.concat base "store" in
      let key_file = Filename.concat base "keys/master.key" in
      let store = Sekrit.Store.v ~root ~key_file () in
      Sekrit.Store.init store;
      let key = Eioext.Native.read_private ~max_bytes:32 key_file in
      check "256-bit random key" (String.length key = 32);
      Sekrit.Store.init store;
      check "init preserves key"
        (Eioext.Native.read_private ~max_bytes:32 key_file = key);
      let secret = "private-fixture-value" in
      Sekrit.Store.put store ~name:"mail/password" ~replace:false
        (Sekrit.Secret.of_string secret);
      check "roundtrip"
        (Option.map value (Sekrit.Store.find store "mail/password")
        = Some secret);
      check "names only" (Sekrit.Store.list store = [ "mail/password" ]);
      check "no value in printer"
        (Format.asprintf "%a" Sekrit.Secret.pp (Sekrit.Secret.of_string secret)
        = "<secret>");
      let vault = Filename.concat root "vault.sekrit" in
      let ciphertext = Eioext.Native.read_private ~max_bytes:65536 vault in
      check "encrypted at rest" (not (contains ciphertext secret));
      check "name encrypted" (not (contains ciphertext "mail/password"));
      check "explicit replacement required"
        (refused (fun () ->
             Sekrit.Store.put store ~name:"mail/password" ~replace:false
               (Sekrit.Secret.of_string "other")));
      let source =
        Sekrit.Source.v
          ~backend:(Sekrit.Store.backend store)
          (Sekrit.Reference.named "mail/password")
      in
      let snapshot = Sekrit.Source.snapshot source in
      Sekrit.Store.put store ~name:"mail/password" ~replace:true
        (Sekrit.Secret.of_string "rotated");
      check "live references refresh"
        (value (Sekrit.Source.get source) = "rotated");
      check "explicit snapshot stable"
        (value (Sekrit.Source.get snapshot) = secret);
      Sekrit.Store.put store ~name:"mail/password" ~replace:true
        (Sekrit.Secret.of_string secret);
      let ciphertext2 = Eioext.Native.read_private ~max_bytes:65536 vault in
      check "fresh nonce" (ciphertext <> ciphertext2);
      let altered = Bytes.of_string ciphertext2 in
      Bytes.set altered
        (Bytes.length altered - 1)
        (Char.chr
           (Char.code (Bytes.get altered (Bytes.length altered - 1)) lxor 1));
      Eioext.Native.atomic_write vault (Bytes.to_string altered);
      check "authentication rejects tampering"
        (refused (fun () -> Sekrit.Store.list store));
      Eioext.Native.atomic_write vault ciphertext2;
      let wrong_key = Filename.concat base "keys/other.key" in
      Sekrit.Key.create wrong_key;
      check "wrong key rejected"
        (refused (fun () ->
             Sekrit.Sealed_file.read ~key_file:wrong_key
               ~context:"sekrit/vault/v1" vault));
      check "wrong context rejected"
        (refused (fun () ->
             Sekrit.Sealed_file.read ~key_file ~context:"another/context" vault));
      Unix.rename key_file (key_file ^ ".saved");
      check "missing key never regenerated"
        (refused (fun () -> Sekrit.Store.init store));
      check "key remains missing" (not (Sys.file_exists key_file));
      Unix.rename (key_file ^ ".saved") key_file;
      check "key cannot live in store"
        (refused (fun () ->
             Sekrit.Store.init
               (Sekrit.Store.v ~root ~key_file:(Filename.concat root "key") ())));
      let private_path = Filename.concat base "private" in
      Eioext.Native.create_private private_path "  spaces preserved  \r\n";
      check "password whitespace preserved"
        (Sekrit.read_file private_path = "  spaces preserved  ");
      Unix.chmod private_path 0o644;
      check "public file refused"
        (refused (fun () -> Sekrit.read_file private_path));
      Unix.chmod private_path 0o600;
      let alias = Filename.concat base "alias" in
      Unix.symlink private_path alias;
      check "symlink refused" (refused (fun () -> Sekrit.read_file alias));
      let fifo = Filename.concat base "fifo" in
      Unix.mkfifo fifo 0o600;
      check "FIFO refused without blocking"
        (refused (fun () -> Sekrit.read_file fifo));
      check "bounded read"
        (refused (fun () -> Sekrit.read_file ~max_bytes:4 private_path));
      Eioext.Native.atomic_write private_path "one\ntwo\n";
      check "multiline refused"
        (refused (fun () -> Sekrit.read_file private_path));
      Eioext.Native.atomic_write private_path "new\n";
      let lock = Filename.concat base "test.lock" in
      Eioext.Native.with_lock ~path:lock (fun () ->
          check "same-process exclusion"
            (match Eioext.Native.with_lock ~path:lock ignore with
            | _ -> false
            | exception Eioext.Native.Busy _ -> true));
      Eioext.Native.with_lock ~path:lock ignore;
      Eio_main.run (fun env ->
          let fs = Eio.Stdenv.fs env in
          Eioext.Native.with_lock ~path:lock (fun () ->
              check "native and Eio locks exclude each other"
                (match Eioext.Path.with_lock Eio.Path.(fs / lock) ignore with
                | _ -> false
                | exception Eioext.Native.Busy _ -> true));
          let stale =
            Filename.concat base
              (Printf.sprintf ".eioext-%d-0" (Unix.getpid ()))
          in
          Eioext.Native.create_private stale "retained stale temporary";
          Eioext.Path.atomic_write Eio.Path.(fs / private_path) "new\n";
          check "stale temporary retained"
            (Eioext.Native.read_private ~max_bytes:100 stale
            = "retained stale temporary");
          check "Eio reader" (Sekrit_eio.read_file ~fs private_path = "new");
          check "Eio FIFO refused"
            (refused (fun () -> Sekrit_eio.read_file ~fs fifo));
          let creation = Eio.Path.(fs / base / "cancelled-creation") in
          let data = String.make (1024 * 1024) 'k' in
          let context, ready = Eio.Promise.create () in
          let finished = ref false in
          Eio.Fiber.both
            (fun () ->
              Fun.protect
                ~finally:(fun () -> finished := true)
                (fun () ->
                  try
                    Eio.Cancel.sub (fun context ->
                        Eio.Promise.resolve ready context;
                        Eioext.Path.create_private creation data)
                  with Eio.Cancel.Cancelled _ -> ()))
            (fun () ->
              let context = Eio.Promise.await context in
              let rec wait () =
                if !finished then ()
                else if Eio.Path.is_file creation then
                  Eio.Cancel.cancel context Exit
                else (
                  Eio.Fiber.yield ();
                  wait ())
              in
              wait ());
          check "cancelled creation leaves complete file"
            (Eioext.Path.read_private ~max_bytes:(String.length data) creation
            = data);
          let closed =
            Eio.Switch.run (fun sw ->
                let store =
                  Sekrit_eio.Store.open_ ~sw
                    ~root:Eio.Path.(fs / root)
                    ~key_file:Eio.Path.(fs / key_file)
                in
                Eio.Fiber.List.iter
                  (fun i ->
                    Sekrit_eio.Store.put store
                      ~name:("parallel/" ^ string_of_int i)
                      ~replace:false
                      (Sekrit.Secret.of_string "parallel-value"))
                  (List.init 32 Fun.id);
                check "concurrent writes retained"
                  (List.length (Sekrit_eio.Store.list store) = 33);
                let cancelled =
                  try
                    Eio.Cancel.sub (fun context ->
                        Eio.Cancel.cancel context Exit;
                        Sekrit_eio.Store.put store ~name:"cancelled"
                          ~replace:false
                          (Sekrit.Secret.of_string "must not be stored"));
                    false
                  with Eio.Cancel.Cancelled _ -> true
                in
                check "cancellation propagates" cancelled;
                check "cancelled write absent"
                  (Sekrit_eio.Store.find store "cancelled" = None);
                store)
          in
          check "switch scopes store lifetime"
            (refused (fun () -> Sekrit_eio.Store.list closed))));
  print_endline
    "Sekrit encryption, references, private files and Eio lifecycle passed"
