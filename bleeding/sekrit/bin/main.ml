open Cmdliner

let root =
  Arg.(
    value
    & opt (some string) None
    & info [ "store" ] ~docv:"DIR"
        ~doc:
          "Encrypted store directory. Defaults to SEKRIT_STORE_DIR or XDG data.")

let key =
  Arg.(
    value
    & opt (some string) None
    & info [ "key-file" ] ~docv:"FILE"
        ~doc:
          "Private master key outside the store. Defaults to SEKRIT_KEY_FILE \
           or XDG config.")

let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME")

let file =
  Arg.(required & opt (some string) None & info [ "file" ] ~docv:"FILE")

let replace =
  Arg.(
    value & flag & info [ "replace" ] ~doc:"Replace an existing named secret.")

let run root key action =
  try
    Eio_main.run (fun env ->
        Eio.Switch.run (fun sw ->
            let fs = Eio.Stdenv.fs env in
            let root =
              Eio.Path.(
                fs
                /
                match root with
                | Some path -> path
                | None -> Sekrit.Paths.root ())
            in
            let key_file =
              Eio.Path.(
                fs
                /
                match key with
                | Some path -> path
                | None -> Sekrit.Paths.key_file ())
            in
            action ~fs ~sw ~root ~key_file));
    0
  with
  | Sekrit.Error message | Eioext.Native.Error message ->
      Printf.eprintf "Error: %s\n" message;
      1
  | Eioext.Native.Busy _ ->
      prerr_endline "Error: Secret store is busy";
      1
  | Eio.Io _ | Unix.Unix_error _ | Sys_error _ ->
      prerr_endline "Error: Cannot access secret storage";
      1

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let init =
  command "init" "Initialize encrypted storage and its private master key."
    Term.(
      const (fun root key ->
          run root key (fun ~fs:_ ~sw:_ ~root ~key_file ->
              Sekrit_eio.Store.init ~root ~key_file;
              print_endline "Encrypted secret store initialized."))
      $ root $ key)

let with_store root key action =
  run root key (fun ~fs ~sw ~root ~key_file ->
      let store = Sekrit_eio.Store.open_ ~sw ~root ~key_file in
      action ~fs store)

let list =
  command "list" "List secret names without their values."
    Term.(
      const (fun root key ->
          with_store root key (fun ~fs:_ store ->
              List.iter print_endline (Sekrit_eio.Store.list store)))
      $ root $ key)

let put =
  command "put" "Encrypt a secret from a private single-line file."
    Term.(
      const (fun root key name file replace ->
          with_store root key (fun ~fs store ->
              let value =
                Sekrit_eio.read_file ~fs file |> Sekrit.Secret.of_string
              in
              Sekrit_eio.Store.put store ~name ~replace value;
              Printf.printf "Saved sekrit:%s\n" name))
      $ root $ key $ name $ file $ replace)

let remove =
  command "remove" "Remove a named secret."
    Term.(
      const (fun root key name ->
          with_store root key (fun ~fs:_ store ->
              Sekrit_eio.Store.remove store name;
              print_endline "Secret removed."))
      $ root $ key $ name)

let export =
  command "export" "Write a plaintext secret to a new private file."
    Term.(
      const (fun root key name file ->
          with_store root key (fun ~fs store ->
              match Sekrit_eio.Store.find store name with
              | None -> raise (Sekrit.Error "Named secret is not configured")
              | Some value ->
                  let path = Eio.Path.native_exn Eio.Path.(fs / file) in
                  Eio_unix.run_in_systhread (fun () ->
                      Sekrit.Secret.with_value value
                        (Eioext.Native.create_private path));
                  print_endline "Private plaintext file created."))
      $ root $ key $ name $ file)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "sekrit"
             ~doc:"Encrypted local secrets and reusable references.")
          [ init; list; put; remove; export ]))
