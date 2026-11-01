let read_file ?(max_bytes = 65536) ~fs path =
  try
    Eioext.Path.read_private ~max_bytes Eio.Path.(fs / path)
    |> Sekrit.single_line
  with
  | Eioext.Path.Error message -> raise (Sekrit.Error message)
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      raise (Sekrit.Error "Secret file is missing")

let resolve ~fs ~backend reference =
  match Sekrit.Reference.source reference with
  | `File path -> Sekrit.Secret.of_string (read_file ~fs path)
  | `Named _ -> Sekrit.resolve ~backend reference

module Source = struct
  type t = unit -> Sekrit.Secret.t

  let v ~fs ~backend reference () = resolve ~fs ~backend reference
  let get source = source ()

  let snapshot source =
    let value = get source in
    fun () -> value
end

let protect f =
  try f () with
  | Eioext.Path.Error message -> raise (Sekrit.Error message)
  | Eioext.Path.Busy _ -> raise (Sekrit.Error "Secret store is busy")

module Store = struct
  type t = { store : Sekrit.Store.t; mutable closed : bool }

  let prepare ~sw ~root ~key_file =
    Eioext.Path.require_private_dir root;
    let root = Eio.Path.open_dir ~sw root in
    Eioext.Path.require_private_dir root;
    let parent, name =
      match Eio.Path.split key_file with
      | Some pair -> pair
      | None -> invalid_arg "Sekrit key path must name a file"
    in
    Eioext.Path.require_private_dir parent;
    let parent = Eio.Path.open_dir ~sw parent in
    Eioext.Path.require_private_dir parent;
    (* Native names are used only to check separation, never to access data.
       All reads and publications below use the pinned directory capabilities. *)
    let root_name, parent_name =
      Eio_unix.run_in_systhread (fun () ->
          ( Unix.realpath (Eio.Path.native_exn root),
            Unix.realpath (Eio.Path.native_exn parent) ))
    in
    if
      parent_name = root_name
      || String.starts_with ~prefix:(root_name ^ "/") parent_name
    then raise (Sekrit.Error "Master key must be outside encrypted storage");
    (root, Eio.Path.(parent / name))

  let key path =
    let value =
      try Eioext.Path.read_private ~max_bytes:32 path
      with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
        raise
          (Sekrit.Error
             "Master key is missing. Run sekrit init for a new store, or \
              restore the original key for existing encrypted data.")
    in
    if String.length value <> 32 then
      raise (Sekrit.Error "Invalid master key length");
    Sekrit.Secret.of_string value

  let read ~root ~key_file =
    let encrypted =
      Eioext.Path.read_private
        ~max_bytes:((4 * 1024 * 1024) + 36)
        Eio.Path.(root / "vault.sekrit")
    in
    let key = key key_file in
    Eio_unix.run_in_systhread ~label:"sekrit.decrypt" (fun () ->
        Sekrit.Envelope.open_ ~key ~context:"sekrit/vault/v1" encrypted)

  let write ~root ~key_file raw =
    let key = key key_file in
    let encrypted =
      Eio_unix.run_in_systhread ~label:"sekrit.encrypt" (fun () ->
          Sekrit.Envelope.seal ~key ~context:"sekrit/vault/v1" raw)
    in
    Eioext.Path.atomic_write Eio.Path.(root / "vault.sekrit") encrypted

  let open_ ~sw ~root ~key_file =
    protect (fun () ->
        let root, key_file = prepare ~sw ~root ~key_file in
        let mutex = Eio.Mutex.create () in
        let closed = ref false in
        let locked f =
          Eio.Mutex.use_ro mutex (fun () ->
              if !closed then invalid_arg "Sekrit store is closed";
              Eioext.Path.with_lock Eio.Path.(root / ".lock") f)
        in
        let store =
          Sekrit.Store.of_storage
            ~read:(fun () -> locked (fun () -> read ~root ~key_file))
            ~update:(fun change ->
              locked (fun () ->
                  let next = change (read ~root ~key_file) in
                  write ~root ~key_file next))
        in
        let t = { store; closed = false } in
        Eio.Switch.on_release sw (fun () ->
            closed := true;
            t.closed <- true);
        t)

  let run t f =
    protect (fun () ->
        if t.closed then invalid_arg "Sekrit store is closed";
        Eio.Fiber.check ();
        f t.store)

  let find t name = run t (fun store -> Sekrit.Store.find store name)
  let list t = run t Sekrit.Store.list

  let put t ~name ~replace value =
    run t (fun store -> Sekrit.Store.put store ~name ~replace value)

  let remove t name = run t (fun store -> Sekrit.Store.remove store name)
  let backend t = Sekrit.Backend.v ~find:(find t)

  let init ~root ~key_file =
    protect (fun () ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
        let parent =
          match Eio.Path.split key_file with
          | Some (parent, _) -> parent
          | None -> invalid_arg "Sekrit key path must name a file"
        in
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent;
        Eio.Switch.run (fun sw ->
            let root, key_file = prepare ~sw ~root ~key_file in
            Eioext.Path.with_lock
              Eio.Path.(root / ".lock")
              (fun () ->
                if
                  Eio.Path.kind ~follow:false Eio.Path.(root / "vault.sekrit")
                  <> `Not_found
                then ignore (read ~root ~key_file)
                else (
                  (if Eio.Path.kind ~follow:false key_file = `Not_found then
                     let bytes =
                       Eio_unix.run_in_systhread (fun () ->
                           Mirage_crypto_rng_unix.getrandom 32)
                     in
                     Eioext.Path.create_private key_file bytes);
                  write ~root ~key_file "[]"))))
end

let read_reference ?(max_bytes = 65536) ~fs reference =
  match Sekrit.Reference.source (Sekrit.Reference.of_string reference) with
  | `File path -> read_file ~max_bytes ~fs path
  | `Named name ->
      Eio.Switch.run (fun sw ->
          let store =
            Store.open_ ~sw
              ~root:Eio.Path.(fs / Sekrit.Paths.root ())
              ~key_file:Eio.Path.(fs / Sekrit.Paths.key_file ())
          in
          match Store.find store name with
          | None -> raise (Sekrit.Error "Named secret is not configured")
          | Some secret ->
              Sekrit.Secret.with_value secret (fun value ->
                  if String.length value > max_bytes then
                    raise (Sekrit.Error "Secret exceeds size limit");
                  Sekrit.single_line value))

module Sealed_file = struct
  let read ~key_file ~context path =
    protect (fun () ->
        let encrypted =
          Eioext.Path.read_private ~max_bytes:((4 * 1024 * 1024) + 36) path
        in
        let key = Store.key key_file in
        Eio_unix.run_in_systhread ~label:"sekrit.decrypt" (fun () ->
            Sekrit.Envelope.open_ ~key ~context encrypted))

  let write ~key_file ~context path plaintext =
    protect (fun () ->
        let directory, name =
          match Eio.Path.split path with
          | Some pair -> pair
          | None -> invalid_arg "Sekrit encrypted path must name a file"
        in
        Eio.Switch.run (fun sw ->
            let directory, key_file =
              Store.prepare ~sw ~root:directory ~key_file
            in
            let key = Store.key key_file in
            let encrypted =
              Eio_unix.run_in_systhread ~label:"sekrit.encrypt" (fun () ->
                  Sekrit.Envelope.seal ~key ~context plaintext)
            in
            Eioext.Path.atomic_write Eio.Path.(directory / name) encrypted))
end
