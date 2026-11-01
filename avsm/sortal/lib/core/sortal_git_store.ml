(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Contact = Sortal_schema.Contact

type t = { store : Sortal_store.t; env : Eio_unix.Stdenv.base }

let create store env = { store; env }
let store t = t.store
let data_dir_path t = Eio.Path.native_exn (Sortal_store.data_dir t.store)

let run_git t args =
  try
    let result =
      Eioext.Process.capture ~process_mgr:t.env#process_mgr ~max_stdout:(1024 * 1024)
        ([ "git"; "-C"; data_dir_path t ] @ args)
    in
    match result.status with
    | `Exited 0 -> Ok ()
    | `Exited code ->
        Error (Printf.sprintf "git %s exited with code %d" (List.hd args) code)
    | `Signaled signal ->
        Error (Printf.sprintf "git killed by signal %d" signal)
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error "git executable not found"
  | Eioext.Process.Output_limit _ -> Error "git command output exceeded its limit"

let is_initialized t =
  let data_dir = data_dir_path t in
  let git_dir = Filename.concat data_dir ".git" in
  Sys.file_exists git_dir && Sys.is_directory git_dir

let init_unlocked t =
  if is_initialized t then Ok ()
  else (
    Sortal_carddav.Common.mkdir (data_dir_path t);
    match run_git t [ "init" ] with
    | Error _ as e -> e
    | Ok () -> (
        (* Create initial commit *)
        match run_git t [ "add"; "--"; "."; ":(exclude).sortal.lock" ] with
        | Error _ as e -> e
        | Ok () ->
            let msg = "Initialize sortal contact database" in
            run_git t [ "commit"; "--allow-empty"; "-m"; msg ]))

(* Auto-initialize git repo if not already initialized *)
let init t = Sortal_store.with_transaction t.store (fun _ -> init_unlocked t)
let ensure_initialized t = if is_initialized t then Ok () else init_unlocked t
let ( let* ) = Result.bind

let commit_file t filename msg =
  let* () = run_git t [ "add"; "--"; "store.json"; filename ] in
  run_git t [ "commit"; "--only"; "-m"; msg; "--"; "store.json"; filename ]

let changed t filename before msg =
  let path = Filename.concat (data_dir_path t) filename in
  if Some (Sortal_carddav.Common.read path) = before then Ok ()
  else commit_file t filename msg

let save t contact =
  Sortal_store.with_transaction t.store (fun transaction ->
      let* () = ensure_initialized t in
      let handle = Contact.handle contact in
      let before =
        Option.bind (Sortal_store.lookup t.store handle) Contact.source
      in
      Sortal_store.Transaction.save transaction contact;
      let filename = Sortal_store.filename t.store handle in
      let msg =
        Printf.sprintf "%s contact @%s (%s)"
          (if before = None then "Add" else "Update")
          handle (Contact.name contact)
      in
      changed t filename before msg)

let delete t handle =
  Sortal_store.with_transaction t.store (fun transaction ->
      match Sortal_store.lookup t.store handle with
      | None -> Error (Printf.sprintf "Contact not found: %s" handle)
      | Some contact ->
          let* () = ensure_initialized t in
          let filename = Sortal_store.filename t.store handle in
          Sortal_store.Transaction.delete transaction handle;
          let* () = run_git t [ "add"; "-u"; "--"; filename ] in
          let msg =
            Printf.sprintf "Delete contact @%s (%s)" handle
              (Contact.name contact)
          in
          run_git t [ "commit"; "--only"; "-m"; msg; "--"; filename ])

let modify t handle f msg =
  Sortal_store.with_transaction t.store (fun transaction ->
      match Sortal_store.lookup t.store handle with
      | None -> Error (Printf.sprintf "Contact not found: %s" handle)
      | Some contact ->
          let* () = ensure_initialized t in
          let filename = Sortal_store.filename t.store handle in
          let* () = f transaction in
          changed t filename (Contact.source contact) msg)

let update_contact t handle f ~msg =
  modify t handle
    (fun transaction ->
      Sortal_store.Transaction.update_contact transaction handle f)
    msg

let set_account t handle account =
  let msg =
    Printf.sprintf "Update @%s: set %s account %s" handle
      (Contact.Platform.key (Contact.Account.platform account))
      (Contact.Account.handle account)
  in
  modify t handle
    (fun transaction ->
      Sortal_store.Transaction.set_account transaction handle account)
    msg

let unset_account t handle platform =
  let msg =
    Printf.sprintf "Update @%s: unset %s account" handle
      (Contact.Platform.key platform)
  in
  modify t handle
    (fun transaction ->
      Sortal_store.Transaction.unset_account transaction handle platform)
    msg
