(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Contact = Sortal_schema.Contact

type t = { store : Sortal_store.t; env : Eio_unix.Stdenv.base }

let create store env = { store; env }
let store t = t.store

(* Helper to check if a string contains a substring *)
let contains_substring ~needle haystack =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(* Helper to get the data directory path as a native string *)
let data_dir_path t = Eio.Path.native_exn (Sortal_store.data_dir t.store)

(* Execute a git command in the data directory *)
let run_git t args =
  let data_dir = data_dir_path t in
  Eio.Switch.run @@ fun sw ->
  try
    let mgr = t.env#process_mgr in
    let cmd = [ "git"; "-C"; data_dir ] @ args in
    let proc = Eio.Process.spawn ~sw mgr cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok ()
    | `Exited n ->
        Error
          (Printf.sprintf "git %s exited with code %d" (String.concat " " args)
             n)
    | `Signaled n -> Error (Printf.sprintf "git killed by signal %d" n)
  with exn ->
    let msg = Printexc.to_string exn in
    if
      contains_substring ~needle:"not found" msg
      || contains_substring ~needle:"No such file" msg
    then Error "git executable not found - please install git"
    else Error (Printf.sprintf "git command failed: %s" msg)

let is_initialized t =
  let data_dir = data_dir_path t in
  let git_dir = Filename.concat data_dir ".git" in
  Sys.file_exists git_dir && Sys.is_directory git_dir

let init t =
  if is_initialized t then Ok ()
  else begin
    Sortal_carddav.Common.mkdir (data_dir_path t);
    match run_git t [ "init" ] with
    | Error _ as e -> e
    | Ok () -> (
        (* Create initial commit *)
        match run_git t [ "add"; "--"; "."; ":(exclude).sortal.lock" ] with
        | Error _ as e -> e
        | Ok () ->
            let msg = "Initialize sortal contact database" in
            run_git t [ "commit"; "--allow-empty"; "-m"; msg ])
  end

(* Auto-initialize git repo if not already initialized *)
let ensure_initialized t = if is_initialized t then Ok () else init t
let ( let* ) = Result.bind

let commit_file t filename msg =
  let* () = run_git t [ "add"; "--"; "store.json"; filename ] in
  run_git t [ "commit"; "--only"; "-m"; msg; "--"; "store.json"; filename ]

let changed t filename before msg =
  let path = Filename.concat (data_dir_path t) filename in
  if Some (Sortal_carddav.Common.read path) = before then Ok ()
  else commit_file t filename msg

let save t contact =
  let* () = ensure_initialized t in
  let handle = Contact.handle contact in
  let before =
    Option.bind (Sortal_store.lookup t.store handle) Contact.source
  in
  Sortal_store.save t.store contact;
  let filename = Sortal_store.filename t.store handle in
  let msg =
    Printf.sprintf "%s contact @%s (%s)"
      (if before = None then "Add" else "Update")
      handle (Contact.name contact)
  in
  changed t filename before msg

let delete t handle =
  match Sortal_store.lookup t.store handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let* () = ensure_initialized t in
      let filename = Sortal_store.filename t.store handle in
      Sortal_store.delete t.store handle;
      let* () = run_git t [ "add"; "-u"; "--"; filename ] in
      let msg =
        Printf.sprintf "Delete contact @%s (%s)" handle (Contact.name contact)
      in
      run_git t [ "commit"; "--only"; "-m"; msg; "--"; filename ]

let modify t handle f msg =
  match Sortal_store.lookup t.store handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let* () = ensure_initialized t in
      let filename = Sortal_store.filename t.store handle in
      let* () = f () in
      changed t filename (Contact.source contact) msg

let update_contact t handle f ~msg =
  modify t handle (fun () -> Sortal_store.update_contact t.store handle f) msg

let set_account t handle account =
  let msg =
    Printf.sprintf "Update @%s: set %s account %s" handle
      (Contact.Platform.key (Contact.Account.platform account))
      (Contact.Account.handle account)
  in
  modify t handle
    (fun () -> Sortal_store.set_account t.store handle account)
    msg

let unset_account t handle platform =
  let msg =
    Printf.sprintf "Update @%s: unset %s account" handle
      (Contact.Platform.key platform)
  in
  modify t handle
    (fun () -> Sortal_store.unset_account t.store handle platform)
    msg
