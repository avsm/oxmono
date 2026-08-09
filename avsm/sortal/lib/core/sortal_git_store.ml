(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Contact = Sortal_schema.Contact

type t = {
  store : Sortal_store.t;
  env : Eio_unix.Stdenv.base;
}

let create store env = { store; env }

let store t = t.store

(* Helper to check if a string contains a substring *)
let contains_substring ~needle haystack =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(* Helper to get the data directory path as a native string *)
let data_dir_path t =
  (* We need to extract the data directory from the store somehow.
     For now, we'll use the XDG environment to locate it. *)
  let xdg = Xdge.create t.env#fs "sortal" in
  let data_path = Xdge.data_dir xdg in
  Eio.Path.native_exn data_path

(* Execute a git command in the data directory *)
let run_git t args =
  let data_dir = data_dir_path t in
  Eio.Switch.run @@ fun sw ->
  try
    let mgr = t.env#process_mgr in
    let cmd = ["git"; "-C"; data_dir] @ args in
    let proc = Eio.Process.spawn ~sw mgr cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok ()
    | `Exited n -> Error (Printf.sprintf "git %s exited with code %d" (String.concat " " args) n)
    | `Signaled n -> Error (Printf.sprintf "git killed by signal %d" n)
  with
  | exn ->
      let msg = Printexc.to_string exn in
      if contains_substring ~needle:"not found" msg ||
         contains_substring ~needle:"No such file" msg then
        Error "git executable not found - please install git"
      else
        Error (Printf.sprintf "git command failed: %s" msg)

let is_initialized t =
  let data_dir = data_dir_path t in
  let git_dir = Filename.concat data_dir ".git" in
  Sys.file_exists git_dir && Sys.is_directory git_dir

let init t =
  if is_initialized t then
    Ok ()
  else begin
    match run_git t ["init"] with
    | Error _ as e -> e
    | Ok () ->
        (* Create initial commit *)
        match run_git t ["add"; "."] with
        | Error _ as e -> e
        | Ok () ->
            let msg = "Initialize sortal contact database" in
            run_git t ["commit"; "--allow-empty"; "-m"; msg]
  end

(* Auto-initialize git repo if not already initialized *)
let ensure_initialized t =
  if is_initialized t then Ok ()
  else init t

(* Helper to commit a file with a message *)
let commit_file t filename msg =
  match run_git t ["add"; filename] with
  | Error _ as e -> e
  | Ok () ->
      run_git t ["commit"; "-m"; msg]

(* Helper to commit a deletion *)
let commit_deletion t filename msg =
  match run_git t ["rm"; filename] with
  | Error _ as e -> e
  | Ok () ->
      run_git t ["commit"; "-m"; msg]

let save t contact =
  let handle = Contact.handle contact in
  let name = Contact.name contact in
  let filename = handle ^ ".yaml" in

  (* Check if contact already exists *)
  let is_new = match Sortal_store.lookup t.store handle with
    | None -> true
    | Some _ -> false
  in

  (* Save to store *)
  Sortal_store.save t.store contact;

  (* Commit to git (auto-init if needed) *)
  match ensure_initialized t with
  | Error _ as e -> e
  | Ok () ->
    let msg = if is_new then
      Printf.sprintf "Add contact @%s (%s)" handle name
    else
      Printf.sprintf "Update contact @%s (%s)" handle name
    in
    commit_file t filename msg

let delete t handle =
  match Sortal_store.lookup t.store handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let name = Contact.name contact in
      let filename = handle ^ ".yaml" in

      (* Delete from store *)
      Sortal_store.delete t.store handle;

      (* Commit deletion to git (auto-init if needed) *)
      match ensure_initialized t with
      | Error _ as e -> e
      | Ok () ->
        let msg = Printf.sprintf "Delete contact @%s (%s)" handle name in
        commit_deletion t filename msg

let update_contact t handle f ~msg =
  match Sortal_store.update_contact t.store handle f with
  | Error _ as e -> e
  | Ok () ->
      match ensure_initialized t with
      | Error _ as e -> e
      | Ok () ->
        let filename = handle ^ ".yaml" in
        commit_file t filename msg

let set_account t handle account =
  let msg = Printf.sprintf "Update @%s: set %s account %s" handle
    (Contact.Platform.key (Contact.Account.platform account))
    (Contact.Account.handle account) in
  match Sortal_store.set_account t.store handle account with
  | Error _ as e -> e
  | Ok () ->
      match ensure_initialized t with
      | Error _ as e -> e
      | Ok () ->
        let filename = handle ^ ".yaml" in
        commit_file t filename msg

let unset_account t handle platform =
  let msg = Printf.sprintf "Update @%s: unset %s account" handle
    (Contact.Platform.key platform) in
  match Sortal_store.unset_account t.store handle platform with
  | Error _ as e -> e
  | Ok () ->
      match ensure_initialized t with
      | Error _ as e -> e
      | Ok () ->
        let filename = handle ^ ".yaml" in
        commit_file t filename msg
