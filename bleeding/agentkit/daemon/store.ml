(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory

type dir = Eio.Fs.dir_ty Eio.Path.t

let default_root xdg = Xdge.state_dir xdg
let schedule_file xdg = Eio.Path.(Xdge.config_dir xdg / "schedule.json")
let under root name = Eio.Path.((root :> dir) / name)
let journal_dir root = under root "journal"
let memory_dir root = under root "memory"
let workspace_dir root = under root "workspace"
let control_path root = under root "control"
let lock_path root = under root "lock"

(* Both kinds name a version, and a handover names one no [memory_write] can
   have missed, so the two together are what [Memory.recover] is settled
   against. *)
let journalled_version root =
  let highest = ref 0 in
  let keep v = if v > !highest then highest := v in
  let dir = journal_dir root in
  if Eio.Path.is_directory dir then
    Journal.iter ~kinds:[ "memory_write"; "handover" ] dir (fun r ->
        match r.Journal.kind with
        | Journal.Memory_write mw -> keep mw.Journal.to_
        | Journal.Handover v -> keep v
        | _ -> ());
  !highest

(* The lock. *)

exception Locked of { path : string; pid : int }

let () =
  Printexc.register_printer (function
    | Locked { path; pid } ->
        Some
          (Printf.sprintf
             "another numpty holds %s, as process %d. One run owns a store, \
              since two would interleave journal lines and race the version \
              counter."
             path pid)
    | _ -> None)

(* A record lock says whether the run that took it is still alive, which is
   what a crashed run's leavings must not claim. It is per process rather than
   per file descriptor, though, so a second [open_] in one process would take a
   lock it already holds. The roots this process has open are therefore kept
   here as well, and asked first. *)
let held : (string, int) Hashtbl.t = Hashtbl.create 4

let read_pid fd =
  let buf = Bytes.create 32 in
  let n = try Unix.read fd buf 0 32 with Unix.Unix_error _ -> 0 in
  match int_of_string_opt (String.trim (Bytes.sub_string buf 0 n)) with
  | Some pid -> pid
  | None -> 0

let write_pid fd =
  Unix.ftruncate fd 0;
  ignore (Unix.lseek fd 0 Unix.SEEK_SET);
  let text = string_of_int (Unix.getpid ()) ^ "\n" in
  ignore (Unix.write_substring fd text 0 (String.length text))

let take_lock path =
  (match Hashtbl.find_opt held path with
  | Some pid -> raise (Locked { path; pid })
  | None -> ());
  let fd =
    Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0o600
  in
  match Unix.lockf fd Unix.F_TLOCK 0 with
  | () ->
      write_pid fd;
      Hashtbl.replace held path (Unix.getpid ());
      fd
  | exception e -> (
      let pid = read_pid fd in
      Unix.close fd;
      match e with
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES | Unix.EDEADLK), _, _) ->
          raise (Locked { path; pid })
      | e -> raise e)

(* The store. *)

type t = {
  root : dir;
  journal : Journal.t;
  memory : Memory.t;
  workspace : dir;
  lock : Unix.file_descr;
  lock_name : string;
  mutable open_ : bool;
}

let root t = t.root
let journal t = t.journal
let memory t = t.memory
let workspace t = t.workspace

let close t =
  if t.open_ then begin
    t.open_ <- false;
    Journal.close t.journal;
    Hashtbl.remove held t.lock_name;
    try Unix.close t.lock with Unix.Unix_error _ -> ()
  end

let open_ ~sw ~clock root =
  let root = (root :> dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (workspace_dir root);
  let lock_name = Eio.Path.native_exn (lock_path root) in
  let lock = take_lock lock_name in
  let recovered = Journal.recover (journal_dir root) in
  let journal =
    Journal.create ~sw ~clock ~run:recovered.Journal.next_run
      ~seq:recovered.Journal.next_seq (journal_dir root)
  in
  let memory = Memory.create ~clock (memory_dir root) in
  let settled = Memory.recover memory ~journalled:(journalled_version root) in
  List.iter
    (fun v ->
      ignore
        (Journal.append journal
           (Journal.Error
              {
                Journal.where = "memory recovery";
                what =
                  Printf.sprintf
                    "removed snapshot %06d, which no journal record names, so \
                     it was never in force"
                    v;
              })))
    settled.Memory.removed;
  (match settled.Memory.adopted with
  | None -> ()
  | Some v ->
      ignore
        (Journal.append journal
           (Journal.Error
              {
                Journal.where = "memory recovery";
                what =
                  Printf.sprintf
                    "moved current forward to version %06d, which the journal \
                     names and a crash left unadopted"
                    v;
              })));
  (* The lock is taken above, so a socket file here belongs to no live run. *)
  (try Eio.Path.unlink (control_path root) with Eio.Exn.Io _ -> ());
  let t =
    {
      root;
      journal;
      memory;
      workspace = workspace_dir root;
      lock;
      lock_name;
      open_ = true;
    }
  in
  Eio.Switch.on_release sw (fun () -> close t);
  t
