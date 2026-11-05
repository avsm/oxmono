(* SPDX-License-Identifier: ISC *)

type t = {
  process : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  name : string;
  url : string;
  branch : string option;
  restricted : bool;
      (* A remote a project names, which git reaches only over https. *)
  dir : string;
  lock : Eio.Mutex.t;
  mutable commit : string option;
  mutable fetched : int64;
}

let make ~restricted process clock ~dir ~name ?branch url =
  {
    process :> Eio_unix.Process.mgr_ty Eio.Resource.t;
    clock :> Eio.Time.Mono.ty Eio.Resource.t;
    name;
    url;
    branch;
    restricted;
    dir = Filename.concat dir name;
    lock = Eio.Mutex.create ();
    commit = None;
    fetched = 0L;
  }

let v process clock ~dir ~name ?branch url =
  make ~restricted:false process clock ~dir ~name ?branch url

let name t = t.name
let url t = t.url
let branch t = t.branch

(* Git never waits for credentials. A remote that a project names is reached
   only over https, whatever protocol a server or submodule names. *)
let git_env =
  Array.append (Unix.environment ())
    [|
      "GIT_TERMINAL_PROMPT=0";
      "GIT_ASKPASS=/bin/false";
      "SSH_ASKPASS=/bin/false";
    |]

let git_args t args =
  ("git" :: "-c" :: "credential.helper=" :: [])
  @ (if t.restricted then
       [ "-c"; "protocol.allow=never"; "-c"; "protocol.https.allow=always" ]
     else [])
  @ args

let git t args = Eio.Process.run t.process ~env:git_env (git_args t args)

let git_out t args =
  String.trim
    (Eio.Process.parse_out t.process ~env:git_env Eio.Buf_read.take_all
       (git_args t args))

(* Only the newest commit is needed to solve, so the clone is shallow. It is
   made beside [dir] and moved into place, so an interrupted clone is never
   taken for a repository. *)
let fetch t =
  if not (Sys.file_exists (Filename.concat t.dir ".git")) then (
    let partial = t.dir ^ ".partial" in
    Eio.Process.run t.process [ "rm"; "-rf"; "--"; partial ];
    Eio_unix.run_in_systhread (fun () ->
        if not (Sys.file_exists (Filename.dirname t.dir)) then
          Sys.mkdir (Filename.dirname t.dir) 0o755);
    git t
      ([ "clone"; "-q"; "--depth=1"; "--single-branch" ]
      @ (match t.branch with Some b -> [ "--branch"; b ] | None -> [])
      @ [ "--"; t.url; partial ]);
    Eio_unix.run_in_systhread (fun () -> Sys.rename partial t.dir))
  else (
    git t
      [
        "-C"; t.dir; "fetch"; "-q"; "--depth=1"; "origin";
        Option.value ~default:"HEAD" t.branch;
      ];
    git t [ "-C"; t.dir; "reset"; "-q"; "--hard"; "FETCH_HEAD" ]);
  git_out t [ "-C"; t.dir; "rev-parse"; "HEAD" ]

(* A fetch that stalls would hold every plan behind the lock. *)
let fetch_timeout = 600.

(* The lock covers the fetch and the caller's reads, so that a refresh never
   rewrites a tree being solved against. A failed fetch keeps the previous
   snapshot, when there is one, until the next refresh. The lock
   is not poisoned by a failure. *)
let with_snapshot ?(max_age = 600.) t f =
  Eio.Mutex.use_ro t.lock @@ fun () ->
  let now = Mtime.to_uint64_ns (Eio.Time.Mono.now t.clock) in
  let age = Int64.to_float (Int64.sub now t.fetched) /. 1e9 in
  (if t.commit = None || age > max_age then
     match
       Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds t.clock fetch_timeout)
         (fun () -> fetch t)
     with
     | commit ->
         t.commit <- Some commit;
         t.fetched <- now
     | exception ((Eio.Io _ | Eio.Time.Timeout) as e) when t.commit <> None ->
         (* The next attempt waits for [max_age] too, so a dead remote does
            not slow every plan. *)
         t.fetched <- now;
         Printf.eprintf "ocaml-ci-spindle: keeping %s at %s: %s\n%!" t.name
           (Option.get t.commit) (Printexc.to_string e));
  let commit =
    match t.commit with
    | Some c -> c
    | None ->
        failwith
          (Printf.sprintf "the opam repository %s%s could not be fetched" t.url
             (match t.branch with Some b -> " at branch " ^ b | None -> ""))
  in
  f ~commit ~packages:(Filename.concat t.dir "packages")

type remotes = {
  r_process : Eio_unix.Process.mgr_ty Eio.Resource.t;
  r_clock : Eio.Time.Mono.ty Eio.Resource.t;
  r_dir : string;
  known : (string * string option, t) Hashtbl.t;
}

let remotes process clock ~dir =
  {
    r_process = (process :> Eio_unix.Process.mgr_ty Eio.Resource.t);
    r_clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t);
    r_dir = dir;
    known = Hashtbl.create 16;
  }

(* Each remote has its own clone, kept for later builds, so their number is
   bounded. *)
let max_remotes = 64

let remote r ~url ?branch () =
  match Hashtbl.find_opt r.known (url, branch) with
  | Some t -> Some t
  | None when Hashtbl.length r.known >= max_remotes -> None
  | None ->
      let key = url ^ "#" ^ Option.value ~default:"" branch in
      let name =
        "remote-" ^ String.sub (Digest.to_hex (Digest.string key)) 0 16
      in
      let t =
        make ~restricted:true r.r_process r.r_clock ~dir:r.r_dir ~name ?branch
          url
      in
      Hashtbl.replace r.known (url, branch) t;
      Some t
