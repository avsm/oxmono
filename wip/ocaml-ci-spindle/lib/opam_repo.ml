(* SPDX-License-Identifier: ISC *)

type t = {
  process : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  name : string;
  url : string;
  dir : string;
  lock : Eio.Mutex.t;
  mutable commit : string option;
  mutable fetched : int64;
}

let v process clock ~dir ~name url =
  {
    process :> Eio_unix.Process.mgr_ty Eio.Resource.t;
    clock :> Eio.Time.Mono.ty Eio.Resource.t;
    name;
    url;
    dir = Filename.concat dir name;
    lock = Eio.Mutex.create ();
    commit = None;
    fetched = 0L;
  }

let name t = t.name
let url t = t.url
let git t args = Eio.Process.run t.process ("git" :: args)

let git_out t args =
  String.trim
    (Eio.Process.parse_out t.process Eio.Buf_read.take_all ("git" :: args))

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
    git t [ "clone"; "-q"; "--depth=1"; "--single-branch"; t.url; partial ];
    Eio_unix.run_in_systhread (fun () -> Sys.rename partial t.dir))
  else (
    git t [ "-C"; t.dir; "fetch"; "-q"; "--depth=1"; "origin"; "HEAD" ];
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
    | None -> failwith ("opam repository " ^ t.name ^ " was never fetched")
  in
  f ~commit ~packages:(Filename.concat t.dir "packages")
