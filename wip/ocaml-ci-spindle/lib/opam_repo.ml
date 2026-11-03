(* SPDX-License-Identifier: ISC *)

type t = {
  process : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  name : string;
  url : string;
  dir : string;
  lock : Eio.Mutex.t;
  mutable commit : string option;
  mutable fetched : float;
}

let v process clock ~dir ~name url =
  {
    process :> Eio_unix.Process.mgr_ty Eio.Resource.t;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    name;
    url;
    dir = Filename.concat dir name;
    lock = Eio.Mutex.create ();
    commit = None;
    fetched = neg_infinity;
  }

let name t = t.name
let url t = t.url
let git t args = Eio.Process.run t.process ("git" :: args)

let git_out t args =
  String.trim
    (Eio.Process.parse_out t.process Eio.Buf_read.take_all ("git" :: args))

(* Only the newest commit is needed to solve, so the clone is shallow. *)
let refresh ?(max_age = 600.) t =
  Eio.Mutex.use_rw ~protect:true t.lock @@ fun () ->
  let now = Eio.Time.now t.clock in
  if t.commit = None || now -. t.fetched > max_age then (
    if not (Sys.file_exists (Filename.concat t.dir ".git")) then (
      Eio_unix.run_in_systhread (fun () ->
          if not (Sys.file_exists (Filename.dirname t.dir)) then
            Sys.mkdir (Filename.dirname t.dir) 0o755);
      git t [ "clone"; "-q"; "--depth=1"; "--single-branch"; t.url; t.dir ])
    else (
      git t [ "-C"; t.dir; "fetch"; "-q"; "--depth=1"; "origin"; "HEAD" ];
      git t [ "-C"; t.dir; "reset"; "-q"; "--hard"; "FETCH_HEAD" ]);
    t.commit <- Some (git_out t [ "-C"; t.dir; "rev-parse"; "HEAD" ]);
    t.fetched <- now)

let commit t =
  match t.commit with
  | Some c -> c
  | None -> failwith ("opam repository " ^ t.name ^ " was never fetched")

let packages t = Filename.concat t.dir "packages"
