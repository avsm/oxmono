(* SPDX-License-Identifier: ISC *)

type t = {
  process : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  name : string;
  url : string;
  branch : string option;
  restricted : string option;
      (* The host of a remote a project names, which git reaches only over
         https, at an address checked to be public. *)
  dir : string;
  lock : Eio.Mutex.t;
  mutable commit : string option;
  mutable fetched : int64;
  mutable failure : string option;
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
    failure = None;
  }

let v process clock ~dir ~name ?branch url =
  make ~restricted:None process clock ~dir ~name ?branch url

let name t = t.name
let url t = t.url
let branch t = t.branch

(* Git never waits for credentials. *)
let quiet =
  [| "GIT_TERMINAL_PROMPT=0"; "GIT_ASKPASS=/bin/false"; "SSH_ASKPASS=/bin/false" |]

(* A remote that a project names is fetched with none of the host's git
   configuration or LFS filters, which could send credentials or fetch
   elsewhere, and as little else of the host's environment as git needs. *)
let isolated =
  Array.append quiet
    [|
      "PATH=" ^ Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH");
      "HOME=/nonexistent";
      "GIT_CONFIG_GLOBAL=/dev/null";
      "GIT_CONFIG_NOSYSTEM=1";
      "GIT_LFS_SKIP_SMUDGE=1";
    |]

let git_env t =
  match t.restricted with
  | None -> Array.append (Unix.environment ()) quiet
  | Some _ -> isolated

(* The first public address of [host], as [curl --resolve] writes it, or
   [None] when any address it resolves to is not public, since a server may
   name several. *)
let public_address host =
  match
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host "443" [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ])
  with
  | [] -> None
  | addrs ->
      let ips =
        List.filter_map
          (fun (a : Unix.addr_info) ->
            match a.ai_addr with
            | Unix.ADDR_INET (ip, _) -> Some ip
            | Unix.ADDR_UNIX _ -> None)
          addrs
      in
      if
        ips = []
        || not
             (List.for_all
                (fun ip ->
                  Xrpc_auth_web.Public_net.is_public (Eio_unix.Net.Ipaddr.of_unix ip))
                ips)
      then None
      else
        let ip = List.hd ips in
        let s = Unix.string_of_inet_addr ip in
        Some (if Unix.is_inet6_addr ip then "[" ^ s ^ "]" else s)

(* A project's remote is reached only over https, at the public address just
   checked, with no redirects, and its checkout makes no symbolic links, so
   that it can neither reach a private address nor point the solver at the
   host's files. *)
let git_args t ~pin args =
  ("git" :: "-c" :: "credential.helper=" :: [])
  @ (match t.restricted with
    | None -> []
    | Some host ->
        [
          "-c"; "protocol.allow=never";
          "-c"; "protocol.https.allow=always";
          "-c"; "http.followRedirects=false";
          "-c"; "core.symlinks=false";
          "-c"; "core.hooksPath=/dev/null";
        ]
        @ match pin with
          | Some ip -> [ "-c"; Printf.sprintf "http.curloptResolve=%s:443:%s" host ip ]
          | None -> [])
  @ args

(* Git runs in a process group of its own, killed whole when the call is
   cancelled or times out, so that the helpers it starts, such as
   git-remote-https, do not outlive it. Its output is the result when [out]
   is set, and otherwise goes to the spindle's standard error. *)
let run_git ?pin ?(out = false) t args =
  let argv = git_args t ~pin args in
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let stdout = if out then Eio_unix.Resource.fd w else Eio_unix.Fd.stderr in
  let child =
    Eio_unix.Process.spawn_unix ~sw t.process ~pgid:0 ~env:(git_env t)
      ~fds:
        [
          (0, Eio_unix.Fd.stdin, `Blocking);
          (1, stdout, `Blocking);
          (2, Eio_unix.Fd.stderr, `Blocking);
        ]
      argv
  in
  Eio.Flow.close w;
  match
    let output =
      if out then Eio.Buf_read.(parse_exn take_all) r ~max_size:(1 lsl 20)
      else ""
    in
    (output, Eio.Process.await child)
  with
  | output, `Exited 0 -> output
  | _, status ->
      raise
        (Eio.Exn.add_context
           (Eio.Process.err (Child_error status))
           "running command: %s" (String.concat " " argv))
  | exception e ->
      (try Unix.kill (-Eio.Process.pid child) Sys.sigkill
       with Unix.Unix_error _ -> ());
      raise e

let git ?pin t args = ignore (run_git ?pin t args : string)
let git_out t args = String.trim (run_git ~out:true t args)

(* Only the newest commit is needed to solve, so the clone is shallow. It is
   made beside [dir] and moved into place, so an interrupted clone is never
   taken for a repository. *)
let fetch t =
  let pin =
    match t.restricted with
    | None -> None
    | Some host -> (
        match public_address host with
        | Some ip -> Some ip
        | None -> failwith (host ^ " does not resolve only to public addresses"))
  in
  let git = git ?pin in
  (* A project's remote is cloned for its packages alone. *)
  let sparse =
    match t.restricted with
    | None -> []
    | Some _ -> [ "--filter=blob:none"; "--sparse" ]
  in
  if not (Sys.file_exists (Filename.concat t.dir ".git")) then (
    let partial = t.dir ^ ".partial" in
    Eio.Process.run t.process [ "rm"; "-rf"; "--"; partial ];
    Eio_unix.run_in_systhread (fun () ->
        if not (Sys.file_exists (Filename.dirname t.dir)) then
          Sys.mkdir (Filename.dirname t.dir) 0o755);
    git t
      ([ "clone"; "-q"; "--depth=1"; "--single-branch" ]
      @ sparse
      @ (match t.branch with Some b -> [ "--branch"; b ] | None -> [])
      @ [ "--"; t.url; partial ]);
    if sparse <> [] then
      git t [ "-C"; partial; "sparse-checkout"; "set"; "--"; "packages" ];
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
  (if (t.commit = None && (t.fetched = 0L || age > max_age)) || age > max_age
   then
     match
       Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds t.clock fetch_timeout)
         (fun () -> fetch t)
     with
     | commit ->
         t.commit <- Some commit;
         t.fetched <- now
     | exception ((Eio.Io _ | Eio.Time.Timeout | Failure _) as e)
       when t.commit <> None ->
         (* The next attempt waits for [max_age] too, so a dead remote does
            not slow every plan. *)
         t.fetched <- now;
         Printf.eprintf "ocaml-ci-spindle: keeping %s at %s: %s\n%!" t.name
           (Option.get t.commit) (Printexc.to_string e)
     | exception ((Eio.Io _ | Eio.Time.Timeout | Failure _) as e) ->
         (* A repository never fetched is not tried again for [max_age]
            either. *)
         t.fetched <- now;
         t.failure <-
           Some
             (match e with
             | Failure m -> m
             | Eio.Time.Timeout -> "the fetch timed out"
             | e -> Printexc.to_string e));
  let commit =
    match t.commit with
    | Some c -> c
    | None ->
        failwith
          (Printf.sprintf "the opam repository %s%s could not be fetched%s"
             t.url
             (match t.branch with Some b -> " at branch " ^ b | None -> "")
             (match t.failure with Some m -> ": " ^ m | None -> ""))
  in
  f ~commit ~packages:(Filename.concat t.dir "packages")

type remotes = {
  r_process : Eio_unix.Process.mgr_ty Eio.Resource.t;
  r_clock : Eio.Time.Mono.ty Eio.Resource.t;
  r_dir : string;
  hosts : string list;
  known : (string * string option, t) Hashtbl.t;
}

let default_hosts =
  [ "github.com"; "gitlab.com"; "codeberg.org"; "bitbucket.org"; "git.sr.ht"; "tangled.org" ]

let remotes ?(hosts = default_hosts) process clock ~dir =
  {
    r_process = (process :> Eio_unix.Process.mgr_ty Eio.Resource.t);
    r_clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t);
    r_dir = dir;
    hosts = List.map String.lowercase_ascii hosts;
    known = Hashtbl.create 16;
  }

let hosts r = r.hosts
let allowed r host = List.mem (String.lowercase_ascii host) r.hosts

(* Each remote has its own clone, kept for later builds, so their number is
   bounded. *)
let max_remotes = 64

let forget r t =
  if t.commit = None then Hashtbl.remove r.known (t.url, t.branch)

let remote r ~url ~host ?branch () =
  match Hashtbl.find_opt r.known (url, branch) with
  | Some t -> Some t
  | None when Hashtbl.length r.known >= max_remotes -> None
  | None ->
      let key = url ^ "#" ^ Option.value ~default:"" branch in
      let name =
        "remote-" ^ String.sub (Digest.to_hex (Digest.string key)) 0 16
      in
      let t =
        make ~restricted:(Some host) r.r_process r.r_clock ~dir:r.r_dir ~name
          ?branch url
      in
      Hashtbl.replace r.known (url, branch) t;
      Some t
