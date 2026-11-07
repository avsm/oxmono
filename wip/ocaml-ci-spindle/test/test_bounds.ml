(* SPDX-License-Identifier: ISC *)
open Ocaml_ci_spindle

let temp_dir () =
  let d = Filename.temp_file "ocis" "" in
  Sys.remove d;
  Sys.mkdir d 0o755;
  d

let write path s = Out_channel.with_open_bin path (fun oc -> output_string oc s)

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false

(* A git that starts a helper, as git-remote-https is, which records its
   process ID and outlives git unless the whole group is killed. *)
let git_group env =
  let dir = temp_dir () in
  let bin = Filename.concat dir "bin" in
  Sys.mkdir bin 0o755;
  let pidfile = Filename.concat dir "helper.pid" in
  write (Filename.concat bin "git")
    (Printf.sprintf "#!/bin/sh\nsleep 300 &\necho $! > %s\nwait\n"
       (Filename.quote pidfile));
  Unix.chmod (Filename.concat bin "git") 0o755;
  let path = Sys.getenv "PATH" in
  Testenv.with_bindings [ ("PATH", Some (bin ^ ":" ^ path)) ] (fun () ->
      let repo =
        Opam_repo.v env#process_mgr env#mono_clock ~dir ~name:"r"
          "https://example.invalid/r"
      in
      match
        Eio.Time.with_timeout env#clock 2. (fun () ->
            Ok
              (Opam_repo.with_snapshot repo (fun ~commit:_ ~packages:_ -> ())))
      with
      | Error `Timeout -> ()
      | Ok () -> assert false);
  let read f = In_channel.with_open_bin f In_channel.input_all in
  let pid = int_of_string (String.trim (read pidfile)) in
  (* The kill is asynchronous, so the helper is given a moment to go. *)
  let rec gone n =
    (not (alive pid))
    || (n > 0 && (Unix.sleepf 0.05; gone (n - 1)))
  in
  (* A zombie still answers [kill 0] until reaped by init, so check its
     state too. *)
  let zombie () =
    match read (Printf.sprintf "/proc/%d/stat" pid) with
    | s -> (
        match String.index_opt s ')' with
        | Some i -> String.length s > i + 2 && s.[i + 2] = 'Z'
        | None -> false)
    | exception Sys_error _ -> true
  in
  assert (gone 40 || zombie ())

let hosts env =
  let r = Opam_repo.remotes env#process_mgr env#mono_clock ~dir:(temp_dir ()) in
  assert (Opam_repo.allowed r "github.com");
  assert (Opam_repo.allowed r "GitHub.com");
  assert (not (Opam_repo.allowed r "evil.github.com"));
  assert (not (Opam_repo.allowed r "example.org"));
  let r =
    Opam_repo.remotes ~hosts:[ "Example.org" ] env#process_mgr env#mono_clock
      ~dir:(temp_dir ())
  in
  assert (Opam_repo.allowed r "example.org");
  assert (not (Opam_repo.allowed r "github.com"))

let platform : Solve.platform =
  {
    os = "linux";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "13";
    arch = "x86_64";
    opam_version = "2.3.0";
  }

(* An opam file over the bound fails the solve rather than being read. *)
let big_opam () =
  let packages = temp_dir () in
  let dir = Filename.concat packages "ocaml" in
  Sys.mkdir dir 0o755;
  Sys.mkdir (Filename.concat dir "ocaml.5.4.0") 0o755;
  write
    (Filename.concat dir "ocaml.5.4.0/opam")
    ("opam-version: \"2.0\"\n# " ^ String.make (300 * 1024) 'x' ^ "\n");
  match
    Solve.solve
      {
        platform;
        repositories = [ packages ];
        pins = [ ("demo", "opam-version: \"2.0\"\ndepends: [\"ocaml\"]\n") ];
        compiler = ("ocaml", "5.4.0");
        requires = [];
      }
  with
  | Error m ->
      let sub = "256 KiB" in
      let n = String.length sub in
      let rec at i =
        i + n <= String.length m && (String.sub m i n = sub || at (i + 1))
      in
      if not (at 0) then failwith m
  | Ok _ -> assert false

let diagnose env =
  let fetch = Fetch_httpz.v env#net ~clock:env#mono_clock () in
  let v ?key url =
    match Diagnose.v ?key ~clock:env#mono_clock fetch url with
    | _ -> true
    | exception Invalid_argument _ -> false
  in
  assert (v "https://openrouter.ai/api/v1");
  assert (v ~key:"k" "https://openrouter.ai/api/v1");
  assert (v "http://sequoia:8000/v1");
  assert (not (v ~key:"k" "http://sequoia:8000/v1"));
  assert (v ~key:"k" "http://127.0.0.1:8000/v1");
  assert (v ~key:"k" "http://[::1]:8000/v1");
  assert (not (v "ftp://example.org/"))

let () =
  Eio_main.run @@ fun env ->
  git_group env;
  hosts env;
  big_opam ();
  diagnose env;
  print_endline
    "ocaml-ci bounds: git process group, remote hosts, solve and diagnose URL \
     passed"
