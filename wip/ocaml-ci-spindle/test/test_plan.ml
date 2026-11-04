(* SPDX-License-Identifier: ISC *)
(* The [ocaml-ci] workflow against a local opam repository: what it spawns,
   and the later platforms it hands to the newest compiler's build. Nothing
   is submitted, so the capability file is never dialled. *)
open Ocaml_ci_spindle

let write root path text =
  let path = Filename.concat root path in
  let rec mkdirs d =
    if not (Sys.file_exists d) then (
      mkdirs (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  mkdirs (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let opam ?(extra = "") deps =
  "opam-version: \"2.0\"\ndepends: [" ^ deps ^ "]\n" ^ extra

let repository () =
  let root = Filename.temp_file "ocaml-ci-plan-" "" in
  Sys.remove root;
  List.iter
    (fun v ->
      write root
        (Printf.sprintf
           "packages/ocaml-base-compiler/ocaml-base-compiler.%s/opam" v)
        (opam "");
      write root
        (Printf.sprintf "packages/ocaml/ocaml.%s/opam" v)
        (opam (Printf.sprintf "\"ocaml-base-compiler\" {= \"%s\"}" v)))
    [ "4.14.2"; "5.4.1" ];
  write root "packages/host-system-mingw/host-system-mingw.1/opam"
    (opam ~extra:"available: os = \"win32\"\n" "");
  let git args =
    let cmd =
      Filename.quote_command "git"
        ([ "-C"; root; "-c"; "user.name=t"; "-c"; "user.email=t@t" ] @ args)
    in
    if Sys.command (cmd ^ " >/dev/null") <> 0 then failwith cmd
  in
  git [ "init"; "-q" ];
  git [ "add"; "." ];
  git [ "commit"; "-qm"; "packages" ];
  root

(* A capability URI in the right form, for a scheduler nobody contacts. *)
let cap_file () =
  let path = Filename.temp_file "ocaml-ci-plan-" ".cap" in
  Out_channel.with_open_bin path (fun oc ->
      output_string oc
        ("capnp://sha-256:" ^ String.make 43 'A' ^ "@127.0.0.1:9/"
       ^ String.make 43 'B' ^ "\n"));
  path

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = repository () in
  let cache = Filename.temp_file "ocaml-ci-cache-" "" in
  Sys.remove cache;
  let config =
    {
      Ci.scheduler = Ocluster_client.scheduler ~sw env#net (cap_file ());
      clock = (env#mono_clock :> Eio.Time.Mono.ty Eio.Resource.t);
      opam_repository =
        Opam_repo.v env#process_mgr env#mono_clock ~dir:cache
          ~name:"opam-repository" ("file://" ^ root);
      oxcaml_repository = None;
      compilers = [ "auto" ];
      platforms = [ Debian Arm64; Debian X86_64; Windows ];
    }
  in
  let spawned = ref [] and log = Buffer.create 256 in
  let ctx : Spindle.Executor.context =
    {
      step = (fun ?kind:_ ?command:_ _ f -> f ());
      log = (fun _ s -> Buffer.add_string log s);
      spawn =
        (fun ?(env = []) name steps ->
          spawned := (name, env, steps) :: !spawned);
    }
  in
  let files : Spindle.Executor.files =
    {
      list = (fun _ -> [ "demo.opam"; "README.md" ]);
      read = (function "demo.opam" -> Some (opam "\"ocaml\"") | _ -> None);
    }
  in
  let request : Spindle.Executor.request =
    {
      pipeline = "p";
      workflow = "ocaml-ci";
      repo = "did:plc:repo";
      source = "https://example.org/repo";
      commit = "c0ffee";
      clone = false;
      metadata = Jsont.Json.object' [];
      definition = None;
      yaml = None;
      env = [];
      variables = [];
      secrets = [];
      steps = [];
      files;
    }
  in
  Ci.executor config ctx request;
  let spawned = List.rev !spawned in
  let x86 = "OCaml 5.4.1 on Debian 13 (x86_64)"
  and arm = "OCaml 5.4.1 on Debian 13 (arm64)"
  and win = "OCaml 5.4.1 on Windows, mingw-w64 (x86_64)" in
  (* One build per compiler, the newest first. *)
  assert (
    List.map (fun (n, _, _) -> n) spawned
    = [ x86; "OCaml 4.14.2 on Debian 13 (x86_64)" ]);
  let _, env, steps = List.hd spawned in
  assert (List.assoc "OCAMLCI_POOL" env = "linux-x86_64");
  (* The newest build carries the other platforms, without x86_64 again, each
     with its own pool and solution. *)
  let names = List.map (fun (s : Spindle.Job.step) -> s.name) steps in
  assert (names = [ "obuilder"; arm; win ]);
  let step name =
    List.find (fun (s : Spindle.Job.step) -> s.name = name) steps
  in
  assert (List.assoc "OCAMLCI_POOL" (step arm).env = "linux-arm64");
  assert (List.assoc "OCAMLCI_VARIANT" (step win).env = win);
  let has s sub =
    let n = String.length sub in
    let rec at i =
      i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
    in
    at 0
  in
  assert (has (step win).command "host-system-mingw.1");
  assert (not (has (step arm).command "host-system-mingw"));
  let _, _, older = List.nth spawned 1 in
  assert (List.length older = 1);
  assert (has (Buffer.contents log) ("Once " ^ x86 ^ " passes:"));
  (* A repository's own workflow never reaches OCluster, whatever its
     environment says. *)
  (match
     Ci.executor config ctx
       {
         request with
         workflow = "evil.yml";
         definition = Some (Jsont.Json.object' []);
         env = [ ("OCAMLCI_VARIANT", "x"); ("OCAMLCI_POOL", "linux-x86_64") ];
         steps = [ Spindle.Job.step "s" "((from busybox))" ];
       }
   with
  | () -> failwith "ran a repository workflow"
  | exception Failure _ -> ());
  print_endline
    "ocaml-ci plan: builds, later platforms, their pools and refusing \
     repository workflows passed"
