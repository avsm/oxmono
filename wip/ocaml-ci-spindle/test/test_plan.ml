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

(* A chat completions server that answers one request with [reply], and
   resolves to the request's head and body. *)
let model_server ~sw net reply =
  let sock =
    Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, p) -> p
    | _ -> assert false
  in
  let got, resolve = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let flow, _ = Eio.Net.accept ~sw sock in
      let r = Eio.Buf_read.of_flow flow ~max_size:(1 lsl 24) in
      let rec head acc =
        match Eio.Buf_read.line r with
        | "" -> List.rev acc
        | l -> head (l :: acc)
      in
      let head = head [] in
      let length =
        List.find_map
          (fun l ->
            match String.index_opt l ':' with
            | Some i
              when String.lowercase_ascii (String.sub l 0 i) = "content-length"
              ->
                int_of_string_opt
                  (String.trim (String.sub l (i + 1) (String.length l - i - 1)))
            | _ -> None)
          head
      in
      let body = Eio.Buf_read.take (Option.get length) r in
      Eio.Promise.resolve resolve (head, body);
      Eio.Flow.copy_string
        (Printf.sprintf
           "HTTP/1.1 200 OK\r\n\
            Content-Type: application/json\r\n\
            Content-Length: %d\r\n\
            Connection: close\r\n\
            \r\n\
            %s"
           (String.length reply) reply)
        flow;
      Eio.Flow.close flow);
  (port, got)

let has s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let x86 = "5.4.1:x86_64:Debian-13"
and old = "4.14.2:x86_64:Debian-13"
and arm = "5.4.1:arm64:Debian-13"
and win = "5.4.1:x86_64:Windows-mingw"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = repository () in
  let cache = Filename.temp_file "ocaml-ci-cache-" "" in
  Sys.remove cache;
  let port, asked =
    model_server ~sw env#net
      {|{"choices":[{"message":{"role":"assistant","content":"<think>hmm</think>\n- Define `x` before `y`."}}]}|}
  in
  let config =
    {
      Ci.scheduler = Ocluster_client.scheduler ~sw env#net (cap_file ());
      clock = (env#mono_clock :> Eio.Time.Mono.ty Eio.Resource.t);
      opam_repository =
        Opam_repo.v env#process_mgr env#mono_clock ~dir:cache
          ~name:"opam-repository" ("file://" ^ root);
      oxcaml_repository = None;
      compilers = [ "auto" ];
      platforms = [ Debian Arm64; Debian X86_64 ];
      full_platforms = [ Windows ];
      diagnose =
        Some
          (Diagnose.v ~key:"sekrit" ~model:"test-model" ~clock:env#mono_clock
             (Fetch_httpz.v env#net ~clock:env#mono_clock ())
             (Printf.sprintf "http://127.0.0.1:%d/v1" port));
    }
  in
  (* The builds end as these outcomes say, the newest one failing. *)
  let outcomes : Spindle.Executor.outcome list =
    [
      {
        workflow = x86;
        status = `Failed;
        error = Some "Failed: Build failed";
        log =
          "## queue\nqueued\n## opam exec -- dune build\n"
          ^ String.concat ""
              (List.init 20 (Printf.sprintf "(cd _build && ocamlopt m%d.ml)\n"))
          ^ "File \"src/a.ml\", line 1, characters 8-9:\n\
             Error: Unbound value x\n";
      };
      { workflow = old; status = `Success; error = None; log = "" };
    ]
  in
  (* Meanwhile the builds progress as these say, one change at a time. *)
  let progress status step workflow : Spindle.Executor.progress =
    { workflow; status; step; error = None }
  in
  let script =
    [
      [ progress `Pending None x86; progress `Pending None old ];
      [ progress `Running (Some "queue") x86; progress `Pending None old ];
      [
        progress `Running (Some "opam install") x86;
        progress `Running (Some "queue") old;
      ];
      [
        { (progress `Failed None x86) with error = Some "Failed: Build failed" };
        progress `Running (Some "queue") old;
      ];
      [
        { (progress `Failed None x86) with error = Some "Failed: Build failed" };
        progress `Success None old;
      ];
    ]
  in
  let remaining = ref script in
  let spawned_ ?since () =
    (match since with
    | Some since ->
        (* The caller always waits on what it last saw. *)
        assert (since = List.hd !remaining);
        remaining := List.tl !remaining
    | None -> ());
    List.hd !remaining
  in
  let spawned = ref [] and log = Buffer.create 256 and shown = ref [] in
  let ctx : Spindle.Executor.context =
    {
      step =
        (fun ?kind:_ ?command:_ name f ->
          match f () with
          | () -> shown := (name, true) :: !shown
          | exception e ->
              shown := (name, false) :: !shown;
              raise e);
      log = (fun _ s -> Buffer.add_string log s);
      spawn =
        (fun ?(env = []) name steps ->
          spawned := (name, env, steps) :: !spawned);
      await_spawned = (fun () -> outcomes);
      spawned = spawned_;
      finished = (fun () -> outcomes);
    }
  in
  let files : Spindle.Executor.files =
    {
      list = (fun _ -> [ "demo.opam"; "README.md" ]);
      read =
        (function
        | "demo.opam" -> Some (opam "\"ocaml\"")
        | "src/a.ml" -> Some "let y = x\n"
        | _ -> None);
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
      labels = (fun () -> [ Ci.full_label; Ci.diagnose_label ]);
    }
  in
  let executor = Ci.executor config in
  (* The summary fails the plan, since a build failed. *)
  (match executor ctx request with
  | () -> failwith "the summary passed with a failed build"
  | exception Failure m -> assert (m = "1 of 2 builds did not pass"));
  let order = List.rev !spawned in
  (* One build per compiler, the newest first, then the diagnosis. *)
  assert (List.map (fun (n, _, _) -> n) order = [ x86; old; "diagnose" ]);
  let _, env, steps = List.hd order in
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
  assert (has (step win).command "host-system-mingw.1");
  assert (not (has (step arm).command "host-system-mingw"));
  let _, _, older = List.nth order 1 in
  assert (List.length older = 1);
  (* Until every build ends, a step waits for the next to end, logging each
     change, and each build gets a step that passes or fails with it. *)
  assert (
    List.rev !shown
    = [
        ("analyse", true);
        ("solve", true);
        ("waiting for 2 of 4 builds", true);
        (x86 ^ " failed", false);
        ("waiting for 1 of 2 builds", true);
        (old ^ " passed", true);
        ("summary", false);
      ]);
  assert (!remaining = [ List.nth script 4 ]);
  let progress = Buffer.contents log in
  assert (has progress (x86 ^ "       queued"));
  assert (has progress (x86 ^ "       building: opam install"));
  assert (has progress (arm ^ "        planned, once " ^ x86 ^ " passes"));
  assert (has progress (win ^ "   skipped, since " ^ x86 ^ " did not pass"));
  assert (has progress "Failed: Build failed\nFAILED after ");
  (* The summary lists every build, and those never started. *)
  let summary = Buffer.contents log in
  assert (has summary ("Once " ^ x86 ^ " passes:"));
  assert (has summary ("FAILED     " ^ x86 ^ " (Failed: Build failed)"));
  assert (has summary ("passed     " ^ old));
  assert (has summary ("skipped    " ^ arm ^ " (" ^ x86 ^ " did not pass)"));
  assert (has summary "1 of 2 builds passed.");
  (* The diagnosis asks the model once, with the failed step and the file it
     names, and shows only its answer. *)
  let _, denv, _ = List.nth order 2 in
  Buffer.clear log;
  executor ctx { request with workflow = "diagnose"; env = denv };
  let head, body = Eio.Promise.await asked in
  assert (List.hd head = "POST /v1/chat/completions HTTP/1.1");
  assert (List.mem "Authorization: Bearer sekrit" head);
  assert (has body "\"model\":\"test-model\"");
  assert (has body "Unbound value x" && has body "let y = x");
  assert (has body old);
  assert (not (has body "tools"));
  assert (not (has body "## queue"));
  let answer = Buffer.contents log in
  assert (has answer "- Define `x` before `y`.");
  assert (not (has answer "hmm"));
  (* The failures are read back from the finished builds, so a pipeline
     without failed builds has nothing to diagnose. *)
  (match
     executor
       { ctx with finished = (fun () -> [ List.nth outcomes 1 ]) }
       { request with workflow = "diagnose"; env = denv }
   with
  | () -> failwith "diagnosed a pipeline without failures"
  | exception Failure _ -> ());
  (* Without a model, nothing is diagnosed. *)
  spawned := [];
  remaining := script;
  (match Ci.executor { config with diagnose = None } ctx request with
  | () -> failwith "the summary passed with a failed build"
  | exception Failure _ -> ());
  assert (not (List.exists (fun (n, _, _) -> n = "diagnose") !spawned));
  (* Without the labels, the full platforms are not built and nothing is
     diagnosed. *)
  spawned := [];
  remaining := script;
  Buffer.clear log;
  (match executor ctx { request with labels = (fun () -> []) } with
  | () -> failwith "the summary passed with a failed build"
  | exception Failure _ -> ());
  assert (not (List.exists (fun (n, _, _) -> n = "diagnose") !spawned));
  let _, _, steps = List.find (fun (n, _, _) -> n = x86) !spawned in
  assert (
    List.map (fun (s : Spindle.Job.step) -> s.name) steps = [ "obuilder"; arm ]);
  assert (
    has (Buffer.contents log)
      "Windows are built only for a pull request a maintainer labels ci:full.");
  (* A repository's own workflow never reaches OCluster, whatever its
     environment says. *)
  (match
     executor ctx
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
    "ocaml-ci plan: builds, later platforms, their pools, the summary, the \
     diagnosis and refusing repository workflows passed"
