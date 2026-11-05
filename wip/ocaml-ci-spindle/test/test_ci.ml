(* SPDX-License-Identifier: ISC *)
open Ocaml_ci_spindle

(* A tiny opam repository with two compilers and a library that needs the
   newer one in its newest release. *)
let repository () =
  let root = Filename.temp_file "ocaml-ci-repo-" "" in
  Sys.remove root;
  let write path text =
    let path = Filename.concat root path in
    let rec mkdirs d =
      if not (Sys.file_exists d) then (
        mkdirs (Filename.dirname d);
        Sys.mkdir d 0o755)
    in
    mkdirs (Filename.dirname path);
    Out_channel.with_open_bin path (fun oc -> output_string oc text)
  in
  let opam deps = "opam-version: \"2.0\"\ndepends: [" ^ deps ^ "]\n" in
  List.iter
    (fun (v, ocaml) ->
      write
        (Printf.sprintf
           "packages/ocaml-base-compiler/ocaml-base-compiler.%s/opam" v)
        (opam "");
      write
        (Printf.sprintf "packages/ocaml/ocaml.%s/opam" v)
        (opam (Printf.sprintf "\"ocaml-base-compiler\" {= \"%s\"}" ocaml)))
    [ ("4.14.2", "4.14.2"); ("5.4.1", "5.4.1"); ("5.4.0", "5.4.0") ];
  write "packages/ocaml-base-compiler/ocaml-base-compiler.5.5.0~beta1/opam"
    (opam "");
  write "packages/lib/lib.1.0/opam" (opam "\"ocaml\" {>= \"4.14\"}");
  write "packages/lib/lib.2.0/opam" (opam "\"ocaml\" {>= \"5.0\"}");
  write "packages/lib/lib.3.0~rc/opam"
    "opam-version: \"2.0\"\nflags: avoid-version\ndepends: [\"ocaml\"]\n";
  write "packages/testlib/testlib.1.0/opam" (opam "");
  root

let () =
  let root = repository () in
  let packages = Filename.concat root "packages" in
  (* Released versions only, newest first. *)
  assert (
    Solve.versions packages "ocaml-base-compiler"
    = [ "5.4.1"; "5.4.0"; "4.14.2" ]);
  (* [auto] picks 4.14 and the newest minors, at their newest release. *)
  let names vs =
    List.map (fun (v : Variant.t) -> (v.name, snd v.compiler)) vs
  in
  assert (
    names (Variant.choose ~packages [ "auto" ])
    = [
        ("5.4.1:x86_64:Debian-13", "5.4.1");
        ("4.14.2:x86_64:Debian-13", "4.14.2");
      ]);
  assert (
    names (Variant.choose ~packages [ "5.4" ])
    = [ ("5.4.1:x86_64:Debian-13", "5.4.1") ]);
  (* Each platform has its pool, image and opam variables, and every name is
     a workflow name a spindle accepts. *)
  let arm = Variant.v ~platform:(Debian Arm64) "5.4.1" in
  assert (arm.pool = "linux-arm64" && arm.vars.arch = "arm64");
  assert (arm.image = "ocaml/opam:debian-13-ocaml-5.4");
  let ppc = Variant.v ~platform:(Debian Ppc64le) "5.4.1" in
  assert (ppc.pool = "linux-ppc64" && ppc.vars.arch = "ppc64");
  assert (ppc.name = "5.4.1:ppc64le:Debian-13");
  let win = Variant.v ~platform:Windows "5.4.1" in
  assert (win.pool = "windows-x86_64" && win.vars.os = "win32");
  assert (win.image = "ocaml/opam:windows-server-mingw-ltsc2025-ocaml-5.4");
  let valid name =
    String.length name <= 255 && not (String.contains name '/')
  in
  List.iter
    (fun (_, platform) -> assert (valid (Variant.v ~platform "5.4.1").name))
    Variant.platforms;
  assert (Variant.oxcaml.name = "5.2.0+ox:x86_64:Debian-13");
  assert (win.name = "5.4.1:x86_64:Windows-mingw");
  assert (valid Variant.oxcaml.name);
  (* The project is solved per compiler, with its test dependencies, and an
     avoided version loses to a released one. *)
  let project =
    ( "demo",
      "opam-version: \"2.0\"\ndepends: [\"lib\" \"testlib\" {with-test}]\n" )
  in
  let vars = (Variant.v "5.4.1").vars in
  let solve ?(requires = []) ?(pins = [ project ]) compiler =
    Solve.solve
      {
        platform = vars;
        repositories = [ packages ];
        pins;
        compiler = ("ocaml", compiler);
        requires;
      }
  in
  (match solve "4.14.2" with
  | Ok p ->
      assert (List.mem "lib.1.0" p && List.mem "testlib.1.0" p);
      assert (List.mem "demo.dev" p && List.mem "ocaml.4.14.2" p)
  | Error e -> failwith e);
  (* A failed solve is explained by the project's own bound on the compiler
     where that is the reason, or else by the solver's first reason for each
     package in conflict. *)
  let bounded =
    ( "demo",
      "opam-version: \"2.0\"\ndepends: [\"ocaml\" {>= \"5.1\"} \"lib\"]\n" )
  in
  let request compiler pins : Solve.request =
    {
      platform = vars;
      repositories = [ packages ];
      pins;
      compiler = ("ocaml", compiler);
      requires = [];
    }
  in
  (match Solve.solve (request "4.14.2" [ bounded ]) with
  | Ok _ -> failwith "solved against the project's bound"
  | Error why ->
      let short = Solve.explain (request "4.14.2" [ bounded ]) why in
      assert (short = "demo requires ocaml >= 5.1"));
  let diagnostics =
    "Can't find all required versions.\n\
     Selected: ocaml-base-compiler.5.5.2\n\
     - ocaml -> ocaml.4.14.4\n\
    \    User requested = 4.14.4\n\
     - ocaml-compiler -> (problem)\n\
    \    ocaml-base-compiler 5.5.2 requires = 5.5.2\n\
    \    Rejected candidates:\n\
    \      ocaml-compiler.5.7: Incompatible with restriction: = 5.5.2\n\
     - ocaml-variants -> (problem)\n\
    \    Rejected candidates:\n\
    \      ocaml-variants.5.7.0+trunk: In same conflict class\n"
  in
  assert (
    Solve.explain (request "4.14.4" [ project ]) diagnostics
    = "ocaml-compiler: ocaml-base-compiler 5.5.2 requires = 5.5.2; \
       ocaml-variants: ocaml-variants.5.7.0+trunk: In same conflict class");
  assert (Solve.direct [ project; bounded ] = [ "lib"; "ocaml"; "testlib" ]);
  (match solve "5.4.1" with
  | Ok p -> assert (List.mem "lib.2.0" p)
  | Error e -> failwith e);
  (match solve "6.0" with
  | Ok _ -> failwith "solved for a compiler that does not exist"
  | Error _ -> ());
  (* A required package is in the solution though nothing depends on it. *)
  let bare = ("bare", "opam-version: \"2.0\"\n") in
  (match solve ~pins:[ bare ] ~requires:[ "testlib" ] "5.4.1" with
  | Ok p -> assert (List.mem "testlib.1.0" p)
  | Error e -> failwith e);
  (match solve ~pins:[ bare ] "5.4.1" with
  | Ok p -> assert (not (List.mem "testlib.1.0" p))
  | Error e -> failwith e);
  (* An earlier repository overrides a later one, as an overlay does. *)
  let overlay = repository () in
  let ov = Filename.concat overlay "packages/lib/lib.9.0" in
  Sys.mkdir ov 0o755;
  Out_channel.with_open_bin (Filename.concat ov "opam") (fun oc ->
      output_string oc "opam-version: \"2.0\"\n");
  (match
     Solve.solve
       {
         platform = vars;
         repositories = [ Filename.concat overlay "packages"; packages ];
         pins = [ project ];
         compiler = ("ocaml", "5.4.1");
         requires = [];
       }
   with
  | Ok p -> assert (List.mem "lib.9.0" p)
  | Error e -> failwith e);
  (* The spec pins the project, installs its solution without itself and
     quotes shell text. *)
  let spec =
    Spec.v ~variant:(Variant.v "5.4.1") ~opam_repository:"abc123"
      ~opam_files:[ "demo.opam" ]
      ~packages:[ "demo.dev"; "lib.2.0"; "ocaml.5.4.1" ]
      ()
  in
  let within spec s =
    let n = String.length s in
    let rec at i =
      i + n <= String.length spec && (String.sub spec i n = s || at (i + 1))
    in
    at 0
  in
  let contains = within spec in
  assert (
    String.starts_with ~prefix:"((from ocaml/opam:debian-13-ocaml-5.4)" spec);
  assert (contains "(env DEPS \"lib.2.0 ocaml.5.4.1\")");
  assert (contains "opam pin add -yn demo.dev './'");
  assert (contains "git reset -q --hard abc123");
  assert (contains "sudo ln -f /usr/bin/opam-2.6 /usr/bin/opam");
  assert (contains "(workdir /src)");
  assert (not (contains "oxcaml"));
  assert (not (contains "opam repository add"));
  (* A project's remotes go above opam-repository, the first highest, each at
     the commit solved against. *)
  let rspec =
    Spec.v ~variant:(Variant.v "5.4.1") ~opam_repository:"abc123"
      ~remotes:
        [
          ("https://github.com/x/one.git", "c1");
          ("https://github.com/x/two", "c2");
        ]
      ~opam_files:[ "demo.opam" ] ~packages:[ "demo.dev" ] ()
  in
  let index s =
    let n = String.length s in
    let rec at i =
      if i + n > String.length rspec then raise Not_found
      else if String.sub rspec i n = s then i
      else at (i + 1)
    in
    at 0
  in
  let two =
    index "opam repository add x2 'git+https://github.com/x/two#c2' --rank=1"
  and one =
    index
      "opam repository add x1 'git+https://github.com/x/one.git#c1' --rank=1"
  in
  assert (two < one && one < index "opam update -u");
  (* On Windows, files are copied to native paths and built from Cygwin's. *)
  let wspec =
    Spec.v ~variant:win ~opam_repository:"abc123" ~opam_files:[ "demo.opam" ]
      ~packages:[ "demo.dev"; "lib.2.0" ] ()
  in
  assert (within wspec "ln -f /usr/local/bin/opam-2.6 /usr/local/bin/opam");
  assert (within wspec "(dst /cygwin64/home/opam/src/)");
  assert (within wspec "(target \"c:\\\\opam\\\\.opam\\\\download-cache\")");
  assert (within wspec "cd /home/opam/src && opam exec -- dune build");
  assert (not (within wspec "sudo"));
  assert (not (within wspec "(workdir"));
  let ox =
    Spec.v ~variant:Variant.oxcaml ~opam_repository:"abc123"
      ~oxcaml_repository:("https://example.org/ox.git", "def456")
      ~opam_files:[ "demo.opam" ] ~packages:[] ()
  in
  assert (
    let n = String.length "oxcaml-compiler.5.2.0minus39" in
    let rec at i =
      i + n <= String.length ox
      && (String.sub ox i n = "oxcaml-compiler.5.2.0minus39" || at (i + 1))
    in
    at 0);
  (* OBuilder's operation lines become steps, without their colours. *)
  assert (
    Obuilder_log.operation
      "\027[01;34m/src: (run (shell \"opam install $DEPS\"))\027[0m"
    = Some "(run (shell \"opam install $DEPS\"))");
  assert (
    Obuilder_log.operation "\027[01;34m(from busybox)\027[0m"
    = Some "(from busybox)");
  assert (Obuilder_log.operation "-> installed dune.3.20.2" = None);
  (* A long operation spans lines until its brackets close. *)
  assert (Obuilder_log.depth "(run (cache (x (target /y)))" = 1);
  assert (Obuilder_log.depth "(run (shell \"echo ) (\\\" (\"))" = 0);
  assert (
    Obuilder_log.step_name
      "(run (cache (x (target /y))) (network host) (shell \"opam update -u\"))"
    = "opam update -u");
  assert (Obuilder_log.operation "(run (shell \"make\"))" = None);
  assert (
    Obuilder_log.step_name "(run (shell \"opam install $DEPS\"))"
    = "opam install $DEPS");
  assert (
    String.length
      (Obuilder_log.step_name ("(copy " ^ String.make 100 'x' ^ ")"))
    = 72);
  (* Only root opam files make a project. *)
  let files list : Spindle.Executor.files =
    {
      list = (fun _ -> list);
      read =
        (fun f -> if Filename.check_suffix f ".opam" then Some "x" else None);
    }
  in
  assert (Analysis.find (files [ "README.md"; "dune-project" ]) = None);
  (* Names that are not opam package names never reach a shell. *)
  assert (
    Analysis.find (files [ "a;b.opam"; "-x.opam"; ".opam"; "1.opam" ]) = None);
  assert (
    match Analysis.find (files [ "b.opam"; "README.md"; "a.opam" ]) with
    | Some a -> List.map fst a.opam_files = [ "a.opam"; "b.opam" ]
    | None -> false);
  (* A project's remotes are https addresses of public hosts, with a branch
     git allows, and the union of its opam files' in order of appearance. *)
  let remote s = Analysis.remote_of_string s in
  assert (
    remote "git+https://github.com/x/opam-repo.git#dev"
    = Ok { url = "https://github.com/x/opam-repo.git"; branch = Some "dev" });
  assert (
    remote "https://tangled.org/a.b/repo"
    = Ok { url = "https://tangled.org/a.b/repo"; branch = None });
  List.iter
    (fun s -> assert (Result.is_error (remote s)))
    [
      "git+ssh://github.com/x/y.git";
      "file:///etc";
      "git://github.com/x/y";
      "https://localhost/x";
      "https://10.0.0.1/x";
      "https://[::1]/x";
      "https://intranet/x";
      "https://svc.internal/x";
      "https://user:pw@github.com/x";
      "https://github.com/x#-upload-pack=evil";
      "https://github.com/x#a..b";
      "https://github.com/x#a b";
      "https://github.com/x y";
      "https://github.com/x';rm -rf /";
    ];
  let project opams : Spindle.Executor.files =
    {
      list = (fun _ -> List.map fst opams);
      read = (fun f -> List.assoc_opt f opams);
    }
  in
  let opam repos =
    Printf.sprintf "opam-version: \"2.0\"\nx-opam-repositories: [%s]\n"
      (String.concat " " (List.map (Printf.sprintf "%S") repos))
  in
  let a =
    Option.get
      (Analysis.find
         (project
            [
              ( "a.opam",
                opam
                  [
                    "git+https://github.com/x/one.git#main";
                    "file:///tmp/r";
                    "https://github.com/x/two";
                  ] );
              ( "b.opam",
                opam
                  [
                    "https://github.com/x/two";
                    "https://github.com/x/three";
                    "https://github.com/x/four";
                    "https://github.com/x/five";
                  ] );
              ("c.opam", "opam-version: \"2.0\"\n");
            ]))
  in
  let used, ignored = Analysis.remotes a in
  assert (
    List.map (fun (r : Analysis.remote) -> (r.url, r.branch)) used
    = [
        ("https://github.com/x/one.git", Some "main");
        ("https://github.com/x/two", None);
        ("https://github.com/x/three", None);
        ("https://github.com/x/four", None);
      ]);
  assert (
    List.map fst ignored = [ "file:///tmp/r"; "https://github.com/x/five" ]);
  (* A model's reasoning is dropped, and only project paths are read. *)
  assert (Diagnose.strip_thinking "<think>a\nb</think>\n\n- fix" = "- fix");
  assert (Diagnose.strip_thinking " - fix " = "- fix");
  assert (
    Diagnose.mentioned
      "File \"src/a.ml\", line 1\n\
       File \"/usr/x.ml\", line 2\n\
       File \"../y.ml\", line 3\n\
       File \"src/a.ml\", line 4"
    = [ "src/a.ml" ]);
  assert (
    Diagnose.answer {|{"error":{"message":"no such model"}}|}
    = Error "no such model");
  assert (
    Diagnose.answer {|{"choices":[{"message":{"content":"ok"}}]}|} = Ok "ok");
  print_endline
    "ocaml-ci: compiler choice, platforms, overlays, avoided versions, test \
     dependencies, specs, OBuilder log steps and diagnosis helpers passed"
