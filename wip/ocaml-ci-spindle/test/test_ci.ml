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
    = [ ("debian-12-ocaml-4.14", "4.14.2"); ("debian-12-ocaml-5.4", "5.4.1") ]);
  assert (
    names (Variant.choose ~packages [ "5.4" ])
    = [ ("debian-12-ocaml-5.4", "5.4.1") ]);
  (* The project is solved per compiler, with its test dependencies, and an
     avoided version loses to a released one. *)
  let project =
    ( "demo",
      "opam-version: \"2.0\"\ndepends: [\"lib\" \"testlib\" {with-test}]\n" )
  in
  let solve compiler =
    Solve.solve
      {
        platform = Solve.debian_12_x86_64;
        repositories = [ packages ];
        pins = [ project ];
        compiler = ("ocaml", compiler);
      }
  in
  (match solve "4.14.2" with
  | Ok p ->
      assert (List.mem "lib.1.0" p && List.mem "testlib.1.0" p);
      assert (List.mem "demo.dev" p && List.mem "ocaml.4.14.2" p)
  | Error e -> failwith e);
  (match solve "5.4.1" with
  | Ok p -> assert (List.mem "lib.2.0" p)
  | Error e -> failwith e);
  (match solve "6.0" with
  | Ok _ -> failwith "solved for a compiler that does not exist"
  | Error _ -> ());
  (* An earlier repository overrides a later one, as an overlay does. *)
  let overlay = repository () in
  let ov = Filename.concat overlay "packages/lib/lib.9.0" in
  Sys.mkdir ov 0o755;
  Out_channel.with_open_bin (Filename.concat ov "opam") (fun oc ->
      output_string oc "opam-version: \"2.0\"\n");
  (match
     Solve.solve
       {
         platform = Solve.debian_12_x86_64;
         repositories = [ Filename.concat overlay "packages"; packages ];
         pins = [ project ];
         compiler = ("ocaml", "5.4.1");
       }
   with
  | Ok p -> assert (List.mem "lib.9.0" p)
  | Error e -> failwith e);
  (* The spec pins the project, installs its solution without itself and
     quotes shell text. *)
  let spec =
    Spec.v
      ~variant:(Variant.ocaml "5.4" "5.4.1")
      ~opam_repository:"abc123" ~opam_files:[ "demo.opam" ]
      ~packages:[ "demo.dev"; "lib.2.0"; "ocaml.5.4.1" ]
      ()
  in
  let contains s =
    let n = String.length s in
    let rec at i =
      i + n <= String.length spec && (String.sub spec i n = s || at (i + 1))
    in
    at 0
  in
  assert (
    String.starts_with ~prefix:"((from ocaml/opam:debian-12-ocaml-5.4)" spec);
  assert (contains "(env DEPS \"lib.2.0 ocaml.5.4.1\")");
  assert (contains "opam pin add -yn demo.dev ./");
  assert (contains "git reset -q --hard abc123");
  assert (not (contains "oxcaml"));
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
  assert (
    match Analysis.find (files [ "b.opam"; "README.md"; "a.opam" ]) with
    | Some a -> List.map fst a.opam_files = [ "a.opam"; "b.opam" ]
    | None -> false);
  print_endline
    "ocaml-ci: compiler choice, overlays, avoided versions, test dependencies, \
     specs and OBuilder log steps passed"
