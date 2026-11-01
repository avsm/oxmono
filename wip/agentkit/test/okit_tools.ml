(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The dune and merlin tools, driven through their JSON schemas against a real
   okitd on a temporary workspace. The property that matters is that a write is
   answered by the build it caused, so the model reads the error in the file it
   has just written without asking for it.

   The tools now run their operations in okitd, and only the capability part is
   still done here: a write saves and a merlin query loads through the
   capability before anything crosses the pipe. So the negative checks matter
   more than they did. A path outside the capability must be refused here, since
   okitd holds the whole authority of this process and would not refuse it.

   The merlin tools are skipped when okitd found no ocamlmerlin to answer. *)

module Tool = Ds4.Tool
module Client = Okit.Client
module Proto = Okit.Proto

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* Drive a tool as the agent does, with JSON arguments through the codec. *)
let call tool args =
  Tool.invoke tool { Dsml.name = Tool.name tool; arguments = args; id = None }

(* A trace that keeps what it was told, so a test can ask whether a call named
   itself and its argument before it did any work. The client passes okitd's
   lines to one sink, the session's own among them, and the tools stream a
   call's traces to that same sink. *)
let recorder () =
  let seen = ref [] in
  ( (fun s -> seen := s :: !seen),
    fun sub -> List.exists (fun s -> contains s sub) !seen )

(* The binary under test, passed by the dune rule so that the test runs the one
   this build produced. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "humpty-cpu"

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test, and the
   server's socket path under it is longer than a unix socket address may be.
*)
let fixture ~fs name =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okit_test" name in
  let root = Eio.Path.(fs / dir) in
  let save p s =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(root / p) s
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "lib");
  save "dune-project" "(lang dune 3.21)\n";
  save "lib/dune" "(library (name fix))\n";
  save "lib/fix.ml" "let x = 1\n";
  (dir, root, save)

let merlin_tools ~saw ~client ~caps ~save =
  if not (Client.hello client).Proto.merlin then
    print_endline "ok   - skipped the merlin tools: no ocamlmerlin"
  else begin
    save "lib/fix.ml" "let x = 1\nlet y = x\n";
    let r =
      call
        (Okit.Toolbox.outline ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml"}|}
    in
    check "outline shows both values" (contains r "x" && contains r "y");
    check "outline names the file it was asked about"
      (saw "outline: lib/fix.ml");
    check
      "outline of an implementation with no interface says nothing about one"
      (not (contains r "fix.mli"));
    (* An interface beside the implementation is what the model should read
       first, so the outline of the implementation names it. *)
    save "lib/fix.mli" "val x : int\nval y : int\n";
    let r =
      call
        (Okit.Toolbox.outline ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml"}|}
    in
    check "outline of an implementation names its interface"
      (contains r "lib/fix.mli" && contains r "Outline that");
    let r =
      call
        (Okit.Toolbox.outline ~client ~caps)
        {|{"cap":"","path":"lib/fix.mli"}|}
    in
    check "outline of an interface names nothing else"
      (contains r "x" && not (contains r "lib/fix.ml "));
    let r =
      call
        (Okit.Toolbox.type_at ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml","line":1,"col":4}|}
    in
    check "type_at answers int" (contains r "int");
    check "type_at names the position it was asked about"
      (saw "type_at: lib/fix.ml:1:4");
    (* The definition is in the workspace, so it is named from the workspace
       root rather than as the absolute path merlin answers with, and the text
       of it is read through the capability and shown. *)
    let r =
      call
        (Okit.Toolbox.locate ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml","line":2,"col":8}|}
    in
    (* The place is matched whole, since a place under the root that is still
       written as an absolute path would satisfy any check on the tail of it. *)
    check "locate answers a place under the root"
      (String.starts_with ~prefix:"lib/fix.ml:1:4\n" r);
    check "locate shows the definition it found" (contains r "let x = 1");
    check "locate names the position it was asked about"
      (saw "locate: lib/fix.ml:2:8");
    (* A definition in an installed library is outside every capability held,
       so it is named and the model is told how to reach it, rather than being
       read by okitd, which would read it with this process's whole
       authority. *)
    let r =
      call
        (Okit.Toolbox.locate ~client ~caps)
        {|{"cap":"","path":"t/t.ml","line":1,"col":10}|}
    in
    check "a definition outside the workspace is named, not read"
      (contains r "outside this workspace" && contains r "open_dir");
    (* Every use of the name, the declaration in the interface among them,
       which is what the index okitd builds first is for. *)
    let r =
      call
        (Okit.Toolbox.occurrences ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml","line":1,"col":4}|}
    in
    check "occurrences finds the uses in the file"
      (contains r "lib/fix.ml:1:4" && contains r "lib/fix.ml:2:8");
    check "occurrences reaches the other files of the workspace"
      (contains r "lib/fix.mli:1:4");
    check "occurrences says it indexed first" (saw "occurrences: indexing");
    let r =
      call
        (Okit.Toolbox.search ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml","query":"int -> string"}|}
    in
    check "search finds a value by its type" (contains r "string_of_int");
    check "search names the query it ran" (saw "search: int -> string");
    let r =
      call
        (Okit.Toolbox.complete ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml","line":2,"col":0,"prefix":"List.ma"}|}
    in
    check "complete lists what an installed module offers"
      (contains r "map" && contains r "'a list");
    let r =
      call
        (Okit.Toolbox.errors ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml"}|}
    in
    check "errors reports a clean file as clean" (contains r "no errors");
    (* Left broken on disk and never built, since the point of this tool is to
       answer without one. *)
    save "lib/fix.ml" "let x : int = \"no\"\nlet y = x\n";
    let r =
      call
        (Okit.Toolbox.errors ~client ~caps)
        {|{"cap":"","path":"lib/fix.ml"}|}
    in
    check "errors reports what is wrong, with its position"
      (contains r "1:14" && contains r "error" && contains r "string");
    (* A capability that is not held is refused by name, not answered with
       nothing, so the model asks for access rather than for another path. *)
    (* The path is one no other call in this test names, so a trace holding it
       could only have come from this call reaching okitd. *)
    let r =
      call
        (Okit.Toolbox.outline ~client ~caps)
        {|{"cap":"<nope>","path":"lib/unasked.ml"}|}
    in
    check "an unknown capability is refused by name"
      (contains r "unknown capability" && contains r "nope");
    check "an unknown capability is refused before okitd is asked"
      (not (saw "lib/unasked.ml"))
  end

let run env =
  Eio.Switch.run @@ fun sw ->
  let proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, save = fixture ~fs "tools" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      let caps = Ds4.Toolbox.Caps.create ~sw ~fs root in
      let trace, saw = recorder () in
      match
        Client.start ~sw ~proc ~clock ~trace
          ~argv:[ exe; "okitd"; "--dir"; dir ]
      with
      | Error e ->
          print_endline e;
          check "start" false
      | Ok client ->
          check "start" (Client.hello client).Proto.dune;
          let write = Okit.Toolbox.write ~client ~caps in
          let r =
            call write
              {|{"cap":"","path":"lib/fix.ml","content":"let x : int = \"no\"\n"}|}
          in
          check "write reports the error"
            (contains r "int" && contains r "string");
          check "write names the line" (contains r "line 1");
          check "write names the file it was given" (saw "write: lib/fix.ml");
          let r =
            call write
              {|{"cap":"","path":"lib/fix.ml","content":"let x = 1\n"}|}
          in
          check "write reports ok" (contains r "build ok");
          (* A path a model writes as "./lib/fix.ml" is the same file, and its
             own diagnostics must still be the ones shown rather than counted
             among the workspace's. *)
          let r =
            call write
              {|{"cap":"","path":"./lib/fix.ml","content":"let x : int = \"no\"\n"}|}
          in
          check "a dotted path reports the file's own error"
            (contains r "int" && contains r "string" && contains r "line 1");
          let r =
            call write
              {|{"cap":"","path":"lib/fix.ml","content":"let x = 1\n"}|}
          in
          check "write reports ok after a dotted path" (contains r "build ok");
          (* A file dune does not compile is written and nothing is built. *)
          let r =
            call write {|{"cap":"","path":"notes.txt","content":"hello\n"}|}
          in
          check "a plain file is written alone"
            (contains r "wrote notes.txt" && not (contains r "build"));
          (* The capability is the only thing standing between the model and
             the rest of the filesystem, since okitd would write wherever it was
             asked to. Absolute and by name, and neither file is made. *)
          let escape = Filename.concat dir "escape.txt" in
          let r =
            call write
              (Printf.sprintf {|{"cap":"","path":%S,"content":"no\n"}|} escape)
          in
          check "an absolute path is refused with what to do instead"
            (contains r "outside this capability" && contains r "open_dir");
          check "an absolute path leaves no file" (not (Sys.file_exists escape));
          (* A path that climbs out is relative, so a check on the spelling of
             it passes the one spelling that matters. The capability refuses it
             because it resolves it, which is why the check is gone. The target
             is named after this workspace, since a file left in the parent by
             a run that escaped would otherwise be read as this run's. *)
          let name = Filename.basename dir ^ "-up.txt" in
          let up = Filename.concat (Filename.dirname dir) name in
          let r =
            call write
              (Printf.sprintf {|{"cap":"","path":"../%s","content":"no\n"}|}
                 name)
          in
          check "a path climbing out of the capability is refused"
            (contains r "outside this capability" && contains r "open_dir");
          check "a climbing path leaves no file" (not (Sys.file_exists up));
          let r =
            call write {|{"cap":"<nope>","path":"a.txt","content":"no\n"}|}
          in
          check "a write through an unknown capability is refused by name"
            (contains r "unknown capability" && contains r "nope");
          check "a refused write leaves no file"
            (not (Sys.file_exists (Filename.concat dir "a.txt")));
          (* An edit is a write of part of a file, so it must be answered by
             the same build the write was. A model that had to call build after
             an edit would learn to call build after a write too. *)
          let edit = Okit.Toolbox.edit ~client ~caps in
          save "lib/fix.ml" "let x = 1\nlet y = 2\n";
          let r =
            call edit
              {|{"cap":"","path":"lib/fix.ml","old":"let y = 2","new":"let y : int = \"no\""}|}
          in
          check "edit reports the error it caused"
            (contains r "int" && contains r "string");
          check "edit names the line it caused it on" (contains r "line 2");
          check "edit names the file it was given" (saw "write: lib/fix.ml");
          let r =
            call edit
              {|{"cap":"","path":"lib/fix.ml","old":"let y : int = \"no\"","new":"let y = 2"}|}
          in
          check "edit reports ok" (contains r "build ok");
          check "edit left the rest of the file alone"
            (Eio.Path.load Eio.Path.(root / "lib" / "fix.ml")
            = "let x = 1\nlet y = 2\n");
          (* A refusal must not build, since a build reporting ok after an edit
             that changed nothing reads as the edit having gone through. *)
          let r =
            call edit
              {|{"cap":"","path":"lib/fix.ml","old":"let z = 3","new":"let z = 4"}|}
          in
          check "an edit of a passage that is not there is refused"
            (contains r "is not in" && not (contains r "build ok"));
          check "a refused edit leaves the file as it was"
            (Eio.Path.load Eio.Path.(root / "lib" / "fix.ml")
            = "let x = 1\nlet y = 2\n");
          let r =
            call edit
              (Printf.sprintf {|{"cap":"","path":%S,"old":"a","new":"b"}|}
                 (Filename.concat dir "lib/fix.ml"))
          in
          check "an edit through an absolute path is refused"
            (contains r "outside this capability" && contains r "open_dir");
          let r =
            call edit
              {|{"cap":"<nope>","path":"lib/fix.ml","old":"a","new":"b"}|}
          in
          check "an edit through an unknown capability is refused by name"
            (contains r "unknown capability" && contains r "nope");
          let r = call (Okit.Toolbox.build ~client) "{}" in
          check "build with no targets builds everything"
            (contains r "build ok");
          check "build names the target it settled on" (saw "build: .");
          let r = call (Okit.Toolbox.build ~client) {|{"targets":"@check"}|} in
          check "an alias target builds" (contains r "build ok");
          (* okit writes the dep-spec, so one written by hand is refused with
             the spelling the tool takes. The server answers a malformed
             dep-spec with a Code_error that names nothing. *)
          let r =
            call (Okit.Toolbox.build ~client) {|{"targets":"(alias check)"}|}
          in
          check "a raw dep-spec is refused, naming the alias spelling"
            (contains r "dep-spec" && contains r "@check");
          let r = call (Okit.Toolbox.test ~client) "{}" in
          check "test runs the tests" (contains r "tests ok");
          check "test names itself before running" (saw "test: running");
          (* The property that matters is the failing one. A runtest request
             that names no directory tests nothing and answers Success, which
             had reported a failing test as a pass. *)
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "t");
          save "t/t.ml" "let () = print_string \"hello\\n\"\n";
          save "t/t.expected" "goodbye\n";
          save "t/dune"
            "(executable (name t) (modules t))\n\
             (rule (with-stdout-to t.out (run ./t.exe)))\n\
             (rule (alias runtest) (action (diff t.expected t.out)))\n";
          let r = call (Okit.Toolbox.test ~client) "{}" in
          check "a failing test is reported, with the file to promote"
            (contains r "tests failed" && contains r "promote");
          let r =
            call (Okit.Toolbox.promote ~client) {|{"path":"t/t.expected"}|}
          in
          check "promote accepts the file" (contains r "promoted");
          check "promote names the file it accepted"
            (saw "promote: t/t.expected");
          let r = call (Okit.Toolbox.test ~client) "{}" in
          check "the promoted test passes" (contains r "tests ok");
          (* okitd describes the workspace on every call, so the map is the one
             the workspace has now and the tool says nothing about a snapshot. *)
          let project = Okit.Toolbox.project ~client in
          let r = call project "{}" in
          check "project lists the library" (contains r "fix");
          (* One module rather than the whole map, since the whole map is what
             the bound cuts on a workspace of any size. *)
          let r = call project {|{"module":"Fix"}|} in
          check "project named a module reports the component holding it"
            (contains r "library fix" && contains r "lib/fix.ml");
          check "and reports that module alone" (not (contains r "\n  T "));
          let r = call project {|{"module":"lib/fix.ml"}|} in
          check "a module written as its file is the same module"
            (contains r "library fix");
          (* A module that is not built is said to be missing, rather than
             answered with an empty map that reads as a workspace with nothing
             in it. *)
          let r = call project {|{"module":"Nowhere"}|} in
          check "a module no component holds says so"
            (contains r "no module named Nowhere");
          check "project answers from a live describe"
            (not (contains r "session started"));
          check "project names the describe it ran" (saw "describe: running");
          (* A describe that fails is reported as such rather than answered
             with an empty map, which would read as a workspace with nothing in
             it. The map is taken per call now, so a workspace made
             undescribable under the session says so on the next call and
             recovers on the one after. *)
          save "lib/dune" "(library (name fix)\n";
          let r = call project "{}" in
          check "a describe that failed is reported rather than hidden"
            (contains r "could not be described");
          save "lib/dune" "(library (name fix))\n";
          let r = call project "{}" in
          check "and a workspace that is well again describes again"
            (contains r "fix" && not (contains r "could not be described"));
          let r = call (Okit.Toolbox.bash ~client) {|{"command":"echo hi"}|} in
          check "bash runs a command in okitd" (r = "hi\n");
          check "bash names the command it ran" (saw "bash: echo hi");
          merlin_tools ~saw ~client ~caps ~save;
          (* A tool answers with the death of the session rather than with
             nothing, so a model is told the server is gone. *)
          Client.stop client;
          let r = call (Okit.Toolbox.build ~client) "{}" in
          check "a tool answers with the death of the session"
            (contains r "okit's server" && contains r "stopped"))

let () =
  Eio_main.run run;
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
