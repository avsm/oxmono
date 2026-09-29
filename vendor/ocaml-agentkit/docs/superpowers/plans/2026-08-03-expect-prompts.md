# Expect Prompts Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `humpty expect` scripts gain prompt lines that drive the real model through the agent loop, printing every tool call, trace and result, so an agent hang is reproducible outside the interface.

**Architecture:** The script parser in `cmd/humpty_cmd.ml` returns a list of items, calls and prompts. The expect function in `bin/humpty.ml` loads the engine only when the script has a prompt, creates an `Agent.t` over the tools `assemble` already builds, and prints agent events through the transcript machinery expect already has. A live Metal test drives the whole thing against a real model.

**Tech Stack:** OCaml, Eio, cmdliner, dune cram tests. The spec is `docs/superpowers/specs/2026-08-03-expect-prompts-design.md`. Read it before starting any task.

## Global Constraints

- Work on the `tools` branch. It is already checked out.
- Build: `opam exec -- dune build`. Tests: `opam exec -- dune runtest`. Both must be clean before every commit.
- Format: `PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt` (ocamlformat 0.29.0 lives in the default opam switch, not the active one). If `--auto-promote` fails to write back, copy the result from `_build/default/.../.formatted/`.
- Prose norms from `CLAUDE.md` apply to every comment, doc string, man page and doc file: POSIX-manual density, complete sentences, no em-dashes, never join two clauses with a semicolon. Document a value as `[foo x y] is ...` naming its arguments.
- Commits: one per self-contained change, one-line message in the imperative, NO trailers and NO sign-off (this overrides any Co-Authored-By default).
- A tool or library must not silence a failure. Test the negative property where there is one.
- Cram fixtures that need a dune workspace go under `/tmp`, not `TMPDIR` (dune points TMPDIR inside its build directory, and unix socket paths have a ~104 byte limit).
- When a cram test's expected output is unknown, write the commands, run `opam exec -- dune runtest`, inspect the diff, then `opam exec -- dune promote` and re-run to confirm it is stable. Never guess transcript text.
- Do not touch `csrc/`.

---

### Task 1: Prompt lines in the script parser

**Files:**
- Modify: `cmd/humpty_cmd.ml` (module `Expect`, lines ~239-286)
- Modify: `cmd/humpty_cmd.mli` (module `Expect`, lines ~82-95)
- Modify: `bin/humpty.ml` (the `expect` function's parse site, ~line 484, and its two `List.iter` loops over `calls`)
- Test: `test/expect_unit.ml`

**Interfaces:**
- Consumes: the existing `Expect.call` record and `Expect.parse`.
- Produces: `type prompt = { line : int; text : string }`, `type item = Call of call | Prompt of prompt`, and `val parse : string -> (item list, string) result` in `Humpty_cmd.Expect`. Task 2 pattern-matches `Script.Call` and `Script.Prompt` (bin/humpty.ml aliases `Humpty_cmd.Expect` as `Script`).

- [ ] **Step 1: Write the failing tests**

Add to `test/expect_unit.ml`, after the existing parser tests. The existing tests destructure `call list` and will stop compiling; update them by mapping items through this helper, placed once near the top:

```ocaml
let calls items =
  List.filter_map
    (function Expect.Call c -> Some c | Expect.Prompt _ -> None)
    items
```

For example the first test becomes `check "reads every call" (List.length (calls items) = 2)` with `Ok items` in the match, and the single-call tests match `Ok [ Expect.Call c ]`. Then add the new checks:

```ocaml
(* Prompt lines. *)

let () =
  let script = "? describe this repository\nproject {}\n?   trimmed   \n" in
  match Expect.parse script with
  | Ok [ Expect.Prompt a; Expect.Call c; Expect.Prompt b ] ->
      check "a prompt keeps its text" (a.text = "describe this repository");
      check "a prompt keeps its line" (a.line = 1);
      check "order is the script's" (c.tool = "project" && c.line = 2);
      check "a prompt's text is trimmed" (b.text = "trimmed" && b.line = 3)
  | _ -> check "prompts and calls mix" false

let () =
  refused "a bare ? is refused, by line" "project {}\n?\n" "line 2";
  refused "a ? with only spaces is refused" "?   \n" "line 1"
```

Note the `refused` helper also asserts the error names the form (`build {}`), which the updated `form` message must still contain.

- [ ] **Step 2: Run the test to verify it fails**

Run: `opam exec -- dune build @test/runtest 2>&1 | head -40` (or `opam exec -- dune runtest test 2>&1 | head -40`).
Expected: a compile error, since `Expect.Prompt` does not exist yet.

- [ ] **Step 3: Implement the parser change**

In `cmd/humpty_cmd.ml`, module `Expect`:

Add after the `call` type:

```ocaml
  type prompt = { line : int; text : string }
  type item = Call of call | Prompt of prompt
```

Replace the `form` message so a refusal states both forms:

```ocaml
  let form =
    "A line is blank, a # comment, a prompt to send to the model, written as ? \
     followed by its text, or a tool call: the tool's name, one space, and its \
     arguments as the JSON object a model would send, as in: build {}"
```

In `parse`, after the blank-and-comment branch and before the `String.index_opt` split, add the prompt branch:

```ocaml
          else if s.[0] = '?' then
            let text = String.trim (String.sub s 1 (String.length s - 1)) in
            if text = "" then bad n "there is no text after ?"
            else go (n + 1) (Prompt { line = n; text } :: acc) rest
```

and wrap the existing call construction in `Call`: `go (n + 1) (Call { line = n; tool; arguments } :: acc) rest`.

In `cmd/humpty_cmd.mli`, module `Expect`, after the `call` type declare (mind the doc-comment spacing rules from CLAUDE.md, one blank line between a doc comment and the next declaration):

```ocaml
  type prompt = {
    line : int;  (** the line of the script the prompt was read from *)
    text : string;  (** the text to send to the model *)
  }
  (** One prompt read from a script, written as [?] followed by its text. *)

  type item = Call of call | Prompt of prompt
  (** One line of a script that does something: a tool call run directly, or a
      prompt sent to the model, which then runs the agent loop. *)

  val parse : string -> (item list, string) result
  (** [parse text] is the items [text] holds, in order. A line is blank, a [#]
      comment, a prompt, or a tool call: the tool's name, one space, and its
      arguments as a JSON object. The error names the line and the forms a line
      may take. *)
```

- [ ] **Step 4: Bridge bin/humpty.ml so the tree builds**

In `bin/humpty.ml`'s `expect` function, the parse site currently binds `calls`. Rename the binding to `items` and derive `calls` beside it. Task 2 replaces this bridge, and until then a script with a prompt is refused in words rather than mis-run:

```ocaml
  let items =
    match Script.parse (script_text env script) with
    | Ok items -> items
    | Error e -> failwith e
  in
  let calls =
    List.map
      (function
        | Script.Call c -> c
        | Script.Prompt (p : Script.prompt) ->
            failwith
              (Printf.sprintf
                 "line %d: a prompt needs a model, and this expect does not \
                  load one yet" p.line))
      items
  in
```

The two `List.iter` loops over `calls` below are unchanged.

- [ ] **Step 5: Run the tests and the cram suite**

Run: `opam exec -- dune build && opam exec -- dune runtest`.
Expected: all pass, including `expect_unit` and the unchanged `test/expect/*.t` transcripts.

- [ ] **Step 6: Format and commit**

```bash
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt --auto-promote
opam exec -- dune build && git add -A cmd test bin
git commit -m "read prompt lines in an expect script"
```

---

### Task 2: The engine branch and event printer in expect

**Files:**
- Modify: `bin/humpty.ml` (the `expect` function ~lines 474-575, the expect cmdliner section ~lines 822-886)
- Test: `test/expect/prompts.t` (new cram test)
- Modify: `CHANGES.md`

**Interfaces:**
- Consumes: `Script.Call`, `Script.Prompt`, `Script.prompt` from Task 1. `Agent.create`, `Agent.send`, `Agent.event` (`Reasoning`, `Content`, `Tool_call`, `Tool_result`, `Stats`, `Expanded`, `Done`) and `Agent.stats` from `lib/agent.mli`. `V4.create ~sw ~domain_mgr ~cache ~model ()`. The existing `assemble`, `resolve_model`, `resolve_seed`, `workspace_instructions`, `okit_status_line`, `script_text` helpers in `bin/humpty.ml`.
- Produces: the finished subcommand. Task 3 invokes `humpty-metal expect --dir DIR --prompt-timeout SECS SCRIPT` and reads its transcript: `? ` echoes, `> name {json}` tool echoes, bracketed traces, result blocks, `= reply` then the reply text, `= timeout after Ns` on expiry.

- [ ] **Step 1: Replace the expect function**

Replace the whole `expect` function in `bin/humpty.ml` with the following. It keeps every existing behaviour for scripted calls (same echoes, same scrubbing, same timeout wording for a call) and adds the prompt path. Read the current function first and carry over its comments where the code they explain survives.

```ocaml
let expect model_opt workspace system think seed ctx_size max_ctx timeout
    prompt_timeout raw script =
  run @@ fun env xdg ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  (* Read the whole script before anything is started, so that a script with a
     bad line costs no dune server. *)
  let items =
    match Script.parse (script_text env script) with
    | Ok items -> items
    | Error e -> failwith e
  in
  let has_prompt =
    List.exists
      (function Script.Prompt _ -> true | Script.Call _ -> false)
      items
  in
  (* A prompt drives the model, so the model is resolved before anything is
     started, as [agent] resolves it. A script with no prompt resolves
     nothing, so it runs on a machine that has no model. The path is checked
     here rather than left to the engine, whose refusal would come after
     okitd and a dune server had already been started. *)
  let model_path =
    if not has_prompt then None
    else
      let p = resolve_model ~xdg model_opt in
      if Sys.file_exists p then Some p
      else failwith (Printf.sprintf "model %s is not there" p)
  in
  Eio.Path.with_subtree Eio.Path.(fs / workspace) @@ fun ws ->
  (* A workspace is reached by two names on a machine where /tmp is a link, and
     tools report both: Eio uses the name it was given, and dune and merlin
     resolve it. *)
  let roots =
    Script.roots
      [
        Option.value ~default:"" (Eio.Path.native ws);
        (try Unix.realpath workspace with Unix.Unix_error _ -> "");
      ]
  in
  let scrub s = if raw then s else Script.scrub ~roots s in
  (* Every line of the transcript goes through here, and is flushed, so that a
     run that is later killed has said everything it had done. *)
  let emit s =
    print_string s;
    print_newline ();
    flush stdout
  in
  let line s = emit (scrub s) in
  (* The trace of the session's own start is not shown. It varies with how long
     dune takes to open its socket, which is what the status line reports the
     outcome of, and a transcript that gains a line per second of waiting is no
     use in a cram test. *)
  let running = ref false in
  (* The brackets go on after the scrubber has seen the line, so that a trace
     line ending in a duration still ends in one when it is dropped. *)
  let trace s = if !running then emit ("[" ^ scrub s ^ "]") in
  (* No approval callback, so a directory outside the workspace is granted when
     it is asked for, as it is for the agent. *)
  let caps = Toolbox.Caps.create ~sw ~fs ws in
  let tools, okit_system, status =
    assemble ~sw ~proc ~net ~clock ~caps ~trace ~root:ws
  in
  line (okit_status_line ~full:raw ~root:ws status);
  let find name = List.find_opt (fun t -> Tool.name t = name) tools in
  (* Checked for the whole script before any of it runs, since a name that no
     tool answers to is a fault in the script rather than a result to record. *)
  List.iter
    (function
      | Script.Prompt _ -> ()
      | Script.Call (c : Script.call) ->
          if find c.tool = None then
            failwith
              (Printf.sprintf
                 "line %d: no tool named %s. This workspace has: %s" c.line
                 c.tool
                 (String.concat ", " (List.map Tool.name tools))))
    items;
  (* The engine after [assemble], in the order [agent] keeps, so that okitd is
     spawned while this process is still small. It runs on its own domain, so
     that the fiber bounding a prompt keeps running during generation. The
     system prompt is assembled as [agent] assembles it, so a transcript
     exercises the conversation the interface would hold. *)
  let agent =
    match model_path with
    | None -> None
    | Some p ->
        let engine =
          V4.create ~sw
            ~domain_mgr:(Eio.Stdenv.domain_mgr env)
            ~cache:(Xdge.cache_dir xdg)
            ~model:Eio.Path.(fs / p)
            ()
        in
        let system =
          match okit_system with
          | None -> system
          | Some prompt -> system ^ "\n\n" ^ prompt
        in
        let system =
          match workspace_instructions ws with
          | None -> system
          | Some text -> system ^ "\n\n" ^ text
        in
        Some
          (Agent.create engine ~system ~thinking:think ~ctx_size
             ~max_ctx_size:max_ctx ~seed:(resolve_seed seed) ~tools)
  in
  running := true;
  (* Verbatim, except that the trailing newlines a block ends with become one,
     so a result is one block whatever the tool does about it. *)
  let chomp s =
    let n = ref (String.length s) in
    while !n > 0 && s.[!n - 1] = '\n' do
      decr n
    done;
    String.sub s 0 !n
  in
  let block s =
    let s = chomp s in
    if s <> "" then line s
  in
  (* One bound for a scripted call and another for a whole prompt exchange.
     Expiry ends the run by leaving the process rather than by returning to
     the script, since unwinding the switch would wait on the work that is
     already too slow. okitd is left with any call it is inside, and reads the
     end of its standard input as soon as that call is answered, so it stops
     itself and the dune server it started. Nothing later in the script could
     have been served anyway, and a run that times out is one to look at by
     hand. *)
  let bound ~seconds ~what f =
    Eio.Fiber.first f (fun () ->
        Eio.Time.sleep clock (float_of_int seconds);
        (* Not through the scrubber, since the seconds here are the ones that
           were asked for rather than ones that were measured. *)
        emit (Printf.sprintf "= timeout after %ds" seconds);
        prerr_endline
          (Printf.sprintf
             "humpty: %s did not answer within %d seconds, so the rest of the \
              script was not run"
             what seconds);
        exit 1)
  in
  let run_call (c : Script.call) =
    let tool = Option.get (find c.tool) in
    line (Printf.sprintf "> %s %s" c.tool c.arguments);
    block
      (bound ~seconds:timeout
         ~what:(Printf.sprintf "the %s call on line %d" c.tool c.line)
         (fun () ->
           Tool.invoke tool
             { Dsml.name = c.tool; arguments = c.arguments; id = None }))
  in
  (* The exchange prints as it happens: the model's tool calls echo as a
     scripted call does, and the reply is held back and printed whole under a
     [= reply] line when the exchange ends. Reasoning, per-turn stats and
     context growth are dropped unless [--raw], each being either unstable
     between runs or timing coloured. *)
  let run_prompt (p : Script.prompt) =
    let agent = Option.get agent in
    line (Printf.sprintf "? %s" p.text);
    let reply = Buffer.create 256 and reasoning = Buffer.create 256 in
    let on_event = function
      | Agent.Tool_call tc ->
          line (Printf.sprintf "> %s %s" tc.Dsml.name tc.Dsml.arguments)
      | Agent.Tool_result (_, result) -> block result
      | Agent.Content c -> Buffer.add_string reply c
      | Agent.Reasoning r -> if raw then Buffer.add_string reasoning r
      | Agent.Stats s ->
          if raw then
            emit
              (Printf.sprintf "= stats turns %d tools %d ctx %d/%d"
                 s.Agent.turns s.Agent.tool_calls s.Agent.ctx_used
                 s.Agent.ctx_size)
      | Agent.Expanded n ->
          if raw then emit (Printf.sprintf "= context expanded to %d" n)
      | Agent.Done ->
          if raw && Buffer.length reasoning > 0 then begin
            emit "= reasoning";
            block (Buffer.contents reasoning)
          end;
          emit "= reply";
          block (Buffer.contents reply)
    in
    bound ~seconds:prompt_timeout
      ~what:(Printf.sprintf "the prompt on line %d" p.line)
      (fun () -> Agent.send agent ~on_event p.text)
  in
  List.iter
    (function Script.Call c -> run_call c | Script.Prompt p -> run_prompt p)
    items
```

Notes for the implementer:
- The old function's timeout branch wording was "the %s call on line %d did not answer within %d seconds, so the rest of the script was not run". The `bound` helper reproduces exactly that sentence for a call (`what` = "the build call on line 3"), so no cram transcript changes.
- `Dsml.tool_call` is `{ name : string; arguments : string; id : string option }` (see `Tool.invoke`'s existing call site).
- `Agent.stats` field access needs the module path once per record as written above.

- [ ] **Step 2: Add the cmdliner arguments and rewire the term**

In the expect cmdliner section of `bin/humpty.ml`:

```ocaml
let expect_seed =
  Arg.(
    value & opt int 1
    & info [ "seed" ] ~docv:"N"
        ~doc:
          "The seed for the sampler when a prompt loads the model. It \
           defaults to a fixed value rather than to a fresh one, so that two \
           runs of one script sample identically.")

let expect_prompt_timeout =
  Arg.(
    value & opt int 600
    & info [ "prompt-timeout" ] ~docv:"SECS"
        ~doc:
          "How long one prompt's whole exchange may take, generation and tool \
           calls together. A prompt that takes longer ends the run, as a tool \
           call that exceeds $(b,--timeout) does.")
```

Rewire the term, reusing the agent's argument definitions:

```ocaml
  let term =
    with_log
      Term.(
        const expect $ model $ workspace $ agent_system $ agent_think
        $ expect_seed $ agent_ctx $ agent_max_ctx $ expect_timeout
        $ expect_prompt_timeout $ expect_raw $ expect_script)
  in
```

`model`, `agent_system`, `agent_think`, `agent_ctx` and `agent_max_ctx` are existing values in this file. They are defined above `expect_cmd` already (the agent command section precedes the expect section), so no reordering is needed.

- [ ] **Step 3: Update the expect man page**

In `expect_cmd`'s `man`, update the script-form paragraph and add one for prompts. Replace the `P` that begins "A script line is blank" with:

```ocaml
      `P
        "A script line is blank, a $(b,#) comment, a tool call, or a prompt. \
         A tool call is the tool's name, one space, and its arguments as the \
         JSON object a model would send, as in $(b,build {}). A prompt is \
         $(b,?) followed by the text to send, as in $(b,? describe this \
         repository).";
      `P
        "A script with no prompt loads no model, and a run takes as long as \
         the tools do. A script with a prompt resolves and loads the model \
         first, then runs each prompt through the same agent loop $(b,humpty \
         agent) runs: the model's tool calls print as scripted calls do, and \
         its reply prints under a $(b,= reply) line. All prompts share one \
         conversation. The seed defaults to a fixed value, so two runs of one \
         script sample identically on one machine.";
```

Also update the subcommand's one-line `doc` to: `"Run the agent's tools, or the agent itself, from a script and print the transcript."` and the first description paragraph to mention that a model is loaded only when the script has a prompt.

- [ ] **Step 4: Build and run the existing suites**

Run: `opam exec -- dune build && opam exec -- dune runtest`.
Expected: clean build, every existing test and cram transcript unchanged. If a transcript changed, the refactor broke a behaviour Task 1 or this task was to preserve. Fix the code, not the transcript.

- [ ] **Step 5: Write the negative cram test**

Create `test/expect/prompts.t` (the directory's dune stanza already runs every `.t` with `humpty-cpu` as a dependency, so no dune change is needed; verify by reading `test/expect/dune` and copy whatever per-file convention the other tests use if one exists):

```
A script with a prompt loads a model, so the model is resolved before
anything is started. A model path that is not there is refused with no
okitd spawned and no dune server started, which the absence of trace and
status lines shows. A script with no prompt never resolves a model, so a
bad --model does not matter to it.

  $ humpty-cpu expect --dir . --model /nonexistent/model.gguf <<'EOF'
  > ? describe this repository
  > EOF

  $ echo hello > note.txt
  $ humpty-cpu expect --dir . --model /nonexistent/model.gguf <<'EOF'
  > read {"cap":"","path":"note.txt"}
  > EOF
```

Run `opam exec -- dune runtest 2>&1 | head -60`, inspect the diff (the first command should fail with `humpty: ... model /nonexistent/model.gguf is not there` and a nonzero exit code recorded as `[N]`, the second should print the status line and the read transcript), then `opam exec -- dune promote` and re-run twice to confirm the output is stable. If the first command's output contains an unstable path or code, adjust the test's prose to explain what is pinned, not the assertion itself.

- [ ] **Step 6: Changelog**

Add to the current group in `CHANGES.md`:

```
- `humpty expect` scripts take prompt lines, written `? text`, which load the
  model and run the agent loop, printing each tool call and the reply.
- `humpty expect --prompt-timeout` bounds one prompt's whole exchange.
```

- [ ] **Step 7: Format, full test run, commit**

```bash
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt --auto-promote
opam exec -- dune build && opam exec -- dune runtest
git add -A bin test CHANGES.md
git commit -m "run prompt lines in an expect script through the model"
```

---

### Task 3: The live Metal scenario

**Files:**
- Create: `test/live_expect_prompts.ml`
- Modify: `test/metal/dune`
- Modify: `test/dune` (a comment only)
- Modify: `ARCH.md` (the tests list)
- Modify: `CHANGES.md`

**Interfaces:**
- Consumes: `humpty-metal expect --dir DIR --prompt-timeout SECS SCRIPT` from Task 2 and its transcript markers (`= reply`, `= timeout after`, `> ` tool echoes). `Humpty_cmd.Model.resolve` for the skip-when-no-model path.
- Produces: nothing later tasks use. This is the end-to-end pin.

- [ ] **Step 1: Write the test**

Create `test/live_expect_prompts.ml`:

```ocaml
(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Prompt lines in [humpty expect], driven end to end against a real model.

   The spawned humpty is what loads the model, so this runner holds nothing
   and the Metal build alone registers it: generation on the CPU build would
   take longer than anyone would wait. The script mixes the forms the parser
   takes. A scripted call writes a file the model has not seen, the first
   prompt is the request that once hung the interface, and the second prompt
   can only be answered by reading the file with a tool, so a reply that
   names its content proves the model drove a tool through okitd.

   It needs a real model, so it is gated on DS4_LIVE and skips when no model
   is downloaded. *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let holds s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let occurrences s sub =
  let n = String.length sub in
  let rec at i acc =
    if n = 0 || i + n > String.length s then acc
    else if String.sub s i n = sub then at (i + n) (acc + 1)
    else at (i + 1) acc
  in
  at 0 0

(* The binary under test, passed by the dune rule. It is the Metal humpty,
   since the spawned process is the one that loads the model. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "humpty-metal"

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test. *)
let fixture ~fs =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "expect_prompts" "ws" in
  let root = Eio.Path.(fs / dir) in
  let save p s =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(root / p) s
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "lib");
  save "dune-project" "(lang dune 3.21)\n";
  save "lib/dune" "(library (name fix))\n";
  save "lib/fix.ml" "let x = 1\n";
  dir

let script =
  "write {\"cap\":\"\",\"path\":\"note.txt\",\"content\":\"the answer is 42\\n\"}\n\
   ? describe this repository\n\
   ? read note.txt and tell me what the answer is\n"

let run env =
  let fs = Eio.Stdenv.fs env in
  let dir = fixture ~fs in
  let script_file = dir ^ ".script" in
  let transcript = dir ^ ".transcript" in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Sys.command
           (Filename.quote_command "rm" [ "-rf"; dir; script_file; transcript ])))
    (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o644)
        Eio.Path.(fs / script_file)
        script;
      Printf.printf "running %s expect (the model loads first) ...\n%!" exe;
      (* Through the shell so that the transcript survives a nonzero exit,
         which is exactly the run worth reading. Standard error is inherited,
         so the engine's own report of a failure to load appears in the test
         log. *)
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:transcript
             [
               "expect";
               "--dir";
               dir;
               "--prompt-timeout";
               "300";
               script_file;
             ])
      in
      let out =
        In_channel.with_open_text transcript In_channel.input_all
      in
      print_string out;
      check "the run exited zero" (status = 0);
      check "no prompt timed out" (not (holds out "= timeout"));
      check "both prompts were answered" (occurrences out "= reply" = 2);
      check "the model called a tool" (holds out "\n> ");
      check "the second reply read the note" (holds out "42"));
  if !failures = 0 then print_string "\nLive expect prompts test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let () =
  match Sys.getenv_opt "DS4_LIVE" with
  | None ->
      print_string "SKIP - live expect prompts test (set DS4_LIVE=1 to run)\n"
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Humpty_cmd.Model.resolve ~dir:(Humpty_cmd.Model.dir xdg) None with
      | Error e -> Printf.printf "SKIP - %s\n" e
      | Ok _ -> run env)
```

One check deserves a note: `holds out "\n> "` accepts any tool echo. The model is steered toward `project` by the system prompt, but pinning the exact tool would make the test fail on a legitimate change of the model's mind. The transcript is printed, so a reader sees which tool it chose.

- [ ] **Step 2: Register the Metal build**

Append to `test/metal/dune`, following the file's existing pattern exactly:

```
(copy_files
 (files ../live_expect_prompts.ml))

(executable
 (name live_expect_prompts)
 (enabled_if
  (= %{system} macosx))
 (libraries humpty.cmd eio eio_main xdge)
 (modules live_expect_prompts))

(rule
 (alias runtest)
 (enabled_if
  (= %{system} macosx))
 (deps %{bin:humpty-metal})
 (action
  (run %{exe:live_expect_prompts.exe} %{bin:humpty-metal})))
```

Add a comment above it in the style of the file's other comments, saying that this one has no CPU registration because the spawned humpty is what loads the model and CPU generation is impractically slow.

Then add a short comment at the end of `test/dune`:

```
; live_expect_prompts.ml has no stanza here on purpose. It drives a spawned
; humpty that loads the model, so only the Metal registration in ./metal runs
; it. Every stanza above lists its modules, so the orphan module is inert.
```

- [ ] **Step 3: Verify the gating without a model**

Run: `opam exec -- dune build && opam exec -- dune runtest`.
Expected: on this machine the new test prints its SKIP line (or runs, if DS4_LIVE is exported, which it should not be for this step). Everything else unchanged.

- [ ] **Step 4: Docs**

In `ARCH.md`, find the tests list (it names `live_okit_fork`) and add one line beside it:

```
- `test/live_expect_prompts.ml` drives `humpty expect` prompt lines against a
  real model, Metal only, gated on `DS4_LIVE`.
```

Match the surrounding list's exact formatting. In `CHANGES.md`, add to the group from Task 2:

```
- A live Metal test pins that an expect prompt exchange completes against a
  real model.
```

- [ ] **Step 5: Format, full run, commit**

```bash
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt --auto-promote
opam exec -- dune build && opam exec -- dune runtest
git add -A test ARCH.md CHANGES.md
git commit -m "pin an expect prompt exchange against a live model"
```

- [ ] **Step 6: Report the live-run command**

The live run itself is for the user's Metal machine with the model present and nothing else using it:

```bash
DS4_LIVE=1 opam exec -- dune build @test/metal/runtest
```

Do not run it if a model is currently loaded elsewhere (one model at a time). If a model is present and nothing else is running, run it once and report the transcript. If the "describe this repository" hang reproduces, the transcript now shows where it cut off: report that as the finding, since producing exactly that evidence is what this feature is for.
