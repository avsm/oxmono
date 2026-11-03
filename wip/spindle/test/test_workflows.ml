(* SPDX-License-Identifier: ISC *)
module W = Spindle__Workflows
module Mask = Spindle__Mask
module J = Spindle__Json

let () =
  (* Doublestar globs, as the reference spindle matches branches and tags. *)
  assert (W.matches "main" [ "main" ]);
  assert (W.matches "release/1.0" [ "release/*" ]);
  assert (not (W.matches "release/1.0/rc" [ "release/*" ]));
  assert (W.matches "release/1.0/rc" [ "release/**" ]);
  assert (W.matches "v1.2" [ "v[0-9].*" ]);
  assert (W.matches "dev" [ "{main,dev}" ]);
  assert (not (W.matches "devel" [ "{main,dev}" ]));
  assert (not (W.matches "main" []));
  let push ref_ =
    J.obj [ ("$type", J.str "org.tangled.event.push"); ("ref", J.str ref_) ]
  in
  let pull ?action target =
    J.obj
      ([
         ("$type", J.str "org.tangled.event.pullRequest");
         ("targetBranch", J.str target);
       ]
      @ Option.fold ~none:[] ~some:(fun a -> [ ("action", J.str a) ]) action)
  in
  let manual = J.obj [ ("$type", J.str "org.tangled.ci.trigger.manual") ] in
  let yaml =
    {|when:
  - event: ["push", "pull_request"]
    branch: main
  - event: push
    tag: "v*"
engine: nixery
environment:
  CGO_ENABLED: 1
  NAME: build
steps:
  - name: test
    command: echo "$NAME"
    environment:
      STEP: one
  - command: true
|}
  in
  let files = [ ("test.yml", yaml); ("bad.yml", "engine: [unclosed") ] in
  let select trigger =
    W.select ~repo:"did:plc:repo" ~commit:(String.make 40 'a') ~trigger
      ~names:[] files
  in
  let names (jobs, _) = List.map (fun (j : Spindle.Job.t) -> j.name) jobs in
  assert (names (select (push "refs/heads/main")) = [ "test.yml" ]);
  assert (names (select (push "refs/heads/dev")) = []);
  assert (names (select (push "refs/tags/v1")) = [ "test.yml" ]);
  assert (names (select (pull "main")) = [ "test.yml" ]);
  (* Closed and merged pull requests run only workflows that ask. *)
  assert (names (select (pull ~action:"closed" "main")) = []);
  assert (names (select (pull ~action:"synchronize" "main")) = [ "test.yml" ]);
  assert (names (select manual) = [ "test.yml" ]);
  let jobs, errors = select (push "refs/heads/main") in
  assert (List.map fst errors = [ "bad.yml" ]);
  let job = List.hd jobs in
  assert (job.env = [ ("CGO_ENABLED", "1"); ("NAME", "build") ]);
  (match job.steps with
  | [ { name = "test"; command; env = [ ("STEP", "one") ] }; second ] ->
      assert (command = {|echo "$NAME"|});
      assert (second.name = "step 2" && second.command = "true")
  | _ -> assert false);
  assert (job.yaml = Some yaml);
  let definition = Option.get job.definition in
  assert (
    J.get "path" (J.required "source" definition)
    = ".tangled/workflows/test.yml");
  assert (
    J.strings "triggers" definition
    = [
        "org.tangled.event.push";
        "org.tangled.event.pullRequest";
        "org.tangled.event.push";
      ]);
  (* A workflow without an engine is refused, as the reference spindle does. *)
  assert (
    names
      (W.select ~repo:"did:plc:repo" ~commit:(String.make 40 'a')
         ~trigger:manual ~names:[]
         [ ("noengine.yml", "steps: []") ])
    = []);
  (* Any printable file name is a workflow, as in the reference spindle. *)
  assert (W.valid_name ".hidden" && W.valid_name "my ci.yml");
  assert (not (W.valid_name "a/b" || W.valid_name "" || W.valid_name ".."));
  assert (not (W.valid_name "line\nbreak" || W.valid_name "\xff.yml"));
  (* Secret values, their lines and base64 forms are hidden in output, even
     split across reads. *)
  let mask = Mask.v [ "hunter2"; "line one\nline two" ] in
  (* Fed in order: a list literal's elements are evaluated right to left. *)
  let out =
    List.fold_left
      (fun acc chunk -> acc ^ Mask.feed mask chunk)
      ""
      [ "pass=hun"; "ter2 and "; Mask.base64 "hunter2"; " line two "; "hunt" ]
  in
  let out = out ^ Mask.finish mask in
  assert (out = "pass=*** and *** *** hunt");
  print_endline
    "workflows: globs, when, YAML steps, definitions and masking passed"
