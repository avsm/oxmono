(* SPDX-License-Identifier: ISC *)
open Tangled_matrix
module W = Tangled.Watch

let has s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let repo : Tangled.Api.repository =
  {
    did = "did:plc:repo";
    owner = "did:plc:alice";
    name = "demo";
    knot = "https://knot.example";
    manifest =
      (match
         Jsont_bytesrw.decode_string Tangled.Api.Lex.Repo.Manifest.main_jsont
           {|{"$type":"org.tangled.repo.manifest","createdAt":"2026-01-01T00:00:00Z",
              "declaration":{"owner":"did:plc:alice","name":"demo"}}|}
       with
      | Ok m -> m
      | Error e -> failwith e);
    manifest_value = Jsont.Json.object' [];
    manifest_cid = None;
  }

let workflow ?error name status : W.workflow =
  { name; status; error; started_at = None; finished_at = None }

let pipeline workflows : W.pipeline =
  {
    repo;
    spindle = "https://spindle.example";
    id = "3m7cd";
    commit = String.make 40 'a';
    trigger =
      Jsont.Json.object'
        [
          Jsont.Json.mem (Jsont.Json.name "$type")
            (Jsont.Json.string "org.tangled.event.push");
          Jsont.Json.mem (Jsont.Json.name "ref")
            (Jsont.Json.string "refs/heads/main");
        ];
    created_at = None;
    workflows;
  }

let render () =
  let p =
    pipeline
      [
        workflow "5.5.1:x86_64:Debian-13" "success";
        workflow "4.14.2:x86_64:Debian-13" "failed"
          ~error:"Failed: <Build> failed\nmore detail";
        workflow "5.5.1:arm64:Debian-13" "running";
      ]
  in
  let m = Render.pipeline ~name:"alice.example/demo" p in
  assert (
    has m.plain
      "🔄 alice.example/demo: push refs/heads/main aaaaaaa running, 1 of 3 \
       passed");
  assert (has m.plain "❌ 4.14.2:x86_64:Debian-13: Failed: <Build> failed\n");
  assert (not (has m.plain "more detail"));
  assert (has m.plain "https://tangled.org/did:plc:alice/demo/pipelines/3m7cd");
  (* HTML from build output is escaped. *)
  assert (has m.html "Failed: &lt;Build&gt; failed");
  assert (not (has m.html "<Build>"));
  assert (has m.html "<li>✅ <code>5.5.1:x86_64:Debian-13</code></li>");
  let done_ =
    Render.pipeline ~name:"alice.example/demo"
      (pipeline [ workflow "a" "success"; workflow "b" "success" ])
  in
  assert (
    has done_.plain
      "✅ alice.example/demo: push refs/heads/main aaaaaaa passed, 2 of 2 passed")

let messages env =
  let dir = Filename.temp_file "tangled-matrix-" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o700;
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree root) @@ fun () ->
  let path = Eio.Path.(root / "messages.json") in
  let t = Messages.load path in
  assert (Messages.entries t = []);
  let entry pipeline finished : Messages.entry =
    {
      repo = "did:plc:repo";
      pipeline;
      room = "!r:x";
      event = None;
      finished;
      shown = "";
    }
  in
  let e = entry "p1" false in
  Messages.add t e;
  e.event <- Some "$ev";
  Messages.add t (entry "p0" true);
  Messages.save t;
  (* What was saved comes back, and is readable only by its owner. *)
  let t = Messages.load path in
  (match Messages.find t ~repo:"did:plc:repo" ~pipeline:"p1" ~room:"!r:x" with
  | Some e -> assert (e.event = Some "$ev" && not e.finished)
  | None -> assert false);
  assert (Messages.unfinished t = [ ("did:plc:repo", "p1") ]);
  assert ((Unix.stat (Eio.Path.native_exn path)).st_perm land 0o077 = 0);
  (* At most 1000 are kept, dropping finished ones first. *)
  for i = 1 to 1100 do
    Messages.add t (entry (string_of_int i) true)
  done;
  assert (List.length (Messages.entries t) = 1000);
  assert (
    Messages.find t ~repo:"did:plc:repo" ~pipeline:"p1" ~room:"!r:x" <> None);
  assert (Messages.find t ~repo:"did:plc:repo" ~pipeline:"1" ~room:"!r:x" = None);
  (* A corrupt store is moved aside. *)
  Eio.Path.save ~create:(`Or_truncate 0o600) path "not json";
  assert (Messages.entries (Messages.load path) = []);
  assert (Sys.file_exists (Filename.concat dir "messages.json.bad"))

let spindles () =
  let on spindle =
    let manifest =
      match
        Jsont_bytesrw.decode_string Tangled.Api.Lex.Repo.Manifest.main_jsont
          (Printf.sprintf
             {|{"$type":"org.tangled.repo.manifest","createdAt":"2026-01-01T00:00:00Z",
                "declaration":{"owner":"did:plc:alice","name":"demo"},"spindle":"%s"}|}
             spindle)
      with
      | Ok m -> m
      | Error e -> failwith e
    in
    { repo with manifest }
  in
  let ours = on "did:web:spindle.recoil.org"
  and theirs = on "did:web:spindle.tangled.network" in
  let served = Bindings.served in
  (* By host or DID, and any spindle when none is named. *)
  assert (served ~spindles:[ "spindle.recoil.org" ] ours);
  assert (served ~spindles:[ "did:web:spindle.recoil.org" ] ours);
  assert (not (served ~spindles:[ "spindle.recoil.org" ] theirs));
  assert (served ~spindles:[] theirs);
  assert (not (served ~spindles:[] repo))

let bindings () =
  let wanted =
    [
      ("did:plc:repo", ("did:plc:alice", "!b:x"));
      ("did:plc:repo", ("did:plc:alice", "!a:x"));
      ("did:plc:repo", ("did:plc:alice", "!a:x"));
      ("did:plc:repo", ("did:plc:bob", "!bob:x"));
      ("did:plc:repo", ("did:plc:mallory", "!spam:x"));
      ("did:plc:other", ("did:plc:alice", "!o:x"));
    ]
  in
  let asked = ref false in
  let collaborators =
    lazy
      (asked := true;
       [ "did:plc:bob" ])
  in
  assert (
    Bindings.rooms ~repo:"did:plc:repo" ~owner:"did:plc:alice" ~collaborators
      wanted
    = [ "!a:x"; "!b:x"; "!bob:x" ]);
  assert !asked;
  (* Collaborators are not asked for when only the owner bound it. *)
  let asked = ref false in
  let collaborators =
    lazy
      (asked := true;
       [])
  in
  assert (
    Bindings.rooms ~repo:"did:plc:other" ~owner:"did:plc:alice" ~collaborators
      wanted
    = [ "!o:x" ]);
  assert (not !asked)

let () =
  Eio_main.run @@ fun env ->
  render ();
  messages env;
  bindings ();
  spindles ();
  print_endline
    "tangled-matrix: rendering, escaping, the message store, binding \
     permissions and spindle filtering passed"
