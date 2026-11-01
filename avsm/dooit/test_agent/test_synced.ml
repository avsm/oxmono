module A = Dooit_agent
module D = Dooit.Doc
module S = Dooit.Store
module G = Dooit.Git
open Dooit.Common

let check message condition = if not condition then failwith message
let ok = function Ok value -> json value | Error message -> failwith message

let error (result : (string, string) result) =
  match result with
  | Error message -> message
  | Ok _ -> failwith "Expected refusal"

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let () =
  let base = Filename.temp_file "dooit-agent-git-" "" in
  Unix.unlink base;
  mkdir base;
  Fun.protect
    ~finally:(fun () -> remove base)
    (fun () ->
      Eio_main.run @@ fun env ->
      let process_mgr = Eio.Stdenv.process_mgr env in
      let git root args =
        Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
          ([ "git"; "-C"; root ] @ args)
      in
      let author store =
        ignore (git store.S.root [ "config"; "user.name"; "Dooit Agent Test" ]);
        ignore
          (git store.root [ "config"; "user.email"; "dooit@example.invalid" ])
      in
      let remote = Filename.concat base "origin.git" in
      ignore (git base [ "init"; "--bare"; "--initial-branch=main"; remote ]);
      let config = G.{ remote; branch = "main" } in
      let a = S.init (Filename.concat base "a") in
      ignore (G.setup ~process_mgr a config);
      author a;
      let agent store =
        A.create ~process_mgr ~git:config ~store ~max_bytes:1024
      in
      let a_agent = agent a in
      let invoke ?(check = Eio.Fiber.check) ?operation agent name args =
        A.invoke agent ~check
          ~operation:(Option.value operation ~default:(new_uuid ()))
          name (json_string args)
      in
      let args =
        obj
          [ ("title", str "Synchronized task"); ("body", str "Original body") ]
      in
      ignore (invoke a_agent "dooit_create" args |> error);
      check "tools never bootstrap an unpushed store" (fst (S.scan a) = []);
      ignore (G.push ~process_mgr a config);
      let operation = new_uuid () in
      let created = invoke ~operation a_agent "dooit_create" args |> ok in
      let id = field "id" created and revision = field "revision" created in
      let remote_note id =
        git remote [ "show"; "refs/heads/main:notes/" ^ id ^ ".md" ] |> D.parse
      in
      check "create returns only after remote contains the note"
        (D.revision (remote_note id) = revision);
      let b = G.clone ~process_mgr ~root:(Filename.concat base "b") config in
      author b;
      let b_agent = agent b in
      let update revision status =
        obj
          [
            ("id", str id);
            ("revision", str revision);
            ( "operations",
              arr [ obj [ ("op", str "set_status"); ("value", str status) ] ] );
          ]
      in
      let updated =
        invoke a_agent "dooit_update" (update revision "done") |> ok
      in
      check "update immediately reaches remote"
        (D.revision (remote_note id) = field "revision" updated
        && D.status (remote_note id) = "done");
      check "other checkout remains stale until its write"
        (D.revision (S.get b id) = revision);
      let revoke_after_pull () =
        Eio.Fiber.check ();
        if D.revision (S.get b id) = field "revision" updated then
          failwith "revoked after pull"
      in
      (try
         ignore
           (invoke ~check:revoke_after_pull b_agent "dooit_update"
              (update revision "active"));
         failwith "authority failure during synchronization was swallowed"
       with Failure message when message = "revoked after pull" -> ());
      ignore (invoke b_agent "dooit_update" (update revision "active") |> error);
      check
        "pull precedes revision validation and stale update cannot overwrite"
        (D.revision (S.get b id) = field "revision" updated
        && D.status (remote_note id) = "done");
      let other =
        invoke b_agent "dooit_create"
          (set "title" (str "From second checkout") args)
        |> ok
      in
      let other_id = field "id" other in
      check "second consumer pushes to same remote"
        (D.revision (remote_note other_id) = field "revision" other);
      let previous_head = git remote [ "rev-parse"; "refs/heads/main" ] in
      let replay = invoke ~operation a_agent "dooit_create" args |> ok in
      check "completed create retry returns original receipt"
        (equal created replay);
      check "completed retry does not republish newer task revisions"
        (git remote [ "rev-parse"; "refs/heads/main" ] = previous_head
        && D.status (remote_note id) = "done");
      let failure_operation = new_uuid () in
      let pending_id = uuid5 a.id ("create:" ^ failure_operation) in
      let offline = Filename.concat base "offline.git"
      and disconnected = ref false in
      let disconnect_after_apply () =
        Eio.Fiber.check ();
        if (not !disconnected) && exists (S.note_path a pending_id) then (
          Unix.rename remote offline;
          disconnected := true)
      in
      let pending =
        invoke ~check:disconnect_after_apply ~operation:failure_operation
          a_agent "dooit_create" args
        |> error
      in
      check "test interrupted synchronization after local apply" !disconnected;
      Unix.rename offline remote;
      check "pending push response identifies locally saved task"
        (String.starts_with
           ~prefix:("Task " ^ pending_id ^ " saved locally")
           pending);
      check "failed push keeps valid local task"
        (D.id (S.get a pending_id) = pending_id);
      check "failed push did not advance remote"
        (git remote [ "rev-parse"; "refs/heads/main" ] = previous_head);
      let resumed =
        invoke ~operation:failure_operation a_agent "dooit_create" args |> ok
      in
      check "same host operation resumes push without duplicate"
        (field "id" resumed = pending_id
        && D.revision (remote_note pending_id) = field "revision" resumed);
      check "next write pulled other consumer's note"
        (D.revision (S.get a other_id) = field "revision" other);
      print_endline "Synchronized Dooit agent tests passed")
