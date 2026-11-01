module M = Siftbot_model
module S = Siftbot.Store
module F = Siftbot.Feedback

let check name ok = if not ok then failwith name
let base = List.hd Siftbot.Demo.messages

let mail id sender text =
  {
    base with
    M.source = { base.source with id };
    sender;
    subject = text;
    text;
    original = text;
    thread = id;
    message_ids = [ id ];
    date = "2026-01-01";
    revision = id;
  }

let label (m : M.message) value : M.assignment =
  {
    target = m.source;
    conversation = false;
    dimension = Attention;
    value;
    origin = Model;
    version = "test";
    source_revision = m.revision;
    explanation = "Model thought a reply was needed";
    evidence = [ m.source ];
    score = None;
  }

let put db job ms = S.commit_batch db ~job ~expected:None ~cursor:"1" ms []

let correct db ?scope m value explanation =
  let a = label m value in
  S.edit db ~revision:(S.revision db)
    (match scope with
    | None -> M.Assign a
    | Some scope -> Assign_rule (scope, a))
    ~explanation

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          let irrelevant =
            List.init 25 (fun i ->
                mail (Printf.sprintf "a%02d" i)
                  (Printf.sprintf "billing%d@example.org" i)
                  (Printf.sprintf "Receipt invoice order %d" i))
          in
          let useful =
            mail "z-useful" "opportunities@example.org"
              "Summer university research internship applications"
          in
          put db "sources" (useful :: irrelevant);
          List.iter (fun m -> correct db m "fyi" "Receipt only") irrelevant;
          S.propose db [] [ label useful "needs-reply" ];
          correct db useful "fyi"
            "Internship announcements do not need an email reply";
          let feedback = S.feedback db in
          check "all corrections retained" (List.length feedback = 26);
          let saved =
            List.find
              (fun (f : M.feedback) -> f.message.source = useful.source)
              feedback
          in
          check "original prediction retained"
            (List.exists
               (fun (a : M.assignment) -> a.value = "needs-reply")
               saved.previous);
          let query =
            {
              (mail "query" "students@example.org"
                 "University summer research internships for students")
              with
              date = "2026-02-01";
            }
          in
          let index = F.prepare db in
          let chosen = F.select index ~limit:8 query in
          check "correction beyond first twenty is retrieved"
            (List.exists
               (fun (e : F.example) -> e.feedback.id = saved.id)
               chosen);
          check "self excluded"
            (F.select index ~limit:100 useful
            |> List.for_all (fun (e : F.example) -> e.feedback.id <> saved.id));
          check "thread excluded"
            (F.select index ~limit:100 { query with thread = useful.thread }
            |> List.for_all (fun (e : F.example) -> e.feedback.id <> saved.id));
          check "duplicate body excluded"
            (F.select index ~limit:100 { query with text = useful.text }
            |> List.for_all (fun (e : F.example) -> e.feedback.id <> saved.id));
          check "duplicate ID excluded"
            (F.select index ~limit:100
               { query with message_ids = useful.message_ids }
            |> List.for_all (fun (e : F.example) -> e.feedback.id <> saved.id));
          check "other account excluded"
            (F.select index ~limit:100
               { query with source = { query.source with account = "another" } }
            = []);
          check "future evidence excluded for evaluation"
            (F.select ~chronological:true index ~limit:100
               { query with date = "2025-01-01" }
            = []);
          correct db useful "deadline" "Applications have a deadline";
          check "superseded correction excluded"
            (List.for_all
               (fun (f : M.feedback) ->
                 f.id <> saved.id || f.correction.value = "deadline")
               (S.feedback db));
          S.edit db ~revision:(S.revision db) Undo
            ~explanation:"Revert correction";
          check "undo restores original feedback"
            (List.exists
               (fun (f : M.feedback) ->
                 f.id = saved.id && f.correction.value = "fyi")
               (S.feedback db));
          let changed = { useful with revision = "new" } in
          put db "changed" [ changed ];
          check "changed source invalidates example"
            (List.for_all
               (fun (f : M.feedback) -> f.id <> saved.id)
               (S.feedback db));
          let semantic_db = S.memory ~sw () in
          let medical =
            mail "medical" "a@example.org" "physician appointment"
          in
          put semantic_db "medical" [ medical ];
          correct semantic_db medical "deadline" "Attend the appointment";
          let documents = ref 0 in
          let provider : Siftbot.Embedding.provider =
            {
              model = "fake";
              dimensions = 2;
              max_batch = 16;
              embed =
                (fun ~task ~check inputs ->
                  check ();
                  if task = `Document then
                    documents := !documents + List.length inputs;
                  List.map (fun _ -> [ 1.; 0. ]) inputs);
            }
          in
          let semantic = F.prepare ~provider semantic_db in
          ignore (F.prepare ~provider semantic_db);
          check "document vectors reused" (!documents = 1);
          let query = mail "doctor" "b@example.org" "doctor consultation" in
          check "no lexical match"
            (F.select (F.prepare semantic_db) ~limit:8 query = []);
          check "semantic match retrieved"
            (List.length (F.select semantic ~limit:8 query) = 1);
          correct semantic_db ~scope:Sender medical "fyi" "Sender rule";
          check "manual rules are feedback too"
            (List.length (S.feedback semantic_db) = 2)));
  print_endline
    "Feedback retrieval, leakage exclusion, correction lifecycle and semantic \
     cache passed"

let contains text pattern =
  let rec loop i =
    i + String.length pattern <= String.length text
    && (String.sub text i (String.length pattern) = pattern || loop (i + 1))
  in
  loop 0

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          let training =
            mail "training" "announcements@example.org"
              "University summer research internship opportunities"
          in
          let target =
            mail "target" "students@example.org"
              "Summer university research applications open"
          in
          let unrelated =
            mail "receipt" "billing@example.org" "Invoice payment receipt"
          in
          put db "mail" [ training; target; unrelated ];
          let calls = ref 0 and used_feedback = ref false in
          let complete (r : Agentkit.Chat.request) =
            incr calls;
            List.iter
              (function
                | Agentkit.Chat.User input ->
                    if contains input "Internship announcements need no reply"
                    then used_feedback := true
                | _ -> ())
              r.messages;
            Agentkit.Chat.response
              (Some
                 {|[{"dimension":"triage","value":"relevant","score":0.8,"explanation":"Evidence"},{"dimension":"delivery","value":"direct","score":0.8,"explanation":"Evidence"},{"dimension":"attention","value":"needs-reply","score":0.8,"explanation":"Evidence"}]|})
          in
          let run ?(refresh_feedback = false) () =
            Siftbot.Classifier.run ~refresh_feedback db ~complete ~model:"test"
          in
          check "initial classifications" (run () = 3 && !calls = 3);
          correct db training "fyi" "Internship announcements need no reply";
          check "correction doesn't invalidate whole mailbox"
            (run () = 0 && !calls = 3);
          check "only related target refreshes"
            (run ~refresh_feedback:true () = 1 && !calls = 4);
          check "correction reaches prompt" !used_feedback;
          check "unchanged feedback is skipped"
            (run ~refresh_feedback:true () = 0 && !calls = 4);
          let view =
            Siftbot.Review.snapshot db ~query:"" ~selected:target.source ()
          in
          check "review shows supplied feedback"
            (contains view.feedback_examples
               "Internship announcements need no reply");
          S.edit db ~revision:(S.revision db) Undo ~explanation:"Undo example";
          check "undo refreshes only affected provenance"
            (run ~refresh_feedback:true () = 1);
          check "old inference is reused when feedback returns to old state"
            (!calls = 4);
          let view =
            Siftbot.Review.snapshot db ~query:"" ~selected:target.source ()
          in
          check "undone example disappears from provenance"
            (not
               (contains view.feedback_examples
                  "Internship announcements need no reply"))));
  print_endline
    "Relevant feedback reaches prompts, selective refresh and provenance passed"
