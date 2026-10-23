module M = Siftbot_model
module S = Siftbot.Store
module I = Siftbot.Import
module P = Jmap.Proto

let check name condition = if not condition then failwith name
let conflict f = match f () with () -> false | exception S.Conflict -> true

let fails f =
  match f () with _ -> false | exception Invalid_argument _ -> true

let first = List.hd Siftbot.Demo.messages
let source id = { first.M.source with id }

let put db (m : M.message) =
  S.commit_batch db ~job:"test" ~expected:(S.checkpoint db "test")
    ~cursor:m.M.revision [ m ] []

let assignment ?(origin = M.Rule) ?(value = "uncertain") (m : M.message) =
  M.
    {
      target = m.source;
      conversation = false;
      dimension = Triage;
      value;
      origin;
      version = "test/v1";
      source_revision = m.revision;
      explanation = "Evidence";
      evidence = [ m.source ];
      score = Some 0.7;
    }

let store sw =
  let db = S.memory ~sw () in
  put db first;
  let second =
    { first with M.source = { first.source with account = "other" } }
  in
  put db second;
  check "account-scoped IDs" (List.length (S.messages db) = 2);
  check "FTS indexes messages" (List.length (S.search db "timetable") = 2);
  S.cache db ~source:first.source ~revision:first.revision ~kind:"summary/v1"
    ~value:"cached";
  let stale = { first with M.text = "updated"; revision = "changed" } in
  let revision = S.revision db in
  check "checkpoint conflict"
    (conflict (fun () ->
         S.commit_batch db ~job:"test" ~expected:None ~cursor:"bad" [ stale ] []));
  check "failed checkpoint leaves revision" (S.revision db = revision);
  check "failed checkpoint leaves source" (S.get db first.source = Some first);
  S.propose db [] [ assignment first ];
  S.edit db ~revision:(S.revision db)
    (M.Assign (assignment ~value:"relevant" first))
    ~explanation:"My supervision responsibility";
  S.propose db [] [ assignment ~value:"spam" first ];
  let snap = S.snapshot db ~query:"" in
  check "human decisions take precedence"
    (List.map (fun a -> a.M.value) snap.assignments = [ "relevant" ]);
  check "review revision conflict"
    (conflict (fun () -> S.edit db ~revision:0 M.Undo ~explanation:"wrong"));
  put db stale;
  check "changed source invalidates cache"
    (S.cached db ~source:first.source ~kind:"summary/v1" = None);
  check "stale cache write rejected"
    (conflict (fun () ->
         S.cache db ~source:first.source ~revision:first.revision
           ~kind:"summary/v1" ~value:"bad"));
  let rev = S.revision db in
  S.edit db ~revision:rev M.Undo ~explanation:"Undo my relevance decision";
  check "undo adds revision" (S.revision db = rev + 1);
  check "undo removes confirmation" ((S.snapshot db ~query:"").assignments = []);
  S.commit_batch db ~job:"test" ~expected:(S.checkpoint db "test")
    ~cursor:"deleted" [] [ first.source ];
  check "deletion removes retrieval" (S.get db first.source = None);
  check "deletion keeps other account" (S.get db second.source = Some second);
  check "deleted text leaves FTS" (List.length (S.search db "timetable") = 1);
  let area =
    M.
      {
        id = "teaching";
        name = "Teaching";
        definition = "Teaching";
        exclusions = "Research";
        parent = None;
        status = "proposed";
        revision = 0;
      }
  in
  S.propose db [ area ] [];
  let a = { (assignment second) with M.dimension = Area; value = "teaching" } in
  check "proposed areas cannot classify"
    (fails (fun () -> S.propose db [] [ a ]));
  S.edit db ~revision:(S.revision db) (M.Approve "teaching")
    ~explanation:"Approved definition";
  S.propose db [] [ a ];
  check "approval enables classification"
    (List.exists
       (fun a -> a.M.value = "teaching")
       (S.snapshot db ~query:"").assignments)

let protocol () =
  let request =
    M.Edit
      (3, Assign (assignment first), "quoted \"text\"\n\027[31m", "timetable")
  in
  check "protocol round trip"
    (Siftbot_protocol.parse_request (Siftbot_protocol.request request)
    = Ok request);
  check "version rejection"
    (Result.is_error
       (Siftbot_protocol.parse_request "(siftbot/v0 (Inspect x))"));
  check "bounded parser"
    (Result.is_error
       (Siftbot_protocol.parse_request
          (String.make (Siftbot_protocol.max_bytes + 1) 'x')));
  check "sanitization" (M.sanitize "hello\027[31m\226\128\174" = "hello?[31m?");
  let ui, id = Option.get (M.begin_request M.initial) in
  check "one operation" (M.begin_request ui = None);
  check "stale request ignored" (M.receive ui (id + 1) (Ok M.empty) = ui);
  let snapshot = { M.empty with messages = [ first ]; revision = 2 } in
  let ui = M.receive ui id (Ok snapshot) in
  let ui, id = Option.get (M.begin_request { ui with scroll = 4 }) in
  let refreshed = M.receive ui id (Ok { snapshot with revision = 3 }) in
  check "selection and scroll preserved"
    (refreshed.selected = Some first.source && refreshed.scroll = 4)

let identity () =
  let module C = Sortal.Contact in
  let old : C.affiliation =
    {
      org = "Old lab";
      department = None;
      title = None;
      url = None;
      address = None;
      from = Some (2020, 1, 1);
      until = Some (2025, 1, 1);
    }
  in
  let new_ =
    { old with org = "New lab"; from = Some (2025, 1, 1); until = None }
  in
  let contact =
    C.make ~handle:"person" ~names:[ "Person" ] ~emails:[ "p@example.test" ]
      ~affiliations:[ old; new_ ] ()
  in
  let people =
    Siftbot.Identity.[ { uid = "one"; contact }; { uid = "two"; contact } ]
  in
  let matches =
    Siftbot.Identity.resolve people ~aliases:[] ~address:"p@EXAMPLE.TEST"
      ~date:(2024, 12, 31)
  in
  check "ambiguous addresses retained" (List.length matches = 2);
  check "historical affiliation" ((List.hd matches).affiliations = [ "Old lab" ]);
  let matches =
    Siftbot.Identity.resolve people
      ~aliases:[ ("alias@example.test", "one") ]
      ~address:"alias@example.test" ~date:(2025, 1, 1)
  in
  check "reviewed alias and exclusive end boundary"
    (List.length matches = 1 && (List.hd matches).affiliations = [ "New lab" ]);
  check "duplicate relationship preserves sources"
    (List.length (Siftbot.Identity.duplicates Siftbot.Demo.messages) = 1)

let evaluation () =
  let split =
    Siftbot.Evaluation.split ~cutoff:"2026-07-01T00:00:00Z"
      Siftbot.Demo.messages
  in
  check "duplicate exclusion" (split.excluded_duplicates = 1);
  check "no future training replies"
    (List.for_all (fun m -> m.M.date < "2026-07-01T00:00:00Z") split.training);
  let truth = [ assignment ~origin:Admin ~value:"relevant" first ] in
  let prediction = [ assignment ~value:"spam" first ] in
  let r = Siftbot.Evaluation.score ~threshold:0.5 ~truth prediction in
  check "false spam and missed relevance denominators"
    (r.reviewed = 1 && r.predicted = 1 && r.false_spam = 1
   && r.missed_relevant = 1);
  let r = Siftbot.Evaluation.score ~threshold:0.9 ~truth prediction in
  check "abstention denominator"
    (r.abstained = 1 && r.reviewed = 1 && r.predicted = 0);
  let r = Siftbot.Evaluation.score ~threshold:0. ~truth:prediction prediction in
  check "predictions are not ground truth" (r.reviewed = 0);
  let proposals =
    Siftbot.Discovery.propose ~before:"2026-07-01T00:00:00Z"
      Siftbot.Demo.messages
  in
  check "reproducible discovery"
    (proposals
    = Siftbot.Discovery.propose ~before:"2026-07-01T00:00:00Z"
        (List.rev Siftbot.Demo.messages));
  let assignments = Siftbot.Discovery.classify [] Siftbot.Demo.messages in
  check "list does not imply spam or broadcast"
    (List.exists
       (fun a ->
         a.M.target.id = "list1" && a.dimension = Delivery
         && a.value = "mailing-list")
       assignments
    && not
         (List.exists
            (fun a -> a.M.value = "spam" || a.value = "broadcast")
            assignments))

let id s = Result.get_ok (P.Id.of_string_received s)

let get ?(state = "s1") ?(not_found = []) emails :
    P.Email.t P.Method.get_response =
  {
    account_id = id "a";
    state;
    list = emails;
    not_found = List.map id not_found;
  }

let email name thread day =
  P.Email.v ~id:(id name) ~thread_id:(id thread)
    ~received_at:
      (match Ptime.of_rfc3339 (day ^ "T12:00:00Z") with
      | Ok (t, _, _) -> t
      | _ -> assert false)
    ~subject:"Teaching timetable"
    ~message_id:[ name ^ "@test" ]
    ~text_body:[ P.Email_body.Part.v ~part_id:"1" () ]
    ~body_values:[ ("1", P.Email_body.Value.v "Body") ]
    ()

let html () =
  let email = email "html" "thread" "2026-01-02" in
  let value =
    {
      P.Email_body.Value.value =
        "<p>Visible &amp; useful</p><script>hidden</script>";
      is_encoding_problem = false;
      is_truncated = false;
    }
  in
  let email =
    {
      email with
      P.Email.text_body = Some [];
      html_body = Some [ P.Email_body.Part.v ~part_id:"html" () ];
      body_values = Some [ ("html", value) ];
    }
  in
  let m =
    I.message ~service:"https://example.test/session" ~account:"a"
      ~since:"2026-01-01T00:00:00Z" ~until:"2027-01-01T00:00:00Z" email
  in
  check "HTML original retained" (m.original = value.value);
  check "HTML text extracted without hidden content"
    (m.text = "Visible & useful")

let import sw =
  let db = S.memory ~sw () in
  let e1 = email "e1" "t1" "2026-01-10" in
  let e2 = email "e2" "t2" "2026-02-10" in
  let context = email "e0" "t1" "2025-01-10" in
  let live = ref [ e1; e2; context ] in
  let fail = ref false and query_state = ref "q1" in
  let changes =
    ref (fun since ->
        P.Method.
          {
            account_id = id "a";
            old_state = since;
            new_state = "s2";
            has_more_changes = false;
            created = [];
            updated = [];
            destroyed = [];
          })
  in
  let query ~since:_ ~until:_ ~position =
    let ids =
      if position = 0 then [ id "e1" ]
      else if position = 1 then [ id "e2" ]
      else []
    in
    let qr =
      Jmap_eio.Codec.decode_exn P.Method.query_response_jsont
        (Printf.sprintf
           {|{"accountId":"a","queryState":"%s","canCalculateChanges":false,"position":%d,"ids":%s,"total":2}|}
           !query_state position
           (Jmap_eio.Codec.encode_exn (Jsont.list P.Id.jsont) ids))
    in
    Jmap_eio.Mail.
      { query = qr; next_position = (if position < 1 then Some 1L else None) }
  in
  let read ids =
    if !fail then failwith "Interrupted";
    let emails =
      List.filter
        (fun e -> List.exists (fun i -> e.P.Email.id = Some (id i)) ids)
        !live
    in
    let found =
      List.map (fun e -> P.Id.to_string (Option.get e.P.Email.id)) emails
    in
    get ~not_found:(List.filter (fun i -> not (List.mem i found)) ids) emails
  in
  let remote =
    I.
      {
        service = "https://example.test/session";
        account = "a";
        username = "owner";
        batch_size = 1;
        state = (fun () -> "s1");
        query;
        read;
        thread =
          (fun tid ~position ->
            let emails =
              List.filter (fun e -> e.P.Email.thread_id = Some (id tid)) !live
            in
            let page = List.filteri (fun i _ -> i = position) emails in
            Jmap_eio.Mail.
              {
                thread_id = id tid;
                position;
                total = List.length emails;
                email_ids = List.map (fun e -> Option.get e.P.Email.id) page;
                emails = Some (get page);
                next_position =
                  (if position + 1 < List.length emails then Some (position + 1)
                   else None);
              });
        changes = (fun s -> !changes s);
      }
  in
  I.start db remote ~since:"2026-01-01T00:00:00Z" ~until:"2027-01-01T00:00:00Z";
  check "account username cannot change on resume"
    (fails (fun () ->
         I.start db
           { remote with username = "other-user" }
           ~since:"2026-01-01T00:00:00Z" ~until:"2027-01-01T00:00:00Z"));
  ignore (I.step db remote);
  let rev = S.revision db in
  fail := true;
  check "interrupted import reports failure"
    (match I.step db remote with _ -> false | exception Failure _ -> true);
  check "interrupted import keeps checkpoint" (S.revision db = rev);
  fail := false;
  I.start db remote ~since:"2020-01-01T00:00:00Z" ~until:"2030-01-01T00:00:00Z";
  check "resume keeps exact interval"
    (I.interval db remote
    = Some ("2026-01-01T00:00:00Z", "2027-01-01T00:00:00Z"));
  query_state := "q2";
  ignore (I.step db remote);
  check "changing listing preserves corpus" (List.length (S.messages db) = 1);
  let rec finish n =
    if n = 0 then failwith "import did not terminate"
    else if I.step db remote then ()
    else finish (n - 1)
  in
  finish 20;
  check "thread hydration" (List.length (S.messages db) = 3);
  check "outside interval marked"
    (List.exists
       (fun m -> m.M.source.id = "e0" && m.outside_interval)
       (S.messages db));
  let m = List.find (fun m -> m.M.source.id = "e1") (S.messages db) in
  S.cache db ~source:m.source ~revision:m.revision ~kind:"summary"
    ~value:"cached";
  (changes :=
     fun since ->
       P.Method.
         {
           account_id = id "a";
           old_state = since;
           new_state = "s3";
           has_more_changes = false;
           created = [];
           updated = [ id "e1" ];
           destroyed = [];
         });
  ignore (I.step db remote);
  check "keyword-only refresh keeps inference"
    (S.cached db ~source:m.source ~kind:"summary" = Some "cached");
  (changes :=
     fun since ->
       P.Method.
         {
           account_id = id "a";
           old_state = since;
           new_state = "s4";
           has_more_changes = false;
           created = [];
           updated = [];
           destroyed = [ id "e1" ];
         });
  ignore (I.step db remote);
  check "incremental deletion"
    (S.get db m.source = None
    && S.cached db ~source:m.source ~kind:"summary" = None);
  live := [ e2 ];
  (changes :=
     fun _ ->
       raise
         (Jmap_eio.Client.Jmap_client_error
            (Method_error
               (Result.get_ok
                  (P.Error.Method_error.v `Cannot_calculate_changes)))));
  ignore (I.step db remote);
  check "expired cursor retains published data until reconciliation"
    (List.length (S.messages db) = 2);
  (changes :=
     fun since ->
       P.Method.
         {
           account_id = id "a";
           old_state = since;
           new_state = "s5";
           has_more_changes = false;
           created = [];
           updated = [];
           destroyed = [];
         });
  finish 30;
  check "expired cursor reconciles missing records"
    (List.length (S.messages db) = 1);
  let incoming = email "new" "new-thread" "2027-02-01" in
  live := incoming :: !live;
  let delivered = ref false in
  (changes :=
     fun since ->
       let newly_created = if !delivered then [] else [ id "new" ] in
       delivered := true;
       P.Method.
         {
           account_id = id "a";
           old_state = since;
           new_state = "s6";
           has_more_changes = false;
           created = newly_created;
           updated = [];
           destroyed = [];
         });
  finish 20;
  check "watch imports new conversations beyond fixed initial interval"
    (List.exists (fun m -> m.M.source.id = "new") (S.messages db));
  check "new conversation hydration survives checkpoint"
    (S.hydration db ~service:remote.service ~account:remote.account = None)

let keyset sw =
  let db = S.memory ~sw () in
  let ids = [ "z"; "a"; "A"; "é"; "aa"; "a\000b" ] in
  let messages =
    List.map (fun id -> { first with M.source = source id; thread = "shared" })
      ids
  in
  let other =
    { first with M.source = { (source "a0") with account = "other" };
      thread = "foreign" }
  in
  S.commit_batch db ~job:"keyset" ~expected:None ~cursor:"1"
    (other :: messages) [];
  let service = first.source.service and account = first.source.account in
  let page after limit =
    S.message_ids_after db ~service ~account ~after ~limit
  in
  let rec collect after =
    match page after 2 with
    | [] -> []
    | ids -> ids @ collect (List.hd (List.rev ids))
  in
  check "keyset pages preserve binary ID order and account scope"
    (collect "" = List.sort String.compare ids);
  check "zero keyset budget returns no IDs" (page "" 0 = []);
  check "negative keyset budget rejected" (fails (fun () -> page "" (-1)));
  check "thread lookup scoped by account"
    (S.has_thread db ~service ~account ~thread:"shared"
    && not (S.has_thread db ~service ~account ~thread:"foreign"));
  let before = collect "" in
  check "keyset checkpoint conflict leaves pages unchanged"
    (conflict (fun () ->
         S.commit_batch db ~job:"keyset" ~expected:None ~cursor:"bad" []
           [ source "a" ])
    && collect "" = before);
  S.commit_batch db ~job:"keyset" ~expected:(Some "1") ~cursor:"2" []
    (List.map source ids);
  check "deletion empties keyset and thread membership"
    (page "" 10 = []
    && not (S.has_thread db ~service ~account ~thread:"shared"))

let () =
  protocol ();
  html ();
  identity ();
  evaluation ();
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          store sw;
          keyset sw;
          import sw));
  print_endline "Siftbot core tests passed"
