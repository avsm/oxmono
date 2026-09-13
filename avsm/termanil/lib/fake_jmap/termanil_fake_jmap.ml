(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module C = Dooit.Common
module M = Termanil_model

let service = "https://mail.example.test/jmap/session"
let account = "personal"
let int n = `Float (float_of_int n)
let number = function `Float n -> int_of_float n | _ -> 0
let text k v = Option.value (Option.map C.string (C.find k v)) ~default:""
let items k v = Option.value (Option.map C.list (C.find k v)) ~default:[]
let fields k v = Option.value (Option.map C.assoc (C.find k v)) ~default:[]
let strings xs = C.arr (List.map C.str xs)
let set k v obj = C.obj ((k, v) :: List.remove_assoc k (C.assoc obj))
let address name email = C.obj [ ("name", C.str name); ("email", C.str email) ]

let email (m : M.message) body =
  C.obj
    [
      ("id", C.str m.source.id);
      ("threadId", C.str m.thread_id);
      ("blobId", C.str ("blob-" ^ m.source.id));
      ("size", int (String.length body));
      ("messageId", strings [ m.source.id ^ "@example.test" ]);
      ("references", strings []);
      ( "from",
        C.arr
          [
            address
              (List.hd (String.split_on_char ' ' m.sender))
              (List.hd m.addresses);
          ] );
      ("to", C.arr [ address "You" "you@example.test" ]);
      ("cc", C.arr [ address "Lin" "lin@example.test" ]);
      ("sentAt", C.str m.received);
      ("subject", C.str m.subject);
      ("receivedAt", C.str m.received);
      ("preview", C.str m.preview);
      ("mailboxIds", C.obj [ ("inbox", `Bool true) ]);
      ( "keywords",
        C.obj
          ((if m.seen then [ ("$seen", `Bool true) ] else [])
          @ if m.flagged then [ ("$flagged", `Bool true) ] else []) );
      ( "textBody",
        C.arr
          [
            C.obj
              [
                ("partId", C.str "text");
                ("type", C.str "text/plain");
                ("size", int (String.length body));
              ];
          ] );
      ( "bodyValues",
        C.obj
          [
            ( "text",
              C.obj
                [
                  ("value", C.str body);
                  ("isTruncated", `Bool false);
                  ("isEncodingProblem", `Bool false);
                ] );
          ] );
    ]

let initial () =
  let messages =
    List.map
      (fun m -> email m (Termanil_demo.body m.M.source.id))
      Termanil_demo.messages
  in
  let messages =
    List.map
      (fun e ->
        if text "id" e <> "email-4" then e
        else
          let html =
            "<html><head><style>body {color: \
             red}</style></head><body><h1>Saturday travel</h1><p>Hello, I \
             arrive at <b>16:20</b> on Saturday.</p><p>Please collect the keys \
             before then. <a \
             href=\"https://trains.example.test/booking\">Booking \
             details</a></p><p>Samira</p><img \
             src=\"https://tracker.example.test/pixel\" \
             alt=\"\"></body></html>"
          in
          e
          |> set "textBody" (C.arr [])
          |> set "htmlBody"
               (C.arr
                  [
                    C.obj
                      [ ("partId", C.str "html"); ("type", C.str "text/html") ];
                  ])
          |> set "bodyValues"
               (C.obj
                  [
                    ( "html",
                      C.obj
                        [
                          ("value", C.str html);
                          ("isTruncated", `Bool false);
                          ("isEncodingProblem", `Bool false);
                        ] );
                  ])
          |> set "hasAttachment" (`Bool true)
          |> set "attachments"
               (C.arr
                  [
                    C.obj
                      [
                        ("blobId", C.str "demo-ticket");
                        ("name", C.str "train-ticket.pdf");
                        ("type", C.str "application/pdf");
                        ("size", int 24576);
                        ("disposition", C.str "attachment");
                      ];
                  ]))
      messages
  in
  let older id thread sender subject body date =
    let base = List.hd Termanil_demo.messages in
    email
      {
        source = { base.source with id };
        thread_id = thread;
        sender;
        subject;
        addresses = [ "river@example.test" ];
        preview = body;
        received = date;
        seen = true;
        flagged = false;
        metadata = [];
      }
      body
  in
  C.obj
    [
      ("schema", C.str "termanil-demo-jmap/v1");
      ("next", int 100);
      ("revision", int 1);
      ("submissions", C.arr []);
      ( "emails",
        C.arr
          (messages
          @ [
              older "garden-previous-1" "thread-email-1" "River"
                "Garden measurements"
                "The greenhouse bench is 180 cm. The old plan used lavender."
                "2026-09-01T09:00:00Z";
              older "garden-previous-2" "thread-email-1" "Ada"
                "Re: Garden measurements"
                "Thanks for measuring. I will put together the planting notes."
                "2026-09-04T09:00:00Z";
              older "workshop-previous" "thread-email-6" "River"
                "Community workshop checklist"
                "We need the room ready by 09:30. Tea and coffee arrive at \
                 10:00."
                "2026-09-02T09:00:00Z";
            ]) );
    ]

let session =
  {|{
"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":50000000,"maxConcurrentUpload":4,"maxSizeRequest":10000000,"maxConcurrentRequests":4,"maxCallsInRequest":16,"maxObjectsInGet":50,"maxObjectsInSet":50,"collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{},"urn:ietf:params:jmap:submission":{}},
"accounts":{"personal":{"name":"Demo","isPersonal":true,"isReadOnly":false,"accountCapabilities":{"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":255,"maxSizeAttachmentsPerEmail":50000000,"emailQuerySortOptions":["receivedAt"],"mayCreateTopLevelMailbox":true},"urn:ietf:params:jmap:submission":{"maxDelayedSend":0,"submissionExtensions":{}}}}},
"primaryAccounts":{"urn:ietf:params:jmap:mail":"personal","urn:ietf:params:jmap:submission":"personal"},"username":"you@example.test",
"apiUrl":"https://mail.example.test/api/","downloadUrl":"https://mail.example.test/download/{accountId}/{blobId}/{name}?type={type}","uploadUrl":"https://mail.example.test/upload/{accountId}/","eventSourceUrl":"https://mail.example.test/events/?types={types}&closeafter={closeafter}&ping={ping}","state":"s1"}|}

let transaction root f =
  C.mkdir root;
  let lock = Filename.concat root "server.lock" in
  if C.exists lock then C.regular lock;
  let fd =
    Unix.openfile lock [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0o600
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      (try Unix.lockf fd Unix.F_TLOCK 0
       with Unix.Unix_error _ -> C.fail "Demo server is busy");
      let p = Filename.concat root "server.json" in
      let before = if C.exists p then C.load_json p else initial () in
      if C.field "schema" before <> "termanil-demo-jmap/v1" then
        C.fail "Unsupported demo server state";
      let state = ref before in
      let result = f state in
      if !state <> before || not (C.exists p) then C.save_json p !state;
      result)

let patch obj entries =
  List.fold_left
    (fun obj (key, value) ->
      match String.split_on_char '/' key with
      | [ key ] -> set key value obj
      | [ parent; child ] ->
          let xs = List.remove_assoc child (fields parent obj) in
          set parent
            (C.obj (if value = `Null then xs else (child, value) :: xs))
            obj
      | _ -> C.fail "Unsupported fake JMAP patch")
    obj entries

let rec matches_filter filter email =
  let matches = matches_filter in
  match text "operator" filter with
  | "AND" -> List.for_all (fun f -> matches f email) (items "conditions" filter)
  | "OR" -> List.exists (fun f -> matches f email) (items "conditions" filter)
  | "NOT" ->
      not (List.exists (fun f -> matches f email) (items "conditions" filter))
  | _ ->
      List.for_all
        (fun (key, value) ->
          let contains key =
            M.matches ~query:(C.string value)
              (Option.fold ~none:"" ~some:C.json_string (C.find key email))
          in
          match key with
          | "inMailbox" ->
              List.mem_assoc (C.string value) (fields "mailboxIds" email)
          | "hasKeyword" ->
              List.mem_assoc (C.string value) (fields "keywords" email)
          | "notKeyword" ->
              not (List.mem_assoc (C.string value) (fields "keywords" email))
          | "hasAttachment" -> value = `Bool (items "attachments" email <> [])
          | "before" -> text "receivedAt" email < C.string value
          | "after" -> text "receivedAt" email >= C.string value
          | "from" | "to" | "cc" | "bcc" | "subject" -> contains key
          | "body" -> contains "bodyValues"
          | "text" -> M.matches ~query:(C.string value) (C.json_string email)
          | _ -> C.fail "Unsupported demo search filter: %s" key)
        (C.assoc filter)

let dispatch state name args =
  let error typ = ("error", C.obj [ ("type", C.str typ) ]) in
  if text "accountId" args <> account then error "accountNotFound"
  else
    let revision () = string_of_int (number (C.get "revision" !state)) in
    let update k v = state := set k v !state in
    let bump () =
      update "revision" (int (1 + number (C.get "revision" !state)))
    in
    let new_id prefix =
      let n = number (C.get "next" !state) in
      update "next" (int (n + 1));
      prefix ^ string_of_int n
    in
    let emails () = items "emails" !state in
    let got all =
      let ids =
        match C.find "ids" args with
        | Some (`A xs) -> List.map C.string xs
        | _ -> List.map (text "id") all
      in
      let found = List.filter (fun e -> List.mem (text "id" e) ids) all in
      let missing =
        List.filter
          (fun id -> not (List.exists (fun e -> text "id" e = id) found))
          ids
      in
      let found =
        match C.find "properties" args with
        | Some (`A props) ->
            List.map
              (fun e ->
                C.obj
                  (List.filter
                     (fun (k, _) ->
                       k = "id"
                       || List.mem (C.str k) props
                       || k = "bodyValues"
                          && C.find "fetchTextBodyValues" args
                             = Some (`Bool true))
                     (C.assoc e)))
              found
        | _ -> found
      in
      ( name,
        C.obj
          [
            ("accountId", C.str account);
            ("state", C.str (revision ()));
            ("list", C.arr found);
            ("notFound", strings missing);
          ] )
    in
    let query rows =
      let position =
        max 0 (number (Option.value (C.find "position" args) ~default:(int 0)))
      in
      let limit =
        max 0
          (min 50
             (number (Option.value (C.find "limit" args) ~default:(int 50))))
      in
      let page =
        List.to_seq rows |> Seq.drop position |> Seq.take limit |> List.of_seq
      in
      ( name,
        C.obj
          [
            ("accountId", C.str account);
            ("queryState", C.str (revision ()));
            ("canCalculateChanges", `Bool false);
            ("position", int position);
            ("ids", strings (List.map (text "id") page));
            ("total", int (List.length rows));
            ("limit", int limit);
          ] )
    in
    match name with
    | "Mailbox/get" ->
        got
          (List.map
             (fun (id, label, role) ->
               let rows =
                 List.filter
                   (fun e -> List.mem_assoc id (fields "mailboxIds" e))
                   (emails ())
               in
               C.obj
                 [
                   ("id", C.str id);
                   ("name", C.str label);
                   ("role", if role = "" then `Null else C.str role);
                   ( "unreadEmails",
                     int
                       (List.length
                          (List.filter
                             (fun e ->
                               not
                                 (List.mem_assoc "$seen" (fields "keywords" e)))
                             rows)) );
                 ])
             [
               ("inbox", "Inbox", "inbox");
               ("archive", "Archive", "archive");
               ("drafts", "Drafts", "drafts");
               ("sent", "Sent", "sent");
             ])
    | "Email/get" -> got (emails ())
    | "Email/query" ->
        let filter = Option.value (C.find "filter" args) ~default:(C.obj []) in
        let rows =
          emails ()
          |> List.filter (matches_filter filter)
          |> List.sort (fun a b ->
              compare
                (text "receivedAt" b, text "id" b)
                (text "receivedAt" a, text "id" a))
        in
        let rows =
          if C.find "collapseThreads" args <> Some (`Bool true) then rows
          else
            let seen = Hashtbl.create 16 in
            List.filter
              (fun e ->
                let id = text "threadId" e in
                if Hashtbl.mem seen id then false
                else (
                  Hashtbl.add seen id ();
                  true))
              rows
        in
        query rows
    | "Thread/get" ->
        let threads =
          List.sort_uniq String.compare (List.map (text "threadId") (emails ()))
        in
        got
          (List.map
             (fun id ->
               C.obj
                 [
                   ("id", C.str id);
                   ( "emailIds",
                     strings
                       (emails ()
                       |> List.filter (fun e -> text "threadId" e = id)
                       |> List.sort (fun a b ->
                           compare
                             (text "receivedAt" a, text "id" a)
                             (text "receivedAt" b, text "id" b))
                       |> List.map (text "id")) );
                 ])
             threads)
    | "Identity/get" ->
        got
          [
            C.obj
              [
                ("id", C.str "demo");
                ("name", C.str "You");
                ("email", C.str "you@example.test");
                ("mayDelete", `Bool false);
                ("textSignature", C.str "-- \nYou\nCommunity garden & workshop");
              ];
          ]
    | "EmailSubmission/get" -> got (items "submissions" !state)
    | "EmailSubmission/query" ->
        let filter = Option.value (C.find "filter" args) ~default:(C.obj []) in
        query
          (List.filter
             (fun s ->
               match C.find "emailIds" filter with
               | None -> true
               | Some ids -> List.mem (C.get "emailId" s) (C.list ids))
             (items "submissions" !state))
    | "Email/set" | "EmailSubmission/set" ->
        if
          Option.fold ~none:false
            ~some:(fun s -> C.string s <> revision ())
            (C.find "ifInState" args)
        then error "stateMismatch"
        else
          let old = revision () in
          let created =
            List.map
              (fun (cid, value) ->
                let id =
                  new_id
                    (if name = "Email/set" then "reply-" else "submission-")
                in
                let value = set "id" (C.str id) value in
                let value =
                  if name = "Email/set" then
                    let refs = items "inReplyTo" value in
                    let parent =
                      List.find_opt
                        (fun e ->
                          List.exists
                            (fun id -> List.mem id (items "messageId" e))
                            refs)
                        (emails ())
                    in
                    let thread =
                      Option.fold ~none:("thread-" ^ id) ~some:(text "threadId")
                        parent
                    in
                    value
                    |> set "threadId" (C.str thread)
                    |> set "receivedAt"
                         (C.str
                            (Ptime.to_rfc3339 ~tz_offset_s:0
                               (Ptime.of_float_s (Unix.gettimeofday ())
                               |> Option.get)))
                    |> set "messageId" (strings [ id ^ "@example.test" ])
                    |> set "preview" (C.str "Your reply")
                  else (
                    if
                      text "identityId" value <> "demo"
                      || not
                           (List.exists
                              (fun e -> text "id" e = text "emailId" value)
                              (emails ()))
                    then C.fail "Invalid demo submission";
                    value
                    |> set "undoStatus" (C.str "final")
                    |> set "sendAt" (C.str "2026-09-13T10:00:00Z"))
                in
                let key =
                  if name = "Email/set" then "emails" else "submissions"
                in
                update key (C.arr (value :: items key !state));
                (if name = "EmailSubmission/set" then
                   let p =
                     List.assoc_opt ("#" ^ cid)
                       (fields "onSuccessUpdateEmail" args)
                   in
                   Option.iter
                     (fun p ->
                       update "emails"
                         (C.arr
                            (List.map
                               (fun e ->
                                 if text "id" e = text "emailId" value then
                                   patch e (C.assoc p)
                                 else e)
                               (emails ()))))
                     p);
                (cid, C.obj [ ("id", C.str id) ]))
              (fields "create" args)
          in
          let updated, not_updated =
            List.partition
              (fun (id, _) ->
                List.exists (fun e -> text "id" e = id) (emails ()))
              (fields "update" args)
          in
          List.iter
            (fun (id, p) ->
              update "emails"
                (C.arr
                   (List.map
                      (fun e ->
                        if text "id" e = id then patch e (C.assoc p) else e)
                      (emails ()))))
            updated;
          let destroyed =
            List.filter
              (fun id -> List.exists (fun e -> C.get "id" e = id) (emails ()))
              (items "destroy" args)
          in
          update "emails"
            (C.arr
               (List.filter
                  (fun e -> not (List.mem (C.get "id" e) destroyed))
                  (emails ())));
          bump ();
          ( name,
            C.obj
              [
                ("accountId", C.str account);
                ("oldState", C.str old);
                ("newState", C.str (revision ()));
                ("created", C.obj created);
                ( "updated",
                  C.obj (List.map (fun (id, _) -> (id, `Null)) updated) );
                ( "notUpdated",
                  C.obj
                    (List.map
                       (fun (id, _) ->
                         (id, C.obj [ ("type", C.str "notFound") ]))
                       not_updated) );
                ("destroyed", C.arr destroyed);
              ] )
    | _ -> error "unknownMethod"

let connect ~sw ~root =
  let fetch =
    Fetch_mock.client (fun req ->
        let data =
          if Fetch.Middleware.Url.path_and_query req.url = "/jmap/session" then (
            transaction root (fun _ -> ());
            session)
          else
            match req.body with
            | Fetch.String raw ->
                transaction root (fun state ->
                    let responses =
                      List.map
                        (fun call ->
                          match C.list call with
                          | [ name; args; cid ] ->
                              let name, args =
                                dispatch state (C.string name) args
                              in
                              C.arr [ C.str name; args; cid ]
                          | _ -> C.fail "Invalid demo method call")
                        (C.items "methodCalls" (C.json raw))
                    in
                    C.json_string
                      (C.obj
                         [
                           ("sessionState", C.str "s1");
                           ("methodResponses", C.arr responses);
                         ]))
            | _ -> C.fail "Demo server expects a JSON request"
        in
        Fetch_mock.respond
          ~headers:
            (Http.Header.of_list [ ("Content-Type", "application/json") ])
          data req)
  in
  match
    Jmap_eio.Client.connect ~sw
      ~auth:(Jmap_eio.Auth.bearer "synthetic")
      (Jmap_eio.Transport.of_fetch fetch)
      service
  with
  | Ok c -> c
  | Error _ -> C.fail "Cannot connect to embedded demo JMAP server"
