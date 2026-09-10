open Jmap_eio
module Mail = Crowthebot.Email_client
module P = Jmap.Proto

let id s = Result.get_ok (P.Id.of_string_received s)

module M = Fetch.Middleware

let check name b = if not b then failwith name
let decode = Codec.decode_exn Jsont.json
let encode = Codec.encode_exn Jsont.json

let obj fs =
  Jsont.Json.object' (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fs)

let field key = function
  | Jsont.Object (fs, _) ->
      List.assoc key (List.map (fun ((k, _), v) -> (k, v)) fs)
  | _ -> failwith "object expected"

let str = function Jsont.String (s, _) -> s | _ -> failwith "string expected"

let array = function
  | Jsont.Array (xs, _) -> xs
  | _ -> failwith "array expected"

let int j = Result.get_ok (Jsont.Json.decode Jsont.int j)
let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let respond s = Fetch_mock.respond ~headers s

let session =
  {|{"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":100000,"maxConcurrentRequests":1,"maxCallsInRequest":4,"maxObjectsInGet":2,"maxObjectsInSet":10,"collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{}},"accounts":{"a":{"name":"Mail","isPersonal":true,"isReadOnly":false,"accountCapabilities":{"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":256,"maxSizeAttachmentsPerEmail":50000000,"emailQuerySortOptions":["receivedAt","subject"],"mayCreateTopLevelMailbox":true}}}},"primaryAccounts":{"urn:ietf:params:jmap:mail":"a"},"username":"owner","apiUrl":"https://example.test/api","downloadUrl":"https://example.test/download/{accountId}/{blobId}/{name}?type={type}","uploadUrl":"https://example.test/upload/{accountId}","eventSourceUrl":"https://example.test/events?types={types}&close={closeafter}&ping={ping}","state":"session1"}|}

let envelope name args =
  {|{"using":["urn:ietf:params:jmap:core","urn:ietf:params:jmap:mail"],"methodCalls":[[|}
  ^ encode (Jsont.Json.string name)
  ^ "," ^ args ^ ",\"mail\"]]}"

let denied f =
  try
    f ();
    false
  with Eio.Io (Fetch.E (Denied _ | Invalid_request _), _) -> true

let rejected f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let policy sw =
  let count = ref 0 in
  let backend =
    Fetch_mock.client (fun r ->
        incr count;
        Fetch_mock.respond "{}" r)
  in
  let ro, bind =
    Mail.Http.create ~url:"https://example.test/session" ~writable:false backend
  in
  let rw, bind_rw =
    Mail.Http.create ~url:"https://example.test/session" ~writable:true backend
  in
  bind ~account:"a" ~api_url:"https://example.test/api";
  bind_rw ~account:"a" ~api_url:"https://example.test/api";
  check "cannot rebind account"
    (rejected (fun () -> bind ~account:"b" ~api_url:"https://example.test/api"));
  check "cannot bind cross origin"
    (rejected (fun () -> bind_rw ~account:"a" ~api_url:"https://evil.test/api"));
  let send client ?(headers = Fetch.Header.[])
      ?(url = "https://example.test/api") ?(meth = `POST) body =
    let r = Fetch.fetch ~sw ~headers ~body client meth url in
    Fetch.close r
  in
  let reject client body =
    let before = !count in
    check "blocked before network"
      (denied (fun () -> send client (Fetch.String body)) && !count = before)
  in
  let patch =
    {|{"accountId":"a","ifInState":"s1","update":{"e1":{"mailboxIds/work":true,"mailboxIds/inbox":null}}}|}
  in
  send rw (Fetch.String (envelope "Email/set" patch));
  reject ro (envelope "Email/set" patch);
  List.iter
    (fun (name, args) ->
      List.iter (fun c -> reject c (envelope name args)) [ ro; rw ])
    [
      ("Email/set", {|{"accountId":"a","destroy":["e1"]}|});
      ("Email/set", {|{"accountId":"a","create":{"x":{"subject":"new"}}}|});
      ( "Email/set",
        {|{"accountId":"a","ifInState":"s1","update":{"e1":{"subject":"new"}}}|}
      );
      ( "Email/set",
        {|{"accountId":"a","ifInState":"s1","update":{"e1":{"keywords/$seen":true}}}|}
      );
      ( "Email/set",
        {|{"accountId":"a","ifInState":"s1","update":{"e1":{"mailboxIds":{"work":true}}}}|}
      );
      ( "Email/set",
        {|{"accountId":"a","update":{"e1":{"mailboxIds/work":true}}}|} );
      ( "Email/set",
        {|{"accountId":"a","ifInState":"s1","update":{"e1":{"mailboxIds/":true}}}|}
      );
      ( "Email/set",
        {|{"accountId":"a","ifInState":"s1","update":{"e1":{"mailboxIds/a/b":true}}}|}
      );
      ("EmailSubmission/set", {|{"accountId":"a","create":{}}|});
      ("Mailbox/set", {|{"accountId":"a","destroy":["inbox"]}|});
      ("Email/import", {|{"accountId":"a","emails":{}}|});
      ("Email/copy", {|{"accountId":"a","fromAccountId":"b","create":{}}|});
      ("Email/get", {|{"accountId":"b","ids":["e1"]}|});
      ("Email/query", {|{"accountId":"a","limit":51}|});
      ("Email/query", {|{"accountId":"a","limit":1.5}|});
      ( "Email/query",
        {|{"accountId":"a","limit":1,"onSuccessDestroyOriginal":true}|} );
    ];
  List.iter (reject ro)
    [
      "{";
      String.make 65537 ' ';
      {|{"using":[],"methodCalls":[]}|};
      {|{"using":["urn:ietf:params:jmap:core","urn:ietf:params:jmap:mail"],"methodCalls":[["Email/get",{"accountId":"a","accountId":"b","ids":["e1"]},"mail"]]}|};
    ];
  let safe =
    Fetch.String (envelope "Email/get" {|{"accountId":"a","ids":["e1"]}|})
  in
  List.iter
    (fun c ->
      send c safe;
      check "method override blocked"
        (denied (fun () ->
             send c
               ~headers:Fetch.Header.[ raw "x-http-method-override" "DELETE" ]
               safe));
      List.iter
        (fun url ->
          check "endpoint pinned" (denied (fun () -> send c ~url safe)))
        [
          "https://evil.test/api";
          "https://example.test/upload/a";
          "https://example.test/api?method=delete";
        ];
      List.iter
        (fun meth ->
          check "HTTP writes blocked" (denied (fun () -> send c ~meth safe)))
        [ `DELETE; `PUT; `PATCH ];
      check "arbitrary GET blocked"
        (denied (fun () -> send c ~meth:`GET Fetch.Empty));
      send c ~meth:`GET ~url:"https://example.test/session" Fetch.Empty)
    [ ro; rw ]

let operations env sw ~relative_api =
  let calls = ref []
  and failure = ref ""
  and mailbox_ids = ref [ "inbox"; "keep" ] in
  let initial_session =
    if not relative_api then decode session
    else
      match decode session with
      | Jsont.Object (fields, meta) ->
          Jsont.Object
            ( List.map
                (fun (((name, _) as key), value) ->
                  ( key,
                    if name = "apiUrl" then Jsont.Json.string "/api" else value
                  ))
                fields,
              meta )
      | _ -> assert false
  in
  let session_value = ref initial_session in
  let query_total = ref (Some 3) and query_ids = ref [ "e1"; "e2" ] in
  let server r =
    if r.M.meth = `GET then respond (encode !session_value) r
    else
      let name, args =
        match r.body with
        | Fetch.String s -> (
            match array (field "methodCalls" (decode s)) with
            | [ call ] -> (
                match array call with
                | [ n; a; _ ] -> (str n, a)
                | _ -> assert false)
            | _ -> assert false)
        | _ -> assert false
      in
      let token = Option.get (Http.Header.get r.headers "authorization") in
      calls := (name, token, args) :: !calls;
      check "account selected" (str (field "accountId" args) = "a");
      let response name body =
        respond
          ("{\"methodResponses\":[["
          ^ encode (Jsont.Json.string name)
          ^ "," ^ body ^ ",\"c0\"]],\"sessionState\":\"session1\"}")
          r
      in
      match name with
      | "Email/query" ->
          check "compound query preserved"
            (field "filter" args
            = decode
                {|{"operator":"AND","conditions":[{"inMailbox":"inbox"},{"text":"meeting"}]}|}
            );
          check "sort preserved" (List.length (array (field "sort" args)) = 1);
          let p = try int (field "position" args) with Not_found -> 0 in
          response name
            (Printf.sprintf
               {|{"accountId":"a","queryState":"q1","position":%d,"ids":%s,%s"limit":2,"canCalculateChanges":true}|}
               p
               (encode
                  (Jsont.Json.list (List.map Jsont.Json.string !query_ids)))
               (Option.fold ~none:""
                  ~some:(fun n -> Printf.sprintf "\"total\":%d," n)
                  !query_total))
      | "Email/get" ->
          let ids = List.map str (array (field "ids" args)) in
          let props = List.map str (array (field "properties" args)) in
          if List.mem "bodyValues" props then (
            check "body values requested"
              (field "fetchTextBodyValues" args = Jsont.Json.bool true);
            check "HTML values requested"
              (field "fetchHTMLBodyValues" args = Jsont.Json.bool true));
          let bodies =
            List.map
              (fun id ->
                obj
                  [
                    ("id", Jsont.Json.string id);
                    ( "mailboxIds",
                      obj
                        (List.map
                           (fun id -> (id, Jsont.Json.bool true))
                           !mailbox_ids) );
                    ("threadId", Jsont.Json.string "t1");
                    ( "bodyValues",
                      decode {|{"1":{"value":"hello","isTruncated":false}}|} );
                  ])
              ids
          in
          response name
            (encode
               (obj
                  [
                    ("accountId", Jsont.Json.string "a");
                    ("state", Jsont.Json.string "s1");
                    ("list", Jsont.Json.list bodies);
                    ("notFound", Jsont.Json.list []);
                  ]))
      | "Thread/get" ->
          response name
            {|{"accountId":"a","state":"t1","list":[{"id":"t1","emailIds":["e1","e2","e3"]}],"notFound":[]}|}
      | "Mailbox/get" ->
          response name
            {|{"accountId":"a","state":"m1","list":[{"id":"inbox","name":"Inbox","role":"inbox","myRights":{"mayReadItems":true,"mayAddItems":true,"mayRemoveItems":true,"maySetSeen":true,"maySetKeywords":true,"mayCreateChild":false,"mayRename":false,"mayDelete":false,"maySubmit":false}}],"notFound":[]}|}
      | "Email/set" ->
          check "only RW token mutates" (token = "Bearer rw-fixture");
          check "state precondition" (str (field "ifInState" args) = "s1");
          check "patch preserves other labels"
            (field "e1" (field "update" args)
            = decode {|{"mailboxIds/work":true,"mailboxIds/inbox":null}|});
          if !failure = "conflict" then
            response "error" {|{"type":"stateMismatch","description":"SECRET"}|}
          else if !failure = "forbidden" then
            response name
              {|{"accountId":"a","oldState":"s1","newState":"s1","notUpdated":{"e1":{"type":"forbidden","description":"SECRET"}}}|}
          else
            response name
              {|{"accountId":"a","oldState":"s1","newState":"s2","updated":{"e1":null},"notUpdated":null}|}
      | _ -> failwith "unexpected method"
  in
  let fetch = Fetch_mock.client server and clock = Eio.Stdenv.clock env in
  let ro =
    Mail.connect_read_only ~sw ~fetch ~clock ~token:"ro-fixture"
      "https://example.test/session"
  in
  let rw =
    Mail.connect_read_write ~sw ~fetch ~clock ~token:"rw-fixture"
      "https://example.test/session"
  in
  check "primary account pinned" ((Mail.identity ro).account = "a");
  ignore (Mail.read ro ~id:(id "e1"));
  ignore (Mail.mailboxes ro);
  let query () =
    Mail.query ro
      ~filter:
        (Codec.decode_exn P.Email.filter_jsont
           {|{"operator":"AND","conditions":[{"inMailbox":"inbox"},{"text":"meeting"}]}|})
      ~sort:
        [
          Codec.decode_exn P.Filter.comparator_jsont
            {|{"property":"receivedAt","isAscending":false}|};
        ]
      ~position:0 ~limit:50 ~collapse_threads:false ()
  in
  check "server-limited query continues" ((query ()).next_position = Some 2L);
  query_total := None;
  check "server limit works without total" ((query ()).next_position = Some 2L);
  query_ids := [ "e1" ];
  check "last page without total" ((query ()).next_position = None);
  query_ids := [];
  query_total := Some 3;
  check "empty page before total is rejected"
    (try
       ignore (query ());
       false
     with Invalid_argument _ -> true);
  let thread = Mail.thread ro ~id:(id "t1") ~position:0 ~limit:10 in
  check "server get limit respected"
    (thread.next_position = Some 2 && List.length thread.email_ids = 2);
  let thread = Mail.thread ro ~id:(id "t1") ~position:2 ~limit:10 in
  check "thread last page"
    (thread.next_position = None && List.length thread.email_ids = 1);
  check "reads all use RO token"
    (List.for_all (fun (_, t, _) -> t = "Bearer ro-fixture") !calls);
  ignore
    (Mail.update_labels rw ~id:(id "e1")
       ~add:[ id "work" ]
       ~remove:[ id "inbox" ]);
  List.iter
    (fun error ->
      failure := error;
      let before = List.length !calls in
      let msg =
        try
          ignore
            (Mail.update_labels rw ~id:(id "e1")
               ~add:[ id "work" ]
               ~remove:[ id "inbox" ]);
          failwith "expected failure"
        with exn -> Mail.error exn
      in
      check "write failures do not retry or leak server text"
        (List.length !calls = before + 2
        && (not (String.contains msg 'S'))
        && msg <> "JMAP mail operation failed."))
    [ "conflict"; "forbidden" ];
  mailbox_ids := [ "inbox" ];
  let before = List.length !calls in
  check "last label cannot be removed"
    (rejected (fun () ->
         Mail.update_labels rw ~id:(id "e1") ~add:[] ~remove:[ id "inbox" ]));
  check "empty label rejection before write" (List.length !calls = before + 1);
  check "overlapping labels rejected"
    (rejected (fun () ->
         Mail.update_labels rw ~id:(id "e1")
           ~add:[ id "work" ]
           ~remove:[ id "work" ]));
  session_value :=
    obj
      (List.map
         (fun ((k, _), v) ->
           ( k,
             if k = "apiUrl" then Jsont.Json.string "https://evil.test/api"
             else v ))
         (match decode session with
         | Jsont.Object (fs, _) -> fs
         | _ -> assert false));
  check "cross-origin discovery rejected"
    (rejected (fun () ->
         Mail.connect_read_only ~sw ~fetch ~clock ~token:"ro-fixture"
           "https://example.test/session"))

let () =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          policy sw;
          operations env sw ~relative_api:false;
          operations env sw ~relative_api:true));
  print_endline
    "JMAP mail reads, paging, label updates and capability confinement passed."
