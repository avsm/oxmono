(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core

module C = struct
  include Dooit.Common

  let int n = `Float (Float.of_int n)
end

module P = Jmap.Proto
module M = Termanil_model

let service = "https://mail.example.test/session"
let source : M.email_ref = { service; account = "acc"; id = "M1" }

let session =
  {|{
  "capabilities":{"urn:ietf:params:jmap:core":{
    "maxSizeUpload":50000000,"maxConcurrentUpload":4,"maxSizeRequest":10000000,
    "maxConcurrentRequests":4,"maxCallsInRequest":16,"maxObjectsInGet":500,
    "maxObjectsInSet":500,"collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{}},
  "accounts":{"acc":{"name":"Test","isPersonal":true,"isReadOnly":false,
    "accountCapabilities":{"urn:ietf:params:jmap:mail":{
      "maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":255,
      "maxSizeAttachmentsPerEmail":50000000,"emailQuerySortOptions":["receivedAt"],
      "mayCreateTopLevelMailbox":true}}}},
  "primaryAccounts":{"urn:ietf:params:jmap:mail":"acc"},"username":"test",
  "apiUrl":"https://mail.example.test/api/",
  "downloadUrl":"https://mail.example.test/download/{accountId}/{blobId}/{name}?type={type}",
  "uploadUrl":"https://mail.example.test/upload/{accountId}/",
  "eventSourceUrl":"https://mail.example.test/events/?types={types}&closeafter={closeafter}&ping={ping}",
  "state":"s1"}|}

let with_client respond f =
  Eio_main.run (fun _env ->
      Eio.Switch.run (fun sw ->
          let backend =
            Fetch_mock.client (fun req ->
                let data =
                  if
                    String.equal
                      (Fetch.Middleware.Url.path_and_query req.url)
                      "/session"
                  then session
                  else
                    let raw =
                      match req.body with
                      | Fetch.String s -> s
                      | _ -> assert false
                    in
                    let calls = C.items "methodCalls" (C.json raw) in
                    let replies =
                      List.map calls ~f:(fun call ->
                          match C.list call with
                          | [ name; args; cid ] ->
                              C.arr [ name; respond (C.string name) args; cid ]
                          | _ -> assert false)
                    in
                    C.json_string
                      (C.obj
                         [
                           ("sessionState", C.str "s1");
                           ("methodResponses", C.arr replies);
                         ])
                in
                Fetch_mock.respond
                  ~headers:
                    (Http.Header.of_list
                       [ ("Content-Type", "application/json") ])
                  data req)
          in
          let client =
            match
              Jmap_eio.Client.connect ~sw
                ~auth:(Jmap_eio.Auth.bearer "synthetic")
                (Jmap_eio.Transport.of_fetch backend)
                service
            with
            | Ok c -> c
            | Error _ -> failwith "mock connection failed"
          in
          f (Termanil_mail.create ~client ~service ~account:"acc")))

let email id =
  C.obj
    [
      ("id", C.str id);
      ("subject", C.str id);
      ("keywords", C.obj [ ("$seen", `Bool true) ]);
    ]

let got ?(account = "acc") ?(not_found = []) emails =
  C.obj
    [
      ("accountId", C.str account);
      ("state", C.str "e1");
      ("list", C.arr emails);
      ("notFound", C.arr (List.map not_found ~f:C.str));
    ]

let refused f =
  try
    f ();
    print_endline "unexpected success"
  with Failure s -> print_endline s

let%expect_test
    "mail queries preserve order, honor server page size and account scope" =
  let methods = ref [] in
  with_client
    (fun name args ->
      methods := name :: !methods;
      assert (String.equal (C.field "accountId" args) "acc");
      match name with
      | "Email/query" ->
          printf "filter: %s\n" (C.json_string (C.get "filter" args));
          C.obj
            [
              ("accountId", C.str "acc");
              ("queryState", C.str "q1");
              ("canCalculateChanges", `Bool false);
              ("position", C.int 0);
              ("ids", C.arr [ C.str "M2"; C.str "M1" ]);
              ("total", C.int 3);
              ("limit", C.int 2);
            ]
      | "Email/get" -> got [ email "M1"; email "M2" ]
      | _ -> assert false)
    (fun c ->
      let page =
        Termanil_mail.messages c ~mailbox:(Some "inbox") ~query:"garden"
          ~position:0
      in
      print_s
        [%sexp
          (List.map page.messages ~f:(fun m -> m.M.source.id) : string list)];
      print_s [%sexp (page.next : int option)];
      refused (fun () ->
          ignore (Termanil_mail.read c { source with account = "other" })));
  print_s [%sexp (List.rev !methods : string list)];
  [%expect
    {|
    filter: {"operator":"AND","conditions":[{"inMailbox":"inbox"},{"text":"garden"}]}
    (M2 M1)
    (2)
    Message belongs to a different JMAP service or account
    (Email/query Email/get)
    |}]

let%expect_test "message GET identities and silent partial pages are rejected" =
  with_client
    (fun _ _ -> got ~account:"wrong" [ email "M1" ])
    (fun c -> refused (fun () -> ignore (Termanil_mail.read c source)));
  with_client
    (fun _ _ -> got [ email "M2" ])
    (fun c -> refused (fun () -> ignore (Termanil_mail.read c source)));
  with_client
    (fun name _ ->
      match name with
      | "Email/query" ->
          C.obj
            [
              ("accountId", C.str "acc");
              ("queryState", C.str "q1");
              ("canCalculateChanges", `Bool false);
              ("position", C.int 0);
              ("ids", C.arr [ C.str "M1" ]);
              ("total", C.int 1);
            ]
      | _ -> got [])
    (fun c ->
      refused (fun () ->
          ignore (Termanil_mail.messages c ~mailbox:None ~query:"" ~position:0)));
  [%expect
    {|
    JMAP returned a different account
    Message vanished or JMAP returned another message
    JMAP returned an incomplete message page
    |}]

let%expect_test
    "keyword changes are conditional patches and object failures are errors" =
  let reject = ref false in
  with_client
    (fun name args ->
      match name with
      | "Email/get" -> got [ email "M1" ]
      | "Email/set" ->
          printf "state: %s\n" (C.field "ifInState" args);
          printf "patch: %s\n" (C.json_string (C.get "update" args));
          C.obj
            ([
               ("accountId", C.str "acc");
               ("oldState", C.str "e1");
               ("newState", C.str "e2");
             ]
            @
            if !reject then
              [
                ( "notUpdated",
                  C.obj [ ("M1", C.obj [ ("type", C.str "forbidden") ]) ] );
              ]
            else [ ("updated", C.obj [ ("M1", `Null) ]) ])
      | _ -> assert false)
    (fun c ->
      Termanil_mail.set_keyword c source `Flagged true;
      reject := true;
      refused (fun () -> Termanil_mail.set_keyword c source `Seen false));
  [%expect
    {|
    state: e1
    patch: {"M1":{"keywords/$flagged":true}}
    state: e1
    patch: {"M1":{"keywords/$seen":null}}
    Server refused the message update; refresh before retrying
    |}]

let%expect_test "message reading is GET only and keeps truncation visible" =
  let calls = ref [] in
  with_client
    (fun name args ->
      calls := name :: !calls;
      assert (Poly.equal (C.get "fetchTextBodyValues" args) (`Bool true));
      assert (Poly.equal (C.get "maxBodyValueBytes" args) (C.int 262144));
      got
        [
          C.obj
            (C.assoc (email "M1")
            @ [
                ( "textBody",
                  C.arr
                    [
                      C.obj
                        [
                          ("partId", C.str "part");
                          ("type", C.str "text/plain");
                          ("size", C.int 100);
                        ];
                    ] );
                ( "bodyValues",
                  C.obj
                    [
                      ( "part",
                        C.obj
                          [
                            ("value", C.str "Hello");
                            ("isTruncated", `Bool true);
                            ("isEncodingProblem", `Bool false);
                          ] );
                    ] );
              ]);
        ])
    (fun c ->
      let _, body = Termanil_mail.read c source in
      print_endline body);
  print_s [%sexp (List.rev !calls : string list)];
  [%expect {|
    Hello
    [Part truncated at 256 KiB]
    (Email/get)
    |}]

let%expect_test "reply targeting prefers Reply-To over From" =
  with_client
    (fun name _ ->
      assert (String.equal name "Email/get");
      got
        [
          C.obj
            (C.assoc (email "M1")
            @ [
                ( "from",
                  C.arr [ C.obj [ ("email", C.str "sender@example.test") ] ] );
                ( "replyTo",
                  C.arr [ C.obj [ ("email", C.str "replies@example.test") ] ] );
              ]);
        ])
    (fun mail ->
      let _, subject, recipients = Termanil_mail.reply_target mail source in
      printf "%s\n" subject;
      print_s [%sexp (recipients : string list)]);
  [%expect {|
    Re: M1
    (replies@example.test)
    |}]

let%expect_test
    "HTML extraction preserves useful text and omits executable content" =
  print_endline
    (Termanil_mail.Html_text.render
       "<style>invisible</style><p>Caf&eacute; &amp; \
        tea</p><script>steal()</script><p><a \
        href='https://example.test/a?x=1&amp;y=2'>Read \
        more</a></p><ul><li>First<li>Second</ul><pre>a\n\
       \  b</pre><div hidden>hidden text</div><img alt='diagram'><a \
        href='javascript:bad()'>plain label</a>");
  [%expect
    {|
    Café & tea
    Read more <https://example.test/a?x=1&y=2>
    * First
    * Second
    a
      b
    diagramplain label
    |}]

let%expect_test
    "search filters support quoting and reject malformed reserved values" =
  List.iter
    [
      "subject:\"garden plan\" from:ada is:unread has:attachment";
      "before:2026-02-30";
      "subject:\"unclosed";
      "is:typo";
    ] ~f:(fun q ->
      try
        let f = Termanil_mail.Search.filter ~mailbox:None q in
        match Jsont_bytesrw.encode_string P.Email.filter_jsont f with
        | Ok json -> print_endline json
        | Error _ -> assert false
      with Failure why -> print_endline why);
  [%expect
    {|
    {"operator":"AND","conditions":[{},{"subject":"garden plan"},{"from":"ada"},{"notKeyword":"$seen"},{"hasAttachment":true}]}
    Invalid search date
    Unclosed search quote
    Unknown search filter: is:typo
    |}]

let%expect_test "text identity signatures take precedence over HTML fallback" =
  let html =
    "<div><b>Anil</b><br>Garden &amp; workshop</div><script>hidden()</script>"
  in
  List.iter [ None; Some ""; Some "-- \nPlain signature" ]
    ~f:(fun text_signature ->
      printf "%S\n"
        (Termanil_mail.identity_signature
           (P.Identity.v ?text_signature ~html_signature:html ())));
  [%expect
    {|
    "Anil\nGarden & workshop"
    "Anil\nGarden & workshop"
    "-- \nPlain signature"
    |}]

let%expect_test "archive checks conditional per-object errors" =
  with_client
    (fun name args ->
      match name with
      | "Email/get" -> got [ email "M1" ]
      | "Mailbox/get" ->
          got
            [
              C.obj [ ("id", C.str "IN"); ("role", C.str "inbox") ];
              C.obj [ ("id", C.str "AR"); ("role", C.str "archive") ];
            ]
      | "Email/set" ->
          printf "state: %s\npatch: %s\n" (C.field "ifInState" args)
            (C.json_string (C.get "update" args));
          C.obj
            [
              ("accountId", C.str "acc");
              ("oldState", C.str "s1");
              ("newState", C.str "s1");
              ( "notUpdated",
                C.obj [ ("M1", C.obj [ ("type", C.str "forbidden") ]) ] );
            ]
      | _ -> assert false)
    (fun c -> refused (fun () -> Termanil_mail.archive c source));
  [%expect
    {|
    state: e1
    patch: {"M1":{"mailboxIds/IN":null,"mailboxIds/AR":true}}
    Server refused archive. Refresh before retrying.
    |}]
