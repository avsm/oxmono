open Crowthebot

let check name b = if not b then failwith name
let decode = Jmap_eio.Codec.decode_exn Jsont.json
let encode = Jmap_eio.Codec.encode_exn Jsont.json

let obj fs =
  Jsont.Json.object' (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fs)

let field key = function
  | Jsont.Object (fs, _) ->
      List.assoc key (List.map (fun ((k, _), v) -> (k, v)) fs)
  | _ -> failwith "object"

let str = function Jsont.String (s, _) -> s | _ -> failwith "string"
let array = function Jsont.Array (xs, _) -> xs | _ -> failwith "array"
let int j = Result.get_ok (Jsont.Json.decode Jsont.int j)
let ok = function Ok v -> v | Error e -> failwith e
let admin = "@admin:example.test"
let friend = "@friend:example.test"
let room = "!email:example.test"

let session =
  {|{"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":100000,"maxConcurrentRequests":1,"maxCallsInRequest":4,"maxObjectsInGet":10,"maxObjectsInSet":10,"collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{}},"accounts":{"a":{"name":"Mail","isPersonal":true,"isReadOnly":false,"accountCapabilities":{"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":256,"maxSizeAttachmentsPerEmail":50000000,"emailQuerySortOptions":["receivedAt"],"mayCreateTopLevelMailbox":true}}}},"primaryAccounts":{"urn:ietf:params:jmap:mail":"a"},"username":"owner","apiUrl":"https://example.test/api","downloadUrl":"https://example.test/download/{accountId}/{blobId}/{name}?type={type}","uploadUrl":"https://example.test/upload/{accountId}","eventSourceUrl":"https://example.test/events?types={types}&close={closeafter}&ping={ping}","state":"session1"}|}

let () =
  Eio_main.run @@ fun env ->
  let filename = Filename.temp_file "crow-email" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) @@ fun () ->
  let now = ref 1000. and calls = ref [] in
  let body =
    "EMAIL_CONTENT_SENTINEL"
    ^ String.concat "" (List.init 3000 (fun _ -> "🤖\"\\\n\t"))
  in
  let result =
    obj
      [
        ("accountId", Jsont.Json.string "a");
        ("state", Jsont.Json.string "s1");
        ( "list",
          Jsont.Json.list
            [
              obj
                [
                  ("id", Jsont.Json.string "e1");
                  ("x-provider", decode {|{"value":"retained"}|});
                  ("mailboxIds", decode {|{"inbox":true,"keep":true}|});
                  ( "bodyValues",
                    obj
                      [
                        ( "1",
                          obj
                            [
                              ("value", Jsont.Json.string body);
                              ("isTruncated", Jsont.Json.bool false);
                            ] );
                      ] );
                ];
            ] );
        ("notFound", Jsont.Json.list []);
      ]
  in
  let buffer = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buffer in
  Logs.set_reporter (Logs.format_reporter ~app:formatter ~dst:formatter ());
  Diagnostics.configure ~verbose:false;
  let fetch =
    Fetch_mock.client (fun r ->
        let headers =
          Http.Header.of_list [ ("content-type", "application/json") ]
        in
        let reply json = Fetch_mock.respond ~headers (encode json) r in
        let token = Option.get (Http.Header.get r.headers "authorization") in
        check "only private token reaches transport"
          (List.mem token [ "Bearer ro-secret"; "Bearer rw-secret" ]);
        if r.meth = `GET then reply (decode session)
        else
          let name, args =
            match r.body with
            | Fetch.String body -> (
                match array (field "methodCalls" (decode body)) with
                | [ c ] -> (
                    match array c with
                    | [ n; a; _ ] -> (str n, a)
                    | _ -> assert false)
                | _ -> assert false)
            | _ -> assert false
          in
          calls := (name, token) :: !calls;
          let payload =
            if name = "Email/set" then (
              check "writes use separate token" (token = "Bearer rw-secret");
              check "only child labels changed"
                (field "e1" (field "update" args)
                = decode {|{"mailboxIds/work":true,"mailboxIds/inbox":null}|});
              decode
                {|{"accountId":"a","oldState":"s1","newState":"s2","updated":{"e1":null},"notUpdated":null}|})
            else (
              check "only reads allowed" (name = "Email/get");
              result)
          in
          reply
            (obj
               [
                 ( "methodResponses",
                   Jsont.Json.list
                     [
                       Jsont.Json.list
                         [
                           Jsont.Json.string name;
                           payload;
                           Jsont.Json.string "c0";
                         ];
                     ] );
                 ("sessionState", Jsont.Json.string "session1");
               ]))
  in
  let open_store sw =
    let db =
      Sqlite3_eio.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / filename)
    in
    let store = Store.create ~now:(fun () -> !now) db ~admin in
    Store.add_room store room;
    let settings token =
      obj
        [
          ("url", Jsont.Json.string "https://example.test/session");
          ("token", Jsont.Json.string token);
          ("account", Jsont.Json.null ());
          ("max_bytes", Jsont.Json.int 1048576);
        ]
    in
    let reader =
      Email_source.initialize_reader ~sw ~fetch ~clock:(Eio.Stdenv.clock env)
        (settings "ro-secret")
    in
    let writer =
      Email_source.initialize_writer ~sw ~fetch ~clock:(Eio.Stdenv.clock env)
        (settings "rw-secret")
    in
    let create writers =
      Emails.create ~state:(Store.emails store)
        ~readers:[ ("fastmail", reader) ]
        ~writers ~default_reader:(Some "fastmail")
        ~default_writer:(if writers = [] then None else Some "fastmail")
    in
    let ro = create [] and rw = create [ ("fastmail", writer) ] in
    let invoke tools actor name args =
      Emails.invoke
        (Emails.for_request tools ~actor ~room ~event:"$origin")
        name args
    in
    (store, ro, rw, invoke)
  in
  let saved_id = ref 0 in
  Eio.Switch.run (fun sw ->
      let store, ro, rw, invoke = open_store sw in
      let read_tools = Emails.tools ro in
      let write_tools =
        List.filter (fun t -> not (List.mem t read_tools)) (Emails.tools rw)
      in
      check "RO advertises reads and paging" (List.length read_tools = 6);
      check "RW adds a separate label tool" (List.length write_tools = 1);
      check "unknown users denied before network"
        (Result.is_error (invoke ro friend "email_read" {|{"id":"e1"}|})
        && !calls = []);
      check "cannot smuggle RW via read tool"
        (Result.is_error
           (invoke ro admin "email_read" {|{"id":"e1","writable":true}|})
        && !calls = []);
      check "RO rejects forged writer call"
        (Result.is_error
           (invoke ro admin "email_update_labels" {|{"id":"e1","add":["work"]}|})
        && !calls = []);
      Store.observe store friend;
      Store.set_person store ~actor:admin ~user:friend ~role:Store.Friend
        ~allowed:true;
      let first =
        invoke ro friend "email_read" {|{"id":"e1"}|} |> ok |> decode
      in
      saved_id := int (field "result_id" first);
      check "read uses RO token" (!calls = [ ("Email/get", "Bearer ro-secret") ]);
      let all = Buffer.create 65536 in
      let rec pages p =
        check "tool page stays below engine cap"
          (String.length (encode p) <= 3800);
        let chunk = str (field "chunk" p) in
        check "UTF8 pages" (String.is_valid_utf_8 chunk && chunk <> "");
        Buffer.add_string all chunk;
        match field "next_offset" p with
        | Jsont.Null _ -> ()
        | j ->
            invoke ro admin "email_page"
              (Printf.sprintf {|{"result_id":%d,"offset":%d}|} !saved_id (int j))
            |> ok |> decode |> pages
      in
      pages first;
      let recovered = decode (Buffer.contents all) in
      let email = List.hd (array (field "list" recovered)) in
      let original = List.hd (array (field "list" result)) in
      check "complete response recovered across pages"
        (field "accountId" recovered = field "accountId" result
        && field "state" recovered = field "state" result
        && array (field "notFound" recovered) = []
        && List.length (array (field "list" recovered)) = 1
        && field "id" email = field "id" original
        && field "mailboxIds" email = field "mailboxIds" original
        && field "x-provider" email = field "x-provider" original);
      let value =
        Jmap_eio.Codec.decode_exn Jmap.Proto.Email_body.Value.jsont
          (encode (field "1" (field "bodyValues" email)))
      in
      check "body bytes and flags recovered losslessly"
        (value.value = body && (not value.is_truncated)
        && not value.is_encoding_problem);
      check "paging never re-fetches" (List.length !calls = 1);
      let invalid =
        invoke ro admin "email_page"
          (Printf.sprintf {|{"result_id":%d,"offset":999999}|} !saved_id)
      in
      check "bad offset rejected" (Result.is_error invalid);
      List.iter
        (fun offset ->
          check "email offsets require exact JSON integers"
            (Result.is_error
               (invoke ro admin "email_page"
                  (Printf.sprintf {|{"result_id":%d,"offset":%s}|} !saved_id
                     offset))))
        [ "0.5"; "-0.5"; {|"0"|}; "null"; "9007199254740992" ];
      check "email IDs cannot select a truncated record number"
        (Result.is_error
           (invoke ro admin "email_page"
              (Printf.sprintf {|{"result_id":%d.5,"offset":0}|} !saved_id)));
      ignore
        (invoke rw friend "email_update_labels"
           {|{"id":"e1","add":["work"],"remove":["inbox"]}|}
        |> ok);
      check "write precondition and mutation use RW token"
        (List.filter (fun (_, t) -> t = "Bearer rw-secret") !calls
        = [
            ("Email/set", "Bearer rw-secret"); ("Email/get", "Bearer rw-secret");
          ]);
      Store.set_person store ~actor:admin ~user:friend ~role:Store.Friend
        ~allowed:false;
      let before = !calls in
      check "revoked users cannot read shared cache"
        (Result.is_error
           (invoke ro friend "email_page"
              (Printf.sprintf {|{"result_id":%d,"offset":0}|} !saved_id)));
      check "revoked users cannot mutate"
        (Result.is_error
           (invoke rw friend "email_update_labels"
              {|{"id":"e1","add":["work"]}|})
        && !calls = before);
      let round = ref 0 in
      let engine =
        Engine.create
          ~config:(Config.default ~admin ~homeserver:"https://example.test")
          ~store ~self:"@crow:example.test" ~plugins:[]
          ~now:(fun () -> !now)
          ~complete:(fun _ tools ->
            incr round;
            if !round = 1 then (
              check "engine advertises only configured tool modes"
                (List.for_all (fun tool -> List.mem tool tools) read_tools
                && not
                     (List.exists (fun tool -> List.mem tool tools) write_tools)
                );
              ( None,
                [
                  Openrouter.Tool.
                    {
                      id = "email-call";
                      name = "email_read";
                      arguments = {|{"id":"e1"}|};
                    };
                ] ))
            else (Some "Done.", []))
        |> fun t -> Engine.with_emails t ro
      in
      Engine.handle engine ~direct:true ~send:ignore
        Engine.
          {
            room;
            sender = admin;
            id = "$engine-email";
            body = "Read the message";
          };
      let logs =
        Store.tool_uses store ~day:(Store.today store) ~after:0 ~through:max_int
          ~limit:100
      in
      check "engine audit redacts email contents"
        (List.exists
           (fun (u : Store.tool_use) ->
             u.tool = "email_read"
             && u.arguments = "[email content omitted]"
             && u.result = "[email content omitted]")
           logs);
      Format.pp_print_flush formatter ();
      let log = Buffer.contents buffer in
      let contains s =
        let n = String.length s in
        let rec loop i =
          i + n <= String.length log && (String.sub log i n = s || loop (i + 1))
        in
        loop 0
      in
      check "default CLI logs useful counts"
        (contains "Email result cached"
        && contains "Email labels updated"
        && contains "Tool started");
      check "CLI logs exclude message and bearer data"
        (not
           (contains "EMAIL_CONTENT_SENTINEL"
           || contains "ro-secret" || contains "rw-secret")));
  Eio.Switch.run (fun sw ->
      let store, ro, _, invoke = open_store sw in
      let before = !calls in
      check "cache survives restart"
        (Result.is_ok
           (invoke ro admin "email_page"
              (Printf.sprintf {|{"result_id":%d,"offset":0}|} !saved_id))
        && before = !calls);
      now := 90000.;
      check "expired snapshots rejected"
        (Result.is_error
           (invoke ro admin "email_page"
              (Printf.sprintf {|{"result_id":%d,"offset":0}|} !saved_id)));
      let save data =
        Email_cache.save (Store.emails store) ~actor:admin ~room ~event:"$cache"
          ~connection:"fastmail" ~writable:false ~operation:"test" data
      in
      let oldest = save "{}" in
      for _ = 1 to 50 do
        ignore (save "{}")
      done;
      check "cache entry count bounded"
        (Result.is_error
           (invoke ro admin "email_page"
              (Printf.sprintf {|{"result_id":%d,"offset":0}|} oldest)));
      let large = "\"" ^ String.make (17 * 1048576) 'x' ^ "\"" in
      let oldest = save large in
      for _ = 1 to 3 do
        ignore (save large)
      done;
      check "cache byte budget evicts oldest results"
        (Result.is_error
           (invoke ro admin "email_page"
              (Printf.sprintf {|{"result_id":%d,"offset":0}|} oldest)));
      let db =
        Sqlite3_eio.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / filename)
      in
      let inspected =
        Inspect.read db ~section:"email-results" ~after:0 ~limit:100
      in
      let rows = array (field "items" inspected) in
      let bytes =
        List.fold_left (fun total row -> total + int (field "bytes" row)) 0 rows
      in
      check "inspection counts bounded cache without loading contents"
        (bytes <= 67108864
        && List.length rows = 3
        && String.length (encode inspected) < 4096));
  print_endline
    "Crow email authorization, token separation, paging, persistence and audit \
     tests passed."
