open Crowthebot
module C = Caldav_eio.Client
module D = Httpz_dav
module M = Fetch.Middleware

let check name value = if not value then failwith name
let origin = "https://example.test/"
let resource = origin ^ "home/calendar/one.ics"
let calls = ref 0
let secret = "synthetic-calendar-app-password"

let denied f =
  try
    ignore (f ());
    false
  with Eio.Io (Fetch.E (Denied _ | Invalid_request _), _) -> true

let result_denied = function
  | Error (C.Transport (Fetch.Denied _, _)) -> true
  | _ -> false

let multi props =
  "<d:multistatus xmlns:d='DAV:' \
   xmlns:c='urn:ietf:params:xml:ns:caldav'><d:response><d:href>/principal/</d:href><d:propstat><d:prop>"
  ^ props
  ^ "</d:prop><d:status>HTTP/1.1 200 \
     OK</d:status></d:propstat></d:response></d:multistatus>"

let () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let backend =
    Fetch_mock.client (fun req ->
        incr calls;
        let url = M.Url.to_string req.url in
        check "only configured origin reaches backend"
          (String.starts_with ~prefix:origin url);
        check "private authentication survives restrictions"
          (match Http.Header.get req.headers "authorization" with
          | Some v -> String.starts_with ~prefix:"Basic " v
          | None -> false);
        let meth = Http.Method.to_string req.meth in
        check "backend never sees a write"
          (List.mem meth [ "GET"; "PROPFIND"; "REPORT" ]);
        if meth = "GET" then
          Fetch_mock.respond ~status:307
            ~headers:
              (Http.Header.of_list [ ("Location", "https://other.test/steal") ])
            "" req
        else
          Fetch_mock.respond ~status:207
            ~headers:
              (Http.Header.of_list [ ("Content-Type", "application/xml") ])
            (multi
               "<d:current-user-principal><d:href>/principal/</d:href></d:current-user-principal><c:calendar-home-set><d:href>/home/</d:href></c:calendar-home-set>")
            req)
  in
  let restricted = Caldav_http.read_only ~url:(origin ^ "principal/") backend in
  let credentials =
    [ Fetch.Credential.basic ~user:"owner@example.test" ~password:secret ]
  in
  let client =
    match C.connect ~sw ~credentials restricted (origin ^ "principal/") with
    | Ok c -> c
    | Error _ -> failwith "discovery rejected"
  in
  let reject_result name f =
    let before = !calls in
    check name (result_denied (f ()));
    check (name ^ " denied before transport") (!calls = before)
  in
  reject_result "delete event" (fun () -> C.delete client resource);
  reject_result "delete calendar" (fun () ->
      C.delete_calendar client (origin ^ "home/calendar/"));
  reject_result "create calendar" (fun () ->
      C.create_calendar client (origin ^ "home/new/"));
  reject_result "update event" (fun () ->
      C.put Caldav.Data.raw client resource
        "BEGIN:VCALENDAR\r\nEND:VCALENDAR\r\n");
  reject_result "create event" (fun () ->
      C.add Caldav.Data.raw client ~name:"new.ics"
        (origin ^ "home/calendar/")
        "BEGIN:VCALENDAR\r\nEND:VCALENDAR\r\n");
  reject_result "change calendar properties" (fun () ->
      C.set_props client
        (origin ^ "home/calendar/")
        [ D.Set [ D.element D.Prop.displayname [ D.Text "changed" ] ] ]);
  reject_result "arbitrary REPORT" (fun () ->
      C.report client resource (D.element ("urn:extension", "delete") []));
  let auth = Fetch.with_credentials ~scope:[ origin ] credentials restricted in
  let widened =
    Fetch.restrict ~under:[ origin ]
      ~methods:[ `GET; `PUT; `POST; `DELETE ]
      auth
  in
  let send client ?(headers = Fetch.Header.[]) ?(body = Fetch.Empty) meth url =
    let r =
      Fetch.fetch ~sw ~headers ~body client (Http.Method.of_string meth) url
    in
    Fetch.close r
  in
  let reject name f =
    let before = !calls in
    check name (denied f);
    check (name ^ " denied before transport") (!calls = before)
  in
  List.iter
    (fun method_ ->
      reject ("raw " ^ method_) (fun () -> send auth method_ resource))
    [
      "PUT";
      "DELETE";
      "PATCH";
      "POST";
      "PROPPATCH";
      "MKCOL";
      "MKCALENDAR";
      "COPY";
      "MOVE";
      "LOCK";
      "UNLOCK";
      "ACL";
      "BIND";
      "UNBIND";
      "REBIND";
      "UPDATE";
      "MERGE";
      "CHECKOUT";
      "CHECKIN";
      "TRACE";
      "HEAD";
      "OPTIONS";
      "delete";
      "Put";
      "X-DELETE";
    ];
  reject "outer restriction cannot restore writes" (fun () ->
      send widened "PUT" resource);
  let request =
    M.
      {
        meth = `DELETE;
        url = Result.get_ok (M.Url.of_string resource);
        headers = Http.Header.init ();
        body = Empty;
        sensitive = [];
        sensitive_query = [];
      }
  in
  reject "raw handler cannot bypass policy" (fun () ->
      M.handler auth ~sw request);
  List.iter
    (fun header ->
      reject ("method override " ^ header) (fun () ->
          send auth ~headers:Fetch.Header.[ raw header "DELETE" ] "GET" resource))
    [
      "X-HTTP-Method-Override";
      "X-HTTP-Method";
      "X-Method-Override";
      "Destination";
      "Lock-Token";
      "If";
      "Schedule-Reply";
      "Cookie";
    ];
  reject "query override" (fun () ->
      send auth "GET" (resource ^ "?_method=DELETE"));
  reject "GET body" (fun () ->
      send auth ~body:(Fetch.String "delete") "GET" resource);
  reject "streamed request" (fun () ->
      send auth
        ~body:
          (Fetch.Stream
             { length = None; flow = Eio.Flow.string_source "delete" })
        "REPORT" resource);
  let xml_headers =
    Fetch.Header.[ raw "Depth" "0"; raw "Content-Type" "application/xml" ]
  in
  List.iter
    (fun (meth, body) ->
      reject
        ("unexpected " ^ meth ^ " XML")
        (fun () ->
          send auth ~headers:xml_headers ~body:(Fetch.String body) meth resource))
    [
      ("PROPFIND", "<d:propertyupdate xmlns:d='DAV:'/>");
      ("PROPFIND", "<d:propfind xmlns:d='DAV:'><d:set/></d:propfind>");
      ( "REPORT",
        "<d:sync-collection xmlns:d='DAV:'><d:delete/></d:sync-collection>" );
      ("REPORT", "<d:sync-collection xmlns:d='urn:wrong-namespace'/>");
      ("REPORT", "not XML");
      ( "REPORT",
        D.Sync.request ~token:(String.make 65536 'x') ~limit:20
          [ D.Prop.getetag ] );
    ];
  reject "foreign origin" (fun () ->
      send auth "GET" "https://other.test/private.ics");
  reject "plaintext transport" (fun () ->
      send auth "GET" "http://example.test/private.ics");
  let before = !calls in
  check "DAV GET does not follow redirects"
    (match C.get Caldav.Data.raw client resource with
    | Error (C.Http (307, _)) -> true
    | _ -> false);
  check "exactly one redirect response" (!calls = before + 1);
  let before = !calls in
  send auth ~headers:xml_headers
    ~body:
      (Fetch.String
         (D.Sync.request ~token:"urn:sync:1" ~limit:20 [ D.Prop.getetag ]))
    "REPORT"
    (origin ^ "home/calendar/");
  check "expected sync report allowed" (!calls = before + 1);
  print_endline "CalDAV writes and policy bypasses denied before transport"
