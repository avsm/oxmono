open Jmap_eio

let check name value = if not value then failwith name
let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let respond body = Fetch_mock.respond ~headers body

let object_ fields =
  Jsont.Json.object' (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fields)

let field name = function
  | Jsont.Object (fs, _) ->
      List.assoc name (List.map (fun ((k, _), v) -> (k, v)) fs)
  | _ -> failwith "object expected"

let string = function
  | Jsont.String (s, _) -> s
  | _ -> failwith "string expected"

let session =
  {|{
 "capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":100000,"maxConcurrentRequests":1,"maxCallsInRequest":4,"maxObjectsInGet":10,"maxObjectsInSet":10,"collationAlgorithms":[]},"urn:ietf:params:jmap:calendars":{}},
 "accounts":{"a":{"name":"Calendar","isPersonal":true,"isReadOnly":false,"accountCapabilities":{"urn:ietf:params:jmap:calendars":{}}}},
 "primaryAccounts":{"urn:ietf:params:jmap:calendars":"a"},"username":"owner",
 "apiUrl":"https://example.test/api","downloadUrl":"https://example.test/download/{accountId}/{blobId}/{name}?type={type}",
 "uploadUrl":"https://example.test/upload/{accountId}","eventSourceUrl":"https://example.test/events?types={types}&close={closeafter}&ping={ping}","state":"session1"}|}

let raw =
  {|{ "id" : "e1", "uid":"uid1", "x-large":9007199254740993, "title":"Office", "x-extension":{"escaped":"\u2603"}, "recurrenceOverrides":{"2026-09-10T10:00:00":{"title":"Elsewhere"}} }|}

let test_read () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let missing_ical = ref false
  and wrong_state = ref false
  and malformed = ref false in
  let methods = ref [] in
  let server request =
    if request.Fetch.Middleware.meth = `GET then respond session request
    else
      let req =
        match request.body with
        | Fetch.String s -> Codec.decode_exn Jsont.json s
        | _ -> assert false
      in
      let name, args =
        match field "methodCalls" req with
        | Jsont.Array ([ Jsont.Array ([ name; args; _ ], _) ], _) ->
            (string name, args)
        | _ -> failwith "unexpected calls"
      in
      methods := name :: !methods;
      check "account restricted" (string (field "accountId" args) = "a");
      let ical =
        try
          let properties = field "properties" args in
          check "only id and iCalendar requested"
            (match properties with
            | Jsont.Array (values, _) ->
                List.map string values = [ "id"; "iCalendar" ]
            | _ -> false);
          true
        with Not_found -> false
      in
      if ical && !missing_ical then
        respond
          {|{"methodResponses":[["error",{"type":"invalidArguments"},"c0"]],"sessionState":"session1"}|}
          request
      else
        let body =
          if ical then
            {|{"id":"e1","iCalendar":{"@type":"ICalComponent","name":"vevent"}}|}
          else raw
        in
        let state = if ical && !wrong_state then "s2" else "s1" in
        let list = if !malformed then body ^ "," ^ body else body in
        respond
          (Printf.sprintf
             {|{"methodResponses":[["%s",{"accountId":"a","state":"%s","list":[%s],"notFound":[]},"c0"]],"sessionState":"session1"}|}
             name state list)
          request
  in
  let client =
    match
      Client.connect ~sw
        (Transport.of_fetch (Fetch_mock.client server))
        "https://example.test/session"
    with
    | Ok c -> c
    | Error e -> failwith (Client.error_to_string e)
  in
  let calendars = Calendars.create client in
  let fetched =
    Calendars.archive ~include_ical:true calendars Event ~ids:(Some [ "e1" ])
  in
  check "exact bytes, unknown fields, and numeric precision retained"
    ((List.hd fetched.items).raw = raw);
  check "iCal fetched explicitly"
    ((List.hd fetched.items).ical <> None && List.length fetched.receipts = 2);
  check "read-only methods"
    (!methods = [ "CalendarEvent/get"; "CalendarEvent/get" ]);
  methods := [];
  let ordinary = Calendars.archive calendars Event ~ids:(Some [ "e1" ]) in
  check "default archive needs one read"
    (!methods = [ "CalendarEvent/get" ]
    && List.length ordinary.receipts = 1
    && (List.hd ordinary.items).ical = None);
  missing_ical := true;
  let fetched =
    Calendars.archive ~include_ical:true calendars Event ~ids:(Some [ "e1" ])
  in
  check "unsupported fidelity is explicit"
    ((not fetched.ical_supported) && (List.hd fetched.items).ical = None);
  missing_ical := false;
  wrong_state := true;
  check "mixed representations rejected"
    (try
       ignore
         (Calendars.archive ~include_ical:true calendars Event
            ~ids:(Some [ "e1" ]));
       false
     with Invalid_argument _ -> true);
  wrong_state := false;
  malformed := true;
  check "duplicate IDs rejected"
    (try
       ignore
         (Calendars.archive ~include_ical:true calendars Event
            ~ids:(Some [ "e1" ]));
       false
     with Invalid_argument _ -> true)

let test_source () =
  let module P = Jmap.Proto in
  let text =
    {|{"methodResponses":[["CalendarEvent/get",{"accountId":"a","state":"s1","list":[|}
    ^ raw ^ {|],"notFound":[]},"c0"]],"sessionState":"session1"}|}
  in
  let response = Result.get_ok (Httpz_media.decode P.Response.media text) in
  check "ordinary response codec retains source"
    (P.Response.source response = Some text);
  let invocation = P.Response.get_response "c0" response in
  let events =
    Result.get_ok
      (Jsont.Json.decode'
         (P.Method.get_response_jsont P.Calendar_event.jsont)
         invocation.arguments)
  in
  let event = List.hd events.list in
  check "typed title" (event.title = Some "Office");
  check "typed object retains exact source"
    (P.Response.source_fragment response event.meta = Some raw);
  check "unknown fields remain available"
    (P.Unknown.find event.unknown "x-extension" <> None);
  check "unlocated values have no source"
    (P.Response.source_fragment response Jsont.Meta.none = None);
  let plain = Codec.decode_exn P.Response.jsont text in
  check "standalone typed JSON decoding has no source"
    (P.Response.source plain = None);
  let encoded = Codec.encode_exn P.Response.jsont response in
  let json = Codec.decode_exn Jsont.json encoded in
  check "source is never encoded"
    (try
       ignore (field "source" json);
       false
     with Not_found -> true)

let test_typed_objects () =
  let module P = Jmap.Proto in
  let module E = P.Calendar_event in
  let module T = P.Calendar_types in
  let fixture =
    {|{
    "@type":"Event","id":"e1","title":"Planning",
    "start":"2026-09-10T10:00:00","timeZone":"Europe/London","duration":"PT1H",
    "recurrenceRule":{"@type":"RecurrenceRule","frequency":"weekly",
      "byDay":[{"day":"th"}],"count":3,"x-rule":"retained"},
    "participants":{"p1":{"name":"Owner","calendarAddress":"mailto:owner@example.test",
      "expectReply":true,"roles":{"owner":true},"x-participant":[1,2]}},
    "alerts":{"a1":{"trigger":{"offset":"-PT15M"},"action":"display",
      "relatedTo":{"previous":{"relation":{"parent":true}}}}},
    "iCalendar":{"name":"vevent","properties":[["x-provider",{},"text","value"]],
      "convertedProperties":{"title":{"name":"summary","parameters":{
        "language":"en","x-multiple":["one","two"]}}}},
    "x-event":{"vendor":true}
  }|}
  in
  let event = Codec.decode_exn E.jsont fixture in
  let rule = Option.get event.recurrence_rule in
  check "recurrence rule is typed"
    (rule.frequency = `Weekly && rule.count = Some 3L
    && (List.hd (Option.get rule.by_day)).day = `Th);
  let participant = snd (List.hd (Option.get event.participants)) in
  check "participant is typed"
    (participant.expect_reply = Some true
    && participant.roles = Some [ ("owner", true) ]);
  let alert = snd (List.hd (Option.get event.alerts)) in
  check "default trigger type is decoded"
    (alert.trigger.type_ = "OffsetTrigger"
    && alert.trigger.offset = Some "-PT15M");
  let relation = snd (List.hd (Option.get alert.related_to)) in
  check "alert relation is typed" (relation.relation = Some [ ("parent", true) ]);
  let ical = Option.get event.icalendar in
  let property = snd (List.hd (Option.get ical.converted_properties)) in
  check "iCalendar parameter alternatives are typed"
    (property.parameters
    = Some [ ("language", T.One "en"); ("x-multiple", T.Many [ "one"; "two" ]) ]
    );
  let again = Codec.decode_exn E.jsont (Codec.encode_exn E.jsont event) in
  let has unknown key = P.Unknown.find unknown key <> None in
  check "nested extensions survive typed roundtrip"
    (has again.unknown "x-event"
    && has (Option.get again.recurrence_rule).unknown "x-rule"
    && has (snd (List.hd (Option.get again.participants))).unknown
         "x-participant");
  List.iter
    (fun text ->
      check "malformed known fields rejected"
        (Result.is_error (P.Json.decode E.jsont text)))
    [
      {|{"title":true}|};
      {|{"participants":{"p1":{"expectReply":"yes"}}}|};
      {|{"recurrenceRule":{"frequency":"fortnightly"}}|};
      {|{"recurrenceRule":{"frequency":"daily","count":1.5}}|};
      {|{"recurrenceRule":{"byDay":[{"day":"mo"}]}}|};
      {|{"alerts":{"a1":{"trigger":{"@type":"AbsoluteTrigger"}}}}|};
      {|{"alerts":{"a1":{"trigger":{}}}}|};
      {|{"iCalendar":["vevent",[],[]]}|};
      {|{"iCalendar":{"properties":[]}}|};
      {|{"iCalendar":{"name":"vevent","convertedProperties":{
        "title":{"name":"summary","parameters":{"language":42}}}}}|};
    ];
  let calendar =
    Codec.decode_exn P.Calendar.jsont
      {|{"id":"c1","name":"Work","includeInAvailability":"attending",
        "myRights":{"mayReadFreeBusy":true,"mayReadItems":true,"mayWriteAll":false,
          "mayWriteOwn":false,"mayUpdatePrivate":false,"mayRSVP":false,
          "mayShare":false,"mayDelete":false},"x-calendar":1}|}
  in
  check "calendar rights are typed"
    ((Option.get calendar.my_rights).may_read_items
    && calendar.include_in_availability = Some `Attending);
  List.iter
    (fun text ->
      check "invalid calendar properties rejected"
        (Result.is_error (P.Json.decode P.Calendar.jsont text)))
    [
      {|{"myRights":{"mayReadItems":true}}|};
      {|{"includeInAvailability":"sometimes"}|};
    ];
  let identity =
    Codec.decode_exn P.Participant_identity.jsont
      {|{"id":"p1","calendarAddress":"mailto:owner@example.test","isDefault":true}|}
  in
  check "participant identity is typed" (identity.is_default = Some true);
  let filter =
    E.filter ~in_calendar:(P.Id.of_string_exn "c1") ~text:"planning" ()
  in
  let decoded =
    Codec.decode_exn E.filter_jsont (Codec.encode_exn E.filter_jsont filter)
  in
  check "typed filter roundtrip" (decoded = filter)

let () =
  test_typed_objects ();
  test_source ();
  test_read ();
  print_endline "Calendar protocol tests passed."
