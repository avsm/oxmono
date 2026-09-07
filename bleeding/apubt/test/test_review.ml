module P = Apubt.Proto
let uri = Uriz.of_string_exn
let check label condition = if not condition then failwith label
let get = function Ok x -> x | Error e -> failwith e
let encode codec value = get (Jsont_bytesrw.encode_string codec value)
let json s req = Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "application/activity+json"]) s req
let actor name = P.Actor.make ~id:(uri ("https://example.com/" ^ name)) ~type_:Person
  ~inbox:(uri ("https://example.com/" ^ name ^ "/inbox"))
  ~outbox:(uri ("https://example.com/" ^ name ^ "/outbox"))
  ~followers:(uri ("https://example.com/" ^ name ^ "/followers")) ()
let decode_ref codec = function Some reference -> get (P.Reference.decode codec reference) | None -> failwith "missing object"
let expect_error f = match f () with _ -> failwith "expected failure" | exception Apubt.E _ -> ()

let () = Eio_mock.Backend.run_full @@ fun env ->
  let alice = actor "alice" and bob = actor "bob" in
  let persisted = ref [] and sent = ref [] and counter = ref 0 in
  let id_generator ~actor ~kind = incr counter; uri (Uriz.to_string actor ^ "/" ^ kind ^ "/" ^ string_of_int !counter) in
  let persist activity = persisted := activity :: !persisted in
  let backend req =
    match req.Fetch.Middleware.meth with
    | `POST ->
        check "persisted before delivery" (!persisted <> []);
        let body = match req.body with Fetch.String body -> body | _ -> failwith "body" in
        sent := (Fetch.Middleware.Url.to_string req.url, get (Jsont_bytesrw.decode_string P.Activity.jsont body)) :: !sent;
        Fetch_mock.respond ~status:202 "" req
    | _ -> match Fetch.Middleware.Url.path_and_query req.url with
      | "/alice/followers" -> json {|{"type":"OrderedCollection","first":{"type":"OrderedCollectionPage","orderedItems":["https://example.com/bob","https://example.com/bob","https://example.com/alice"]}}|} req
      | "/bob" -> json (encode P.Actor.jsont bob) req
      | "/alice" -> json (encode P.Actor.jsont alice) req
      | "/note" -> json {|{"id":"https://example.com/note","type":"Note","content":"old","attributedTo":"https://example.com/alice","to":["https://example.com/bob"],"attachment":[{"type":"Document","url":"https://example.com/photo.png"}],"tag":[{"type":"Hashtag","name":"#test","href":"https://example.com/tag"}],"x-example":{"nested":42}}|} req
      | _ -> failwith ("unexpected request " ^ Fetch.Middleware.Url.to_string req.url) in
  let client = Apubt.of_fetch ~clock:env#clock ~id_generator ~persist (Fetch_mock.client backend) in
  let note_activity = Apubt.Outbox.public_note client ~actor:alice ~sensitive:true ~summary:"CW" ~content:"hello" () in
  check "follower delivery deduplicated and excludes sender" (List.length !sent = 1 && fst (List.hd !sent) = "https://example.com/bob/inbox");
  let note = decode_ref P.Object.jsont (P.Activity.object_ note_activity) in
  check "content warning preserved" (P.Object.summary note = Some "CW" && P.Object.sensitive note = Some true);
  let follow = Apubt.Actor.follow client ~actor:alice ~target:bob in
  check "Follow gets ID" (Option.is_some (P.Activity.id follow));
  let undo = Apubt.Actor.unfollow client ~actor:alice ~follow in
  let original = decode_ref P.Activity.jsont (P.Activity.object_ undo) in
  check "Undo contains original Follow" (P.Activity.id original = P.Activity.id follow && P.Activity.type_ original = Follow);
  expect_error (fun () -> Apubt.Actor.unfollow client ~actor:bob ~follow);
  let accept = Apubt.Actor.accept_follow client ~actor:bob ~follow in
  check "Accept embeds Follow" (P.Activity.id (decode_ref P.Activity.jsont (P.Activity.object_ accept)) = P.Activity.id follow);
  let like = P.Activity.make ~id:(uri "https://example.com/alice/likes/original")
      ~type_:Like ~actor:(P.Actor_ref.actor alice) ~object_:(P.Reference.uri (uri "https://example.com/note"))
      ~to_:[P.Recipient.make (P.Actor.id bob)] () in
  let unlike = Apubt.Outbox.unlike client ~actor:alice ~like in
  check "Unlike keeps original ID" (P.Activity.id (decode_ref P.Activity.jsont (P.Activity.object_ unlike)) = P.Activity.id like);
  let updated = Apubt.Outbox.update_note client ~actor:alice ~object_:(uri "https://example.com/note") ~content:"new" () in
  let object_ = decode_ref P.Object.jsont (P.Activity.object_ updated) in
  check "update content" (P.Object.content object_ = Some "new");
  check "update attachments" (Option.map List.length (P.Object.attachment object_) = Some 1);
  let encoded = get (Jsont.Json.encode P.Object.jsont object_) in
  check "update extension preservation" (get (Jsont.Json.decode (Jsont.mem "x-example" (Jsont.mem "nested" Jsont.int)) encoded) = 42);
  expect_error (fun () -> Apubt.Outbox.delete client ~actor:bob ~object_:(uri "https://example.com/note"));
  let deletion = Apubt.Outbox.delete client ~actor:alice ~object_:(uri "https://example.com/note") in
  check "Tombstone deleted timestamp" (Option.is_some (P.Object.deleted (decode_ref P.Object.jsont (P.Activity.object_ deletion))));
  expect_error (fun () -> Apubt.Outbox.deliver client ~actor:alice
    (P.Activity.make ~actor:(P.Actor_ref.actor alice) ~type_:Like ()));
  let blind = P.Activity.make ~id:(uri "https://example.com/alice/blind")
      ~actor:(P.Actor_ref.actor alice) ~type_:Like
      ~bto:[P.Recipient.make (P.Actor.id bob)]
      ~audience:[P.Recipient.make (P.Actor.id bob)] () in
  let count = List.length !sent in
  Apubt.Outbox.deliver client ~actor:alice blind;
  check "blind and audience delivery deduplicated" (List.length !sent = count + 1);
  check "blind addressing removed from outgoing activity" (P.Activity.bto (snd (List.hd !sent)) = None);
  let missing_persistence = Apubt.of_fetch ~clock:env#clock ~id_generator
      (Fetch_mock.client (fun _ -> failwith "sent before persisting")) in
  expect_error (fun () -> Apubt.Actor.follow missing_persistence ~actor:alice ~target:bob);
  let requests = ref 0 in
  let pages = Apubt.of_fetch ~clock:env#clock (Fetch_mock.client (fun req ->
    incr requests;
    json (Printf.sprintf {|{"type":"CollectionPage","items":[1],"next":"https://example.com/page/%d"}|} !requests) req)) in
  let collection = P.Collection.make ~ordered:false ~first:(P.Reference.uri (uri "https://example.com/page/start")) () in
  expect_error (fun () -> Apubt.Collection.to_list ~max_pages:2 pages collection Jsont.int);
  check "page budget enforced" (!requests = 2);
  let collection = P.Collection.make ~ordered:false ~items:[1;2] () in
  expect_error (fun () -> Apubt.Collection.to_list ~max_items:1 pages collection Jsont.int);
  let client = Apubt.of_fetch ~clock:env#clock (Fetch_mock.client (fun req ->
    Fetch_mock.respond ~status:429 ~headers:(Http.Header.of_list ["Retry-After", "Thu, 01 Jan 1970 00:00:07 GMT"]) "" req)) in
  (match Apubt.Http.get client (uri "https://example.com") with
   | _ -> failwith "missing rate limit"
   | exception Apubt.E (Rate_limited (Some delay)) -> check "HTTP date retry-after" (delay = 7.)
   | exception _ -> failwith "wrong retry-after error");
  let client = Apubt.of_fetch ~clock:env#clock (Fetch_mock.client (fun req ->
    Fetch_mock.respond ~status:401 "" req)) in
  expect_error (fun () -> Apubt.Inbox.post_to_shared_inbox client ~host:"example.com" follow)

let () = Eio_mock.Backend.run_full @@ fun env ->
  let client = Apubt.of_fetch ~clock:env#clock (Fetch_mock.client (fun req ->
    json {|{"subject":"acct:alice@example.com","links":[{"rel":"http://ostatus.org/schema/1.0/subscribe","template":"https://example.com/follow?uri={uri}"}]}|} req)) in
  check "subscribe template retained" (Apubt.Webfinger.subscribe_template
    (Apubt.Webfinger.lookup client "alice@example.com") = Some "https://example.com/follow?uri={uri}");
  let requests = ref 0 in
  let client = Apubt.of_fetch ~clock:env#clock (Fetch_mock.client (fun req ->
    incr requests;
    json {|{"links":[{"rel":"https://evil.example/schema/2.1","href":"https://evil.example/info"}]}|} req)) in
  expect_error (fun () -> Apubt.Nodeinfo.fetch client ~host:"example.com");
  check "NodeInfo checks complete relation" (!requests = 1)
