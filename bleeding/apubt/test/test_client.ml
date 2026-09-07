module P = Apubt.Proto
module S = Fetch_signature

let check name condition = if not condition then failwith name
let get = function Ok x -> x | Error e -> failwith e
let uri = Uriz.of_string_exn
let contains s part =
  let rec loop i = i + String.length part <= String.length s &&
    (String.sub s i (String.length part) = part || loop (i + 1)) in
  loop 0

let response ?(status = 200) ?(headers = []) ?(close = fun () -> ()) body req =
  Fetch.Middleware.Pi.response ~status
    ~headers:(Http.Header.of_list headers) ~version:`HTTP_1_1
    ~body:(Eio.Flow.string_source body) ~close ~url:req.Fetch.Middleware.url ()

let json ?(status = 200) ?(close = fun () -> ()) body req =
  response ~status ~close ~headers:["Content-Type", "application/activity+json"] body req

let apub_error name predicate f =
  match f () with
  | _ -> failwith (name ^ ": expected error")
  | exception Apubt.E e -> check name (predicate e)

let test_http () = Eio_mock.Backend.run_full @@ fun env ->
  let closed = ref 0 in
  let fetch = Fetch_mock.client (fun req ->
    check "ActivityPub Accept" (Http.Header.get req.headers "accept" =
      Some "application/activity+json, application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"");
    check "configured User-Agent" (Http.Header.get req.headers "user-agent" = Some "audit-test");
    json ~close:(fun () -> incr closed) "42" req) in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock ~user_agent:"audit-test" fetch in
  check "typed JSON GET" (Apubt.Http.get_typed client Jsont.int (uri "https://example.com/value") = 42);
  check "success released" (!closed = 1);
  List.iter (fun (status, headers, body, predicate) ->
    let closed = ref false in
    let fetch = Fetch_mock.client (response ~status ~headers
      ~close:(fun () -> closed := true) body) in
    let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock ~max_response_bytes:32 fetch in
    apub_error "HTTP classification" predicate (fun () ->
      Apubt.Http.get client (uri "https://example.com/value"));
    check "failure released" !closed)
    [404, [], "", (function Apubt.Error.Not_found -> true | _ -> false);
     403, [], "", (function Unauthorized -> true | _ -> false);
     429, ["Retry-After", "7"], "", (function Rate_limited (Some 7.) -> true | _ -> false);
     500, [], "failed", (function Http_error (500, "failed") -> true | _ -> false);
     500, [], String.make 33 'x', (function Http_error (500, _) -> true | _ -> false);
     200, ["Content-Type", "text/html"], "42", (function Json_error _ -> true | _ -> false);
     200, ["Content-Type", "application/json"], "[", (function Json_error _ -> true | _ -> false);
     200, ["Content-Type", "application/json"], String.make 33 ' ', (function Json_error _ -> true | _ -> false)];
  let fetch = Fetch_mock.client (json (String.make 129 '[' ^ "0" ^ String.make 129 ']')) in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock fetch in
  apub_error "bounded JSON depth" (function Json_error _ -> true | _ -> false)
    (fun () -> Apubt.Http.get client (uri "https://example.com/deep"));
  let fetch = Fetch_mock.client (fun _ -> raise (Fetch.err (Fetch.Tls_failure "test"))) in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock fetch in
  apub_error "transport errors use public API" (function Network_error _ -> true | _ -> false)
    (fun () -> Apubt.Http.get client (uri "https://example.com"));
  let cancelled = Eio.Cancel.Cancelled (Failure "test cancellation") in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock
    (Fetch_mock.client (fun _ -> raise cancelled)) in
  let activity = P.Activity.make ~type_:Follow
      ~actor:(P.Actor_ref.uri (uri "https://example.com/alice")) () in
  (match Apubt.Inbox.post_to_shared_inbox client ~host:"example.com" activity with
   | _ -> failwith "cancellation was swallowed"
   | exception Eio.Cancel.Cancelled _ -> ())

let test_signing () =
  Mirage_crypto_rng_unix.use_default ();
  let priv = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
  let key = S.Key.rsa ~priv in
  Eio_mock.Backend.run_full @@ fun env ->
  let signing = Apubt.Signing.create ~key_id:"https://sender.example/key" ~key () in
  let seen = ref 0 in
  let fetch = Fetch_mock.client (fun req ->
    incr seen;
    let body = match req.body with Fetch.String s -> s | _ -> failwith "expected replayable JSON" in
    check "typed JSON encoding" (body = "42");
    check "ActivityPub Content-Type" (Http.Header.get req.headers "content-type" = Some "application/activity+json");
    let input = Option.get (Http.Header.get req.headers "signature-input") in
    check "RSA-SHA256 selected" (contains input "alg=\"rsa-v1_5-sha256\"");
    check "complete URI covered" (contains input "\"@target-uri\"");
    let digest = Option.get (Http.Header.get req.headers "content-digest") in
    check "body digest" (Result.is_ok (S.Content_digest.verify ~header:digest ~body));
    let target = Fetch.Middleware.Url.to_string req.url in
    check "sign canonical Fetch URL" (target = "https://example.com/inbox?part=1");
    let verify target =
      let context = S.Context.request ~method_:req.meth ~uri:(Uriz.of_string_exn target) ~headers:req.headers in
      S.verify ~clock:env#clock ~key ~context ~headers:req.headers
        ~required_components:S.Component.[method_; target_uri; content_digest] () in
    check "signature verifies" (Result.is_ok (verify target));
    check "query tampering rejected" (Result.is_error (verify "https://example.com/inbox?part=2"));
    response ~status:307 ~headers:["Location", "https://other.example/inbox"] "" req) in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock ~signing fetch in
  apub_error "signed writes do not follow redirects"
    (function Http_error (307, _) -> true | _ -> false)
    (fun () -> Apubt.Http.post_typed client Jsont.int
      (uri "https://EXAMPLE.com:443/a/../inbox?part=1#fragment") 42);
  check "no second delivery" (!seen = 1);
  let context = S.Context.request ~method_:`POST ~uri:(Uriz.of_string_exn "https://example.com/")
      ~headers:(Http.Header.init ()) in
  let sign config = S.sign ~clock:env#clock ~config ~context ~headers:(Http.Header.init ()) in
  let defaults = match sign (S.config ~key ()) with Ok h -> h | Error _ -> failwith "default signing failed" in
  check "Fetch RSA default remains PSS"
    (contains (Option.get (Http.Header.get defaults "signature-input")) "rsa-pss-sha512");
  check "incompatible override rejected"
    (Result.is_error (sign (S.config ~key ~algorithm:`Ed25519 ())))

let test_discovery () = Eio_mock.Backend.run_full @@ fun env ->
  let actor = {|{"id":"https://example.com/alice","type":"Person","inbox":"https://example.com/inbox","outbox":"https://example.com/outbox"}|} in
  let fetch = Fetch_mock.client (fun req ->
    if Fetch.Middleware.Url.path_and_query req.url = "/alice" then json actor req
    else begin
      check "WebFinger overrides Accept" (Http.Header.get req.headers "accept" = Some "application/jrd+json");
      response ~headers:["Content-Type", "application/jrd+json"]
        {|{"subject":"acct:alice@example.com","links":[{"rel":"self","type":"text/html","href":"https://example.com/profile"},{"rel":"self","type":"application/activity+json","href":"https://example.com/alice"}]}|} req
    end) in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock fetch in
  check "search every self link" (P.Actor.id (Apubt.Actor.lookup client "alice@example.com") = uri "https://example.com/alice");
  let calls = ref 0 in
  let fetch = Fetch_mock.client (fun req ->
    incr calls;
    check "NodeInfo overrides Accept" (Http.Header.get req.headers "accept" = Some "application/json");
    if !calls = 1 then
      response ~headers:["Content-Type", "application/json"]
        {|{"links":[{"rel":"http://nodeinfo.diaspora.software/ns/schema/2.1","href":"https://example.com/nodeinfo"}]}|} req
    else response ~headers:["Content-Type", "application/json"]
      {|{"version":"2.1","software":{"name":"test","version":"1"},"protocols":["activitypub"],"usage":{},"openRegistrations":false}|} req) in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock fetch in
  check "NodeInfo decode" (Apubt.Nodeinfo.software_name (Apubt.Nodeinfo.fetch client ~host:"example.com") = "test")

let test_collections_and_delivery () = Eio_mock.Backend.run_full @@ fun env ->
  let requests = ref 0 in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock (Fetch_mock.client (fun req ->
    incr requests;
    json {|{"type":"OrderedCollectionPage","orderedItems":[1,2],"next":"https://example.com/page"}|} req)) in
  let collection = P.Collection.make ~ordered:true ~first:(P.Reference.uri (uri "https://example.com/page")) () in
  apub_error "pagination cycles fail" (function Json_error _ -> true | _ -> false)
    (fun () -> Apubt.Collection.to_list client collection Jsont.int);
  check "cyclic page fetched once" (!requests = 1);
  let actor = P.Actor.make ~id:(uri "https://example.com/alice") ~type_:Person
      ~inbox:(uri "https://example.com/inbox") ~outbox:(uri "https://example.com/outbox") () in
  let remote = P.Actor.make ~id:(uri "https://example.com/bob") ~type_:Person
      ~inbox:(uri "https://example.com/inbox") ~outbox:(uri "https://example.com/outbox") () in
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock (Fetch_mock.client (fun req ->
    match req.meth with
    | `GET -> json (get (Jsont_bytesrw.encode_string P.Actor.jsont remote)) req
    | _ -> response ~status:503 "unavailable" req)) in
  apub_error "delivery failure propagates" (function Http_error (503, _) -> true | _ -> false)
    (fun () -> Apubt.Outbox.direct_note client ~actor ~to_:[P.Actor.make ~id:(uri "https://example.com/bob") ~type_:Person
      ~inbox:(uri "https://example.com/inbox") ~outbox:(uri "https://example.com/outbox") ()] ~content:"test" ());
  let client = Apubt.of_fetch ~persist:(fun _ -> ()) ~clock:env#clock (Fetch_mock.client (fun _ -> failwith "unexpected recipient request")) in
  let activity = Apubt.Outbox.public_note client ~actor ~content:"test" () in
  check "no empty follower URI" (P.Activity.cc activity = Some [])

let () =
  test_http ();
  test_signing ();
  test_discovery ();
  test_collections_and_delivery ()
