[@@@alert "-do_not_spawn_domains"]

type todo =
  { id : int
  ; title : string
  ; done_ : bool
  }

type nested_case = C of int | D of int
type outer_case = A of nested_case | B of int

module Media = Httpz.Media

let todo_jsont =
  Jsont.Object.map ~kind:"Todo" (fun id title done_ -> { id; title; done_ })
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun t -> t.id)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.mem "done" Jsont.bool ~enc:(fun t -> t.done_)
       ~dec_absent:(fun () -> false)
  |> Jsont.Object.finish
;;

let todo = Proffer.Json.v todo_jsont
let fetch_todo = Fetch.Json.v todo_jsont
let todos = Proffer.Json.v (Jsont.list todo_jsont)
let todo_lines = Proffer.Json.lines todo_jsont
let todo_text =
  Media.text
  |> Media.map ~encode:(fun t -> t.title) ~decode:(fun _ -> Error "no")

let either = [ todo; todo_text ]

let mutable_default_jsont =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem
       "values"
       (Jsont.array Jsont.int)
       ~dec_absent:(fun () -> [| 0 |])
  |> Jsont.Object.finish
;;

let int_case name =
  Jsont.Object.map ~kind:name Fun.id
  |> Jsont.Object.mem (name ^ "-data") Jsont.int
  |> Jsont.Object.finish
;;

let nested_case_jsont =
  let c_jsont = int_case "c" in
  let d_jsont = int_case "d" in
  let case_c = Jsont.Object.Case.map "c" c_jsont ~dec:(fun c -> C c) in
  let case_d = Jsont.Object.Case.map "d" d_jsont ~dec:(fun d -> D d) in
  let cases = Jsont.Object.Case.[ make case_c; make case_d ] in
  Jsont.Object.map ~kind:"nested case" Fun.id
  |> Jsont.Object.case_mem
       "subtype"
       Jsont.string
       cases
       ~tag_to_string:Fun.id
       ~dec_absent:"c"
  |> Jsont.Object.finish
;;

let outer_case_jsont =
  let case_a =
    Jsont.Object.Case.map "a" nested_case_jsont ~dec:(fun nested -> A nested)
  in
  let case_b = Jsont.Object.Case.map "b" (int_case "b") ~dec:(fun b -> B b) in
  let cases = Jsont.Object.Case.[ make case_a; make case_b ] in
  Jsont.Object.map ~kind:"outer case" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string cases ~tag_to_string:Fun.id
  |> Jsont.Object.finish
;;

let a = { id = 1; title = "write docs"; done_ = false }
let b = { id = 2; title = "ship"; done_ = true }
let check_str = Alcotest.(check string)
let check_int = Alcotest.(check int)

let test_codec () =
  let empty_object =
    Jsont.Object.map 42 |> Jsont.Object.finish |> Proffer.Json.v
  in
  (match Media.decode empty_object "{}" with
   | Ok 42 -> ()
   | Ok n -> Alcotest.failf "empty object decoded to %d" n
   | Error e -> Alcotest.fail (Media.error_to_string e));
  let mutable_default = Proffer.Json.v mutable_default_jsont in
  (match Media.decode mutable_default "{}", Media.decode mutable_default "{}" with
   | Ok first, Ok second ->
     first.(0) <- 1;
     check_int "fresh mutable default" 0 second.(0)
   | Error e, _ | _, Error e -> Alcotest.fail (Media.error_to_string e));
  let int64_codec = Proffer.Json.v Jsont.int64 in
  check_str
    "ambiguous int64 encoded as string"
    {|"9007199254740992"|}
    (Media.encode int64_codec 9007199254740992L);
  (match Media.decode int64_codec "9007199254740992" with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "ambiguous numeric int64 was accepted");
  (match Media.decode int64_codec {|"9007199254740992"|} with
   | Ok 9007199254740992L -> ()
   | Ok n -> Alcotest.failf "decoded int64 as %Ld" n
   | Error e -> Alcotest.fail (Media.error_to_string e));
  let enum_codec =
    Proffer.Json.v
      (Jsont.enum [ "same", 0; "same", 1; "first", 2; "last", 2 ])
  in
  (match Media.decode enum_codec {|"same"|} with
   | Ok 1 -> ()
   | Ok n -> Alcotest.failf "duplicate enum decoded as %d" n
   | Error e -> Alcotest.fail (Media.error_to_string e));
  check_str "duplicate enum encoding" {|"last"|} (Media.encode enum_codec 2);
  let nested_default_codec = Proffer.Json.v nested_case_jsont in
  (match Media.decode nested_default_codec {|{"c-data":4}|} with
   | Ok (C 4) -> ()
   | Ok _ -> Alcotest.fail "absent case tag selected the wrong case"
   | Error e -> Alcotest.fail (Media.error_to_string e));
  let nested_codec = Proffer.Json.v outer_case_jsont in
  (match
     Media.decode
       nested_codec
       {|{"subtype":"c","c-data":3,"type":"a"}|}
   with
   | Ok (A (C 3)) -> ()
   | Ok _ -> Alcotest.fail "nested out-of-order case decoded incorrectly"
   | Error e -> Alcotest.fail (Media.error_to_string e));
  check_str "type" "application/json" (Media.content_type todo);
  check_str "encode" {|{"id":1,"title":"write docs","done":false}|} (Media.encode todo a);
  Alcotest.(check bool)
    "vendor json"
    true
    (Media.accepts todo (Some "application/vnd.api+json; charset=utf-8"));
  (match Media.decode todo {|{"id": 2, "title": "ship"}|} with
   | Ok t -> Alcotest.(check bool) "absent member" false t.done_
   | Error e -> Alcotest.fail (Media.error_to_string e));
  (match Media.decode todo {|{"id": "x"}|} with
   | Error (Media.Malformed { message; loc; detail }) ->
     Alcotest.(check bool) "no escapes" false (String.contains message '\027');
     Alcotest.(check bool)
       "mentions id"
       true
       (let re = Str.regexp_string "id" in
        try
          ignore (Str.search_forward re message 0);
          true
        with
        | Not_found -> false);
     Alcotest.(check bool) "has location" true (Option.is_some loc);
     (match detail with
      | Proffer.Json.Jsont _ -> ()
      | _ -> Alcotest.fail "expected structured Jsont detail")
   | _ -> Alcotest.fail "expected malformed");
  check_str
    "lines"
    "{\"id\":1,\"title\":\"write docs\",\"done\":false}\n\
     {\"id\":2,\"title\":\"ship\",\"done\":true}\n"
    (Media.encode_items todo_lines (List.to_seq [ a; b ]));
  check_str "lines type" "application/jsonl" (Media.seq_content_type todo_lines)
;;

let test_portable_domains () =
  let start = Atomic.make false in
  let shared_json = Proffer.Json.json in
  let shared_todo = todo in
  let worker : (unit -> bool) @ portable =
    fun () ->
      while not (Atomic.get start) do
        Domain.cpu_relax ()
      done;
      let rec round_trip n =
        if n = 0
        then true
        else
          match
            Media.decode
              shared_json
              {|{"nested":[{"ok":true},null,[1,2,3]]}|}
          with
          | Error _ -> false
          | Ok json ->
            let encoded = Media.encode shared_json json in
            (match
               Media.decode shared_json encoded,
               Media.decode shared_todo {|{"id":7,"title":"shared"}|}
             with
             | Ok _, Ok ({ id = 7; title = "shared"; done_ = false } as value) ->
               String.equal
                 (Media.encode shared_todo value)
                 {|{"id":7,"title":"shared","done":false}|}
               && round_trip (n - 1)
             | _ -> false)
      in
      round_trip 100
  in
  let d0 = Domain.Safe.spawn worker in
  let d1 = Domain.Safe.spawn worker in
  let d2 = Domain.Safe.spawn worker in
  let d3 = Domain.Safe.spawn worker in
  Atomic.set start true;
  let ok0 = Domain.join d0 in
  let ok1 = Domain.join d1 in
  let ok2 = Domain.join d2 in
  let ok3 = Domain.join d3 in
  Alcotest.(check bool) "shared recursive codec" true
    (ok0 && ok1 && ok2 && ok3)
;;

(* Proffer *)
open Proffer
open Proffer.Route

type env =
  { store : (int, todo) Hashtbl.t }

(* This annotation, plus the portable route constructors below, checks that a
   complete Jsont descriptor and its streaming media codec can be captured by
   portable closures. *)
let todo_of_env : (env -> todo Media.t) @ portable = fun _ -> todo

let site =
  Site.of_routes
    [ get (s "todos") (fun env _req respond ->
        Resp.encode respond todos (Hashtbl.to_seq_values env.store |> List.of_seq))
    ; get
        (s "todos" / int)
        (fun id env _req respond ->
          match Hashtbl.find_opt env.store id with
          | Some t -> Resp.encode respond todo t
          | None -> Resp.not_found respond ())
    ; post
        (s "todos")
        (with_body
           todo_of_env
           (fun t env _req respond ->
             Hashtbl.replace env.store t.id t;
             Resp.encode respond ~status:Created todo t))
    ; get (s "export") (fun env _req respond ->
        Resp.encode_seq respond todo_lines (Hashtbl.to_seq_values env.store))
    ; get (s "either") (fun _env req respond -> Negotiate.encode respond req either a)
    ]
;;

let env () =
  let store = Hashtbl.create 4 in
  Hashtbl.replace store 1 a;
  { store }
;;

let code r = Status.code (Proffer_mock.status r)

let test_server () =
  let env = env () in
  let r = Proffer_mock.request site env Get "/todos/1" in
  check_int "get status" 200 (code r);
  check_str
    "get type"
    "application/json"
    (Option.get (Proffer_mock.header r Content_type));
  check_str "get body" (Media.encode todo a) (Proffer_mock.body r);
  let r = Proffer_mock.request site env Head "/todos/1" in
  check_str "head empty" "" (Proffer_mock.body r);
  Alcotest.(check (option int64)) "head length" (Some 42L) (Proffer_mock.content_length r);
  let r =
    Proffer_mock.request
      site
      env
      Post
      "/todos"
      ~headers:[ "Content-Type", "application/json" ]
      ~body:(Media.encode todo b)
  in
  check_int "post status" 201 (code r);
  check_int "stored" 2 (Hashtbl.length env.store);
  let r =
    Proffer_mock.request
      site
      env
      Post
      "/todos"
      ~headers:[ "Content-Type", "text/plain" ]
      ~body:"hello"
  in
  check_int "415" 415 (code r);
  let r =
    Proffer_mock.request
      site
      env
      Post
      "/todos"
      ~headers:[ "Content-Type", "application/json" ]
      ~body:{|{"id": "x"}|}
  in
  check_int "400" 400 (code r);
  Alcotest.(check bool)
    "400 message"
    true
    (String.starts_with ~prefix:"Bad Request: " (Proffer_mock.body r));
  let r = Proffer_mock.request site env Get "/export" in
  check_str
    "export type"
    "application/jsonl"
    (Option.get (Proffer_mock.header r Content_type));
  (match Media.decode_items todo_lines (Proffer_mock.body r) with
   | Ok items -> check_int "export items" 2 (List.length items)
   | Error e -> Alcotest.fail (Media.error_to_string e));
  let r =
    Proffer_mock.request site env Get "/either" ~headers:[ "Accept", "text/plain" ]
  in
  check_str "negotiated text" "write docs" (Proffer_mock.body r);
  check_str "vary" "Accept" (Option.get (Proffer_mock.header r Vary));
  let r = Proffer_mock.request site env Get "/either" ~headers:[ "Accept", "*/*" ] in
  check_str
    "negotiated fallback"
    "application/json"
    (Option.get (Proffer_mock.header r Content_type))
;;

(* Fetch *)
let json_response ?(status = 200) ?(ct = "application/json") body req =
  Fetch_mock.respond
    ~status
    ~headers:(Http.Header.of_list [ "Content-Type", ct ])
    body
    req
;;

let drain = function
  | Fetch.Empty -> ""
  | Fetch.String s -> s
  | Fetch.Stream { flow; _ } ->
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:1_000_000 flow)
;;

let api (req : Fetch.Middleware.request) =
  match Fetch.Middleware.Url.path_segments req.url, req.meth with
  | [ "todos"; "1" ], `GET ->
    Alcotest.(check (option string))
      "accept sent"
      (Some "application/json")
      (Http.Header.get req.headers "accept");
    json_response (Media.encode todo a) req
  | [ "todos"; "9" ], `GET -> json_response ~status:404 {|{"message":"no such todo"}|} req
  | [ "todos" ], `POST ->
    Alcotest.(check (option string))
      "content type sent"
      (Some "application/json")
      (Http.Header.get req.headers "content-type");
    json_response ~status:201 (drain req.body) req
  | [ "html" ], `GET -> json_response ~ct:"text/html" "<p>hi</p>" req
  | [ "broken" ], `GET -> json_response "{\"id\":" req
  | [ "large" ], `GET -> json_response "{\"id\":123456789}" req
  | [ "export" ], `GET ->
    json_response
      ~ct:"application/x-ndjson"
      (Media.encode_items todo_lines (List.to_seq [ a; b ]) ^ "\n{\"id\":\"bad\"}\n")
      req
  | [ "upload" ], `POST ->
    let items = Result.get_ok (Media.decode_items todo_lines (drain req.body)) in
    json_response (string_of_int (List.length items)) req
  | _ -> Fetch_mock.respond ~status:404 "" req
;;

let error_json =
  Fetch.Json.v
    (Jsont.Object.map (fun m -> m)
     |> Jsont.Object.mem "message" Jsont.string ~enc:Fun.id
     |> Jsont.Object.finish)
;;

let run f = Eio_mock.Backend.run f

let test_client () =
  run
  @@ fun () ->
  let client = Fetch_mock.client api in
  let base = "https://api.example" in
  (match Fetch.read_as client todo (base ^ "/todos/1") with
   | Ok t -> check_str "read_as" "write docs" t.title
   | Error r -> Alcotest.failf "unexpected status %d" (Fetch.status r));
  (match Fetch.read_as client todo (base ^ "/todos/9") with
   | Ok _ -> Alcotest.fail "expected error"
   | Error r ->
     check_int "404" 404 (Fetch.status r);
     check_str "error body decoded" "no such todo" (Fetch.decode error_json r));
  Eio.Switch.run (fun sw ->
    match Fetch.get_as ~sw client todo (base ^ "/todos/9") with
    | Error r -> check_str "get_as error" "no such todo" (Fetch.decode error_json r)
    | Ok _ -> Alcotest.fail "expected error");
  (match Fetch.read_as client todo (base ^ "/html") with
   | exception
       Eio.Io
         ( Fetch.E
             (Fetch.Decode_failure
               { media; error = Media.Unsupported (Some "text/html") })
         , _ ) -> check_str "expected media" "application/json" media
   | _ -> Alcotest.fail "expected unsupported");
  (match Fetch.read_as client fetch_todo (base ^ "/broken") with
   | exception
       Eio.Io
         ( Fetch.E
             (Fetch.Decode_failure
               { error = Media.Malformed { loc = Some _; detail = Fetch.Json.Jsont _; _ }
               ; _
               })
         , _ ) -> ()
   | _ -> Alcotest.fail "expected malformed");
  Eio.Switch.run (fun sw ->
    let response = Fetch.get ~sw client (base ^ "/large") in
    match Fetch.decode ~limit:8 fetch_todo response with
    | exception Eio.Io (Fetch.E (Fetch.Decode_failure { error = Media.Too_large 8; _ }), _)
      -> ()
    | _ -> Alcotest.fail "expected typed body limit");
  (match Fetch.expect (Fetch.read_as client todo (base ^ "/todos/9")) with
   | exception Fetch.Rejected r -> check_int "rejected" 404 (Fetch.status r)
   | _ -> Alcotest.fail "expected Rejected");
  Eio.Switch.run (fun sw ->
    let headers, body = Fetch.encode todo b in
    let r = Fetch.post ~sw ~headers ~body client (base ^ "/todos") in
    check_int "created" 201 (Fetch.status r);
    check_str "echoed" "ship" (Fetch.decode todo r).title);
  Eio.Switch.run (fun sw ->
    let r = Fetch.get ~sw client (base ^ "/export") in
    let items = Fetch.decode_seq todo_lines r in
    match items () with
    | Seq.Cons (t, rest) ->
      check_str "first item" "write docs" t.title;
      (match rest () with
       | Seq.Cons (t, rest) ->
         check_str "second item" "ship" t.title;
         (match rest () with
          | exception Eio.Io (Fetch.E (Fetch.Decode_failure _), _) -> ()
          | _ -> Alcotest.fail "expected failure on bad line")
       | Seq.Nil -> Alcotest.fail "expected second item")
    | Seq.Nil -> Alcotest.fail "expected items");
  Eio.Switch.run (fun sw ->
    let headers, body = Fetch.encode_seq todo_lines (List.to_seq [ a; b; a ]) in
    let r = Fetch.post ~sw ~headers ~body client (base ^ "/upload") in
    check_str
      "streamed upload"
      "3"
      (Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:100 (Fetch.body r))))
;;

let () =
  Alcotest.run
    "proffer-jsont"
    [ ( "jsont"
      , [ Alcotest.test_case "codec" `Quick test_codec
        ; Alcotest.test_case "portable domains" `Quick test_portable_domains
        ; Alcotest.test_case "proffer" `Quick test_server
        ; Alcotest.test_case "fetch" `Quick test_client
        ] )
    ]
;;
