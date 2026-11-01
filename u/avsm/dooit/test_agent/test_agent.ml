module A = Dooit_agent
module D = Dooit.Doc
module S = Dooit.Store
open Dooit.Common

let assert_true message condition = if not condition then failwith message
let ok = function Ok value -> json value | Error message -> failwith message

let error (result : (string, string) result) =
  match result with Error _ -> () | Ok _ -> failwith "Expected tool error"

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let fixture f =
  let root = Filename.temp_file "dooit-agent-" "" in
  Unix.unlink root;
  Fun.protect
    ~finally:(fun () -> if exists root then remove root)
    (fun () -> f (S.init root))

let op kind value = obj [ ("op", str kind); ("value", value) ]

let () =
  Eio_main.run @@ fun _ ->
  fixture @@ fun store ->
  let agent = A.create_local ~store ~max_bytes:1024 in
  let fiber_thread = Thread.id (Thread.self ()) and checks = ref 0 in
  let check () =
    Eio.Fiber.check ();
    incr checks;
    assert_true "authority check stays on Eio thread"
      (Thread.id (Thread.self ()) = fiber_thread)
  in
  let invoke ?operation name args =
    let operation = Option.value operation ~default:(new_uuid ()) in
    let result = A.invoke agent ~check ~operation name (json_string args) in
    (match result with
    | Ok raw ->
        assert_true "complete result respects byte budget"
          (String.length raw <= 1024)
    | Error _ -> ());
    result
  in
  List.iter
    (fun name -> assert_true "tools exported" (A.is_tool name))
    [ "dooit_search"; "dooit_get"; "dooit_create"; "dooit_update" ];
  let operation = new_uuid () in
  let args =
    obj
      [
        ("title", str "Prepare internship report");
        ("body", str "Research at university");
        ("tags", arr [ str "work" ]);
        ("due", str "2026-11-30");
      ]
  in
  let created = invoke ~operation "dooit_create" args |> ok in
  let id = field "id" created and revision = field "revision" created in
  assert_true "host operation determines stable task ID"
    (id = uuid5 store.id ("create:" ^ operation));
  assert_true "create checks authority before and after work" (!checks >= 2);
  let read = invoke "dooit_get" (obj [ ("id", str id) ]) |> ok in
  assert_true "read exposes complete metadata"
    (items "tags" (get "metadata" read) = [ str "work" ]);
  let replay = invoke ~operation "dooit_create" (canonical args) |> ok in
  assert_true "create retry returns recorded revision" (equal created replay);
  assert_true "create retry does not duplicate note"
    (List.length (fst (S.scan store)) = 1);
  invoke ~operation "dooit_create" (set "title" (str "Different title") args)
  |> error;
  invoke "dooit_create" (set "operation" (str operation) args) |> error;
  invoke "dooit_create" (set "due" (str "2026-02-30") args) |> error;
  assert_true "invalid create cannot write a task"
    (List.length (fst (S.scan store)) = 1);
  let found =
    invoke "dooit_search"
      (obj
         [
           ("query", str "internship");
           ("status", str "open");
           ("tags", arr [ str "work" ]);
         ])
    |> ok
  in
  assert_true "search filters and returns task identity"
    (List.map (field "id") (items "items" found) = [ id ]);
  let update =
    obj
      [
        ("id", str id);
        ("revision", str revision);
        ( "operations",
          arr [ op "set_status" (str "done"); op "add_tag" (str "reviewed") ] );
      ]
  in
  let update_operation = new_uuid () in
  let updated =
    invoke ~operation:update_operation "dooit_update" update |> ok
  in
  assert_true "update persists status" (D.status (S.get store id) = "done");
  assert_true "update retry succeeds with its old expected revision"
    (equal updated
       (invoke ~operation:update_operation "dooit_update" update |> ok));
  invoke "dooit_update" update |> error;
  let latest = S.get store id in
  assert_true "stale update preserves current note"
    (D.revision latest = field "revision" updated);
  let bad =
    set "revision" (str (D.revision latest)) update
    |> set "operations"
         (arr
            [
              op "set_title" (str "Must not persist");
              op "set_status" (str "bogus");
            ])
  in
  invoke "dooit_update" bad |> error;
  assert_true "invalid later operation rolls back entire update"
    (D.revision (S.get store id) = D.revision latest);
  let parallel_operation = new_uuid () in
  let first, second =
    Eio.Fiber.pair
      (fun () -> invoke ~operation:parallel_operation "dooit_create" args)
      (fun () -> invoke ~operation:parallel_operation "dooit_create" args)
  in
  let retry = function
    | Ok result -> json result
    | Error message when String.starts_with ~prefix:"store is busy:" message ->
        invoke ~operation:parallel_operation "dooit_create" args |> ok
    | Error message -> failwith message
  in
  assert_true "concurrent retries return one durable operation"
    (equal (retry first) (retry second));
  let body =
    String.concat "" (List.init 1000 (fun _ -> "café\n\"quoted\"\n"))
  in
  let large =
    invoke "dooit_create"
      (obj [ ("title", str "Large body"); ("body", str body) ])
    |> ok
  in
  let large_id = field "id" large in
  let rec read offset acc =
    let result =
      invoke "dooit_get"
        (obj [ ("id", str large_id); ("offset", `Float (float offset)) ])
      |> ok
    in
    let acc = field "body" result :: acc in
    match get "next_offset" result with
    | `Null -> String.concat "" (List.rev acc)
    | `Float next ->
        let next = int_of_float next in
        assert_true "UTF-8 pages always advance" (next > offset);
        read next acc
    | _ -> failwith "Invalid next offset"
  in
  assert_true "paged body reconstructs exact original bytes" (read 0 [] = body);
  invoke "dooit_get" (obj [ ("id", str large_id); ("offset", `Float 4.) ])
  |> error;
  invoke "dooit_get"
    (obj [ ("id", str large_id); ("offset", `Float (float_of_int max_int)) ])
  |> error;
  let before = List.length (fst (S.scan store)) in
  let denied () = failwith "denied" in
  (try
     ignore
       (A.invoke agent ~check:denied ~operation:(new_uuid ()) "dooit_create"
          (json_string args));
     failwith "authority check was swallowed"
   with
  | Failure message when message = "denied" -> ()
  | ex -> raise ex);
  assert_true "denied invocation does not write"
    (List.length (fst (S.scan store)) = before);
  (try
     Eio.Cancel.sub (fun context ->
         Eio.Cancel.cancel context Exit;
         ignore (invoke "dooit_create" args));
     failwith "cancellation was swallowed"
   with Eio.Cancel.Cancelled _ -> ());
  assert_true "cancelled invocation does not write"
    (List.length (fst (S.scan store)) = before);
  let late_operation = new_uuid () in
  let late_id = uuid5 store.id ("create:" ^ late_operation) in
  let late_check () =
    if exists (S.note_path store late_id) then failwith "revoked"
  in
  (try
     ignore
       (A.invoke agent ~check:late_check ~operation:late_operation
          "dooit_create" (json_string args));
     failwith "post-write authority check was swallowed"
   with
  | Failure message when message = "revoked" -> ()
  | ex -> raise ex);
  let recovered = invoke ~operation:late_operation "dooit_create" args |> ok in
  assert_true "retry recovers committed result after authority revocation"
    (field "id" recovered = uuid5 store.id ("create:" ^ late_operation));
  for i = 1 to 12 do
    ignore
      (invoke "dooit_create"
         (obj [ ("title", str ("Page task " ^ string_of_int i)) ])
      |> ok)
  done;
  let rec pages offset acc =
    let page =
      invoke "dooit_search" (obj [ ("offset", `Float (float offset)) ]) |> ok
    in
    let acc = List.map (field "id") (items "items" page) @ acc in
    match get "next_offset" page with
    | `Null -> acc
    | `Float next ->
        let next = int_of_float next in
        assert_true "search pages advance" (next > offset);
        pages next acc
    | _ -> failwith "Invalid search offset"
  in
  let ids = pages 0 [] in
  assert_true "search pagination returns every task once"
    (List.length ids = List.length (fst (S.scan store))
    && List.length ids = List.length (List.sort_uniq compare ids));
  let bad_id = new_uuid () in
  atomic_write (S.note_path store bad_id) "broken note";
  invoke "dooit_search" (obj []) |> error;
  invoke "dooit_get" (obj [ ("id", str bad_id) ]) |> error;
  print_endline "Dooit agent tests passed"
