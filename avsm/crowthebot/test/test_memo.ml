open Crowthebot

let check name condition = if not condition then failwith name
let admin = "@admin:example.org"
let friend = "@friend:example.org"
let bot = "@bot:example.org"
let room = "!memo:example.org"

let contains haystack needle =
  let rec loop i =
    i + String.length needle <= String.length haystack
    && (String.sub haystack i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let rejected f =
  match f () with exception Invalid_argument _ -> true | _ -> false

let add store i =
  Store.add_fact store ~actor:admin ~room
    ~event:("$" ^ string_of_int i)
    ~source:"command"
    ~body:("Observation " ^ string_of_int i)

let access ?summarize store actor =
  Memory.for_request ?summarize store ~actor ~room ~event:"$overview"
    ~source:"command"

let overview access = Memory.invoke access "memory_overview" "{}"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let store = Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin in
  Store.add_room store room;
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:true;
  Store.set_person store ~actor:admin ~user:bot ~role:Bot ~allowed:true;
  let ids = List.init 32 (add store) in
  List.iter
    (fun actor ->
      check "overview requires authorization"
        (Result.is_error (overview (access store actor)));
      check "expansion requires authorization"
        (Result.is_error
           (Memory.invoke (access store actor) "memory_expand"
              {|{"key":"forged"}|})))
    [ bot; "@unknown:example.org" ];
  let tree, _ = Store.memory_tree store ~actor:friend in
  let merges = ref 0 in
  let summarize ~limit input =
    incr merges;
    check "input retains attribution" (contains input admin);
    check "input retains event" (contains input "$0");
    check "expected byte budget" (limit = 512);
    "Source 1: observation by admin."
  in
  check "overview succeeds"
    (Result.is_ok (overview (access ~summarize store friend)));
  check "one merge per request" (!merges = 1);
  let _, cache = Store.memory_tree store ~actor:admin in
  check "summary persisted" (List.length cache = 1);
  let old_key = List.hd (Agentkit.Memo.keys tree) in
  check "erase source" (Store.erase_fact store ~actor:admin (List.hd ids));
  let _, cache = Store.memory_tree store ~actor:admin in
  check "erasure purges all derived text" (cache = []);
  check "stale summary cannot be reinserted"
    (not
       (Store.save_memory_summary store ~actor:admin ~key:old_key ~body:"stale"));
  check "stale expansion rejected"
    (Result.is_error
       (Memory.invoke (access store admin) "memory_expand"
          (Printf.sprintf {|{"key":"%s"}|} old_key)));
  let stale_access = access store friend in
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:false;
  check "captured overview rechecks revocation"
    (Result.is_error (overview stale_access));
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:true;
  let revoke ~limit:_ _ =
    Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:false;
    "must not leak"
  in
  check "authorization rechecked after inference"
    (Result.is_error (overview (access ~summarize:revoke store friend)));
  let erase ~limit:_ _ =
    ignore (Store.erase_fact store ~actor:admin (List.nth ids 1));
    "erased observation must not return"
  in
  check "erasure during inference uses fresh sources"
    (match overview (access ~summarize:erase store admin) with
    | Ok text ->
        contains text "maintenance failed"
        && not (contains text "erased observation must not return")
    | Error _ -> false);
  check "summary failure is visible"
    (match
       overview
         (access ~summarize:(fun ~limit:_ _ -> failwith "offline") store admin)
     with
    | Ok text -> contains text "maintenance failed"
    | Error _ -> false);
  List.iter (fun actor ->
      check "automatic context requires authorization"
        (rejected (fun () -> Memory.context store ~actor ~limit:512)))
    [ friend; bot ];
  let block = Option.get (Memory.context store ~actor:admin ~limit:512) in
  check "automatic memory context fits its budget" (String.length block <= 512);
  let requests = ref 0 and summaries = ref 0 in
  let complete (request : Agentkit.Chat.request) =
    if request.tools = [] then begin
      incr summaries;
      check "summarizer has no tools" (request.tools = []);
      Agentkit.Chat.response ~finish:Stop
        (Some {|{"summary":"Source 3: admin observed an activity."}|})
    end
    else begin
      incr requests;
      check "runtime injects the automatic memory overview"
        (List.exists (function
             | Agentkit.Chat.User text -> contains text "Shared memory overview"
             | _ -> false) request.messages);
      if !requests = 1 then
        Agentkit.Chat.response ~finish:Tool_calls
          ~calls:
            [
              {
                Agentkit.Agent.id = "overview";
                name = "memory_overview";
                arguments = "{}";
              };
            ]
          None
      else Agentkit.Chat.response ~finish:Stop (Some "Overview read.")
    end
  in
  let config =
    {
      (Config.default ~admin ~homeserver:"https://matrix.example.org") with
      plugins = [];
    }
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[] ~complete
      ~now:(fun () -> 0.)
  in
  Engine.handle engine
    ~send:(fun _ -> ())
    { room; sender = admin; id = "$runtime"; body = "!crow ask Review memory." };
  check "runtime delegates one summary request" (!summaries = 1 && !requests = 2);
  check "summary has durable cache"
    (snd (Store.memory_tree store ~actor:admin) <> []);
  let tmp = Filename.temp_dir "memo-sqlite" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun name -> Eio.Path.unlink Eio.Path.(dir / name))
        (Eio.Path.read_dir dir);
      Eio.Path.rmdir dir)
    (fun () ->
      Eio.Switch.run (fun sw ->
          let store =
            Store.create
              (Sqlite3_eio.open_path ~sw Eio.Path.(dir / "store.sqlite"))
              ~admin
          in
          ignore (add store 0);
          ignore (add store 1);
          check "file-backed overview succeeds"
            (Result.is_ok
               (overview
                  (access
                     ~summarize:(fun ~limit:_ _ -> "durable summary")
                     store admin))));
      Eio.Switch.run (fun sw ->
          let db = Sqlite3_eio.open_path ~sw Eio.Path.(dir / "store.sqlite") in
          let store = Store.create db ~admin in
          let before, _ = Store.memory_tree store ~actor:admin in
          let key = List.hd (Agentkit.Memo.keys before) in
          let writer =
            Sqlite3_eio.open_path ~sw Eio.Path.(dir / "store.sqlite")
          in
          Sqlite3.Rc.check
            (Sqlite3_eio.exec writer
               "UPDATE facts SET body='External correction' WHERE id=1");
          check "same-revision external edit rejects stale summary"
            (not
               (Store.save_memory_summary store ~actor:admin ~key
                  ~body:"stale external edit"));
          let after, _ = Store.memory_tree store ~actor:admin in
          check "external edit changes current keys"
            (not (Agentkit.Memo.mem after key));
          check "external edit reaches current source view"
            (List.exists
               (fun (v : Agentkit.Memo.view) ->
                 contains v.text "External correction")
               (Agentkit.Memo.overview after ~budget:8 ~lookup:(fun _ -> None)));
          let key = List.hd (Agentkit.Memo.keys after) in
          check "current summary is accepted after an external edit"
            (Store.save_memory_summary store ~actor:admin ~key
               ~body:"current external edit");
          Sqlite3.Rc.check
            (Sqlite3_eio.exec writer "DELETE FROM facts WHERE id=1");
          check "external deletion rejects stale summary"
            (not
               (Store.save_memory_summary store ~actor:admin ~key
                  ~body:"stale external deletion"));
          check "external deletion clears cached summary text"
            (snd (Store.memory_tree store ~actor:admin) = []);
          Sqlite3.Rc.check
            (Sqlite3_eio.exec writer
               "INSERT INTO facts(id,created_at,author,room,event,source,body) \
                VALUES(1,'restored','@admin:example.org','!memo:example.org', \
                '$restored','command','Restored source')");
          let restored, _ = Store.memory_tree store ~actor:admin in
          check "external insertion restores the source count"
            (List.fold_left
               (fun n (v : Agentkit.Memo.view) -> n + v.count)
               0
               (Agentkit.Memo.overview restored ~budget:8
                  ~lookup:(fun _ -> None))
            = 2);
          let key = List.hd (Agentkit.Memo.keys restored) in
          check "restored current summary persists"
            (Store.save_memory_summary store ~actor:admin ~key
               ~body:"restored source summary"));
      Eio.Switch.run (fun sw ->
          let store =
            Store.create
              (Sqlite3_eio.open_path ~sw Eio.Path.(dir / "store.sqlite"))
              ~admin
          in
          check "SQLite cache survives process-style reopen"
            (List.length (snd (Store.memory_tree store ~actor:admin)) = 1);
          ignore (Store.erase_fact store ~actor:admin 1));
      Eio.Switch.run (fun sw ->
          let store =
            Store.create
              (Sqlite3_eio.open_path ~sw Eio.Path.(dir / "store.sqlite"))
              ~admin
          in
          check "erasure remains effective after reopen"
            (snd (Store.memory_tree store ~actor:admin) = [])));
  ignore env;
  print_endline
    "Memory tree authorization, erasure races and model delegation passed."
