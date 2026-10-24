module M = Siftbot_model
module S = Siftbot.Store

let check name b = if not b then failwith name
let first = List.hd Siftbot.Demo.messages
let second = List.nth Siftbot.Demo.messages 2

let metadata_deletion () =
  Sqlite_vec.register ();
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let exec sql = Sqlite3.Rc.check (Sqlite3.exec db sql) in
      exec
        "CREATE VIRTUAL TABLE metadata_test USING vec0(v float[2], label text)";
      exec
        "INSERT INTO metadata_test(rowid, v, label) VALUES (1, '[1,0]', 'long \
         metadata exceeding twelve characters')";
      exec "DELETE FROM metadata_test WHERE rowid = 1";
      let count = ref "" in
      Sqlite3.Rc.check
        (Sqlite3.exec db "SELECT count(*) FROM metadata_test" ~cb:(fun row _ ->
             count := Option.get row.(0)));
      check "long metadata deletion completes" (!count = "0");
      exec
        "INSERT INTO metadata_test(rowid, v, label) VALUES (1, '[0,1]', 'long \
         metadata inserted again after deletion')";
      exec "DELETE FROM metadata_test WHERE rowid = 1")

let () =
  metadata_deletion ();
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          S.commit_batch db ~job:"fixture" ~expected:None ~cursor:"1"
            [ first; second ] [];
          let index =
            S.vector_index db ~model:"test/model-v1" ~dimensions:2
              ~normalization:"none"
          in
          S.vector_put db ~index ~source:first.source ~revision:first.revision
            ~input_hash:"a" [ 1.; 0. ];
          S.vector_put db ~index ~source:second.source ~revision:second.revision
            ~input_hash:"b" [ 0.; 1. ];
          let neighbours = S.vector_search db ~index ~limit:2 [ 0.; 0.9 ] in
          check "vec0 nearest neighbour"
            ((fst (List.hd neighbours)).M.source = second.source);
          check "vec0 distance"
            (abs_float (snd (List.hd neighbours) -. 0.1) < 1e-5);
          check "cached model/input identity"
            (S.vector_current db ~index ~source:first.source
               ~revision:first.revision ~input_hash:"a");
          let other =
            S.vector_index db ~model:"test/model-v2" ~dimensions:2
              ~normalization:"none"
          in
          check "provider change separates index"
            (S.vector_search db ~index:other ~limit:2 [ 1.; 0. ] = []);
          let changed =
            { first with M.revision = "new"; text = "Changed body" }
          in
          S.commit_batch db ~job:"fixture" ~expected:(Some "1") ~cursor:"2"
            [ changed ] [ second.source ];
          check "source edits and deletion remove vectors"
            (S.vector_search db ~index ~limit:2 [ 1.; 0. ] = []);
          check "stale vector rejected"
            (match
               S.vector_put db ~index ~source:first.source
                 ~revision:first.revision ~input_hash:"a" [ 1.; 0. ]
             with
            | () -> false
            | exception S.Conflict -> true);
          let calls = ref 0 in
          let provider =
            Siftbot.Embedding.
              {
                model = "test/constant";
                dimensions = 2;
                max_batch = 2;
                embed =
                  (fun ~task:_ ~check inputs ->
                    check ();
                    incr calls;
                    List.map (fun _ -> [ 1.; 1. ]) inputs);
              }
          in
          let check_cancel () = () in
          check "bounded indexing"
            (Siftbot.Embedding.index db provider ~check:check_cancel
               ~max_batches:1
            = 1);
          check "embedding cache avoids repeated inference"
            (Siftbot.Embedding.index db provider ~check:check_cancel
               ~max_batches:1
             = 0
            && !calls = 1);
          let bad = { provider with dimensions = 3; model = "test/bad" } in
          check "dimension mismatch rejected"
            (match
               Siftbot.Embedding.index db bad ~check:check_cancel ~max_batches:1
             with
            | _ -> false
            | exception Invalid_argument _ -> true);
          let cancel () = failwith "cancelled" in
          check "cancellation before inference"
            (match
               Siftbot.Embedding.search db provider ~check:cancel "query"
             with
            | _ -> false
            | exception Failure s -> s = "cancelled");
          print_endline "sqlite-vec OxCaml consumer tests passed"))
