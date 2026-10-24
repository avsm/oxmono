module I = Retrieval.Index
module E = Retrieval.Embedding

let check s b = if not b then failwith s
let noop () = ()

let doc id kind text =
  I.
    {
      id;
      kind;
      text;
      revision = "1";
      title = id;
      url = "";
      date = "2026-10-22";
      facets = [];
    }

let provider calls =
  E.
    {
      model = "fixture";
      dimensions = 2;
      max_batch = 16;
      embed =
        (fun ~task:_ ~check inputs ->
          check ();
          incr calls;
          List.map (fun _ -> [ 1.; 0. ]) inputs);
    }

let snapshots env =
  let file = Filename.temp_file "retrieval-snapshot" ".sqlite" in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun suffix ->
          let path = file ^ suffix in
          if Sys.file_exists path then Sys.remove path)
        [ ""; "-wal"; "-shm" ])
    (fun () ->
      Eio.Switch.run (fun sw ->
          let path = Eio.Path.(Eio.Stdenv.fs env / file) in
          let writer = I.open_db ~sw path and reader = I.open_db ~sw path in
          let version c =
            let revision = String.make 1 c in
            { (doc "shared" "note" (String.make 48000 c)) with
              revision; facets = [ ("revision", revision) ] }
          in
          let a = version 'a' and b = version 'b' in
          I.replace writer [ a ];
          let backend = I.backend reader I.all in
          Eio.Fiber.pair
            (fun () ->
              for n = 1 to 20 do
                I.replace writer [ if n mod 2 = 0 then a else b ]
              done)
            (fun () ->
              for _ = 1 to 50 do
                let chunks = backend.documents () in
                let document = (List.hd chunks).I.document in
                check "concurrent reader sees one coherent snapshot"
                  (document.facets = [ ("revision", document.revision) ]
                  && document.text = String.make 48000 document.revision.[0]
                  && String.concat ""
                       (List.map (fun (c : I.chunk) -> c.text) chunks)
                     = document.text)
              done)
          |> ignore))

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db = I.memory ~sw () in
  let calls = ref 0 in
  let p = provider calls in
  let long =
    String.concat "\n" (List.init 4000 (fun _ -> "café repeated passage"))
    ^ "\nUNIQUE_TAIL"
  in
  let d =
    { (doc "note" "note" long) with
      facets = [ ("z", "last"); ("topic", "test"); ("a", "first") ] }
  in
  let chunks = I.chunks d in
  check "all long-note bytes survive chunking"
    (String.concat "" (List.map (fun (c : I.chunk) -> c.text) chunks) = long);
  check "bounded chunks"
    (List.for_all (fun (c : I.chunk) -> String.length c.text <= 6000) chunks);
  let distractors =
    List.init 30 (fun i -> doc (Printf.sprintf "a%02d" i) "feed" "noise")
  in
  I.replace db (d :: distractors);
  let backend = I.backend db I.all in
  let selected = backend.documents () in
  let note_chunks =
    List.filter (fun (c : I.chunk) -> c.document.id = "note") selected
  in
  let shared = (List.hd note_chunks).document in
  check "chunk results share document bodies and facets"
    (List.for_all (fun (c : I.chunk) -> c.document == shared) note_chunks);
  check "shared document facets stay sorted"
    (shared.facets = List.sort_uniq compare d.facets);
  check "selected chunks retain full documents and ordinal order"
    (List.map (fun (c : I.chunk) -> (c.ordinal, c.text)) note_chunks
    = List.map (fun (c : I.chunk) -> (c.ordinal, c.text)) chunks
    && shared.text = long);
  let filter =
    { I.kind = Some "note"; since = Some d.date;
      until = Some d.date; facets = [ ("topic", "test") ] }
  in
  check "shared selection preserves combined filters"
    ((I.backend db filter).documents () = note_chunks);
  check "empty lexical query preserves date and ID ordering"
    (I.lexical db I.all "" = selected);
  let legacy =
    backend.index ~model:p.model ~dimensions:p.dimensions
      ~normalization:"passage-8192-l2/v1"
  in
  let cached = List.hd (backend.documents ()) in
  let input_hash = E.hash (backend.passage cached) in
  backend.put ~index:legacy ~document:cached ~input_hash [ 1.; 0. ];
  let active = E.index_name backend p in
  check "normalization upgrade isolates legacy indexes" (active <> legacy);
  check "legacy vector does not satisfy the upgraded cache"
    (backend.current ~index:legacy ~document:cached ~input_hash
    && not (backend.current ~index:active ~document:cached ~input_hash));
  let first_batch = E.index backend p ~check:noop ~max_batches:1 in
  check "normalization refresh respects batch budget"
    (first_batch = p.max_batch);
  check "index all passages"
    (first_batch + E.index backend p ~check:noop ~max_batches:100
    = List.length chunks + 30);
  check "normalization upgrade fills the new index"
    (backend.current ~index:active ~document:cached ~input_hash);
  check "tail found lexically"
    (List.exists
       (fun (c : I.chunk) -> c.document.id = "note")
       (I.lexical db I.all "UNIQUE_TAIL"));
  let before = !calls in
  check "unchanged snapshot reuses vectors"
    (I.replace db (List.rev (d :: distractors));
     E.index backend p ~check:noop ~max_batches:100 = 0 && !calls = before);
  let filtered = I.backend db { I.all with kind = Some "note" } in
  check "filter before vector ranking despite 30 tied distractors"
    (List.length (E.search filtered p ~check:noop "query")
    = min 20 (List.length chunks));
  let old = List.hd (backend.documents ()) in
  let index = E.index_name backend p in
  I.replace db [ { d with text = "replacement"; revision = "2" } ];
  check "deletions and edits remove vectors"
    (E.search (I.backend db I.all) p ~check:noop "query" = []);
  check "stale inference cannot be committed"
    (try
       backend.put ~index ~document:old ~input_hash:"old" [ 1.; 0. ];
       false
     with I.Conflict -> true);
  check "replacement is indexed"
    (E.index (I.backend db I.all) p ~check:noop ~max_batches:1 = 1);
  let bad = { p with model = "invalid"; dimensions = 3 } in
  check "dimension mismatch rejected"
    (try
       ignore (E.index (I.backend db I.all) bad ~check:noop ~max_batches:1);
       false
     with Invalid_argument _ -> true);
  check "duplicate IDs rejected atomically"
    (try
       I.replace db [ d; d ];
       false
     with Invalid_argument _ -> I.get db "note" <> None);
  List.iter
    (fun values ->
      let invalid =
        {
          p with
          model = "bad-values";
          embed =
            (fun ~task:_ ~check:_ inputs -> List.map (fun _ -> values) inputs);
        }
      in
      check "invalid vector values rejected"
        (try
           ignore
             (E.index (I.backend db I.all) invalid ~check:noop ~max_batches:1);
           false
         with Invalid_argument _ -> true))
    [ [ 0.; 0. ]; [ nan; 1. ]; [ infinity; 0. ] ];
  let short =
    { p with model = "short-batch"; embed = (fun ~task:_ ~check:_ _ -> []) }
  in
  check "incomplete batch rejected"
    (try
       ignore (E.index (I.backend db I.all) short ~check:noop ~max_batches:1);
       false
     with Invalid_argument _ -> true);
  let before = !calls in
  check "cancellation prevents inference"
    (try
       ignore (E.search backend p ~check:(fun () -> failwith "cancel") "q");
       false
     with Failure _ -> !calls = before);
  check "rank ties are stable"
    (E.fuse ~key:Fun.id ~limit:10 [ [ "b" ]; [ "a" ] ] = [ "a"; "b" ]);
  snapshots env;
  print_endline "retrieval tests passed"
