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
        (fun ~check inputs ->
          check ();
          incr calls;
          List.map (fun _ -> [ 1.; 0. ]) inputs);
    }

let () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let db = I.memory ~sw () in
  let calls = ref 0 in
  let p = provider calls in
  let long =
    String.concat "\n" (List.init 4000 (fun _ -> "café repeated passage"))
    ^ "\nUNIQUE_TAIL"
  in
  let d = doc "note" "note" long in
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
          embed = (fun ~check:_ inputs -> List.map (fun _ -> values) inputs);
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
    { p with model = "short-batch"; embed = (fun ~check:_ _ -> []) }
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
  print_endline "retrieval tests passed"
