module I = Retrieval.Index
module E = Retrieval.Embedding

let check name condition = if not condition then failwith name

let document text =
  I.
    {
      id = "text";
      revision = "1";
      kind = "note";
      title = "";
      text;
      url = "";
      date = "";
      facets = [];
    }

let passages text =
  List.map (fun (c : I.chunk) -> c.text) (I.chunks (document text))

(* The previous chunker is an oracle for byte and newline boundaries. *)
let reference_passages text =
  let rec loop offset acc =
    if offset >= String.length text then List.rev acc
    else
      let rest = String.sub text offset (String.length text - offset) in
      let prefix = E.clip 6000 rest in
      let length = String.length prefix in
      let stop =
        if length < String.length rest then
          match String.rindex_opt prefix '\n' with
          | Some i when i > length / 2 -> i + 1
          | _ -> length
        else length
      in
      loop (offset + stop) (String.sub rest 0 stop :: acc)
  in
  match loop 0 [] with [] -> [ "" ] | chunks -> chunks

let check_passages text =
  let actual = passages text in
  check "passage boundaries match the previous chunker"
    (actual = reference_passages text);
  check "passages retain every byte" (String.concat "" actual = text);
  check "each passage is bounded UTF-8"
    (List.for_all
       (fun s -> String.length s <= 6000 && String.is_valid_utf_8 s)
       actual)

let test_passages () =
  List.iter
    (fun n -> check_passages (String.make n 'x'))
    [ 0; 1; 5999; 6000; 6001; 12000; 12001; 500000 ];
  List.iter
    (fun at ->
      check_passages (String.make at 'x' ^ "\n" ^ String.make 7000 'y'))
    [ 2999; 3000; 3001; 5998; 5999; 6000 ];
  List.iter
    (fun uchar ->
      for at = 5996 to 6001 do
        check_passages (String.make at 'x' ^ uchar ^ String.make 7000 'y')
      done)
    [ "é"; "€"; "🦀" ];
  let state = Random.State.make [| 14097 |] in
  let chars = [| "a"; " "; "\n"; "é"; "€"; "🦀" |] in
  for _ = 1 to 50 do
    let b = Buffer.create 18000 in
    for _ = 1 to Random.State.int state 12000 do
      Buffer.add_string b chars.(Random.State.int state (Array.length chars))
    done;
    check_passages (Buffer.contents b)
  done

let test_malformed_passages () =
  let run text =
    let chunks = passages text in
    check "malformed passages retain every byte" (String.concat "" chunks = text);
    check "malformed passages make bounded progress"
      (List.for_all
         (fun chunk ->
           let size = String.length chunk in
           size > 0 && size <= 6000)
         chunks)
  in
  List.iter run
    [
      String.make 6001 '\x80';
      String.make 500000 '\xbf';
      "prefix" ^ String.make 18000 '\x80';
      String.make 6000 'a' ^ String.make 18000 '\x80';
      "\xc2" ^ String.make 6001 '\x80';
    ];
  let random = Random.State.make [| 122 |] in
  for _ = 1 to 100 do
    run (String.init 12001 (fun _ -> Char.chr (Random.State.int random 256)))
  done

let test_clip () =
  let text = "aé€🦀z" in
  for limit = 0 to String.length text + 1 do
    let clipped = E.clip limit text in
    check "clip respects its byte bound" (String.length clipped <= limit);
    check "clip preserves valid UTF-8" (String.is_valid_utf_8 clipped);
    check "clip returns a prefix" (String.starts_with ~prefix:clipped text)
  done;
  check "negative clip bounds fail"
    (try
       ignore (E.clip (-1) text);
       false
     with Invalid_argument _ -> true)

let test_batches ~size ~max_batch ~max_batches =
  let documents = List.init size string_of_int in
  let current id = int_of_string id mod 3 = 0 in
  let eligible = List.filter (fun id -> not (current id)) documents in
  let expected_count = min (List.length eligible) (max_batch * max_batches) in
  let expected = List.filteri (fun i _ -> i < expected_count) eligible in
  let batches = ref [] and written = ref [] and checks = ref 0 in
  let scanned = ref [] in
  let backend =
    E.
      {
        documents = (fun () -> documents);
        key = Fun.id;
        revision = (fun _ -> "1");
        passage =
          (fun id ->
            scanned := id :: !scanned;
            id);
        index = (fun ~model:_ ~dimensions:_ ~normalization:_ -> "fixture");
        current = (fun ~index:_ ~document ~input_hash:_ -> current document);
        put =
          (fun ~index:_ ~document ~input_hash:_ vector ->
            check "normalization preserved" (vector = [ 0.6; 0.8 ]);
            written := document :: !written);
        nearest = (fun ~index:_ ~limit:_ _ -> []);
        lexical = (fun _ -> []);
      }
  in
  let provider =
    E.
      {
        model = "fixture";
        dimensions = 2;
        max_batch;
        embed =
          (fun ~task:_ ~check:_ inputs ->
            check "batch is nonempty and bounded"
              (inputs <> [] && List.length inputs <= max_batch);
            batches := inputs :: !batches;
            List.map (fun _ -> [ 3.; 4. ]) inputs);
      }
  in
  let progress = ref [] in
  let count =
    E.index backend provider ~check:(fun () -> incr checks) ~max_batches
      ~on_progress:(fun count ->
        check "progress follows all writes in its batch"
          (count = List.length !written);
        progress := count :: !progress)
  in
  let _, expected_progress =
    List.fold_left
      (fun (total, counts) batch ->
        let total = total + List.length batch in
        (total, total :: counts))
      (0, []) (List.rev !batches)
  in
  check "progress reports cumulative completed batches only"
    (!progress = expected_progress);
  check "batch count respects budget" (count = expected_count);
  check "batch order and current filtering preserved"
    (List.concat (List.rev !batches) = expected);
  check "writes preserve batch order" (List.rev !written = expected);
  check "cancellation checks surround every inference"
    (!checks = 2 * List.length !batches);
  let scans = List.length !scanned in
  check "each inspected passage is read once"
    (scans = List.length (List.sort_uniq String.compare !scanned));
  if max_batches = 0 then check "zero budget avoids document scans" (scans = 0)
  else if expected_count < List.length eligible then
    check "budget stops scanning after the last selected document"
      (List.hd !scanned = List.nth expected (expected_count - 1))

let test_normalization () =
  let reference dimensions values =
    if
      List.length values <> dimensions
      || List.exists
           (fun x -> (not (Float.is_finite x)) || abs_float x > 1e30)
           values
    then invalid_arg "Embedding provider returned invalid dimensions or values";
    let norm = sqrt (List.fold_left (fun sum x -> sum +. (x *. x)) 0. values) in
    if norm = 0. then invalid_arg "Embedding provider returned a zero vector";
    List.map (fun x -> x /. norm) values
  in
  let outcome f =
    try Ok (List.map Int64.bits_of_float (f ()))
    with Invalid_argument message -> Error message
  in
  let run dimensions values =
    let provider =
      E.
        {
          model = "fixture";
          dimensions;
          max_batch = 16;
          embed = (fun ~task:_ ~check:_ _ -> assert false);
        }
    in
    check "normalization preserves bits and validation failures"
      (outcome (fun () -> E.checked provider values)
      = outcome (fun () -> reference dimensions values))
  in
  List.iter
    (fun values ->
      let n = List.length values in
      List.iter (fun dimensions -> run dimensions values) [ n; n + 1; -1 ])
    [
      [];
      [ 0.; -0. ];
      [ 3.; 4. ];
      [ 1.; -0. ];
      [ nan ];
      [ infinity ];
      [ neg_infinity ];
      [ 1e30; -1e30 ];
      [ 1e31 ];
    ];
  let state = Random.State.make [| 30914 |] in
  List.iter
    (fun dimensions ->
      for _ = 1 to 20 do
        run dimensions
          (List.init dimensions (fun _ -> Random.State.float state 2e10 -. 1e10))
      done)
    [ 1; 2; 16; 1536; 4096 ]

let test_small_normalization () =
  let run expected values =
    let provider =
      E.
        {
          model = "small";
          dimensions = List.length values;
          max_batch = 1;
          embed = (fun ~task:_ ~check:_ _ -> assert false);
        }
    in
    let actual = E.checked provider values in
    check "tiny nonzero vectors normalize"
      (List.for_all2 (fun a b -> abs_float (a -. b) <= 1e-15) actual expected);
    check "tiny normalized vectors have unit norm"
      (abs_float (List.fold_left (fun sum x -> sum +. (x *. x)) 0. actual -. 1.)
      <= 1e-14)
  in
  run [ 1. ] [ 1e-300 ];
  run [ -1. ] [ -.Int64.float_of_bits 1L ];
  run [ 0.6; 0.8 ] [ 3e-300; 4e-300 ];
  run [ 0.6; 0.8 ] [ 3e-160; 4e-160 ];
  run (List.init 1024 (fun _ -> 1. /. 32.)) (List.init 1024 (fun _ -> 1e-300))

let test_batch_snapshot () =
  let passage_calls = ref 0 and writes = ref [] and embedded = ref [] in
  let backend =
    E.
      {
        documents = (fun () -> [ "first"; "second" ]);
        key = Fun.id;
        revision = (fun _ -> "1");
        passage =
          (fun id ->
            incr passage_calls;
            id ^ string_of_int !passage_calls);
        index = (fun ~model:_ ~dimensions:_ ~normalization:_ -> "fixture");
        current = (fun ~index:_ ~document:_ ~input_hash:_ -> false);
        put =
          (fun ~index:_ ~document:_ ~input_hash vector ->
            writes := (input_hash, vector) :: !writes);
        nearest = (fun ~index:_ ~limit:_ _ -> []);
        lexical = (fun _ -> []);
      }
  in
  let provider =
    E.
      {
        model = "snapshot";
        dimensions = 2;
        max_batch = 2;
        embed =
          (fun ~task:_ ~check:_ inputs ->
            embedded := inputs;
            List.map (fun _ -> [ 3.; 4. ]) inputs);
      }
  in
  ignore (E.index backend provider ~check:(fun () -> ()) ~max_batches:1);
  check "passage producer called once per document" (!passage_calls = 2);
  check "stored hashes match the exact inference input"
    (List.map fst (List.rev !writes) = List.map E.hash !embedded);
  writes := [];
  let invalid =
    { provider with
      embed = (fun ~task:_ ~check:_ _ -> [ [ 3.; 4. ]; [ 0.; 0. ] ]) }
  in
  check "invalid later vector rejects the whole batch"
    (try
       ignore (E.index backend invalid ~check:(fun () -> ()) ~max_batches:1);
       false
     with Invalid_argument _ -> !writes = []);
  check "cancellation prevents any write"
    (try
       ignore
         (E.index backend provider
            ~check:(fun () -> failwith "cancel")
            ~max_batches:1);
       false
     with Failure _ -> !writes = [])

let test_failed_progress () =
  let writes = ref [] and progress = ref [] in
  let backend =
    E.
      {
        documents = (fun () -> [ "first"; "second" ]);
        key = Fun.id;
        revision = Fun.id;
        passage = Fun.id;
        index = (fun ~model:_ ~dimensions:_ ~normalization:_ -> "progress");
        current = (fun ~index:_ ~document:_ ~input_hash:_ -> false);
        put =
          (fun ~index:_ ~document ~input_hash:_ _ ->
            writes := document :: !writes);
        nearest = (fun ~index:_ ~limit:_ _ -> []);
        lexical = (fun _ -> []);
      }
  in
  let provider =
    E.
      {
        model = "progress";
        dimensions = 2;
        max_batch = 2;
        embed =
          (fun ~task:_ ~check:_ inputs ->
            List.map (fun _ -> [ 1.; 0. ]) inputs);
      }
  in
  let run backend provider check_call =
    writes := [];
    progress := [];
    try
      ignore
        (E.index backend provider ~check:check_call ~max_batches:2
           ~on_progress:(fun count -> progress := count :: !progress));
      false
    with Invalid_argument _ | Failure _ | I.Conflict -> true
  in
  let cached =
    { backend with current = (fun ~index:_ ~document:_ ~input_hash:_ -> true) }
  in
  check "generic cache hit emits no initial progress"
    (E.index cached provider ~check:(fun () -> ()) ~max_batches:2
       ~on_progress:(fun count -> progress := count :: !progress) = 0
    && !progress = [] && !writes = []);
  let failing =
    { provider with embed = (fun ~task:_ ~check:_ _ -> failwith "inference") }
  in
  check "inference failure emits no progress"
    (run backend failing (fun () -> ()) && !writes = [] && !progress = []);
  List.iter
    (fun vectors ->
      let invalid =
        { provider with embed = (fun ~task:_ ~check:_ _ -> vectors) }
      in
      check "failed provider batch never reports progress"
        (run backend invalid (fun () -> ())
        && !writes = [] && !progress = []))
    [ []; [ [ 1.; 0. ]; [ 0.; 0. ] ] ];
  let checks = ref 0 in
  check "cancellation after inference never reports progress"
    (run backend provider (fun () ->
         incr checks;
         if !checks = 2 then failwith "cancel")
    && !writes = [] && !progress = []);
  let conflicting =
    { backend with
      put =
        (fun ~index ~document ~input_hash vector ->
          if document = "second" then raise I.Conflict;
          backend.put ~index ~document ~input_hash vector) }
  in
  check "partially written failed batch never reports completion"
    (run conflicting provider (fun () -> ())
    && !writes = [ "first" ] && !progress = []);
  let single = { provider with max_batch = 1 } in
  check "later failed batch retains earlier completed progress only"
    (run conflicting single (fun () -> ())
    && !writes = [ "first" ] && !progress = [ 1 ])

let test_task_roles () =
  let calls = ref [] in
  let provider =
    E.
      {
        model = "tasks";
        dimensions = 2;
        max_batch = 1;
        embed =
          (fun ~task ~check:_ inputs ->
            calls := (task, inputs) :: !calls;
            List.map (fun _ -> [ 1.; 0. ]) inputs);
      }
  in
  let backend =
    E.
      {
        documents = (fun () -> [ "document" ]);
        key = Fun.id;
        revision = Fun.id;
        passage = Fun.id;
        index = (fun ~model:_ ~dimensions:_ ~normalization:_ -> "tasks");
        current = (fun ~index:_ ~document:_ ~input_hash:_ -> false);
        put = (fun ~index:_ ~document:_ ~input_hash:_ _ -> ());
        nearest = (fun ~index:_ ~limit:_ _ -> []);
        lexical = (fun _ -> []);
      }
  in
  ignore (E.index backend provider ~check:(fun () -> ()) ~max_batches:1);
  ignore (E.search backend provider ~check:(fun () -> ()) "question");
  check "indexing and search identify their task to providers"
    (List.rev !calls = [ (`Document, [ "document" ]); (`Query, [ "question" ]) ])

let () =
  test_task_roles ();
  test_failed_progress ();
  test_passages ();
  test_malformed_passages ();
  test_clip ();
  test_normalization ();
  test_small_normalization ();
  test_batch_snapshot ();
  List.iter
    (fun max_batch ->
      List.iter
        (fun max_batches -> test_batches ~size:1033 ~max_batch ~max_batches)
        [ 0; 1; 3; 10000 ])
    [ 1; 16; 128 ];
  test_batches ~size:0 ~max_batch:16 ~max_batches:100;
  test_batches ~size:10000 ~max_batch:16 ~max_batches:10000;
  print_endline
    "retrieval passage, batch and normalization differentials passed"
