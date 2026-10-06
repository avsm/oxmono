let check message condition = if not condition then failwith message
let lookup _ = None

let source i =
  {
    Agentkit.Memo.id = string_of_int i;
    revision = "1";
    text = Printf.sprintf "Fact %d: café visit" i;
  }

let sources n = List.init n source
let bad f = match f () with exception Invalid_argument _ -> true | _ -> false

let utf8 text =
  let rec loop i =
    if i = String.length text then true
    else
      let d = String.get_utf_8_uchar text i in
      Uchar.utf_decode_is_valid d && loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let () =
  for n = 0 to 150 do
    let tree = Agentkit.Memo.create (sources n) in
    for budget = 1 to 24 do
      let views = Agentkit.Memo.overview tree ~budget ~lookup in
      check "node budget" (List.length views <= budget);
      let next = ref 0 in
      List.iter
        (fun (v : Agentkit.Memo.view) ->
          check "contiguous cover" (int_of_string v.first = !next);
          next := !next + v.count;
          check "range endpoint" (int_of_string v.last = !next - 1);
          let children = Agentkit.Memo.expand tree ~key:v.key ~lookup in
          check "expansion preserves coverage"
            (List.fold_left
               (fun n (c : Agentkit.Memo.view) -> n + c.count)
               0 children
            = v.count))
        views;
      check "complete cover" (!next = n)
    done
  done;
  let tree = Agentkit.Memo.create (sources 64) in
  let views = Agentkit.Memo.overview tree ~budget:8 ~lookup in
  let newest = List.hd (List.rev views) in
  check "recent sources retain detail" (newest.count = 1);
  check "oldest sources compress" ((List.hd views).count > 1);
  check "missing summary explicit" (List.hd views).missing;
  let cache = Hashtbl.create 16 in
  let lookup key = Hashtbl.find_opt cache key in
  let calls = ref 0 in
  let merged =
    Agentkit.Memo.maintain tree ~max_merges:3 ~limit:512 ~lookup
      ~save:(fun ~key ~text -> Hashtbl.replace cache key text)
      ~summarize:(fun ~limit:_ input ->
        incr calls;
        check "has source material" (String.trim input <> "");
        "summary")
  in
  check "maintenance bounded" (merged = 3 && !calls = 3);
  let stale = List.hd (Agentkit.Memo.keys tree) in
  let edited =
    Agentkit.Memo.create
      ({ (source 0) with revision = "2"; text = "corrected" }
      :: List.tl (sources 64))
  in
  check "edit invalidates ancestor"
    (bad (fun () -> Agentkit.Memo.expand edited ~key:stale ~lookup));
  let erased = Agentkit.Memo.create (List.tl (sources 64)) in
  check "erasure invalidates ancestor"
    (bad (fun () -> Agentkit.Memo.expand erased ~key:stale ~lookup));
  check "duplicate source rejected"
    (bad (fun () -> Agentkit.Memo.create [ source 0; source 0 ]));
  check "invalid UTF-8 rejected"
    (bad (fun () -> Agentkit.Memo.create [ { (source 0) with text = "\255" } ]));
  check "nonpositive budget rejected"
    (bad (fun () -> Agentkit.Memo.overview tree ~budget:0 ~lookup));
  check "empty summary rejected"
    (bad (fun () -> Agentkit.Memo.validate_summary ~limit:512 " "));
  let tiny =
    Agentkit.Memo.create
      [
        {
          (source 0) with
          text = String.concat "" (List.init 1000 (fun _ -> "é"));
        };
      ]
  in
  for limit = 256 to 300 do
    let text =
      Agentkit.Memo.overview tiny ~budget:1 ~lookup
      |> Agentkit.Memo.render ~limit
    in
    check "render bounded UTF-8" (String.length text <= limit && utf8 text)
  done;
  for n = 2 to 40 do
    let tree = Agentkit.Memo.create (sources n) in
    let cache = Hashtbl.create 64 in
    let lookup key = Hashtbl.find_opt cache key in
    let count =
      Agentkit.Memo.maintain tree ~max_merges:100 ~limit:512 ~lookup
        ~save:(fun ~key ~text -> Hashtbl.replace cache key text)
        ~summarize:(fun ~limit:_ _ -> "merged sources")
    in
    check "all internal nodes can be summarized" (count = n - 1);
    check "complete cache has no missing ranges"
      (List.for_all
         (fun (v : Agentkit.Memo.view) -> not v.missing)
         (Agentkit.Memo.overview tree ~budget:1 ~lookup))
  done;
  let large =
    Agentkit.Memo.create
      (List.init 8 (fun i -> { (source i) with text = String.make 2048 'x' }))
  in
  let views = Agentkit.Memo.overview large ~budget:8 ~lookup in
  let text = Agentkit.Memo.render ~limit:3500 views in
  check "large originals keep every range pointer"
    (List.length (String.split_on_char '\n' text) >= 24
    && String.length text <= 3500);
  List.iter
    (fun (v : Agentkit.Memo.view) ->
      let rec scan i =
        i + String.length v.key <= String.length text
        && (String.sub text i (String.length v.key) = v.key || scan (i + 1))
      in
      check "render preserves recent and old expansion keys" (scan 0))
    views;
  let saved = ref false in
  check "failed inference is visible"
    (bad (fun () ->
         Agentkit.Memo.maintain tree ~max_merges:1 ~limit:512
           ~lookup:(fun _ -> None)
           ~save:(fun ~key:_ ~text:_ -> saved := true)
           ~summarize:(fun ~limit:_ _ -> "")));
  check "failed inference does not poison cache" (not !saved);
  print_endline "Memo coverage, recency, invalidation and byte budgets passed."
