(* Run with: dune exec --profile release-check
   bleeding/agentkit/bench/bench_memo.exe *)

let sink = ref false
let allocated s = s.Gc.minor_words +. s.Gc.major_words -. s.Gc.promoted_words

let measure name iterations f =
  for i = 0 to 999 do
    sink := f i
  done;
  Gc.full_major ();
  let before = Gc.stat () in
  let counter = Mtime_clock.counter () in
  for i = 0 to iterations - 1 do
    sink := f i
  done;
  let ns = Mtime.Span.to_float_ns (Mtime_clock.count counter) in
  Gc.full_major ();
  let after = Gc.stat () in
  Printf.printf "%s: %.1f ns, %.2f words/query\n%!" name
    (ns /. float_of_int iterations)
    ((allocated after -. allocated before) /. float_of_int iterations)

let () =
  let tree =
    Agentkit.Memo.create
      (List.init 1024 (fun i ->
           {
             Agentkit.Memo.id = string_of_int i;
             revision = "1";
             text = "A recorded source";
           }))
  in
  let keys = Agentkit.Memo.keys tree in
  let queries = Array.of_list keys in
  let key i = queries.(i mod Array.length queries) in
  measure "rebuild keys and scan" 10_000 (fun i ->
      List.mem (key i) (Agentkit.Memo.keys tree));
  measure "scan cached keys" 10_000 (fun i -> List.mem (key i) keys);
  measure "indexed membership" 10_000 (fun i -> Agentkit.Memo.mem tree (key i))
