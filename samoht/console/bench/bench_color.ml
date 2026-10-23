(* Run with: dune exec --profile release-check
   samoht/console/bench/bench_color.exe *)

let sink = ref (Console.Color.rgb 0 0 0)
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
  Printf.printf "%s: %.1f ns, %.2f words/call\n%!" name
    (ns /. float_of_int iterations)
    ((allocated after -. allocated before) /. float_of_int iterations)

let () =
  let open Console in
  let first = Color.rgb 20 40 60 and last = Color.rgb 100 160 240 in
  let gradient = Gradient.v ~length:80 [ first; last ] in
  measure "RGB blend" 1_000_000 (fun i ->
      Color.blend first last (float_of_int (i mod 80) /. 80.));
  measure "RGB scale" 1_000_000 (fun i ->
      Color.scale (float_of_int (i mod 80) /. 40.) first);
  measure "gradient cell" 1_000_000 (fun i ->
      Gradient.cell gradient ~row:0 ~column:(i mod 80))
