(* Run with: dune exec --profile release-check
   bleeding/tessera/bench/bench_crs.exe *)

let sink = ref (0., 0.)
let allocated s = s.Gc.minor_words +. s.Gc.major_words -. s.Gc.promoted_words

let measure name crs project =
  let iterations = 100_000 in
  for i = 0 to 999 do
    sink := project crs i
  done;
  Gc.full_major ();
  let before = Gc.stat () in
  let counter = Mtime_clock.counter () in
  for i = 0 to iterations - 1 do
    sink := project crs i
  done;
  let ns = Mtime.Span.to_float_ns (Mtime_clock.count counter) in
  Gc.full_major ();
  let after = Gc.stat () in
  let x, y = !sink in
  Printf.printf "%s: %.1f ns, %.2f words/point, checksum=%.12g\n%!" name
    (ns /. float_of_int iterations)
    ((allocated after -. allocated before) /. float_of_int iterations)
    (x +. y)

let () =
  let open Tessera in
  let crs = Crs.utm_north ~zone:31 in
  measure "UTM forward" crs (fun crs i ->
      Crs.forward crs ~lon:(2. +. (float_of_int (i mod 100) *. 0.001)) ~lat:51.);
  measure "UTM inverse" crs (fun crs i ->
      Crs.inverse crs ~e:(430000. +. float_of_int (i mod 100)) ~n:5650000.)
