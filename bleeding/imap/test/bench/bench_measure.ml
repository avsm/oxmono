let run name f =
  Gc.full_major ();
  let bytes = Gc.allocated_bytes () and minor = Gc.minor_words () in
  let start = Unix.gettimeofday () in
  let result = f () in
  let wall = Unix.gettimeofday () -. start in
  let bytes = Gc.allocated_bytes () -. bytes
  and minor = Gc.minor_words () -. minor in
  Printf.printf "%s: wall %.4f s, allocated %.0f B, minor %.0f words\n%!"
    name wall bytes minor;
  result
