let () =
  let input = String.init 100 (fun i -> Char.chr (i mod 256)) in
  let compressed = Brotli.compress ~quality:Brotli.Q1 input in
  Printf.printf "Original compressed bytes (%d):\n" (String.length compressed);
  for i = 0 to String.length compressed - 1 do
    Printf.printf "%02x " (Char.code compressed.[i]);
    if (i + 1) mod 20 = 0 then print_newline ()
  done;
  print_newline ()
