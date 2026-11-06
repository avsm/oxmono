(* Digestif's C backend must take byte lengths at full width. Upstream 1.4.0
   declares hash input lengths [uint32_t] and narrows them, and the BLAKE3
   XOF output length, in the stub glue. A single feed of 4 GiB or more is
   then hashed modulo 2^32 bytes. A behavioural check needs such a feed,
   which is too slow for the default test alias, so this guard rejects the
   narrowing spellings in the vendored C sources given as arguments. *)

let contains line sub =
  let n = String.length line and m = String.length sub in
  let rec go i = i + m <= n && (String.sub line i m = sub || go (i + 1)) in
  go 0

let narrowed_prototype line =
  contains line "_update"
  && (contains line "uint32_t len" || contains line "uint32_t inlen")

let narrowed_glue line =
  contains line "uint32_t len_"
  || contains line "Int_val"
     && (contains line "(src, off)"
        || contains line "(context, off)"
        || contains line "(dst, off)")

let () =
  let failures = ref 0 and updates = ref 0 and glue = ref false in
  for i = 1 to Array.length Sys.argv - 1 do
    let path = Sys.argv.(i) in
    if Filename.basename path = "stubs.c" then glue := true;
    In_channel.with_open_bin path In_channel.input_lines
    |> List.iteri (fun n line ->
           if contains line "_update" && contains line "size_t" then
             incr updates;
           if narrowed_prototype line || narrowed_glue line then (
             incr failures;
             Printf.eprintf "%s:%d: 32-bit length: %s\n" path (n + 1)
               (String.trim line)))
  done;
  if !failures > 0 then exit 1;
  if not !glue then failwith "the Digestif stub glue was not checked";
  if !updates = 0 then failwith "no Digestif update declaration was checked";
  print_endline "Digestif C byte lengths are full width"
