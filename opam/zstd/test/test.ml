(* Test zstd compression with direct C bindings *)

let printf = Printf.printf

let test (name, src) =
  let orig = String.length src in
  let a = Array.init 20 (fun level -> Zstd.compress ~level src) in
  a |> Array.iteri begin fun i s ->
    assert (Zstd.get_decompressed_size s = orig);
    let s = Zstd.decompress orig s in
    if s <> src then failwith @@ Printf.sprintf "%s : level %d failed" name i;
  end;
  let best = Array.fold_left (fun m s -> min m (String.length s)) (String.length a.(0)) a in
  let best_level =
    let rec find i =
      if i >= Array.length a then 0
      else if String.length a.(i) = best then i
      else find (i + 1)
    in
    find 0
  in
  printf "%50s : best compression %02.1fx at level %d : %d -> %d\n"
    name (float orig /. float best) best_level orig best

let read_file f =
  let ic = open_in_bin f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  let file f =
    try Some (f, read_file f)
    with _ -> None
  in
  let inputs = [
    file "/bin/bash";
    file Sys.executable_name;
    file "/etc/ld.so.cache";
    file "/etc/mailcap";
    Some ("environment", String.concat " " @@ Array.to_list @@ Unix.environment ());
  ] |> List.filter_map Fun.id
  in
  (* Test standard functions *)
  printf "Testing standard compress/decompress:\n";
  List.iter test inputs;

  (* Test version *)
  let major, minor, patch = Zstd.version () in
  printf "\nZstd version: %d.%d.%d\n" major minor patch;

  (* Test buffer-passing API *)
  printf "\nTesting buffer-passing compress_to/decompress_to:\n";
  List.iter (fun (name, src) ->
    let orig_len = String.length src in
    let dst_size = Zstd.compress_bound orig_len in
    let compress_buf = Bytes.create dst_size in
    let compressed_len = Zstd.compress_to ~level:3 src compress_buf in
    let decompress_buf = Bytes.create orig_len in
    let decompressed_len = Zstd.decompress_to orig_len (Bytes.sub_string compress_buf 0 compressed_len) decompress_buf in
    let decompressed = Bytes.sub_string decompress_buf 0 decompressed_len in
    assert (decompressed_len = orig_len);
    assert (decompressed = src);
    printf "%50s : buffer API OK (%d -> %d)\n" name orig_len compressed_len
  ) inputs;

  printf "\nAll tests passed!\n"
