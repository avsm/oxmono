let uris =
  [ "https://user:pass@www.example.com:8080/a/b/c?foo=bar&baz=qux,quux#frag";
    "http://example.com/";
    "/relative/path?q=1";
    "https://[2001:db8::1]:443/api/v1/items?filter=a,b,c";
    "urn:isbn:0451450523" ]

let sink = ref 0

let () =
  let n = 200_000 in
  let total = n * List.length uris in
  List.iter (fun s -> ignore (Uriz.of_string s)) uris;
  (* of_string *)
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    List.iter
      (fun s ->
        match Uriz.of_string s with This u -> sink := !sink + Uriz.port_int u | Null -> ())
      uris
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  Printf.printf "of_string: %.0f ns/parse, %.0f bytes alloc/parse\n"
    ((t1 -. t0) *. 1e9 /. float total)
    ((a1 -. a0) /. float total);
  (* to_string *)
  let ts = List.filter_map (fun s -> match Uriz.of_string s with This u -> Some u | Null -> None) uris in
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    List.iter (fun t -> sink := !sink + String.length (Uriz.to_string t)) ts
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  Printf.printf "to_string: %.1f ns/call, %.0f bytes alloc/call\n"
    ((t1 -. t0) *. 1e9 /. float total)
    ((a1 -. a0) /. float total);
  (* of_string_canonical: stack-allocated record, shared canonical string *)
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    List.iter
      (fun s ->
        match Uriz.of_string_canonical s with
        | This u -> sink := !sink + Uriz.port_int u
        | Null -> ())
      uris
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  Printf.printf "of_string_canonical: %.0f ns/parse, %.0f bytes alloc/parse\n"
    ((t1 -. t0) *. 1e9 /. float total)
    ((a1 -. a0) /. float total);
  (* Raw.parse *)
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    List.iter (fun s -> sink := !sink + Uriz.Raw.err (Uriz.Raw.parse s)) uris
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  Printf.printf "Raw.parse: %.0f ns/parse, %.0f bytes alloc/parse\n"
    ((t1 -. t0) *. 1e9 /. float total)
    ((a1 -. a0) /. float total);
  (* resolve, global against region-allocated *)
  let base = match Uriz.of_string "http://a/b/c/d;p?q" with This u -> u | Null -> assert false in
  let rel = match Uriz.of_string "../../g" with This u -> u | Null -> assert false in
  let n2 = n * 5 in
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n2 do
    sink := !sink + String.length (Uriz.to_string (Uriz.resolve ~base rel))
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  Printf.printf "resolve: %.0f ns/call, %.0f bytes alloc/call\n"
    ((t1 -. t0) *. 1e9 /. float n2)
    ((a1 -. a0) /. float n2);
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n2 do
    sink := !sink + String.length (Uriz.to_string__local (Uriz.resolve__local ~base rel))
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  Printf.printf "resolve__local: %.0f ns/call, %.0f bytes alloc/call\n"
    ((t1 -. t0) *. 1e9 /. float n2)
    ((a1 -. a0) /. float n2);
  ignore !sink
