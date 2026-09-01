(* Manual release benchmarks; timings are observations, never test thresholds. *)
let measure name fn =
  Gc.full_major ();
  let before = Gc.allocated_bytes () and started = Unix.gettimeofday () in
  let result = fn () in
  Printf.printf "%s: %.3fs; %.0f allocated bytes\n%!" name
    (Unix.gettimeofday () -. started) (Gc.allocated_bytes () -. before);
  result

let rec equal_at text literal offset index =
  index = String.length literal
  || (text.[offset + index] = literal.[index]
      && equal_at text literal offset (index + 1))

let scan () =
  let text = String.make (16 * 1024 * 1024) 'x' in
  let old i literal =
    let n = String.length literal in
    i <= String.length text - n && String.sub text i n = literal in
  let current i literal =
    i <= String.length text - String.length literal && equal_at text literal i 0 in
  let run starts =
    let matches = ref 0 in
    for i = 0 to String.length text - 1 do
      if starts i " href=\"" || starts i " src=\"" then incr matches
    done;
    assert (!matches = 0) in
  measure "16 MiB rendered-URL scan, old substring probes" (fun () -> run old);
  measure "16 MiB rendered-URL scan, current byte probes" (fun () -> run current)

let cookies () = Eio_main.run @@ fun env ->
  let size = 32 * 1024 * 1024 in
  let buffer = Buffer.create size in
  for i = 0 to 2999 do
    Buffer.add_string buffer
      (Printf.sprintf "d%d.example\tFALSE\t/\tFALSE\t0\tc%d\tv\n" (i / 50) i)
  done;
  let line = "overflow.example\tFALSE\t/\tFALSE\t0\tignored\tv\n" in
  while Buffer.length buffer + String.length line <= size - 2 do
    Buffer.add_string buffer line
  done;
  Buffer.add_char buffer '#';
  Buffer.add_string buffer (String.make (size - Buffer.length buffer - 1) ' ');
  Buffer.add_char buffer '\n';
  assert (Buffer.length buffer = size);
  let file = Filename.temp_file "httpz-cookie-benchmark-" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  let path = Eio.Path.(env#fs / file) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path (Buffer.contents buffer);
  let jar = measure "full 32 MiB Netscape load, already at 3000-cookie cap"
    (fun () -> Cookie_jar.of_file ~clock:env#clock ~save:`Manual path) in
  assert (List.length (Cookie_jar.cookies jar) = 3000)

let () = scan (); cookies ()
