(* Frame and parse 100,000 FETCH rows fed in 64 KiB chunks. *)

let rows = 100_000

let response =
  let b = Buffer.create (rows * 48) in
  for n = 1 to rows do
    Printf.bprintf b "* %d FETCH (UID %d FLAGS (\\Seen) MODSEQ (%d))\r\n"
      n n n
  done;
  Buffer.contents b

let chunks =
  let size = 65_536 and len = String.length response in
  List.init ((len + size - 1) / size) (fun i ->
    String.sub response (i * size) (min size (len - i * size)))

let workload () =
  let wire = Imap.Wire.create () in
  let parsed = ref 0 and pending = ref [] in
  let event e =
    pending := e :: !pending;
    if e = Imap.Wire.End_of_response then begin
      (match Imap.Response.parse_parts (List.rev !pending) with
       | Ok (Imap.Response.Untagged (Imap.Response.Fetch _)) -> incr parsed
       | Ok _ -> failwith "not a FETCH row"
       | Error e -> failwith e);
      pending := []
    end in
  List.iter (fun chunk ->
    match Imap.Wire.feed wire chunk with
    | Ok events -> List.iter event events
    | Error e -> failwith e.message) chunks;
  (match Imap.Wire.finish wire with
   | Ok () -> ()
   | Error e -> failwith e.message);
  !parsed

let () =
  let parsed = Bench_measure.run "wire+parse 100000 FETCH rows" workload in
  if parsed <> rows then failwith "row count"
