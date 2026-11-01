open Taposaur
module C = Taposaur_catalog
let get = function Ok x -> x | Error e -> failwith e
let timed f =
  let before = Unix.gettimeofday () in
  let result = f () in
  result, Unix.gettimeofday () -. before
let () =
  if Array.length Sys.argv <> 3 then
    failwith "Usage: catalogue.exe NEW_DATABASE ROW_COUNT";
  let path = Sys.argv.(1) and rows = int_of_string Sys.argv.(2) in
  if rows < 1 then invalid_arg "ROW_COUNT must be positive";
  if Sys.file_exists path then failwith "Benchmark requires a new database";
  Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    let c = C.open_write ~sw Eio.Path.(env#fs / path) in
    let shards = Hashtbl.create 256 in
    for i = 1 to rows do
      let uid = get (Uid.of_input (Printf.sprintf "10.1000/benchmark-%08d" i)) in
      let shard = Layout.index uid in
      let row = get (Index.of_string (Printf.sprintf
        {|{"uid":%S,"scheme":"doi",
          "pdf_path":"","pdf_size":0,"pdf_mtime":0,
          "tei_path":"","tei_mtime":0,"title":"Bird migration habitat",
          "abstract":"A representative searchable abstract about migration and habitat.",
          "publisher":"Example","year":2024}|}
        (Uid.to_string uid))) in
      Hashtbl.replace shards shard
        (row :: Option.value ~default:[] (Hashtbl.find_opt shards shard))
    done;
    let (), seconds = timed (fun () -> Hashtbl.iter (fun shard records ->
      ignore (C.Shard.replace c ~shard {C.Shard.size = 1L; mtime = 1.}
        (fun emit -> List.iter emit records))) shards) in
    Printf.printf "rows=%d import_seconds=%.3f\n%!" rows seconds;
    let measure name f =
      let samples = List.init 101 (fun _ -> snd (timed f)) |> List.sort compare in
      Printf.printf "%s p50_ms=%.3f p95_ms=%.3f\n%!" name
        (1000. *. List.nth samples 50) (1000. *. List.nth samples 95) in
    measure "stats" (fun () -> ignore (C.stats c));
    measure "search" (fun () -> ignore (C.search c
      (C.Query.make ~text:"migration" ())));
    measure "browse" (fun () -> ignore (C.search c (C.Query.make ())));
    let s = C.stats c in
    if s.papers <> rows || s.titles <> rows then failwith "Incorrect counters"))
