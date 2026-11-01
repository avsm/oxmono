open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module E = Taposaur_indexing.Enrich
module P = Taposaur_paper

let () =
  if Array.length Sys.argv <> 5 then
    failwith "Usage: ordering.exe ARCHIVE SOURCE_DB NEW_BENCH_DIR SHARD_PREFIX";
  if Sys.file_exists Sys.argv.(3) then failwith "Benchmark directory exists";
  Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    let path s = Eio.Path.(env#fs / s) in
    let archive = A.open_root ~sw (path Sys.argv.(1)) in
    let source = C.open_read ~sw (path Sys.argv.(2)) in
    let prefix = Sys.argv.(4) in
    let shards = C.Shard.paths ~after:prefix ~limit:16 source in
    if List.length shards <> 16 || not (List.for_all
      (String.starts_with ~prefix) shards) then
      failwith "Prefix must contain at least 16 shards";
    Eio.Path.mkdir ~perm:0o700 (path Sys.argv.(3));
    List.iteri (fun pass order ->
      let target = C.open_write ~sw
        Eio.Path.(path Sys.argv.(3) / (string_of_int pass ^ ".db")) in
      let selected = List.filteri (fun i _ -> i mod 4 = pass) shards in
      List.iter (fun shard ->
        let rows = C.Metadata.pending ~force:true ~shard ~limit:128
          ~version:P.version source in
        ignore (C.Shard.replace target ~shard {size = 0L; mtime = 0.}
          (fun emit -> List.iter (fun c -> emit (C.Metadata.index c)) rows)))
        selected;
      let started = Unix.gettimeofday () and cpu = Unix.times () in
      let result = E.run ~order ~jobs:32 target archive in
      let seconds = Unix.gettimeofday () -. started and used = Unix.times () in
      Printf.printf
        "pass=%d order=%s jobs=32 papers=%d seconds=%.3f rate=%.2f cpu=%.3f cached=%d errors=%d\n%!"
        pass (match order with E.Directory -> "directory" | Uid -> "uid")
        result.examined seconds (float_of_int result.examined /. seconds)
        (used.tms_utime +. used.tms_stime -. cpu.tms_utime -. cpu.tms_stime)
        (C.Metadata.detail_count target) result.unavailable)
      [E.Uid; E.Directory; E.Directory; E.Uid]))
