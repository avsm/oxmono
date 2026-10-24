open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module P = Taposaur_paper

let legacy_fingerprint paper =
  let manifest = A.manifest paper in
  let encoded = match Manifest.to_string manifest with
    | Ok s -> s | Error e -> failwith e in
  let stamps = List.map (fun (a : Manifest.Artifact.t) ->
    let stamp = try A.with_artifact paper a.file (fun file ->
      let s = Eio.File.stat file in
      Printf.sprintf "%Ld:%.17g:%.17g"
        (Optint.Int63.to_int64 s.size) s.mtime s.ctime)
      with Eio.Io _ as ex -> Printexc.to_string ex in
    a.file ^ ":" ^ stamp) manifest.artifacts in
  Digestif.SHA256.(to_hex (digest_string
    (String.concat "\000" (encoded :: stamps))))

let () =
  let compare = Array.length Sys.argv = 5 &&
    Sys.argv.(4) = "--compare-fingerprint" in
  if Array.length Sys.argv <> 4 && not compare then
    failwith
      "Usage: enrichment.exe ARCHIVE SOURCE_DB NEW_BENCH_DB \
       [--compare-fingerprint]";
  if Sys.file_exists Sys.argv.(3) then failwith "Benchmark DB already exists";
  Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    let path s = Eio.Path.(env#fs / s) in
    let archive = A.open_root ~sw (path Sys.argv.(1)) in
    let source = C.open_read ~sw (path Sys.argv.(2)) in
    let target = C.open_write ~sw (path Sys.argv.(3)) in
    let shards = Hashtbl.create 256 in
    List.iter (fun after ->
      C.Metadata.pending ~force:true ~after ~limit:512 ~version:P.version source
      |> List.filteri (fun i _ -> compare || i >= 128)
      |> List.iter (fun candidate ->
        let row = C.Metadata.index candidate in
        let shard = Layout.index row.uid in
        Hashtbl.replace shards shard
          (row :: Option.value ~default:[] (Hashtbl.find_opt shards shard))))
      (if compare then ["doi:10.1007/s"; "doi:10.1016/j."]
       else ["doi:10.1000/"; "doi:10.1007/"; "doi:10.1016/"; "pmc:"]);
    Hashtbl.iter (fun shard rows ->
      let rows = List.sort_uniq (fun a b ->
        String.compare (Uid.to_string a.Index.uid) (Uid.to_string b.Index.uid))
        rows in
      ignore (C.Shard.replace target ~shard {size=0L; mtime=0.}
        (fun emit -> List.iter emit rows))) shards;
    List.iteri (fun pass jobs ->
      let legacy = compare && (pass = 0 || pass = 3) in
      let fingerprint = if legacy then legacy_fingerprint else A.fingerprint in
      let samples = C.Metadata.pending ~force:true ~limit:1000
        ~version:P.version target
        |> List.filteri (fun i _ -> i mod 4 = pass) in
      let stages = Hashtbl.create 8 in
      let measure name f =
        let start = Unix.gettimeofday () in
        Fun.protect f ~finally:(fun () ->
          let elapsed = Unix.gettimeofday () -. start in
          Hashtbl.replace stages name
            (elapsed +. Option.value ~default:0. (Hashtbl.find_opt stages name)))
      in
      let start = Unix.gettimeofday () and cpu = Unix.times () in
      let unavailable = ref 0 and changed = ref 0 in
      Eio.Fiber.List.iter ~max_fibers:jobs (fun candidate ->
        let row = C.Metadata.index candidate in
        let stamp = ref None in
        let result = match measure "manifest" (fun () -> A.find archive row.uid) with
          | None -> incr unavailable; Error "Absent manifest"
          | Some paper ->
              let before = measure "fingerprint_before"
                (fun () -> fingerprint paper) in
              let resolved = measure "read_and_parse"
                (fun () -> Taposaur_reader.load ~index:row paper) in
              let after = measure "fingerprint_after" (fun () ->
                Option.map fingerprint (A.find archive row.uid)) in
              if after <> Some before then
                (incr changed; Error "Source changed")
              else (stamp := Some before; Ok (P.bibliography resolved)) in
        ignore (measure "publish" (fun () -> C.Metadata.publish
          ?source_stamp:!stamp target candidate ~version:P.version result))) samples;
      let elapsed = Unix.gettimeofday () -. start in
      let used = Unix.times () in
      Printf.printf
        "mode=%s jobs=%d papers=%d seconds=%.3f papers_per_second=%.2f \
         cpu_seconds=%.3f unavailable=%d changed=%d\n%!"
        (if legacy then "open/fstat" else "stat") jobs
        (List.length samples) elapsed
        (float_of_int (List.length samples) /. elapsed)
        (used.tms_utime +. used.tms_stime -. cpu.tms_utime -. cpu.tms_stime)
        !unavailable !changed;
      Hashtbl.iter (fun name seconds ->
        Printf.printf "  %s summed_worker_seconds=%.3f\n%!" name seconds) stages)
      (if compare then [32; 32; 32; 32] else [1; 8; 32; 64])))
