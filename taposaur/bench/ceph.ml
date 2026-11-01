open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module P = Taposaur_paper

let () =
  if Array.length Sys.argv <> 6 && Array.length Sys.argv <> 7 then
    failwith "Usage: ceph.exe ARCHIVE CATALOGUE AFTER COUNT JOBS [SHARD]";
  let count = int_of_string Sys.argv.(4) in
  let jobs = int_of_string Sys.argv.(5) in
  if count < 4 || count > 1000 || jobs < 1 || jobs > 64 then
    invalid_arg "COUNT must be 4..1000 and JOBS 1..64";
  Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    let root = Eio.Path.(env#fs / Sys.argv.(1)) in
    let archive = A.open_root ~sw root in
    let catalog = C.open_read ~sw Eio.Path.(env#fs / Sys.argv.(2)) in
    let rows = if Array.length Sys.argv = 7 then begin
      let rows = ref [] in
      A.iter_index archive ~shard:Sys.argv.(6) (fun _ row ->
        if String.compare (Uid.to_string row.Index.uid) Sys.argv.(3) > 0 then
          rows := row :: !rows);
      List.sort (fun a b -> Uid.compare a.Index.uid b.Index.uid) !rows
      |> List.filteri (fun i _ -> i < count)
    end else
      C.Metadata.pending ~force:true ~after:Sys.argv.(3)
        ~limit:count ~version:P.version catalog |> List.map C.Metadata.index in
    List.iteri (fun pass prefetch ->
      let rows = List.filteri (fun i _ -> i mod 4 = pass) rows in
      let stages = Hashtbl.create 8 in
      let measure name f =
        let start = Unix.gettimeofday () in
        Fun.protect f ~finally:(fun () ->
          let elapsed = Unix.gettimeofday () -. start in
          Hashtbl.replace stages name (elapsed ::
            Option.value ~default:[] (Hashtbl.find_opt stages name))) in
      let start = Unix.gettimeofday () and cpu = Unix.times () in
      Eio.Fiber.List.iter ~max_fibers:jobs (fun (row : Index.t) ->
        if prefetch then measure "readdir" (fun () ->
          Eio.Path.with_dir_entries
            Eio.Path.(root / "papers" / Layout.paper row.uid)
            (Seq.iter (fun _ -> ())));
        match measure "manifest" (fun () -> A.find archive row.uid) with
        | None -> ()
        | Some paper ->
            let before = measure "fingerprint_before"
              (fun () -> A.fingerprint paper) in
            let resolved = measure "read_and_parse"
              (fun () -> Taposaur_reader.load ~index:row paper) in
            let after = measure "fingerprint_after" (fun () ->
              Option.map A.fingerprint (A.find archive row.uid)) in
            if after <> Some before then failwith "Source changed";
            ignore (measure "cache_encode" (fun () -> P.cache_string resolved));
            ignore (measure "warm_manifest" (fun () -> A.find archive row.uid)))
        rows;
      let seconds = Unix.gettimeofday () -. start in
      let used = Unix.times () in
      Printf.printf "pass=%d prefetch=%b jobs=%d papers=%d seconds=%.3f rate=%.2f cpu=%.3f\n%!"
        pass prefetch jobs (List.length rows) seconds
        (float_of_int (List.length rows) /. seconds)
        (used.tms_utime +. used.tms_stime -. cpu.tms_utime -. cpu.tms_stime);
      Hashtbl.to_seq stages |> List.of_seq |> List.sort compare
      |> List.iter (fun (name, samples) ->
        let sorted = Array.of_list (List.sort Float.compare samples) in
        let percentile p = sorted.(min (Array.length sorted - 1)
          (int_of_float (float_of_int (Array.length sorted) *. p))) in
        Printf.printf "  %s sum=%.3f p50=%.6f p95=%.6f max=%.6f\n%!"
          name (List.fold_left (+.) 0. samples) (percentile 0.5)
          (percentile 0.95) (percentile 0.999)))
      [false; true; true; false]))
