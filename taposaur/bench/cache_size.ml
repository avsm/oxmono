open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module P = Taposaur_paper
let get = function Ok x -> x | Error e -> failwith e
let quote s = get (Jsont_bytesrw.encode_string Jsont.string s)

let () =
  if Array.length Sys.argv <> 3 then
    failwith "Usage: cache_size.exe ARCHIVE CATALOGUE";
  Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    let a = A.open_root ~sw Eio.Path.(env#fs / Sys.argv.(1)) in
    let c = C.open_read ~sw Eio.Path.(env#fs / Sys.argv.(2)) in
    let samples = List.concat_map (fun prefix ->
      C.Metadata.pending ~force:true ~after:prefix ~limit:16
        ~version:P.version c
      |> List.map C.Metadata.index
      |> List.filter (fun row ->
        String.starts_with ~prefix (Uid.to_string row.Index.uid)))
      ["doi:10.0000/"; "doi:10.1007/bf"; "doi:10.1007/s";
       "doi:10.1016/j."; "doi:10.1038/"; "doi:10.1371/";
       "doi:10.1093/"; "pmc:"] in
    Eio.Fiber.List.iter ~max_fibers:8 (fun row ->
      match A.find a row.Index.uid with
      | None -> Printf.eprintf "Missing: %s\n%!" (Uid.to_string row.uid)
      | Some paper ->
          let p = Taposaur_reader.load ~index:row paper in
          let documents = List.map (fun (d : P.document) ->
            let observation = match d.bibliography with
              | Ok b -> "\"bibliography\":" ^ get (Bibliography.to_string b)
              | Error e -> "\"error\":" ^ quote e in
            "{\"file\":" ^ quote d.artifact.file ^ "," ^ observation ^ "}")
            p.documents in
          (* Original index JSON and freshness stamps are already stored.
             These inputs can reconstruct the complete resolved view with
             the pure resolver, without duplicating per-field evidence. *)
          let payload = "{\"version\":1,\"manifest\":" ^
            get (Manifest.to_string p.manifest) ^ ",\"documents\":[" ^
            String.concat "," documents ^ "]}" in
          Printf.printf
            "{\"uid\":%s,\"bytes\":%d,\"artifacts\":%d,\"documents\":%d}\n%!"
            (quote (Uid.to_string row.uid)) (String.length payload)
            (List.length p.manifest.artifacts) (List.length p.documents)) samples))
