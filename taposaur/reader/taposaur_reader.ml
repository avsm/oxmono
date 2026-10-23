open Taposaur
module A = Taposaur_archive
module D = Taposaur_document
module P = Taposaur_paper

let load ?index paper =
  let manifest = A.manifest paper in
  let documents = List.filter_map (fun (artifact : Manifest.Artifact.t) ->
    let parser = match artifact.format, artifact.source with
      | Manifest.Format.Tei, _ -> Some (fun flow -> D.Tei.metadata flow)
      | Xml, Publisher -> Some (fun flow ->
          D.Elsevier.metadata flow)
      | Json, S2orc -> Some (fun flow ->
          D.S2orc.metadata flow)
      | _ -> None in
    Option.map (fun parse ->
      let bibliography = try A.with_artifact paper artifact.file parse
        with Eio.Io _ as ex -> Error (Printexc.to_string ex) in
      { P.artifact; bibliography }) parser) manifest.artifacts in
  P.resolve ?index manifest documents
