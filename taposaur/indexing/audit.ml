open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog

type coverage = Not_indexed | Tei | Held | No_artifacts
let coverage catalog uid = match C.find catalog uid with
  | None -> Not_indexed
  | Some summary when summary.holdings.tei -> Tei
  | Some summary when summary.holdings.pdf || summary.holdings.xml ||
      summary.holdings.s2orc -> Held
  | Some _ -> No_artifacts

let coverage_name = function
  | Not_indexed -> "not-indexed" | Tei -> "tei"
  | Held -> "held" | No_artifacts -> "no-artifacts"

let validate ?(limit = 100) ?shard ?(artifacts = false) archive =
  if limit < 1 then invalid_arg "limit must be positive";
  let count = ref 0 in
  let exception Enough in
  let check shard = A.iter_index archive ~shard (fun line e ->
    if !count >= limit then raise Enough;
    if Layout.index e.Index.uid <> shard then
      failwith (Printf.sprintf "%s:%d: wrong shard" shard line);
    let paper = match A.find archive e.uid with
      | Some p -> p
      | None -> failwith ("Paper not found: " ^ Uid.to_string e.uid) in
    if artifacts then List.iter (fun (artifact : Manifest.Artifact.t) ->
      A.with_artifact paper artifact.file (fun file ->
        if Optint.Int63.to_int64 (Eio.File.size file) <> artifact.bytes then
          failwith (Uid.to_string e.uid ^ "/" ^ artifact.file ^
            ": size mismatch"))) (A.manifest paper).artifacts;
    incr count) in
  (try match shard with Some s -> check s | None -> A.iter_shards archive check
   with Enough -> ());
  !count
