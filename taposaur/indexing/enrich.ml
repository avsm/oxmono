open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module P = Taposaur_paper

type result = Updated | Partial of string list | Unavailable of string | Superseded
type order = Directory | Uid
type stats = {
  examined : int; updated : int; unavailable : int; superseded : int;
  partial : int;
}

let run ?audit ?(publisher_lookup = fun _ -> None) ?(retry_errors = false) ?(force = false) ?(limit = max_int) ?uid ?(jobs = 8)
    ?(order = Directory)
    ?(progress = fun _ _ -> ()) catalog archive =
  if limit < 0 then invalid_arg "limit must be nonnegative";
  if jobs < 1 || jobs > 64 then invalid_arg "jobs must be 1..64";
  let examined = ref 0 and updated = ref 0 and unavailable = ref 0 in
  let partial = ref 0 in
  let superseded = ref 0 and cursor = ref "" in
  let process candidate =
    let row = C.Metadata.index candidate in
    let stamp = ref None and diagnostics = ref [] and detail_cache = ref None in
    let result = try match A.find archive row.uid with
      | None -> Error "Paper is absent from the archive"
      | Some paper ->
          let before = A.fingerprint paper in
          let resolved = Taposaur_reader.load ~index:row paper in
          let resolved = match Option.bind (Uid.doi row.uid) publisher_lookup with
            | None -> resolved | Some e -> P.with_publisher e resolved in
          let after = Option.map A.fingerprint (A.find archive row.uid) in
          if after <> Some before then Error "Paper changed during resolution"
          else begin
            stamp := Some before;
            detail_cache := Some (match P.cache_string resolved with
              | Ok json -> json | Error e -> failwith e);
            diagnostics := List.filter_map (fun (d : P.document) ->
              match d.bibliography with Ok _ -> None
              | Error e -> Some (d.artifact.file ^ ": " ^ e))
              resolved.documents;
            Ok (P.bibliography resolved)
          end
    with
    | A.Parse_error e -> Error (A.error_to_string e)
    | Eio.Io _ as ex -> Error (Printexc.to_string ex) in
    let changed = C.Metadata.publish ?source_stamp:!stamp
      ?detail_cache:!detail_cache
      ~diagnostics:!diagnostics catalog candidate ~version:P.version result in
    let status = if not changed then (incr superseded; Superseded) else
      match result with
      | Ok _ when !diagnostics <> [] -> incr partial; Partial !diagnostics
      | Ok _ -> incr updated; Updated
      | Error e -> incr unavailable; Unavailable e in
    Option.iter (fun session ->
      let outcome = match status with Updated -> "updated" | Partial _ -> "partial"
        | Unavailable _ -> "unavailable" | Superseded -> "superseded" in
      Taposaur_audit.record session ~action:"catalog.metadata.resolve"
        ~subject:(Uid.to_string row.uid)
        ~details:["outcome",outcome;"diagnostics",string_of_int (List.length !diagnostics)] ()) audit;
    incr examined; progress row.uid status in
  let rec uid_loop () =
    let remaining = limit - !examined in
    if remaining > 0 then
      let rows = C.Metadata.pending ~force ~retry_errors ~need_details:true
        ?uid ~after:!cursor
        ~limit:(min 64 remaining) ~version:P.version catalog in
      match List.rev rows with
      | [] -> ()
      | last :: _ ->
          Eio.Fiber.List.iter ~max_fibers:jobs process rows;
          cursor := Uid.to_string (C.Metadata.index last).uid;
          uid_loop () in
  let rec shard_loop shard after_row =
    let remaining = limit - !examined in
    if remaining > 0 then
      let rows = C.Metadata.pending ~force ~retry_errors ~need_details:true
        ~shard ~after_row ~limit:(min 512 remaining) ~version:P.version catalog in
      match List.rev rows with
      | [] -> ()
      | last :: _ ->
          Eio.Fiber.List.iter ~max_fibers:jobs process rows;
          shard_loop shard (C.Metadata.row_id last) in
  let rec directories after =
    if !examined < limit then
      let shards = C.Shard.paths ~after ~limit:256 catalog in
      List.iter (fun shard ->
        if !examined < limit then shard_loop shard 0L) shards;
      match List.rev shards with
      | [] -> ()
      | last :: _ -> directories last in
  (match order, uid with
   | Uid, _ | _, Some _ -> uid_loop ()
   | Directory, None -> directories "");
  { examined = !examined; updated = !updated; unavailable = !unavailable;
    superseded = !superseded; partial = !partial }

type freshness = { checked : int; stale : int; next : string option }
let check ?(limit = 1000) ?after catalog archive =
  let rows = C.Metadata.pending ~force:true ?after ~limit
    ~version:P.version catalog in
  let stale = ref 0 in
  List.iter (fun candidate ->
    let current = try Option.map A.fingerprint
      (A.find archive (C.Metadata.index candidate).uid)
      with A.Parse_error _ | Eio.Io _ -> None in
    if current = None || current <> C.Metadata.source_stamp candidate then
      if C.Metadata.invalidate catalog candidate then incr stale) rows;
  let next = if List.length rows < limit then None else
    Option.map (fun c -> Uid.to_string (C.Metadata.index c).uid)
      (List.nth_opt rows (List.length rows - 1)) in
  {checked = List.length rows; stale = !stale; next}
