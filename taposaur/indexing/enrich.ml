open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module P = Taposaur_paper

type result = Updated | Unavailable of string | Superseded
type stats = {
  examined : int; updated : int; unavailable : int; superseded : int;
}

let run ?(force = false) ?(limit = max_int) ?uid ?(jobs = 8)
    ?(progress = fun _ _ -> ()) catalog archive =
  if limit < 0 then invalid_arg "limit must be nonnegative";
  if jobs < 1 || jobs > 64 then invalid_arg "jobs must be 1..64";
  let examined = ref 0 and updated = ref 0 and unavailable = ref 0 in
  let superseded = ref 0 and cursor = ref "" in
  let process candidate =
    let row = C.Metadata.index candidate in
    let result = try match A.find archive row.uid with
      | None -> Error "Paper is absent from the archive"
      | Some paper ->
          Ok (P.bibliography (Taposaur_reader.load ~index:row paper))
    with
    | A.Parse_error e -> Error (A.error_to_string e)
    | Eio.Io _ as ex -> Error (Printexc.to_string ex) in
    let changed =
      C.Metadata.publish catalog candidate ~version:P.version result in
    let status = if not changed then (incr superseded; Superseded) else
      match result with
      | Ok _ -> incr updated; Updated
      | Error e -> incr unavailable; Unavailable e in
    incr examined; progress row.uid status in
  let rec loop () =
    let remaining = limit - !examined in
    if remaining > 0 then
      let rows = C.Metadata.pending ~force ?uid ~after:!cursor
        ~limit:(min 64 remaining) ~version:P.version catalog in
      match List.rev rows with
      | [] -> ()
      | last :: _ ->
          Eio.Fiber.List.iter ~max_fibers:jobs process rows;
          cursor := Uid.to_string (C.Metadata.index last).uid;
          loop () in
  loop ();
  { examined = !examined; updated = !updated; unavailable = !unavailable;
    superseded = !superseded }
