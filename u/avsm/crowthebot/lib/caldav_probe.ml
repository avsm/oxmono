module Source = Caldav_source
module Log = Diagnostics.Log

let check ~name open_source =
  let stage = ref "configuration" in
  try
    let source = open_source () in
    stage := "authentication";
    ignore (Source.identity source);
    Log.info (fun m -> m "CalDAV probe authenticated connection=%S" name);
    stage := "discovery";
    let collections = Source.discover source in
    Log.info (fun m ->
        m "CalDAV probe discovered connection=%S calendars=%d" name
          (List.length collections));
    let reads = ref 0 and incremental = ref 0 in
    List.iteri
      (fun index (collection : Source.collection) ->
        if collection.sync then incr incremental;
        stage :=
          Printf.sprintf "calendar %d %s" (index + 1)
            (if collection.sync then "sync report" else "ETag inventory");
        let page = Source.next source collection ~token:None in
        let sample_bytes = ref 0 in
        (match
           List.find_opt
             (fun (change : Source.change) -> not change.removed)
             page.changes
         with
        | None -> ()
        | Some change -> (
            stage := Printf.sprintf "calendar %d sample read" (index + 1);
            match Source.get source ~collection:collection.href change.href with
            | None ->
                Log.info (fun m ->
                    m
                      "CalDAV probe sample disappeared connection=%S \
                       calendar=%d"
                      name (index + 1))
            | Some item ->
                if not item.parsed then
                  invalid_arg "The sample is not valid iCalendar data.";
                incr reads;
                sample_bytes := String.length item.raw));
        Log.info (fun m ->
            m
              "CalDAV probe checked connection=%S calendar=%d mode=%s \
               changes=%d more=%b sample_bytes=%d"
              name (index + 1)
              (if collection.sync then "sync" else "inventory")
              (List.length page.changes) page.more !sample_bytes))
      collections;
    Ok
      (Printf.sprintf "calendars=%d incremental=%d inventory=%d samples_read=%d"
         (List.length collections) !incremental
         (List.length collections - !incremental)
         !reads)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (Printf.sprintf "%s: %s" !stage (Source.error exn))

let run ~emit sources =
  if sources = [] then begin
    emit "CalDAV: skipped (no configured connections).";
    true
  end
  else
    List.fold_left
      (fun success (name, open_source) ->
        Secret_store.validate_name name;
        Log.info (fun m -> m "CalDAV probe started connection=%S" name);
        match check ~name open_source with
        | Ok detail ->
            emit (Printf.sprintf "CalDAV %S: OK (%s)" name detail);
            success
        | Error detail ->
            emit (Printf.sprintf "CalDAV %S: FAILED (%s)" name detail);
            false)
      true sources
