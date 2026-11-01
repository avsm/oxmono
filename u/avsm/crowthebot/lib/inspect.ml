open Persistence

let sections =
  [
    "pending";
    "reminders";
    "runs";
    "tools";
    "traces";
    "context";
    "summaries";
    "feeds";
    "subscriptions";
    "polls";
    "locations";
    "caldav";
    "caldav-collections";
    "caldav-pending";
    "caldav-versions";
    "caldav-deletions";
    "caldav-agendas";
    "caldav-occurrences";
    "calendars";
    "calendar-cursors";
    "calendar-versions";
    "calendar-deletions";
    "calendar-receipts";
    "calendar-blobs";
    "email-results";
    "memory";
    "notes";
  ]

let string value = Jsont.String (value, Jsont.Meta.none)
let number value = Jsont.Number (value, Jsont.Meta.none)
let null = Jsont.Null ((), Jsont.Meta.none)

let object_ fields =
  Jsont.Object
    ( List.map (fun (name, value) -> ((name, Jsont.Meta.none), value)) fields,
      Jsont.Meta.none )

let array values = Jsont.Array (values, Jsont.Meta.none)

let value = function
  | Sqlite3.Data.TEXT s -> string s
  | INT i -> number (Int64.to_float i)
  | FLOAT f -> number f
  | NULL | NONE -> null
  | BLOB _ -> string "[binary data]"

let read db ~section ~after ~limit =
  if after < 0 || limit < 1 || limit > 100 then
    invalid_arg "Use --after >= 0 and --limit between 1 and 100";
  let table, predicate =
    match section with
    | "pending" -> ("reminders", " AND state='active'")
    | "reminders" -> ("reminders", "")
    | "runs" -> ("reminder_runs", "")
    | "tools" -> ("tool_uses", "")
    | "traces" -> ("model_traces", "")
    | "context" -> ("room_observations", "")
    | "summaries" -> ("conversation_summaries", " AND body<>''")
    | "feeds" -> ("feeds_sources", "")
    | "subscriptions" -> ("feeds_subscriptions", "")
    | "polls" -> ("feeds_memberships", "")
    | "locations" -> ("locations_people", "")
    | "caldav" -> ("caldav_mirrors", "")
    | "caldav-collections" -> ("caldav_collections", "")
    | "caldav-pending" -> ("caldav_pending", "")
    | "caldav-versions" -> ("caldav_versions", "")
    | "caldav-deletions" -> ("caldav_deletions", "")
    | "caldav-agendas" -> ("caldav_agendas", "")
    | "caldav-occurrences" -> ("caldav_occurrences", "")
    | "calendars" -> ("calendar_mirrors", "")
    | "calendar-cursors" -> ("calendar_cursors", "")
    | "calendar-versions" -> ("calendar_versions", "")
    | "calendar-deletions" -> ("calendar_deletions", "")
    | "calendar-receipts" -> ("calendar_receipts", "")
    | "calendar-blobs" -> ("calendar_blobs", "")
    | "email-results" -> ("email_results", "")
    | "memory" -> ("facts", "")
    | "notes" -> ("daily_notes", "")
    | _ -> invalid_arg "Unknown inspection section"
  in
  sql db "PRAGMA query_only=ON; BEGIN";
  Fun.protect ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> sql db "ROLLBACK"))
  @@ fun () ->
  let exists table =
    rows db "SELECT 1 FROM sqlite_schema WHERE type='table' AND name=?"
      [ text table ]
      (fun _ -> ())
    <> []
  in
  let items =
    if not (exists table) then []
    else
      let columns =
        if section = "email-results" then
          "rowid AS \
           _cursor,id,connection,mode,operation,actor,room,event,observed,length(data) \
           AS bytes"
        else "rowid AS _cursor,*"
      in
      rows db
        ("SELECT " ^ columns ^ " FROM " ^ table ^ " WHERE rowid>?" ^ predicate
       ^ " ORDER BY rowid LIMIT ?")
        [ integer after; integer (limit + 1) ]
        (fun stmt ->
          let id = Sqlite3.column_int stmt 0 in
          let fields =
            List.init (Sqlite3.column_count stmt) (fun i ->
                (Sqlite3.column_name stmt i, value (Sqlite3.column stmt i)))
          in
          (id, object_ fields))
  in
  let more = List.length items > limit in
  let rec take n = function
    | _ when n = 0 -> []
    | [] -> []
    | item :: rest -> item :: take (n - 1) rest
  in
  let items = take limit items in
  let count table where =
    if not (exists table) then number 0.
    else
      match
        rows db
          ("SELECT count(*) FROM " ^ table ^ " WHERE " ^ where)
          []
          (fun stmt -> value (Sqlite3.column stmt 0))
      with
      | [ n ] -> n
      | _ -> assert false
  in
  object_
    [
      ("section", string section);
      ("items", array (List.map snd items));
      ( "next_after",
        if more then number (float_of_int (fst (List.hd (List.rev items))))
        else null );
      ( "outstanding",
        object_
          [
            ("reminders", count "reminders" "state='active'");
            ("tools_running", count "tool_uses" "status='running'");
            ("model_requests_running", count "model_traces" "status='running'");
            ( "reminder_failures",
              count "reminder_runs"
                "status IN ('error','interrupted','cancelled')" );
          ] );
    ]

let run ~env ~sw ~profile ~section ~after ~limit =
  let dir = Profile.directory env profile in
  let path = Eio.Path.(dir / "crowthebot.sqlite3") in
  Profile.private_file (Eio.Path.native_exn path);
  let db =
    Sqlite3_eio.open_path ~sw ~mode:`READONLY ~uri:false ~busy_timeout:5000 path
  in
  let result = read db ~section ~after ~limit in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json result with
  | Ok json -> print_endline json
  | Error _ -> failwith "Cannot encode inspection result"
