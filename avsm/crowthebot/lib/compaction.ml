open Persistence

type scope = Thread of { room : string; user : string } | Room of string
type t = { db : Sqlite3_eio.t; mutex : Eio.Mutex.t; now : unit -> string }

let init db =
  sql db
    {|
CREATE TABLE IF NOT EXISTS conversation_summaries (
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 scope TEXT NOT NULL CHECK(scope IN ('thread','room')),
 room TEXT NOT NULL, user TEXT NOT NULL,
 revision INTEGER NOT NULL DEFAULT 0, through_id INTEGER NOT NULL DEFAULT 0,
 updated_at TEXT NOT NULL DEFAULT '', body TEXT NOT NULL DEFAULT '',
 first_event TEXT NOT NULL DEFAULT '', last_event TEXT NOT NULL DEFAULT '',
 UNIQUE(scope,room,user));
|}

let create ~db ~mutex ~now = { db; mutex; now }

let key = function
  | Thread { room; user } -> [ text "thread"; text room; text user ]
  | Room room -> [ text "room"; text room; text "" ]

let touch db scope =
  execute db
    "INSERT INTO conversation_summaries(scope,room,user,revision) VALUES \
     (?,?,?,1) ON CONFLICT(scope,room,user) DO UPDATE SET revision=revision+1"
    (key scope)

let clear db ~room ~user =
  let predicate, args =
    match room with
    | None -> ("scope='room' OR user=?", [ text user ])
    | Some room ->
        ("room=? AND (scope='room' OR user=?)", [ text room; text user ])
  in
  (* A room summary can still contain a speaker whose raw rows have expired. *)
  execute db
    ("UPDATE conversation_summaries SET \
      revision=revision+1,through_id=0,updated_at='',body='',first_event='',last_event='' \
      WHERE " ^ predicate)
    args

type summary = {
  revision : int;
  through_id : int;
  updated_at : string;
  body : string;
  first_event : string;
  last_event : string;
}

let summary t scope =
  match
    rows t.db
      "SELECT revision,through_id,updated_at,body,first_event,last_event FROM \
       conversation_summaries WHERE scope=? AND room=? AND user=?"
      (key scope) (fun s ->
        {
          revision = Sqlite3.column_int s 0;
          through_id = Sqlite3.column_int s 1;
          updated_at = Sqlite3.column_text s 2;
          body = Sqlite3.column_text s 3;
          first_event = Sqlite3.column_text s 4;
          last_event = Sqlite3.column_text s 5;
        })
  with
  | [ value ] -> value
  | _ ->
      {
        revision = 0;
        through_id = 0;
        updated_at = "";
        body = "";
        first_event = "";
        last_event = "";
      }

let object_ fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> ((name, Jsont.Meta.none), value)) fields)

let encode json = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json)

let render s body =
  let open Jsont.Json in
  object_
    [
      ("summary", string body);
      ("updated_at", string s.updated_at);
      ("through_id", int s.through_id);
      ("first_event", string s.first_event);
      ("last_event", string s.last_event);
    ]

let context t scope ~bytes =
  locked t.mutex @@ fun () ->
  let s = summary t scope in
  let rec fit body =
    if body = "" then None
    else
      let json =
        encode
          (object_
             [
               ("summary", Jsont.Json.string body);
               ("updated_at", Jsont.Json.string s.updated_at);
             ])
      in
      if String.length json <= bytes then Some json
      else if String.length body < 32 then None
      else fit (Plugin.clip ~bytes:(String.length body / 2) body)
  in
  fit s.body

type entry = {
  id : int;
  event : string;
  role : string;
  size : int;
  json : Jsont.json;
}

let entries t scope =
  let query, args =
    match scope with
    | Thread { room; user } ->
        ( "SELECT id,event,role,created_at,body,source_event,'' FROM history \
           WHERE room=? AND user=? ORDER BY id",
          [ text room; text user ] )
    | Room room ->
        ( "SELECT id,event,sender,observed_at,body,event,note FROM \
           room_observations WHERE room=? ORDER BY id",
          [ text room ] )
  in
  rows t.db query args (fun s ->
      let str i = Sqlite3.column_text s i in
      let id = Sqlite3.column_int s 0 and event = str 1 and role = str 2 in
      let body = str 4 and note = str 6 in
      let role_key =
        match scope with Thread _ -> "role" | Room _ -> "sender"
      in
      {
        id;
        event;
        role;
        size = String.length body + String.length note;
        json =
          object_
            [
              ("id", Jsont.Json.int id);
              ("event", Jsont.Json.string event);
              (role_key, Jsont.Json.string role);
              ("at", Jsont.Json.string (str 3));
              ("message", Jsont.Json.string body);
              ("observation", Jsont.Json.string note);
              ("source_event", Jsont.Json.string (str 5));
            ];
      })

let groups scope entries =
  match scope with
  | Room _ -> List.map (fun e -> [ e ]) entries
  | Thread _ ->
      let rec loop acc = function
        | a :: b :: rest when a.role = "user" && b.role = "assistant" ->
            loop ([ a; b ] :: acc) rest
        | a :: rest -> loop ([ a ] :: acc) rest
        | [] -> List.rev acc
      in
      loop [] entries

type plan = {
  scope : scope;
  previous : summary;
  through : entry;
  first_event : string;
  input : string;
  limit : int;
}

let input p = p.input
let limit p = p.limit

let prepare t scope ~max_messages ~max_bytes ~incoming_messages ~incoming_bytes
    =
  if
    max_messages < 2 || max_bytes < 1024 || incoming_messages < 0
    || incoming_bytes < 0
  then invalid_arg "Invalid compaction bounds.";
  locked t.mutex @@ fun () ->
  let previous = summary t scope and entries = entries t scope in
  let bytes =
    List.fold_left (fun n e -> n + e.size) (String.length previous.body) entries
  in
  if
    entries = []
    || List.length entries + incoming_messages < max_messages * 3 / 4
       && bytes + incoming_bytes < max_bytes * 3 / 4
  then None
  else
    let limit = min 6000 (max_bytes / 4) in
    let keep_messages = min 8 (max 0 (max_messages - incoming_messages))
    and keep_bytes = max 0 ((max_bytes * 3 / 4) - limit - incoming_bytes) in
    let rec older count bytes = function
      | [] -> []
      | group :: rest as remaining ->
          let count = count + List.length group
          and bytes = List.fold_left (fun n e -> n + e.size) bytes group in
          if count > keep_messages || bytes > keep_bytes then List.rev remaining
          else older count bytes rest
    in
    let candidates = older 0 0 (List.rev (groups scope entries)) in
    let payload entries =
      let scope_fields =
        match scope with
        | Thread { room; user } ->
            [
              ("scope", Jsont.Json.string "thread");
              ("room", Jsont.Json.string room);
              ("user", Jsont.Json.string user);
            ]
        | Room room ->
            [
              ("scope", Jsont.Json.string "room");
              ("room", Jsont.Json.string room);
            ]
      in
      let previous_body =
        if String.length previous.body <= limit then previous.body
        else Plugin.clip ~bytes:(limit - 12) previous.body
      in
      encode
        (object_
           (scope_fields
           @ [
               ("previous", render previous previous_body);
               ("messages", Jsont.Json.list (List.map (fun e -> e.json) entries));
             ]))
    in
    (* Bound the encoded request too. JSON escaping can expand small bodies. *)
    let rec batch selected = function
      | [] -> selected
      | group :: rest ->
          let candidate = selected @ group in
          if String.length (payload candidate) > max_bytes then selected
          else batch candidate rest
    in
    match batch [] candidates with
    | [] -> None
    | first :: _ as selected ->
        Some
          {
            scope;
            previous;
            through = List.hd (List.rev selected);
            first_event =
              (if previous.first_event = "" then first.event
               else previous.first_event);
            input = payload selected;
            limit;
          }

let commit t plan ~body =
  if String.trim body = "" || String.length body > plan.limit then
    invalid_arg "Invalid compaction summary.";
  locked t.mutex @@ fun () ->
  transaction t.db @@ fun () ->
  if (summary t plan.scope).revision <> plan.previous.revision then false
  else begin
    touch t.db plan.scope;
    execute t.db
      "UPDATE conversation_summaries SET \
       through_id=?,updated_at=?,body=?,first_event=?,last_event=? WHERE \
       scope=? AND room=? AND user=?"
      ([
         integer plan.through.id;
         text (t.now ());
         text body;
         text plan.first_event;
         text plan.through.event;
       ]
      @ key plan.scope);
    (match plan.scope with
    | Thread { room; user } ->
        execute t.db "DELETE FROM history WHERE room=? AND user=? AND id<=?"
          [ text room; text user; integer plan.through.id ]
    | Room room ->
        execute t.db "DELETE FROM room_observations WHERE room=? AND id<=?"
          [ text room; integer plan.through.id ]);
    true
  end
