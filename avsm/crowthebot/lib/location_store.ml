open Persistence

type t = {
  db : Sqlite3_eio.t;
  mutex : Eio.Mutex.t;
  admin : string;
  now : unit -> float;
  timestamp : float -> string;
}

type point = {
  latitude : float;
  longitude : float;
  accuracy : float option;
  recorded_at : float;
}

type link = {
  person : string;
  connection : string;
  user : string;
  device : string;
  actor : string;
  room : string;
  event : string;
  attached_at : string;
  point : point option;
  checked_at : string option;
}

let create ~db ~mutex ~admin ~now ~timestamp =
  { db; mutex; admin; now; timestamp }

let init db =
  sql db
    {|
CREATE TABLE IF NOT EXISTS tool_schemas(name TEXT PRIMARY KEY, version INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS locations_people(
 person TEXT PRIMARY KEY, connection TEXT NOT NULL, user TEXT NOT NULL,
 device TEXT NOT NULL, actor TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
 attached_at TEXT NOT NULL, latitude REAL, longitude REAL, accuracy REAL,
 recorded_at REAL, checked_at TEXT);
INSERT OR IGNORE INTO tool_schemas VALUES('locations',1);
|};
  if
    rows db "SELECT version FROM tool_schemas WHERE name='locations'" []
      (fun s -> Sqlite3.column_int s 0)
    <> [ 1 ]
  then invalid_arg "Unsupported locations tool schema."

let access t actor f =
  locked t.mutex (fun () ->
      if
        actor <> t.admin
        && rows t.db
             "SELECT 1 FROM people WHERE user=? AND role='friend' AND allowed=1"
             [ text actor ]
             (fun _ -> ())
           = []
      then invalid_arg "Location tools require the admin or an allowed friend.";
      f ())

let authorize t ~actor = access t actor Fun.id

let validate_label value =
  if
    String.trim value <> value
    || value = ""
    || String.length value > 256
    || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value
  then
    invalid_arg
      "Location labels must be 1 to 256 bytes without control characters or \
       surrounding spaces."

let valid_point ~now p =
  List.for_all Float.is_finite [ p.latitude; p.longitude; p.recorded_at ]
  && p.latitude >= -90. && p.latitude <= 90. && p.longitude >= -180.
  && p.longitude <= 180. && p.recorded_at >= 0.
  && p.recorded_at <= now +. 300.
  && Option.fold ~none:true
       ~some:(fun a -> Float.is_finite a && a >= 0.)
       p.accuracy

let float_opt s i =
  match Sqlite3.column s i with
  | Sqlite3.Data.NULL -> None
  | _ -> Some (Sqlite3.column_double s i)

let columns =
  "person,connection,user,device,actor,room,event,attached_at,latitude,longitude,accuracy,recorded_at,checked_at"

let row s =
  {
    person = Sqlite3.column_text s 0;
    connection = Sqlite3.column_text s 1;
    user = Sqlite3.column_text s 2;
    device = Sqlite3.column_text s 3;
    actor = Sqlite3.column_text s 4;
    room = Sqlite3.column_text s 5;
    event = Sqlite3.column_text s 6;
    attached_at = Sqlite3.column_text s 7;
    point =
      Option.map
        (fun recorded_at ->
          {
            latitude = Sqlite3.column_double s 8;
            longitude = Sqlite3.column_double s 9;
            accuracy = float_opt s 10;
            recorded_at;
          })
        (float_opt s 11);
    checked_at = string_opt s 12;
  }

let find t person =
  rows t.db
    ("SELECT " ^ columns ^ " FROM locations_people WHERE person=?")
    [ text person ]
    row
  |> List.to_seq |> Seq.uncons |> Option.map fst

let get t ~actor ~person =
  validate_label person;
  access t actor (fun () -> find t person)

let list t ~actor ~after =
  if after <> "" then validate_label after;
  access t actor (fun () ->
      rows t.db
        ("SELECT " ^ columns
       ^ " FROM locations_people WHERE person>? ORDER BY person LIMIT 21")
        [ text after ]
        row)

let attach t ~actor ~room ~event ~person ~connection ~user ~device =
  List.iter validate_label [ person; connection; user; device ];
  access t actor (fun () ->
      execute t.db
        {|
INSERT INTO locations_people(person,connection,user,device,actor,room,event,attached_at)
VALUES(?,?,?,?,?,?,?,?) ON CONFLICT(person) DO UPDATE SET
connection=excluded.connection,user=excluded.user,device=excluded.device,
actor=excluded.actor,room=excluded.room,event=excluded.event,attached_at=excluded.attached_at,
latitude=NULL,longitude=NULL,accuracy=NULL,recorded_at=NULL,checked_at=NULL
|}
        [
          text person;
          text connection;
          text user;
          text device;
          text actor;
          text room;
          text event;
          text (t.timestamp (t.now ()));
        ];
      Option.get (find t person))

let update t ~actor (link : link) point =
  Option.iter
    (fun p ->
      if not (valid_point ~now:(t.now ()) p) then
        invalid_arg "Invalid reported location.")
    point;
  access t actor (fun () ->
      let current =
        match find t link.person with
        | Some current
          when current.connection = link.connection
               && current.user = link.user
               && current.device = link.device
               && current.attached_at = link.attached_at
               && current.event = link.event ->
            current
        | _ -> invalid_arg "Location link changed while polling."
      in
      let point =
        match (current.point, point) with
        | Some old, Some fresh when old.recorded_at > fresh.recorded_at ->
            Some old
        | old, None -> old
        | _, point -> point
      in
      let field f = optional (fun p -> Sqlite3.Data.FLOAT (f p)) point in
      execute t.db
        {|
UPDATE locations_people SET latitude=?,longitude=?,accuracy=?,recorded_at=?,checked_at=? WHERE person=?
|}
        [
          field (fun p -> p.latitude);
          field (fun p -> p.longitude);
          optional
            (fun a -> Sqlite3.Data.FLOAT a)
            (Option.bind point (fun p -> p.accuracy));
          field (fun p -> p.recorded_at);
          text (t.timestamp (t.now ()));
          text link.person;
        ];
      Option.get (find t link.person))

let detach t ~actor ~person =
  validate_label person;
  access t actor (fun () ->
      let exists = find t person <> None in
      execute t.db "DELETE FROM locations_people WHERE person=?" [ text person ];
      exists)
