open Taposaur
open Db
module S = Sqlite3

type stamp = { size : int64; mtime : float }

let unchanged t ~shard stamp = run t (fun db ->
  with_stmt db
    "SELECT size,mtime FROM shards WHERE path=? AND NOT EXISTS \
     (SELECT 1 FROM import_errors WHERE path=shards.path)" (fun s ->
    bind s [text shard];
    S.step s = S.Rc.ROW && S.column_int64 s 0 = stamp.size &&
      S.column_double s 1 = stamp.mtime))

let columns =
  "uid,shard,scheme,title,authors,journal,publisher,year,abstract,\
   pdf,xml,tei,s2orc,json"

let data shard (e : Index.t) =
  let json = match Index.to_string e with Ok s -> s | Error e -> failwith e in
  let author (a : Author.t) =
    String.concat " " (List.filter ((<>) "") [opt_text a.given; a.family]) in
  [ text (Uid.to_string e.uid); text shard;
    text (Uid.Scheme.to_string (Uid.scheme e.uid));
    text (opt_text e.title);
    text (String.concat "; " (List.map author e.authors));
    text (opt_text e.journal); text (opt_text e.publisher);
    (match e.year with None -> S.Data.NULL | Some n -> int n);
    text (opt_text e.abstract); int (Bool.to_int (e.pdf_path <> ""));
    int (Bool.to_int e.has_xml); int (Bool.to_int (e.tei_path <> ""));
    int (Bool.to_int e.has_s2orc); text json ]

let replace t ~shard {size; mtime} read =
  Db.exclusive t @@ fun sql ->
  sql (fun db ->
    exec db "DROP TABLE IF EXISTS temp.incoming";
    exec db ("CREATE TEMP TABLE incoming AS SELECT " ^ columns ^
      " FROM papers WHERE 0");
    exec db "CREATE UNIQUE INDEX incoming_uid ON incoming(uid)");
  let batch = ref [] and count = ref 0 and total = ref 0 in
  let flush () =
    let rows = List.rev !batch in
    sql (fun db -> with_stmt db
      ("INSERT INTO incoming(" ^ columns ^
        ") VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
      (fun s -> List.iter (fun row ->
        bind s row; done_ s; check (S.reset s)) rows));
    batch := []; count := 0
  in

  read (fun e ->
    if Layout.index e.Index.uid <> shard then
      invalid_arg (Uid.to_string e.uid ^ " belongs in another shard");
    batch := data shard e :: !batch;
    incr count; incr total;
    if !count = 512 then flush ());
  if !count > 0 then flush ();
  Eio.Cancel.protect (fun () -> sql (fun db -> transaction db (fun () ->
    with_stmt db "DELETE FROM papers WHERE shard=?" (fun s ->
      bind s [text shard]; done_ s);
    exec db "UPDATE settings SET value=value+1 WHERE key='generation'";
    exec db ("INSERT INTO papers(" ^ columns ^ ",generation) SELECT " ^
      columns ^ ", (SELECT value FROM settings WHERE key='generation')" ^
      " FROM incoming");
    with_stmt db {|
INSERT OR REPLACE INTO shards
 SELECT ?,?,?,count(*),coalesce(sum(pdf),0),coalesce(sum(xml),0),
 coalesce(sum(tei),0),coalesce(sum(s2orc),0),? FROM incoming|} (fun s ->
      bind s [text shard; S.Data.INT size; S.Data.FLOAT mtime;
        S.Data.FLOAT (Unix.gettimeofday ())]; done_ s);
    with_stmt db "DELETE FROM import_errors WHERE path=?" (fun s ->
      bind s [text shard]; done_ s))));
  !total

let record_error t ~shard message = run t (fun db ->
  with_stmt db "INSERT OR REPLACE INTO import_errors VALUES(?,?,?)" (fun s ->
    bind s [text shard; text message; S.Data.FLOAT (Unix.gettimeofday ())];
    done_ s))

let set_discovered t count =
  if count < 0 then invalid_arg "Shard count must be nonnegative";
  run t (fun db -> with_stmt db
    "INSERT OR REPLACE INTO settings VALUES('discovered_shards',?)" (fun s ->
      bind s [int count]; done_ s))

let errors t = run t (fun db ->
  with_stmt db
    "SELECT path,message FROM import_errors ORDER BY at DESC LIMIT 100"
    (fun s ->
      let rec loop acc = match S.step s with
        | S.Rc.ROW -> loop ((S.column_text s 0, S.column_text s 1) :: acc)
        | S.Rc.DONE -> List.rev acc | rc -> check rc; assert false
      in loop []))
