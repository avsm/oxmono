(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** SQLite-backed HTTP request logging *)

open Base

type t = {
  db : Sqlite3_eio.t;
  reader : Sqlite3_eio.t;
}

let schema_sql = {|
CREATE TABLE IF NOT EXISTS requests (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp REAL NOT NULL,
  remote_addr TEXT NOT NULL,
  forwarded_for TEXT,
  forwarded_proto TEXT,
  method TEXT NOT NULL,
  target TEXT NOT NULL,
  path TEXT NOT NULL,
  host TEXT,
  user_agent TEXT,
  referer TEXT,
  accept TEXT,
  request_headers TEXT,
  status_code INTEGER NOT NULL,
  response_content_type TEXT,
  response_body_size INTEGER NOT NULL,
  cache_status TEXT,
  duration_us INTEGER NOT NULL
)|}

let index_sql = [
  "CREATE INDEX IF NOT EXISTS idx_requests_timestamp ON requests(timestamp)";
  "CREATE INDEX IF NOT EXISTS idx_requests_path ON requests(path)";
  "CREATE INDEX IF NOT EXISTS idx_requests_status_ts \
   ON requests(status_code, timestamp)";
  (* Covers every column the overview aggregates read, so those queries
     never touch the table itself, whose rows are dominated by the
     request_headers JSON. *)
  "CREATE INDEX IF NOT EXISTS idx_requests_metrics \
   ON requests(timestamp, status_code, cache_status, duration_us, \
   response_body_size)";
]

(* status_code has five distinct values and cache_status three, so an
   index on either narrows nothing. The planner picked them over the
   timestamp range anyway and then read a million rows from the table,
   which is why they are dropped rather than merely unused. *)
let drop_index_sql = [
  "DROP INDEX IF EXISTS idx_requests_status";
  "DROP INDEX IF EXISTS idx_requests_cache";
]

let insert_sql = {|
INSERT INTO requests (
  timestamp, remote_addr, forwarded_for, forwarded_proto,
  method, target, path, host, user_agent, referer, accept,
  request_headers, status_code, response_content_type,
  response_body_size, cache_status, duration_us
) VALUES (
  ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15, ?16, ?17
)|}

(* Jsont codec: header pair as ["name","value"], headers as list of pairs *)
let header_pair_jsont : (string * string) Jsont.t =
  Jsont.map ~kind:"header_pair"
    ~dec:(function [n; v] -> (n, v) | _ -> ("", ""))
    ~enc:(fun (n, v) -> [n; v])
    (Jsont.list Jsont.string)

let headers_jsont : (string * string) list Jsont.t =
  Jsont.list header_pair_jsont

let encode_headers_json headers =
  match Jsont_bytesrw.encode_string headers_jsont headers with
  | Ok s -> s
  | Error _ -> "[]"

let bind_text_opt stmt pos v =
  Sqlite3.Rc.check (Sqlite3.bind stmt pos (Sqlite3.Data.opt_text v))

let reader t = t.reader

let create ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  Sqlite3.Rc.check (Sqlite3_eio.exec db "PRAGMA journal_mode=WAL");
  Sqlite3.Rc.check (Sqlite3_eio.exec db "PRAGMA synchronous=NORMAL");
  Sqlite3.Rc.check (Sqlite3_eio.exec db schema_sql);
  List.iter drop_index_sql ~f:(fun sql ->
    Sqlite3.Rc.check (Sqlite3_eio.exec db sql)
  );
  List.iter index_sql ~f:(fun sql ->
    Sqlite3.Rc.check (Sqlite3_eio.exec db sql)
  );
  (* Give the query planner size and cardinality figures. Without them it
     estimates a range constraint at a quarter of the table and picks
     whatever equality index it can find instead. *)
  Sqlite3.Rc.check (Sqlite3_eio.exec db "ANALYZE");
  (* Analytics run on their own read-only connection. SQLite serializes
     every call on a connection, and an aggregate does its whole scan
     inside one step, so a dashboard query sharing this handle would hold
     the mutex for seconds and stall the insert of every request that
     arrived meanwhile. WAL lets a second connection read without
     blocking the writer at all. *)
  let reader =
    Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path
  in
  { db; reader }

(* The event carries the request fields the parser did not consume, in
   arrival order, so the named columns are derived from it here. A name is
   spelled as the parser gives it, hence the case-insensitive match. *)
let header headers name =
  List.find_map headers ~f:(fun (k, v) ->
    if String.Caseless.equal k name then Some v else None)

let log_request t ~timestamp (event : Proffer_httpz.event) =
  let headers = event.request_headers in
  (* Prepare a fresh statement per insert. Concurrent fibers from keep-alive
     connections would race on a shared prepared statement. *)
  let stmt = Sqlite3_eio.prepare t.db insert_sql in
  Sqlite3.Rc.check (Sqlite3.bind_double stmt 1 timestamp);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 2 event.remote_addr);
  bind_text_opt stmt 3 (header headers "x-forwarded-for");
  bind_text_opt stmt 4 (header headers "x-forwarded-proto");
  Sqlite3.Rc.check
    (Sqlite3.bind_text stmt 5 (Proffer.Method.to_string event.meth));
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 6 event.target);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 7 event.path);
  bind_text_opt stmt 8 (header headers "host");
  bind_text_opt stmt 9 (header headers "user-agent");
  bind_text_opt stmt 10 (header headers "referer");
  bind_text_opt stmt 11 (header headers "accept");
  Sqlite3.Rc.check
    (Sqlite3.bind_text stmt 12 (encode_headers_json headers));
  Sqlite3.Rc.check
    (Sqlite3.bind_int stmt 13 (Proffer.Status.code event.status));
  bind_text_opt stmt 14 event.response_content_type;
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 15 event.body_size);
  bind_text_opt stmt 16 event.cache_status;
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 17 event.duration_us);
  let rc = Sqlite3_eio.step t.db stmt in
  ignore (Sqlite3_eio.finalize t.db stmt : Sqlite3.Rc.t);
  match rc with
  | Sqlite3.Rc.DONE -> ()
  | rc -> Sqlite3.Rc.check rc
