(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** SQLite-backed HTTP request logging *)

open Base

module F64 = Stdlib_upstream_compatible.Float_u

type t = {
  db : Sqlite3_eio.t;
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
  "CREATE INDEX IF NOT EXISTS idx_requests_status ON requests(status_code)";
  "CREATE INDEX IF NOT EXISTS idx_requests_cache ON requests(cache_status)";
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

(* Copy a local string to a global string *)
let globalize (local_ s : string) : string =
  let len = String.length s in
  let dst = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst i (String.unsafe_get s i)
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst

let globalize_or_null (local_ v : string or_null) : string option =
  match v with
  | This s -> Some (globalize s)
  | Null -> None

let rec globalize_pairs (local_ l : (string * string) list) : (string * string) list =
  match l with
  | [] -> []
  | (k, v) :: rest -> (globalize k, globalize v) :: globalize_pairs rest

let bind_text_opt stmt pos v =
  Sqlite3.Rc.check (Sqlite3.bind stmt pos (Sqlite3.Data.opt_text v))

let db t = t.db

let create ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  Sqlite3.Rc.check (Sqlite3_eio.exec db "PRAGMA journal_mode=WAL");
  Sqlite3.Rc.check (Sqlite3_eio.exec db "PRAGMA synchronous=NORMAL");
  Sqlite3.Rc.check (Sqlite3_eio.exec db schema_sql);
  List.iter index_sql ~f:(fun sql ->
    Sqlite3.Rc.check (Sqlite3_eio.exec db sql)
  );
  { db }

let log_request t (local_ info : Httpz_eio.request_info) =
  (* Globalize all local string fields before binding to SQLite *)
  let timestamp = F64.to_float info.timestamp in
  let remote_addr = globalize info.remote_addr in
  let forwarded_for = globalize_or_null info.forwarded_for in
  let forwarded_proto = globalize_or_null info.forwarded_proto in
  let meth = Httpz.Method.to_string info.meth in
  let target = globalize info.target in
  let path = globalize info.path in
  let host = globalize_or_null info.host in
  let user_agent = globalize_or_null info.user_agent in
  let referer = globalize_or_null info.referer in
  let accept = globalize_or_null info.accept in
  let request_headers = encode_headers_json (globalize_pairs info.request_headers) in
  let status_code = Httpz.Res.status_code info.status in
  let response_content_type = globalize_or_null info.response_content_type in
  let response_body_size = info.response_body_size in
  let cache_status = globalize_or_null info.cache_status in
  let duration_us = info.duration_us in
  (* Prepare a fresh statement per insert — concurrent fibers from
     keep-alive connections would race on a shared prepared statement. *)
  let stmt = Sqlite3_eio.prepare t.db insert_sql in
  Sqlite3.Rc.check (Sqlite3.bind_double stmt 1 timestamp);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 2 remote_addr);
  bind_text_opt stmt 3 forwarded_for;
  bind_text_opt stmt 4 forwarded_proto;
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 5 meth);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 6 target);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 7 path);
  bind_text_opt stmt 8 host;
  bind_text_opt stmt 9 user_agent;
  bind_text_opt stmt 10 referer;
  bind_text_opt stmt 11 accept;
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 12 request_headers);
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 13 status_code);
  bind_text_opt stmt 14 response_content_type;
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 15 response_body_size);
  bind_text_opt stmt 16 cache_status;
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 17 duration_us);
  let rc = Sqlite3_eio.step t.db stmt in
  ignore (Sqlite3_eio.finalize t.db stmt : Sqlite3.Rc.t);
  match rc with
  | Sqlite3.Rc.DONE -> ()
  | rc -> Sqlite3.Rc.check rc
