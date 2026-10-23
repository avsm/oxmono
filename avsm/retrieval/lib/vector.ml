open Sql

let name value =
  if
    (not (String.starts_with ~prefix:"vec_" value))
    || not
         (String.for_all
            (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
            value)
  then invalid_arg "Invalid vector index name";
  value

let init db =
  execute db
    "CREATE TABLE IF NOT EXISTS vector_indexes(name TEXT PRIMARY KEY, model \
     TEXT NOT NULL, dimensions INTEGER NOT NULL, normalization TEXT NOT NULL)"
    []

let index db ~model ~dimensions ~normalization =
  if dimensions < 1 || dimensions > 4096 || model = "" || normalization = ""
  then invalid_arg "Invalid embedding identity or dimensions";
  let identity =
    Json.encode (Jsont.list Jsont.string)
      [ model; string_of_int dimensions; normalization ]
  in
  let table = "vec_" ^ Embedding.hash identity in
  execute db "INSERT OR IGNORE INTO vector_indexes VALUES(?,?,?,?)"
    [ txt table; txt model; int dimensions; txt normalization ];
  execute db
    (Printf.sprintf
       "CREATE VIRTUAL TABLE IF NOT EXISTS %s USING vec0(source TEXT PRIMARY \
        KEY, embedding float[%d], +revision TEXT, +input_hash TEXT)"
       table dimensions)
    [];
  table

let current db ~index ~source ~revision ~input_hash =
  rows db
    ("SELECT 1 FROM " ^ name index
   ^ " WHERE source=? AND revision=? AND input_hash=?")
    [ txt source; txt revision; txt input_hash ]
  <> []

let json vector =
  if List.exists (fun f -> not (Float.is_finite f)) vector then
    invalid_arg "Embedding contains non-finite values";
  Json.encode (Jsont.list Jsont.number) vector

let put db ~index ~source ~revision ~input_hash vector =
  let value = json vector in
  execute db ("DELETE FROM " ^ name index ^ " WHERE source=?") [ txt source ];
  execute db
    ("INSERT INTO " ^ name index
   ^ "(source,embedding,revision,input_hash) VALUES(?,?,?,?)")
    [ txt source; txt value; txt revision; txt input_hash ]

let invalidate db source =
  List.iter
    (fun r ->
      execute db
        ("DELETE FROM " ^ name (str r.(0)) ^ " WHERE source=?")
        [ txt source ])
    (rows db "SELECT name FROM vector_indexes" [])
