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

let identity ~model ~dimensions ~normalization =
  if dimensions < 1 || dimensions > 4096 || model = "" || normalization = ""
  then invalid_arg "Invalid embedding identity or dimensions";
  let identity =
    Json.encode (Jsont.list Jsont.string)
      [ model; string_of_int dimensions; normalization ]
  in
  "vec_" ^ Embedding.hash identity

let exists db index =
  rows db "SELECT 1 FROM vector_indexes WHERE name=?" [ txt (name index) ] <> []

let index db ~model ~dimensions ~normalization =
  let table = identity ~model ~dimensions ~normalization in
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

let[@zero_alloc] rec fill bytes offset = function
  | [] -> ()
  | value :: rest ->
      Bytes.set_int32_ne bytes offset (Int32.bits_of_float value);
      fill bytes (offset + 4) rest

let binding vector =
  (* Retain the JSON parser's range and empty-vector behaviour for exceptional
     inputs. Embedding.checked bounds normal inference values well inside this
     range. The native-endian BLOB is sqlite-vec's float32 wire format. *)
  if
    vector = []
    || List.exists
         (fun f ->
           (not (Float.is_finite f))
           || abs_float f > 1e30
           || (f <> 0. && abs_float f < 1e-30))
         vector
  then txt (json vector)
  else
    let bytes = Bytes.create (4 * List.length vector) in
    fill bytes 0 vector;
    D.BLOB (Bytes.unsafe_to_string bytes)

let put db ~index ~source ~revision ~input_hash vector =
  let value = binding vector in
  execute db ("DELETE FROM " ^ name index ^ " WHERE source=?") [ txt source ];
  execute db
    ("INSERT INTO " ^ name index
   ^ "(source,embedding,revision,input_hash) VALUES(?,?,?,?)")
    [ txt source; value; txt revision; txt input_hash ]

let invalidate db source =
  List.iter
    (fun r ->
      execute db
        ("DELETE FROM " ^ name (str r.(0)) ^ " WHERE source=?")
        [ txt source ])
    (rows db "SELECT name FROM vector_indexes" [])
