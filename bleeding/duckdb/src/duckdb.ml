exception Error of string

let () = Callback.Safe.register_exception "Duckdb.Error" (Error "")

type database
type connection
type result

(* ── Init ────────────────────────────────────────────────────────── *)

external init : unit -> unit = "caml_duckdb_init"
let () = init ()

(* ── Lifecycle ───────────────────────────────────────────────────── *)

external open_raw : string option -> database = "caml_duckdb_open"
external close : database -> unit = "caml_duckdb_close"
external connect : database -> connection = "caml_duckdb_connect"

let open_database ?path () = open_raw path

(* ── Query ───────────────────────────────────────────────────────── *)

external query : connection -> string -> result = "caml_duckdb_query"

(* ── Prepared statements ─────────────────────────────────────────── *)

type stmt

external prepare : connection -> string -> stmt = "caml_duckdb_prepare"
external execute : stmt -> result = "caml_duckdb_execute_prepared"

external bind_bool_raw : stmt -> (int[@untagged]) -> bool -> unit
  = "caml_duckdb_bind_bool_bc" "caml_duckdb_bind_bool"
[@@noalloc]

external bind_int32_raw : stmt -> (int[@untagged]) -> (int32[@unboxed]) -> unit
  = "caml_duckdb_bind_int32_bc" "caml_duckdb_bind_int32"
[@@noalloc]

external bind_int64_raw : stmt -> (int[@untagged]) -> (int64[@unboxed]) -> unit
  = "caml_duckdb_bind_int64_bc" "caml_duckdb_bind_int64"
[@@noalloc]

external bind_double_raw : stmt -> (int[@untagged]) -> (float[@unboxed]) -> unit
  = "caml_duckdb_bind_double_bc" "caml_duckdb_bind_double"
[@@noalloc]

external bind_string : stmt -> int -> string -> unit
  = "caml_duckdb_bind_string"

external bind_blob : stmt -> int -> bytes -> unit
  = "caml_duckdb_bind_blob"

external bind_null_raw : stmt -> (int[@untagged]) -> unit
  = "caml_duckdb_bind_null_bc" "caml_duckdb_bind_null"
[@@noalloc]

external clear_bindings : stmt -> unit = "caml_duckdb_clear_bindings"

external param_count : stmt -> (int[@untagged])
  = "caml_duckdb_nparams_bc" "caml_duckdb_nparams"
[@@noalloc]

let bind_bool stmt idx v = bind_bool_raw stmt idx v
let bind_int32 stmt idx v = bind_int32_raw stmt idx v
let bind_int64 stmt idx v = bind_int64_raw stmt idx v
let bind_double stmt idx v = bind_double_raw stmt idx v
let bind_null stmt idx = bind_null_raw stmt idx

(* ── Result metadata ─────────────────────────────────────────────── *)

external column_count : result -> (int[@untagged])
  = "caml_duckdb_column_count_bc" "caml_duckdb_column_count"
[@@noalloc]

external row_count : result -> (int[@untagged])
  = "caml_duckdb_row_count_bc" "caml_duckdb_row_count"
[@@noalloc]

external rows_changed : result -> (int[@untagged])
  = "caml_duckdb_rows_changed_bc" "caml_duckdb_rows_changed"
[@@noalloc]

external column_name : result -> int -> string = "caml_duckdb_column_name"

(* ── Column types ────────────────────────────────────────────────── *)

module Type = struct
  type t =
    | Invalid
    | Boolean
    | Tinyint
    | Smallint
    | Integer
    | Bigint
    | UTinyint
    | USmallint
    | UInteger
    | UBigint
    | Float
    | Double
    | Timestamp
    | Date
    | Time
    | Interval
    | Hugeint
    | Varchar
    | Blob
    | Decimal
    | Timestamp_s
    | Timestamp_ms
    | Timestamp_ns
    | Enum
    | List
    | Struct
    | Map
    | UUID
    | Union
    | Bit
    | Time_tz
    | Timestamp_tz
    | UHugeint
    | Array
    | Geometry
    | Unknown of int

  let of_int = function
    | 0 -> Invalid
    | 1 -> Boolean
    | 2 -> Tinyint
    | 3 -> Smallint
    | 4 -> Integer
    | 5 -> Bigint
    | 6 -> UTinyint
    | 7 -> USmallint
    | 8 -> UInteger
    | 9 -> UBigint
    | 10 -> Float
    | 11 -> Double
    | 12 -> Timestamp
    | 13 -> Date
    | 14 -> Time
    | 15 -> Interval
    | 16 -> Hugeint
    | 17 -> Varchar
    | 18 -> Blob
    | 19 -> Decimal
    | 20 -> Timestamp_s
    | 21 -> Timestamp_ms
    | 22 -> Timestamp_ns
    | 23 -> Enum
    | 24 -> List
    | 25 -> Struct
    | 26 -> Map
    | 27 -> UUID
    | 28 -> Union
    | 29 -> Bit
    | 30 -> Time_tz
    | 31 -> Timestamp_tz
    | 32 -> UHugeint
    | 33 -> Array
    | 60 -> Geometry
    | n -> Unknown n

  let to_string = function
    | Invalid -> "INVALID"
    | Boolean -> "BOOLEAN"
    | Tinyint -> "TINYINT"
    | Smallint -> "SMALLINT"
    | Integer -> "INTEGER"
    | Bigint -> "BIGINT"
    | UTinyint -> "UTINYINT"
    | USmallint -> "USMALLINT"
    | UInteger -> "UINTEGER"
    | UBigint -> "UBIGINT"
    | Float -> "FLOAT"
    | Double -> "DOUBLE"
    | Timestamp -> "TIMESTAMP"
    | Date -> "DATE"
    | Time -> "TIME"
    | Interval -> "INTERVAL"
    | Hugeint -> "HUGEINT"
    | Varchar -> "VARCHAR"
    | Blob -> "BLOB"
    | Decimal -> "DECIMAL"
    | Timestamp_s -> "TIMESTAMP_S"
    | Timestamp_ms -> "TIMESTAMP_MS"
    | Timestamp_ns -> "TIMESTAMP_NS"
    | Enum -> "ENUM"
    | List -> "LIST"
    | Struct -> "STRUCT"
    | Map -> "MAP"
    | UUID -> "UUID"
    | Union -> "UNION"
    | Bit -> "BIT"
    | Time_tz -> "TIME WITH TIME ZONE"
    | Timestamp_tz -> "TIMESTAMP WITH TIME ZONE"
    | UHugeint -> "UHUGEINT"
    | Array -> "ARRAY"
    | Geometry -> "GEOMETRY"
    | Unknown n -> Printf.sprintf "UNKNOWN(%d)" n
end

external column_type_raw : result -> (int[@untagged]) -> (int[@untagged])
  = "caml_duckdb_column_type_bc" "caml_duckdb_column_type"
[@@noalloc]

let column_type result col = Type.of_int (column_type_raw result col)

(* ── Typed column access ─────────────────────────────────────────── *)

external value_int64 : result -> (int[@untagged]) -> (int[@untagged]) -> (int64[@unboxed])
  = "caml_duckdb_value_int64_bc" "caml_duckdb_value_int64"
[@@noalloc]

external value_int32 : result -> (int[@untagged]) -> (int[@untagged]) -> (int32[@unboxed])
  = "caml_duckdb_value_int32_bc" "caml_duckdb_value_int32"
[@@noalloc]

external value_double : result -> (int[@untagged]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_duckdb_value_double_bc" "caml_duckdb_value_double"
[@@noalloc]

external value_string : result -> col:int -> row:int -> string
  = "caml_duckdb_value_string"

external value_is_null : result -> (int[@untagged]) -> (int[@untagged]) -> bool
  = "caml_duckdb_value_is_null_bc" "caml_duckdb_value_is_null"
[@@noalloc]

(* ── Data chunk API ──────────────────────────────────────────────── *)

module Data_chunk = struct
  type t

  external chunk_count : result -> (int[@untagged])
    = "caml_duckdb_result_chunk_count_bc" "caml_duckdb_result_chunk_count"
  [@@noalloc]

  let chunk_count r = chunk_count r

  external get_chunk : result -> int -> t = "caml_duckdb_result_get_chunk"

  external size : t -> (int[@untagged])
    = "caml_duckdb_chunk_get_size_bc" "caml_duckdb_chunk_get_size"
  [@@noalloc]

  external column_count : t -> (int[@untagged])
    = "caml_duckdb_chunk_get_column_count_bc" "caml_duckdb_chunk_get_column_count"
  [@@noalloc]
end

module Vector = struct
  external data : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    = "caml_duckdb_vector_data"

  external validity : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t option
    = "caml_duckdb_vector_validity"

  external get_string : Data_chunk.t -> col:int -> row:int -> string
    = "caml_duckdb_vector_get_string"

  external is_valid : Data_chunk.t -> (int[@untagged]) -> (int[@untagged]) -> bool
    = "caml_duckdb_vector_is_valid_bc" "caml_duckdb_vector_is_valid"
  [@@noalloc]
end

external vector_size : unit -> (int[@untagged])
  = "caml_duckdb_vector_size_bc" "caml_duckdb_vector_size"
[@@noalloc]

(* ── Extension loading ────────────────────────────────────────────── *)

let install_extension conn name =
  let _ = query conn (Printf.sprintf "INSTALL %s" name) in ()

let load_extension conn name =
  let _ = query conn (Printf.sprintf "LOAD %s" name) in ()

(* ── Geometry ────────────────────────────────────────────────────── *)

module Geometry = struct
  type geom_type =
    | Point
    | Linestring
    | Polygon
    | Multi_point
    | Multi_linestring
    | Multi_polygon
    | Geometry_collection

  type coord = { x : float; y : float }

  type t =
    | Point of coord
    | Linestring of coord array
    | Polygon of coord array array  (* outer ring, then holes *)
    | Multi_point of coord array
    | Multi_linestring of coord array array
    | Multi_polygon of coord array array array
    | Geometry_collection of t list

  (* ── WKT output ────────────────────────────────────────────── *)

  let coord_to_wkt buf c =
    Buffer.add_string buf (Printf.sprintf "%g %g" c.x c.y)

  let coords_to_wkt buf cs =
    Buffer.add_char buf '(';
    Array.iteri (fun i c ->
      if i > 0 then Buffer.add_string buf ", ";
      coord_to_wkt buf c) cs;
    Buffer.add_char buf ')'

  let ring_array_to_wkt buf rings =
    Buffer.add_char buf '(';
    Array.iteri (fun i ring ->
      if i > 0 then Buffer.add_string buf ", ";
      coords_to_wkt buf ring) rings;
    Buffer.add_char buf ')'

  let rec to_wkt_buf buf = function
    | Point c ->
      Buffer.add_string buf "POINT (";
      coord_to_wkt buf c;
      Buffer.add_char buf ')'
    | Linestring cs ->
      Buffer.add_string buf "LINESTRING ";
      coords_to_wkt buf cs
    | Polygon rings ->
      Buffer.add_string buf "POLYGON ";
      ring_array_to_wkt buf rings
    | Multi_point cs ->
      Buffer.add_string buf "MULTIPOINT ";
      coords_to_wkt buf cs
    | Multi_linestring css ->
      Buffer.add_string buf "MULTILINESTRING (";
      Array.iteri (fun i cs ->
        if i > 0 then Buffer.add_string buf ", ";
        coords_to_wkt buf cs) css;
      Buffer.add_char buf ')'
    | Multi_polygon psss ->
      Buffer.add_string buf "MULTIPOLYGON (";
      Array.iteri (fun i rings ->
        if i > 0 then Buffer.add_string buf ", ";
        ring_array_to_wkt buf rings) psss;
      Buffer.add_char buf ')'
    | Geometry_collection gs ->
      Buffer.add_string buf "GEOMETRYCOLLECTION (";
      List.iteri (fun i g ->
        if i > 0 then Buffer.add_string buf ", ";
        to_wkt_buf buf g) gs;
      Buffer.add_char buf ')'

  let to_wkt t =
    let buf = Buffer.create 64 in
    to_wkt_buf buf t;
    Buffer.contents buf

  (* ── WKT parsing ───────────────────────────────────────────── *)

  let skip_ws s i =
    let len = String.length s in
    let j = ref i in
    while !j < len && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\n') do
      incr j
    done;
    !j

  let expect_char s i c =
    let i = skip_ws s i in
    if i >= String.length s || s.[i] <> c then
      raise (Error (Printf.sprintf "WKT: expected '%c' at position %d" c i));
    i + 1

  let parse_number s i =
    let i = skip_ws s i in
    let len = String.length s in
    let j = ref i in
    while !j < len &&
          (let c = s.[!j] in
           (c >= '0' && c <= '9') || c = '.' || c = '-' || c = '+' ||
           c = 'e' || c = 'E') do
      incr j
    done;
    if !j = i then raise (Error "WKT: expected number");
    let n = float_of_string (String.sub s i (!j - i)) in
    (n, !j)

  let parse_coord s i =
    let (x, i) = parse_number s i in
    let (y, i) = parse_number s i in
    ({ x; y }, i)

  let parse_coord_list s i =
    let i = expect_char s i '(' in
    let coords = ref [] in
    let i = ref i in
    let first = ref true in
    while !i < String.length s && s.[skip_ws s !i] <> ')' do
      if not !first then i := expect_char s !i ',';
      first := false;
      let (c, j) = parse_coord s !i in
      coords := c :: !coords;
      i := j
    done;
    let i = expect_char s !i ')' in
    (Array.of_list (List.rev !coords), i)

  let parse_ring_list s i =
    let i = expect_char s i '(' in
    let rings = ref [] in
    let i = ref i in
    let first = ref true in
    while !i < String.length s && s.[skip_ws s !i] <> ')' do
      if not !first then i := expect_char s !i ',';
      first := false;
      let (ring, j) = parse_coord_list s !i in
      rings := ring :: !rings;
      i := j
    done;
    let i = expect_char s !i ')' in
    (Array.of_list (List.rev !rings), i)

  let starts_with_at s i prefix =
    let plen = String.length prefix in
    let slen = String.length s in
    if i + plen > slen then false
    else
      let ok = ref true in
      for k = 0 to plen - 1 do
        let c1 = Char.uppercase_ascii s.[i + k] in
        let c2 = Char.uppercase_ascii prefix.[k] in
        if c1 <> c2 then ok := false
      done;
      !ok

  let rec parse_wkt_at s i =
    let i = skip_ws s i in
    if starts_with_at s i "GEOMETRYCOLLECTION" then begin
      let i = i + 18 in
      let i = expect_char s i '(' in
      let geoms = ref [] in
      let i = ref i in
      let first = ref true in
      while !i < String.length s && s.[skip_ws s !i] <> ')' do
        if not !first then i := expect_char s !i ',';
        first := false;
        let (g, j) = parse_wkt_at s !i in
        geoms := g :: !geoms;
        i := j
      done;
      let i = expect_char s !i ')' in
      (Geometry_collection (List.rev !geoms), i)
    end
    else if starts_with_at s i "MULTIPOLYGON" then begin
      let i = i + 12 in
      let i = expect_char s i '(' in
      let polys = ref [] in
      let i = ref i in
      let first = ref true in
      while !i < String.length s && s.[skip_ws s !i] <> ')' do
        if not !first then i := expect_char s !i ',';
        first := false;
        let (rings, j) = parse_ring_list s !i in
        polys := rings :: !polys;
        i := j
      done;
      let i = expect_char s !i ')' in
      (Multi_polygon (Array.of_list (List.rev !polys)), i)
    end
    else if starts_with_at s i "MULTILINESTRING" then begin
      let i = i + 15 in
      let i = expect_char s i '(' in
      let lines = ref [] in
      let i = ref i in
      let first = ref true in
      while !i < String.length s && s.[skip_ws s !i] <> ')' do
        if not !first then i := expect_char s !i ',';
        first := false;
        let (cs, j) = parse_coord_list s !i in
        lines := cs :: !lines;
        i := j
      done;
      let i = expect_char s !i ')' in
      (Multi_linestring (Array.of_list (List.rev !lines)), i)
    end
    else if starts_with_at s i "MULTIPOINT" then begin
      let i = i + 10 in
      let (cs, i) = parse_coord_list s i in
      (Multi_point cs, i)
    end
    else if starts_with_at s i "POLYGON" then begin
      let i = i + 7 in
      let (rings, i) = parse_ring_list s i in
      (Polygon rings, i)
    end
    else if starts_with_at s i "LINESTRING" then begin
      let i = i + 10 in
      let (cs, i) = parse_coord_list s i in
      (Linestring cs, i)
    end
    else if starts_with_at s i "POINT" then begin
      let i = i + 5 in
      let i = expect_char s i '(' in
      let (c, i) = parse_coord s i in
      let i = expect_char s i ')' in
      (Point c, i)
    end
    else
      raise (Error (Printf.sprintf
        "WKT: unknown geometry type at position %d" i))

  let of_wkt s =
    let (g, _) = parse_wkt_at s 0 in
    g

  let geom_type (g : t) : geom_type = match g with
    | Point _ -> Point
    | Linestring _ -> Linestring
    | Polygon _ -> Polygon
    | Multi_point _ -> Multi_point
    | Multi_linestring _ -> Multi_linestring
    | Multi_polygon _ -> Multi_polygon
    | Geometry_collection _ -> Geometry_collection

  let geom_type_to_string (gt : geom_type) = match gt with
    | Point -> "POINT"
    | Linestring -> "LINESTRING"
    | Polygon -> "POLYGON"
    | Multi_point -> "MULTIPOINT"
    | Multi_linestring -> "MULTILINESTRING"
    | Multi_polygon -> "MULTIPOLYGON"
    | Geometry_collection -> "GEOMETRYCOLLECTION"

  (* ── Convenience constructors ──────────────────────────────── *)

  let point x y = Point { x; y }
  let linestring coords = Linestring (Array.of_list coords)
  let polygon rings = Polygon (Array.of_list (List.map Array.of_list rings))

  (* ── SQL helpers ───────────────────────────────────────────── *)

  let to_sql_literal t =
    Printf.sprintf "'%s'::GEOMETRY" (to_wkt t)

  let of_result result ~col ~row =
    of_wkt (value_string result ~col ~row)
end

(* ── Library info ────────────────────────────────────────────────── *)

external library_version : unit -> string = "caml_duckdb_library_version"
