val name : string -> string
(** SQLite vector operations shared by corpus-specific stores. Callers own
    transactions and must check source revisions before [put]. *)

val init : Sqlite3.db -> unit

val index :
  Sqlite3.db -> model:string -> dimensions:int -> normalization:string -> string

val current :
  Sqlite3.db ->
  index:string ->
  source:string ->
  revision:string ->
  input_hash:string ->
  bool

val json : float list -> string

val binding : float list -> Sqlite3.Data.t
(** [binding vector] encodes a float32 SQLite vector. Exceptional magnitudes
    retain the JSON parser's validation and rounding behaviour. *)

val put :
  Sqlite3.db ->
  index:string ->
  source:string ->
  revision:string ->
  input_hash:string ->
  float list ->
  unit

val invalidate : Sqlite3.db -> string -> unit
