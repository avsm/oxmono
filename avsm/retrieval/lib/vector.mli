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

val put :
  Sqlite3.db ->
  index:string ->
  source:string ->
  revision:string ->
  input_hash:string ->
  float list ->
  unit

val invalidate : Sqlite3.db -> string -> unit
