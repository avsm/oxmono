type read
type write
type 'access t
val open_read : sw:Eio.Switch.t -> _ Eio.Path.t -> read t
val open_write : sw:Eio.Switch.t -> _ Eio.Path.t -> write t
val run : 'access t -> (Sqlite3.db -> 'a) -> 'a
val exclusive :
  write t -> (((Sqlite3.db -> unit) -> unit) -> 'a) -> 'a
val check : Sqlite3.Rc.t -> unit
val exec : Sqlite3.db -> string -> unit
val with_stmt : Sqlite3.db -> string -> (Sqlite3.stmt -> 'a) -> 'a
val bind : Sqlite3.stmt -> Sqlite3.Data.t list -> unit
val done_ : Sqlite3.stmt -> unit
val text : string -> Sqlite3.Data.t
val int : int -> Sqlite3.Data.t
val opt_text : string option -> string
val transaction : Sqlite3.db -> (unit -> 'a) -> 'a
