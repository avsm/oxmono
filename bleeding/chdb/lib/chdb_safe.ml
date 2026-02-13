[@@@alert "-deprecated"]

type 'k t = {
  conn : Chdb_connection.t;
  mutex : 'k Capsule_blocking_sync.Mutex.t;
}

type packed = Pack : 'k t -> packed

let create ?path () =
  let conn = Chdb_connection.connect ?path () in
  let (Capsule_expert.Key.P key) = Capsule_expert.create () in
  let mutex = Capsule_blocking_sync.Mutex.create key in
  Pack { conn; mutex }

let mutex t = t.mutex

let with_lock t ~f =
  Capsule_blocking_sync.Mutex.with_lock t.mutex ~f

let query_string t ~password:_ ?(format = Chdb_format.CSV) query =
  Chdb_query.execute_string t.conn ~format query

let execute_ignore t ~password:_ ?(format = Chdb_format.Null) query =
  Chdb_query.execute_ignore t.conn ~format query

let close t ~password:_ =
  Chdb_connection.close t.conn
