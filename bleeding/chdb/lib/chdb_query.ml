let execute conn ?(format = Chdb_format.CSV) query =
  let format_str = Chdb_format.to_string format in
  Chdb_connection.with_handle conn ~f:(fun handle ->
    match Chdb_raw.query handle query format_str with
    | Null ->
      Chdb_error.raise_query_failed "Query returned NULL"
    | This raw ->
      let result = Chdb_result.of_raw raw in
      Chdb_result.check_error result;
      result)

let execute_string conn ?format query =
  let result = execute conn ?format query in
  let s = Chdb_result.to_string result in
  Chdb_result.destroy result;
  s

let execute_bigstring conn ?format query =
  let result = execute conn ?format query in
  let buf = Chdb_result.to_bigstring result in
  (result, buf)

let with_result conn ?format query ~f =
  let result = execute conn ?format query in
  match f result with
  | value -> Chdb_result.destroy result; value
  | exception e -> Chdb_result.destroy result; raise e

let with_buffer conn ?format query ~f =
  with_result conn ?format query ~f:(fun result ->
    f (Chdb_result.to_bigstring result))

let execute_ignore conn ?(format = Chdb_format.Null) query =
  let result = execute conn ~format query in
  Chdb_result.destroy result

let try_execute conn ?format query =
  match execute conn ?format query with
  | result -> Ok result
  | exception Chdb_error.E err ->
    Base.Or_error.error_string (Chdb_error.to_string err)

let try_execute_string conn ?format query =
  match execute_string conn ?format query with
  | s -> Ok s
  | exception Chdb_error.E err ->
    Base.Or_error.error_string (Chdb_error.to_string err)
