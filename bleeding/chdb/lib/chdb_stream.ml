type t = {
  conn : Chdb_connection.t;
  mutable stream_handle : Chdb_raw.result_handle option;
  mutable exhausted : bool;
}

let start conn ?(format = Chdb_format.CSV) query =
  let format_str = Chdb_format.to_string format in
  Chdb_connection.with_handle conn ~f:(fun handle ->
    match Chdb_raw.stream_query handle query format_str with
    | Null ->
      Chdb_error.raise_query_failed "Failed to start streaming query"
    | This stream ->
      if Chdb_raw.result_has_error stream then begin
        let err = Chdb_raw.result_error stream in
        Chdb_raw.destroy_result stream;
        Chdb_error.raise_query_failed err
      end;
      { conn; stream_handle = Some stream; exhausted = false })

let fetch t =
  if t.exhausted then None
  else match t.stream_handle with
  | None -> None
  | Some stream ->
    Chdb_connection.with_handle t.conn ~f:(fun handle ->
      match Chdb_raw.stream_fetch handle stream with
      | Null ->
        t.exhausted <- true;
        None
      | This result ->
        if Chdb_raw.result_has_error result then begin
          let err = Chdb_raw.result_error result in
          Chdb_raw.destroy_result result;
          t.exhausted <- true;
          Chdb_error.raise_query_failed err
        end;
        Some (Chdb_result.of_raw result))

let cancel t =
  match t.stream_handle with
  | None -> ()
  | Some stream ->
    (try
       Chdb_connection.with_handle t.conn ~f:(fun handle ->
         Chdb_raw.stream_cancel handle stream)
     with Chdb_error.E Chdb_error.Use_after_close -> ());
    t.exhausted <- true

let iter t ~f =
  let rec loop () =
    match fetch t with
    | None -> ()
    | Some chunk -> f chunk; loop ()
  in
  loop ()

let fold t ~init ~f =
  let rec loop acc =
    match fetch t with
    | None -> acc
    | Some chunk -> loop (f acc chunk)
  in
  loop init

let to_seq t =
  let rec next () =
    match fetch t with
    | None -> Seq.Nil
    | Some chunk -> Seq.Cons (chunk, next)
  in
  next

let count t =
  fold t ~init:0 ~f:(fun n _chunk -> n + 1)

let collect t =
  List.rev (fold t ~init:[] ~f:(fun acc chunk ->
    Chdb_result.to_string chunk :: acc))

let collect_string t =
  String.concat "" (collect t)
