type t = {
  elapsed : float;
  rows_read : int64;
  bytes_read : int64;
  storage_rows_read : int64;
  storage_bytes_read : int64;
}

let[@inline always] of_result_handle h =
  { elapsed = Chdb_raw.result_elapsed h;
    rows_read = Chdb_raw.result_rows_read h;
    bytes_read = Chdb_raw.result_bytes_read h;
    storage_rows_read = Chdb_raw.result_storage_rows_read h;
    storage_bytes_read = Chdb_raw.result_storage_bytes_read h }

external of_result_handle_local
  : Chdb_raw.result_handle -> local_ t
  = "caml_chdb_result_stats_local"

let pp fmt t =
  Format.fprintf fmt
    "@[<v>elapsed: %.6f s@,\
     rows_read: %Ld@,\
     bytes_read: %Ld@,\
     storage_rows_read: %Ld@,\
     storage_bytes_read: %Ld@]"
    t.elapsed t.rows_read t.bytes_read
    t.storage_rows_read t.storage_bytes_read
